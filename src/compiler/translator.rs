use std::collections::HashMap;

use crate::ast::expr::Expr;
use crate::ast::literal::Literal;

use super::error::CompilerError;
use super::val::*;
use cranelift::module::{FuncId, Module};
use cranelift::object::ObjectModule;
use cranelift::prelude::*;

#[derive(Debug, Clone, Copy)]
pub enum VarType {
    Value(Variable),
    Func(FuncId),
}

impl VarType {
    pub fn require_value(&self) -> Result<Variable, CompilerError> {
        match self {
            Self::Value(v) => Ok(*v),
            _ => Err(CompilerError::InvalidType(format!(
                "Expected `value`, got {:?}",
                self
            ))),
        }
    }

    pub fn require_fn(&self) -> Result<FuncId, CompilerError> {
        match self {
            Self::Func(id) => Ok(*id),
            _ => Err(CompilerError::InvalidType(format!(
                "Expected `function`, got {:?}",
                self
            ))),
        }
    }
}

/// The main system used to translate zel AST into functions
pub struct Translator<'a> {
    pub builder: FunctionBuilder<'a>,
    pub vars: HashMap<String, (bool, VarType)>,
    pub module: &'a mut ObjectModule,
    pub var_id: usize,
}

impl Translator<'_> {
    pub fn finish(self) {
        self.builder.finalize();
    }

    pub fn declare_args(&mut self, args: Vec<String>, entry_block: Block) {
        for (i, arg) in args.into_iter().enumerate() {
            let val = self.builder.block_params(entry_block)[i];
            let var = self.declare_variable(false, arg);
            self.builder.def_var(var, val);
        }
    }

    pub fn declare_variable(&mut self, mutable: bool, name: String) -> Variable {
        let var = Variable::new(self.var_id);
        if let std::collections::hash_map::Entry::Vacant(e) = self.vars.entry(name) {
            e.insert((mutable, VarType::Value(var)));
            self.builder.declare_var(var, types::I64);
            self.var_id += 1;
        }
        var
    }

    pub fn new_var(
        &mut self,
        mutable: bool,
        name: String,
        val: TranslationValue,
    ) -> Result<(), CompilerError> {
        match val {
            TranslationValue::Null => {
                return Err(CompilerError::InvalidType(
                    "null when needed anything else".to_string(),
                ));
            }
            TranslationValue::Value(v) => {
                let var = self.declare_variable(mutable, name);
                self.builder.def_var(var, v);
            }
            TranslationValue::Func(f) => {
                self.vars.insert(name, (mutable, VarType::Func(f)));
            }
        }
        Ok(())
    }

    pub fn def_var(&mut self, name: String, val: TranslationValue) -> Result<(), CompilerError> {
        let (mutable, var) = self
            .vars
            .get_mut(&name)
            .ok_or(CompilerError::Missing(name.clone()))?;

        if !*mutable {
            return Err(CompilerError::MutationError(name));
        }
        match val {
            TranslationValue::Null => {
                return Err(CompilerError::InvalidType(
                    "null when needed anything else".to_string(),
                ));
            }
            TranslationValue::Value(v) => {
                let var = var.require_value()?;
                self.builder.def_var(var, v);
            }
            TranslationValue::Func(new) => {
                let _old = var.require_fn()?;
                *var = VarType::Func(new);
            }
        }
        Ok(())
    }

    pub fn translate(&mut self, expr: Expr) -> Result<TranslationValue, CompilerError> {
        Ok(match expr {
            Expr::Literal(v) => match v {
                Literal::Num(n) => val(self.builder.ins().iconst(types::I64, n)),
                Literal::Bool(b) => val(self.builder.ins().iconst(
                    types::I8,
                    match b {
                        true => 1,
                        false => 0,
                    },
                )),
            },
            Expr::Then { first, next } => {
                self.translate(*first)?;
                self.translate(*next)?
            }
            Expr::Binary { lhs, op, rhs } => {
                let lhs = self.translate(*lhs)?;
                let rhs = self.translate(*rhs)?;
                lhs.bin_op(&rhs, op, self)?
            }
            Expr::Local(var) => {
                let var = self
                    .vars
                    .get(&var)
                    .ok_or_else(|| CompilerError::Missing(var.to_string()))?
                    .1
                    .require_value()?;
                val(self.builder.use_var(var))
            }
            Expr::Set { name, body } => {
                let value = self.translate(*body)?;
                self.def_var(name, value)?;
                TranslationValue::Null
            }
            Expr::Let {
                mutable,
                name,
                body,
            } => {
                let value = self.translate(*body)?;
                self.new_var(mutable, name, value)?;
                TranslationValue::Null
            }
            Expr::Func { args, body } => {
                let mut new_ctx = self.module.make_context();

                for _arg in args.iter() {
                    new_ctx
                        .func
                        .signature
                        .params
                        .push(AbiParam::new(types::I64));
                }

                new_ctx
                    .func
                    .signature
                    .returns
                    .push(AbiParam::new(types::I64));

                let mut builder_context = FunctionBuilderContext::new();

                let mut builder = FunctionBuilder::new(&mut new_ctx.func, &mut builder_context);

                let entry_block = builder.create_block();

                builder.switch_to_block(entry_block);
                builder.seal_block(entry_block);

                builder.append_block_params_for_function_params(entry_block);

                let mut translator = Translator {
                    builder,
                    vars: HashMap::new(),
                    module: self.module,
                    var_id: 0,
                };

                translator.declare_args(args, entry_block);

                let ret = translator.translate(*body)?;
                translator.builder.ins().return_(&[ret.require_value()?]);

                translator.finish();
                println!("{}", new_ctx.func);

                let id = self
                    .module
                    .declare_anonymous_function(&new_ctx.func.signature)?;
                self.module.define_function(id, &mut new_ctx)?;
                func(id)
            }
            Expr::Call { func, args } => {
                let mut sig = self.module.make_signature();

                for _arg in &args {
                    sig.params.push(AbiParam::new(types::I64));
                }
                sig.returns.push(AbiParam::new(types::I64));

                let id = match *func {
                    Expr::Local(l) => self
                        .vars
                        .get(&l)
                        .ok_or_else(|| CompilerError::Missing(l.to_string()))?
                        .1
                        .require_fn()?,
                    f => self.translate(f)?.require_fn()?,
                };

                let func_ref = self.module.declare_func_in_func(id, self.builder.func);

                let mut solved_args = vec![];

                for arg in args {
                    let value = self.translate(arg)?.require_value()?;
                    solved_args.push(value)
                }

                let call = self.builder.ins().call(func_ref, &solved_args);

                val(self.builder.inst_results(call)[0])
            }
            Expr::Null => TranslationValue::Null,
            _ => todo!(),
        })
    }
}
