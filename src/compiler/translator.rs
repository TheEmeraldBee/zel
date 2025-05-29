use std::collections::HashMap;

use crate::ast::expr::Expr;
use crate::ast::literal::Literal;
use crate::semantic::SemanticSolver;
use crate::types::CompType;

use super::error::CompilerError;
use super::val::*;
use cranelift::codegen::ir::BlockArg;
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
    pub sem: &'a mut SemanticSolver,
    pub var_id: usize,
}

impl Translator<'_> {
    pub fn finish(self) {
        self.builder.finalize();
    }

    pub fn declare_args(&mut self, args: Vec<(String, Type)>, entry_block: Block) {
        for (i, (arg, type_)) in args.into_iter().enumerate() {
            let val = self.builder.block_params(entry_block)[i];
            let var = self.declare_variable(false, arg, type_);
            self.builder.def_var(var, val);
        }
    }

    pub fn declare_variable(&mut self, mutable: bool, name: String, type_: Type) -> Variable {
        let var = Variable::new(self.var_id);
        if let std::collections::hash_map::Entry::Vacant(e) = self.vars.entry(name) {
            e.insert((mutable, VarType::Value(var)));
            self.builder.declare_var(var, type_);
            self.var_id += 1;
        }
        var
    }

    pub fn new_var(
        &mut self,
        mutable: bool,
        name: String,
        type_: CompType,
        val: TranslationValue,
    ) -> Result<(), CompilerError> {
        match val {
            TranslationValue::Null => {
                return Err(CompilerError::InvalidType(
                    "null when needed anything else".to_string(),
                ));
            }
            TranslationValue::Value(v) => {
                let var = self.declare_variable(mutable, name, type_.as_type().unwrap());
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
            return Err(CompilerError::Mutation(name));
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
        let expr_type = self.sem.type_of(&expr)?;
        Ok(match expr {
            Expr::Literal(v) => match v {
                Literal::Num(n) => val(self.builder.ins().iconst(expr_type.as_type().unwrap(), n)),
                Literal::Bool(b) => val(self.builder.ins().iconst(
                    Type::int(8).unwrap(),
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
                let type_ = self.sem.type_of(&body)?;
                let value = self.translate(*body)?;
                self.new_var(mutable, name, type_, value)?;
                TranslationValue::Null
            }
            Expr::Func { args, body } => {
                let CompType::Function(arg_types, body_type) = expr_type else {
                    unreachable!("Function should be correctly typed");
                };

                let mut new_ctx = self.module.make_context();

                let mut solved_args = vec![];
                for ((name, _), type_) in args.iter().zip(arg_types.into_iter()) {
                    solved_args.push((name.clone(), type_.as_type().unwrap()));
                    new_ctx
                        .func
                        .signature
                        .params
                        .push(AbiParam::new(type_.as_type().unwrap()));
                }

                new_ctx
                    .func
                    .signature
                    .returns
                    .push(AbiParam::new(body_type.as_type().unwrap()));

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
                    sem: self.sem,
                    var_id: 0,
                };

                translator.declare_args(solved_args, entry_block);

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

                for arg in &args {
                    let arg_type = self.sem.type_of(&arg)?.as_type().unwrap();
                    sig.params.push(AbiParam::new(arg_type));
                }

                sig.returns
                    .push(AbiParam::new(expr_type.as_type().unwrap()));

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
            Expr::If { cond, body, else_ } => {
                let condition_value = self.translate(*cond)?.require_value()?;
                // let condition_value = self.builder.ins().iconst(Type::int(8).unwrap(), 1);

                if let Some(else_) = else_ {
                    let then_block = self.builder.create_block();
                    let else_block = self.builder.create_block();
                    let merge_block = self.builder.create_block();

                    self.builder
                        .append_block_param(merge_block, expr_type.as_type().unwrap());

                    self.builder
                        .ins()
                        .brif(condition_value, then_block, &[], else_block, &[]);

                    self.builder.switch_to_block(then_block);
                    self.builder.seal_block(then_block);
                    let then_return = self.translate(*body)?.require_value()?;

                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(then_return)]);

                    self.builder.switch_to_block(else_block);
                    self.builder.seal_block(else_block);
                    let else_ret = self.translate(*else_)?.require_value()?;

                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(else_ret)]);

                    self.builder.switch_to_block(merge_block);

                    self.builder.seal_block(merge_block);

                    let phi = self.builder.block_params(merge_block)[0];

                    val(phi)
                } else {
                    let then_block = self.builder.create_block();
                    let merge_block = self.builder.create_block();

                    // Test the if condition and conditionally branch.
                    self.builder
                        .ins()
                        .brif(condition_value, then_block, &[], then_block, &[]);

                    self.builder.switch_to_block(then_block);
                    self.builder.seal_block(then_block);
                    self.translate(*body)?.require_value()?;

                    // Jump to the merge block, passing it the block return value.
                    self.builder.ins().jump(merge_block, &[]);

                    // Switch to the merge block for subsequent statements.
                    self.builder.switch_to_block(merge_block);

                    // We've now seen all the predecessors of the merge block.
                    self.builder.seal_block(merge_block);

                    TranslationValue::Null
                }
            }
            Expr::Null => TranslationValue::Null,
            _ => todo!(),
        })
    }
}
