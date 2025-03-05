use std::collections::HashMap;

use codegen::CodegenError;
use cranelift::{
    module::{FuncId, Module, ModuleError},
    object::{ObjectBuilder, ObjectModule, ObjectProduct},
    prelude::*,
};
use isa::LookupError;
use settings::SetError;
use thiserror::Error;

use crate::ast::{expr::Expr, literal::Literal, ops::BinaryOp};

/// Represents an error that may be thrown by any method in the compiler
#[derive(Error, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    IsaLookup(#[from] LookupError),

    #[error(transparent)]
    Module(#[from] ModuleError),

    #[error(transparent)]
    Codegen(#[from] CodegenError),

    #[error(transparent)]
    Set(#[from] SetError),

    #[error("Variable {0} does not exist")]
    Missing(String),

    #[error("Expression {0} should be a function, but is not")]
    NotAFunction(String),

    #[error("{0}")]
    InvalidType(String),
}

/// A structure storing context and requirements to convert the AST into cranelift IR, and eventually an object file
pub struct Compiler {
    builder_context: FunctionBuilderContext,

    ctx: codegen::Context,

    module: ObjectModule,
}

impl Compiler {
    /// Create a new compiler with the given out target
    pub fn new(name: &str) -> Result<Self, CompilerError> {
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic")?;
        let isa_builder = isa::lookup_by_name("aarch64-apple-darwin")?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

        let builder = ObjectBuilder::new(
            isa,
            name.to_owned(),
            cranelift::module::default_libcall_names(),
        )?;

        let module = ObjectModule::new(builder);

        Ok(Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
        })
    }

    /// Compile the given instruction into the system
    pub fn compile(&mut self, name: impl AsRef<str>, expr: Expr) -> Result<(), CompilerError> {
        if let Expr::Func { args: _, body } = expr {
            self.ctx
                .func
                .signature
                .returns
                .push(AbiParam::new(types::I64));

            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

            let entry_block = builder.create_block();

            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            let mut translator = Translator {
                builder,
                vars: HashMap::new(),
                module: &mut self.module,
            };

            let ret = translator.translate(*body)?;
            translator.builder.ins().return_(&[ret.require_value()?]);

            translator.builder.finalize();

            println!("{:?}", self.ctx.func);

            let id = self.module.declare_function(
                name.as_ref(),
                cranelift::module::Linkage::Export,
                &self.ctx.func.signature,
            )?;

            self.module.define_function(id, &mut self.ctx)?;

            self.module.clear_context(&mut self.ctx);
            Ok(())
        } else {
            Err(CompilerError::NotAFunction(name.as_ref().to_string()))
        }
    }

    /// Consume the compiler and build an object file
    pub fn finish(self) -> ObjectProduct {
        self.module.finish()
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TranslationValue {
    Value(Value),
    Func(FuncId),
}

impl TranslationValue {
    pub fn require_value(&self) -> Result<Value, CompilerError> {
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

    pub fn bin_op(
        &self,
        rhs: &Self,
        op: BinaryOp,
        translator: &mut Translator<'_>,
    ) -> Result<Self, CompilerError> {
        let lhs = self.require_value()?;
        let rhs = rhs.require_value()?;

        Ok(val(match op {
            BinaryOp::Add => translator.builder.ins().iadd(lhs, rhs),
            BinaryOp::Sub => translator.builder.ins().isub(lhs, rhs),
            BinaryOp::Mul => translator.builder.ins().imul(lhs, rhs),
            BinaryOp::Div => translator.builder.ins().sdiv(lhs, rhs),
        }))
    }
}

fn val(value: Value) -> TranslationValue {
    TranslationValue::Value(value)
}

fn func(id: FuncId) -> TranslationValue {
    TranslationValue::Func(id)
}

/// The main system used to translate zel AST into functions
pub struct Translator<'a> {
    builder: FunctionBuilder<'a>,
    vars: HashMap<String, Variable>,
    module: &'a mut ObjectModule,
}

impl Translator<'_> {
    pub fn finish(self) {
        self.builder.finalize();
    }
    pub fn translate(&mut self, expr: Expr) -> Result<TranslationValue, CompilerError> {
        Ok(match expr {
            Expr::Literal(v) => match v {
                Literal::Num(n) => val(self.builder.ins().iconst(types::I64, n)),
                Literal::Bool(_) => todo!(),
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
                    .get(var)
                    .ok_or_else(|| CompilerError::Missing(var.to_string()))?;
                val(self.builder.use_var(*var))
            }
            Expr::Let {
                mutable: _,
                name,
                body,
            } => {
                let value = self.translate(*body)?.require_value()?;
                let variable = self
                    .vars
                    .get(name)
                    .ok_or_else(|| CompilerError::Missing(name.to_string()))?;
                self.builder.def_var(*variable, value);
                val(value)
            }
            Expr::Func { args, body } => {
                let mut new_ctx = self.module.make_context();

                for _arg in args {
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

                let mut translator = Translator {
                    builder,
                    vars: HashMap::new(),
                    module: self.module,
                };

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

                let id = self.translate(*func)?.require_fn()?;

                let func_ref = self.module.declare_func_in_func(id, self.builder.func);

                let mut solved_args = vec![];

                for arg in args {
                    let value = self.translate(arg)?.require_value()?;
                    solved_args.push(value)
                }

                let call = self.builder.ins().call(func_ref, &solved_args);

                val(self.builder.inst_results(call)[0])
            }
            _ => todo!(),
        })
    }
}
