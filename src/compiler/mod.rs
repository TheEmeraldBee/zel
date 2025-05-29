use std::collections::HashMap;

use cranelift::{
    module::{FuncId, Module},
    object::{ObjectBuilder, ObjectModule, ObjectProduct},
    prelude::*,
};
use translator::{Translator, VarType};

use crate::{
    ast::{expr::Expr, top_level::TopLevel},
    semantic::{SemanticSolver, SemanticState},
    types::CompType,
};

mod error;
use error::*;

mod val;

mod translator;

enum CompVarType {
    Variable(Variable),
    CompiledFunction(FuncId),
    Function((FuncId, Signature, Vec<(String, CompType)>, Expr)),
    Empty,
}

impl CompVarType {
    pub fn compiled_function(
        &mut self,
    ) -> Option<(FuncId, Signature, Vec<(String, CompType)>, Expr)> {
        match std::mem::replace(self, CompVarType::Empty) {
            Self::Function(a) => {
                // This will be a compiled function, so change the var type in place
                *self = Self::CompiledFunction(a.0);
                Some(a)
            }
            old => {
                // Replace old value and return None
                *self = old;
                None
            }
        }
    }

    pub fn as_var(&self) -> VarType {
        match self {
            Self::Variable(v) => VarType::Value(*v),
            Self::CompiledFunction(f) => VarType::Func(*f),
            Self::Function((f, ..)) => VarType::Func(*f),
            Self::Empty => {
                unreachable!("Empty CompVarType found, but should not be possible to find")
            }
        }
    }
}

/// A structure storing context and requirements to convert the AST into cranelift IR, and eventually an object file
pub struct Compiler {
    builder_context: FunctionBuilderContext,

    ctx: codegen::Context,

    module: ObjectModule,

    vars: HashMap<String, CompVarType>,
    index: usize,
}

impl Compiler {
    /// Create a new compiler with the given out target
    pub fn new(name: &str, triple: &str) -> Result<Self, CompilerError> {
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic")?;
        let isa_builder = isa::lookup_by_name(triple)?;
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

            vars: HashMap::new(),
            index: 0,
        })
    }

    pub fn compile_top_level(
        &mut self,
        top_level: TopLevel,
        sem: &mut SemanticSolver,
    ) -> Result<(), CompilerError> {
        let finished = top_level.finish();
        // Register indevidual functions as variables.
        for (name, expr) in finished.iter() {
            sem.scope
                .register(name.clone(), false, SemanticState::Unsolved(expr.clone()));
        }
        for (name, expr) in finished.iter() {
            match expr {
                Expr::Func { args, body } => {
                    let CompType::Function(arg_types, ret_type) = sem.get(name)?.1 else {
                        unreachable!("Function should be correctly typed");
                    };
                    let mut solved_args = vec![];
                    for ((name, _), type_) in args.iter().zip(arg_types.iter()) {
                        self.ctx
                            .func
                            .signature
                            .params
                            .push(AbiParam::new(type_.as_type().unwrap()));
                        solved_args.push((name.clone(), type_.clone()))
                    }

                    self.ctx
                        .func
                        .signature
                        .returns
                        .push(AbiParam::new(ret_type.as_type().unwrap()));

                    let id = self.module.declare_function(
                        name,
                        cranelift::module::Linkage::Export,
                        &self.ctx.func.signature,
                    )?;

                    self.vars.insert(
                        name.clone(),
                        CompVarType::Function((
                            id,
                            self.ctx.func.signature.clone(),
                            solved_args,
                            *body.clone(),
                        )),
                    );

                    self.module.clear_context(&mut self.ctx);
                }
                _ => {
                    todo!("Only functions allowed rn")
                }
            }
        }

        Ok(())
    }

    pub fn compile_funcs(&mut self, sem: &mut SemanticSolver) -> Result<(), CompilerError> {
        for name in self
            .vars
            .keys()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .into_iter()
        {
            // Force the semantics engine to solve the function!
            let my_type = sem.get(&name)?.1;
            println!("{my_type}");
            let var = self.vars.get_mut(&name).unwrap();
            let Some((id, sig, args, body)) = var.compiled_function() else {
                continue;
            };
            self.compile_func(id, sig, args, body, sem)?;
        }
        Ok(())
    }

    /// Compile the given instruction into the system
    pub fn compile_func(
        &mut self,
        id: FuncId,
        sig: Signature,
        args: Vec<(String, CompType)>,
        body: Expr,
        sem: &mut SemanticSolver,
    ) -> Result<(), CompilerError> {
        self.ctx.func.signature = sig;

        println!("Compiling function: {}", body);

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();

        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        builder.append_block_params_for_function_params(entry_block);

        let vars = self
            .vars
            .iter()
            .map(|x| (x.0.clone(), (false, x.1.as_var())))
            .collect::<HashMap<_, _>>();

        let mut translator = Translator {
            builder,
            vars,
            sem,
            module: &mut self.module,
            var_id: self.index,
        };

        translator.declare_args(
            args.into_iter()
                .map(|x| (x.0, x.1.as_type().unwrap()))
                .collect(),
            entry_block,
        );

        let ret = translator.translate(body)?;
        translator.builder.ins().return_(&[ret.require_value()?]);

        translator.builder.finalize();

        println!("{:?}", self.ctx.func);

        self.module.define_function(id, &mut self.ctx)?;

        self.module.clear_context(&mut self.ctx);
        Ok(())
    }
    /// Consume the compiler and build an object file
    pub fn finish(self) -> ObjectProduct {
        self.module.finish()
    }
}
