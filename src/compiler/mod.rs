use std::collections::HashMap;

use cranelift::{
    module::Module,
    object::{ObjectBuilder, ObjectModule, ObjectProduct},
    prelude::*,
};
use translator::Translator;

use crate::ast::expr::Expr;

mod error;
use error::*;

mod val;

mod translator;

/// A structure storing context and requirements to convert the AST into cranelift IR, and eventually an object file
pub struct Compiler {
    builder_context: FunctionBuilderContext,

    ctx: codegen::Context,

    module: ObjectModule,
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
        })
    }

    /// Compile the given instruction into the system
    pub fn compile(&mut self, name: impl AsRef<str>, expr: Expr) -> Result<(), CompilerError> {
        if let Expr::Func { args, body } = expr {
            for _arg in args.iter() {
                self.ctx
                    .func
                    .signature
                    .params
                    .push(AbiParam::new(types::I64));
            }

            self.ctx
                .func
                .signature
                .returns
                .push(AbiParam::new(types::I64));

            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

            let entry_block = builder.create_block();

            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            builder.append_block_params_for_function_params(entry_block);

            let mut translator = Translator {
                builder,
                vars: HashMap::new(),
                module: &mut self.module,
                var_id: 0,
            };

            translator.declare_args(args, entry_block);

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
