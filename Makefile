each-expr:
	cargo run -- examples/each_expr.zl examples/each_expr && \
	./examples/each_expr

test:
	cargo test
