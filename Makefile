print_input:
	cargo run -- ./examples/print_input.zl ./examples/bin/print_input && \
	./examples/bin/print_input

vec:
	cargo run -- ./examples/vec.zl ./examples/bin/vec && \
	./examples/bin/vec

test:
	cargo test
