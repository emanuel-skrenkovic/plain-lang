build:
	cargo build

run:
	./target/debug/sage

test: build
	./test.py
