RUSTC=rustc
RFLAGS=--out-dir=build
OUTPUTLIB=--crate-type=lib --out-dir=lib
TESTFLAGS=-L./lib --out-dir=test

clean: 
	rm build/cpu test/ld lib/*.rlib

all: src/cpu.rs
	$(RUSTC) $(RFLAGS) src/cpu.rs

test: lib/libcpu*.rlib test/ld.rs
	$(RUSTC) $(TESTFLAGS) test/ld.rs

lib/libcpu*.rlib: src/cpu.rs
	$(RUSTC) $(OUTPUTLIB) src/cpu.rs
