BINDIR=bin
LIBSRCDIR=libbancnova
EXESRCDIR=programs

LIBSOURCES=$(shell find $(LIBSRCDIR) -iname '*.rs')
UTILSOURCES=$(shell find $(EXESRCDIR)/util -iname '*.rs')

TESTNAME=$(BINDIR)/test-harness

RUSTC=rustc
RFLAGS=-L $(BINDIR) --out-dir $(BINDIR)
TFLAGS=--test --crate-type=bin

all: library assembler disassembler

library: $(LIBSOURCES)
	$(RUSTC) $(RFLAGS) $(LIBSRCDIR)/lib.rs

assembler: $(EXESRCDIR)/assemble.rs $(UTILSOURCES)
	$(RUSTC) $(RFLAGS) $<

disassembler: $(EXESRCDIR)/disassemble.rs $(UTILSOURCES)
	$(RUSTC) $(RFLAGS) $<

test: $(TESTNAME)
	$(TESTNAME)

$(TESTNAME): $(SOURCES)
	$(RUSTC) $(TFLAGS) -o $(TESTNAME) $(LIBSRCDIR)/lib.rs

clean:
	-rm $(BINDIR)/*

spotless: clean
	-find . -iname '*~' -exec rm {} \;
