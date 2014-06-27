BINDIR=bin
LIBSRCDIR=libbancnova
ASMSRCDIR=assemble
DISSRCDIR=disassemble

LIBSOURCES=$(shell find $(LIBSRCDIR) -iname '*.rs')
ASMSOURCES=$(shell find $(ASMSRCDIR) -iname '*.rs')
DISSOURCES=$(shell find $(DISSRCDIR) -iname '*.rs')

TESTNAME=$(BINDIR)/test-harness

RUSTC=rustc
RFLAGS=-L $(BINDIR) --out-dir $(BINDIR)
TFLAGS=--test --crate-type=bin

all: library assembler disassembler

library: $(LIBSOURCES)
	$(RUSTC) $(RFLAGS) $(LIBSRCDIR)/lib.rs

assembler: $(ASMSOURCES)
	$(RUSTC) $(RFLAGS) $(ASMSRCDIR)/bin.rs

disassembler: $(DISSOURCES)
	$(RUSTC) $(RFLAGS) $(DISSRCDIR)/bin.rs

test: $(TESTNAME)
	$(TESTNAME)

$(TESTNAME): $(SOURCES)
	$(RUSTC) $(TFLAGS) -o $(TESTNAME) $(LIBSRCDIR)/lib.rs

clean:
	-rm $(BINDIR)/*

spotless: clean
	-find . -iname '*~' -exec rm {} \;
