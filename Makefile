BINDIR=bin
LIBSRCDIR=libbancnova
EXESRCDIR=programs

LIBSOURCES=$(shell find $(LIBSRCDIR) -iname '*.rs')
UTILSOURCES=$(shell find $(EXESRCDIR)/util -iname '*.rs')

TESTNAME=$(BINDIR)/test-harness
ROBINFILE=test_data/MM1SM1

RUSTC=rustc
RFLAGS=-L $(BINDIR) --out-dir $(BINDIR)
TFLAGS=--test --crate-type=bin

all: library bin/assemble bin/disassemble bin/compare

library: $(LIBSOURCES)
	$(RUSTC) $(RFLAGS) $(LIBSRCDIR)/lib.rs

bin/%: $(EXESRCDIR)/%.rs $(UTILSOURCES)
	$(RUSTC) $(RFLAGS) $<

test: $(TESTNAME)
	$(TESTNAME)

roundrobin: all
	-rm $(ROBINFILE).asm
	-rm $(ROBINFILE).out
	bin/disassemble $(ROBINFILE).SCN $(ROBINFILE).asm
	bin/assemble $(ROBINFILE).asm $(ROBINFILE).out
	bin/compare $(ROBINFILE).SCN $(ROBINFILE).out

$(TESTNAME): $(SOURCES)
	$(RUSTC) $(TFLAGS) -o $(TESTNAME) $(LIBSRCDIR)/lib.rs

clean:
	-rm $(BINDIR)/*

spotless: clean
	-find . -iname '*~' -exec rm {} \;
