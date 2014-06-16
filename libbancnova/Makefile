PROJECT=bancnova
BINDIR=../bin
SRCDIR=.

SOURCES=$(shell find $(SRCDIR) -iname '*.rs')
OUTNAME=$(BINDIR)/$(PROJECT)
TESTNAME=$(BINDIR)/$(PROJECT)-test

RUSTC=rustc
RFLAGS=
TFLAGS=--test --crate-type=bin

all: $(OUTNAME)

test: $(TESTNAME)
	./$(TESTNAME)

$(OUTNAME): $(SOURCES)
	$(RUSTC) $(RFLAGS) -o $(OUTNAME) lib.rs

$(TESTNAME): $(SOURCES)
	$(RUSTC) $(TFLAGS) -o $(TESTNAME) lib.rs

clean:
	-rm $(OUTNAME)
	-rm $(TESTNAME)

spotless: clean
	-rm *~
