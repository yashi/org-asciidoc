EMACS ?= emacs
ORG_DIR ?= ../org-mode
LOAD_PATH = $(addprefix $(ORG_DIR),/lisp)

ELS = ox-asciidoc.el
ELCS = $(ELS:.el=.elc)

all: $(ELCS)


test: $(ELCS)
	$(EMACS) -Q --script test-runner.el

clean:
	$(RM) *.elc

%.elc: %.el
	$(EMACS) -Q -L $(LOAD_PATH) -batch -f batch-byte-compile $<
