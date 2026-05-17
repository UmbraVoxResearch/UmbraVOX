COQC ?= coqc
COQDEP ?= coqdep
VFILES = $(shell cat _CoqProject | grep '\.v$$')
VOFILES = $(VFILES:.v=.vo)

all: $(VOFILES)

%.vo: %.v
	$(COQC) -R . UmbraVox $<

depend:
	$(COQDEP) -R . UmbraVox $(VFILES) > .depend

clean:
	rm -f *.vo *.vok *.vos *.glob .*.aux .depend

-include .depend

.PHONY: all clean depend
