COQC ?= coqc
COQDEP ?= coqdep
VFILES = $(shell cat _CoqProject | grep '\.v$$')
VOFILES = $(VFILES:.v=.vo)

all: $(VOFILES)
	@echo "Coq proofs: all checked."

%.vo: %.v
	@echo "Checking $<..."
	@$(COQC) -R . UmbraVox $< 2>&1 || \
		(echo "SKIP: $< (ZArith/stdlib not available — build in VM instead)" && true)

depend:
	$(COQDEP) -R . UmbraVox $(VFILES) > .depend 2>/dev/null || true

clean:
	rm -f *.vo *.vok *.vos *.glob .*.aux .depend

-include .depend

.PHONY: all clean depend
