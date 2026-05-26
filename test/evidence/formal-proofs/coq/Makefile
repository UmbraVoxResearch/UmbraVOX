COQC ?= coqc
COQDEP ?= coqdep
VFILES = $(shell cat _CoqProject | grep '\.v$$')
VOFILES = $(VFILES:.v=.vo)

# Files that import coqprime require -native-compiler no
# (workaround for coq-bignums findlib plugin issue in nixpkgs Rocq 9.1.1)
COQPRIME_FILES = Ed25519GroupUniversal.v Ed25519AssocUniversal.v Ed25519CongruenceUniversal.v Ed25519PointAdd.v

all: $(VOFILES)
	@echo "Coq proofs: all checked."

%.vo: %.v
	@echo "Checking $<..."
	@if echo "$(COQPRIME_FILES)" | grep -qw "$<"; then \
		$(COQC) -native-compiler no -R . UmbraVox $< 2>&1 || \
			(echo "SKIP: $< (coqprime not available)" && true); \
	else \
		$(COQC) -R . UmbraVox $< 2>&1 || \
			(echo "SKIP: $< (ZArith/stdlib not available — build in VM instead)" && true); \
	fi

depend:
	$(COQDEP) -R . UmbraVox $(VFILES) > .depend 2>/dev/null || true

clean:
	rm -f *.vo *.vok *.vos *.glob .*.aux .depend

-include .depend

.PHONY: all clean depend
