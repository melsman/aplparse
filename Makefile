MLCOMP ?= mlton -mlb-path-map $(HOME)/.mlton/mlb-path-map
FILES=aplparse.mlb REGION.sig Region.sml AplLex.sml AplParse.sml ParseComb.sml PARSE_COMB.sig test.sml test.mlb

.PHONY: all
all: $(FILES) Makefile
	$(MLCOMP) -output aplparse aplparse.mlb

test: $(FILES) test.mlb test.sml Makefile
	$(MLCOMP) -output test test.mlb

TESTFILES=test.apl test1.apl test2.apl test3.apl test4.apl test5.apl
.PHONY: tests
tests: test Makefile
	$(foreach tf,$(TESTFILES),./test tests/$(tf);)

.PHONY: clean
clean:
	rm -rf *~ MLB aplparse run test tests/*~
