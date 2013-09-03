MLCOMP=mlton
FILES=aplparse.mlb AplLex.sml AplParse.sml ParseComb.sml PARSE_COMB.sig test.sml test.mlb

all: $(FILES)
	$(MLCOMP) aplparse.mlb

test: $(FILES) test.mlb test.sml
	$(MLCOMP) test.mlb

TESTFILES=test.apl test1.apl test2.apl test3.apl test4.apl test5.apl
.PHONY: tests
tests: test Makefile
	$(foreach tf,$(TESTFILES),./test tests/$(tf);)

clean:
	rm -rf *~ MLB aplparse test tests/*~
