MLCOMP=mlton
FILES=aplparse.mlb AplLex.sml AplParse.sml ParseComb.sml PARSE_COMB.sig test.sml test.mlb

all: $(FILES)
	$(MLCOMP) aplparse.mlb

test: $(FILES) test.mlb test.sml
	$(MLCOMP) test.mlb

clean:
	rm -rf *~ MLB aplparse test