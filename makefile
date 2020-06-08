files = thoughts.pdf output.txt
.PHONY: all clean upload

all: $(files)

%.pdf: %.txt
	pandoc -o $@ $<

output.txt: analysis.R Random_COVID_PAT.tsv
	Rscript analysis.R > $@

upload:
	cp thoughts.pdf /Users/ajz/Box\ Sync/COVID_DATATHON

clean: 
	rm -f $(files)
