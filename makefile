files = thoughts.pdf old_output.txt
.PHONY: all clean upload

all: $(files)

%.pdf: %.txt
	pandoc -o $@ $<

old_output.txt: old_monolithic_analysis.R Random_COVID_PAT.tsv
	Rscript old_monolithic_analysis.R > $@

upload:
	cp thoughts.pdf /Users/ajz/Box\ Sync/COVID_DATATHON

clean: 
	rm -f $(files)
