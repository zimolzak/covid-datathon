files = thoughts.pdf old_output.txt Rplots.pdf
.PHONY: all clean upload
infiles = PATIENT.txt PAT_ORDERS_PROCEDURES.txt PAT_ENC_DX.txt PAT_PRBL_LIST.txt Pat_FlowSheet_PulseOx.txt

all: $(files)

%.pdf: %.txt
	pandoc -o $@ $<

old_output.txt: old_monolithic_analysis.R Random_COVID_PAT.tsv
	Rscript old_monolithic_analysis.R > $@

Rplots.pdf: analysis_with_joins.R $(infiles)
	Rscript analysis_with_joins.R

upload:
	cp thoughts.pdf /Users/ajz/Box\ Sync/COVID_DATATHON

clean: 
	rm -f $(files)
