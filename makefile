files = thoughts.pdf Rplots.pdf covid_procs_stlukes.csv Routputs_vali_procs.txt
.PHONY: all clean upload
infiles = PATIENT.txt PAT_ORDERS_PROCEDURES.txt PAT_ENC_DX.txt PAT_PRBL_LIST.txt Pat_FlowSheet_PulseOx.txt

all: $(files)

covid_procs_stlukes.csv: COVID_PROCS_STLUKES.txt txt2csv.pl
	./txt2csv.pl $< > $@

%.pdf: %.txt
	pandoc -o $@ $<

Rplots.pdf: analysis_with_joins.R $(infiles)
	Rscript $< > Routputs.txt

Routputs_vali_procs.txt: validate_covid_procs.R covid_procs_stlukes.csv
	Rscript $< > $@

clean: 
	rm -f $(files)

#### not part of "all"

old_output.txt: old_monolithic_analysis.R Random_COVID_PAT.tsv
	Rscript old_monolithic_analysis.R > $@

upload:
	cp thoughts.pdf /Users/ajz/Box\ Sync/COVID_DATATHON
