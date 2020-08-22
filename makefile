files = thoughts.pdf Rplots.pdf covid_procs_stlukes.csv Routputs_vali_procs.txt Routputs_bslmc.txt Routputs_bslmc_v4.txt thoughts.docx thoughts-notes-appendix.pdf
files_unmentioned = Routputs.txt Rplots_bslmc.pdf
.PHONY: all clean upload
infiles = PATIENT.txt PAT_ORDERS_PROCEDURES.txt PAT_ENC_DX.txt PAT_PRBL_LIST.txt Pat_FlowSheet_PulseOx.txt
infile_bsl = COVID_1_SLH.tab

all: $(files)

covid_procs_stlukes.csv: COVID_PROCS_STLUKES.txt txt2csv.pl
	./txt2csv.pl $< > $@

%.pdf: %.txt
	pandoc -o $@ $<

%.docx: %.txt
	pandoc -o $@ $<

Routputs_bslmc.txt: analysis_inpat.R $(infile_bsl)
	Rscript $< > $@

Rplots.pdf: analysis_outpat.R $(infiles)
	Rscript $< > Routputs.txt

Routputs_vali_procs.txt: validate_covid_procs.R covid_procs_stlukes.csv
	Rscript $< > $@

Routputs_bslmc_v4.txt: bslmc_v4_DataSets_pipe.R # lots of pipe delim inputs are depends too.
	Rscript $< > $@

clean: 
	rm -f $(files)

#### not part of "all"

old_output.txt: old_monolithic_analysis.R Random_COVID_PAT.tsv
	Rscript old_monolithic_analysis.R > $@

upload:
	cp thoughts.pdf /Users/ajz/Box\ Sync/COVID_DATATHON
