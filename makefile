files = thoughts.pdf Rplots_outpat.pdf covid_procs_stlukes.csv Routputs_vali_procs.txt Routputs_inpat.txt Routputs_inpat_v4.txt thoughts.docx thoughts-notes-appendix.pdf Routputs_inpat_v5.txt
files_unmentioned = Routputs_outpat.txt Rplots_inpat.pdf
.PHONY: all clean upload
infiles_outpat = PATIENT.txt PAT_ORDERS_PROCEDURES.txt PAT_ENC_DX.txt PAT_PRBL_LIST.txt Pat_FlowSheet_PulseOx.txt
infile_bsl = COVID_1_SLH.tab

all: $(files)

covid_procs_stlukes.csv: COVID_PROCS_STLUKES.txt txt2csv.pl
	./txt2csv.pl $< > $@

%.pdf: %.txt
	pandoc -o $@ $<

%.docx: %.txt
	pandoc -o $@ $<

Routputs_inpat.txt: analysis_inpat.R $(infile_bsl)
	Rscript $< > $@

Rplots_outpat.pdf: analysis_outpat.R $(infiles_outpat)
	Rscript $< > Routputs_outpat.txt
	mv -f Rplots.pdf Rplots_outpat.pdf

Routputs_vali_procs.txt: validate_covid_procs.R covid_procs_stlukes.csv
	Rscript $< > $@

Routputs_inpat_v4.txt: bslmc_v4_DataSets_pipe.R # infiles_v4, sadly 15 of them
	Rscript $< > $@

Routputs_inpat_v5.txt: bslmc_v5_covid_only.R
	Rscript $< > $@

clean: 
	rm -f $(files)

#### not part of "all"

old_output.txt: old_monolithic_analysis.R Random_COVID_PAT.tsv
	Rscript old_monolithic_analysis.R > $@

upload:
	cp thoughts.pdf /Users/ajz/Box\ Sync/COVID_DATATHON
