files = thoughts.pdf Rplots_outpat.pdf Routputs_inpat_v4.txt thoughts.docx thoughts-notes-appendix.pdf Routputs_inpat_v5.txt
files_unmentioned = Routputs_outpat.txt Rplots_inpat_v4.pdf
.PHONY: all clean upload
infiles_outpat = PATIENT.txt PAT_ORDERS_PROCEDURES.txt PAT_ENC_DX.txt PAT_PRBL_LIST.txt Pat_FlowSheet_PulseOx.txt

all: $(files)

%.pdf: %.txt
	pandoc -o $@ $<

%.docx: %.txt
	pandoc -o $@ $<

#### Analyses

Rplots_outpat.pdf: analysis_outpat.R $(infiles_outpat)
	Rscript $< > Routputs_outpat.txt
	mv -f Rplots.pdf Rplots_outpat.pdf

Routputs_inpat_v4.txt: bslmc_v4_DataSets_pipe.R # infiles_v4, sadly 15 of them
	Rscript $< > $@

Routputs_inpat_v5.txt: bslmc_v5_covid_only.R
	Rscript $< > $@

#### not part of "all"

clean: 
	rm -f $(files) $(files_unmentioned)

upload:
	cp thoughts.pdf /Users/ajz/Box\ Sync/COVID_DATATHON
