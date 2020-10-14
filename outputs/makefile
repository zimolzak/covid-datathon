analyses = Rplots_outpat.pdf Routputs_inpat_v4.txt Routputs_inpat_v5.txt Routputs_inpat.txt
analyses_unmentioned = Routputs_outpat.txt Rplots_inpat_v4.pdf Rplots_inpat.pdf
documents = README.pdf README.docx thoughts-notes-appendix.pdf

.PHONY: all

all: $(documents) $(analyses)

#### Documents

%.pdf: ../%.md
	pandoc -o $@ $<

%.docx: ../%.md
	pandoc -o $@ $<

#### Analyses

Rplots_outpat.pdf: ../analysis_outpat.R
	Rscript $< > Routputs_outpat.txt
	mv -f ../data/2020-06-08/Rplots.pdf ./Rplots_outpat.pdf

Routputs_inpat.txt: ../analysis_inpat.R # COVID_1....
	Rscript $< > $@

Routputs_inpat_v4.txt: ../bslmc_v4_DataSets_pipe.R # infiles_v4, sadly 15 of them
	Rscript $< > $@

Routputs_inpat_v5.txt: ../bslmc_v5_covid_only.R
	Rscript $< > $@
