files = Rplots_outpat.pdf Routputs_inpat_v4.txt Routputs_inpat_v5.txt Routputs_inpat.txt
files_unmentioned = Routputs_outpat.txt Rplots_inpat_v4.pdf Rplots_inpat.pdf

.PHONY: all clean upload

all: $(files)
	$(MAKE) -C outputs

#### Analyses

Rplots_outpat.pdf: analysis_outpat.R
	Rscript $< > Routputs_outpat.txt
	mv -f Rplots.pdf Rplots_outpat.pdf

Routputs_inpat.txt: analysis_inpat.R # COVID_1....
	Rscript $< > $@

Routputs_inpat_v4.txt: bslmc_v4_DataSets_pipe.R # infiles_v4, sadly 15 of them
	Rscript $< > $@

Routputs_inpat_v5.txt: bslmc_v5_covid_only.R
	Rscript $< > $@

#### not part of "all"

clean:
	rm -f $(files) $(files_unmentioned)
	rm -f pngs/*.png new-pngs/*.png

upload:
	cp README.pdf /Users/ajz/Box\ Sync/COVID_DATATHON/thoughts.pdf
