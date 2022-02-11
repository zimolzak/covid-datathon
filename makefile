survey_rendered = datathon-survey.pdf datathon-survey.docx

.PHONY: all clean upload

all: $(survey_rendered)
	$(MAKE) -C outputs

clean:
	rm -f outputs/*.docx outputs/*.pdf outputs/*.txt
	rm -f pngs/*.png pngs-v4/*.png

upload:
	cp outputs/README.pdf /Users/ajz/Box\ Sync/COVID_DATATHON/thoughts.pdf

datathon-survey.pdf: datathon-survey.txt
	pandoc -o $@ $<

datathon-survey.docx: datathon-survey.txt
	pandoc -o $@ $<
