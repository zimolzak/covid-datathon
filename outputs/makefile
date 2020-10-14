files = README.pdf README.docx thoughts-notes-appendix.pdf

.PHONY: all

all: $(files)

#### Documents

%.pdf: ../%.md
	pandoc -o $@ $<

%.docx: ../%.md
	pandoc -o $@ $<
