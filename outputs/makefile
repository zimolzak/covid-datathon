documents = README.pdf README.docx thoughts-notes-appendix.pdf

.PHONY: all

all: $(documents)

#### Documents

%.pdf: ../%.md
	pandoc -o $@ $<

%.docx: ../%.md
	pandoc -o $@ $<
