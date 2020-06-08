files = thoughts.pdf
.PHONY: all clean upload

all: $(files)

%.pdf: %.txt
	pandoc -o $@ $<

upload:
	cp $(files) /Users/ajz/Box\ Sync/COVID_DATATHON

clean: 
	rm -f $(files)
