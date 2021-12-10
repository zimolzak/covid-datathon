.PHONY: all clean upload

all:
	$(MAKE) -C outputs

clean:
	rm -f outputs/*.docx outputs/*.pdf outputs/*.txt
	rm -f pngs/*.png pngs-v4/*.png

upload:
	cp outputs/README.pdf /Users/ajz/Box\ Sync/COVID_DATATHON/thoughts.pdf
