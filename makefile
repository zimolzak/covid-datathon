.PHONY: all clean upload

all:
	$(MAKE) -C outputs

#### not part of "all"

clean:
	rm -f $(files) $(files_unmentioned) # TODO
	rm -f pngs/*.png new-pngs/*.png

upload:
	cp outputs/README.pdf /Users/ajz/Box\ Sync/COVID_DATATHON/thoughts.pdf
