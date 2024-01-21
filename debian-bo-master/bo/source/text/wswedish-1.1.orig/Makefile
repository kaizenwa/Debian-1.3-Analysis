LANG = swedish

all: $(LANG)

$(LANG): words.$(LANG).gz
	zcat words.$(LANG).gz >$(LANG)

install: all
	install -o 0 -g 0 -m 755 -d $(prefix)/dict
	install -o 0 -g 0 -m 755 -d $(prefix)/man/man5
	install -o 0 -g 0 -m 644 $(LANG) $(prefix)/dict
	install -o 0 -g 0 -m 644 words.5 $(prefix)/man/man5/$(LANG).5

clean:
	rm -f $(LANG) *~ ./#*
