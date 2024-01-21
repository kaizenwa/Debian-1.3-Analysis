all:	install.txt dselect.beginner.8.txt cfdisk.txt

install.html:	install.in.html
	rm -f $@
	sed -e "s/__date__/`date '+%B %d, %Y'`/" <$^ >$@
	chmod a-w $@

install.txt:	install.html
	rm -f $@
	lynx -dump $^ | grep -v "/boot-floppies-"> $@
	chmod a-w $@

dselect.beginner.8.html: dselect.beginner.8.in.html
	rm -f $@
	sed -e '' <$^ >$@
	chmod a-w $@

dselect.beginner.8.txt:	dselect.beginner.8.html
	rm -f $@
	lynx -dump $^ > $@
	chmod a-w $@

cfdisk.txt::
	man cfdisk | col -b >cfdisk.tmp
	if cmp -s cfdisk.tmp $@; then \
			rm -f cfdisk.tmp; \
		else \
			mv -f cfdisk.tmp $@; chmod a-w $@; \
		fi

clean:
	- rm -f *~

distclean: clean
	- rm -f install.txt dselect.beginner.6.txt cfdisk.txt install.html
