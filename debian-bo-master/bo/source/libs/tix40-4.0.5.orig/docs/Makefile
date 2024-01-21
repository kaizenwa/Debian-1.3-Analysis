# WARNING
#
# This Makefile is NOT for installation purposes. Please read the file
# docs/Install.html for information about installing Tix.
#
#
#
#
#
#

.SUFFIXES: .html .txt

TXT = Changes.txt FAQ.txt Install.txt \
	Release.txt  \
	Release-4.0.0.txt  \
	Release-4.0.1.txt \
	Release-4.0.2.txt \
	Release-4.0.3.txt \
	Release-4.0.4.txt \
	Release-4.0.5.txt  

warning::
	@ head -10 Makefile
	@ exit 1

docs: html txt

html:: FAQ.html

txt:: $(TXT)

FAQ.html: FAQ.tcl
	FAQ.tcl > FAQ.html

.html.txt:
	lynx -dump $< | skipblank.tcl > $@

clean::
	- rm -f *~

distclean: clean


