#===============================================================
#  doc directory Makefile
#
#  Copyright (C) 1995  Bruce E. Wampler
#
#  This file is part of the V C++ GUI Framework.
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#===============================================================

CONFIG=../Config.mk
include $(CONFIG)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

.PHONY: default all checkspelling latex html clean cleanall

default: all

all: checkspelling latex html

clean:
	-rm -f $(CLEANEXTS) *.aux *.log *.toc *.ind *.idx *.ilg
	(cd figs; -rm -f $(CLEANEXTS))
	(cd html; -rm -f $(CLEANEXTS))

cleanall: clean
	-rm -f vrefman.dvi vrefman.ps

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

latex:
	latex vrefman.tex
	latex vrefman.tex
	makeindex vrefman
	latex vrefman.tex
	dvips -t letter -o vrefman.ps vrefman.dvi

html:
	rm -rf vwebref
	latex2html -split 1 -link 1 -no_images -no_reuse \
		-show_section_numbers -t "V Reference Manual" vwebref.tex
	cp fig/line.gif vwebref/image.gif
	chmod 755 vwebref
	chmod go+r vwebref/*

checkspelling:
	ispell cxxstyle.tex
	ispell vrefch1.tex
	ispell vrefch2.tex
	ispell vrefch3.tex
	ispell vrefch4.tex
	ispell vrefch5.tex
	ispell vrefch6.tex
	ispell vrefch7.tex
	ispell vrefch8.tex
	ispell vrefch9.tex
	ispell vrefman.tex
	ispell release.tex 
