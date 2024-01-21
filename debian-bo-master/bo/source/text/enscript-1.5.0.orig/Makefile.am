#
# Top level Makefile for enscript.
# Copyright (c) 1995 Markku Rossi
#
# Author: Markku Rossi <mtr@iki.fi>
#

#
# This file is part of GNU enscript.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.
#

SUBDIRS = compat afm afmlib lib scripts intl src po states tests

man_MANS = enscript.1

EXTRA_DIST = README.ESCAPES README.DOS ABOUT-NLS \
	ascii.txt asciifise.txt asciidkno.txt latin1.txt latin2.txt \
	latin3.txt latin5.txt ibmpc.txt mac.txt vms.txt hp8.txt \
	koi8.txt \
	mtr.eps mtr_small.eps mtr_tiny.eps \
	enscript.1.in \
	FAQ.html genscript-vs-enscript.html \
	genscript-vs-nenscript.html genscript-vs-a2ps.html

enscript.man.html: enscript.1
	nroff -man $(srcdir)/enscript.1 > enscript.man
	many2html enscript.man > enscript.man.html
	rm -f enscript.man
