#
# Makefile for the library files.
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

pkgdata_DATA = ascii.enc asciifise.enc asciidkno.enc hp8.enc ibmpc.enc \
koi8.enc latin1.enc latin2.enc latin3.enc latin5.enc mac.enc ps.enc \
pslatin1.enc vms.enc a2ps.hdr edd.hdr emacs.hdr enscript.hdr frame.hdr \
mp.hdr simple.hdr enscript.pro

EXTRA_DIST = enscript.cfg.in $(pkgdata_DATA)
CLEANFILES = enscript.cfg

all-local: enscript.cfg

install-data-local: enscript.cfg
	$(top_srcdir)/mkinstalldirs $(sysconfdir)
	if test -r $(sysconfdir)/enscript.cfg; then \
	  cp $(sysconfdir)/enscript.cfg $(sysconfdir)/enscript.cfg.old; \
	else :; \
	fi
	$(INSTALL_DATA) enscript.cfg $(sysconfdir)/enscript.cfg
	
uninstall-local:
	rm -f $(sysconfdir)/enscript.cfg

enscript.cfg: $(srcdir)/enscript.cfg.in Makefile
	sed 's%@DATADIR@%$(datadir)%g; s%@media@%@MEDIA@%g;' \
	$(srcdir)/enscript.cfg.in >enscript.cfg

