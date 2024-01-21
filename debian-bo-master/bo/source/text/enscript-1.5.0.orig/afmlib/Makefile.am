#
# Makefile for AFM library.
# Copyright (c) 1995 Markku Rossi
#
# Author: Markku Rossi <mtr@iki.fi>
#

#
# This file is part of the AFM library.
# 
# This library is free software; you can redistribute it and/or modify
# it under the terms of the GNU Library General Public License as
# published by the Free Software Foundation; either version 2, or (at
# your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this library; see the file COPYING.LIB.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.
#

AUTOMAKE_OPTIONS = ansi2knr

noinst_LIBRARIES = libafm.a
libafm_a_SOURCES = afm.c afmparse.c strhash.c enc_iso.c enc_iso2.c \
	enc_iso3.c enc_iso5.c enc_pc.c enc_mac.c enc_vms.c enc_hp8.c \
	enc_koi8.c deffont.c

noinst_HEADERS = afm.h afmint.h strhash.h

EXTRA_DIST = ansi2knr.c ansi2knr.1

afmtest: afmtest.o libafm.a
	$(CC) $(LDFLAGS) -o afmtest afmtest.o libafm.a
