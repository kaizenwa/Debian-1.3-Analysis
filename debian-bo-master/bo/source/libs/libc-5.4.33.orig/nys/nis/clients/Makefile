#
# Makefile		NIS+ clients
#
# Copyright (c) 1993 Signum Support AB, Sweden.
#
# This file is part of the NYS Library.
#
# The NYS Library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# The NYS Library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
# 
# You should have received a copy of the GNU Library General Public
# License along with the NYS Library; see the file COPYING.LIB.  If
# not, write to the Free Software Foundation, Inc., 675 Mass Ave,
# Cambridge, MA 02139, USA.
#
all: ../bin/nisinfo ../bin/niscat ../bin/nistest

ifneq ($(NYS),true)

../bin/nisinfo: nis_info.o ../lib/libnis.a
	$(CC) -o ../bin/nisinfo nis_info.o $(LIBS)
	
../bin/niscat: nis_cat.o ../lib/libnis.a
	$(CC) -o ../bin/niscat nis_cat.o $(LIBS)

../bin/nistest: nis_test.o ../lib/libnis.a
	$(CC) -o ../bin/nistest nis_test.o $(LIBS)

else

../bin/nisinfo: nis_info.o
	$(CC) -o $@ $^

../bin/niscat: nis_cat.o
	$(CC) -o $@ $^

../bin/nistest: nis_test.o
	$(CC) -o $@ $^

endif

clean:
	rm -f core *.o *~ \#*

install:
	cp ../bin/nisinfo /usr/bin
	cp ../bin/niscat  /usr/bin
	cp ../bin/nistest /usr/bin

TOP=../..
include $(TOP)/Makeconfig



