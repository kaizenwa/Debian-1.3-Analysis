#
# Makefile
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

all:	../bin/ypmatch ../bin/ypcat ../bin/domainname

ifneq ($(NYS),true)

../bin/ypmatch:	ypmatch.o ../lib/libyp.a
	$(CC) $(CFLAGS) -o ../bin/ypmatch ypmatch.o $(LIBS)

../bin/ypcat:	ypcat.o ../lib/libyp.a
	$(CC) $(CFLAGS) -o ../bin/ypcat ypcat.o $(LIBS)

../bin/domainname: domainname.o
	$(CC) $(CFLAGS) -o ../bin/domainname domainname.o

else

../bin/ypmatch:	ypmatch.o
	$(CC) $(CFLAGS) -o $@ $^

../bin/ypcat:	ypcat.o
	$(CC) $(CFLAGS) -o $@ $^

../bin/domainname:	domainname.o
	$(CC) $(CFLAGS) -o $@ $^

endif

clean:
	-rm -f *.o core *~ \#*

install:
	cp ../bin/ypmatch /usr/bin
	cp ../bin/ypcat   /usr/bin

TOP=../..
include $(TOP)/Makeconfig
