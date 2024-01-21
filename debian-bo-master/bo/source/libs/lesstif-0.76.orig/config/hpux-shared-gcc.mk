#
# Makefile for the GNU Lesstif HPUX gcc-based shared libraries
#
# Copyright (C) 1995 Free Software Foundation, Inc.
#
# This file is part of the GNU LessTif Library.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the Free
# Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

SHARED_LIB=lib${lib_name}.sl

all:: createshared ${SHARED_LIB}

${SHARED_LIB}: $(SH_OBJS)
	/bin/ld -b -n -o ${SHARED_LIB} ${SH_OBJS}
	chmod a+x ${SHARED_LIB}

install:: ${SHARED_LIB}
	mkdir -p ${lib_dir}
	${INSTALL_DATA} ${SHARED_LIB} ${lib_dir}

clean::
	rm -f ${SHARED_LIB} ${SH_OBJS}
 
createshared::
	@test -d shared || mkdir shared
