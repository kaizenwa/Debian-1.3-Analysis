#
# Makefile for the GNU Lesstif Linux shared libraries
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
# This file has been updated to deal with the linking semantics of
# gcc-2.7.2 and binutils-2.6.0.2.  However, you should have no trouble
# running with previous versions of the compiler/assembler/linker, if they
# support ELF.
#

SHARED_LIB=lib${lib_name}.so.${major_shared}.${minor_shared}

all:: createshared ${SHARED_LIB}

${SHARED_LIB}: $(SH_OBJS)
	gcc -shared -Wl,-soname,lib${lib_name}.so.${major_shared} -o ${SHARED_LIB} ${SH_OBJS}
	chmod a+x ${SHARED_LIB}
	rm -f lib${lib_name}.so.${major_shared}
	ln -s ${SHARED_LIB} lib${lib_name}.so.${major_shared}
	rm -f lib${lib_name}.so
	ln -s lib${lib_name}.so.${major_shared} lib${lib_name}.so

install:: ${SHARED_LIB}
	mkdir -p ${lib_dir}
	${INSTALL_DATA} ${SHARED_LIB} ${lib_dir}
	rm -f ${lib_dir}/lib${lib_name}.so.${major_shared}
	ln -s ${SHARED_LIB} ${lib_dir}/lib${lib_name}.so.${major_shared}
	rm -f ${lib_dir}/lib${lib_name}.so
	ln -s lib${lib_name}.so.${major_shared} ${lib_dir}/lib${lib_name}.so

clean::
	rm -f ${SHARED_LIB} lib${lib_name}.so.${major_shared} lib${lib_name}.so ${SH_OBJS}

createshared::
	@test -d shared || mkdir shared
