#
# Makefile for the GNU Lesstif OS/2 shared libraries
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
#

SHARED_LIB=${lib_name}.dll
LOADER_LIB_O =${lib_name}.a
LOADER_LIB_OBJ =${lib_name}.lib

all:: createshared ${SHARED_LIB}

${SHARED_LIB}: $(SH_OBJS)

	copy ${lib_name}OS2.def.in ${lib_name}OS2.def 
	emxexp -o -u shared/*.OBJ >> ${lib_name}OS2.def
	${CC}  -o ${SHARED_LIB} -Zdll -Zomf -Zmtd $(SH_OBJS) ${lib_name}os2.def -L${lib_dir} -lXt -lX11 -lSM -lICE ${OS2LIB}
	emximp -o ${lib_name}.lib ${lib_name}os2.def
	emximp -o ${lib_name}.a ${lib_name}.lib
 
install:: ${SHARED_LIB}
	-del ${lib_dir}\${SHARED_LIB} > nul 2>&1
	-del ${lib_dir}\${LOADER_LIB_O} > nul 2>&1
	-del ${lib_dir}\${LOADER_LIB_OBJ} > nul 2>&1
	${INSTALL_DATA} ${SHARED_LIB} ${lib_dir}
	${INSTALL_DATA} ${LOADER_LIB_O} ${lib_dir}
	${INSTALL_DATA} ${LOADER_LIB_OBJ} ${lib_dir}

clean::
	del ${SHARED_LIB} ${SH_OBJS}

 
createshared::
	@test -d shared || mkdir shared

