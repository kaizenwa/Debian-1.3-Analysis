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

#
# Build lesstif shared image.
# Required: dlltools >= 2.16
#

#
# Adress space reserved for Motif.
#
JUMP_BASE  = 0x62000000
JUMP_SIZE  = 0x8000
JUMP_GOT   = 0x2000

#
# Your X11 directory setup
#
SO_LIBPATH =/usr/X11R6/lib

#------------ No user-serviceble parts beyond this point ;-> ------------

SO_LIBGCC =`gcc --print-libgcc-file-name`
SO_REV=${major_shared}.${minor_shared}
SO_OBJS=$(SRCS:%.c=%.so)

JUMP_DIR=./jump
JUMP_LIB=lib${lib_name}
JUMP_AR=lib${lib_name}.a~
JUMP_SA=lib${lib_name}.sa
JUMP_SO=lib${lib_name}.so.$(SO_REV)

REQ_LIBS=-lXt -lX11

SHARED_LIB=$(JUMP_SA)

export JUMP_DIR JUMP_LIB

%.so : $(srcdir)/%.c
	$(CC) -B/usr/dll/jump/ -c $(CFLAGS) -o $@ $<

all:: createshared $(SHARED_LIB)

$(JUMP_AR): $(SO_OBJS)
	rm -f $(JUMP_AR)
	(cd .; ar cq  $(JUMP_AR) $(SO_OBJS)) || exit 1
	ranlib $(JUMP_AR)

$(JUMP_DIR)/jump.log:
	@echo "#"
	@echo "# Stage 1: Collect global vars and exported functions."
	@echo "#"
	@if [ -s $(JUMP_DIR)/jump.log ]; then echo "Error: Leftover globals for shared lib"; exit 1; fi
	@if [ -f jump ]; then /bin/false; else ln -s shared jump; fi
	rm -f $(JUMP_AR)
	$(MAKE) $(JUMP_AR)
	touch $@

./mk_JUMP_SO_0: $(JUMP_DIR)/jump.log
	rm -f $(SO_OBJS)
	rm -f $(JUMP_AR)
	getfuncs
	getvars
	touch $@

./mk_JUMP_SO_1: ./mk_JUMP_SO_0
	@echo "#"
	@echo "# Stage 2: Build shared image."
	@echo "#"
	$(MAKE) $(JUMP_AR)
	getsize > j.v
	mv j.v $(JUMP_DIR)/jump.vars
	touch $@

$(JUMP_SO): ./mk_JUMP_SO_1 $(JUMP_AR)
	mkimage -f -l $(JUMP_LIB) -v $(SO_REV) -a $(JUMP_BASE) -j $(JUMP_SIZE) -g $(JUMP_GOT) -- $(JUMP_AR) -L$(SO_LIBPATH) $(REQ_LIBS) $(SO_LIBGCC) -lc -dll-verbose 

$(JUMP_SA): $(JUMP_SO)
	mkstubs -f -l $(JUMP_LIB) -v $(SO_REV) -a $(JUMP_BASE) -j $(JUMP_SIZE) -g $(JUMP_GOT) -- $(JUMP_LIB)
	verify-shlib -l $(JUMP_SO) $(JUMP_SA)

stage1:
	make clean
	make $(JUMP_DIR)/jump.log

stage2: $(JUMP_SA)

install:: ${SHARED_LIB}
	mkdir -p ${lib_dir}
	${INSTALL_DATA} ${SHARED_LIB} ${lib_dir}
	rm -f ${lib_dir}/lib${lib_name}.so.${major_shared}
	ln -s ${lib_dir}/${SHARED_LIB} ${lib_dir}/lib${lib_name}.so.${major_shared}

clean::
	rm -f $(JUMP_SO) $(JUMP_SA) $(JUMP_AR) $(SO_OBJS)
	rm -f $(JUMP_DIR)/*
	rm -f size.nm ./mk_JUMP_SO_0 ./mk_JUMP_SO_1
 
createshared::
	@test -d shared || mkdir shared
