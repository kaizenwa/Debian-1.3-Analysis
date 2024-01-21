#
# Copyright (c) 1990 Jan-Simon Pendry
# Copyright (c) 1990 Imperial College of Science, Technology & Medicine
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Jan-Simon Pendry at Imperial College, London.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#      This product includes software developed by the University of
#      California, Berkeley and its contributors.
# 4. Neither the name of the University nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
#
#	%W% (Berkeley) %G%
#
# $Id: Makefile.com,v 5.2.2.2 1992/03/07 10:22:38 jsp Exp $
#

#
# -------- Users may care to override these values --------
#
# Any of these values can be overridden by redefining them
# in a file called config/Makefile.local or config/Makefile.local.foo
# (where "foo" is the OS name) or by a Makefile.local.bar_foo where bar is
# and architecture name.
#

# Where to install amd
ETC = /usr/local/etc

# Where to install man pages
MANDIR = $(DESTDIR)/usr/local/man
MANEXT = 8

# With what to install amd
INSTALL = install
INSTALL_BIN = ${INSTALL} -c -m 711 -o root

# Uncomment the next CC line if you want to use GNU CC
# Better yet - put the definition in Makefile.local.<foo>
#CC = gcc ${GCCOPTS}
GCCOPTS = -W -Wunused -fstrength-reduce #-finline-functions -fcombine-regs

# This option is needed for wire.c if you are using gcc 2.x.x and did not
# run the fixincludes script (it mainly fixes definitions for _IOWR macros.)
# Since the distributed gcc binaries from Cygnus (for Solaris) did not
# include fixed headers, you need to set this to -traditional-cpp
FIXINC_OPT =

# Basic C compiler options
CCOPTS = -O

# These are for testing/debugging...
# Best to put your own definitions in Makefile.local.<foo>
#CCOPTS =  -g
# Turn on -DDEBUG if you want a running commentary
#DEBUG = -DDEBUG

# Define RPCINC if Sun RPC header files are not in the standard place
RPCINC = #-I../../rpc

# Not currently used but...
# Define RPCGEN as the name of your Sun *RPC/4* RPCGEN program (not RPC/3)
RPCGEN = rpcgen

# System C Compiler - one that is FULLY call compatible with your C libraries
SYSCC = cc
SYSCCFLAGS = ${CFLAGS}

# For old makes
SHELL = /bin/sh

# -------- YOU SHOULD NOT NEED TO CHANGE ANYTHING BELOW THIS LINE --------

# Magic
OS_HDR = os-${OS}.h
OSDEF = -DOS_HDR=\"${OS_HDR}\"
CFLAGS = ${CCOPTS} ${DEBUG} ${OSDEF} -I../rpcx -I../config -I../include -I.

# Basename of the program we are trying to build
AMD = amd
AMQ = amq
MKMAP = mk-amd-map

CC_COMPILE = ${CC} -c ${CFLAGS} ${RPCINC} ${CONFIG}
SYSCC_COMPILE = ${SYSCC} -c ${SYSCCFLAGS} ${RPCINC} ${CONFIG}

#
# Keeps sysV make happy:
#
#VPATH = ..
