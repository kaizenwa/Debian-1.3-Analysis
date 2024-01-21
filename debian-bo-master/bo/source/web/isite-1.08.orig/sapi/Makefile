#
# CNIDR Search API
#
# This makefile results in:
#
#	libsapi.a	- Common API to various search/database systems
#
#  To use this library in an application, you must link with libcnidr.a
#  as well as any libraries specific to the database systems you've chosen
#  to compile in.
#
#  Compile only those search engines you wish (are able) to support
#
SHELL=/bin/sh

#
# Compiler
#
CC=g++

# 
# Compiler Flags
#
CFLAGS=-g -Wall -DUNIX

#
# Borrowed from GNU gcc distribution
#
MYRANLIB = ranlib
MYRANLIB_TEST = [ -f /usr/bin/ranlib -o -f /bin/ranlib ]

#
# Library Directory
#
# Where should I store the library?
#
LIB_DIR=../bin

#
# libz3950
#
# Where are the libz3950 include files?
#
ZDIST_DIR=../zdist

#
# Search Engines
#
#DBASE_CHOICE - Definitions from list of supported engines.
#
# 	Current choices: -DSAPI_ISEARCH
#
#DBASE_OBJ - Any object files that need to be linked into libsapi
#DBASE_INC - Location of include files for all database engines
#DBASE_CFLAGS - Any compiler flags required by particular database engines
#
DBASE_CHOICE=
DBASE_OBJ=
DBASE_INC=
DBASE_CFLAGS=

#
# Isearch
#
# Where is the Isearch distribution?
#
ISEARCH_DIR=../Isearch-1.10.isite
ISEARCH_INC=-I$(ISEARCH_DIR)/src
ISEARCH_CFLAGS=

#
# Script Search Engine
#
SCRIPT_OBJ=script.o
SCRIPT_INC=

#
# PGIF (Postgres interface)
#
# Where are the PGIF files?
#
PGIF_DIR=../pgif
PGIF_OBJ=$(PGIF_DIR)/pgif.o
PGIF_INC=-I$(PGIF_DIR)
#
# Where is the Postgres distribution? (not PGIF)
#
POSTGRES_DIR=/usr/users/postgres
#

#
# That should be all you need to configure
#
H=sapi.h ini.h list.h strbox.h script.h
LIB=$(LIB_DIR)/libsapi.a
AR=ar
ARFLAGS=r
RM=rm -f

all:$(LIB)

imem.o:$(H) imem.c
	$(CC) $(CFLAGS) $(ISEARCH_INC) -c imem.c

script.o:$(H) script.c
	$(CC) $(CFLAGS) $(ISEARCH_INC) -c script.c

list.o:$(H) list.c
	$(CC) $(CFLAGS) -c list.c
 
ini.o:$(H) ini.c
	$(CC) $(CFLAGS) $(ISEARCH_INC) -c ini.c
 
strbox.o:$(H) strbox.c
	$(CC) $(CFLAGS) $(ISEARCH_INC) -c strbox.c
 
sapi.o:$(H) sapi.c
	$(CC) $(CFLAGS) $(DBASE_CHOICE) $(DBASE_CFLAGS) $(DBASE_INC) \
	$(ISEARCH_INC) -I$(ZDIST_DIR) -c sapi.c

$(LIB):$(DBASE_OBJ) sapi.o list.o script.o ini.o strbox.o imem.o
	$(AR) $(ARFLAGS) $(LIB) imem.o strbox.o list.o ini.o script.o sapi.o $(DBASE_OBJ)
	`if $(MYRANLIB_TEST) ; then $(MYRANLIB) $(LIB); else true; fi`

clean:
	$(RM) *~ *.o $(LIB) core

