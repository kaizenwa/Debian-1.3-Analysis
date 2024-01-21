#
# CNIDR Isite
#
# Make sure these settings are correct for you platform.  In particular,
# check the CFLAGS value.
#
# This makefile results in:
#
#	libsapi.a	- CNIDR search api
#	libz3950.a	- Z39.50 version 2/3 library
#	libIsearch.a	- CNIDR Isearch library
#	izclient	- Simple, interactive Z39.50 client
#	zserver		- Z39.50 server
#	zcon		- Z39.50 stateful gateway utility
#	zgate		- Z39.50 stateful gateway utility
#	Iindex		- Isearch Text indexer
#	Isearch		- Search tool for Iindexed databases
#	Iutil		- Tool for manipulating Iindexed databases
#
SHELL=/bin/sh

#
# Compiler?
#
# Must be a c++ compiler
#
CC=g++

#
# Compiler flags?
#
# Select your platform and comment/uncomment the appropriate CFLAGS
# 
# Solaris, SunOS, Ultrix, OSF, Linux
#
#CFLAGS=-g -W -m486 -DUNIX
CFLAGS=-O2 -DUNIX

#
# HP
#
#CFLAGS=-O2 -DUNIX -D_HP_ -DNO_MMAP

#
# Binary directory
#
# Where should I store binaries?
#
BIN_DIR=bin

#
# STOP!!
#
# For the default Isite operation, that should be all you need to change!
# Save this file and type 'make'
#

#
# Search Engines
#
# Isite comes with the following engines precompiled into the Search API:
#
#	SAPI_SCRIPT	- simple shell script search engine
#	SAPI_ISEARCH	- CNIDR Isearch engine.
#
# To link in other databases, refer to the Search API (sapi) documentation.
#

#
# Ranlib
#
# Borrowed from GNU gcc distribution
#
MYRANLIB=ranlib
MYRANLIB_TEST= [ -f /usr/bin/ranlib -o -f /bin/ranlib ]

#
# Extra libraries?
#
# On System V machines (Solaris), you must link with nsl and socket
#
EXTRA_LIBS=-ldl -lnsl -lsocket
EXTRA_LIBS_TEST= [ -f /usr/lib/libnsl.a -a -f /usr/lib/libsocket.a ]

#
# Library directory
#
# Where should I store libraries (*.a)?
#
LIB_DIR=$(BIN_DIR)

#
# libz3950
#
# Where is libz3950 code?
#
ZDIST_DIR=zdist

#
# Isearch
#
# Where is Isearch code?
#
ISEARCH_DIR=Isearch

#
# Isearch-cgi
#
# Where is Isearch code?
#
CGI_DIR=$(ISEARCH_DIR)/Isearch-cgi

#
# Doctype-specific libs (e.g., -lgdbm)
#
DOCLIB=

#
# libsapi
#
# Where is libsapi code?
#
SAPI_DIR=sapi

#
# That should be all you need to change
#
#
VER=1.08
LIB=-lz3950 -lsapi -lIsearch -lm 
LDFLAGS=
RM=rm -f
DIST=Isite-$(VER)
all: isearch isearch-cgi libsapi zdist done
OSNAME=`uname -s`
OSVER=`uname -r`
OS=$(OSNAME)_$(OSVER)

isearch::
	@echo "*** ISEARCH ***"
	cd $(ISEARCH_DIR);make isearch "CC=$(CC)" \
	"CFLAGS=$(CFLAGS)" \
	"MYRANLIB=$(MYRANLIB)" \
	"MYRANLIB_TEST=$(MYRANLIB_TEST)" \
	"BIN_DIR=../$(BIN_DIR)" \
	"DOCLIB=$(DOCLIB)" "LDFLAGS=$(LDFLAGS)"

isearchb::
	@echo "*** ISEARCH ***"
	cd $(ISEARCH_DIR);make build "CC=$(CC)" \
	"CFLAGS=$(CFLAGS)" \
	"MYRANLIB=$(MYRANLIB)" \
	"MYRANLIB_TEST=$(MYRANLIB_TEST)" \
	"BIN_DIR=../$(BIN_DIR)" \
	"DOCLIB=$(DOCLIB)" "LDFLAGS=$(LDFLAGS)"

isearch-cgi::
	@echo "*** ISEARCH-CGI ***"
	cd $(ISEARCH_DIR);make isearch-cgi "CC=$(CC)" \
	"CFLAGS=$(CFLAGS)" \
	"MYRANLIB=$(MYRANLIB)" \
	"MYRANLIB_TEST=$(MYRANLIB_TEST)" \
	"BIN_DIR=../$(BIN_DIR)" \
	"ISEARCH_DIR=.." \
	"LIB_DIR=../$(BIN_DIR)" \
	"DOCLIB=$(DOCLIB)" "LDFLAGS=$(LDFLAGS)"

libsapi::
	@echo "*** LIBSAPI ***"
	cd $(SAPI_DIR);make "CC=$(CC)" \
	"CFLAGS=$(CFLAGS)" \
	"MYRANLIB=$(MYRANLIB)" \
	"MYRANLIB_TEST=$(MYRANLIB_TEST)" \
	"LIB_DIR=../$(LIB_DIR)" \
	"DBASE_OBJ=$(DBASE_OBJ)" \
	"ZDIST_DIR=../$(ZDIST_DIR)" \
	"ISEARCH_INC=-I../$(ISEARCH_DIR)/src -I../$(ISEARCH_DIR)/doctype"

zdist::
	@echo "*** ZDIST ***"
	cd $(ZDIST_DIR);make "CC=$(CC)" \
	"CFLAGS=$(CFLAGS)" \
	"LIB_DIR=../$(LIB_DIR)" \
	"EXTRA_LIBS=$(EXTRA_LIBS)" \
	"EXTRA_LIBS_TEST=$(EXTRA_LIBS_TEST)" \
	"BIN_DIR=../$(BIN_DIR)" \
	"MYRANLIB=$(MYRANLIB)" \
	"MYRANLIB_TEST=$(MYRANLIB_TEST)" \
	"ISEARCH_DIR=../$(ISEARCH_DIR)" \
	"SAPI_DIR=../$(SAPI_DIR)" \
	"LS=../$(SAPI_DIR)" \
	"DOCLIB=$(DOCLIB)" "LDFLAGS=$(LDFLAGS)"

build:
	make -i realclean
	cd $(ISEARCH_DIR);./configure;make "CC=$(CC)" \
	"CFLAGS=$(CFLAGS)" \
	"MYRANLIB=$(MYRANLIB)" \
	"MYRANLIB_TEST=$(MYRANLIB_TEST)" \
	"BIN_DIR=../$(BIN_DIR)" \
	"DOCLIB=$(DOCLIB)" "LDFLAGS=$(LDFLAGS)"
	make libsapi
	make zdist
	make isearch-cgi
	make done

srcdist:
	make -i distclean;cd ..;tar cvf $(DIST).tar $(DIST);gzip $(DIST).tar

bindist:
	cd ..; \
	tar cvf $(DIST)_$(OS)$(LDFLAGS).tar \
		$(DIST)/bin/Iindex \
		$(DIST)/bin/Isearch \
		$(DIST)/bin/Iutil \
		$(DIST)/bin/bib1.map \
		$(DIST)/bin/my.map \
		$(DIST)/bin/build_test_db.sh \
		$(DIST)/bin/izclient \
		$(DIST)/bin/sapi.ini \
		$(DIST)/bin/zclient \
		$(DIST)/bin/zcon \
		$(DIST)/bin/zgate \
		$(DIST)/bin/zserver \
		$(DIST)/bin/zserver.ini \
		$(DIST)/$(CGI_DIR)/Configure \
		$(DIST)/$(CGI_DIR)/README \
		$(DIST)/$(CGI_DIR)/isrch_fetch \
		$(DIST)/$(CGI_DIR)/isrch_srch \
		$(DIST)/$(CGI_DIR)/search_form \
		$(DIST)/doc \
		$(DIST)/README \
		$(DIST)/RELEASE \
		$(DIST)/html  \
		$(DIST)/COPYRIGHT; \
		gzip $(DIST)_$(OS)$(LDFLAGS).tar

pcdist:
	@echo "Creating directory 'PC'..."
	@mkdir PC;
	@echo "Copying all source needed for PC version..."
	@cp $(ISEARCH_DIR)/src/*.[c,h] PC/.;
	@cp $(ISEARCH_DIR)/src/*.cxx PC/.;
	@cp $(ISEARCH_DIR)/src/*.hxx PC/.;
	@cp $(ZDIST_DIR)/*.[c,h] PC/.;
	@cp $(ZDIST_DIR)/*.cxx PC/.;
	@cp $(ZDIST_DIR)/*.hxx PC/.;
	@echo "Done.  You're on your own;-)"

done:
	@echo ""
	@echo "Welcome to CNIDR $(DIST)!"
	@echo ""
	@echo "All executables have been placed in the $(BIN_DIR) directory."
	@echo ""
	@echo "Please read all docs (doc/*) for usage instructions."
	@echo ""

clean:
	cd $(ISEARCH_DIR);make -i clean
	cd $(SAPI_DIR);make -i clean
	cd $(ZDIST_DIR);make -i clean
	cd $(CGI_DIR);make -i clean
	cd bin;$(RM) *~ *.a zclient zserver Iindex Isearch Iutil \
		TESTHTML* core *.log izclient zcon zgate
	cd html;$(RM) *~
	cd doc;$(RM) *~
	$(RM) *~ core

realclean:
	cd $(ISEARCH_DIR);make -i realclean
	cd $(SAPI_DIR);make -i clean
	cd $(ZDIST_DIR);make -i clean
	cd $(CGI_DIR);make -i clean
	cd bin;$(RM) *~ *.a zclient zserver Iindex Isearch Iutil \
		TESTHTML* core *.log izclient zcon zgate
	cd html;$(RM) *~
	cd doc;$(RM) *~
	$(RM) *~ core

distclean:
	cd $(ISEARCH_DIR);make -i distclean
	cd $(SAPI_DIR);make -i clean
	cd $(ZDIST_DIR);make -i clean
	cd $(CGI_DIR);make -i clean
	cd bin;$(RM) *~ *.a zclient zserver Iindex Isearch Iutil \
		TESTHTML* core *.log izclient zcon zgate
	cd html;$(RM) *~
	cd doc;$(RM) *~
	$(RM) *~ core

binclean:
	cd bin;$(RM) *~ *.a zclient zserver Iindex Isearch Iutil \
		TESTHTML* core *.log izclient zcon zgate
	cd $(CGI_DIR);$(RM) isrch_fetch isrch_srch search_form

