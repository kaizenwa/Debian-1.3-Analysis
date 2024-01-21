SHELL=/bin/sh
MAKE=make
name=unspecified

help:
		@echo "See the Icon installation documents."
		@echo "Copies are in the doc directory here.

##################################################################
#
# Code configuration.

#
# Configure the code for a specific system.
#

Configure:
		make Clean
		echo '#define BinPath' \"`pwd`/bin/\" >src/h/path.h
		cat config/unix/$(name)/vtran.hdr \
		config/unix/Config/vt3.make >config/unix/Config/Makefile
		cp config/unix/Common/Makefile config/unix/$(name)
		cd config/unix/$(name);	$(MAKE) 
		rm -f config/unix/$(name)/Makefile

X-Configure:
		make Clean
		echo '#define BinPath' \"`pwd`/bin/\" >src/h/path.h
		cat config/unix/$(name)/vtran.hdr \
		config/unix/Config/vt3.make >config/unix/Config/Makefile
		cp config/unix/Common/Makefile config/unix/$(name)
		cd config/unix/$(name);	$(MAKE) X-Icon
		rm -f config/unix/$(name)/Makefile

#
# Check to see what systems have configuration information.
#

Supported:
		@echo "There is configuration information for"
		@echo " the following systems:"
		@echo ""
		@cd config/unix;		ls -d [a-z]*

#
# Get the status information for a specific system.
#

Status:
		@cat config/unix/$(name)/status

#
# Build a prototype configuration for a new system.
#

Platform:
		mkdir config/unix/$(name)
		cp config/unix/Common/* config/unix/$(name)

#
# Copy default header files.
#

Headers:
		cp config/unix/Common/*.hdr config/unix/$(name)

##################################################################
#
# Compilation and installation.
#

#
# The interpreter.
#

Icon:
		$(MAKE) Icon-icont

#
# The compiler: rtt, the run-time system, and iconc.
#

Icon-iconc:	Common
		cd src/runtime;		$(MAKE) comp_all
		cd src/iconc;		$(MAKE)
#
# The interpreter: icont and iconx.
#

Icon-icont:	Common
		cd src/icont;		$(MAKE)
		cd src/runtime;		$(MAKE) interp_all

#
# Common components.
#

Common:
		cd src/common;		$(MAKE)
		cd src/rtt;		$(MAKE)

##################################################################
#
# 
#

CopyLib:
		cp bin/dlrgint.o bin/rt.db bin/rt.h bin/*.a $(Target)
		-(test -f NoRanlib) || (ranlib $(Target)/*.a)

##################################################################
#
# Construction of auxiliary components.
#

#
# Icon program library.
#

Ipl:
		make Ipl-icont

Ipl-icont:
		cd ipl;			$(MAKE) Ipl-icont

Ipl-iconc:
		cd ipl;			$(MAKE) Ipl-iconc

##################################################################
#
# Tests.
#

#
# Some simple tests to be sure Icon works.
#

Samples:
		$(MAKE) Samples-icont

Samples-iconc:
		cd tests/samples;	$(MAKE) Samples-iconc

Samples-icont:
		cd tests/samples;	$(MAKE) Samples-icont

#
# More exhaustive tests of various features of Icon and larger programs.
#

#
# Basic tests. Should show only insignificant differences.
#

Test:
		$(MAKE) Test-icont

Test-iconc:
		cd tests/general;	$(MAKE) test-iconc

Test-icont:
		cd tests/general;	$(MAKE) test-icont

Test-opt:
		cd tests/general;	$(MAKE) test-opt

Test-noopt:
		cd tests/general;	$(MAKE) test-noopt

#
# Tests of co-expressions. Should not show differences if co-expressions
# are implemented.
#

Test-coexpr:
		$(MAKE) Test-coexpr-icont

Test-coexpr-iconc:
		cd tests/general;	$(MAKE) test-coexpr-iconc

Test-coexpr-icont:
		cd tests/general;	$(MAKE) test-coexpr-icont

Test-large:
		cd tests/general;	$(MAKE) test-large

#
# The rest of the tests check other components of the Icon system.
#

#
# Test the Icon program library.
#

Test-ipl:
		make Test-ipl-icont

Test-ipl-icont:
		cd tests/ipl;			$(MAKE) Test-ipl-icont

#
# Test the Icon variant translator system.
#

Test-vtran:
		cd tests/vtran;		$(MAKE) Test-vtran

#################################################################
#
# Run benchmarks.
#

Benchmark:
		$(MAKE) Benchmark-icont

Benchmark-iconc:
		cd tests/bench;		$(MAKE) benchmark-iconc

Benchmark-icont:
		cd tests/bench;		$(MAKE) benchmark-icont


##################################################################
#
# Clean-up.
#

Clean:
		-cd docs;		$(MAKE) Clean
		-cd ipl;		$(MAKE) Clean
		-cd src;		$(MAKE) Clean
		-cd tests;		$(MAKE) Clean

##################################################################
