#-----------------------------------------------------------------------------
#                               EXAMPLES  Makefile
#-----------------------------------------------------------------------------
#
# Do not modify this file.  If you want to add a new example, modify the file
# Makefile.dep.
#-----------------------------------------------------------------------------

# the C compiler
CC = gcc

# the Ada Compiler
ADAC = $(CC)

# the C++ compiler
CPLUSPLUS = gcc

# Gnat1 compilation flags
GF = -O2

# Gnatbind binder flags
BF =

# Extension for ".o" files
o = o

# Extension for executable files
e =


#-----------------------------------------------------------------------------
# Main rule

main_rule : process

include Makefile.dep

process : clean $(LIST_EXEC) announce

###############################################################################
# General rules
###############################################################################
.SUFFIXES: .adb .ads .ali .o
.PHONY: clean process announce force

.adb.o:
	$(ADAC) -c $(GF)  $<

.ads.o:
	$(ADAC) -c $(GF)  $<

.c.o:
	$(CC) -c $(CFLAGS) $<

% : %.o
	gnatbl $@.ali $($@_DEP)
	./$@

clean : force
	@/bin/rm -f *.$o *.ali b_*.c *.s *~ $(LIST_EXEC)

announce :
	@echo " "
	@echo "ANNOUNCES:"
	@echo "   * if you are interested by the C++ interface: try 'make ex6_main'"
	@echo " "

force :

##############################################################################
# special stuff for the c++ interface
##############################################################################

ex6.o :
	@echo -n "if g++ is not installed on your standard 'gcc', the next "
	@echo "command will fail"
	$(CPLUSPLUS) -c ex6.C


ex6_main$e: ex6_main.$o ex6_if.$o $(ex6_main_DEP)
	gnatbl ex6_main.ali ex6.$o -lstdc++
	./ex6_main$e

##############################################################################
# special exec for test_cl
##############################################################################
test_cl$e: test_cl.$o
	gnatbl test_cl.ali
	@echo "example kindly sent by Timothy J. Halloran"
	./test_cl arg1 arg2 arg3
	./test_cl a bb cccccc   d eeeeeee ff
