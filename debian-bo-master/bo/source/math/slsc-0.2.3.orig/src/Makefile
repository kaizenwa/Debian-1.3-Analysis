# Generated automatically from Makefile.in by configure.
# Unix Makefile for most                               -*- sh -*-
#---------------------------------------------------------------------------
# 1. Choose a C compiler.  It must understand prototypes.
#-----------------------------------------------------------------------------
CC = gcc -g
CFLAGS = -Wall -Wmissing-prototypes -Wnested-externs -Wpointer-arith -Wcast-align -Wshadow -Wstrict-prototypes -fno-strength-reduce
#  Note: gcc -O2 is buggy under Linux.  If you use it, include the
#        -fno-strength-reduce flag!!
#-----------------------------------------------------------------------------
# 2. On a few systems, additional libraries are required.
#-----------------------------------------------------------------------------
MISCLIBS = 
#MISCLIBS = -lnsl -lsocket                             # Solaris
#MISCLIBS = -ltermcap                                  # NeXT

#-----------------------------------------------------------------------------
# 3. Directory where the slang library, slanglib.a, is located.
#    BUILD SLANG FIRST!!!!  If you do not have it, get it from the same place
#    you picked up most.  The current home is space.mit.edu:/pub/davis/slang.
#-----------------------------------------------------------------------------
#Location of slang.h
SLANGINC = /home/john/src/slang/src#
#Location of libslang.a
SLANGLIB = /home/john/src/slang/src/objs#
#SLANGLIB = ../../slang/$(ARCH)elfobjs#    Location of shared elf library
#SLANGINC = ../../slang/src#               Location of slang.h
#SLANGLIB = $(SLANGINC)/$(ARCH)objs#       Location of libslang.a


#-----------------------------------------------------------------------------
# 4. Location where object files are placed (Absolute pathnames!!)
#-----------------------------------------------------------------------------
OBJDIR = /home/john/src/slsc/src/objs
SRCDIR = /home/john/src/slsc/src
BINDIR = ${exec_prefix}/bin
#---------------------------------------------------------------------------
# SLSC Install Dirs
#---------------------------------------------------------------------------
# Root directory where slsc files are going to be kept. 
# MAKE SURE THAT THE '#'IMMEDIATELY FOLLOWS THE PATH.  THERE CAN BE NO 
# WHITESPACE HERE!  You might want to use the second form.
SLSC_ROOT = /usr/local/lib/slsc#  <--- Note location of the `#' !!!
SLSC_BIN = /usr/local/bin
#---------------------------------------------------------------------------
# 5. Misc
#---------------------------------------------------------------------------
YACC = bison -y

#----------------------------------------------------------------------------
# Nothing below here should require changing.
#----------------------------------------------------------------------------
SHELL = /bin/sh
OBJS =  $(OBJDIR)/sc.o $(OBJDIR)/interp.o $(OBJDIR)/cmds.o $(OBJDIR)/crypt.o $(OBJDIR)/range.o $(OBJDIR)/scdokey.o $(OBJDIR)/schelp.o $(OBJDIR)/lex.o $(OBJDIR)/gram.o
OFILES =  sc.o interp.o cmds.o crypt.o range.o scdokey.o schelp.o lex.o gram.o

EXEC = slsc
CONFIG_H = config.h
ALL_CFLAGS = $(CFLAGS) -Dunix -DSLSC_ROOT='"$(SLSC_ROOT)"' -I$(SLANGINC) $(MISCINCLUDES)
EXECLIBS = -L$(SLANGLIB) -lslang $(MISCLIBS) -lm
EXECINC =

#
COMPILE_CMD = $(CC) -c $(ALL_CFLAGS)
#
all: $(EXEC) vprint
$(EXEC): $(OBJDIR) $(CONFIG_H) $(OBJDIR)/$(EXEC)
	@echo $(EXEC) created in $(OBJDIR)
$(OBJDIR)/$(EXEC): $(OBJS)
	cd $(OBJDIR); $(CC) $(OFILES) -o $(EXEC) $(EXECLIBS)
#
$(OBJDIR):
	-mkdir $(OBJDIR)
#
$(CONFIG_H) : $(SRCDIR)/sysconf.h
	/bin/cp $(SRCDIR)/sysconf.h $(CONFIG_H)
clean:
	/bin/rm -f $(OBJDIR)/* *~ $(SRCDIR)/gram.c

distclean: clean
	/bin/rm -rf $(OBJDIR) Makefile sysconf.h
install: all
	if [ ! -d $(SLSC_ROOT) ] ; then mkdir $(SLSC_ROOT) ; fi
	if [ ! -d $(SLSC_BIN) ] ; then mkdir $(SLSC_BIN) ; fi
	chmod +rwx $(SLSC_ROOT) $(SLSC_BIN)
	cp $(OBJDIR)/slsc $(SLSC_BIN)
	cp $(OBJDIR)/vprint slsc.hlp slsc.rc tutorial.sc $(SLSC_ROOT)
#---------------------------------------------------------------------------
$(SRCDIR)/gram.c: $(SRCDIR)/gram.y
	cd $(SRCDIR); $(YACC) -d gram.y; mv y.tab.c gram.c
# There appears to be a bug in the GNU cpp.  I need -I$(SRCDIR).
$(OBJDIR)/gram.o: $(SRCDIR)/gram.c
	cd $(OBJDIR); $(COMPILE_CMD) -I$(SRCDIR) $(SRCDIR)/gram.c
$(OBJDIR)/lex.o: $(SRCDIR)/lex.c $(SRCDIR)/y.tab.h $(OBJDIR)/gram.o
	cd $(OBJDIR); $(COMPILE_CMD) $(SRCDIR)/lex.c
$(SRCDIR)/lex.c : $(SRCDIR)/statres.h
$(SRCDIR)/statres.h : $(SRCDIR)/gram.y
	cd $(SRCDIR); sed<gram.y >experres.h -f eres.sed;sed < gram.y > statres.h -f sres.sed
$(SRCDIR)/y.tab.h : $(SRCDIR)/gram.y

#---------------------------------------------------------------------------
# Rules (automatically generated)
#---------------------------------------------------------------------------

$(OBJDIR)/sc.o : $(SRCDIR)/sc.c
	cd $(OBJDIR); $(COMPILE_CMD) $(SRCDIR)/sc.c

$(OBJDIR)/interp.o : $(SRCDIR)/interp.c
	cd $(OBJDIR); $(COMPILE_CMD) $(SRCDIR)/interp.c

$(OBJDIR)/cmds.o : $(SRCDIR)/cmds.c
	cd $(OBJDIR); $(COMPILE_CMD) $(SRCDIR)/cmds.c

$(OBJDIR)/crypt.o : $(SRCDIR)/crypt.c
	cd $(OBJDIR); $(COMPILE_CMD) $(SRCDIR)/crypt.c

$(OBJDIR)/range.o : $(SRCDIR)/range.c
	cd $(OBJDIR); $(COMPILE_CMD) $(SRCDIR)/range.c

$(OBJDIR)/scdokey.o : $(SRCDIR)/scdokey.c
	cd $(OBJDIR); $(COMPILE_CMD) $(SRCDIR)/scdokey.c

$(OBJDIR)/schelp.o : $(SRCDIR)/schelp.c
	cd $(OBJDIR); $(COMPILE_CMD) $(SRCDIR)/schelp.c

vprint : $(OBJDIR)/vprint
	@echo vprint created in $(OBJDIR)
$(OBJDIR)/vprint : $(OBJDIR)/vprint.o $(EXECDEPS)
	$(CC) -o $(OBJDIR)/vprint $(OBJDIR)/vprint.o $(EXECLIBS)
$(OBJDIR)/vprint.o : $(SRCDIR)/vprint.c
	cd $(OBJDIR); $(COMPILE_CMD) $(EXECINC) $(SRCDIR)/vprint.c

