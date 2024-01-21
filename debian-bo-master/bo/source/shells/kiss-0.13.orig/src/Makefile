# Makefile for KISS

# The destination directory for a "make [rbg]install"
BINDIR=/usr/local/bin

# Your favorite C compiler:
CC = gcc

# Your installer, for "make (s)install": install or cp
INSTALL = install

# Default compile flags:
STDCFLAGS = -c -O2

# Warning level of compilation: I suggest -Wall
WARNFLAG = -Wall

# Additional compile flags for readline usage:
# You need to define USE_READLINE and to specifify the include dir,
# if you don't have readline under /usr/include. I have readline under
# /usr/local/include/readline.. If you don't want to compile with readline
# at all, just leave this out.
READLINECFLAGS =  -DUSE_READLINE -I/usr/local/include

# Additional compile flags for getline usage: Even if you don't have the
# readline libs (see above) and/or headers, I suggest you try out getline -
# it's a small readline-look-alike as far as command line editing goes.
# It doesn't support file completion though (it has hooks for it.. any
# potential heros out there?). The full sources for getline are under
# the getline/ subdir under this directory.
GETLINECFLAGS =  -DUSE_GETLINE -Igetline

# Additional compile flag for the bare version:
BARECFLAGS = -DUSE_BARE

# Default linkage flags: -s gives stripping. Always included, in all versions.
STDLFLAGS = -s

# Additional flags for readline: I have libreadline.a in /usr/local/lib,
# and readline requires libtermcap.a. If you don't want to compile with
# readline at all, just leave this out. Only included in "make rkiss".
READLINELFLAGS = -L/usr/local/lib -lreadline -ltermcap

# Additional linkage flags for getline: libgetline.a gets built under
# the subdir getline. Only included in "make gkiss".
GETLINELFLAGS = -Lgetline -lgetline

# Thazzal! The rest should be ok.
#-------------------------------

# objects that we're using
OBJ = kiss.o parser.tab.o lex.yy.o yyerror.o setstring.o setquotedstring.o \
      addstring.o clearstack.o setvariable.o warning.o getprogname.o \
      dumpstack.o error.o runcmd.o runchild.o setexpandedstring.o \
      sighandler.o getprompt.o banner.o redirected.o dokiss.o onechild.o \
      launch.o docat.o catfile.o domore.o morefile.o dohelp.o docp.o \
      copyfiletodir.o copyfiletofile.o copydirtodir.o runinternal.o docd.o \
      doquit.o getinput.o doexec.o dohistory.o addtohistory.o dorecall.o \
      islink.o getbasename.o setshlvl.o doprintenv.o dosetenv.o doecho.o \
      dounsetenv.o addtoenv.o dover.o domv.o movefiletodir.o \
      movefiletofile.o dowhere.o dorm.o dormdir.o removefile.o \
      recursiveremove.o domkdir.o dopwd.o dochgrp.o dochmod.o doln.o \
      doalias.o expandalias.o reexpand.o expandvars.o xmalloc.o xrealloc.o \
      xstrdup.o command.o waitforchild.o file2file.o doread.o dosource.o \
      sourcefile.o startupfiles.o isinternal.o isalias.o copystringstack.o \
      addstringstack.o expandtilde.o splitcmd.o addstringtostack.o \
      dokill.o dogrep.o dochown.o dosleep.o expandbackquotes.o dols.o \
      listdir.o listfile.o listoutput.o domknod.o dowc.o domount.o \
      doumount.o dotouch.o

# entry point for making
foo:
	@echo "Make what? Choose:"
	@echo "    make rkiss - make program in this dir, with readline"
	@echo "    make gkiss - make program in this dir, with getline"
	@echo "    make bkiss - make program in this dir, without editing"
	@echo "    make all - make readline, getline and bare versions"
	@echo "    make rinstall - \"make rkiss\" and install to" $(BINDIR)
	@echo "    make ginstall - \"make gkiss\" and install to" $(BINDIR)
	@echo "    make binstall - \"make bkiss\" and install to" $(BINDIR)
	@echo "    make clean - clean up stale files"
	exit 1

# how to make readline version
rkiss: always
	touch lexer
	make "CFLAGS=$(STDCFLAGS) $(READLINECFLAGS)" $(OBJ)
	$(CC) -o rkiss.new $(OBJ) $(READLINELFLAGS) $(STDLFLAGS)
	mv rkiss.new rkiss

# and install it
rinstall: rkiss
	$(INSTALL) rkiss $(BINDIR)

# how to make getline version
gkiss: always
	make -C getline libgetline.a
	touch lexer
	make "CFLAGS=$(STDCFLAGS) $(GETLINECFLAGS)" $(OBJ)
	$(CC) -o gkiss.new $(OBJ) $(GETLINELFLAGS) $(STDLFLAGS)
	mv gkiss.new gkiss

# and install it
ginstall: gkiss
	$(INSTALL) gkiss $(BINDIR)

# how to make bare version
bkiss: always
	touch lexer
	make "CFLAGS=$(STDCFLAGS) $(BARECFLAGS)" $(OBJ)
	$(CC) -o bkiss.new $(OBJ) $(STDLFLAGS)
	mv bkiss.new bkiss

# and install it
binstall: bkiss
	$(INSTALL) bkiss $(BINDIR)

# how to make both versions:
all:
	make rkiss
	make gkiss
	make bkiss

always:
	touch TimeStampOfCompilation

# extra: banner display needs version.h and subversion.h
banner.o: banner.c version.h subversion.h

# extra: "ver" command needs version.h and subversion.h
dover.o: dover.c version.h subversion.h

# how to make a new parser
parser.tab.c: parser
	bison -d parser

# how to make a new lexer
lex.yy.c: lexer parser.tab.h
	flex -I lexer

# suppress warnings on lexer
lex.yy.o: lex.yy.c
	@echo "Ignore any warnings in the lexer compilation"
	$(CC) $(CFLAGS) lex.yy.c

# suppress warnings on parser
parser.tab.o: parser.tab.c
	@echo "Ignore any warnings in the parser compilation"
	$(CC) $(CFLAGS) parser.tab.c

# cleanup: remove objects and intermediate files, but leave program
clean:
	rm -f $(OBJ) lex.yy.c parser.tab.c parser.tab.h a.out core \
	    TimeStampOfCompilation
	make -C getline clean

# how to get from *.c to *.o
.c.o:
	$(CC) $(CFLAGS) $(WARNFLAG) $<
