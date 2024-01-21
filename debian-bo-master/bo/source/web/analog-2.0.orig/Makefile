# Makefile for analog 2.0
# Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/
CC = gcc           # which compiler to use: eg cc, acc, gcc. NB Different
#                    compilers need different CFLAGS, e.g., -O instead of -O2.
CFLAGS = -O2       # options, e.g. for optimisation or ANSI compilation.
DEFS =             # any of -DNOPIPES -DNODNS -DNODIRENT
OS = UNIX          # Operating system: UNIX, DOS, WIN32, MAC or VMS
LIBS = -lm         # extra libraries needed; Solaris 2 (SunOS 5) needs
#                    LIBS = -lnsl -lm and SCO Unix needs LIBS = -lsocket -lm
#                    IRIX reportedly needs LIBS = -lc -lm
PAGER = more       # Your favourite pager program, e.g. more

PROGRAM = analog
SOURCES = alias.c analog.c formgen.c hash.c init.c init2.c macstuff.c \
	output.c output2.c output3.c sscanf.c utils.c
OBJS = alias.o analog.o formgen.o hash.o init.o init2.o macstuff.o \
	output.o output2.o output3.o sscanf.o utils.o
HEADERS = analhead.h analhea2.h

# Form interface options:
FORMPROG = analform.cgi     # The program that processes the data from the form
FORMSRC = analform.c        # The source code for that program
FORMHTML = analogform.html  # Where the form itself will go
FORMOPTS =                  # Extra analog options when constructing the form

$(PROGRAM): $(OBJS) $(HEADERS) Makefile
	$(CC) $(CFLAGS) $(OBJS) -o $(PROGRAM) $(LIBS)
	@echo '***'
	@echo '***IMPORTANT: You must read the licence before using analog'
	@echo '***'

$(OBJS): $(HEADERS) Makefile
	$(CC) $(CFLAGS) $(DEFS) -D$(OS) -c $*.c

$(FORMPROG): $(FORMSRC) Makefile
	$(CC) $(CFLAGS) $(FORMSRC) -o $(FORMPROG)

$(FORMHTML): $(PROGRAM) Makefile
	./$(PROGRAM) -form $(FORMOPTS) +O$(FORMHTML)

form: $(FORMHTML) $(FORMPROG)

force:
	rm -f $(OBJS)
	make $(PROGRAM) 'CFLAGS=$(CFLAGS)' 'LIBS=$(LIBS)' 'DEFS=$(DEFS)' \
	CC=$(CC)

clean:
	rm -f *.o analog
