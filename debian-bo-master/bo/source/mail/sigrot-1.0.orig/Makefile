DEST	      = /usr/local/bin

MANDEST	      = /usr/man/man1

MANFILE	      = sigrot.1

INSTALL	      = cp

LD	      = $(CC)

PROGRAM       = sigrot

SHELL	      = /bin/sh

SRCS	      = sigrot.c

all:		$(PROGRAM)

$(PROGRAM):     $(LIBS)
		@$(LD) $(SRCS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f core

clobber:;	@rm -f $(PROGRAM) core tags

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@-strip $(PROGRAM)
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(PROGRAM); $(INSTALL) $(PROGRAM) $(DEST)); fi
		@chmod 755 $(DEST)/$(PROGRAM)
		@echo Installing $(MANFILE) in $(MANDEST)
		@$(INSTALL) $(MANFILE) $(MANDEST)
		@chmod 644 $(MANDEST)/$(MANFILE)
