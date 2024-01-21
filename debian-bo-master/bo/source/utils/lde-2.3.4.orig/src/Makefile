#################################################################
# YOU SHOULD NOT EDIT THIS FILE, EDIT THE MAIN MAKEFILE INSTEAD #
#################################################################

SRCS = minix.c xiafs.c ext2fs.c no_fs.c recover.c \
       main_lde.c $(CURSES_SRC) tty_lde.c bitops.c msdos_fs.c

OBJS = minix.o xiafs.o ext2fs.o no_fs.o recover.o \
       main_lde.o $(CURSES_OBJ) tty_lde.o bitops.o msdos_fs.o \
       swiped.a

HEADERS = lde.h nc_lde.h bitops.h nc_block_help.h nc_inode_help.h \
	nc_lde_help.h no_fs.h xiafs.h curses.h minix.h nc_dir.h \
	nc_lde.h recover.h ext2fs.h nc_block.h nc_inode.h tty_lde.h \
	msdos_fs.h keymap.h

dummy:
	( cd .. ; make )

all:	$(PROG_NAME)

$(PROG_NAME):	../$(PROG_NAME)

../$(PROG_NAME):	.depend $(OBJS)
	$(CC) $(LDFLAGS) -o ../$(PROG_NAME) $(OBJS) $(LIBCURSES)

swiped.a:
	@(cd swiped; make -f Makefile all)

clean:
	rm -f *.o ChangeLog~
	@(cd swiped; make -f Makefile clean)

clear:	clean
	rm -f .depend $(PROG_NAME)
	@(cd swiped; make -f Makefile clear)

checkout:
	RCS/checkout_all

checkin:
	RCS/checkin_all

.depend:
	@echo Doing \"make depend\"
	@for i in *.c;do $(CPP) -M $$i;done >> .depend
	(cd swiped; make depend)

depend:
	@rm -f .depend
	@$(MAKE) .depend

dep:	depend

ifeq (.depend,$(wildcard .depend))
include .depend
endif

.c.o:	.depend
	$(CC) -c $(CFLAGS) -o $*.o $*.c


