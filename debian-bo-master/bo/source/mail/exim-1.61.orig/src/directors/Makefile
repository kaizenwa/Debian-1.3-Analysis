# Make file for building a library containing all the available directors and
# calling it directors.a. This is called from the main make file, after cd'ing
# to the directors subdirectory.

OBJ = aliasfile.o forwardfile.o localuser.o smartuser.o

directors.a:     $(OBJ)
		 /bin/rm -f directors.a
		 ar cq directors.a $(OBJ)
		 $(RANLIB) $@

.SUFFIXES:       .o .c
.c.o:;           $(CC) -c $(CFLAGS) $(INCLUDE) $*.c

aliasfile.o:     $(HDRS) aliasfile.c   aliasfile.h
forwardfile.o:   $(HDRS) forwardfile.c forwardfile.h ../config.h
localuser.o:     $(HDRS) localuser.c   localuser.h
smartuser.o:     $(HDRS) smartuser.c   smartuser.h

# End
