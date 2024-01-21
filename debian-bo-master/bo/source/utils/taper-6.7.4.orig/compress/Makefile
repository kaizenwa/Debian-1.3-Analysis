#
#   Time-stamp: <96/07/19 20:16:01 yusuf>
#
#   $Id: Makefile,v 1.4 1996/07/27 20:42:27 yusuf Exp $

#All the user config stuff, including directories etc..
#is found in Makefile.common
#
#
include ../version
include ../Makefile.common


all: $(COMPRESS_LIB)


$(COMPRESS_LIB): $(COMPRESS_OBJS)
	ar rc $(COMPRESS_LIB) $(COMPRESS_OBJS)
	ranlib $(COMPRESS_LIB)


.c.o:
	$(CC) $(CFLAGS) $(CFLAGS_NOLNK) $(DEFINES) $(INCLUDE) -o $@ -c $<

clean:
	rm -rf core *.o *~ $(COMPRESS_LIB) .depend

.depend:
	make depend

depend:
	$(GCC) -M $(INCLUDE) $(COMPRESS_SRCS) > .depend


dep: depend

ifeq (.depend, $(wildcard .depend))
include .depend
endif
#
#
