#
# Makefile
#
# This is the Makefile for the pvftools.
#
# $Id: Makefile,v 1.13 1997/02/20 06:52:01 marc Exp $
#

VOICELIBS=$(LIBPVF) $(LIBUTIL)

all: $(PVFTOOLS)

install:

clean:
	@$(RM) -f $(PVFTOOLS) *.o

fullclean: clean

%.o : %.c depend Makefile
	$(CC) $(CFLAGS) -DMAIN -c $<

$(PVFTOOLS) : % : %.o $(VOICELIBS)
	$(CC) $(LDFLAGS) -o $@ $^ -lm $(VOICELIBS) $(LIBS) 

include depend
