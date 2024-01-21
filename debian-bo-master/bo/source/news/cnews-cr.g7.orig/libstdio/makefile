MORECFLAGS='-DPTR_TYPE=unsigned char *'
# =()<@<INCLUDE>@>()=
.include "../include/config.make"

OBJS=fgets.o fputs.o rdwr.o
ALL=$(OBJS)

u:	$(ALL)
	$(UPDATE) $(OBJS)

maybe:
	@-if $(MAKE) r ; \
	then $(MAKE) u ; \
	else \
	echo ; \
	echo ; \
	echo "Don't panic."'  (That "Stop." you just saw is not an order!)' ; \
	echo 'It would appear that you cannot use our stdio speedups due' ; \
	echo 'to system incompatibility.  They may be superfluous; systems' ; \
	echo 'modern enough to be incompatible often have a fast stdio.' ; \
	echo 'In any case, C News runs properly without the speedups.' ; \
	fi

o:	$(OBJS)

cmp:	$(ALL)
	$(LIBCMP) $(OBJS)

rsetup:	$(OBJS) stdiock.stock stdiock.fast runtrials
	$(MX) stdiock.stock stdiock.fast runtrials

r:	rclean rsetup
	: run first with stdout directed to a plain file
	./runtrials >junk
	: and again with stdout directed to a tty, we hope.
	: do not panic.  you can ignore the output of runtrials.
	: only its exit status really matters.
	./runtrials

stdiock.stock:	stdiock.o
	$(CC) $(LDFLAGS) stdiock.o -o $@

stdiock.fast:	stdiock.o $(OBJS)
	$(CC) $(LDFLAGS) stdiock.o $(OBJS) -o $@

rclean:
	rm -f stdiock stdiock.fast stdiock.stock junk

clean:	rclean
	rm -f *.o junk *mon.out
