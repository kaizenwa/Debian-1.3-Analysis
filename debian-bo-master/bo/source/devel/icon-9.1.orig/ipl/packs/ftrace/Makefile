MAKE=make
SHELL=/bin/sh

ICONT=icont
IFLAGS=-us
ITRAN = $(ICONT) $(IFLAGS)

ftrace.u2:	ftrace.icn		
		$(ITRAN) -c ftrace

ftrace.icn: 	gftrace.icn 
		$(ITRAN) gftrace -x >ftrace.icn
		rm -f gftrace

clean Clean:		
		rm -f gftrace ftrace ftrace.icn *.u?
