#$Id: Makefile,v 11.39 96/03/30 16:05:03 al Exp $
#-----------------------------------------------------------------------------
debug:
	-mkdir O
	cat Make1 Make2.Debug Make3 Make.depend >O/Makefile
	(cd O; make -k)
#-----------------------------------------------------------------------------
# In some cases you can just type "make" to build it.
# Generally, you will want "make your-system".
# Look in this file for the choices.
# The special configurations listed here allow you to do multiple builds
# from the same source directory, and patch some problems.
#-----------------------------------------------------------------------------
# The most common configuration is g++
# This should work if it is properly installed
# and has the proper libraries and headers.
g++:
	-mkdir O-G++
	cat Make1 Make2.g++ Make3 Make.depend >O-G++/Makefile
	(cd O-G++; make -k)
#-----------------------------------------------------------------------------
# If you don't have g++, try this one
# This is based on assumptions that you have a cfront based compiler called CC
# and there is nothing strange about your system
CC:
	-mkdir O-CC
	cat Make1 Make2.CC Make3 Make.depend >O-CC/Makefile
	(cd O-CC; make -k)
#-----------------------------------------------------------------------------
next:
	-mkdir NEXT
	cat Make1 Make2.g++ Make3 Make.depend >NEXT/Makefile
	(cd NEXT; make -k)
#-----------------------------------------------------------------------------
osf-g++:
	-mkdir OSF-G++
	cat Make1 Make2.g++ Make3 Make.depend >OSF-G++/Makefile
	(cd OSF-G++; make -k)
#-----------------------------------------------------------------------------
ultrix-g++:
	-mkdir ULTRIX-G++
	cat Make1 Make2.g++ Make3 Make.depend >ULTRIX-G++/Makefile
	-(cd ULTRIX-G++; make -k)
#-----------------------------------------------------------------------------
sun3-g++:
	-mkdir SUN3-G++
	cat Make1 Make2.g++ Make3 Make.depend >SUN3-G++/Makefile
	(cd SUN3-G++; make -k)
#-----------------------------------------------------------------------------
sun4-g++:
	-mkdir SUN4-G++
	cat Make1 Make2.g++ Make3 Make.depend >SUN4-G++/Makefile
	(cd SUN4-G++; make -k)
#-----------------------------------------------------------------------------
sun4-CC:
	-mkdir SUN4-CC
	cat Make1 Make2.sun-CC Make3 Make.depend >SUN4-CC/Makefile
	(cd SUN4-CC; make -k)
#-----------------------------------------------------------------------------
sgi-g++:
	-mkdir SGI-G++
	cat Make1 Make2.g++ Make3 Make.depend >SGI-G++/Makefile
	(cd SGI-G++; make -k)
#-----------------------------------------------------------------------------
sgi-NCC:
	-mkdir SGI-NCC
	cat Make1 Make2.sgi-NCC Make3 Make.depend >SGI-NCC/Makefile
	(cd SGI-NCC; make -k)
#-----------------------------------------------------------------------------
sgi-DCC:
	-mkdir SGI-DCC
	cat Make1 Make2.sgi-DCC Make3 Make.depend >SGI-DCC/Makefile
	(cd SGI-DCC; make -k)
#-----------------------------------------------------------------------------
hppa-g++:
	-mkdir HPPA-G++
	cat Make1 Make2.g++ Make3 Make.depend >HPPA-G++/Makefile
	(cd HPPA-G++; make -k)
#-----------------------------------------------------------------------------
linux:
	-mkdir LINUX
	cat Make1 Make2.g++ Make3 Make.depend >LINUX/Makefile
	(cd LINUX; make -k)
#-----------------------------------------------------------------------------
freebsd:
	-mkdir FreeBSD
	cat Make1 Make2.g++ Make3 Make.depend >FreeBSD/Makefile
	(cd FreeBSD; make -k)
#-----------------------------------------------------------------------------
tags: nothing
	(make tags -f Make.aux)
#-----------------------------------------------------------------------------
depend: nothing
	make -f Make.aux depend
#-----------------------------------------------------------------------------
clean:
	-rm */*.o */*.obj
#-----------------------------------------------------------------------------
makefiles:
	cat Make1 Make2.g++ Make3 >Make.aux
	-mkdir MSDOS-BC
	sed 's/\.o/.obj/g' Make1 Make2.bc >MSDOS-BC/makefile
	sed -e 's/\.o/.obj/g' -e 's/_unix/_msdos/g' \
		Make.depend >>MSDOS-BC/makefile
	-mkdir VMS
	sed -e 's/\.o/.obj/g' -e 's/#/!/g' -e 's/-/=/g' \
		Make1 >VMS/descrip.mms
	cat Make2.vms  >>VMS/descrip.mms
	sed -e 's/\.o/.obj/g' -e 's/#/!/g' \
		-e 's/_unix/_vms/g' -e 's=\.\./=\[-\]=g' \
		Make.depend >>VMS/descrip.mms
	grep \\.o Make1 | sed -e 's/\\//g' -e 's/OBJS = //g' \
		-e 's/$$/-/g' -e 's/\.o/.obj,/g' >VMS/acs.opt
	echo 'sys$$library:vaxcrtl/share' >> VMS/acs.opt
#-----------------------------------------------------------------------------
nothing:
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
