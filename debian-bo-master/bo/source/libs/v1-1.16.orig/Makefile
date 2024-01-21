#===============================================================
# V Makefile - Version 1.16 - 1/20/97
#
# Copyright (C) 1995,1996,1997  Bruce E. Wampler
#
# This file is part of the V C++ GUI Framework, and is covered
# under the terms of the GNU Library General Public License,
# Version 2. This library has NO WARRANTY. See the source file
# vapp.cxx for more complete information about license terms.
#
# Many thanks to Jan Broeckhove for contributing this new
# set of makefiles, which are the standard method of building
# V beginning with Version 1.16.
#===============================================================
#
# IMPORTANT NOTE:
# This Makefile is the top level makefile used to build V on
# various Unix-like platforms. First and foremost, this Makefile
# requires a version of make compatible with Gnu make. One
# specific requirement is VPATH support, for example. There
# are other incompatibilities with gmake and the old standard
# Unix make.
#
# Also, all the Makefiles in this version are controlled by
# the file Config.mk. It defines various directory locations
# and things needed to make V work correctly. You should only
# have to edit Config.mk to get V compiled correctly.
#

CONFIG = ./Config.mk
include $(CONFIG)

#===============================================================
# Available targets:
#
# 	all (default):	vlib, vtest, utils, examp ...
#	vlib:		build V library
#       vtest:		build exhaustive test program v1
#	utils:		vgen, viconed, vdraw, vtexted, bmp2vbm
#	examples:	demos proto, drawex, tutor, icondemo
#	clean:		general cleanup
#	cleanmotif:	cleanup motif .o's, etc.
#	cleanobj:	cleanup .o's
#	cleanall:	cleanup all non-source stuff
#	tar:		X and Windows Distribution
#	tarwin:		Windows only Distribution"
#	tarx:		X only distribution
#	tarfull:	everthing, including .o's, etc.
#	getwin:		copy MS-Windows source from /dosc
#	permissions:	make sources readable to world
#	installv:	run as root to install to /usr/local/v
#	installvlinux:	run as root to install to /usr/local
#	installunm:	install to ../pubv (UNM specific)
#
#===============================================================


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#
# composite targets and targets for builds in subdirs
# (with some in-between targets to map target to directory)
#
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

.PHONY: default all vlib vtest utils examples testlib
.PHONY: srcx test appgen iconed draw texted bmp2vbm tutor examp drawex icons

default: all

all:	vlib vtest utils examples
	@echo "Library, test, utils, examples and tutorial made"

vlib: srcx

vtest: test

testlib:
	-rm bin/linux/v1
	cd srcx ; $(MAKE)
	cd test ; $(MAKE)

utils: appgen iconed draw texted bmp2vbm

examples: examp drawex icons tutor

srcx test appgen iconed draw texted bmp2vbm tutor examp drawex icons:
	cd $@ ; $(MAKE) 


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#
# Platform specific MAKES
#
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

.PHONY:	linux linuxelf sun4 sgi hpux aix solaris bsd
linux linuxelf sun4 sgi hpux aix solaris bsd:
	$(MAKE) ARCH=$@ vlib
	$(MAKE) ARCH=$@ vtest
	(cd appgen ; $(MAKE) ARCH=$@)
	(cd iconed ; $(MAKE) ARCH=$@)


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#
# targets for cleaning up
#
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

.PHONY:	clean
clean:
	-rm -f $(CLEANEXTS) *.bkh v.tgz
	for i in srcx appgen doc draw drawex examp iconed icons \
		srcx test texted tutor; do \
		(cd $$i; $(MAKE) clean); \
	done
	for i in contrib vxgl includex/v includew/v srcwin; do \
		(cd $$i; rm -f $(CLEANEXTS)); \
	done
	-rm -f bccide/*.bak bccide/*.tmp bccide/*.\~* bccide/*.csm
	-rm -f bccide/*.dsw bccide/*.obr bccide/*.tr
	-rm -f watcom/*.bak watcom/*.tmp watcom/*/*.exe watcom/*/*.obj
	-rm -f watcom/*/*.map watcom/*/*.lib watcom/*/*.err watcom/*/*.sym

.PHONY: cleanmotif
cleanmotif:
	-rm -f objm/*/* lib/*/libVm.a bin/*/v1m
	for i in linuxelf linux mips sgi hpux solaris aix sun4; do \
		touch objm/$$i/ThisIs;\
	done

.PHONY:	cleantestlib
cleantestlib:
	for i in linuxelf linux mips sgi hpux solaris aix sun4 win; do \
		-rm bin/$$i/v1 ; \
	done

.PHONY:	cleanbin
cleanbin:
	-rm -f bin/*/*
	for i in linuxelf linux mips sgi hpux solaris aix sun4 win; do \
		touch lib/$$i/ThisIs; touch bin/$$i/ThisIs ; \
	done

.PHONY:	cleanobj
cleanobj:
	-rm -f obj/*/* objm/*/*
	-rm -f watcom/*/*.obj
	for i in linuxelf linux mips sgi hpux solaris aix sun4 win16 win32 win32db; do \
		touch obj/$$i/ThisIs; \
	done
	for i in linuxelf linux mips sgi hpux solaris aix sun4; do \
		touch objm/$$i/ThisIs; \
	done

.PHONY:	cleanall
cleanall: clean cleanobj
	-rm -f lib/*/* bin/*/*
	for i in linuxelf linux mips sgi hpux solaris aix sun4 win; do \
		touch lib/$$i/ThisIs; touch bin/$$i/ThisIs ; \
	done


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#
# targets for making tar files
#
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

.PHONY:	tar
tar: clean
	@echo "Making v.tgz - X and Windows Distribution"
	@echo "    Cleaning up old tar file"
	-rm ../v-$(VV).tar$(TARZ)
	cd .. ; tar cvf$(TARZ) v-$(VV).tar$(TARZ) v/Make* v/R* v/C* v/P* v/B* \
	 v/doc/* v/examp/* v/bmp2vbm/* v/tutor/* v/include v/includex/* \
	 v/includew/* v/lib/*/ThisIs v/obj/*/ThisIs v/objm/*/ThisIs v/srcx/* \
	 v/srcwin/* v/test/* v/draw/* v/iconed/* v/drawex/* v/bin/*/ThisIs v/clnupwin.bat \
	 v/PACKLIST v/bccide v/BUGS v/icons/* v/watcom/* v/contrib/* v/appgen/* \
	 v/texted/* v/vxgl/* v/msvc/* ; cd v

.PHONY:	tarwin
tarwin: clean
	@echo "Making vwin.tar - Windows Distribution"
	@echo "    Cleaning up old tar file"
	-rm ../vwin$(VVW).tar$(TARZ)
	cd .. ; tar cvf$(TARZ) vwin$(VVW).tar$(TARZ) v/Make* v/R* v/C* \
	 v/doc/* v/examp/* v/bmp2vbm/* v/tutor/* v/include \
	 v/includew/* v/lib/*/ThisIs v/lib/win/* v/obj/*/ThisIs v/objm/*/ThisIs \
	 v/srcwin/* v/test/* v/draw/* v/drawex/* v/bin/*/ThisIs v/clnupwin.bat \
	 v/PACKLIST v/bccide v/BUGS v/icons/* v/iconed/* v/contrib/* \
	 v/appgen/* v/texted/* v/vxgl/* v/msvc/* ; cd v

.PHONY:	tarx
tarx: clean
	@echo "Making vx-$(VV).tar - X only distribution"
	@echo "    Cleaning up old tar file"
	-rm ../vx-$(VV).tar$(TARZ)
	cd .. ; tar cvf$(TARZ) vx-$(VV).tar$(TARZ) v/Make* v/R* v/C* v/P* \
	 v/doc/* v/examp/* v/bmp2vbm/* v/tutor/* v/include v/includex/* \
	 v/lib/*/ThisIs v/obj/*/ThisIs v/objm/*/ThisIs v/srcx/* v/icons/* \
	 v/test/* v/draw/* v/iconed/* v/drawex/* v/bin/*/ThisIs \
	 v/contrib/* v/appgen/* v/texted/* v/vxgl/* v/BUGS ; cd v

.PHONY:	tarfull
tarfull: clean
	@echo "Making vfull.tgz - FULL VERSION"
	@echo "    Cleaning up old tar file"
	-rm ../vfull.tar$(TARZ)
	cd .. ; tar cvf$(TARZ) vfull.tar$(TARZ) v/* ; cd v

.PHONY:	getwin
getwin:
	-mkdir srcwin
	-mkdir includew includew/v
	-mkdir bccide
	-mkdir msvc msvc/viconed msvc/vmsvc32 msvc/vappgen msvc/vtstms32
	-mkdir msvc/viconed/release msvc/vmsvc32/release msvc/vappgen/release msvc/vtstms32/release
	-mkdir bin/win
	-mkdir watcom watcom/win16 watcom/win16db watcom/win32 watcom/win32db
	-mkdir obj/win16 obj/win32 obj/win32db
	-mkdir lib/win
	touch obj/win16/ThisIs obj/win32/ThisIs obj/win32db/ThisIs
	touch lib/win/ThisIs
	-rm watcom/*/*
	-rm srcwin/*
	-rm includew/v/*
	-rm bccide/*
	-rm msvc/* msvc/viconed/* msvc/vmsvc32/* msvc/vappgen/* msvc/vtstms32/*
	-rm msvc/viconed/release/* msvc/vmsvc32/release/* msvc/vappgen/release/* msvc/vtstms32/release/*
	-rm bin/win/*.exe
	-rm lib/win/*
	-rm RDMEWIN*
	-rm clnupwin.bat
	cp /dosc/v/srcwin/* srcwin
	cp /dosc/v/includew/v/* includew/v
	cp /dosc/v/bccide/*.ide bccide
	cp /dosc/v/bccide/*.rc bccide
	cp /dosc/v/bccide/*.ico bccide
	cp /dosc/v/bccide/*.def bccide
	cp /dosc/v/bccide/winstub.exe bccide
	cp /dosc/v/msvc/* msvc
	cp /dosc/v/msvc/viconed/* msvc/viconed
	cp /dosc/v/msvc/viconed/release/* msvc/viconed/release
	cp /dosc/v/msvc/vmsvc32/* msvc/vmsvc32
	cp /dosc/v/msvc/vmsvc32/release/* msvc/vmsvc32/release
	cp /dosc/v/msvc/vappgen/* msvc/vappgen
	cp /dosc/v/msvc/vappgen/release/* msvc/vappgen/release
	cp /dosc/v/msvc/vtstms32/release/* msvc/vtstms32/release
	cp /dosc/v/rdmewin.txt RDMEWIN.TXT
	cp /dosc/v/clnupwin.bat clnupwin.bat
	cp -r /dosc/v/watcom/* watcom
	-rm watcom/*.bak watcom/*.tmp watcom/*/*.exe watcom/*/*.sym
	-rm watcom/*/*.obj watcom/*/*.map watcom/*/*.lib watcom/*/*.err 


.PHONY:	tarxdemo
tarxdemo:
	-rm ../vdemox-$(VV).tar$(TARZ)
	-cp bin/linux/vdraw vdraw.linux.bin
	tar cvf$(TARZ) vdemox-$(VV).tar$(TARZ) vdraw.linux.bin
	mv vdemox-$(VV).tar$(TARZ) ..
	-rm vdraw.linux.bin

.PHONY:	tardemo
tardemo:
	-rm ../vdemos-$(VV).tar$(TARZ)
	-cp bin/linux/vdraw vdraw.linux.bin
	-cp bin/win/vdraw.exe vdraw.exe
	tar cvf$(TARZ) vdemos-$(VV).tar$(TARZ) vdraw.exe vdraw.linux.bin
	mv vdemos-$(VV).tar$(TARZ) ..
	-rm vdraw.linux.bin
	-rm vdraw.exe


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#
# target for fixing permissions
#
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

PERMDIRS =	. doc doc/fig doc/html examp tutor includex includex/v includew/v lib \
		lib/* obj obj/* objm objm/* srcx srcwin test draw drawex iconed bin \
		bin/* bmp2vbm bccide icons watcom watcom/win16 watcom/win16db \
		watcom/win32 watcom/win32db contrib appgen texted vxgl

PERMFILES =	Make* NEWS README BUGS C* RDMEWIN.TXT doc/*/* doc/* includex/v/* \
		includew/v/* lib/*/* obj/*/* objm/*/* srcx/* srcwin/* test/* \
		draw/* iconed/* drawex/* examp/* bin/*/* tutor/* bmp2vbm/* bccide/* \
		icons/* clnupwin.bat PACKLIST watcom/*/* watcom/* contrib/*  \
		appgen/* texted/* vxgl/*

EXEFILES =	bin/*/* doc/checkspelling doc/cleanup \
		doc/dovwebref

.PHONY:	permissions
permissions:
	-chmod 755 $(PERMDIRS)
	-chmod 644 $(PERMFILES)
	-chmod 755 $(PERMDIRS)
	-chmod 755 $(EXEFILES)


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#
# installation targets
# These work best for dedicated systems where you can run as
# root. It will also be easier if you make a symbolic link
# in the home directory of root to whereever V is before
# doing the install. This makefile uses $(LibDir), which is
# set to HOME by default, so adding a link from the root
# directory of root to the real v directory makes this work right.
#
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

.PHONY:	installLinux
installLinux:
	$(MAKE) ARCH=linux installVLinux

.PHONY:	installLinuxElf
installLinuxElf:
	$(MAKE) ARCH=linuxelf installVLinux

.PHONY:	installv
installv:
	-mkdir /usr/local/v
	-mkdir /usr/local/v/lib
	-mkdir /usr/local/v/bin
	-mkdir /usr/local/v/include
	-mkdir /usr/local/v/include/v
	-chmod 755 /usr/local/v 
	-chmod 755 /usr/local/v/lib
	-chmod 755 /usr/local/v/bin
	-chmod 755 /usr/local/v/include
	-chmod 755 /usr/local/v/include/v
	cp includex/v/* /usr/local/v/include/v
	cp $(LibDir)/*.a /usr/local/v/lib
	cp $(Bin)/* /usr/local/v/bin
	-chmod 644 /usr/local/v/include/v/*
	-chmod 644 /usr/local/v/lib/*
	-chmod 755 /usr/local/v/bin/*

.PHONY:	instalVlLinux
installVLinux:
	@echo "You must run this as root"
	-mkdir /usr/include/v
	chmod 644 includex/v/*
	cp includex/v/* /usr/include/v
ifeq ($(Arch),linuxelf)
	chmod 644 $(LibDir)/*.so.$(VV)
	cp $(LibDir)/*.so.$(VV) /usr/lib
	rm -f /usr/lib/$(LIBNAME).so.1
	ln -s /usr/lib/$(LIBNAME).so.$(VV) /usr/lib/$(LIBNAME).so.1
	rm -f /usr/lib/$(LIBNAME).so
	ln -s /usr/lib/$(LIBNAME).so.$(VV) /usr/lib/$(LIBNAME).so
else
	chmod 644 $(LibDir)/*.a
	cp $(LibDir)/*.a /usr/lib
endif
	chmod 755 $(Bin)/*
	cp $(Bin)/* /usr/X11/bin

# To install V to /usr/local hierarchy at UNM
# The strategy I used at UNM, where I don't have access to
# the system as root was to get symbolic links on all the local
# architectures to ~wampler/pubv/xxx for each architecture.
# I then make the files on my private directory, and them
# copy them to the /pubv directory for public consumption.
# This scheme allows you to add whatever you need, such
# as /usr/local/v/doc, or whatever.
.PHONY: installunm
installunm:
	cp includex/v/* ../pubv/irix/include/v
	-chmod 644 ../pubv/irix/include/v/*
	cp includex/v/* ../pubv/sunos/include/v
	-chmod 644 ../pubv/sunos/include/v/*

	cp lib/sgi/* ../pubv/irix/lib
	-chmod 644 ../pubv/irix/lib/*
	cp lib/sun4/* ../pubv/sunos/lib
	ranlib ../pubv/sunos/lib/libV.a
	-chmod 644 ../pubv/sunos/lib/*

	cp bin/sgi/* ../pubv/irix/bin
	-chmod 755 ../pubv/irix/bin/*
	cp bin/sun4/* ../pubv/sunos/bin
	-chmod 755 ../pubv/sunos/bin/*
