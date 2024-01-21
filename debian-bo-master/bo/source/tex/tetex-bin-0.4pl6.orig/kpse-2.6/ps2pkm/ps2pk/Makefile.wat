#
# Makefile for Watcom C/386 9.0 (wmake)
#
# Version 1.4: Jan 1994
#

# Here we install the binaries:
BINDIR = c:\emtex

# Here we install the manual pages:
MANDIR = c:\emtex\doc

# This are the built in paths
T1INPUTS = .;c:\\emtex\\ps

# Compiler directives and other goodies
CC = wcl386 -p
LD = wlinkp
T1LIB = ..\type1\type1.lib

.EXTENSIONS:
.EXTENSIONS: .exe .obj .c

.BEFORE
	@set INCLUDE=.;$(%watcom)\h
	@set DOS4G=QUIET

OPTIMIZE = /oaxe /zp4 /4r
OPT = -dMSDOS -zq -d2 -j

CFLAGS = $(OPT) $(OPTIMIZE)

all: ps2pk.exe pk2bm.exe .SYMBOLIC

ps2pk.exe: ps2pk.obj encoding.obj pkout.obj filenames.obj $(T1LIB)
	@echo name ps2pk.exe > ps2pk.rsp
	@echo system dos4g >> ps2pk.rsp
	@echo debug all >> ps2pk.rsp
	@echo option stack=8192 >> ps2pk.rsp
	@echo option dosseg  >> ps2pk.rsp
	@echo file encoding,ps2pk,pkout,filenames >> ps2pk.rsp
	@echo libpath ..\type1 >> ps2pk.rsp
	@echo library type1 >> ps2pk.rsp
	$(LD) @ps2pk.rsp

ps2pk.obj: ps2pk.c pkout.h filenames.h
	$(CC) -DT1INPUTS="$(T1INPUTS)" $(CFLAGS) -I..\type1 -c ps2pk.c

pk2bm.exe: pk2bm.obj pkin.obj
	$(LD) name pk2bm.exe file pk2bm,pkin

mag.exe: mag.obj
	$(LD) system dos4g name mag.exe file mag

pfb2pfa.exe: pfb2pfa.obj filenames.obj
	$(LD) system dos4g name pfb2pfa.exe file pfb2pfa,filenames

pktest.exe: pktest.obj pkout.obj
	$(LD) system dos4g name pktest.exe file pktest,pkout

$(T1LIB)
	cd ..\type1
	wmake -f Makefile.wat type1.lib
	cd ..\src
	
t1test.exe: $(T1LIB)
	cd ..\type1
	wmake -f Makefile.wat t1test.exe
	cd ..\src

install: ps2pk.exe pk2bm.exe .SYMBOLIC
	copy ps2pk.exe   $(BINDIR)
	copy pk2bm.exe   $(BINDIR)

install.man
	copy ..\man\ps2pk.1   $(MANDIR)\ps2pk.doc
	copy ..\man\pk2bm.1   $(MANDIR)\pk2bm.doc

clean
	-rm -f *.obj *.pk *.exe *.rsp *.err
	cd ..\type1
	wmake -f Makefile.wat clean
	cd ..\src

.c.obj:
	$(CC) $(CFLAGS) -c $[*
