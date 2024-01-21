#
# Makefile for Watcom C/386 9.0 for MSDOS (called from ../src)
#
# Version 1.4: Jan 1994
#

OPTIMIZE = /oaxe /zp4 /4r
OPT = -dMSDOS -zq -d2 -j

# Compiler directives and other goodies
CFLAGS = $(OPT) $(OPTIMIZE)
CC = wcl386 -p
LD = wlinkp

OBJ = arith.obj curves.obj fontfcn.obj hints.obj lines.obj objects.obj &
      paths.obj regions.obj scanfont.obj spaces.obj t1funcs.obj &
      t1info.obj t1io.obj t1snap.obj t1stub.obj token.obj type1.obj &
      util.obj bstring.obj

.BEFORE
	@set INCLUDE=.;$(%watcom)\h
	@set DOS4G=QUIET

.EXTENSIONS:
.EXTENSIONS: .exe .obj .c

type1.lib: $(OBJ) type1.rsp
	-rm -f type1.lib
	wlib type1.lib @type1.rsp

type1.rsp: $(OBJ)
	-rm -f type1.rsp
	for %f in ($(OBJ)) do echo +%f >> type1.rsp

# breaks optimizer ...
hints.obj: hints.c
	$(CC) $(OPT) -c hints.c

t1test.exe: t1test.obj type1.lib
	$(LD) system dos4g name t1test.exe file t1test.obj lib type1

clean: .SYMBOLIC
	-rm -f *.obj *.lib t1test.exe *.err

.c.obj:
	$(CC) $(CFLAGS) -c $[*

# Dependencies
arith.obj:    arith.c types.h objects.h spaces.h arith.h
bstring.obj:  bstring.c 
curves.obj:   curves.c types.h objects.h spaces.h paths.h regions.h curves.h &
               lines.h  arith.h 
fontfcn.obj:  fontfcn.c t1imager.h util.h fontfcn.h fontmisc.h
hints.obj:    hints.c types.h objects.h spaces.h
objects.obj:  objects.c types.h objects.h spaces.h paths.h regions.h fonts.h &
               pictures.h strokes.h cluts.h
paths.obj:    paths.c paths.h regions.h hints.h
lines.obj:    lines.c types.h objects.h spaces.h regions.h lines.h
regions.obj:  regions.c types.h objects.h spaces.h regions.h paths.h curves.h &
               lines.h pictures.h fonts.h hints.h strokes.h
scanfont.obj: scanfont.c t1stdio.h util.h token.h fontfcn.h blues.h
spaces.obj:   spaces.c types.h objects.h spaces.h paths.h pictures.h fonts.h &
               arith.h trig.h
t1stubs.obj:  t1stubs.c objects.h
t1funcs.obj:  t1funcs.c objects.h spaces.h regions.h t1stdio.h util.h fontfcn.h
t1info.obj:   t1info.c types.h ffilest.h t1intf.h t1stdio.h t1hdigit.h
t1snap.obj:   t1snap.c objects.h spaces.h paths.h
t1test.obj:   t1test.c ffilest.h
token.obj:    token.c types.h t1stdio.h util.h digit.h token.h tokst.h hdigit.h
type1.obj:    type1.c types.h objects.h spaces.h paths.h fonts.h pictures.h &
               util.h blues.h
util.obj:     util.c types.h util.h fontmisc.h
