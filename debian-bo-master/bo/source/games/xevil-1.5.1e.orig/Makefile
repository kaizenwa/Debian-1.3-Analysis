#### Makefile for XEvil.   
# Steve Hardt  
# hardts@mit.edu (valid until Nov. 1996)
# hardts@netscape.com
# hardts@alum.mit.edu
# http://graphics.lcs.mit.edu/~hardts

#### Defines that are applicable to makes on all architectures
SHELL		=	/bin/sh
TARGETS		=	xevil #xshow
DEBUG_OPT	=	-g#  #-DPRINT_ERRORS  
LINK_OPT	=	#-O

#SRC_DIR and WORK_DIR are only used for `make workdir`
SRC_DIR		= 	/mit/hardts/src/X/xevil1.4.9
WORK_DIR 	=	/tmp/hardts3

FILES		= 	*README*  \
			xevil.6 Makefile *.C *.h \
			bitmaps NOTES app-defaults CLASSES run

OBJS		=	physical.o actual.o game.o main.o intel.o locator.o world.o \
			ui.o coord.o area.o utils.o 
ARCHITECTURES	=	athena-sun4 athena-sun5 alpha decmips ds decstation \
			freebsd hp700 iris4d \
			jsc-sun4 i386-linux rsaix sun4 sun5 vision-sun4 
STRIP		=	strip


#### Other macros are defined in the Specific Architectures section below.



#### Attempt to guesss the host architecture using the HOSTYPE and hostype
# variables.  Then call self with the architecture name.
default:
	@if [ $${HOSTTYPE-bob} != bob ] ; then \
		archit=$$HOSTTYPE ; \
	elif [ $${hosttype-bob} != bob ] ; then \
		archit=$$hosttype ; \
	else  \
		archit=unknown ; \
	fi ; \
	if [ $$archit != unknown ] ; then \
		echo Making for $$archit ; \
		$(MAKE) $$archit ; \
	else \
		echo "Could not figure out host architecture." ; \
		echo "Type 'make <architecture>' where <architecture> is one of" ; \
		echo "{$(ARCHITECTURES)}" ; \
	fi ;



#### Specific architectures.  
## Feel free to modify one of these to accomodate your machine's configuration.
alpha:
	@$(MAKE) CC="g++" LIBS="-lX11 -lm -ldnet_stub" \
	CFLAGS="-DUSE_RANDOM -DRANDOM_NEEDS_PROTOTYPES -DPROTECTED_IS_PUBLIC -DSELECT_NEEDS_PROTOTYPES" \
	LINK_FLAGS="-static" \
	$(TARGETS)


athena-sun4:
	@$(MAKE) CC="g++" LIBS="-lX11 -lm -lsocket -lnsl" \
CFLAGS="-DUSE_RANDOM -DRANDOM_NEEDS_PROTOTYPES -DXEVIL_KEYSET=UIsun4 -DPROTECTED_IS_PUBLIC" \
INCL_DIRS="-I/usr/openwin/include -I/mit/gnu/lib" \
LIBS_DIRS="-L/usr/openwin/lib" \
$(TARGETS)
## NOTE: The -lsocket and -lnsl libraries are not necessary on many systems.

athena-sun5:
	@$(MAKE) CC="g++" LIBS="-lX11 -lm -lsocket -lnsl" \
CFLAGS="-DXEVIL_KEYSET=UIsun4 -DPROTECTED_IS_PUBLIC" \
INCL_DIRS="-I/usr/openwin/include -I/mit/gnu/lib" \
LIBS_DIRS="-L/usr/openwin/lib" \
$(TARGETS)
## NOTE: The -lsocket and -lnsl libraries are not necessary on many systems.



decmips:
	@$(MAKE) CC="g++" LIBS="-lX11 -lm" \
INCL_DIRS="-I/mit/gnu/lib" \
CFLAGS="-DUSE_RANDOM -DRANDOM_NEEDS_PROTOTYPES -DSELECT_NEEDS_PROTOTYPES" \
$(TARGETS)

decstation:
	@$(MAKE) decmips

ds:
	@$(MAKE) decmips


# The -DMSEC_PER_CLOCK=8 is kind of a hack, take it out if the game speed of 
# XEvil is all screwed up.
freebsd:
	@$(MAKE) CC="c++" \
CFLAGS="-DUSE_RANDOM -DPROTECTED_IS_PUBLIC -DMSEC_PER_CLOCK=8" \
INCL_DIRS=-I/usr/X11R6/include LIBS_DIRS=-L/usr/X11R6/lib \
LIBS="-lX11 -lm" $(TARGETS)



hp700:
	@$(MAKE) CC="g++" LIBS="-lX11 -lm" INCL_DIRS="" CFLAGS="-DUSE_RANDOM -DRANDOM_NEEDS_PROTOTYPES -DMATH_H_IS_CC -DPROTECTED_IS_PUBLIC" \
$(TARGETS)


iris:
	@$(MAKE) CC="CC" CFLAGS="-32 -DXEVIL_KEYSET=UIiris -DNO_PRAGMAS" \
INCL_DIRS="" LIBS_DIRS="" LIBS="-lX11 -lm" LINK_FLAGS="-32" $(TARGETS)



iris4d:
	@$(MAKE) iris



jsc-sun4:
	@$(MAKE) CC="g++" LIBS="-lX11 -lm" \
CFLAGS="-DUSE_RANDOM -DCLOCKS_PER_SEC=1000000L" \
INCL_DIRS=-I/usr/local/X11R5/sun4/include \
LIBS_DIRS=-L/usr/local/X11R5/sun4/lib $(TARGETS)



i386-linux:
	@$(MAKE) CC="g++" CFLAGS="-DUSE_RANDOM -DPROTECTED_IS_PUBLIC" \
INCL_DIRS="" LIBS_DIRS="-L/usr/X11R6/lib" LIBS="-lX11 -lm" $(TARGETS)


rsaix: 
	@$(MAKE) CC="g++" CFLAGS="-DUSE_RANDOM -DRANDOM_NEEDS_PROTOTYPES -DUSE_SELECT_H -DXEVIL_KEYSET=UIrsaix -DPROTECTED_IS_PUBLIC" \
LIBS="-lX11 -lm" INCL_DIRS="-I/usr/athena/include -I/mit/gnu/lib" \
LIBS_DIRS="-L/usr/athena/lib/shared" \
$(TARGETS)



sun4:
	@$(MAKE) CC="g++" LIBS="-lX11 -lm" \
CFLAGS="-DCLOCKS_PER_SEC=1000000L -DUSE_RANDOM -DRANDOM_NEEDS_PROTOTYPES -DXEVIL_KEYSET=UIsun4 -DPROTECTED_IS_PUBLIC -DSELECT_NEEDS_PROTOTYPES" \
$(TARGETS)



sun5:
	@$(MAKE) CC="g++" LIBS="-lX11 -lm" \
CFLAGS="-DUSE_RANDOM -DRANDOM_NEEDS_PROTOTYPES -DXEVIL_KEYSET=UIsun4 -DPROTECTED_IS_PUBLIC" \
INCL_DIRS="-I/usr/openwin/include" LIBS_DIRS="-L/usr/openwin/lib" \
$(TARGETS)



vision-sun4:
	@$(MAKE) CC="g++" LIBS="-lX11 -lm" \
CFLAGS="-DCLOCKS_PER_SEC=1000000L -DUSE_RANDOM -DRANDOM_NEEDS_PROTOTYPES -DXEVIL_KEYSET=UIsun4 -DPROTECTED_IS_PUBLIC" \
INCL_DIRS="-I/usr/openwin/include" \
$(TARGETS)



#### Executables
xevil: $(OBJS)
	$(CC) $(LINK_FLAGS) $(LINK_OPT) $(LIBS_DIRS) -o  xevil $(OBJS) $(LIBS)
#	$(STRIP) xevil

# This is a simple program to show a bitmap and mask with arbitrary foreground
# and background colors.  I used it to test many of the bitmaps.  It is not
# needed to compile or run xevil.
xshow: xshow.C
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) $(LIBS_DIRS) -o xshow xshow.C $(LIBS)



#### XEvil modules
utils.o: utils.C utils.h 
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o utils.o -c utils.C 

coord.o: coord.C utils.h coord.h 
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o coord.o -c coord.C 

area.o: area.C utils.h coord.h area.h 
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o area.o -c area.C 

world.o: world.C utils.h coord.h area.h world.h \
	bitmaps/world/world.bitmaps
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o world.o -c world.C 

intel.o: intel.C utils.h coord.h area.h world.h id.h intel.h actual.h\
	locator.h physical.h 
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o intel.o -c intel.C

locator.o: locator.C utils.h coord.h area.h world.h id.h intel.h \
	physical.h locator.h actual.h
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o locator.o -c locator.C

ui.o: ui.C utils.h coord.h area.h world.h id.h intel.h \
	physical.h locator.h ui.h bitmaps/ui/ui.bitmaps
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o ui.o -c ui.C 

physical.o: physical.C utils.h coord.h area.h world.h id.h intel.h \
	locator.h physical.h
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o physical.o -c physical.C 

actual.o: actual.C utils.h coord.h area.h world.h id.h intel.h \
	locator.h physical.h actual.h \
	bitmaps/altar_of_sin/altar_of_sin.bitmaps bitmaps/bomb/bomb.bitmaps \
	bitmaps/chainsaw/chainsaw.bitmaps bitmaps/enforcer/enforcer.bitmaps \
	bitmaps/f_thrower/f_thrower.bitmaps bitmaps/fireball/fireball.bitmaps \
	bitmaps/frog/frog.bitmaps bitmaps/frog_gun/frog_gun.bitmaps \
	bitmaps/frog_shell/frog_shell.bitmaps bitmaps/hero/hero.bitmaps \
	bitmaps/lance/lance.bitmaps bitmaps/lancer/lancer.bitmaps \
	bitmaps/laser/laser.bitmaps \
	bitmaps/m_gun/m_gun.bitmaps bitmaps/med_kit/med_kit.bitmaps \
	bitmaps/ninja/ninja.bitmaps bitmaps/pistol/pistol.bitmaps \
	bitmaps/rock/rock.bitmaps bitmaps/shell/shell.bitmaps \
	bitmaps/swap_shell/swap_shell.bitmaps bitmaps/swapper/swapper.bitmaps \
	bitmaps/alien/alien.bitmaps \
	bitmaps/weight/weight.bitmaps \
	bitmaps/launcher/launcher.bitmaps \
	bitmaps/missile/missile.bitmaps bitmaps/grenades/grenades.bitmaps \
	bitmaps/grenade/grenade.bitmaps \
	bitmaps/chopper_boy/chopper_boy.bitmaps \
	bitmaps/n_protection/n_protection.bitmaps \
	bitmaps/n_shield/n_shield.bitmaps \
	bitmaps/t_protection/t_protection.bitmaps \
	bitmaps/t_shield/t_shield.bitmaps \
	bitmaps/transmogifier/transmogifier.bitmaps bitmaps/xit/xit.bitmaps \
	bitmaps/flag/flag.bitmaps bitmaps/doppel/doppel.bitmaps \
	bitmaps/home/home.bitmaps bitmaps/trapdoor/trapdoor.bitmaps \
	bitmaps/lemming/lemming.bitmaps bitmaps/fire_demon/fire_demon.bitmaps \
	bitmaps/stars/stars.bitmaps bitmaps/star/star.bitmaps \
	bitmaps/walker/walker.bitmaps bitmaps/hugger/hugger.bitmaps
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o actual.o -c actual.C 

game.o: game.C utils.h coord.h area.h world.h id.h intel.h \
	physical.h actual.h locator.h ui.h game.h
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o game.o -c game.C 

# NOTE: Does not need actual.h
main.o: main.C utils.h coord.h area.h world.h id.h intel.h \
	physical.h locator.h ui.h game.h
	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o main.o -c main.C 


#.SUFFIXES: .C .o
#.C.o: $*.C
#	$(CC) $(DEBUG_OPT) $(CFLAGS) $(INCL_DIRS) -o $*.o -c $*.C 


## Remove executables and all junk.
clean:
	/bin/rm -f $(TARGETS) $(OBJS) core test test.o xshow.o xshow


## Make a distribution file.
dist:
	tar chf xevil`date +%m.%d.%y`.tar $(FILES)
	compress xevil`date +%m.%d.%y`.tar


## Make a shadow tree for the XEvil source.
workdir:
	@if test ! -d $(WORK_DIR); then \
		mkdir $(WORK_DIR); \
	else \
		echo $(WORK_DIR) already exists; \
	fi; \
	for filee in $(FILES); do \
		/bin/rm -f $(WORK_DIR)/$$filee; \
		ln -s $(SRC_DIR)/$$filee $(WORK_DIR); \
	done



