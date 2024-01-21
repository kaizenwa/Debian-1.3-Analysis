
#======================================================================
#          See README.First for instructions on compiling.
#======================================================================
# Useful targets:
#
#	make		# Make everything (no install).
#	make install	# Install latest binaries, sounds, maps, etc.
#			# * DOES NOT REMAKE! *
#	make clean	# Remove everything from /lux that didn't come
#                       # in the distribution file.
#	make uninstall	# Remove things that were installed (doesn't
#			# delete highscores).
#			# Be careful! If you have modified any files
#			# under /usr/games/lib/luxman this may delete
#			# them!
#=======================================================================

export BASIC_C_FLAGS = -Wall -O2 -m486 -fomit-frame-pointer
# export BASIC_C_FLAGS = -Wall -DDEBUG

# Prevent myself from accidentally running make in subdirs (they
# need BASIC_C_FLAGS)
export TOP_LEVEL=ok

CXXFLAGS = $(BASIC_C_FLAGS) -I.
CFLAGS = $(CXXFLAGS)

#ifeq (.depend,$(wildcard .depend))
all: gtoolsdir rawkeylib libmisc argvlib luxdir  
#include .depend
#else
#all: dep gtoolsdir rawkeylib libmisc argvlib luxdir
#endif

libmisc:
	make -k -C lib

gtoolsdir:
	make -k -C gtools

rawkeylib:
	make -C rawkey

argvlib:
	make -C argv

luxdir:
	make -C src
	@echo
	@echo 'You may now play LuxMan from this directory,'
	@echo 'or type "make install" to install the proper'
	@echo 'files in /usr/games and /usr/games/lib/luxman'.
	@echo

#-------------------------------------------------------------------------
# Installs latest binaries only. Doesn't check remake.
#
# Note: Removes things from the `old' data file location /usr/lib/luxman
#-------------------------------------------------------------------------
install:
	# Remove stuff from old (non-FSSTND) location
	rm -rf /usr/lib/luxman
	# Make sure directories exist
	install -m 755 -d /usr/games/lib/luxman/11k
	install -m 755 -d /usr/games/lib/luxman/8k
	install -m 755 -d /usr/games/lib/luxman/modern/maps
	install -m 755 -d /usr/games/lib/luxman/modern/mazes
	install -m 755 -d /usr/games/lib/luxman/oldscen
	install -m 755 -d /usr/games/lib/luxman/fonts
	# Install modern scenario
	ln -sf /usr/games/lib/luxman/fonts /usr/games/lib/luxman/modern/fonts
	ln -sf /usr/games/lib/luxman/11k /usr/games/lib/luxman/modern/11k
	ln -sf /usr/games/lib/luxman/8k /usr/games/lib/luxman/modern/8k
	# Install old scenario
	ln -sf /usr/games/lib/luxman/modern/mazes /usr/games/lib/luxman/oldscen/mazes
	ln -sf /usr/games/lib/luxman/modern/maps /usr/games/lib/luxman/oldscen/maps
	ln -sf /usr/games/lib/luxman/modern/fonts /usr/games/lib/luxman/oldscen/fonts
	ln -sf /usr/games/lib/luxman/modern/11k /usr/games/lib/luxman/oldscen/11k
	ln -sf /usr/games/lib/luxman/modern/8k /usr/games/lib/luxman/oldscen/8k
	install -m 755 -d /usr/man/man6
	# Install maps, mazes, sounds, fonts
	install -m 444 ./maps/*.map /usr/games/lib/luxman/modern/maps
	install -m 444 ./mazes/*.map /usr/games/lib/luxman/modern/mazes
	install -m 444 ./11k/*.snd /usr/games/lib/luxman/11k
	install -m 444 ./8k/*.snd /usr/games/lib/luxman/8k
	install -m 444 ./fonts/*.font /usr/games/lib/luxman/fonts
	# Install fonts here too so other utils can find them
	install -m 444 ./fonts/*.font /usr/games/lib/luxman
	# Install scenario file and level files
	install -m 444 scenarios /usr/games/lib/luxman
	install -m 444 levels.def /usr/games/lib/luxman/modern
	install -m 444 old.def /usr/games/lib/luxman/oldscen
	# Manpages
	install -m 444 luxman.6 luxchk.6 /usr/man/man6
	# Binaries
	install -s -m 4755 luxman /usr/games
	install -s -m 755 luxman-snd /usr/games
	install -s -m 755 luxchk /usr/games

uninstall:
	#
	# We can't just do a blind delete since the user may have
	# added a scenario
	#
	# Remove binaries
	rm -f /usr/games/luxchk
	rm -f /usr/games/luxman
	rm -f /usr/games/luxman-snd
	# Remove manpages
	rm -f /usr/man/man6/luxman.6 /usr/man/man6/luxchk.6
	# Remove modern scenario
	rm -rf /usr/games/lib/luxman/modern
	rm -rf /usr/games/lib/luxman/oldscen
	# Remove sounds
	rm -rf /usr/games/lib/luxman/11k
	rm -rf /usr/games/lib/luxman/8k
	# Leave the rest

clean:
	rm -f *~ core
	make -C gtools clean
	make -C rawkey clean
	make -C lib clean
	make -C argv clean
	make -C src clean

dep:
	make -C gtools dep
	make -C lib dep
	make -C src dep

