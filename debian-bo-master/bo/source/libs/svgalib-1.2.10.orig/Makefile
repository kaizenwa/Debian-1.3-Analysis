#---------------------------------------------------------------------
# Makefile for SVGAlib.
#
# It's pretty unreadable, but just doing make install should be
# enough. This will install the headerfiles and shared library first
# (which is enough to compile things), after which the static version is
# optionally compiled and installed (if it fails, the shared libraries
# should still work fine).
#
#----------------------------------------------------------------------

# *** NO SERVICIBLE PARTS HERE!
#     All options are in Makefile.cfg.

include Makefile.cfg

#----------------------------------------------------------------------
# Rules Section
#----------------------------------------------------------------------

JUMP_DIR=$(shell sh -c pwd)/jump/
JUMP_LIB=libvga

export JUMP_DIR
export JUMP_LIB

# Search the old directories for mkimage and mkstubs.
# Make sure mkimage get the right "as" assembler.
ifeq (a.out, $(TARGET_FORMAT))
  PATH := /usr/i486-linuxaout/bin:$(PATH):/usr/dll/bin
  export PATH
endif	

LIBS     = $(LVGA) #-ltermcap

# Determine what library (static or shared) we will be linking programs with
ifdef INSTALLSHAREDLIB
  LVGA = -lvga
else
  LVGA    = ./libvga.a
  LVGADEP = libvga.a
endif

# A flag if this is a distribution:
DISTRIBUTION = $(shell sh -c "echo sharedlib/DIST*")

UTILS	= restorefont runx restorepalette restoretextmode textmode \
	  savetextmode dumpreg fix132x43
OBSOLETESHAREDIMAGES = /lib/libvga.so.1.0 /lib/libvga.so.1.0.1 \
	/lib/libvga.so.1.0.2 /lib/libvga.so.1.0.3 /lib/libvga.so.1.0.4 \
	/lib/libvga.so.1.0.5 /lib/libvga.so.1.0.6 /lib/libvga.so.1.0.7 \
	/lib/libvga.so.1.0.8 /lib/libvga.so.1.0.9 /lib/libvga.so.1.0.10 \
	/lib/libvga.so.1.0.11 /lib/libvga.so.1.0.12 /lib/libvga.so.1.1 \
	/lib/libvga.so.1.1.1 /lib/libvga.so.1.1.2 /lib/libvga.so.1.1.3 \
	/lib/libvga.so.1.1.4 /lib/libvga.so.1.1.5 /lib/libvga.so.1.1.6 \
	/lib/libvga.so.1.1.7 /lib/libvga.so.1.1.8 /lib/libvga.so.1.1.9 \
	/lib/libvga.so.1.2.0 /lib/libvga.so.1.2.1 /lib/libvga.so.1.2.2 \
	/lib/libvga.so.1.2.3 /lib/libvga.so.1.2.4 /lib/libvga.so.1.2.5 \
	/lib/libvga.so.1.2.6 /lib/libvga.so.1.2.7 /lib/libvga.so.1.2.8 \
	/lib/libvga.so.1.2.9

# for ELF
OBSOLETESHAREDIMAGES +=	/lib/libvgagl.so.1.2.0 /lib/libvgagl.so.1.2.1 /lib/libvgagl.so.1.2.2 \
	/lib/libvgagl.so.1.2.3 /lib/libvgagl.so.1.2.4 /lib/libvgagl.so.1.2.5 \
	/lib/libvgagl.so.1.2.6 /lib/libvgagl.so.1.2.7 /lib/libvgagl.so.1.2.8 \
	/lib/libvgagl.so.1.2.9

ifeq (a.out, $(TARGET_FORMAT))
	SHAREDLIBS = sharedlib/libvga.so.$(VERSION)
	SVGALIBSHAREDSTUBS = sharedlib/libvga.sa sharedlib/libvgagl.sa
	JUMP = jump
else	
	SHAREDLIBS = sharedlib/libvga.so.$(VERSION) sharedlib/libvgagl.so.$(VERSION)
	SVGALIBSHAREDSTUBS =
	JUMP =
endif	

BACKUP   = ../svgalib-$(VERSION).tar.gz

default:
	@echo "To install SVGAlib, do one of the following:"
	@echo ""
	@echo "	make shared	- compile shared libraries, in sharedlib/"
	@echo "	make static	- compile static libraries, in staticlib/"
	@echo "	make textutils	- make utilities in utils/"
	@echo "	make demoprogs	- make demo programs in demo/ and threeDKit/"
	@echo ""
	@echo "	make install	- install components specified in Makefile.cfg"
	@echo ""

# all:	installed demoprogs

.PHONY: default all install installheaders installconfig
.PHONY: clean distclean indent
.PHONY: force_remake remake_shared shared static
.PHONY: indent-gnu

#installed:
#	@if [ ! -f $(includedir)/vga.h ]; then \
#		echo Do make install first.; \
#		false; \
#	fi
#	@if [ ! -f $(libdir)/libvga.a ]; then \
#		if [ ! -f $(sharedlibdir)/libvga.so.$(VERSION) ]; then \
#			echo Do make install first.; \
#			false; \
#		fi; \
#	fi

installheaders:
	@echo Installing header files in $(includedir).
	@if [ -f /usr/include/vga.h ]; then \
		rm -f /usr/include/vga.h /usr/include/vgagl.h; \
		echo Old header files in /usr/include removed.; \
	fi
	@cp $(srcdir)/src/vga.h $(includedir)/vga.h
	@chmod a+r $(includedir)/vga.h
	@cp $(srcdir)/gl/vgagl.h $(includedir)/vgagl.h
	@chmod a+r $(includedir)/vgagl.h
	@cp $(srcdir)/src/mouse/vgamouse.h $(includedir)/vgamouse.h
	@chmod a+r $(includedir)/vgamouse.h
	@cp $(srcdir)/src/keyboard/vgakeyboard.h $(includedir)/vgakeyboard.h
	@chmod a+r $(includedir)/vgakeyboard.h

installsharedlib: $(SHAREDLIBS) $(SVGALIBSHAREDSTUBS)
	@echo Installing shared library stubs in $(libdir).
	@for foo in $(notdir $(SVGALIBSHAREDSTUBS)); do \
		$(INSTALL_DATA) sharedlib/$$foo $(libdir)/$$foo; \
		chmod a+r $(libdir)/$$foo; \
	done
	@if [ -f /usr/lib/libvga.sa ]; then \
		rm -f /usr/lib/libvga.sa /usr/lib/libvgagl.sa; \
		echo Old stubs in /usr/lib removed.; \
	fi
	@echo Installing shared library image as \
		$(addprefix $(sharedlibdir)/,$(notdir $(SHAREDLIBS))).
	@rm -f $(OBSOLETESHAREDIMAGES)
	@for foo in $(notdir $(SHAREDLIBS)); do \
		$(INSTALL_SHLIB) sharedlib/$$foo $(sharedlibdir)/$$foo; \
		(cd $(sharedlibdir); \
		 ln -sf $$foo `echo $$foo | sed 's/\.so\..*/.so/'` ); \
	done
	@ldconfig

installstaticlib: $(STATICLIBS)
	@echo Installing static libraries in $(libdir).
	@rm -f /usr/lib/libvga.a
	@$(INSTALL_DATA) staticlib/libvga.a $(libdir)/libvga.a
	@chmod a+r $(libdir)/libvga.a
	@rm -f /usr/lib/libvgagl.a
	@$(INSTALL_DATA) staticlib/libvgagl.a $(libdir)/libvgagl.a
	@chmod a+r $(libdir)/libvgagl.a

installutils: textutils
	@if [ ! -d $(bindir) ]; then \
		echo No $(bindir) directory, creating it.; \
		mkdir $(bindir); \
	fi
	@if [ -f /usr/bin/restorefont ]; then \
		echo Removing old utilities in /usr/bin.; \
		for x in $(UTILS); do rm -f /usr/bin/$$x; done; \
	fi
	@if [ -f /usr/bin/convfont ]; then \
		echo Removing inappropriate utilities in /usr/bin.; \
		rm -f /usr/bin/convfont /usr/bin/setmclk; \
	fi
	@echo Installing textmode utilities in $(bindir):
	@echo "restorefont:      Save/restore textmode font."
	@cp utils/restorefont $(bindir)
	@echo "restorepalette:   Set standard VGA palette."
	@cp utils/restorepalette $(bindir)
	@echo "dumpreg:          Write ASCII dump of SVGA registers."
	@cp utils/dumpreg $(bindir)
	@echo "restoretextmode:  Save/restore textmode registers."
	@cp utils/restoretextmode $(bindir)
	@echo "textmode:         Script that tries to restore textmode."
	@cp utils/textmode $(bindir)
	@echo "savetextmode:     Script that saves textmode information used by 'textmode'."
	@cp utils/savetextmode $(bindir)

installconfig:
	@if [ ! -d $(datadir) ]; then \
		echo Creating configuration directory $(datadir).; \
		mkdir $(datadir); \
		chmod go-w $(datadir); \
	fi
	@if [ \( -f /usr/local/lib/libvga.config -a ! -f $(datadir)/libvga.config \) ]; then \
		echo "Moving old config file /usr/local/lib/libvga.config to $(datadir)." ; \
		mv -f /usr/local/lib/libvga.config $(datadir)/libvga.config; \
	fi
	@if [ \( -f /usr/local/lib/libvga.et4000 -a ! -f $(datadir)/libvga.et4000 \) ]; then \
		echo "Moving old config file /usr/local/lib/libvga.et4000 to $(datadir)." ; \
		mv -f /usr/local/lib/libvga.et4000 $(datadir)/libvga.et4000; \
	fi
	@if [ \( -f /usr/local/lib/libvga.ega -a ! -f $(datadir)/libvga.ega \) ]; then \
		echo "Moving old config file /usr/local/lib/libvga.ega to $(datadir)." ; \
		mv -f /usr/local/lib/libvga.ega $(datadir)/libvga.ega; \
	fi
	@if [ \( -f /etc/mach32.eeprom -a ! -f $(datadir)/mach32.eeprom \) ]; then \
		echo Consider moving your /etc/mach32.eeprom file to $(datadir) ; \
		echo and changing $(datadir)/libvga.config appropriately. ; \
	fi
	@if [ ! -f $(datadir)/libvga.config ]; then \
		echo Installing default configuration file in $(datadir).; \
		cp $(srcdir)/libvga.config $(datadir)/libvga.config; \
	fi
	@if [ ! -f $(datadir)/libvga.et4000 ]; then \
		echo Installing dynamically loaded ET4000 registers in $(datadir).; \
		cp $(srcdir)/et4000.regs $(datadir)/libvga.et4000; \
	fi
 
install: installheaders $(INSTALLSHAREDLIB) installconfig $(INSTALLUTILS) $(INSTALLSTATICLIB)
	@echo Now run "'make demoprogs'" to make the test and demo programs in demos/.

uninstall:
	@echo Removing textmode utilities.
	@for x in $(UTILS); do rm -f $(bindir)/$$; done
	@echo Removing shared library stubs \(shared image not removed\).
#	@rm -f /lib/$(SHAREDLIBS)
#	@rm -f /lib/libvga.so.1
	@rm -f $(libdir)/libvga.sa
	@rm -f $(libdir)/libvgagl.sa
	@echo Removing static libraries.
	@rm -f $(libdir)/libvga.a
	@rm -f $(libdir)/libvgagl.a
	@echo Removing header files.
	@rm -f $(includedir)/vga.h
	@rm -f $(includedir)/vgagl.h
	@rm -f $(includedir)/vgamouse.h
	@rm -f $(includedir)/vgakeyboard.h

SHAREDDIRS0 = sharedlib/mouse sharedlib/keyboard sharedlib/ramdac \
		sharedlib/clockchip
SHAREDDIRS = $(SHAREDDIRS0) $(JUMP)
STATICDIRS = staticlib/mouse staticlib/keyboard staticlib/ramdac \
		staticlib/clockchip
UTILDIRS = utils
DEMODIRS = demos threeDKit

$(SHAREDDIRS0) $(STATICDIRS) $(DEMODIRS):
	mkdir -p $@
	
jump:
	mkdir -p jump
	if [ ! -f jump/jump.funcs ]; then \
		cp $(srcdir)/jump/jump.funcs $(srcdir)/jump/jump.ignore \
		   $(srcdir)/jump/jump.vars jump; \
	fi
	
utils:
	mkdir -p utils
	if [ ! -f utils/runx ]; then \
		cp $(srcdir)/utils/runx $(srcdir)/utils/savetextmode \
		   $(srcdir)/utils/textmode utils; \
	fi

static: staticlib/libvga.a staticlib/libvgagl.a

.PHONY: staticlib/libvgagl.a staticlib/libvga.a

staticlib/libvgagl.a staticlib/libvga.a: $(STATICDIRS)
	(cd $(dir $@); \
	 $(MAKE) -f $(srcdir)/src/Makefile $(notdir $@) \
	 	srcdir="$(srcdir)" DLLFLAGS="" \
	)

ifeq (a.out, $(TARGET_FORMAT))

shared: force_remake $(SHAREDLIBS) $(SVGALIBSHAREDSTUBS)

force_remake:
	@if [ -f sharedlib/DISTRIBUTION ] ; then \
		rm -f sharedlib/DISTRIBUTION ; \
		$(MAKE) shared; \
	fi
	
sharedlib/libsources:
	@mkdir -p sharedlib
	@echo sharedlib/libvga.a sharedlib/libvgagl.a >sharedlib/libsources

DISTRIBUTION = $(shell sh -c "echo sharedlib/DIST*")

ifeq ($(DISTRIBUTION), sharedlib/DISTRIBUTION)

sharedlib/libvga.so.$(VERSION):
	@echo "Ooops, this shouldn\'t happen.. I\'m going to fix it..."
	@rm -f sharedlib/DISTRIBUTION
	@$(MAKE) sharedlib/libvga.so.$(VERSION)

else

sharedlib/libvga.so.$(VERSION): sharedlib/libvga.a sharedlib/libvgagl.a
	(cd sharedlib; \
	 mkstubs -a 0x63b00000 -l libvga -v $(VERSION) -j 0x00001000 -g 4096 \
		-- libvga; \
	 mkstubs -a 0x63b00000 -l libvga -v $(VERSION) -j 0x00001000 -g 4096 \
		-- libvgagl; \
	 mkimage -a 0x63b00000 -l libvga -v $(VERSION) -j 0x00001000 -g 4096 \
		-- $(MKIMAGEFLAGS) libvga.a libvgagl.a \
		`$(CC) --print-libgcc-file-name` -lc; \
	 strip libvga.so.$(VERSION) \
	)

endif	# DISTRIBUTION
	
sharedlib/libvga.a: $(SHAREDDIRS)
	(cd $(dir $@); \
	 $(MAKE) -f $(srcdir)/src/Makefile $(notdir $@) \
	 	srcdir="$(srcdir)" DLLFLAGS="$(DLLFLAGS)" \
	)

else

# ELF

.PHONY: sharedlib/libvga.so.$(VERSION) sharedlib/libvgagl.so.$(VERSION)

shared: $(SHAREDLIBS) $(SVGALIBSHAREDSTUBS)

sharedlib/libvga.so.$(VERSION): $(SHAREDDIRS)
	(cd $(dir $@); \
	 $(MAKE) -f $(srcdir)/src/Makefile $(notdir $@) \
	 	srcdir="$(srcdir)" DLLFLAGS="$(DLLFLAGS)" \
	)

endif	# a.out

sharedlib/libvgagl.a sharedlib/libvgagl.so.$(VERSION): $(SHAREDDIRS)
	(cd $(dir $@); \
	 $(MAKE) -f $(srcdir)/gl/Makefile $(notdir $@) \
	 	srcdir="$(srcdir)" DLLFLAGS="$(DLLFLAGS)" \
	)

demoprogs: $(DEMODIRS)
	@for dir in $(DEMODIRS); do \
		if [ -d $(srcdir)/$$dir ]; then \
			(cd $$dir; \
			$(MAKE) -f $(srcdir)/$$dir/Makefile srcdir="$(srcdir)"); \
		fi \
	done

textutils: $(UTILDIRS)
	(cd utils; \
	$(MAKE) -f $(srcdir)/utils/Makefile srcdir="$(srcdir)")

backup: $(BACKUP)

$(BACKUP): shared distclean
	(cd ..; \
	find svgalib-$(VERSION) ! -type d -print | sort | tar -cvf- -T- ) \
		| gzip -9 >$(BACKUP)

distclean:
	(cd $(srcdir)/src; $(MAKE) clean)
	(cd $(srcdir)/gl; $(MAKE) clean)
	(cd $(srcdir)/utils; $(MAKE) clean)
	(cd $(srcdir)/demos; $(MAKE) clean)
	(cd $(srcdir)/mach32; $(MAKE) clean)
	(cd $(srcdir)/threeDKit; $(MAKE) clean)
	(cd $(srcdir)/jump; rm -f *.s jump.log jump.params jump.undefs)
	(cd jump; rm -f *.s jump.log jump.params jump.undefs)
	find . \( -name '.depend*' -o -name '*~*' \) -exec rm {} \;
	rm -rf sharedlib/[!l]* sharedlib/l[!i]* sharedlib/li[!b]* staticlib
	rm -rf sharedlib/*.a
	mkdir -p sharedlib
	touch sharedlib/DISTRIBUTION
	
clean: distclean
	rm -rf sharedlib

indent:
	find demos gl mach32 src support -name '*.[ch]' -exec indent -kr {} \;
	indent -kr src/*.regs

indent-gnu:
	find demos gl mach32 src support -name '*.[ch]' -exec indent -gnu {} \;
	indent -gnu src/*.regs

FORCE:
