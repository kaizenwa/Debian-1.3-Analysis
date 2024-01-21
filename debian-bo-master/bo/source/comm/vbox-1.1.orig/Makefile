
#-------------------------------------------------------------------------#
# Path for the vbox spool directory (please do not include the trailing   #
# slash).                                                                 #
#-------------------------------------------------------------------------#

SPOOLDIR		= /var/spool/vbox

#-------------------------------------------------------------------------# 
# Path where vbox (the console message viewer) should be installed.       #
#-------------------------------------------------------------------------#

PATH_VBOX_TOOLS		= /usr/local/bin

#-------------------------------------------------------------------------#
# Path where vboxgetty (the main answering machine) should be installed.  #
#-------------------------------------------------------------------------#

PATH_VBOX_GETTY		= /usr/local/sbin

#-------------------------------------------------------------------------#
# Path where addons (additional tools) should be installed. The addons    #
# are vboxplay, vboxmail, vboxmime and the rmdtools.                      #
#-------------------------------------------------------------------------#

PATH_VBOX_ADDON		= /usr/local/lib/vbox

#-------------------------------------------------------------------------#
# Program to install vbox.                                                #
#-------------------------------------------------------------------------#

INSTALL_VBOX_TOOLS      = install -c -o root -g bin -m 755

#-------------------------------------------------------------------------#
# Program to install the vbox addons.                                     #
#-------------------------------------------------------------------------#

INSTALL_VBOX_ADDON      = install -c -o root -g bin -m 755

#-------------------------------------------------------------------------#
# Program to install vboxgetty (runs only by root).                       #
#-------------------------------------------------------------------------#

INSTALL_VBOX_GETTY	= install -c -o root -g bin -m 700

#-------------------------------------------------------------------------#
# Compiler & Compiler flags.                                              #
#-------------------------------------------------------------------------#

CC			= gcc
CCFLAGS			= -i486 -Wall -O2

#-------------------------------------------------------------------------#
# Names for some needed programs.                                         #
#-------------------------------------------------------------------------#

MAKE			= make
FIND			= find
RM			= rm
MKDIR			= mkdir -p -m 755
STRIP			= strip

###########################################################################
##                                                                       ##
##        END OF USER CONFIGURATION - DO NOT EDIT BELOW THIS LINE!       ##
##                                                                       ##
###########################################################################

VERSION	= 1.1

SUBDIRS	= tools/xvboxled tools/mam

#-- exports --------------------------------------------------------------#

export CC
export CCFLAGS
export SPOOLDIR
export VERSION
export SUBDIRS

#-- all ------------------------------------------------------------------#

all:	ignore
	@echo ""
	@echo "Be sure you are root and then do one ore more of the following:"
	@echo ""
	@echo "make vbox               - Compiles the vbox package v$(VERSION)."
	@echo "make vbox-install       - Installs the vbox package v$(VERSION)."
	@echo "make vbox-spooldir      - Installs the spool directory ($(SPOOLDIR))."
	@echo ""
	@echo "make clean              - Deletes *all* backups."
	@echo "make clear              - Deletes *all* backups, objects and executables."
	@echo ""
	@echo "To compile or install the external tools you can do the following:"
	@echo ""
	@echo "make mam                - Compiles the mam package."
	@echo "make mam-install        - Installs the mam package."
	@echo ""
	@echo "make xvboxled           - Compiles the xvboxled package."
	@echo "make xvboxled-install   - Installs the xvboxled package."
	@echo ""
	@echo "make tools              - Compiles *all* packages in the tools directory."
	@echo "make tools-install      - Installs *all* packages in the tools directory."
	@echo ""
	@echo "Note: Please *read* the instructions in the subdirectories of each external"
	@echo "      program - some of them need a seperate setup!"
	@echo ""

#-- external tools -------------------------------------------------------#

tools:			ignore
			@set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i; done

tools-install:		ignore
			@set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i install; done

xvboxled:		ignore
			@$(MAKE) -C tools/xvboxled

xvboxled-install:	xvboxled
			@$(MAKE) -C tools/xvboxled install

mam:			ignore
			@$(MAKE) -C tools/mam

mam-install:		mam
			@$(MAKE) -C tools/man install

#-- cleanups -------------------------------------------------------------#

rmbak:			ignore
			@$(FIND) . -name '*~' -exec $(RM) {} ';'
			@$(FIND) . -name '*#' -exec $(RM) {} ';'

clean:			rmbak
			@for i in $(SUBDIRS) src; do $(MAKE) -C $$i clean; done

clear:			clean
			@for i in $(SUBDIRS) src; do $(MAKE) -C $$i clear; done

#-- vbox package ---------------------------------------------------------#

vbox:			ignore
			@$(MAKE) -C src

vbox-install:		vbox
			@$(MKDIR) $(PATH_VBOX_GETTY)
			@$(MKDIR) $(PATH_VBOX_TOOLS)
			@$(MKDIR) $(PATH_VBOX_ADDON)
			@$(STRIP) ./bin/vboxgetty
			@$(STRIP) ./bin/vbox
			@$(STRIP) ./bin/rmdcatheader
			@$(STRIP) ./bin/rmdcutheader
			@$(STRIP) ./bin/rmdgetheader
			$(INSTALL_VBOX_GETTY) ./bin/vboxgetty    $(PATH_VBOX_GETTY)
			$(INSTALL_VBOX_TOOLS) ./bin/vbox         $(PATH_VBOX_TOOLS)
			$(INSTALL_VBOX_ADDON) ./bin/rmdcatheader $(PATH_VBOX_ADDON)
			$(INSTALL_VBOX_ADDON) ./bin/rmdcutheader $(PATH_VBOX_ADDON)
			$(INSTALL_VBOX_ADDON) ./bin/rmdgetheader $(PATH_VBOX_ADDON)

vbox-spooldir:		ignore
			@$(MKDIR) $(SPOOLDIR)

#-- release --------------------------------------------------------------#

release:		ignore
			@scripts/release

#-- ignore me :-) --------------------------------------------------------#

ignore:
