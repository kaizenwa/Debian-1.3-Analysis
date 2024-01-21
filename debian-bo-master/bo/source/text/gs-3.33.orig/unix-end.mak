#    Copyright (C) 1994, 1995 Aladdin Enterprises.  All rights reserved.
# 
# This file is part of GNU Ghostscript.
# 
# GNU Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
# anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the GNU Ghostscript General Public License for full details.
# 

# Partial makefile common to all Unix and Desqview/X configurations.

# This is the very last part of the makefile for these configurations.
# Since Unix make doesn't have an 'include' facility, we concatenate
# the various parts of the makefile together by brute force (in tar_cat).

# The rule for gconfigv.h is here because it is shared between Unix and
# DV/X environments.
gconfigv.h: unix-end.mak $(MAKEFILE) echogs$(XE)
	$(EXP)echogs -w gconfigv.h -x 23 define USE_ASM -x 2028 -q $(USE_ASM)-0 -x 29
	$(EXP)echogs -a gconfigv.h -x 23 define USE_FPU -x 2028 -q $(FPU_TYPE)-0 -x 29

# The following rules are equivalent to what tar_cat does.
GS_MAK=gs.mak jpeg.mak devs.mak
unix.mak: dvx-gcc.mak unixansi.mak unix-cc.mak unix-gcc.mak

DVX_GCC_MAK=dgc-head.mak dvx-head.mak $(GS_MAK) dvx-tail.mak unix-end.mak
dvx-gcc.mak: $(DVX-GCC_MAK)
	$(CAT) $(DVX-GCC_MAK) >dvx-gcc.mak

UNIXANSI_MAK=ansihead.mak unixhead.mak $(GS_MAK) unixtail.mak unix-end.mak
unixansi.mak: $(UNIXANSI_MAK)
	$(CAT) $(UNIXANSI_MAK) >unixansi.mak

UNIX_CC_MAK=cc-head.mak unixhead.mak $(GS_MAK) unixtail.mak unix-end.mak
unix-cc.mak: $(UNIX_CC_MAK)
	$(CAT) $(UNIX_CC_MAK) >unix-cc.mak

UNIX_GCC_MAK=gcc-head.mak unixhead.mak $(GS_MAK) unixtail.mak unix-end.mak
unix-gcc.mak: $(UNIX_GCC_MAK)
	$(CAT) $(UNIX_GCC_MAK) >unix-gcc.mak

# Installation

TAGS:
	etags -t *.c *.h

install: install-exec install-data

# The sh -c in the rules below is necessary because the Ultrix `make'
# uses sh -e, which terminates execution of a command if any error occurs,
# even if the command traps the error with ||.

install-exec: $(GS)
	-mkdir $(bindir)
	$(INSTALL_PROGRAM) $(GS) $(bindir)/$(GS)
	-mkdir $(scriptdir)
	sh -c 'for f in gsbj gsdj gsdj500 gslj gslp gsnd bdftops font2c \
ps2ascii ps2epsi wftopfa ;\
	do if ( test -f $$f ) then $(INSTALL_PROGRAM) $$f $(scriptdir)/$$f; fi;\
	done'

install-data: gs.1
	-mkdir $(mandir)
	-mkdir $(man1dir)
	for f in gs ps2epsi ;\
	do $(INSTALL_DATA) $$f.1 $(man1dir)/$$f.$(manext) ;\
	done
	-mkdir $(datadir)
	-mkdir $(gsdir)
	-mkdir $(gsdatadir)
	sh -c 'for f in gslp.ps gs_init.ps gs_btokn.ps gs_ccfnt.ps gs_cidfn.ps \
gs_dps1.ps gs_fonts.ps gs_kanji.ps gs_lev2.ps gs_pfile.ps gs_res.ps \
gs_setpd.ps gs_statd.ps gs_type0.ps gs_type1.ps \
gs_dbt_e.ps gs_iso_e.ps gs_ksb_e.ps gs_std_e.ps gs_sym_e.ps \
quit.ps Fontmap bdftops.ps decrypt.ps font2c.ps impath.ps landscap.ps \
level1.ps packfile.ps prfont.ps printafm.ps ps2ascii.ps ps2epsi.ps \
ps2image.ps pstoppm.ps showpage.ps type1enc.ps type1ops.ps \
wftopfa.ps wrfont.ps \
gs_pdf.ps pdf_base.ps pdf_draw.ps pdf_font.ps pdf_main.ps pdf_2ps.ps \
gs_mex_e.ps gs_mro_e.ps gs_pdf_e.ps gs_wan_e.ps ;\
	do if ( test -f $$f ) then $(INSTALL_DATA) $$f $(gsdatadir)/$$f; fi;\
	done'
	-mkdir $(docdir)
	sh -c 'for f in COPYING NEWS PUBLIC README current.doc devices.doc \
drivers.doc fonts.doc gs.1 hershey.doc history1.doc history2.doc humor.doc \
language.doc lib.doc make.doc ps2epsi.1 ps2epsi.doc psfiles.doc public.doc \
use.doc xfonts.doc ;\
	do if ( test -f $$f ) then $(INSTALL_DATA) $$f $(docdir)/$$f; fi;\
	done'
	-mkdir $(exdir)
	for f in chess.ps cheq.ps colorcir.ps golfer.ps escher.ps \
snowflak.ps tiger.ps ;\
	do $(INSTALL_DATA) $$f $(exdir)/$$f ;\
	done
