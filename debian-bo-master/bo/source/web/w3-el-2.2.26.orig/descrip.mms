# This file kindly written by Richard Levitte - GNU on VMS hacker
#                                               <levitte@vms.stacken.kth.se>
#
# what emacs is called on your system
EMACS = emacs

# How to make a directory
# need a -p if you want to make the parents!
MKDIR = create/dir

# where the Info file should go
INFODIR = gnu_root:[info]

# where the w3 lisp files should go
LISPDIR = gnu_root:[lib.emacs.site-lisp]

# Change this to be where your .emacs file is stored
DOTEMACS      = gnu_root:[lib.emacs.site-lisp]default.el

# Change this to be how to convert texinfo files into info files
# examples:
#	$(EMACS) -batch -q -f batch-texinfo-format
#	makeinfo
MAKEINFO      = makeinfo

############## no user servicable parts beyond this point ###################
# Have to preload a few things to get a nice clean compile

DEPS = -l sys$disk:[]vmsloadup.el

# compile with noninteractive and relatively clean environment
#BATCHFLAGS = -batch -q -no-site-file
BATCHFLAGS = -batch

# files that contain variables and macros that everything else depends on
CORE = docomp.el

OBJECTS = \
	w3.elc,mm.elc,url.elc,w3-beta.elc,w3-draw.elc,w3-e19.elc,	\
	w3-emacs.elc,w3-epoch.elc,w3-mac.elc,w3-mule.elc,w3-next.elc, 	\
	w3-parse.elc,w3-print.elc,w3-srch.elc,w3-vars.elc,		\
	w3-xemac.elc,md5.elc,w3-style.elc,w3-about.elc,base64.elc,	\
	ssl.elc w3-wemac.elc

SOURCES = \
	w3.el,mm.el,url.el,w3-beta.el,w3-draw.el,w3-e19.el,w3-emacs.el,	\
	w3-epoch.el,w3-mac.el,w3-mule.el,w3-next.el,w3-parse.el,	\
	w3-print.el,w3-srch.el,w3-sysdp.el,w3-vars.el,			\
	w3-xemac.el,md5.el,w3-style.el,w3-about.el,base64.el,ssl.el	\
	w3-wemac.el

DISTFILES     = Makefile ChangeLog $(SOURCES) w3.txi docomp.el		\
		W3.ad clean-cache

.SUFFIXES : .elc .el
.el.elc :
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile $(MMS$SOURCE)

w3 :	vmsloadup.el docomp.el $(OBJECTS)
	write sys$output "Build of w3 complete..."

#all :	w3.info w3 emacs
all :	w3
	@ !

install : all emacs
	write sys$output "Installing in $(LISPDIR)"
	if f$parse("$(LISPDIR)") .eqs. "" then $(MKDIR) $(LISPDIR)
	copy/log $(SOURCES),$(OBJECTS),w3.elc $(LISPDIR)
	- purge/log $(LISPDIR)
	copy/log w3.info* $(INFODIR)
	- purge/log $(INFODIR)

emacs :	
	write sys$output "Adding w3 setup to $(DOTEMACS)"
	$(EMACS) -batch -l docomp.el -f hack-dot-emacs $(DOTEMACS) \
		 $(LISPDIR)

clean :
	! rm -f $(OBJECTS)

w3.info :	w3.txi
	@$(MAKEINFO) w3.txi

w3.dvi :	w3.txi
	tex w3.txi
	texindex w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr
	tex w3.txi
	rm -f 	w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr 	\
		w3.cps w3.fns w3.kys w3.pgs w3.tps w3.vrs	\
		w3.log w3.toc w3.aux
