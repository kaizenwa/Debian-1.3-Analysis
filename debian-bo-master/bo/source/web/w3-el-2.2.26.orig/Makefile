# what emacs is called on your system
EMACS = emacs

# How to make a directory
# need a -p if you want to make the parents!
MKDIR = mkdir

# Various other stuff used
RM    = rm -f
CP    = cp

# where the Info file should go
INFODIR = ../info

# where the w3 lisp files should go
LISPDIR = $$HOME/lisp

# Change this to be where your .emacs file is stored
DOTEMACS      = $$HOME/.emacs

# Change this to be how to convert texinfo files into info files
# examples:
#	$(EMACS) -batch -q -f batch-texinfo-format
#	makeinfo
MAKEINFO      = makeinfo

############## no user servicable parts beyond this point ###################
# Have to preload a few things to get a nice clean compile

DEPS = -l ./docomp.el -l ./w3-vars.el -l ./url.el

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file

# files that contain variables and macros that everything else depends on
CORE = docomp.el

OBJECTS = \
	url-hash.elc w3.elc mm.elc url.elc w3-beta.elc w3-draw.elc	\
	w3-e19.elc w3-emacs.elc w3-epoch.elc w3-mac.elc w3-mule.elc	\
	w3-next.elc w3-parse.elc w3-print.elc w3-srch.elc w3-vars.elc	\
	w3-xemac.elc md5.elc w3-style.elc w3-about.elc base64.elc	\
	ssl.elc w3-wemac.elc urlauth.elc w3-hot.elc xpm-button.elc	\
	xbm-button.elc font.elc w3-sysdp.elc

SOURCES = \
	url-hash.el w3.el mm.el url.el w3-beta.el w3-draw.el		\
	w3-e19.el w3-emacs.el w3-epoch.el w3-mac.el w3-mule.el		\
	w3-next.el w3-parse.el w3-print.el w3-srch.el w3-vars.el	\
	w3-xemac.el md5.el w3-style.el w3-about.el base64.el		\
	ssl.el w3-wemac.el urlauth.el w3-hot.el xpm-button.el		\
	xbm-button.el font.el w3-sysdp.el

DISTFILES     = Makefile ChangeLog $(SOURCES) w3.txi docomp.el		\
		W3.ad clean-cache

w3:	docomp.el $(OBJECTS)
	@echo Build of w3 complete...

all:	w3.info w3 emacs

install: all
	@echo Installing in $(LISPDIR)
	if [ ! -d $(LISPDIR) ] ; then $(MKDIR) $(LISPDIR) ; fi
	(cd $(LISPDIR) ; $(RM) -f $(SOURCES) $(OBJECTS) w3-sysdp.el*)
	$(CP) $(SOURCES) $(OBJECTS) $(LISPDIR)
	if [ ! -d $(INFODIR) ] ; then $(MKDIR) $(INFODIR) ; fi
	(cd $(INFODIR) ; $(RM) -f w3.info*)
	$(CP) w3.info* $(INFODIR)

emacs:	
	@echo Adding w3 setup to $(DOTEMACS)
	$(EMACS) -batch -q -l docomp.el -f hack-dot-emacs $(DOTEMACS) \
		 $(LISPDIR)

clean:
	$(RM) $(OBJECTS)

w3.info:	w3.txi
	@$(MAKEINFO) w3.txi

w3.dvi:		w3.txi
	tex w3.txi
	texindex w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr
	tex w3.txi
	$(RM) 	w3.cp  w3.fn  w3.ky  w3.pg  w3.tp  w3.vr 	\
		w3.cps w3.fns w3.kys w3.pgs w3.tps w3.vrs	\
		w3.log w3.toc w3.aux

w3-emacs.elc:	w3-emacs.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-emacs.el

w3-mac.elc:	w3-mac.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-mac.el

w3-e19.elc:	w3-e19.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-e19.el

w3-epoch.elc:	w3-epoch.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-epoch.el

w3-next.elc:	w3-next.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-next.el

w3-mule.elc:	w3-mule.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-mule.el

w3-vars.elc:	w3-vars.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-vars.el

w3-srch.elc: w3-srch.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-srch.el

mm.elc: mm.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile mm.el

url.elc: url.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile url.el

w3-xemac.elc: w3-xemac.el w3-vars.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-xemac.el

w3-parse.elc: w3-parse.el w3-vars.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-parse.el

w3-draw.elc: w3-draw.el w3-vars.elc
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-draw.el

w3-beta.elc: w3-beta.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-beta.el

w3-print.elc: w3-print.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-print.el

w3.elc: w3.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3.el

w3-style.elc: w3-style.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-style.el

md5.elc: md5.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile md5.el

w3-about.elc: w3-about.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-about.el

base64.elc: base64.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile base64.el

ssl.elc: ssl.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile ssl.el

w3-wemac.elc: w3-wemac.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-wemac.el

urlauth.elc: urlauth.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile urlauth.el

w3-hot.elc: w3-hot.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-hot.el

xpm-button.elc: xpm-button.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile xpm-button.el

xbm-button.elc: xbm-button.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile xbm-button.el

url-hash.elc: url-hash.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile url-hash.el

w3-toolbar.elc: w3-toolbar.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-toolbar.el

font.elc: font.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile font.el

w3-sysdp.elc: w3-sysdp.el
	$(EMACS) $(BATCHFLAGS) $(DEPS) -f batch-byte-compile w3-sysdp.el
