AUTOMAKE_OPTIONS = foreign
man_MANS = clipboard.1
bin_PROGRAMS = clipboard
clipboard_SOURCES = xclipboard.c icon.c icon.h Clipboard.ad.h
LDADD = $(X_EXTRA_LIBS) -lDnd -lXaw -lXmu -lXt -lXext \
	$(X_PRE_LIBS) -lX11 -lm $(X_LIBS)
INCLUDES = $(X_CFLAGS) 
EXTRA_DIST = Clipboard.ad clipboard.xbm clipboard.xpm clipboard_mask.xbm \
		offix.m4 ad2c

Clipboard.ad.h: Clipboard.ad
	cd $(srcdir) && $(top_srcdir)/ad2c $< > $@



