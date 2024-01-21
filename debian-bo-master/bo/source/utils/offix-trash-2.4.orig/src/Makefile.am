EXTRA_DIST = Trash.ad trash.xbm trash.xpm trash_full.xbm trash_full.xpm \
	trash_full_mask.xbm trash_mask.xbm

bin_PROGRAMS = trash
trash_SOURCES = Files.cc Icon.cc List.cc SortedList.cc\
	 Trash.cc Trash.h Trash.ad.h List.h SortedList.h
LDADD =  $(X_EXTRA_LIBS) -lDnd++ -lXaw -lXmu -lXt \
        -lXext $(X_PRE_LIBS) -lX11 -lm $(X_LIBS)
INCLUDES = -I$(top_srcdir) $(X_CFLAGS)

$(srcdir)/Trash.ad.h: $(srcdir)/Trash.ad
	cd $(srcdir) && $(top_srcdir)/ad2c $< > $@
