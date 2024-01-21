offixdir = @offixdir@
offix_DATA = editor.help

EXTRA_DIST = Editor.mutekeys Editor.nomute editor.xbm editor.xpm \
	editor_mask.xbm editor.help

man_MANS = editor.1
noinst_HEADERS = accent.h icon.h patchlevel.h xedit.h
bin_PROGRAMS = editor
editor_SOURCES = xedit.c icon.c dnd.c jump.c edit.c file.c warning.c \
	search.c file_sel.c special.c commands.c accent.c \
	Editor.mutekeys.ad.h Editor.nomute.ad.h
LDADD = $(X_EXTRA_LIBS) -lDnd -lXaw -lXmu -lXt -lXext \
	$(X_PRE_LIBS) -lX11 -lm $(X_LIBS)
INCLUDES = -I$(top_srcdir) $(X_CFLAGS) \
	-DHELPFILE=\"$(offixdir)/editor.help\" 

Editor.nomute.ad.h: Editor.nomute
	cd $(srcdir) && $(top_srcdir)/ad2c $< > $@
Editor.mutekeys.ad.h: Editor.mutekeys
	cd $(srcdir) && $(top_srcdir)/ad2c $< > $@


