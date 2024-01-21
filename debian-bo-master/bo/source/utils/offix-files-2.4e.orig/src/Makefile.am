offixdir = @offixdir@
SUBDIRS = bitmaps external pixmaps regexp

man_MANS = files.1
bin_PROGRAMS = files
files_SOURCES = Bitmaps.c Chmod.c Comms.c Confirm.c Delete.c Dirs.c Dnd.c \
	Errors.c Exec.c Fw.c FwActions.c FwCb.c Info.c Main.c Ops.c Popup.c \
	Utils.c magic.c Comms.h Files.h magic.h Files.ad.h
LDADD = -Lexternal -Lregexp -lexternal -lregx $(X_EXTRA_LIBS) -lDnd \
		-lXaw -lXmu -lXt -lXext $(X_PRE_LIBS) -lX11 -lm $(X_LIBS)
INCLUDES = -I$(top_srcdir) $(X_CFLAGS) \
	-DROOTDROP_DEFAULT_ICON=\"linux_make.xpm\" \
	-DRCFILE=\"$(offixdir)/Filesrc\" \
	-DMAGICFILE=\"$(offixdir)/FilesMagic\" \
	-DPIXDIR=\"$(includedir)/pixmaps\"
offix_DATA = FilesMagic Filesrc
EXTRA_DIST = Files.ad xfmtype.c $(offix_DATA)

Files.ad.h: Files.ad
	cd $(srcdir) && $(top_srcdir)/ad2c $< > $@
