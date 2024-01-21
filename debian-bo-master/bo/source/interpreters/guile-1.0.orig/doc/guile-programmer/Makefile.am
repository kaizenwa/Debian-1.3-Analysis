## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign

## Add our own -I option.
MAKEINFO = makeinfo -I ../guile-user

info_TEXINFOS = guile-programmer.texi

## Just a convenience var; no special significance.
dist_texis = applet-ref.texi gh-ref.texi scm-ref.texi tk.texi \
	ci.texi pi.texi version.texi goonix.texi threads.texi vi.texi \
	guile-programmer.texi ti.texi what-goes.texi libguile-procs.texi

## FIXME: for now we just omit ../guile-user/more-resources.texi from
## the dependencies.  We don't want to try to put it into the
## distribution.
guile_programmer_TEXINFOS = $(dist_texis)

EXTRA_DIST = $(dist_texis)
