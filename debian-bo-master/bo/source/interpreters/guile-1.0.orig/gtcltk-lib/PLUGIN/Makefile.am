## Process this file with Automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = REQ guile.config.in

noinst_DATA = guile.inits guile.libs guile.config

guile.inits: Makefile
	: > tmpinit; if test -n "$(GTCLTK)"; then \
	  echo 'scm_init_gtcl scm_init_gtk' >> tmpinit; \
	fi; \
	mv tmpinit guile.inits

guile.libs: Makefile
	: > tmpinit; if test -n "$(GTCLTK)"; then \
	  echo '-lgtcltk -ltk4.1' >> tmpinit; \
	  echo '-ltk4.1 -ltcl7.5' >> tmpinit; \
	  echo '-ltcl7.5 -lguile' >> tmpinit; \
	  echo '-ltcl7.5 -lm'     >> tmpinit; \
	fi; \
	mv tmpinit guile.libs

guile.config: Makefile
	: > tmpinit; if test -n "$(GTCLTK)"; then \
	  echo 'xtra_cflags="-L../gtcltk-lib $(X_CFLAGS) $$xtra_cflags"' >> tmpinit; \
	  echo 'xtra_libs="$(X_PRE_LIBS) $(X_LIBS) -lX11 $(X_EXTRA_LIBS) $(TCL_EXTRA_LIBS) $$xtra_libs"' >> tmpinit; \
	  echo 'xtra_dependencies="../gtcltk-lib/libgtcltk.a $$xtra_dependencies"' >> tmpinit; \
	fi; \
	mv tmpinit guile.config
