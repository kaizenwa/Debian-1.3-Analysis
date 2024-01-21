# Sed commands to finish translating the grez Unix makefile into MPW syntax.

# Whack out unused host define bits.
/HDEFINES/s/@HDEFINES@//

# Fix and add to the include paths.
/^INCLUDES = .*$/s/$/ -i "{INCDIR}":mpw: -i ::extra-include:/
/BFDDIR/s/-i {BFDDIR} /-i "{BFDDIR}": /
/INCDIR/s/-i {INCDIR} /-i "{INCDIR}": /

# Use byacc instead of bison (for now anyway).
/BISON/s/^BISON =.*$/BISON = byacc/
#/BISONFLAGS/s/^BISONFLAGS =.*$/BISONFLAGS = /

# Fix pathnames to various files.
/config.h/s/"{s}"config\.h/"{o}"config.h/g
/config.h/s/^config\.h/"{o}"config.h/

/y.tab.c/s/"{s}"y\.tab\.c/"{o}"y.tab.c/g
/y.tab.c/s/^y\.tab\.c/"{o}"y.tab.c/
/y.tab.h/s/"{s}"y\.tab\.h/"{o}"y.tab.h/g
/y.tab.h/s/^y\.tab\.h/"{o}"y.tab.h/
/rez.c/s/"{s}"rez\.c/"{o}"rez.c/g
/rez.c/s/^rez\.c/"{o}"rez.c/
/rez.h/s/"{s}"rez\.h/"{o}"rez.h/g
/rez.h/s/^rez\.h/"{o}"rez.h/

/"{s}"{INCDIR}/s/"{s}"{INCDIR}/"{INCDIR}"/g

# Rename the resource file correctly.
/grez\.r/s/"{s}"grez\.r/"{s}"mac-grez.r/

# Remove un-useful targets.
/^Makefile \\Option-f/,/^$/d
/^"{o}"config.h \\Option-f/,/^$/d
/^config.status \\Option-f/,/^$/d
