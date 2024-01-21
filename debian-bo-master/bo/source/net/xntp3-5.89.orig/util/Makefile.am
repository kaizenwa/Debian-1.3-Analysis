AUTOMAKE_OPTIONS = no-dependencies
bin_PROGRAMS = @MAKE_TICKADJ@ @MAKE_NTPTIME@
EXTRA_PROGRAMS = byteorder hist jitter kern longsize ntptime \
precision tickadj testrs6000 timetrim

INCLUDES = -I$(top_srcdir)/include
# LDADD might need RESLIB and ADJLIB
LDADD =	../libntp/libntp.a
#EXTRA_DIST = README TAGS
EXTRA_DIST = 
ETAGS_ARGS = Makefile.am

../libntp/libntp.a:
	cd ../libntp && $(MAKE)
