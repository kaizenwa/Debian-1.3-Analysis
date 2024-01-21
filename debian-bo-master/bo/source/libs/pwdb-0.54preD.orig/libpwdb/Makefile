# The libpwdb Makefile
#
# this compiles only as a static lib, so all functions withing this lib
# will be statically linked in the modules which makes use of it.
#

dummy:
	@echo "*** This is not a top-level Makefile!"

# ///////////////////////////////////////////////////////////////////

LIBNAME = pwdb
MAJOR=.0
MINOR=.54

FILES = common/commonio common/grcommon common/lockpw common/misc \
	unix/group unix/passwd unix/pwio unix/groupio \
	shadow/gshadow shadow/shadow shadow/shadowio shadow/sgroupio \
	nis/group nis/password \
	radius/radius radius/util radius/md5 radius/helpfunc radius/dict \
	pwdb/pwdb_error pwdb/pwdb_helper pwdb/pwdb_locate \
	pwdb/pwdb_module pwdb/pwdb_start \
	posix/pwd posix/grp posix/getlogin posix/undefined

HEADERS	= pwdb/pwdb_public.h pwdb/pwdb_config.h pwdb/pwdb_module.h

CFLAGS+=-I. 	# needed for generic interface compilation
		# if header files are not installed (CG)

CFLAGS+=# -DDEBUG

#
# Probably no need to alter anything below here.
#

LIBDYNAME=lib$(LIBNAME).so
LIBDYNMAJ=$(LIBDYNAME)$(MAJOR)
LIBDYNMIN=$(LIBDYNMAJ)$(MINOR)

LIBSTATIC = lib$(LIBNAME).a

# sources and object files

LIBSRC = $(addsuffix .c,$(FILES))
LIBOBJ = $(addsuffix .o,$(FILES))


# rules
%.o : %.c $(HEADERS)
	$(CC) $(CFLAGS) $(DYNAMIC) $(CPPFLAGS) $(TARGET_ARCH) -c $< -o $@

all: $(LIBSTATIC) $(LIBDYNAME) 


$(LIBDYNAME): $(LIBOBJ)
	$(LD) -soname $(LIBDYNMAJ) -x -shared -o $@ $(LIBOBJ)

$(LIBSTATIC): $(LIBOBJ)
	$(AR) $@ $(LIBOBJ)
	$(RANLIB) $@

install: all
	$(MKDIR) $(INCLUDED)
	$(INSTALL) -m 644 pwdb/pwdb_public.h $(INCLUDED)
	$(INSTALL) -m 644 ./pwdb_map.h $(INCLUDED)
	$(INSTALL) -m 644 unix/public.h $(INCLUDED)/pwdb_unix.h
	$(INSTALL) -m 644 shadow/public.h $(INCLUDED)/pwdb_shadow.h
	$(INSTALL) -m 644 common/public.h $(INCLUDED)/pwdb_common.h
	$(INSTALL) -m 644 radius/public.h $(INCLUDED)/pwdb_radius.h
	$(INSTALL) -m 644 radius.h $(INCLUDED)/radius.h
	$(INSTALL) -m 644 _pwdb_macros.h $(INCLUDED)/_pwdb_macros.h
	$(INSTALL) -m 644 $(LIBDYNAME) $(LIBDIR)/$(LIBDYNMIN)
	$(LDCONFIG)
	( cd $(LIBDIR) ; ln -sf $(LIBDYNMAJ) $(LIBDYNAME) )
	$(INSTALL) -m 644 $(LIBSTATIC) $(LIBDIR)

clean:
	@echo Cleaning up...
	@rm -vf *.so *.a core a.out `find . -name "*.o" -print` `find . -name "*~" -print`
	make -C pwdb/interface clean

remove:
	rm -f $(INCLUDED)/pam_public.h
	rm -f $(INCLUDED)/pam_map.h
	rm -f $(LIBDIR)/$(LIBDYNAME)*
	$(LDCONFIG)
	rm -f $(LIBDIR)/$(LIBSTATIC)

.c.o:
	$(CC) -c $(DEFS) $(CFLAGS) $<

extraclean: clean

# hardcoded dependencies
pwdb/pwdb_module.o: pwdb/pwdb_module.c pwdb/helpers.c \
	pwdb/interface/unix.c pwdb/interface/shadow.c \
	pwdb/interface/radius.c pwdb/interface/nis.c \
	pwdb/interface/unix/user.c pwdb/interface/unix/group.c \
	pwdb/interface/shadow/user.c pwdb/interface/shadow/group.c \
	pwdb/interface/radius/user.c pwdb/interface/radius/group.c \
	pwdb/interface/nis/user.c pwdb/interface/nis/group.c
