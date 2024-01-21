#include "unix.h"

#ifdef TEMPNAM    /* Make sure only one of these is defined (if any) */
#  undef TMPNAM   /* Order of preference: tempnam, mktemp, tmpnam */
#  undef MKTEMP
#endif
#ifdef MKTEMP
#  undef TMPNAM
#  undef TEMPNAM
#endif
#ifdef TMPNAM
#  undef TEMPNAM
#  undef MKTEMP
#endif

static Object P_Tempname(argc, argv) int argc; Object *argv; {
    char *name, *dir = 0, *pref = 0;
    Object ret;
#ifdef TMPNAM
    extern char *tmpnam();
#else
#ifdef TEMPNAM
    extern char *tempnam();
#else
    char buf[1024];
#ifdef MKTEMP
    extern char *mktemp();
#else
    char *p, *q;
    struct stat st;
#endif
#endif
#endif

    if (argc > 0)
	dir = Get_Strsym(argv[0]);
    if (argc > 1)
	pref = Get_Strsym(argv[1]);
#ifdef TMPNAM
    name = tmpnam((char *)0);
#else
#ifdef TEMPNAM
    Disable_Interrupts;        /* Make sure result gets freed */
    name = tempnam(dir, pref);
#else
    if (!dir) dir = "/tmp";
    if (!pref) pref = "elk";
    if (strlen(dir) + strlen(pref) > 1000)
	Primitive_Error("directory/prefix argument too long");
#ifdef MKTEMP
    sprintf(buf, "%s/%sXXXXXX", dir, pref);
    name = mktemp(buf);
#else
    name = 0;
    sprintf(buf, "%s/%sa%d", dir, pref, getpid());
    p = buf+strlen(dir)+strlen(pref)+1;
    while (stat(buf, &st) == 0) {         /* Simple ersatz mktemp */
	q = p;
	while (1) {
	    if (*q == '\0') goto fail;
	    if (*q == 'z') {
		*q++ = 'a';
	    } else {
		if (*q >= '0' && *q <= '9')
		    *q = 'a';
		else
		    *++q;
		break;
	    }
	}
    }
    if (errno == ENOENT)
	name = buf;
fail: ;
#endif
#endif
#endif
    if (name == 0 || name[0] == '\0') {
	Enable_Interrupts;
	Raise_Error("cannot create temp file name");
    }
    ret = Make_String(name, strlen(name));
#ifdef TEMPNAM
    free(name);
    Enable_Interrupts;
#endif
    return ret;
}

elk_init_unix_temp() {
    Def_Prim(P_Tempname,           "unix-tempname",             0, 2, VARARGS);
}
