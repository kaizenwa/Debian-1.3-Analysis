/* 
 * routines to safely overwrite a file by first writing to a temporary
 * file, and renaming to the real file only when sure that the temporary
 * file has been successfully created.
 */
#include <stdio.h>
#include <string.h>
#include "libcnews.h"
#include "safe.h"

/* imports */
extern int errno;
extern char *mktemp();

/*
 * Warning:  if fname is relative, and the caller chdirs after calling
 * safeopen, safeclose will not work correctly.
 */
SFILE *
safeopen(fname, makedirs)
char *fname;
int makedirs;		/* non-zero means create intervening directories. */
{
	SFILE *sfp = (SFILE *) emalloc(sizeof(SFILE));
	char *cp;
	sizeint prefixlen;
#define SAFE_TEMPLATE	"safe.XXXXXX"

	sfp->sf_name = strsave(fname);
	cp = strrchr(fname, '/');
	if (cp != NULL)
		prefixlen = cp - fname + 1;
	else
		prefixlen = 0;
	sfp->sf_tmp = emalloc(prefixlen + sizeof SAFE_TEMPLATE);
	sfp->sf_tmp[prefixlen] = '\0';
	if (prefixlen) {
		(void) strncpy(sfp->sf_tmp, fname, prefixlen);
		if (makedirs && mkdirs(sfp->sf_tmp, getuid(), getgid()) == 0)
			error("mkdirs(%s) failed", sfp->sf_tmp);
	}
	(void) strcpy(sfp->sf_tmp + prefixlen, SAFE_TEMPLATE);
	(void) mktemp(sfp->sf_tmp);
	sfp->sf_fp = efopen(sfp->sf_tmp, "w");
	return sfp;
}

/*
 * aborts a safe file.  It closes, unlinks and frees everything
 * associated with sfp, which should not be used again after this.
 */
void
safeabort(sfp)
SFILE *sfp;
{
	(void) fclose(sfp->sf_fp);
	(void) unlink(sfp->sf_tmp);
	(void) free(sfp->sf_tmp);
	(void) free(sfp->sf_name);
	(void) free((char *) sfp);
}

/*
 * closes sfp and renamed it safely.  Returns -1 on error, 0 on success.
 * sfp should not be used after calling this routine.
 */
int
safeclose(sfp)
SFILE *sfp;
{
	int saverrno;
	
	if (fclose(sfp->sf_fp) != 0) {
		saverrno = errno;
		safeabort(sfp);
		errno = saverrno;
		return -1;
	}
	if (rename(sfp->sf_tmp, sfp->sf_name) < 0) {
		saverrno = errno;
		safeabort(sfp);
		errno = saverrno;
		return -1;
	}
	safeabort(sfp);
	return 0;
}

#ifdef TEST
int
main(argc, argv)
int argc;
char **argv;
{
	SFILE *sfp;
	char buf[1024];
	
	if (argc != 2) {
		fprintf(stderr, "Usage: safe filename\n");
		exit(1);
	}
	sfp = safeopen(argv[1], 1);
	if (sfp == NULL)
		error("safeopen(%s) failed", argv[1]);
	while (fgets(buf, sizeof buf, stdin) != NULL)
		(void) fputs(buf, safefp(sfp));
	if (safeclose(sfp) < 0)
		error("safeclose(%s) failed", argv[1]);
	return 0;
}
#endif /* TEST */
