/*
**  VMS readdir() routines.
**  Written by Rich $alz, <rsalz@bbn.com> in August, 1990.
**  This code has no copyright.
*/

/* 12-NOV-1990 added d_namlen field and special case "." name -GJC@MITECH.COM 
 * 31-DEC-1992 modified for use with VMS-FSP 2.6.5jt.2 -S.A.Pechler@bdk.tue.nl
 * 08-FEB-1993 Optimized for use with VMS-FSP 2.6.5jt.3 -S.A.Pechler@bdk.tue.nl
 * 07-MAY-1993 Modified for use with VMS-FSP V2.7.0 -S.A.Pechler@bdk.tue.nl
 */

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <descrip.h>
#include <rmsdef.h>
#include "dirent.h"

    /* Uncomment the next line to get a test routine. */
/*#define TEST*/

    /* Number of elements in vms_versions array */
#define VERSIZE(e)	(sizeof e->vms_versions / sizeof e->vms_versions[0])

    /* Linked in later. */
extern char	*malloc();
extern char	*strrchr();
extern char	*strcpy();


/*
**  Open a directory, return a handle for later use.
*/
DIR *
opendir(name)
    char	*name;
{
    DIR		*dd;

    /* Get memory for the handle, and the pattern. */
    if ((dd = (DIR *)malloc(sizeof *dd)) == NULL) {
	errno = ENOMEM;
	return NULL;
    }
    
    if (strcmp(".",name) == 0) name = "";
    
    dd->pattern = malloc((unsigned int)(strlen(name) + sizeof "*.*" + 1));
    if (dd->pattern == NULL) {
	free((char *)dd);
	errno = ENOMEM;
	return NULL;
    }

    /* Fill in the fields; mainly playing with the descriptor. */
    (void)sprintf(dd->pattern, "%s*.*", name);
    dd->context = 0;
    dd->vms_wantversions = 0;
    dd->pat.dsc$a_pointer = dd->pattern;
    dd->pat.dsc$w_length = strlen(dd->pattern);
    dd->pat.dsc$b_dtype = DSC$K_DTYPE_T;
    dd->pat.dsc$b_class = DSC$K_CLASS_S;

    return dd;
}


/*
**  Set the flag to indicate we want versions or not.
*/
void
vmsreaddirversions(dd, flag)
    DIR		*dd;
    int		flag;
{
    dd->vms_wantversions = flag;
}


/*
**  Free up an opened directory.
*/
void
closedir(dd)
    DIR		*dd;
{
    free(dd->pattern);
    free((char *)dd);
}


/*
**  Collect all the version numbers for the current file.
*/
static void
collectversions(dd)
    DIR				*dd;
{
    struct dsc$descriptor_s	pat;
    struct dsc$descriptor_s	res;
    struct dirent		*e;
    char			*p;
    char			buff[sizeof dd->entry.d_name];
    int				i;
    char			*text;
    long			context;

    /* Convenient shorthand. */
    e = &dd->entry;

    /* Add the version wildcard, ignoring the "*.*" put on before */
    i = strlen(dd->pattern);
    text = malloc((unsigned int)(i + strlen(e->d_name)+ 2 + 1));
    if (text == NULL)
	return;
    (void)strcpy(text, dd->pattern);
    (void)sprintf(&text[i - 3], "%s;*", e->d_name);

    /* Set up the pattern descriptor. */
    pat.dsc$a_pointer = text;
    pat.dsc$w_length = strlen(text);
    pat.dsc$b_dtype = DSC$K_DTYPE_T;
    pat.dsc$b_class = DSC$K_CLASS_S;

    /* Set up result descriptor. */
    res.dsc$a_pointer = buff;
    res.dsc$w_length = sizeof buff - 2;
    res.dsc$b_dtype = DSC$K_DTYPE_T;
    res.dsc$b_class = DSC$K_CLASS_S;

    /* Read files, collecting versions. */
    for (context = 0; e->vms_verscount < VERSIZE(e); e->vms_verscount++) {
	if (lib$find_file(&pat, &res, &context) == RMS$_NMF || context == 0)
	    break;
	buff[sizeof buff - 1] = '\0';
	if (p = strchr(buff, ';'))
	    e->vms_versions[e->vms_verscount] = atoi(p + 1);
	else
	    e->vms_versions[e->vms_verscount] = -1;
    }

    free(text);
}


/*
**  Read the next entry from the directory.
*/
struct dirent *
readdir(dd)
    DIR				*dd;
{
    struct dsc$descriptor_s	res;
    char			*p;
    char			buff[sizeof dd->entry.d_name];
    int				i;

    /* Set up result descriptor, and get next file. */
    res.dsc$a_pointer = buff;
    res.dsc$w_length = sizeof buff - 2;
    res.dsc$b_dtype = DSC$K_DTYPE_T;
    res.dsc$b_class = DSC$K_CLASS_S;
    if (lib$find_file(&dd->pat, &res, &dd->context) == RMS$_NMF
     || dd->context == 0L)
	/* None left... */
	return NULL;

    /* Force the buffer to end with a NUL. */
    buff[sizeof buff - 1] = '\0';
    for (p = buff; !isspace(*p); p++)
	;
    *p = '\0';

    /* Skip any directory component and just copy the name. */
    if (p = strchr(buff, ']'))
	(void)strcpy(dd->entry.d_name, p + 1);
    else
	(void)strcpy(dd->entry.d_name, buff);

    /* Clobber the version. */
    if (p = strchr(dd->entry.d_name, ';'))
	*p = '\0';

    dd->entry.d_namlen = strlen(dd->entry.d_name);

    dd->entry.vms_verscount = 0;
    if (dd->vms_wantversions)
	collectversions(dd);
    return &dd->entry;
}


/*
**  Return something that can be used in a seekdir later.
*/
long
telldir(dd)
    DIR		*dd;
{
    return dd->context;
}

/*
** RMDIR emulation function.
** Converts the given path to VMS-style, and uses the delete() function
*/
#ifndef rmdir
long
rmdir (path)
   char *path;
{

  /* change the protection, to enable deletion */
  if (chmod(path,0000700)) return(-1);
#ifdef DEBUG
 printf("rmdir: %s\n",path);
#endif
  return(delete(path));
}
#endif

/*
**  Return to a spot where we used to be.
*/
void
seekdir(dd, pos)
    DIR		*dd;
    long	pos;
{
    dd->context = pos;
}

/*
 * FOPEN emulation function.
 * !! The number of arguments differs from the open-definition in unixio.h,
 * only 2 arguments are allowed here; filename and mode.
 */
int vms_fopen(filespec,a_mode)
const char *filespec,*a_mode;
/*
{ return (fopen(filespec,a_mode,"mbf=2","mbc=32","shr=get","mrs=1024",
                "ctx=rec","rfm=fix")); }
*/
{  return (fopen(filespec,a_mode,"mbf=2","mbc=32","shr=get","mrs=1024")); }

/*
 * OPEN emulation function.
 * Converts pathname to VMS-style first.
 * !! The number of arguments differs from the open-definition in unixio.h,
 * only 3 arguments are allowed here; filename, flags and mode.
 */
int vms_open(filespec, flags, mode)
char *filespec;
int flags;
unsigned int mode;
{ return(open(filespec,flags,mode,"mbf=2","mbc=32")); }

#ifdef	TEST
main()
{
    char		buff[256];
    DIR			*dd;
    struct dirent	*dp;
    int			i;
    int			j;

    for ( ; ; ) {
	printf("\n\nEnter dir:  ");
	(void)fflush(stdout);
	(void)gets(buff);
	if (buff[0] == '\0')
	    break;
	if ((dd = opendir(buff)) == NULL) {
	    perror(buff);
	    continue;
	}

	/* Print the directory contents twice, the second time print
	 * the versions. */
	for (i = 0; i < 2; i++) {
	    while (dp = readdir(dd)) {
		printf("%s%s", i ? "\t" : "    ", dp->d_name);
		for (j = 0; j < dp->vms_verscount; j++)
		    printf("  %d", dp->vms_versions[j]);
		printf("\n");
	    }
	    rewinddir(dd);
	    vmsreaddirversions(dd, 1);
	}
	closedir(dd);
    }
    exit(0);
}
#endif	/* TEST */
