/* convpath.c
 *
 * Some awful Unix to VMS-directory conversion routines for use with
 * VMS-fsp.
 *
 * 28-DEC-92 First version for VMS-FSP 2.6.5 <S.A.Pechler@bdk.tue.nl>
 * 22-JAN-93 Added convdots routine <S.A.Pechler@bdk.tue.nl>
 * 07-FEB-93 Optimized with SHELL$-routine <S.A.Pechler@bdk.tue.nl>
 */

#include "common_def.h"

char *vms_name; /* global namespec */

int do_shell_filespec(fs)
char *fs;
{
  vms_name=fs;
  return(1);
}

char *unixtovms(unixpath)
char *unixpath;
{
  if (SHELL$TO_VMS(unixpath, do_shell_filespec, 1)) return(vms_name);
  else return(NULL);
}

#ifdef TEST
/*
 * convert unixpath to absolute VMS-path
 * args: dest: vms-pathname
 *       path: unix-style pathname
 * ret:     0: conversion ok
 *         -1: error.
 */
int convpath(dest,path)
char *dest,*path;
{
 char *trans,*here;

 strcpy(dest,home_dir);     /* add home_dir always (absolute path) */
 if (*path)                 /* something to convert? */
 {
   here=dest+strlen(dest);  /* find last char of string */
   strcpy(here,path);       /* add path to it */
   if (trans=unixtovms(here))  /* if conversion ok, */
       strcpy(here-1,trans+1); /* put it back, strip trailing ']'
                                  from trans and leading '[' from here */
   else return(-1);            /* otherwise return error */
 }
 return(0);
}


/*
 * convert unixdir to VMS-dirname
 * args: dest: name of vms-directoryfile
 *       path: unix-style directoryname
 * ret:     0: conversion ok
 *         -1: error.
 */
int convdir(dest,path)
char *dest,*path;

{
 char *trans,*here;

 if (*path)                 /* something to convert? */
 {
   strcpy(dest,path);       /* add path to it */
   strcat(dest,".dir.1");   /* concatenate .dir.1 extension, so that it will
                               be converted to a directory-filename */
   if (trans=unixtovms(dest)) strcpy(dest,trans);   /* conv.ok? put it back */
   else return(-1);           /* otherwise return error */
 }
 else                         /* empty path, so only home-directory */
 {
   if (getcwd(dest,512,0)) strcat(dest,".dir.1");
   else return(-1);
   if (trans=unixtovms(dest)) strcpy(dest,trans);
   else return(-1);
 }
 return(0);
}
#endif

/*
 * convert unixfilepath to VMS-filepath
 * args: dest: name of vms-directoryfile
 *       path: unix-style filepath
 * ret:     0: conversion ok
 *         -1: error.
 */
int convfile(dest,path)
char *dest,*path;
{
 char *trans;

 if (!*path) return(-1);                 /* something to convert? */
 if (!(trans=unixtovms(path))) return (-1); /* error during conversion */
 strcpy(dest,trans);                     /* put it back */
 return(0);
}

/* convdots
 * If more then 1 dot in filename, convert them to an underscore.
 * If any invalid character in filename (exclamation marks, question marks,
 * colons, etc.) , then convert it also to an underscore.
 */
int convdots(target,source)
char *target,*source;

{ char *dot,*here;

  strcpy(target,source);              /* make a copy first */
  if (dot=strchr(target,'.'))       /* is there a dot present?*/
     while (here=strchr(dot+1,'.')) /* look for more dots */
     {
       *here='_';                   /* replace with '_' */
       dot=here;                    /* find next dot */
     }
  /* Convert invalid characters to an underscore */
  here=target;
  while (here=strpbrk(here,"?!:")) *here++='_';

  return(0); /* can't go wrong */
}
