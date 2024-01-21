/*
   Time-stamp: <96/07/19 20:16:38 yusuf>

   $Id: non-ansi.h,v 1.4 1996/07/27 20:42:11 yusuf Exp $
*/


/* This file contains headers for the non-ansi functions.
 * 
 * They are non-ansi but are implemented on Linux (the development
 * system of taper). If you are porting, and do not have these
 * functions, you will have to write them yourself or find
 * alternatives.

 * When time permits, I will write these functions myself and
 * include them with taper
 */

int strcasecmp(const char *s1, const char *s2);
int strncasecmp(const char *s1, const char *s2, size_t len);
int fsync(int fd);
int readlink(const char *path, char *buf, size_t bufsize);
int symlink(const char *oldpath, const char *newpath);
char *tempnam(const char *dir, const char *prefix);
