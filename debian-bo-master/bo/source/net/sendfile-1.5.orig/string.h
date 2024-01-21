/*
 * File:	string.h
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 *		29 Feb 96   Framstag	added streq and strneq macros
 *		 5 May 96   Framstag	merged streq and strneq
 *
 * Header-file of the extended string functions for the sendfile package,
 * which are not found in the standard C library.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


/* trim white spaces */
char *str_trim(char *);

/* transform string to upper case */
char *str_toupper(char *);

/* transform string to lower case */
char *str_tolower(char *);

/* insert one string in another */
void strins(char *, char *);

/* match a simple pattern */
int simplematch(char *, char *, int);

/* string begin equal test */
int strbeq(const char *, const char *);

/* string equal test */
#define streq(s1,s2) (strcmp((s1),(s2)) == 0)
/* #define streq(s1,s2) (strncmp((s1),(s2),(strlen(s1)<strlen(s2))?strlen(s1):strlen(s2))==0) */
