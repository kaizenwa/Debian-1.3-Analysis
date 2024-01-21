/* winiodef.h */
/* $Id: winiodef.h,v 1.4 1996/03/12 20:07:27 mdavidso Exp $ */
/* Last edited by Mike Spreitzer November 3, 1995 2:40 pm PST */

#include <stdio.h>

#undef putchar
#define putchar fputchar
#undef getchar
#define getchar fgetchar


#undef vfprintf
#define vfprintf vfprintf_winio

int vfprintf_winio(FILE* stream, const char *fmt,
					   va_list marker);
/*
 * Like ordinary vfprintf, but if /stream/ == /stdout/ or /stderr/,
 * output goes to the special window managed by this package.
 */
