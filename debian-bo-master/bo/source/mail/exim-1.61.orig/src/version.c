/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Function for setting up the version string. */

#include "exim.h"


#define THIS_VERSION  "1.61"


/* The header file cnumber.h contains a single line containing the
compilation number, making it easy to have it updated automatically.
Hence the fudgery below to get the number turned into a string, since
we can't use #include inside a macro argument list */

void
version_init(void)
{
int i = 0;
char today[20];

int cnumber =
#include "cnumber.h"
;

version_cnumber = store_malloc(8);
sprintf(version_cnumber, "%d", cnumber);
version_string = THIS_VERSION;

strcpy(today, __DATE__);
if (today[4] == ' ') i = 1;
today[3] = today[6] = '-';

version_date = (char *)store_malloc(32);
version_date[0] = 0;
strncat(version_date, today+4+i, 3-i);
strncat(version_date, today, 4);
strncat(version_date, today+7, 4);
strcat(version_date, " ");
strcat(version_date, __TIME__);
}

/* End of version.c */
