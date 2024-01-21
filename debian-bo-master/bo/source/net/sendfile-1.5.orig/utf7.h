/*
 * File:	utf7.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 *
 * Header-file and size definitions of the UTF-7 and Unicode coding routines
 * for the sendfile package.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include "pstring.h"

/*
#define LEN_ISO 256
#define LEN_UNI 512
#define LEN_UTF 683
*/

#ifndef MAXLEN
  #define MAXLEN 32768
#endif
#define LEN_ISO MAXLEN
#define LEN_UNI 2*MAXLEN
#define LEN_UTF 3*MAXLEN

/* UTF-7 to ISO Latin-1 decoding */
int utf2iso(int, char *, char *, char *, char *);

/* ISO Latin-1 to UTF-7 encoding */
void iso2utf(char *, char *);

/* transform ISO Latin-1 to Unicode */
void iso2uni(pstr_t *, char *);

/* add a char depending on its range */
void add_char(int, char *, char *, char *, char, int *);

/* decode mbase64 string to Unicode */
void decode_mbase64(pstr_t *, char *);

/* encode Unicode pstring to mbase64 */
void encode_mbase64(char *, pstr_t *);
