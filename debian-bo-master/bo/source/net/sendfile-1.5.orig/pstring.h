/*
 * File:	pstring.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 *
 * Header-file for the Pascal like pstring functions.
 * Strings start by definition at pstr.length[1]
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


typedef struct {
  int size;	/* absolute length */
  int length;	/* used string length */
  char *string;	/* the string itself without any terminating symbol */
} pstr_t;

/* create a pstring*/
pstr_t *pstr_create(int);

/* delete a pstring */
void pstr_delete(pstr_t *);

/* add a char to a pstring */
int pstr_addchar(pstr_t *, char);

/* assign one pstring to another (p1<-p2) */
int pstr_assign(pstr_t *, pstr_t *);

/* add a string to a pstring */
int pstr_addstring(pstr_t *, const char *);

/* print a pstring */
void pstr_print(pstr_t *);
