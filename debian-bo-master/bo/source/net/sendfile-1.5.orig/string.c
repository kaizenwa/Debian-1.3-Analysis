/*
 * File:	string.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 *     		 5 Feb 96   Framstag	faster str_trim function
 *     		 7 May 96   Framstag	added strbeq()
 *
 * Extended string functions for the sendfile package, which are not found
 * in the standard C library.
 * Look at string.h for a list of the functions.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "string.h"

/*
#ifdef IRIX
  int toupper(char);
  int tolower(char);
#endif
*/

/*
 * str_trim - substitute multiple white spaces with one space
 *            and delete heading and trailing whitespace
 *
 * INPUT:  string  - string to trim
 *
 * OUTPUT: string  - trimmed string
 *
 * RETURN: trimmed string
 */
char *str_trim(char *string)
{ char *rp,	/* reading string pointer */
       *wp;	/* writing string pointer */
  int ws=1;	/* white space flag */

  /* loop over string */
  for (wp=rp=string; *rp; rp++)
  {
    /* is it a white space? */
    if (*rp==' ' || *rp=='\t' || *rp=='\n' || *rp=='\r')
    {
      /* was the last character not a white space? */
      if (ws==0)
      {
	/* store a blank */
	*wp++ = ' ';
	ws = 1;
      }
    } else /* no white space */
    {
      /* store the character */
      *wp++ = *rp;
      ws = 0;
    }
  }

  /* delete trailing blank */
  if (ws) wp--;
  *wp = 0;

  return(string);
}


/*
 * str_toupper - transform string to upper case
 *
 * INPUT:  string  - string to transform
 *
 * OUTPUT: string  - transformed string
 *
 * RETURN: string  - transformed string
 */
char *str_toupper(char *string)
{ char *cp;

  /* change each character to it's upper case pedant */
  for (cp=string; *cp; cp++) *cp = (char) toupper(*cp);

  return(string);
}


/*
 * str_tolower - transform string to lower case
 *
 * INPUT:  string  - string to transform
 *
 * OUTPUT: string  - transformed string
 *
 * RETURN: string  - transformed string
 */
char *str_tolower(char *string)
{ char *cp;

  /* change each character to it's upper case pedant */
  for (cp=string; *cp; cp++) *cp = (char) tolower(*cp);

  return(string);
}


/* insert one string in another */
void strins(char *to, char *from)
{ while (*from) *to++ = *from++; }


/*
 * strbeq  - string begin equal test
 *
 * INPUT:  s1  - string 1
 *         s1  - string 1
 *
 * RETURN: 1 if begin is equal, 0 if begin is not equal
 */
int strbeq(const char *s1, const char *s2)
{ int len;
  if (strlen(s1)<strlen(s2)) len=strlen(s1); else len=strlen(s2);
  if (strncmp(s1,s2,len)==0) return(1); else return(0);
}


/***************************************************************
 *
 *  simplematch:  Einfaches Wildcard-Matching.
 *
 *  ?           matcht jedes beliebige Zeichen
 *  *           matcht eine beliebig lange Folge von beliebigen Zeichen
 *  [abc]  	matcht 'a', 'b' und 'c'
 *              innerhalb [] gelten '?', '*' und '[' als normale Zeichen
 *  \?          testet auf "echtes" '?'   Alternative:  [?]
 *  \*          testet auf "echtes" '*'   Alternative:  [*]
 *  \[          testet auf "echtes" '['   Alternative:  [[]
 *              ']' gilt ausserhalb [] als normales Zeichen, wenn es
 *              innerhalb [] verwendet werden soll muss ein \ davor
 *  \n          NewLine
 *  \r          Return
 *  \t          Tabulator
 *  \\          Schraegstrich
 *
 *  ACHTUNG:
 *  Matching auf "[]" (leere Klammer) schlaegt immer fehl
 *
 *  26/Jul/1991 Ingo.Wilken@informatik.uni-oldenburg.de
 */

/*
#include <stddef.h>
#define NULL        (char *)0
*/

#define SM_MATCH	1
#define SM_NOMATCH      0
#define SM_ERROR        -1        /* Fehler im Pattern */


static char sm_escchar(char c)

{ switch( c ) {
        case 'n': c = '\n'; break;
        case 'r': c = '\r'; break;
        case 't': c = '\t'; break;
    }
    return(c);
}


int simplematch(char *text, char *pattern, int nocase)
{
    char *retrypat = NULL, *retrytxt = NULL;
    register char c;
    int notfound;

    while( *text || *pattern ) {
        c = *(pattern++);
        if( nocase )
            c = tolower(c);

        switch( c ) {
            case '*' :
                retrypat = pattern;
                retrytxt = text;
                break;
            case '[' :
                notfound = 1;
                while( (c = *(pattern++)) != ']' ) {
                    if( c == '\\' )
                        c = sm_escchar(*(pattern++));
                    if( c == '\0' )
                        return(SM_ERROR);
                    if( *text == c || (nocase && tolower(*text) == c) )
                        notfound = 0;
                }
                if( notfound ) {
                    pattern = retrypat;
                    text = ++retrytxt;
                }
            case '?' :
                if( *(text++) == '\0' )
                    return(SM_NOMATCH);
                break;
            case '\\':
                c = sm_escchar(*(pattern++));
                if( c == '\0' )
                    return(SM_ERROR);
            default  :
                if( *text == c || (nocase && tolower(*text) == c) ) {
                    if( *text )
                        text++;
                }
                else {
                    if( *text ) {
                        pattern = retrypat;
                        text = ++retrytxt;
                    }
                    else
                        return(SM_NOMATCH);
                }
                break;
        }
        if( pattern == NULL )
            return(SM_NOMATCH);
    }
    return(SM_MATCH);
}
