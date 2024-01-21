/*
 * File:	utf7.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 *              22 Mar 96   Framstag	replaced some unsigned char with char
 *               4 Apr 96   Framstag	fixed memory leak in utf2iso
 *
 * UTF-7 and Unicode coding routines for the sendfile package.
 * Look at utf7.h for a list of the functions.
 *
 * Copyright © 1995,1996 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "utf7.h"
#include "string.h"     /* extended string functions */


/*
 * utf2iso - UTF-7 to ISO Latin-1 decoding
 *
 * INPUT:  fnf   - unix file name flag; if >0 substitute '/' with '_'
 *         utf	 - UTF-7 encoded string
 *
 * OUTPUT: iso	 - ISO Latin-1 string
 *         show	 - ISO Latin-1 string without control codes
 *         shell - ISO Latin-1 string without control codes and meta characters
 *
 * RETURN: 2 digit binary code
 *         - if no digit is set:  no special chars found
 *         - if 1st digit is set: Unicode characters or '/' or '\0' found
 *         - if 2nd digit is set: meta chars or control code chars found
 */
int utf2iso(int fnf, char *iso, char *show, char *shell, char *utf)
{ int ucc,			/* Unicode character count */
      flags=0;			/* output flags */
  char *cp, *cp2, *cp3, 	/* char pointers for positioning substrings */
       mbase_part[LEN_UTF];	/* mbase64 string */
  pstr_t *uni_part;		/* Unicode part pstring */

  /* initialisize the strings */
  *iso=0;
  *show=0;
  *shell=0;
  uni_part=pstr_create(LEN_UNI);

  /* loop over the UTF-7 encoded string to find mbase64 parts */
  for (cp=utf, cp2=&utf[1]; *cp!=0; cp++, cp2++)
  {
    /* mbase64 shift char? */
    if (*cp=='+')
    {
      /* end of mbase64 part? */
      if (*cp2=='-' || *cp2==0)

      { strcat(iso,"+");
        strcat(show,"+");
        strcat(shell,"+");

	/* still more string to parse? */
        if (*cp2!=0)
	{ cp++;
          cp2++;
        }

      } else
      {
	/* find end of mbase64 part */
	for (cp3=cp2; *cp3!='-'; cp3++);

	/* cut out mbase64 part string */
	*mbase_part=0;
	strncat(mbase_part,cp2,cp3-cp2);

	/* decode it to Unicode */
	decode_mbase64(uni_part,mbase_part);

	/* loop over Unicode pstring to look for ISO Latin-1 chars */
	for (ucc=1; ucc<=uni_part->length; ucc+=2)
	{
	  /* next character a ISO Latin-1 char? */
	  if (uni_part->string[ucc]==0)
	    add_char(fnf,iso,show,shell,uni_part->string[ucc+1],&flags);
	  else
	  {
	    /* substitute non valid Unicode character with '_' */
	    flags = flags|1;
	    strcat(iso,"_");
	    strcat(show,"_");
	    strcat(shell,"_");
	  }
	}

	/* adjust the pointers */
	cp = cp3;
	cp2 = ++cp3;

      }
    } else

      /* add a ISO Latin-1 char */
      add_char(fnf,iso,show,shell,*cp,&flags);
  }

  /* dont allow "." or ".." as file names */
  if (fnf)
  { if (streq(iso,"."))
    { flags = flags|1;
      strcpy(iso,"_");
      strcpy(show,"_");
      strcpy(shell,"_");
    }
    if (streq(iso,".."))
    { flags = flags|1;
      strcpy(iso,"__");
      strcpy(show,"__");
      strcpy(shell,"__");
    }
  }

  /* free memory for no longer used Unicode pstring */
  pstr_delete(uni_part);

  return flags;
}


/*
 * add_char - add a char depending on its range
 *
 * INPUT:  fnf   - unix file name flag; if >0 substitute '/' with '_'
 *         c	 - char to add
 *         flags - return flags for utf2iso function
 *
 * OUTPUT: iso	 - ISO Latin-1 string
 *         show	 - ISO Latin-1 string without control codes
 *         shell - ISO Latin-1 string without control codes and meta characters
 *         flags - return flags for utf2iso function
 */
void add_char(int fnf, char *iso, char *show, char *shell, char c, int *flags)
{ unsigned char sc[2];			/* string to add */
  const char *meta="\"!#$&'()*?\\`| ";	/* (bourne) shell meta characters */

  /* build the string to add */
  sc[0] = c;
  sc[1] = 0;

  /* is it a non valid char for a UNIX file name? */
  if (*sc==0 || (*sc=='/' && fnf))
  { *flags = *flags|1;
    *sc = '_';
  }

  /* add the char to the iso-string */
  strcat(iso,(char *)sc);

  /* is it a control code? */
  if (*sc<32 || (*sc>126 && *sc<161))
  { *flags = *flags|2;
    *sc = '_';
  }

  /* add the char to the show-string */
  strcat(show,(char *)sc);

  /* is it a meta char? */
  if (strchr(meta,*(char *)sc))
  { *flags = *flags|2;
    *sc = '_';
  }

  /* add the char to the shell-string */
  strcat(shell,(char *)sc);
}


/*
 * iso2utf - ISO Latin-1 to UTF-7 encoding
 *
 * INPUT:  utf	 - UTF-7 encoded string
 *
 * OUTPUT: iso	 - ISO Latin-1 string
 *         show	 - ISO Latin-1 string without control codes
 *         shell - ISO Latin-1 string without control codes and meta characters
 */
void iso2utf(char *utf_name, char *iso_name)
{ char *cp, *cp2,				/* string pointers */
       *DO_set="abcdefghijklmnopqrstuvwxyz"	/* mbase64 D and O sets */
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               "01234567890"
               "'(),-./:?!\"#$%&*;<=>@[]^_`{}|";
  char iso_part[LEN_ISO],			/* ISO Latin-1 part string */
       mbase_part[LEN_UTF];			/* mbase64 part string */
  pstr_t *uni_part;				/* Unicode part pstring */

  /* initialisize the strings */
  utf_name[0]=0;
  uni_part=pstr_create(LEN_UNI);

  /* scan the iso-string */
  for (cp=iso_name; *cp!=0 ; cp++)
  {
    /* char in DO set? */
    if (strchr(DO_set,*cp))
      strncat(utf_name,cp,1);
    else
    {
      /* add UTF-7 shift char */
      strcat(utf_name,"+");

      /* +- short encoding? */
      if (*cp=='+')
        strcat(utf_name,"-");
      else
      {
	/* search for the next char in the DO set */
	cp2=cp;
	cp2++;
	while (strchr(DO_set,*cp2)==NULL && *cp2!=0) cp2++;

	/* cut out the iso-part string */
	*iso_part=0;
	strncat(iso_part,cp,cp2-cp);

	/* translate it to Unicode */
        iso2uni(uni_part,iso_part);

	/* encode to mbase64 */
	encode_mbase64(mbase_part,uni_part);

	/* add it to the utf string */
	strcat(utf_name,mbase_part);
        strcat(utf_name,"-");
	cp=cp2-1;

      }
    }
  }

  /* free memory for no longer used Unicode pstring */
  pstr_delete(uni_part);
}


/*
 * iso2uni - transform ISO Latin-1 to Unicode
 *
 * INPUT:  iso  - ISO Latin-1 string
 *
 * OUTPUT: uni  - Unicode pstring
 */
void iso2uni(pstr_t *uni, char *iso)
{ char *cp;	/* character pointer */

  /* Unicode length is 0 at start */
  uni->length=0;

  /* loop over iso string */
  for (cp=iso; *cp!=0; cp++)
  {
    /* first byte of Unicode character is always 0 */
    pstr_addchar(uni,0);

    /* add the ISO Latin-1 char byte */
    pstr_addchar(uni,*cp);
  }
}


/*
 * The functions decode_mbase64 and encode_mbase64 are based on encdec.c
 * by Jvrgen Hdgg which has been debugged and rewritten to use as C functions.
 * The original header was:
 *
 *	Written by Jvrgen Hdgg 1993 <jh@efd.lth.se>
 *	Version 1.1
 *
 *	(This filter is written for use in a MTA written in perl.)
 *
 *	Please send comments and bugfixes when you find them.
 *	Permission to use and change this program is given for any purpose
 *	as long as this note remains unchanged.
 *
 *	The usage() is the manual.
 *	Use encdec as you wish :-)
 *
 */


void decode_mbase64(pstr_t *outstring, char *instring)
{ int	i, j, num, len;
  long	d, val;
  char	*p, *c;
  static char 
    vec[]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
  unsigned char	nw[4];

  outstring->length = 0;
  len = strlen(instring);
  strcat(instring,"==");
  for (i=0; i<len-1; i+=4)
  { val = 0;
    num = 3;
    c = instring+i;
    if (c[2] == '=') num = 1;
    if (c[3] == '=') num = 2;
    for (j=0; j<=num; j++)
    { if (!(p = strchr(vec,c[j])))
      { fprintf(stderr,"encdec: %s not in base64\n",instring);
	exit(1);
      }
      d = p-vec;
      d <<= (3-j)*6;
      val += d;
    }
    for (j=2; j>=0; j--)
    { nw[j] = val & 255;
      val >>= 8;
    }
    for (j=0; j<num; j++) pstr_addchar(outstring,nw[j]);
  }
  if (outstring->length/2 != outstring->length/2.) outstring->length--;
}


void encode_mbase64(char *outstring, pstr_t *instring)
{ int n = 0, iz, oz = 0, i;
  unsigned char c;
  long val = 0;
  unsigned char enc[4];
  static char 
    vec[]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

  for (iz=1; iz<=instring->length; iz++)
  { c=instring->string[iz];
    if (n++<=2)
    { val <<= 8;
      val += c;
      continue;
    }
    for (i=0; i<4; i++)
    { enc[i] = val&63;
      val >>= 6;
    }
    for (i=3; i>=0; i--) outstring[oz++] = vec[(int)enc[i]];
    n = 1;
    val = c;
  }
  if (n==1)
  { val <<= 16;
    for (i=0; i<4; i++)
    { enc[i] = val&63;
      val >>= 6;
    }
    enc[0] = enc[1] = 64;
  }
  if (n==2)
  { val <<= 8;
    for (i=0; i<4; i++)
    { enc[i] = val&63;
      val >>= 6;
    }
    enc[0] = 64;
  }
  if (n==3)
    for (i=0; i<4; i++)
  { enc[i] = val&63;
      val >>= 6;
    }
  if (n)
    for (i=3; i>=0; i--)
  { c = vec[(int)enc[i]];
      if (c!='=') outstring[oz++] = c;
    }
  outstring[oz] = 0;
}
