/*
 * File:	utf7encode.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	24 Mar 95   Framstag	initial version
 *
 * Filter to encode or decode UTF-7.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifndef AIX
  #if defined(IRIX) || defined(LINUX)
    #include <getopt.h>
  #else
    int getopt(int, char * const *, const char *);
  #endif
#endif

#if defined(IRIX) || defined(LINUX)
  FILE *fdopen(int, char *);
#endif


#include "message.h"	/* information, warning and error messages */
#include "utf7.h"	/* UTF-7 coding */
#include "string.h"     /* extended string functions */


int usage();

char *prg; 	/* program name */


int main(int argc, char *argv[])
{ extern int
    optind;
  int
    opt;
  char
    *cp,
    line[MAXLEN],
    tmp[MAXLEN],
    utf_line[LEN_UTF],
    iso_line[LEN_ISO];
  int
    decode;
  FILE *inf;

  decode=0;
  prg=argv[0];

  if (streq(prg,"utf7decode")) decode=1;

  while ((opt=getopt(argc,argv,"h?d")) > 0)
  { switch (opt)
    { case ':':
      case 'h':
      case '?': exit(usage());
      case 'd': decode=1;
    }
  }

  if (argc-optind==0)
    inf=fdopen(0,"r");
  else
  { inf=fopen(argv[optind],"r");
    if (inf==NULL)
    { sprintf(tmp,"cannot open %s",argv[optind]);
      message(prg,'F',tmp);
    }
  }

  while (fgets(line,MAXLEN-1,inf))
  { if ((cp=strchr(line,'\n'))) *cp=0;
    if (decode)
    { utf2iso(0,iso_line,tmp,tmp,line);
      printf("%s\n",iso_line);
    }
    else
    { iso2utf(utf_line,line);
      printf("%s\n",utf_line);
    }
  }

  exit(0);
}


/* void function for message() */
void cleanup() { }


/*
 * usage - print short help usage text
 */
int usage()
{ fprintf(stderr,"usage:  %s [-d] [text-file]\n",prg);
  fprintf(stderr,"option: -d  decode\n");
  return(2);
}
