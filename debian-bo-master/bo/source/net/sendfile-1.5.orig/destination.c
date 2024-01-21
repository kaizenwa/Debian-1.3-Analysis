/*
 * File:	destination.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 *               7 Nov 95   Framstag	added URL addressing
 *              15 Nov 95   Framstag	added sendfile alias file
 *              13 Dec 95   Framstag	correct bug when reading alias file
 *              30 Apr 96   Framstag	checking elm alias only if configured
 *
 * Determine own username, recipient username and host and look
 * for an alias in /USERSPOOL/config/aliases
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <stdio.h>
#include <pwd.h>
#include <string.h>
#include <unistd.h>

#include "config.h"		/* various #defines */
#include "string.h"		/* Extended string functions */
#include "utf7.h"		/* UTF-7 coding */
#include "message.h"		/* information, warning and error messages */
#include "destination.h"	/* check recipient and host */


#ifndef OSF1
  int strncasecmp(const char *, const char *, size_t);
#endif

/* check an alias file */
int check_alias(char *, char *, char *);


/*
 * destination  - get recipient user and host
 *
 * INPUT:  argc		- shell argument count
 *         argv		- the shell arguments
 *
 * OUTPUT: user		- own user login and real name
 *         recipient	- recipient user name
 *         host		- recipient host name
 */
void destination(int argc, char **argv,
		 char *user, char *recipient, char *host)
{ char *cp,			/* simple char pointer */
       *at, 			/* @ character in recepient@host */
       *larg,			/* last argument */
       gecos[FLEN],		/* user real name */
       aliasfile[MAXLEN];	/* the alias file */
  extern char *prg;		/* name of the game */
  struct passwd *pwe;		/* password entry */

  /* get the own user name */
  if ((pwe=getpwuid(getuid())) == NULL)
    message(prg,'F',"could not determine own user name");

  /* translate the real name to UTF-7 and add it */
  iso2utf(gecos,pwe->pw_gecos);
  if ((cp=strchr(gecos,','))) *cp=0;
  sprintf(user,"%s %s",pwe->pw_name,gecos);

  /* trick: argc <= 0, when called from quak */
  if (argc<=0)
    larg=argv[-argc];
  else
    larg=argv[argc-1];

  /* user@host specified? */
  if ((at=strchr(larg,'@')))
  {
   /* store recipient name and host */
    *recipient=0;
    strncat(recipient,larg,at-larg);
    strcpy(host,at+1);

  /* URL specified? */
  } else if (strncasecmp(larg,"saft://",7)==0)
  { larg+=7;
    cp=strrchr(larg,'/');
    strcpy(recipient,cp+1);
    *cp=0;
    while ((cp=strchr(larg,'/'))) *cp='.';
    strcpy(host,larg);

  /* local user or alias specified */
  } else
  {
    strcpy(recipient,larg);

    /* check the sendfile alias file */
    sprintf(aliasfile,SPOOL"/%s/config/aliases",pwe->pw_name);
    if (check_alias(aliasfile,recipient,host)<0)
    {
#ifdef RESPECT_MAIL_ALIASES
      /* check the sendfile alias file */
      sprintf(aliasfile,"%s/.elm/aliases.text",pwe->pw_dir);
      if (check_alias(aliasfile,recipient,host)<0)
      {
#endif
	/* store local recipient name and local host */
	/* trick: argc <= 0, when called from quak */
	if (argc<=0)
	  strcpy(recipient,argv[-argc]);
	else
	  strcpy(recipient,argv[argc-1]);
	strcpy(host,"127.0.0.1");

#ifdef RESPECT_MAIL_ALIASES
      }
#endif
    }
  }
}


/*
 * check_alias  - check an alias file
 *
 * INPUT:  aliasfile	- the alias file
 *         recipient	- recipient alias name
 *
 * OUTPUT: recipient	- recipient user name
 *         host		- recipient host name
 */
int check_alias(char *aliasfile, char *recipient, char *host)
{ char *cp, 			/* a character pointer */
       line[MAXLEN],		/* one line of the alias file */
       address[MAXLEN];		/* address from the alias */
  FILE *inf;			/* input file to read */

  *address=0;

  /* if there is an alias file, open it (what else? :-) ) */
  inf=fopen(aliasfile,"r");
  if (inf==NULL) return(-1);

  /* loop over all lines */
  while (fgets(line,MAXLEN-1,inf))
  {
    /* trim line */
    if ((cp=strchr(line,'\n'))) *cp=0;
    if ((cp=strchr(line,'#'))) *cp=0;
    str_trim(line);

    /* save the address and check for empty or wrong lines */
    cp=strrchr(line,' ');
    if (cp==NULL) continue;
    strcpy(address,cp+1);
    cp=strchr(line,' ');
    if (cp) *cp=0;

    /* is it the correct alias, we are ready */
    if (streq(recipient,line))
      break;
    else
      *address=0;

  }
  fclose(inf);

  /* alias found? */
  if (*address)
  {
    /* store recipient name and host */
    cp=strchr(address,'@');
    if (cp)
    { strcpy(host,cp+1);
      *cp=0;
      strcpy(recipient,address);
      return(0);
    }

  }
  return(-1);
}
