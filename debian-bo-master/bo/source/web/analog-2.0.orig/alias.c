/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** alias.c; functions to cope with aliasing, etc. ***/

#include "analhea2.h"

flag doaliaslist(char *name, struct alias *listhead)
{                     /* do requested aliases on an object */
  struct alias *aliasp;
  int anydone = OFF;
  char *p1, *p2, *c;
  char answer[MAXSTRINGLENGTH];

  for (aliasp = listhead; aliasp -> from[0] != '\0' && !anydone;
       aliasp = aliasp -> next) {
    if (wildmatch(name, aliasp -> from, &p1, &p2)) {
      anydone = ON;
      if ((c = strchr(aliasp -> to, '*')) == NULL)
	strcpy(name, aliasp -> to);
      else {
	*c = '\0';
	if ((int)strlen(aliasp -> to) + (p2 - p1) + (int)strlen(c + 1) <
	    MAXSTRINGLENGTH) {
	  strcpy(answer, aliasp -> to);
	  strncat(answer, p1, (size_t)(p2 - p1));
	  strcat(answer, c + 1);
	  *c = '*';
	  strcpy(name, answer);
	}
      }
    }
  }

  return(anydone);

}

#ifndef NODNS
flag dnsresolve(char *hostn)
{  /* assume dnsq and isdigit(hostn[0]) already tested */
  flag done = FALSE;
  extern int debug;
#ifdef MAC
  printf("Lookup for '%s'",hostn);
  done=IpAddr2Name(hostn);
  if (done)
    printf(" gives '%s'.\n",hostn);
  else
    printf(" failed.\n");
#else
  unsigned long addr;
  const char *addrp;
  struct hostent *tempp;

  addr = inet_addr(hostn);
  if (addr != INET_ADDR_ERR) {
    addrp = (const char *) &addr;
    tempp = gethostbyaddr(addrp, sizeof(struct in_addr), AF_INET);
    if (tempp != NULL) {
      if (debug >= 2)
	fprintf(stderr, "D: %s -> %s\n", hostn, tempp -> h_name);
      strncpy(hostn, tempp -> h_name, MAXSTRINGLENGTH - 1);
      hostn[MAXSTRINGLENGTH - 1] = '\0';
      done = TRUE;
    }
    else if (debug >= 2)
      fprintf(stderr, "D: %s -> [unresolvable]\n", hostn);
  }
#endif
  return(done);
}
#endif

int doaliashost(char *hostn)
{   /* apply all required aliases to a hostname */
  extern struct alias *hostaliashead;
#ifndef NODNS
  extern flag dnsq;
#endif

  int anydone = OFF;
  int i, j;

  /* DNS lookup */

#ifndef NODNS
  if (dnsq && isdigit(hostn[0]) && isdigit(hostn[strlen(hostn) - 1]))
    /* isdigit(hostn[0]) is quick test to save time before proper test */
    anydone = (dnshashadd(hostn, (char *)NULL, (time_t)(-1)) -> alias != NULL);
#endif

  /* remove trailing dots */

  j = (int)strlen(hostn);

  if (hostn[MAX(j - 1, 0)] == '.') {
    hostn[j - 1] = '\0';
    anydone = ON;
  }

  /* convert to lower case */

  for (i = j - 1; i >= 0; i--) {
    if (isupper(hostn[i])) {
      hostn[i] = tolower(hostn[i]);
      anydone = ON;
    }
  }

  /* alias conversion from configuration file */

  if (doaliaslist(hostn, hostaliashead))
    anydone = ON;

  return(anydone);

}

char *reversehostname(char *hostn)
{  /* reverse an internet hostname (cam.ac.uk -> uk.ac.cam). Return a pointer
      to the reversed name, and leave the hostname changed on exit */

  char tempstr[MAXSTRINGLENGTH];
  char *tempp;
  int i = 0;

  while ((tempp = strrchr(hostn, '.')) != NULL) {
    strcpy(tempstr + i, tempp + 1);
    i = (int)strlen(tempstr);
    tempstr[i] = '.';
    i++;
    *tempp = '\0';
  }

  strcpy(tempstr + i, hostn);

  return(strcpy(hostn, tempstr));

}


int doaliasfile(char *filename)
{  /* apply all required aliases to a filename */

  extern int dirsufflength;
  extern struct alias *filealiashead;
  extern char *dirsuffix;
  extern flag case_insensitive;

  int anydone = OFF;
  char *tempp;
  char tempstr[MAXSTRINGLENGTH];
  unsigned int tempint = 0;

  /* First, change %7E to ~, etc. */
  tempp = filename;
  while ((tempp = strchr(tempp, '%')) != NULL) {
    if (*(tempp + 1) != '\0' && *(tempp + 2) != '\0')
      sscanf(tempp + 1, "%2x", &tempint);
    if (tempint >= 0x20 && tempint < 0x7F) {
      anydone = ON;
      *tempp = tempint;
      strcpy(tempstr, tempp + 3);
      strcpy(tempp + 1, tempstr);
      /* strcpy(tempp + 1, tempp + 3) may not be safe on all machines
	 (overlapping arguments) */
    }
    tempp++;
  }

  /* Secondly, convert to lower case if desired */
  /* If contains ?, already done at sscanf time */

  if (case_insensitive && strrchr(filename, '?') == NULL)
    anydone = strtolowerf(filename);

  /* Thirdly, if it ends with the DIRSUFFIX (often index.html), strip it */

  if (dirsufflength > 0) {
    if ((STREQ(filename + MAX((int)strlen(filename) - dirsufflength, 0),
	       dirsuffix)) &&
      *(filename + MAX((int)strlen(filename) - dirsufflength - 1, 0)) == '/') {
      filename[(int)strlen(filename) - dirsufflength] = '\0';
      anydone = ON;
    }
  }

  /* Fourthly, // -> /  ;  /./ -> /  ;  /spam/../ -> /  (except  at start) */
  /* We could worry about trailing /. and /.. but they should be 302 redirects
     anyway so shouldn't matter */
  /* NB On our server //Dept/ -> /Dept/, but //~sret1/ doesn't -> /~sret1/
     But that doesn't matter because the latter will be 404 not found, so
     won't ever get this far. Successes will have been interpreted in the
     following way, and that's all we care about. */
  /* NB Used to use 3 strstr()s. But that is very slow if we've got INCLUDEs
     or EXCLUDEs (so have to alias every time). */

  tempp = filename;
  while ((tempp = strchr(tempp, '/')) != NULL) {
    if (*(tempp + 1) == '/') {
      if (tempp != filename && *(tempp - 1) == ':')
	tempp++;   /* Don't translate http:// ; just skip to next /  */
      else {
	anydone = ON;
	strcpy(tempstr, tempp + 2);
	strcpy(tempp + 1, tempstr);
      }
    }
    else if (*(tempp + 1) == '.') {
      if (*(tempp + 2) == '/') {
	anydone = ON;
	strcpy(tempstr, tempp + 3);
	strcpy(tempp + 1, tempstr);
      }
      else if (*(tempp + 2) == '.' && *(tempp + 3) == '/') {
	anydone = ON;
	strcpy(tempstr, tempp + 4);
	/* go back to prev slash (but not past // or start of name) */
	if (tempp != filename && *(tempp - 1) != '/')
	  while (*(--tempp) != '/' && tempp != filename)
	    ;
	strcpy(tempp + 1, tempstr);
	if (tempp == filename)
	  *tempp = '/';
      }
      else
	tempp++;
    }
    else
      tempp++;
  }

  /* Lastly, alias conversion from config. file */

  if(doaliaslist(filename, filealiashead))
    anydone = ON;

  return(anydone);

}

int doaliasref(char *name)
{        /* apply all required aliases to a referrer */
         /* return 1 if any done, -1 if url corrupt or wrong type, 0 o/wise */

  extern struct alias *refaliashead;

  int anydone = OFF;
  int defaultport = -1;
  int tempint;
  char *tempp, *tempp2;
  char tempstr[MAXSTRINGLENGTH];
  flag tempf;

  /* First, strip off #'s */
  if ((tempp = strchr(name, '#')) != NULL) {
    *tempp = '\0';
    anydone = ON;
  }

  /* Next, change %7E to ~, etc. */
  tempp = name;
  while ((tempp = strchr(tempp, '%')) != NULL) {
    sscanf(tempp + 1, "%2x", &tempint);
    if (tempint >= 0x20 && tempint < 0x7F) {
      anydone = ON;
      *tempp = tempint;
      strcpy(tempstr, tempp + 3);
      strcpy(tempp + 1, tempstr);
      /* strcpy(tempp + 1, tempp + 3) may not be safe on all machines
	 (overlapping arguments) */
    }
    tempp++;
  }

  /* Coerce method to lower case */

  for (tempp = name; *tempp != ':' && *tempp != '\0'; tempp++)
    if (*tempp != tolower(*tempp)) {
      *tempp = tolower(*tempp);
      anydone = ON;
    }
  if (*tempp != ':')
    return(-1);

  /* find out what sort of URL it is */

  *tempp = '\0';
  if (STREQ(name, "http"))
    defaultport = 80;
  else if (STREQ(name, "ftp"))
    defaultport = 21;
  else if (STREQ(name, "file"))
    return(-1);
  else if (STREQ(name, "news"))
    defaultport = 0;
  else if (STREQ(name, "gopher"))
    defaultport = 70;
  else if (STREQ(name, "telnet"))
    defaultport = 23;
  else if (STREQ(name, "wais"))
    defaultport = 210;
  else if (STREQ(name, "nntp"))
    defaultport = 119;
  else if (STREQ(name, "prospero"))
    defaultport = 1525;
  else if (STREQ(name, "mailto"))
    defaultport = 0;
  *tempp = ':';

  /* Unless "news:" or unknown, check it has the // next and coerce hostname
     to lower case */
  
  if (defaultport > 0) {
    if (*(tempp + 1) != '/' || *(tempp + 2) != '/')
      return(-1);
    else for (tempp += 3; *tempp != '/' && *tempp != ':' && *tempp != '\0';
	      tempp++) {
      if (*tempp != tolower(*tempp)) {
	*tempp = tolower(*tempp);
	anydone = ON;
      }
    }

    /* strip trailing .'s from hostname */

    for (tempp2 = tempp - 1; *tempp2 == '.'; tempp2--)
      ;
    if (tempp2 != tempp - 1) {
      strcpy(tempstr, tempp);
      tempp = tempp2 + 1;
      strcpy(tempp, tempstr);
      anydone = ON;
    }

    /* strip leading 0s from port numbers and cross out default port numbers */

    if (*tempp == ':') {
      while (*(tempp + 1) == '0') {
	strcpy(tempstr, tempp + 2);
	strcpy(tempp + 1, tempstr);
      }
      if ((tempp2 = strchr(tempp, '/')) != NULL) {
	*tempp2 = '\0';
	tempf = TRUE;
      }
      else
	tempf = FALSE;
      if (defaultport == atoi(tempp + 1)) {
	strcpy(tempstr, "/");
	if (tempf)
	  strcat(tempstr, tempp2 + 1);
	strcpy(tempp, tempstr);
	anydone = ON;
      }
      else if (tempf)
	*tempp2 = '/';        /* restore slash */
      else
	strcpy(tempp + strlen(tempp), "/"); /* port but no pathname; + slash */
    }

    /* trailing slash on methods without portname or pathname */

    else if (*tempp == '\0')
      strcpy(tempp, "/");
  }

  /* We probably don't want to change /./ -> / etc. even in http protocol,
     because we don't want to make assumptions about other people's file
     systems. */

  /* Finally, alias conversion from config. file */

  if(doaliaslist(name, refaliashead))
    anydone = ON;

  return(anydone);

}

int doalias(char *name, char codeletter)
{
  if (codeletter == 'S')
    return(doaliashost(name));
  else if (codeletter == 'r')
    return(doaliasfile(name));
  else /* codeletter == 'f' */
    return(doaliasref(name));
}
    

/* Now a function to run through all hosts, files or refs doing all aliases */

void allaliases(struct genstruct **objhead, struct genstruct **objhead2,
		int hashsize, int *totalobjs, int *totalobjs7, int *totalnew7,
		char code)
{
  int onlist;
  struct genstruct *p, *nextp;

  onlist = 0;                      /* the list of items we are on */
  p = objhead[0];                  /* starting at list 0 */
  for ( ; onlist < hashsize; p = nextp)   {  /* run through items */
    if (p -> name == NULL)           /* then finished this list */
      nextp = objhead[++onlist];     /* so start the next list */
    else {
      if (p -> wanted)    /* file the alias if any, else original name */
	hashadd(objhead2, hashsize,
		(p -> alias == NULL)?(p -> name):(p -> alias), p -> reqs,
		p -> bytes, p -> pages, p -> last7 * (1 + p -> pre7),
		totalobjs, totalobjs7, totalnew7, ON, OFF, p -> ispage, p,
		(p -> alias == NULL)?onlist:(-1), code);
      nextp = p -> next;
    }

  }   /* end for all items */

}

int hosttodomcode(char *hostn)
{    /* find the domain code for a certain hostname */

  extern struct domain **ohead;

  char domainname[MAXSTRINGLENGTH];
  register int i;

  int x;

  /* first change hostn into a domain */

  for (i = (int)strlen(hostn) - 1; hostn[i] != '.' && i > 0; i--)
    ;     /* run back to final '.' (or initial char) */

  if (isdigit(hostn[i + 1]))
    return(DOMHASHSIZE - 1);   /* representing numerical hosts */

  if (i == 0)
    return(DOMHASHSIZE - 2);   /* representing unknown hosts */

  strcpy(domainname, hostn + i + 1);
	    
  x = (domainname[0] - 'a') * 52 + (domainname[1] - 'a') * 2 +
    (domainname[2] != '\0');

  if (x < 0 || x > DOMHASHSIZE - 3)  /* some funny characters messed it up */
    return(DOMHASHSIZE - 2);

  if (ohead[x] -> name == NULL)
    return(DOMHASHSIZE - 2);      /* no domain at that domaincode */

  if (!STREQ(ohead[x] -> id, domainname))
    return(DOMHASHSIZE - 2);      /* right code, nevertheless wrong domain */

  return(x);

}

void urltodir(char *filename)
{  /* converts a filename to a dir for a level 'dirlevel' directory report */

  extern int dirlevel;

  register int i = 0, j;

  for (j = 0; j < dirlevel; j++) {
    if (filename[i] == '/')
      i++;
    for ( ; filename[i] != '/' && filename[i] != '\0'; i++)
      ;     /* run through to level'th slash, if any */
  }
  
  if (filename[i] == '\0')   /* not j levels; run back */
    for ( i-- ; filename[i] != '/' && i > 0; i--)
      ;

  if (filename[i] != '/')
    strcpy(filename, "[no directory]");
  else {
    filename[i + 1] = '\0';     /* Terminate it straight after the / */
    if (i == 0)
      strcpy(filename, "[root directory]");
  }

}

char *urltoext(char *filename)
{  /* finds the extension (file type) of a given filename */

  char *answer;
  int len;
  char *p;

  len = strlen(filename);
  if (len == 0) {
    answer = (char *)xmalloc(14);
    strcpy(answer, "(no extension)");
  }
  else if (filename[len - 1] == '/') {
    answer = (char *)xmalloc(13);
    strcpy(answer, "(directories)");
  }
  else {
    for (p = filename + len - 1; *p != '.' && *p != '/' && p != filename; p--)
      ;   /* run back to the final dot */
    if (*p == '.' && p != filename + len - 1)
      answer = p;
    else {
      answer = (char *)xmalloc(14);      
      strcpy(answer, "(no extension)");
    }
  }

  return(answer);
}

/* whether a certain string is wanted given a list of includes and excludes */
/* NB Need to make sure aliases have been done before calling this */
/* ispage says whether it's a page if we know, else UNSET */

flag included(char *name, flag ispage, struct include *listhead)
{
  extern struct include *ispagehead;

  struct include *stringp;
  flag answer;
  char *tempp1, *tempp2;

  if (listhead -> in == TRUE)
    answer = FALSE;
  else
    answer = TRUE;

  for (stringp = listhead; stringp -> in != UNSET;
       stringp = stringp -> next) {
    if (STREQ(stringp -> name, "pages") && listhead != ispagehead) {
      if (ispage == TRUE ||
	  (ispage == UNSET && included(name, UNSET, ispagehead))) {
	if (stringp -> in == TRUE)
	  answer = TRUE;
	else
	  answer = FALSE;
      }
    }
    else if (wildmatch(name, stringp -> name, &tempp1, &tempp2)) {
      if (stringp -> in == TRUE)
	answer = TRUE;
      else
	answer = FALSE;
    }
  }

  return(answer);
}

/* use included() to see whether a certain item is wanted */

flag itemwanted(char *name, flag ispage, char codeletter)
{
  extern struct include *wantfilehead, *wanthosthead, *wantrefhead;

  if (codeletter == 'S')
    return(included(name, ispage, wanthosthead));
  else if (codeletter == 'r')
    return(included(name, ispage, wantfilehead));
  else /* codeletter == 'f' */
    return(included(name, ispage, wantrefhead));
}
