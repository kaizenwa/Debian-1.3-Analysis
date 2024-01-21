/* analform.c 2.0 -- parse the output of the analog form interface */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

extern FILE *popen();  /* to stop gcc complaining */
extern int pclose();
extern int putenv();

/* You must change the next line to indicate where the analog program lives */

#define COMMAND "/usr/local/etc/httpd/analog/analog"
#define MAXARGLENGTH (2048)   /* should be plenty */
#define OK (0)
#define ERR (-1)
#define STRLENGTH (64)

typedef int flag;

int unhttp(char *url)     /* converts back all http special characters */
{
  char *tempp;
  char tempstr[MAXARGLENGTH];
  int i;

  /* firstly, change +'s into spaces */

  for (i = strlen(url) - 1; i >= 0; i--) {
    if (url[i] == '+')
      url[i] = ' ';
  }

  /* secondly change %7E to ~, etc. */

  tempp = url;
  while ((tempp = strchr(tempp, '%')) != NULL) {
    sscanf(tempp + 1, "%2x", &i);
    if (i >= 0x20 && i < 0x7F) {
      *tempp = i;
      strcpy(tempstr, tempp + 3);
      strcpy(tempp + 1, tempstr);
      /* strcpy(tempp + 1, tempp + 3) may not be safe on all machines
	 (overlapping arguments) */
    }
    tempp++;
  }

  /* Finally, check no unsafe characters. */

  for (tempp = url; *tempp != '\0'; tempp++) {
    if (*tempp == ';' || *tempp == '\n' || *tempp == '\r' || *tempp == '`' ||
	*tempp == '|' || *tempp == '<' || *tempp == '>')
      return(ERR);
  }

  return(OK);
}

void genopts(FILE *thepipe, char name[9], int sortby, char *astr, char *cstr)
{
  if (sortby <= 4 && sortby >= 0) {
    fprintf(thepipe, "%sSORTBY ", name);
    if (sortby == 4)
      fprintf(thepipe, "PAGES\n");
    else if (sortby == 3)
      fprintf(thepipe, "RANDOM\n");
    else if (sortby == 2)
      fprintf(thepipe, "ALPHABETICAL\n");
    else if (sortby == 1)
      fprintf(thepipe, "BYTES\n");
    else if (sortby == 0)
      fprintf(thepipe, "REQUESTS\n");
    if (sortby == 1) {
      if (cstr[0] != '\0')
	fprintf(thepipe, "%sMINBYTES %s\n", name, cstr);
    }
    else if (sortby == 4) {
      if (astr[0] != '\0')
	fprintf(thepipe, "%sMINPAGES %s\n", name, astr);
    }
    else {
      if (astr[0] != '\0')
	fprintf(thepipe, "%sMINREQS %s\n", name, astr);
    }
  }
}

int main()
{
  extern void exit();

  /* the input */
  char *argstring;
  char *nextarg;
  char *nextval;

  /* the variables that can be read in */
  int xq = 2, mq = 2, Wq = 2, dq = 2, Dq = 2, hq = 2, oq = 2, Sq = 2, tq = 2;
  int iq = 2, rq = 2, fq = 2, bq = 2, Bq = 2, cq = 2, eq = 2, Hq = 2, Vq = 0;
  char mg = '\0', Wg = '\0', dg = '\0', Dg = '\0', hg = '\0', Hg = '\0';
  int os = 6, Ss = 6, is = 6, rs = 6, fs = 6, Bs = 6, bs = 6, ts = 6;
  int ou = 0, ch = 3, gr = 2;
  char oa[STRLENGTH], Sa[STRLENGTH], ia[STRLENGTH], ra[STRLENGTH];
  char ta[STRLENGTH], fa[STRLENGTH], Ba[STRLENGTH], ba[STRLENGTH];
  char oc[STRLENGTH], Sc[STRLENGTH], ic[STRLENGTH], rc[STRLENGTH];
  char tc[STRLENGTH], fc[STRLENGTH], Bc[STRLENGTH], bc[STRLENGTH];
  int dirlevel = 0;
  char reqtype = 'd', reqlinks = 'd';
  char *from = NULL, *to = NULL;
  char *fonly = NULL, *fign = NULL;
  char *honly = NULL, *hign = NULL;
  char *org = NULL, *home = NULL;
  char *logfile = NULL, *reflog = NULL, *browlog = NULL;
  char *errlog = NULL, *cachefile = NULL, *cg = NULL, *cm = NULL;
  char *TZ = NULL;
  char TZenv[STRLENGTH];
  int wa = 1;

  FILE *thepipe;
  int ret;

  oa[0] = '\0';
  Sa[0] = '\0';
  ia[0] = '\0';
  ra[0] = '\0';
  fa[0] = '\0';
  ba[0] = '\0';
  Ba[0] = '\0';
  oc[0] = '\0';
  Sc[0] = '\0';
  ic[0] = '\0';
  rc[0] = '\0';
  fc[0] = '\0';
  bc[0] = '\0';
  Bc[0] = '\0';

  if ((argstring = getenv("QUERY_STRING")) == NULL) {
    printf("Content-type: text/plain\n\n");
    printf("Error: cannot find environment variable QUERY_STRING\n");
    exit(ERR);
  }

  if (strlen(argstring) >= MAXARGLENGTH - 1) {
    fprintf(stderr, "Analog form interface: Security warning on request from %s (%s): QUERY_STRING too long\n",
     (getenv("REMOTE_HOST") == NULL)?"unknown host":getenv("REMOTE_HOST"),
     (getenv("REMOTE_ADDR") == NULL)?"unknown address":getenv("REMOTE_ADDR"));
    printf("Content-type: text/plain\n\n");
    printf("Error in QUERY_STRING\n");
    exit(ERR);
  }

  ret = unhttp(argstring);
  if (ret == ERR) {      /* suspicious characters in argstring */
    fprintf(stderr, "Analog form interface: Security warning on request from %s (%s): QUERY_STRING contains unusual characters\n",
     (getenv("REMOTE_HOST") == NULL)?"unknown host":getenv("REMOTE_HOST"),
     (getenv("REMOTE_ADDR") == NULL)?"unknown address":getenv("REMOTE_ADDR"));
    printf("Content-type: text/plain\n\n");
    printf("Error in QUERY_STRING\n");
    exit(ERR);
  }

  nextarg = strtok(argstring, "&");

  while (nextarg != NULL) {
    nextval = strchr(nextarg, '=') + 1;
    if (nextval[0] != '\0') {
      switch(nextarg[0]) {
      case 'b':
	switch(nextarg[1]) {
	case 'a':
	  strncpy(ba, nextval, STRLENGTH - 1);
	  break;
	case 'b':
	  strcpy(ba, "-");
	  strncat(ba, nextval, STRLENGTH - 2);
	  break;
	case 'c':
	  strncpy(bc, nextval, STRLENGTH - 1);
	  break;
	case 'd':
	  strcpy(bc, "-");
	  strncat(bc, nextval, STRLENGTH - 2);
	  break;
	case 'q':
	  bq = atoi(nextval);
	  break;
	case 's':
	  bs = atoi(nextval);
	  break;
	}
	break;
      case 'B':
	switch(nextarg[1]) {
	case 'a':
	  strncpy(Ba, nextval, STRLENGTH - 1);
	  break;
	case 'b':
	  strcpy(Ba, "-");
	  strncat(Ba, nextval, STRLENGTH - 2);
	  break;
	case 'c':
	  strncpy(Bc, nextval, STRLENGTH - 1);
	  break;
	case 'd':
	  strcpy(Bc, "-");
	  strncat(Bc, nextval, STRLENGTH - 2);
	  break;
	case 'q':
	  Bq = atoi(nextval);
	  break;
	case 's':
	  Bs = atoi(nextval);
	  break;
	}
	break;
      case 'c':
	switch (nextarg[1]) {
	case 'g':
	  cg = nextval;
	  break;
	case 'h':
	  ch = atoi(nextval);
	  break;
	case 'm':
	  cm = nextval;
	  break;
	case 'q':
	  cq = atoi(nextval);
	  break;
	}
	break;
      case 'd':
	switch(nextarg[1]) {
	case 'g':
	  dg = nextval[0];
	  break;
	case 'q':
	  dq = atoi(nextval);
	  break;
	}
	break;
      case 'D':
	switch(nextarg[1]) {
	case 'g':
	  Dg = nextval[0];
	  break;
	case 'q':
	  Dq = atoi(nextval);
	  break;
	}
	break;
      case 'e':
	eq = atoi(nextval);
	break;
      case 'f':
	switch(nextarg[1]) {
	case 'a':
	  strncpy(fa, nextval, STRLENGTH - 1);
	  break;
	case 'b':
	  strcpy(fa, "-");
	  strncat(fa, nextval, STRLENGTH - 2);
	  break;
	case 'c':
	  strncpy(fc, nextval, STRLENGTH - 1);
	  break;
	case 'd':
	  strcpy(fc, "-");
	  strncat(fc, nextval, STRLENGTH - 2);
	  break;
	case 'i':
	  fign = nextval;
	  break;
	case 'q':
	  fq = atoi(nextval);
	  break;
	case 'r':
	  from = nextval;
	  break;
	case 's':
	  fs = atoi(nextval);
	  break;
	case 'y':
	  fonly = nextval;
	  break;
	}
	break;
      case 'g':
	gr = atoi(nextval);
	break;
      case 'h':
	switch(nextarg[1]) {
	case 'g':
	  hg = nextval[0];
	  break;
	case 'i':
	  hign = nextval;
	  break;
	case 'o':
	  home = nextval;
	  break;
	case 'q':
	  hq = atoi(nextval);
	  break;
	case 'y':
	  honly = nextval;
	  break;
	}
	break;
      case 'H':
	switch(nextarg[1]) {
	case 'g':
	  Hg = nextval[0];
	  break;
	case 'q':
	  Hq = atoi(nextval);
	  break;
	}
	break;
      case 'i':
	switch(nextarg[1]) {
	case 'a':
	  strncpy(ia, nextval, STRLENGTH - 1);
	  break;
	case 'b':
	  strcpy(ia, "-");
	  strncat(ia, nextval, STRLENGTH - 2);
	  break;
	case 'c':
	  strncpy(ic, nextval, STRLENGTH - 1);
	  break;
	case 'd':
	  strcpy(ic, "-");
	  strncat(ic, nextval, STRLENGTH - 2);
	  break;
	case 'e':
	  dirlevel = atoi(nextval);
	  break;
	case 'q':
	  iq = atoi(nextval);
	  break;
	case 's':
	  is = atoi(nextval);
	  break;
	}
	break;
      case 'l':
	switch(nextarg[1]) {
	case 'b':
	  browlog = nextval;
	  break;
	case 'c':
	  cachefile = nextval;
	  break;
	case 'e':
	  errlog = nextval;
	  break;
	case 'f':
	  reflog = nextval;
	  break;
	case 'o':
	  logfile = nextval;
	  break;
	}
	break;
      case 'm':
	switch(nextarg[1]) {
	case 'g':
	  mg = nextval[0];
	  break;
	case 'q':
	  mq = atoi(nextval);
	  break;
	}
	break;
      case 'o':
	switch(nextarg[1]) {
	case 'a':
	  strncpy(oa, nextval, STRLENGTH - 1);
	  break;
	case 'b':
	  strcpy(oa, "-");
	  strncat(oa, nextval, STRLENGTH - 2);
	  break;
	case 'c':
	  strncpy(oc, nextval, STRLENGTH - 1);
	  break;
	case 'd':
	  strcpy(oc, "-");
	  strncat(oc, nextval, STRLENGTH - 2);
	  break;
	case 'q':
	  oq = atoi(nextval);
	  break;
	case 'r':
	  org = nextval;
	  break;
	case 's':
	  os = atoi(nextval);
	  break;
	case 'u':
	  ou = atoi(nextval);
	  break;
	}
	break;
      case 'r':
	switch(nextarg[1]) {
	case 'a':
	  strncpy(ra, nextval, STRLENGTH - 1);
	  break;
	case 'b':
	  strcpy(ra, "-");
	  strncat(ra, nextval, STRLENGTH - 2);
	  break;
	case 'c':
	  strncpy(rc, nextval, STRLENGTH - 1);
	  break;
	case 'd':
	  strcpy(rc, "-");
	  strncat(rc, nextval, STRLENGTH - 2);
	  break;
	case 'l':
	  reqlinks = nextval[0];
	  break;
	case 'q':
	  rq = atoi(nextval);
	  break;
	case 's':
	  rs = atoi(nextval);
	  break;
	case 't':
	  reqtype = nextval[0];
	  break;
	}
	break;
      case 'S':
	switch(nextarg[1]) {
	case 'a':
	  strncpy(Sa, nextval, STRLENGTH - 1);
	  break;
	case 'b':
	  strcpy(Sa, "-");
	  strncat(Sa, nextval, STRLENGTH - 2);
	  break;
	case 'c':
	  strncpy(Sc, nextval, STRLENGTH - 1);
	  break;
	case 'd':
	  strcpy(Sc, "-");
	  strncat(Sc, nextval, STRLENGTH - 2);
	  break;
	case 'q':
	  Sq = atoi(nextval);
	  break;
	case 's':
	  Ss = atoi(nextval);
	  break;
	}
	break;
      case 't':
	switch(nextarg[1]) {
	case 'a':
	  strncpy(ta, nextval, STRLENGTH - 1);
	  break;
	case 'b':
	  strcpy(ta, "-");
	  strncat(ta, nextval, STRLENGTH - 2);
	  break;
	case 'c':
	  strncpy(tc, nextval, STRLENGTH - 1);
	  break;
	case 'd':
	  strcpy(tc, "-");
	  strncat(tc, nextval, STRLENGTH - 2);
	  break;
	case 'o':
	  to = nextval;
	  break;
	case 'q':
	  tq = atoi(nextval);
	  break;
	case 's':
	  ts = atoi(nextval);
	  break;
	}
	break;
      case 'T':
	TZ = nextval;
	break;
      case 'V':
	Vq = atoi(nextval);
	break;
      case 'w':
	wa = atoi(nextval);
	break;
      case 'W':
	switch(nextarg[1]) {
	case 'g':
	  Wg = nextval[0];
	  break;
	case 'q':
	  Wq = atoi(nextval);
	  break;
	}
	break;
      case 'x':
	xq = 1;
	break;
      }
    }
    nextarg = strtok((char *)NULL, "&");
  }

  /* OK, so we've read everything in, now send it to the program */
  
  if (TZ != NULL) {
    strcpy(TZenv, "TZ=");
    strcat(TZenv, TZ);
    putenv(TZenv);
  }

  if (Vq)
    thepipe = stdout;

  if (!Vq && (thepipe = popen(COMMAND " +g-", "w")) == NULL) {
    printf("Content-type: text/plain\n\n");
    printf("Error: cannot start analog program at %s\n", COMMAND);
  }

  else {
    printf("Content-type: text/%s\n\n", (Vq || ou)?"plain":"html");
    fflush(stdout);

    fprintf(thepipe, "OUTFILE stdout\n");

    if (cg != NULL)
      fprintf(thepipe, "CONFIGFILE %s\n", cg);
    if (xq < 2)
      fprintf(thepipe, "GENERAL %s\n", xq?"ON":"OFF");
    if (mq < 2)
      fprintf(thepipe, "MONTHLY %s\n", mq?"ON":"OFF");
    if (Wq < 2)
      fprintf(thepipe, "WEEKLY %s\n", Wq?"ON":"OFF");
    if (dq < 2)
      fprintf(thepipe, "DAILY %s\n", dq?"ON":"OFF");
    if (Dq < 2)
      fprintf(thepipe, "FULLDAILY %s\n", Dq?"ON":"OFF");
    if (hq < 2)
      fprintf(thepipe, "HOURLY %s\n", hq?"ON":"OFF");
    if (Hq < 2)
      fprintf(thepipe, "FULLHOURLY %s\n", Hq?"ON":"OFF");
    if (oq < 2)
      fprintf(thepipe, "DOMAIN %s\n", oq?"ON":"OFF");
    if (Sq < 2)
      fprintf(thepipe, "FULLHOSTS %s\n", Sq?"ON":"OFF");
    if (iq < 2)
      fprintf(thepipe, "DIRECTORY %s\n", iq?"ON":"OFF");
    if (rq < 2)
      fprintf(thepipe, "REQUEST %s\n", rq?"ON":"OFF");
    if (bq < 2)
      fprintf(thepipe, "BROWSER %s\n", bq?"ON":"OFF");
    if (Bq < 2)
      fprintf(thepipe, "FULLBROWSER %s\n", Bq?"ON":"OFF");
    if (cq < 2)
      fprintf(thepipe, "STATUS %s\n", cq?"ON":"OFF");
    if (eq < 2)
      fprintf(thepipe, "ERROR %s\n", eq?"ON":"OFF");
    if (fq < 2)
      fprintf(thepipe, "REFERRER %s\n", fq?"ON":"OFF");
    if (tq < 2)
      fprintf(thepipe, "FILETYPE %s\n", tq?"ON":"OFF");
    if (ch < 3)
      fprintf(thepipe, "COUNTHOSTS %s\n",
	      (ch == 2)?"APPROX":(ch?"ON":"OFF"));
    if (gr < 2)
      fprintf(thepipe, "GRAPHICAL %s\n", gr?"ON":"OFF");

    if (mq && mg != '\0')
      fprintf(thepipe, "MONTHGRAPH %c", mg);
    if (Wq && Wg != '\0')
      fprintf(thepipe, "WEEKGRAPH %c", Wg);
    if (hq && hg != '\0')
      fprintf(thepipe, "HOURGRAPH %c", hg);
    if (Hq && Hg != '\0')
      fprintf(thepipe, "FULLHOURGRAPH %c", Hg);
    if (dq && dg != '\0')
      fprintf(thepipe, "DAYGRAPH %c", dg);
    if (Dq && Dg != '\0')
      fprintf(thepipe, "FULLDAYGRAPH %c", Dg);

    if (oq)
      genopts(thepipe, "DOM", os, oa, oc);
    if (Sq)
      genopts(thepipe, "HOST", Ss, Sa, Sc);
    if (iq) {
      genopts(thepipe, "DIR", is, ia, ic);
      fprintf(thepipe, "DIRLEVEL %d\n", dirlevel);
    }
    if (rq) {
      genopts(thepipe, "REQ", rs, ra, rc);
      if (reqtype != 'd')
	fprintf(thepipe, "REQINCLUDE %s\n", (reqtype == 'f')?"*":"pages");
      if (reqlinks == 'n')
	fprintf(thepipe, "LINKEXCLUDE *\n");
      else if (reqlinks != 'd')
	fprintf(thepipe, "LINKINCLUDE %s\n", (reqlinks == 'f')?"*":"pages");
    }
    if (bq)
      genopts(thepipe, "BROW", bs, ba, bc);
    if (Bq)
      genopts(thepipe, "FULLBROW", Bs, Ba, Bc);
    if (fq)
      genopts(thepipe, "REF", fs, fa, fc);
    if (tq)
      genopts(thepipe, "TYPE", ts, ta, tc);

    if (ou < 3)
      fprintf(thepipe, "OUTPUT %s\n",
	      (ou == 2)?"PREFORMATTED":((ou == 1)?"ASCII":"HTML"));

    fprintf(thepipe, "WARNINGS %s\n", wa?"ON":"OFF");

    if (from != NULL)
      fprintf(thepipe, "FROM %s\n", from);
    if (to != NULL)
      fprintf(thepipe, "TO %s\n", to);
    if (org != NULL)
      fprintf(thepipe, "HOSTNAME \"%s\"\n", org);
    if (home != NULL)
      fprintf(thepipe, "HOSTURL %s\n", home);
    else
      fprintf(thepipe, "HOSTURL -\n");
    
    /* That just leaves the only's and ignore's and logfiles, which are a bit
       more complicated as we have to parse them still. Recycle 'nextarg'. */

    nextarg = strtok(fonly, " ,");   /* split at spaces and commas */
    while (nextarg != NULL) {
      fprintf(thepipe, "FILEINCLUDE %s\n", nextarg);
      nextarg = strtok((char *)NULL, " ,");
    }

    nextarg = strtok(fign, " ,");
    while (nextarg != NULL) {
      fprintf(thepipe, "FILEEXCLUDE %s\n", nextarg);
      nextarg = strtok((char *)NULL, " ,");
    }

    nextarg = strtok(honly, " ,");
    while (nextarg != NULL) {
      fprintf(thepipe, "HOSTINCLUDE %s\n", nextarg);
      nextarg = strtok((char *)NULL, " ,");
    }

    nextarg = strtok(hign, " ,");
    while (nextarg != NULL) {
      fprintf(thepipe, "HOSTEXCLUDE %s\n", nextarg);
      nextarg = strtok((char *)NULL, " ,");
    }

    nextarg = strtok(browlog, " ,");
    while (nextarg != NULL) {
      fprintf(thepipe, "BROWLOG %s\n", nextarg);
      nextarg = strtok((char *)NULL, " ,");
    }

    nextarg = strtok(errlog, " ,");
    while (nextarg != NULL) {
      fprintf(thepipe, "ERRLOG %s\n", nextarg);
      nextarg = strtok((char *)NULL, " ,");
    }

    nextarg = strtok(reflog, " ,");
    while (nextarg != NULL) {
      fprintf(thepipe, "REFLOG %s\n", nextarg);
      nextarg = strtok((char *)NULL, " ,");
    }

    nextarg = strtok(logfile, " ,");
    while (nextarg != NULL) {
      fprintf(thepipe, "LOGFILE %s\n", nextarg);
      nextarg = strtok((char *)NULL, " ,");
    }

    nextarg = strtok(cachefile, " ,");
    while (nextarg != NULL) {
      fprintf(thepipe, "CACHEFILE %s\n", nextarg);
      nextarg = strtok((char *)NULL, " ,");
    }

    if (cm != NULL)
      fprintf(thepipe, "CONFIGFILE %s\n", cm);

  }

  if (Vq)
    ret = 0;
  else {
    fflush(thepipe);
    ret = pclose(thepipe);
    if (ret != 0) {
      printf("Analog failed to run or returned an error code.\n");
      printf("Maybe your server's error log will give a clue why.\n");
    }
  }
  fflush(stdout);
  return(ret);

}
