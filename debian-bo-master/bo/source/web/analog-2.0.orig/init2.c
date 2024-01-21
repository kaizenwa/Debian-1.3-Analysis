/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** init2.c; some initialisation functions not using many global variables */
/* See also init.c */

#include "analhea2.h"

/*** The function that parses the domains file ***/

void domainscan(void)
{
  extern char *domainsfile;
  extern struct domain **ohead;
  extern struct alias *subdomshead;
  extern char *commandname;
  extern flag oq, warnq, anywarns;

  FILE *df;
  struct domain *p, *lastp;
  struct alias *subdomp;
  char tempstr[MAXSTRINGLENGTH], tempstr2[MAXSTRINGLENGTH];
  char inputline[MAXLINELENGTH];
  int rc;    /* return code */
  int domcode;   /* the code of a particular domain;
		    see the long comment just below */
  flag ispipe;

  df = fopenlog(domainsfile, "domains file", &ispipe);
  if (df == NULL) {
    if (warnq) {
      fprintf(stderr,"  will not construct domain report.\n");
      anywarns = ON;
    }
    oq = OFF;
    return;
  }

  /* We put the domains in the following order. aa = 0, ab = 2, ...,
     ba = 52, ... Domains with more than two letters go in the spaces;
     co = 134, com = 135, cp = 136. We assume that there are no two long
     domain names with the same two initial letters. Finally zz = 1350,
     zzspam = 1351, Unknown = 1352, Numerical = 1353. Each domain contains
     a 'nexti' element to show which is the next domain that occurs in the
     domains file. */

  else {   /* there is a domains file */
    p = ohead[DOMHASHSIZE - 2];
    p -> id = xmalloc(5);      /* enough for "*UNK\0" */
    strcpy(p -> id, "*UNK");
    p -> name = xmalloc(8);
    strcpy(p -> name, "unknown");
    p -> reqs = 0;
    p -> pages = 0;
    p -> bytes = 0;
    p -> next = (struct domain *)xmalloc(sizeof(struct domain));
    p -> next -> name = NULL;
    p = ohead[DOMHASHSIZE - 1];
    p -> id = xmalloc(5);
    strcpy(p -> id, "*NUM");
    p -> name = xmalloc(31);
    strcpy(p -> name, "unresolved numerical addresses");
    p -> reqs = 0;
    p -> pages = 0;
    p -> bytes = 0.0;
    p -> next = (struct domain *)xmalloc(sizeof(struct domain));
    p -> next -> name = NULL;
    lastp = p;
    p = p -> next;

    while (fgets(inputline, MAXLINELENGTH, df) != NULL) {
      rc = sscanf_domains(inputline, tempstr, tempstr2);
      if (rc == 2) {
	if ((!isdigit(tempstr[0])) && strchr(tempstr, '.') == NULL) {
                                                    /* new domain */
	  domcode = (tempstr[0] - 'a') * 52 + (tempstr[1] - 'a') * 2 +
	    (tempstr[2] != '\0');
	  if ((domcode < 0 || domcode > DOMHASHSIZE - 3) && warnq) {
	    fprintf(stderr, "%s: Warning: Ignoring corrupt line in domains file looking like\n", commandname);
	    fprintf(stderr,"  %s", inputline);
	    anywarns = ON;
	  }
	  p = ohead[domcode];
	  p -> id = xmalloc((size_t)((int)strlen(tempstr) + 1));
	  strcpy(p -> id, tempstr);
	  p -> name = xmalloc((size_t)((int)strlen(tempstr2) + 1));
	  strcpy(p -> name, tempstr2);
	  p -> reqs = 0;
	  p -> pages = 0;
	  p -> bytes = 0.0;
	  p -> next = (struct domain *)xmalloc(sizeof(struct domain));
	  p -> next -> name = NULL;
	  lastp -> nexti = domcode;
                            /* domlastp is the last domain we looked at */
	  lastp = p;
	  p = p -> next;
	}
	else if (warnq) {   /* old specification subdomain */
	  fprintf(stderr, "%s: Warning: Ignoring corrupt line in domains file looking like\n", commandname);
	  fprintf(stderr,"  %s", inputline);
	  anywarns = ON;
	}
      }
      else if (rc == 1 && warnq) {
	fprintf(stderr, "%s: Warning: Ignoring corrupt line in domains file looking like\n", commandname);
	fprintf(stderr, "  %s", inputline);
	anywarns = ON;
      }
    }

    lastp -> nexti = -1;   /* marker; last domain has no subsequent one */
    
    fcloselog(df, domainsfile, "domains file", ispipe);

    /* Now do the subdomains which we read in at configuration time */

    for (subdomp = subdomshead; subdomp -> from[0] != '\0';
	 subdomp = subdomp -> next)
      if (subdomp -> from[0] != '?')
	subdomadd(subdomp -> from, subdomp -> to);

  }     /* end else can read domains file */
}

/*** Now the function that parses the DNS cache ***/
/* Test dnsq before calling this */

#ifndef NODNS
void dnscachescan(void)
{
  extern char *dnsfile;
  extern time_t dnsstaletime;
  extern char *commandname;
  extern flag warnq, anywarns;

  FILE *cf;
  time_t timec;
  char number[MAXSTRINGLENGTH], alias[MAXSTRINGLENGTH];
  char inputline[MAXLINELENGTH];
  flag ispipe;

  cf = fopenlog(dnsfile, "DNS cache file", &ispipe);
  if (cf != NULL) {
    while (fgets(inputline, MAXLINELENGTH, cf) != NULL) {
      if (sscanf(inputline, "%ld %s%s", &timec, number, alias) != 3) {
	if (warnq) {  /* NB: Poss. problem if strings too long */
	  fprintf(stderr, "%s: Warning: Ignoring corrupt line in DNS cache file looking like\n", 
		  commandname);
	  anywarns = ON;
	}
      }
      else if (timec > dnsstaletime)
	dnshashadd(number, (alias[0] == '*')?NULL:alias, timec);
    }
    fcloselog(cf, dnsfile, "DNS cache file", ispipe);
  }
}
#endif

/*** Functions used in parsing the configuration file ***/
/* First some warning messages */

void configwarning(char *comname, char *inputline)
{
  extern char *commandname;
  extern flag warnq, anywarns;

  if (warnq) {
    fprintf(stderr, "%s: Warning: Not enough arguments to %s command: ignoring line\n", commandname, comname);
    fprintf(stderr, "  %s", inputline);
    if (inputline[MAX(strlen(inputline) - 1, 0)] != '\n')
      fprintf(stderr, "\n");
    anywarns = ON;
  }
}

void configwarning2(char *inputline)
{
  extern char *commandname;
  extern flag warnq, anywarns;

  if (warnq) {
    fprintf(stderr, "%s: Warning: Discarding illegal configuration command\n",
	    commandname);
    fprintf(stderr, "  %s", inputline);
    if (inputline[MAX(strlen(inputline) - 1, 0)] != '\n')
      fprintf(stderr, "\n");
    anywarns = ON;
  }
}

void configwarning3(char *comname, char *inputline)
{
  extern char *commandname;
  extern flag warnq, anywarns;

  if (warnq) {
    fprintf(stderr, "%s: Warning: Too many arguments to %s command: ignoring end of line\n", commandname, comname);
    fprintf(stderr, "  %s", inputline);
    if (inputline[MAX(strlen(inputline) - 1, 0)] != '\n')
      fprintf(stderr, "\n");
    anywarns = ON;
  }
}

/* Next some sub-functions to initialise configuration things */

void addonelogfile(struct loglist **p, char name[MAXSTRINGLENGTH],
		   char prefix[MAXSTRINGLENGTH])
{
  strncpy((*p) -> name, name, MAXSTRINGLENGTH - 1);
  (*p) -> name[MAXSTRINGLENGTH - 1] = '\0';
  strncpy((*p) -> prefix, prefix, MAXSTRINGLENGTH - 1);
  (*p) -> prefix[MAXSTRINGLENGTH - 1] = '\0';
  (*p) -> next = (struct loglist *)xmalloc(sizeof(struct loglist));
  *p = (*p) -> next;
  (*p) -> name[0] = '\0';
}

#ifndef NODIRENT
#ifndef VMSDIRENT
void addwildlogs(struct loglist **p, char name[MAXSTRINGLENGTH],
		 char prefix[MAXSTRINGLENGTH])
{
  extern char *commandname;
  extern flag warnq, anywarns;

  DIR *dirp;
  struct dirent *filep;
  struct stat *buf;
  char dirname[MAXSTRINGLENGTH];
  int dirnamelen;
  char filename[MAXSTRINGLENGTH];
  char *tempc, *temp1, *temp2;
  flag matched = FALSE;

  strcpy(dirname, name);
  for (tempc = dirname + strlen(dirname) - 1; tempc >= dirname &&
       *tempc != DIRSEP; tempc--)
    ;   /* run back to last DIRSEP if any */
  if (tempc < dirname) {
    strcpy(filename, dirname);
#ifdef MAC
    dirname[0] = '\0';
#else
    sprintf(dirname, ".%c", DIRSEP);
#endif
  }
  else {
    strcpy(filename, tempc + 1);
    *(tempc + 1) = '\0';
  }
  if ((dirp = opendir(dirname)) == NULL) {
    (*p) -> name[0] = '\0';
    if (warnq) {
      fprintf(stderr,
         "%s: Warning: can't open directory %s:\n  ignoring logfiles %s\n",
	      commandname, dirname, name);
      if (strchr(dirname, '*') != NULL || strchr(dirname, '?') != NULL)
	fprintf(stderr, "  (wildcards not allowed in directory name)\n");
      anywarns = ON;
    }
  }
  else {
    dirnamelen = strlen(dirname);
    buf = (struct stat *)xmalloc(sizeof(struct stat));
    while ((filep = readdir(dirp)) != NULL) {
      strncpy(dirname + dirnamelen, filep -> d_name,
	      (size_t)(MAXSTRINGLENGTH - dirnamelen - 1));
      stat(dirname, buf);    /* dirname now contains the complete filename */
      if (S_ISREG(buf -> st_mode) &&  /* is ordinary file */
	  wildmatch(filep -> d_name, filename, &temp1, &temp2)) {
	addonelogfile(p, dirname, prefix);
	matched = TRUE;
      }
    }
    closedir(dirp);
    if (!matched)
      addonelogfile(p, name, prefix);   /* so as to get proper error message */
  }
}
#else  /* VMSDIRENT */
/* The next function is due to Dave Jones */
void addwildlogs(struct loglist **p, char name[MAXSTRINGLENGTH],
		 char prefix[MAXSTRINGLENGTH])
{
    static char fspec[VMS_FSPEC_MAX], related[VMS_FSPEC_MAX];
    static char result[VMS_FSPEC_MAX];
    static $DESCRIPTOR(fspec_dx,fspec);
    static $DESCRIPTOR(related_dx,"");
    static $DESCRIPTOR(default_dx,".log");
    static $DESCRIPTOR(result_dx,result);
    char *space, *ques;
    long int context;
    int status, stsval, length, LIB$FIND_FILE(), LIB$FIND_FILE_END();
    flag matched;

    length = strlen ( name );
    if ( length >= VMS_FSPEC_MAX ) length = VMS_FSPEC_MAX - 1;
    strncpy ( fspec, name, length ); fspec[length] = '\0';
    while ( ques = strchr(fspec,'?') ) *ques = '%';
    fspec_dx.dsc$w_length = length;
    for ( matched = FALSE, context = 0; 
	1&(status=LIB$FIND_FILE ( &fspec_dx, &result_dx, &context,
		&default_dx, &related_dx, &stsval, (long *) 0 )); 
		matched = TRUE) {
	space = strchr ( result, ' ' );
	if ( !space ) space = &result[VMS_FSPEC_MAX-1];
	*space = '\0';
	addonelogfile ( p, result, prefix );
	/* Save last result to use as defaul for next lookup */
	strcpy ( related, result );
	related_dx.dsc$w_length = strlen(result);
	related_dx.dsc$a_pointer = related;
    }
    if ( context ) LIB$FIND_FILE_END ( &context );
    if (!matched)
      addonelogfile(p, name, prefix);   /* so as to get proper error message */
}
#endif
#endif  /* NODIRENT */

void addlogfile(struct loglist **p, char name[MAXSTRINGLENGTH],
		char prefix[MAXSTRINGLENGTH], flag wildexpand)
{         /* wildexpand only used if NODIRENT off */
  char *tempstr;
  struct stringlist *tempstrlist, *tempslp;

  /* NB Do all strtok's before calling subsiduary functions that use it too */
  tempstrlist = (struct stringlist *)xmalloc(sizeof(struct stringlist));
  tempstrlist -> name[0] = '\0';
  tempslp = tempstrlist;

  tempstr = strtok(name, ","); /* break at commas */
  addonestrlist(&tempslp, tempstr);
  while ((tempstr = strtok((char *)NULL, ",")) != NULL)
    addonestrlist(&tempslp, tempstr);

  /* now check for wildcards and add to logfile list */
  for ( ; tempstrlist -> name[0] != '\0'; tempstrlist = tempstrlist -> next) {
#ifndef NODIRENT
#ifndef VMSDIRENT
    if (wildexpand && (strchr(tempstrlist -> name, '*') != NULL ||
		       strchr(tempstrlist -> name, '?') != NULL))
#else /* VMSDIRENT; wildcards only supported for VMS file syntax, not Unix */
    if (wildexpand && (strchr(tempstrlist -> name, '/' ) == NULL))
#endif
      addwildlogs(p, tempstrlist -> name, prefix);
    else
#endif
      addonelogfile(p, tempstrlist -> name, prefix);
  }
}

void includeone(char *name, struct include **p, struct include *head, int in,
		char *comname, char *inputline)
{                          /* put something in an include list */
  extern char *commandname;
  extern flag warnq, anywarns;

  struct include *tempp, *oldp;
  int tempflag;
  char tempstr[MAXSTRINGLENGTH];

  if (in == UNSET) {   /* *ALLOW */
    tempflag = OFF;
    for (tempp = head; tempp -> in != UNSET; tempp = tempp -> next) {
      if (STREQ(tempp -> name, name) && tempp -> in == FALSE) {
	if (tempp == head)
	  tempp -> name[0] = '\0';
	else
	  oldp -> next = tempp -> next;
	tempflag = ON;
      }
      oldp = tempp;
    }
    if (!tempflag && warnq) {
      fprintf(stderr, "%s: Warning: %s command before corresponding EXCLUDE at\n", commandname, comname);
      fprintf(stderr, "  %s", inputline);
      if (inputline[MAX(strlen(inputline) - 1, 0)] != '\n')
	fprintf(stderr, "\n");
      anywarns = ON;
    }
  }
  else {
    strcpy(tempstr, name);
    if (STREQ(strtolower(tempstr), "pages"))
      strcpy((*p) -> name, "pages");
    else {
      strncpy((*p) -> name, name, MAXSTRINGLENGTH - 1);
      (*p) -> name[MAXSTRINGLENGTH - 1] = '\0';
    }
    (*p) -> in = in;
    (*p) -> next = (struct include *)xmalloc(sizeof(struct include));
    *p = (*p) -> next;
    (*p) -> in = UNSET;
  }
}

void include(char *name, struct include **p, struct include *head, int in,
	     char *comname, char *inputline, int rc, flag *maskq)
{                          /* put something(s) in/out of an include list */
  char *tempstr;

  if (rc < 2)
    configwarning(comname, inputline);
  else {
    if (rc > 2)
      configwarning3(comname, inputline);
    *maskq = ON;
    tempstr = strtok(name, ",");
    includeone(tempstr, p, head, in, comname, inputline);
    while ((tempstr = strtok((char *)NULL, ",")) != NULL)
      includeone(tempstr, p, head, in, comname, inputline);
  }
}

void configalias(char *from, char *to, struct alias **p, char *comname,
		 char *inputline, int rc)
{
  if (rc < 3)
    configwarning(comname, inputline);
  else if (from[0] == '\0' || to[0] == '\0')
    configwarning2(inputline);
  else {
    if (rc > 3)
      configwarning3(comname, inputline);
    strcpy((*p) -> from, from);
    strcpy((*p) -> to, to);
    (*p) -> next = (struct alias *)xmalloc(sizeof(struct alias));
    *p = (*p) -> next;
    (*p) -> from[0] = '\0';
  }
}

void fromtodate(char *tstr, struct timestruct *t, flag from,
		char *comname, char *inputline, int rc)
{  /* NB should we put more syntax checking in this function? */
  extern struct timestruct starttimec;
  extern int monthlength[];

  if (rc < 2)
    configwarning(comname, inputline);
  else if (STREQ(strtoupper(tstr), "OFF")) {
    if (from)
      t -> code = -INFINITY;
    else
      t -> code = INFINITY;
  }
  else if ((int)strlen(tstr) < 6)
    configwarning2(inputline);
  else {
    if (rc > 2)
      configwarning3(comname, inputline);
    if (*tstr == '-') {
      t -> year = starttimec.year;
      t -> year -= 10 * (*(++tstr) - '0');
      t -> year -= (*(++tstr) - '0');
    }
    else {
      t -> year = 10 * (*tstr - '0');
      t -> year += (*(++tstr) - '0');
      if (t -> year > 70)   /* (say) */
	t -> year += 1900;
      else
	t -> year += 2000;
    }
    tstr++;
    if (*tstr == '-') {
      t -> monthno = starttimec.monthno;
      t -> monthno -= 10 * (*(++tstr) - '0');
      t -> monthno -= (*(++tstr) - '0');
      while (t -> monthno < 0) {
	t -> monthno += 12;
	t -> year--;
      }
    }
    else if (*tstr == '+') {
      t -> monthno = starttimec.monthno;
      t -> monthno += 10 * (*(++tstr) - '0');
      t -> monthno += (*(++tstr) - '0');
      while (t -> monthno > 11) {
	t -> monthno -= 12;
	t -> year++;
      }
    }
    else {
      t -> monthno = 10 * (*tstr - '0');
      t -> monthno += (*(++tstr) - '0');
      t -> monthno--;  /* prog. months are 1 out from standard */
    }
    tstr++;
    if (*tstr == '-') {
      t -> date = starttimec.date;
      t -> date -= atoi(++tstr);
      while (t -> date <= 0) {
	t -> monthno--;
	if (t -> monthno < 0) {
	  t -> monthno += 12;
	  t -> year--;
	}
	t -> date += monthlength[t -> monthno] +
	  ISLEAPFEB(t -> monthno, t -> year);
      }
    }
    else if (*tstr == '+') {
      t -> date = starttimec.date;
      t -> date += atoi(++tstr);
      while (t -> date > monthlength[t -> monthno] +
	     ISLEAPFEB(t -> monthno, t -> year)) {
	t -> date -= monthlength[t -> monthno] +
	  ISLEAPFEB(t -> monthno, t -> year);
	t -> monthno++;
	if (t -> monthno > 11) {
	  t -> monthno -= 12;
	  t -> year++;
	}
      }
    }
    else {
      t -> date = 10 * (*tstr - '0');
      t -> date += (*(++tstr) - '0');
    }
    if (from) {
      t -> hr = 0;
      t -> min = 0;
    }
    else {
      t -> hr = 23;
      t -> min = 59;
    }
    t -> code = timecode(t -> date, t -> monthno, t -> year, t -> hr,
			 t -> min);
  }
}

void configstr(char *name, char *target, char *comname, char *inputline,
	       int rc)  /* NB: name must have size at least MSL */
{
  if (rc < 2)
    configwarning(comname, inputline);
  else {
    if (rc > 2)
      configwarning3(comname, inputline);
    strncpy(target, name, MAXSTRINGLENGTH - 1);
  }
}

/* a couple of functions for configstrlist() below */

void addonestrlist(struct stringlist **p, char *name)
{
  strncpy((*p) -> name, name, MAXSTRINGLENGTH - 1);
  (*p) -> name[MAXSTRINGLENGTH - 1] = '\0';
  (*p) -> next = (struct stringlist *)xmalloc(sizeof(struct stringlist));
  *p = (*p) -> next;
  (*p) -> name[0] = '\0';
}

#ifndef NODIRENT
#ifndef VMSDIRENT
void addwildstrlist(struct stringlist **p, char *name)
{    /* exactly the same as addwildlogs() above */
  extern char *commandname;
  extern flag warnq, anywarns;

  DIR *dirp;
  struct dirent *filep;
  struct stat *buf;
  char dirname[MAXSTRINGLENGTH];
  int dirnamelen;
  char filename[MAXSTRINGLENGTH];
  char *tempc, *temp1, *temp2;
  flag matched = FALSE;

  strcpy(dirname, name);
  for (tempc = dirname + strlen(dirname) - 1; tempc >= dirname &&
       *tempc != DIRSEP; tempc--)
    ;      /* run back to last DIRSEP if any */
  if (tempc < dirname) {
    strcpy(filename, dirname);
#ifdef MAC
    dirname[0] = '\0';
#else
    sprintf(dirname, ".%c", DIRSEP);
#endif
  }
  else {
    strcpy(filename, tempc + 1);
    *(tempc + 1) = '\0';
  }
  if ((dirp = opendir(dirname)) == NULL) {
    (*p) -> name[0] = '\0';
    if (warnq) {
      fprintf(stderr,
         "%s: Warning: can't open directory %s:\n  ignoring files %s\n",
	      commandname, dirname, name);
      if (strchr(dirname, '*') != NULL || strchr(dirname, '?') != NULL)
	fprintf(stderr, "  (wildcards not allowed in directory name)\n");
      anywarns = ON;
    }
  }
  else {
    dirnamelen = strlen(dirname);
    buf = (struct stat *)xmalloc(sizeof(struct stat));
    while ((filep = readdir(dirp)) != NULL) {
      strncpy(dirname + dirnamelen, filep -> d_name,
	      (size_t)(MAXSTRINGLENGTH - dirnamelen - 1));
      stat(dirname, buf);
      if (S_ISREG(buf -> st_mode) &&  /* is ordinary file */
	  wildmatch(filep -> d_name, filename, &temp1, &temp2)) {
	addonestrlist(p, dirname);
	matched = TRUE;
      }
    }
    closedir(dirp);
    if (!matched)
      addonestrlist(p, name);
  }
}
#else /* VMSDIRENT */
/* The next function is due to Dave Jones */
void addwildstrlist(struct stringlist **p, char *name)
{
    static char fspec[VMS_FSPEC_MAX], related[VMS_FSPEC_MAX];
    static char result[VMS_FSPEC_MAX];
    static $DESCRIPTOR(fspec_dx,fspec);
    static $DESCRIPTOR(related_dx,"");
    static $DESCRIPTOR(default_dx,".log");
    static $DESCRIPTOR(result_dx,result);
    char *space, *ques;
    long int context;
    int status, stsval, length, LIB$FIND_FILE(), LIB$FIND_FILE_END();
    flag matched;

    length = strlen ( name );
    if ( length >= VMS_FSPEC_MAX ) length = VMS_FSPEC_MAX - 1;
    strncpy ( fspec, name, length ); fspec[length] = '\0';
    while ( ques = strchr(fspec,'?') ) *ques = '%';
    fspec_dx.dsc$w_length = length;
    for ( matched = FALSE, context = 0; 
	1&(status=LIB$FIND_FILE ( &fspec_dx, &result_dx, &context,
		&default_dx, &related_dx, &stsval, (long *) 0 )); 
		matched = TRUE) {
	space = strchr ( result, ' ' );
	if ( !space ) space = &result[VMS_FSPEC_MAX-1];
	*space = '\0';
	addonestrlist ( p, result );
	/* Save last result to use as default for next lookup */
	strcpy ( related, result );
	related_dx.dsc$w_length = strlen(result);
	related_dx.dsc$a_pointer = related;
    }
    if ( context ) LIB$FIND_FILE_END ( &context );
    if (!matched)
      addonestrlist(p, name );   /* so as to get proper error message */
}
#endif
#endif /* #define NODIRENT */

void configstrlist(char *name, struct stringlist **p, char *comname,
		   char *inputline, int rc, flag wildexpand)
{   /* one or more of a list of strings (with filename matching as in */
    /* addlogfile()). NB in configstr() applies */
  char *tempstr;
  struct stringlist *tempstrlist, *tempslp;

  if (rc < 2)
    configwarning(comname, inputline);
  else {
    if (rc > 2)
      configwarning3(comname, inputline);
    tempstrlist = (struct stringlist *)xmalloc(sizeof(struct stringlist));
    tempstrlist -> name[0] = '\0';
    tempslp = tempstrlist;
    tempstr = strtok(name, ",");
    addonestrlist(&tempslp, tempstr);
    while ((tempstr = strtok((char *)NULL, ",")) != NULL)
      addonestrlist(&tempslp, tempstr);

    for ( ; tempstrlist -> name[0] != '\0';
	 tempstrlist = tempstrlist -> next) {
#ifndef NODIRENT
#ifndef VMSDIRENT
      if (wildexpand && (strchr(tempstrlist -> name, '*') != NULL ||
			 strchr(tempstrlist -> name, '?') != NULL))
#else /* VMSDIRENT */
      if (wildexpand && (strchr(tempstrlist -> name, '/' ) == NULL))
#endif
	addwildstrlist(p, tempstrlist -> name);
      else
#endif
	addonestrlist(p, tempstrlist -> name);
    }
  }
}

void configcols(char *cols, char *target, char *comname, char *inputline,
		int rc)
{                           /* as configstr, but extra length check */
  if (rc < 2)
    configwarning(comname, inputline);
  else {
    if (rc > 2 || (int)strlen(cols) > 6)
      configwarning3(comname, inputline);
    strncpy(target, cols, 6);
  }
}

void configchar(char *str, char *target, char *comname, char *inputline,
		int rc)
{                           /* read in a single character */
  if (rc < 2)
    configwarning(comname, inputline);
  else {
    if (rc > 2 || str[1] != '\0')
      configwarning3(comname, inputline);
    *target = str[0];
  }
}

void configint(char *number, int *target, char *comname, char *inputline,
	       int rc)
{
  if (rc < 2)
    configwarning(comname, inputline);
  else {
    if (rc > 2)
      configwarning3(comname, inputline);
    *target = atoi(number);
  }
}

void configsizet(char *number, size_t *target, char *comname, char *inputline,
		 int rc)  /* same as configint() except the target is size_t */
{
  if (rc < 2)
    configwarning(comname, inputline);
  else {
    if (rc > 2)
      configwarning3(comname, inputline);
    *target = MAX(atoi(number), 1);  /* we only use this for hash sizes, so
					we want them to be at least 1 */
  }
}

void configsortby(char *method, int *target, char *comname, char *inputline,
		  int rc)
{
  if (rc < 2)
    configwarning(comname, inputline);
  else {
    if (rc > 2)
      configwarning3(comname, inputline);
    strtoupper(method);
    if (STREQ(method, "REQUESTS"))
      *target = BYREQUESTS;
    else if (STREQ(method, "BYTES"))
      *target = BYBYTES;
    else if (STREQ(method, "PAGES"))
      *target = BYPAGES;
    else if (STREQ(method, "ALPHABETICAL"))
      *target = ALPHABETICAL;
    else if (STREQ(method, "RANDOM"))
      *target = RANDOMLY;
    else
      configwarning2(inputline);
  }
}

void onoff(char *method, flag *target, char *comname, char *inputline, int rc)
{
  if (rc < 2)
    configwarning(comname, inputline);
  else {
    if (rc > 2)
      configwarning3(comname, inputline);
    strtoupper(method);
    if (STREQ(method, "ON"))
      *target = ON;
    else if (STREQ(method, "OFF"))
      *target = OFF;
    else
      configwarning2(inputline);
  }
}

/*** Now some subfunctions for commandline() ***/

void clflag(flag *f, char *arg)   /* simple on/off switches */
{
  extern char *commandname;
  extern flag warnq, anywarns;

  if (arg[0] == '-')
    *f = OFF;
  else
    *f = ON;
  if (arg[2] != '\0' && warnq) {
    fprintf(stderr, "%s: Warning: ignoring extra text after %c%c option\n",
	    commandname, arg[0], arg[1]);
    anywarns = ON;
  }
}

void cldaterep(flag *f, char *graph, char *arg)
{
  extern char *commandname;
  extern flag warnq, anywarns;

  if (arg[0] == '-') {
    *f = OFF;
    if (arg[2] != '\0' && warnq) {
      fprintf(stderr, "%s: Warning: ignoring extra text after -%c option\n",
	      commandname, arg[1]);
      anywarns = ON;
    }
  }
  else {
    *f = ON;
    if (arg[2] != '\0') {
      if (arg[2] != 'b' && arg[2] != 'r' && arg[2] != 'B' && arg[2] != 'R'
	  && arg[2] != 'p' && arg[2] != 'P') {
	if (warnq) {
	  fprintf(stderr, "%s: Warning: unknown graph method %c at %s\n",
		  commandname, arg[2], arg);
	  anywarns = ON;
	}
      }
      else {
	*graph = arg[2];
	if (arg[3] != '\0') {
	  fprintf(stderr,
		  "%s: Warning: ignoring extra text after +%c%c option\n",
		  commandname, arg[1], arg[2]);
	  anywarns = ON;
	}
      }
    }
  }
}

void clgenrep(flag *f, int *sortby, char *minreqstr, char *minpagestr,
	      char *minbytestr, char *arg)    /* generic reports */
{
  extern char *commandname;
  extern flag warnq, anywarns;

  if (arg[0] == '-') {
    *f = OFF;
    if (arg[2] != '\0' && warnq) {
      fprintf(stderr, "%s: Warning: ignoring extra text after -%c option\n",
	      commandname, arg[1]);
      anywarns = ON;
    }
  }
  else {
    *f = ON;
    switch (arg[2]) {
    case '\0':
      break;
    case 'a':
    case 'A':
      *sortby = ALPHABETICAL;
      if (arg[3] != '\0')
	strcpy(minreqstr, arg + 3);
      break;
    case 'b':
    case 'B':
      *sortby = BYBYTES;
      if (arg[3] != '\0')
	strcpy(minbytestr, arg + 3);
      break;
    case 'p':
    case 'P':
      *sortby = BYPAGES;
      if (arg[3] != '\0')
	strcpy(minpagestr, arg + 3);
      break;
    case 'r':
    case 'R':
      *sortby = BYREQUESTS;
      if (arg[3] != '\0')
	strcpy(minreqstr, arg + 3);
      break;
    case 'x':
    case 'X':
      *sortby = RANDOMLY;
      if (arg[3] != '\0')
	strcpy(minreqstr, arg + 3);
      break;
    default:
      if (warnq) {
	fprintf(stderr, "%s: Warning: unknown sort method %c at %s\n",
		commandname, arg[2], arg);
	anywarns = ON;
      }
    }
  }
}

flag clfile(char *filename, char *arg)   /* read in a filename */
{
  extern char *commandname;
  extern flag warnq, anywarns;

  if (arg[0] == '-') {
    strcpy(filename, "none");
    if (arg[2] != '\0' && warnq) {
      fprintf(stderr, "%s: Warning: ignoring extra text after -%c option\n",
	      commandname, arg[1]);
      anywarns = ON;
    }
  }
  else if (arg[2] == '\0') {
    if (warnq) {
      fprintf(stderr, "%s: Warning: no filename supplied after +%c option\n",
	      commandname, arg[1]);
      fprintf(stderr, "  (or space left before filename)\n");
      anywarns = ON;
    }
    return(ERR);
  }
  else
    strncpy(filename, arg + 2, MAXSTRINGLENGTH - 1);
  return(OK);
}

/*** Functions for conversion to lower case of alias list and include list ***/

void alias_to_lower(struct alias *head)
{
  struct alias *p;

  for (p = head; p -> from[0] != '\0'; p = p -> next) {
    strtolower(p -> from);
    strtolower(p -> to);
  }
}

void include_to_lower(struct include *head)
{
  struct include *p;

  for (p = head; p -> in != UNSET; p = p -> next)
    strtolower(p -> name);
}

/*** And finally some functions in connection with printvbles() ***/

void pvfilelist(struct stringlist *head, char filetype[MAXSTRINGLENGTH])
{
  extern flag stdin_used;

  FILE *filep;
  struct stringlist *p;

  printf("%s to analyse:\n", filetype);
  if (head -> name[0] == '\0')
    printf("  none\n");
  for (p = head; p -> name[0] != '\0'; p = p -> next) {  /* for each file */
    printf("  %s", p -> name);
    if ((filep = fopen(p -> name, "r")) == NULL && !STREQ(p -> name, "stdin")
	&& !STREQ(p -> name, "-"))
      printf(":   Warning: cannot open that file");
    else
      fclose(filep);  /* NB So condl 3 lines back must be in that order */
    if (STREQ(p -> name, "stdin") || STREQ(p -> name, "-")) {
      if (stdin_used)
	printf("\n  Warning: attempt to use stdin twice will fail");
      stdin_used = TRUE;
    }
    printf("\n");
  }
}

void pvcols(char *cols)
{
  char *c;

  printf("  Columns:");
  if (*cols == '\0')
    printf("  none");
  else for (c = cols; *c != '\0'; c++) {
    switch(*c) {
    case 'b':
      printf("  %%bytes");
      break;
    case 'B':
      printf("  #bytes");
      break;
    case 'p':
      printf("  %%pages");
      break;
    case 'P':
      printf("  #pages");
      break;
    case 'r':
      printf("  %%requests");
      break;
    case 'R':
      printf("  #requests");
      break;
    }
  }
  printf("\n");
}

void pvtime(char name[15], flag q, char graph, int unit, char cols[], int rows)
{
  extern char sepchar;

  int i;

  printf("%s", name);
  for (i = 19 - (int)strlen(name); i > 0; i--)
    printf(" ");
  printf("%s\n", q?"ON":"OFF");
  if (q) {
    printf("  Drawing bar chart by ");
    if (graph == 'b' || graph == 'B')
      printf("bytes\n");
    else if (graph == 'p' || graph == 'P')
      printf("pages\n");
    else
      printf("requests\n");
    if (unit != 0) {
      printf("  Each character in the graph will represent");
      int3printf(stdout, unit, sepchar, 0);
      if (graph == 'b' || graph == 'B')
	printf("bytes\n");
      else if (graph == 'p' || graph == 'P')
	printf("pages\n");
      else
	printf("requests\n");
    }
    if (rows > 0)
      printf("  Maximum number of rows in report will be %d\n", rows);
    pvcols(cols);
  }
}

void pvgen(char name[17], flag q, int sortby, char *minreqstr,
	   char *minpagestr, char *minbytestr, char cols[], char singular[20],
	   char plural[21])
{
  int i;

  printf("%s", name);
  for (i = 19 - (int)strlen(name); i > 0; i--)
    printf(" ");
  printf("%s\n", q?"ON":"OFF");
  if (q) {
    printf("  ");
    whatincluded(stdout, sortby, minreqstr, minpagestr, minbytestr, singular,
		 plural, OFF, 'm');
    pvcols(cols);
  }
}

void pvinout(char name[14], struct include *head)
{
  struct include *p;

  if (head -> in != UNSET) {
    printf("Including (+) and excluding (-) the following %s:\n", name);
    printf("  All %s, then\n", (head -> in == TRUE)?"excluded":"included");
    for (p = head; p -> in != UNSET; p = p -> next)
      printf("  %c %s\n", (p -> in)?'+':'-', p -> name);
  }
}

void pvalias(char name[8], struct alias *head)
{
  struct alias *p;

  if (head -> from[0] != '\0') {
    printf("%s aliases:\n", name);
    for (p = head; p -> from[0] != '\0'; p = p -> next)
      printf("  %s -> %s\n", p -> from, p -> to);
  }
}
