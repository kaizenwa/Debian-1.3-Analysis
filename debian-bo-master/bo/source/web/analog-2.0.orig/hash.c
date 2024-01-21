/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

#include "analhea2.h"

/*** hash.c; the functions which do all the work in the hash tables. ***/

/*** The first few are all exactly the same, adding a certain number of
  requests and a certain number of bytes to the hash entry for that item,
  creating a new hash entry if none existed before. ***/

/* Add a genstruct to the hash table. Return a pointer to the object. */
struct genstruct *hashadd(struct genstruct **objhead, int hashsize,
			  char *name, int reqs, double bytes, int pages,
			  flag last7q, int *totalobjs, int *totalobjs7,
			  int *totalnew7, flag al, flag maskq, flag ispage,
			  struct genstruct *freespace,
			  int magicnumber, char codeletter)
{  /* last7q can == 2 meaning this one in last7 and pre7 */
  extern struct include *ispagehead;

  flag finished;
  struct genstruct *p, *lastp, *prevp;
  int tempint;

  /* First calculate name's "magic number" */

  if (magicnumber < 0)
    MAGICNO(magicnumber, name, hashsize);

  /* Now look through the magicnumber'th list for that object */

  finished = FALSE;
  p = (objhead[magicnumber]);
  lastp = p;
  prevp = p;
  while (p -> name != NULL && !finished) {
    if (STREQ(p -> name, name)) {   /* then done */
      p -> reqs += reqs;
      if (pages == UNSET)
	p -> pages += reqs * (p -> ispage);
      else
	p -> pages += pages;
      p -> bytes += bytes;
      if (reqs > 0) {   /* don't count false files */
	if (!(p -> last7) && !(p -> pre7)) { /* name prev. but with no reqs */
	  (*totalobjs)++;
	  if (last7q > 0) {
	    (*totalobjs7)++;
	    p -> last7 = TRUE;
	  }
	  if (last7q != 1)
	    p -> pre7 = TRUE;
	  else /* last7q == 1 */
	    (*totalnew7)++;
	}
	else if (last7q > 0 && !(p -> last7)) {
	  (*totalobjs7)++;
	  p -> last7 = TRUE;
	}
	else if (last7q != 1 && !(p -> pre7)) {
	  p -> pre7 = TRUE;
	  if (p -> last7)
	    (*totalnew7)--;     /* Have wrongly counted it as new in last 7 */
	}
      }
      /* keep the list in rough (not exact) order of number of
	 accesses for quicker searching; particularly useful if
	 the HASHSIZE is set too low. */
      if (p -> reqs > lastp -> reqs) {
	if (prevp == lastp) {   /* iff p is 2nd in the list */
	  objhead[magicnumber] = p;
	  lastp -> next = p -> next;
	  p -> next = lastp;
	}
	else {  /* p is 3rd or later in the list */
	  prevp -> next = p;
	  lastp -> next = p -> next;
	  p -> next = lastp;
	}
      }
      finished = TRUE;
    }
    else {      /* look at the next one */
      prevp = lastp;
      lastp = p;
      p = p -> next;
    }
  }

  if (!finished) {   /* reached the end of the list without success; new one */
    p -> name = (char *)xmalloc(strlen(name) + 1);
    strcpy(p -> name, name);
    if (reqs > 0) {
      (*totalobjs)++;
      if (last7q == 2) {
	(*totalobjs7)++;
	p -> last7 = TRUE;
	p -> pre7 = TRUE;
      }
      else if (last7q == 1) {
	(*totalobjs7)++;
	(*totalnew7)++;
	p -> last7 = TRUE;
	p -> pre7 = FALSE;
      }
      else {  /* last7q == 0 */
	p -> last7 = FALSE;
	p -> pre7 = TRUE;
      }
    }
    else {
      p -> last7 = FALSE;
      p -> pre7 = FALSE;
    }
    /* if aliasing previously done, ignore p -> alias */
    if (al) {
      p -> alias = NULL;
      tempint = 0;
    }
    else {   /* aliasing not previously done; need to alias and exclude */
      tempint = doalias(name, codeletter);
      if (tempint == 0)   /* no alias */
	p -> alias = NULL;
      else if (tempint == 1) { /* true alias */
	p -> alias = (char *)xmalloc(strlen(name) + 1);
	strcpy(p -> alias, name);
      }
      else  /* tempint == -1; discard */
	p -> wanted = FALSE;
      /* name is now alias or orig name as appropriate */
    }
    if (tempint >= 0) {
      if (ispage == UNSET)
	p -> ispage = included(name, UNSET, ispagehead);
      else
	p -> ispage = ispage;
      if (maskq)
	p -> wanted = itemwanted(name, p -> ispage, codeletter);
      else
	p -> wanted = TRUE;
    }
    p -> reqs = reqs;
    if (pages == UNSET)
      p -> pages = p -> reqs * p -> ispage;
    else
      p -> pages = pages;
    p -> bytes = bytes;
    if (freespace == (struct genstruct *)NULL)
      p -> next = (struct genstruct *)xmalloc(sizeof(struct genstruct));
    else
      p -> next = freespace;    /* NB: don't allocate p -> next -> next */
    p -> next -> name = NULL;   /* or allaliases will go wrong */
  }

  return(p);
}

#ifndef NODNS
/* Add an address to (or look up in) the DNS hash table. Based on hashadd()
   but much smaller. Return a pointer to the object. */
/* NB: Changes "number" to its alias. */
/* Use altimecode = (time_t)(-1) for uncached. */
/* Assumes dnsq and isdigit(hostn[0]) already tested. */
struct dnscache *dnshashadd(char *number, char *alias, time_t altimecode)
{
  extern struct dnscache **dnshead;
  extern size_t dnshashsize;
  extern time_t starttime;

  flag finished;
  struct dnscache *p, *lastp, *prevp;
  int magicnumber;
  int tempint;

  /* First calculate the "magic number" */

  MAGICNO(magicnumber, number, dnshashsize);

  /* Now look through the magicnumber'th list for that host */

  finished = FALSE;
  p = (dnshead[magicnumber]);
  lastp = p;
  prevp = p;
  while (p -> number != NULL && !finished) {
    if (STREQ(p -> number, number)) {  /* then done */
      finished = TRUE;
      if (p -> alias != NULL)
	strncpy(number, p -> alias, MAXSTRINGLENGTH - 1);
    }
    else {      /* look at the next one */
      prevp = lastp;
      lastp = p;
      p = p -> next;
    }
  }

  if (!finished) {   /* reached the end of the list without success; new one */
    p -> number = (char *)xmalloc(strlen(number) + 1);
    strcpy(p -> number, number);
    if (altimecode != (time_t)(-1)) {
      if (alias != NULL) {
	p -> alias = (char *)xmalloc(strlen(alias) + 1);
	strcpy(p -> alias, alias);
	strncpy(number, alias, MAXSTRINGLENGTH - 1);
      }
      else
	p -> alias = NULL;
      p -> altimecode = altimecode;
    }
    else {
      tempint = dnsresolve(number); /* which changes number to its alias */
      if (tempint == 0)   /* no alias */
	p -> alias = NULL;
      else {
	p -> alias = (char *)xmalloc(strlen(number) + 1);
	strcpy(p -> alias, number);
      }
      p -> altimecode = starttime;
    }
    p -> next = (struct dnscache *)xmalloc(sizeof(struct dnscache));
    p -> next -> number = NULL;
  }

  return(p);
}
#endif

/*** The domain hashadd function is different from the others, because we
  already know all domains, so there need be no clashes in the hash table. ***/

void domhashadd(char *hostn, int reqs, int pages, double bytes)
{
  extern struct domain **ohead, **Ohead;
  extern struct domain *wildOhead, *nwildOhead;
  extern size_t Ohashsize;
  extern int debug;

  int domcode;   /* domcode is as explained at the bit where we read the
		    domains in. It's the equivalent of magic number for
		    the other hashadd functions. */
  int magicnumber;  /* for subdomains */
  flag finished;
  struct domain *domp;
  char *tempp;
  char tempchar;

  domcode = hosttodomcode(hostn);
  if (domcode == DOMHASHSIZE - 2 && debug >= 2)
    fprintf(stderr, "U: %s\n", hostn);

  ohead[domcode] -> reqs += reqs;
  ohead[domcode] -> pages += pages;
  ohead[domcode] -> bytes += bytes;

  /* Next run through the list of wild subdomains. There are two cases;
     numerical and ordinary subdomains */

  if (!isdigit(hostn[(int)strlen(hostn) - 1])) {  /* non-numerical */

    for (domp = wildOhead; domp -> id != NULL; domp = domp -> next) {
      if(STREQ(domp -> id,
	       hostn + MAX((int)strlen(hostn) - (int)strlen(domp -> id), 0))) {
	/* run back to just after the previous . (or the initial character) */
	tempp = hostn + MAX((int)strlen(hostn) - (int)strlen(domp -> id), 0);
	while (tempp != hostn && *(tempp - 1) != '.')
	  tempp--;
	/* now add that one to the list of subdoms; it will get looked at
	   in the next stage */
	subdomadd(tempp, "?");
      }
    }

    /* now run through the subdomains this hostname could belong to and see
       if any of them are required to be analysed */

    tempp = strrchr(hostn, '.');  /* the final dot */

    if (tempp != NULL) {

      while (tempp != hostn) {
	tempp--;
	while (tempp != hostn && *(tempp - 1) != '.')
	  tempp--;
	MAGICNO(magicnumber, tempp, Ohashsize);
	finished = OFF;
	for (domp = Ohead[magicnumber]; domp -> name != NULL && !finished;
	     domp = domp -> next) {
	  if (STREQ(domp -> id, tempp)) {
	    domp -> reqs += reqs;
	    domp -> pages += pages;
	    domp -> bytes += bytes;
	    finished = ON;
	  }
	}
      }
    }

  }   /* end non-numerical subdomains; now do numerical ones */

  else {

    /* wild numerical subdomains */

    for (domp = nwildOhead; domp -> id != NULL; domp = domp -> next) {
      if(strncmp(domp -> id, hostn, (size_t)(int)strlen(domp -> id)) == 0) {
	/* run to the next . (or the end) */
	tempp = hostn + (int)strlen(domp -> id);
	while (*tempp != '.' && *tempp != '\0')
	  tempp++;
	/* now add that one to the subdoms */
	tempchar = *tempp;
	*tempp = '\0';   /* temporarily trucate the string after the subdom */
	subdomadd(hostn, "?");
	*tempp = tempchar;
      }
    }

    /* and now run through the ordinary subdoms for this numerical host */

    tempp = hostn + (int)strlen(hostn);

    while (tempp != hostn) {
      while (tempp != hostn && *tempp != '.' && *tempp != '\0')
	tempp--;
      if (tempp != hostn) {
	tempchar = *tempp;
	*tempp = '\0';   /* temporarily trucate the string again */
	MAGICNO(magicnumber, hostn, Ohashsize);
	finished = OFF;
	for (domp = Ohead[magicnumber]; domp -> name != NULL && !finished;
	     domp = domp -> next) {
	  if (STREQ(domp -> id, hostn)) {
	    domp -> reqs += reqs;
	    domp -> pages += pages;
	    domp -> bytes += bytes;
	    finished = ON;
	  }
	}
	*tempp = tempchar;
	tempp--;
      }
    }
  }

}

/*** Add a new subdomain to the list of subdomains ***/

void subdomadd(char *id, char *name)
{
  extern struct domain **Ohead;
  extern struct domain *wildOhead, *nwildOhead;
  extern size_t Ohashsize;

  struct domain *domp, *domnextp;
  int magicnumber;
  flag numeric;       /* whether it's a numerical subdomain */

  if (id[0] == '?')  /* we don't want it */
    ;

  else if (id[0] == '*') {  /* put at start wild list (order doesn't matter) */
    domnextp = wildOhead;
    wildOhead = (struct domain *)xmalloc(sizeof(struct domain));
    wildOhead -> next = domnextp;
    wildOhead -> id = xmalloc(strlen(id));
    strcpy(wildOhead -> id, id + 1);
  }

  else if (id[(int)strlen(id) - 1] == '*') {
    domnextp = nwildOhead;
    nwildOhead = (struct domain *)xmalloc(sizeof(struct domain));
    nwildOhead -> next = domnextp;
    nwildOhead -> id = xmalloc(strlen(id));
    strncpy(nwildOhead -> id, id, strlen(id) - 1);
    *(nwildOhead -> id + (int)strlen(id) - 1) = '\0';
  }

  else if (id[0] == '%') {   /* token representing "all numerical domains" */
    domnextp = nwildOhead;
    nwildOhead = (struct domain *)xmalloc(sizeof(struct domain));
    nwildOhead -> next = domnextp;
    nwildOhead -> id = xmalloc(1);
    nwildOhead -> id[0] = '\0';
  }

  else {
    MAGICNO(magicnumber, id, Ohashsize);
    domp = Ohead[magicnumber];

    /* look through that list and slot it in alphabetically
       (for quicker finding than random if it's repeated later) */

    if (!isdigit(id[(int)strlen(id) - 1])) {
      reversehostname(id);
      numeric = FALSE;
    }
    else
      numeric = TRUE;

    /* if it goes at the beginning of the list, slot it in there */
    if (domp -> name == NULL || strcmp(domp -> revid, id) > 0) {
      domnextp = domp;
      domp = (struct domain *)xmalloc(sizeof(struct domain));
      Ohead[magicnumber] = domp;
      domp -> revid = xmalloc(strlen(id) + 1);
      strcpy(domp -> revid, id);
      domp -> id = xmalloc(strlen(id) + 1);
      if (!numeric)
	reversehostname(id);
      strcpy(domp -> id, id);
      domp -> name = xmalloc(strlen(name) + 1);
      strcpy(domp -> name, name);
      domp -> reqs = 0;
      domp -> pages = 0;
      domp -> bytes = 0;
      domp -> next = domnextp;
    }
    else if (strcmp(domp -> revid, id) == 0) {
      if (!numeric)
	reversehostname(id);
    }
    else {   /* run to right place in alphabet */
      while (domp -> next -> name != NULL &&
	     strcmp(domp -> next -> revid, id) < 0)
	domp = domp -> next;
      if (domp -> next -> name != NULL &&
	  strcmp(domp -> next -> revid, id) == 0) {
	if (!numeric)
	  reversehostname(id);   /* so as to leave it unchanged on exit */
      }
      else {
	domnextp = domp -> next;
	domp -> next = (struct domain *)xmalloc(sizeof(struct domain));
	domp = domp -> next;
	domp -> revid = xmalloc(strlen(id) + 1);
	strcpy(domp -> revid, id);
	domp -> id = xmalloc(strlen(id) + 1);
	if (!numeric)
	  reversehostname(id);
	strcpy(domp -> id, id);
	domp -> name = xmalloc(strlen(name) + 1);
	strcpy(domp -> name, name);
	domp -> reqs = 0;
	domp -> pages = 0;
	domp -> bytes = 0;
	domp -> next = domnextp;
      }
    }
  }
}

/*** Check if we want a certain referrer, and add it ***/

void addref(char *fromurl, char *filename, flag ispage, double bytes,
	    flag last7q, flag filemaskq)
     /* last ON if extern one ON and not yet done */
{
  extern flag refmaskq;
  extern struct genstruct **fhead2;
  extern struct include *wantfilehead, *ispagehead;
  extern int total_good_refs, total_masked_refs, total_ref_pages;
  extern double total_ref_bytes;
  extern size_t fhashsize;

  int tempint;

  if (!filemaskq || included(filename, UNSET, wantfilehead)) {
    /* need to check this every line of separate log unfortunately, because
       it's not recorded in hash table. Also ispage below. (Not v. efficient).
       NB for combined log, this local filemaskq is OFF and ispage is done */
    if (ispage == UNSET)
      ispage = included(filename, UNSET, ispagehead);
    if (hashadd(fhead2, fhashsize, fromurl, 1, bytes, ispage, last7q,
		&tempint, &tempint, &tempint, OFF, refmaskq, UNSET,
		(struct genstruct *)NULL, -1, 'f') -> wanted) {
      ++total_good_refs;
      total_ref_pages += ispage;
      total_ref_bytes += bytes;
    }
    else
      ++total_masked_refs;  /* not wanted because referrer excluded */
  }
  else
    ++total_masked_refs;    /* not wanted because 'referee' excluded */
}

/*** Process a browser line, and add it to the lists ***/
/* (Little to do, so we don't use a browser alias function) */

void addbrowser(char *browser, flag ispage, double bytes, flag last7q)
{
  extern flag bq, Bq;
  extern struct genstruct **bhead, **Bhead;
  extern int total_good_brows, total_brow_pages;
  extern double total_brow_bytes;
  extern size_t Bhashsize, bhashsize;
  extern struct alias *browaliashead;

  char *c;
  int tempint;

  /* if the browser name is just "-", ignore it */

  if (!STREQ(browser, "-")) {

    /* cut (illegal) "via"s (e.g., via proxy gateway or via Harvest cache) */
    if ((c = strstr(browser, " via ")) != NULL) {
      while (*c == ' ' && c != browser)
	c--;
      *(c + 1) = '\0';
    }

    /* cut trailing spaces */
    for (c = browser + strlen(browser); *(c - 1) == ' ' && c > browser; c--)
      *(c - 1) =  '\0';

    doaliaslist(browser, browaliashead);   /* user-requested aliases */

    ++total_good_brows;
    total_brow_pages += ispage;
    total_brow_bytes += bytes;

    /* add to full browser report */
    if (Bq)
      hashadd(Bhead, Bhashsize, browser, 1, bytes, ispage, last7q,
	      &tempint, &tempint, &tempint, ON, OFF, OFF,
	      (struct genstruct *)NULL, -1, 'B');

    if (bq) {

      /* generate shortened name */
      if ((c = strchr(browser, '/')) != NULL)
	*c = '\0';

      /* special cases */
      if (strstr(browser, "Mosaic") != NULL ||
	  strstr(browser, "mosaic") != NULL)
	strcpy(browser, "Mosaic");

      if (STREQ(browser, "Mozilla") &&
	  strstr(browser + 10, "ompatible") != NULL)
	strcpy(browser, "Mozilla (compatible)");

      /* and add to browser summary */
      hashadd(bhead, bhashsize, browser, 1, bytes, ispage, last7q, &tempint,
	      &tempint, &tempint, ON, OFF, OFF, (struct genstruct *)NULL,
	      -1, 'b');
    }
  }
}

/*** And one to add an error to the list of errors ***/

void adderr(char *errstr)
{
  extern char errs[NO_ERRS][MAXERRLENGTH];
  extern int errors[NO_ERRS];
  extern int debug;

  char e1[MAXLINELENGTH], e2[MAXSTRINGLENGTH];
  int i;
  flag done = OFF;

  if (errstr != NULL && errstr[0] != '\0') { /* shouldn't occur, but be safe */
    strcpy(e1, errstr);
    strtoupper(e1);
    for (i = 0; !done; i++) {
      strcpy(e2, errs[i]);
      if (strstr(e1, strtoupper(e2)) != NULL) {
	done = ON;
	errors[i]++;
	if (debug >= 2 && i == NO_ERRS - 1)  /* unknown error type */
	  fprintf(stderr, "E: %s\n", errstr);
      }
    }
  }
}

/*** Next a function to do an approx count of the number of distinct hosts ***/
/* First some utilities for it */

flag approxhostfilled(char *space, unsigned int i)
{    /* whether there is already a 1 at entry i of the table */
  unsigned int j, k;

  j = i / 8;
  k = i % 8;

  return((*(space + j) >> k) & 1);
}

void approxhostfill(char *space, unsigned int i)
{    /* put a 1 at entry i; ASSUMES IT IS CURRENTLY EMPTY */
  unsigned int j, k;

  j = i / 8;
  k = i % 8;
  *(space + j) += (1 << k);

}

void approxhosthashadd(char *hostn, flag last7q)
{
  extern char *approxhostspace, *approxhostspace7;
  extern size_t approxhostsize;
  extern int no_hosts, no_hosts7, no_new_hosts7;
  extern flag q7;

  register unsigned int magicnumber;
  char *storehostn;
  flag seen1, seen2, seen3, seen4; /* whether we'd already seen that number */
  flag seen17, seen27, seen37, seen47;  /* ditto in last 7 days */
  flag seen, seen7;        /* whether we've seen all 4 numbers before */
  int approxhostsize8;
  /* NB Note approxhostfill assumes empty; be careful about two equal magic
     numbers for some host */
  /* NB2 We really do have to calculate all 4 numbers every time; even though
     if any is not previously seen we have a new host, we still have to fill
     the later ones up. */

  approxhostsize8 = 8 * approxhostsize;

  magicnumber = 0;  /* 1st magic number: c_i.101^i */
  for (storehostn = hostn; *hostn != '\0'; hostn++) {
    magicnumber = 101 * magicnumber + *hostn;
    while (magicnumber >= approxhostsize8)
      magicnumber -= approxhostsize8;
  }
  seen1 = approxhostfilled(approxhostspace, magicnumber);
  if (!seen1 && (!q7 || !last7q))
    /* if q7 only pre-7 go here (we need to know pre- and last-);
       if !q7 everything does */
    approxhostfill(approxhostspace, magicnumber);
  if (q7) {
    seen17 = approxhostfilled(approxhostspace7, magicnumber);
    if (!seen17 && last7q)
      approxhostfill(approxhostspace7, magicnumber);
  }

  magicnumber = 0;   /* 2nd magic number: c_i.101^{n-i} */
  for (hostn--; hostn != storehostn; hostn--) {
    magicnumber = 101 * magicnumber + *hostn;
    while (magicnumber >= approxhostsize8)
      magicnumber -= approxhostsize8;
  }
  seen2 = approxhostfilled(approxhostspace, magicnumber);
  if (!seen2 && (!q7 || !last7q))
    approxhostfill(approxhostspace, magicnumber);
  if (q7) {
    seen27 = approxhostfilled(approxhostspace7, magicnumber);
    if (!seen27 && last7q)
      approxhostfill(approxhostspace7, magicnumber);
  }

  magicnumber = 0;  /* 3rd magic number: c_i.103^i */
  for ( ; *hostn != '\0'; hostn++) {
    magicnumber = 103 * magicnumber + *hostn;
    while (magicnumber >= approxhostsize8)
      magicnumber -= approxhostsize8;
  }
  seen3 = approxhostfilled(approxhostspace, magicnumber);
  if (!seen3 && (!q7 || !last7q))
    approxhostfill(approxhostspace, magicnumber);
  if (q7) {
    seen37 = approxhostfilled(approxhostspace7, magicnumber);
    if (!seen37 && last7q)
      approxhostfill(approxhostspace7, magicnumber);
  }

  magicnumber = 0;   /* 4th magic number: c_i.103^{n-i} */
  for (hostn--; hostn != storehostn; hostn--) {
    magicnumber = 103 * magicnumber + *hostn;
    while (magicnumber >= approxhostsize8)
      magicnumber -= approxhostsize8;
  }
  seen4 = approxhostfilled(approxhostspace, magicnumber);
  if (!seen4 && (!q7 || !last7q))
    approxhostfill(approxhostspace, magicnumber);
  if (q7) {
    seen47 = approxhostfilled(approxhostspace7, magicnumber);
    if (!seen47 && last7q)
      approxhostfill(approxhostspace7, magicnumber);
    seen7 = seen17 && seen27 && seen37 && seen47;
  }

  seen = seen1 && seen2 && seen3 && seen4;

  if (!q7) {
    if (!seen)   /* new host */
      ++no_hosts;
  }

  else {    /* q7 */
    if (!seen && !seen7) {
      ++no_hosts;
      if (last7q) {
	++no_hosts7;
	++no_new_hosts7;
      }
    }
    else if (!seen && seen7 && !last7q)  /* it wasn't really a new host 7 */
      --no_new_hosts7;
    else if (seen && !seen7 && last7q)
      ++no_hosts7;

  }    /* end else (= if q7) */

}   /* end approxhosthashadd() */

/*** Next the functions to do with dates ***/
/* add to the number of requests for a particular month */
void addmonthlydata(int year, int monthno, int reqs, int pages, double bytes)
{
  extern struct monthly *firstm, *lastm;
  extern struct timestruct firsttime, lasttime;
  extern flag mback;

  struct monthly *mp;
  int i;

  mp = mback?lastm:firstm;

  for (i = mback?(lasttime.year - year):(year - firsttime.year); i > 0; i--)
    mp = mp -> next;       /* run to the right year */

  mp -> reqs[monthno] += reqs;   /* and add to the right month in that year */
  mp -> pages[monthno] += pages;
  mp -> bytes[monthno] += bytes;

}

/* add to the number of requests for a particular day */
void adddailydata(int year, int monthno, int date, int reqs, int pages,
		  double bytes)
{
  extern struct daily *firstD, *lastD;
  extern struct timestruct firsttime, lasttime;
  extern flag Dback;

  struct daily *dp;
  int i;

  dp = Dback?lastD:firstD;

  for (i = Dback?((lasttime.year - year) * 12 + lasttime.monthno - monthno):
       ((year - firsttime.year) * 12 + monthno - firsttime.monthno);
       i > 0; i--)
    dp = dp -> next;       /* run to the right month */

  dp -> reqs[date - 1] += reqs;  /* and add to the right date */
  dp -> pages[date - 1] += pages;
  dp -> bytes[date - 1] += bytes;

}

/* add to the number of requests for a particular hour */
void addhourlydata(int year, int monthno, int date, int hr, int reqs,
		   int pages, double bytes)
{
  extern struct hourly *firstH, *lastH;
  extern struct timestruct firsttime, lasttime;
  extern flag Hback;

  struct hourly *hp;
  int i;

  hp = Hback?lastH:firstH;

  for (i = Hback?minsbetween(date, monthno, year, 0, 0, lasttime.date,
			     lasttime.monthno, lasttime.year, 0, 0) / 1440:
       minsbetween(firsttime.date, firsttime.monthno, firsttime.year, 0, 0,
		   date, monthno, year, 0, 0) / 1440;
       i > 0; i--)
    hp = hp -> next;       /* run to the right day */

  hp -> reqs[hr] += reqs;   /* and add to the right hour */
  hp -> pages[hr] += pages;
  hp -> bytes[hr] += bytes;

}

/* add to the number of requests for a particular week */
void addweeklydata(int year, int monthno, int date, int reqs, int pages,
		   double bytes)
{
  extern struct weekly *firstW, *lastW;
  extern flag Wback;

  struct weekly *wp;

  wp = Wback?lastW:firstW;

  while(minsbetween(wp -> start.date, wp -> start.monthno, wp -> start.year,
		    0, 0, date, monthno, year, 0, 0) * (Wback?(-1):1)
	>= (Wback?1:10080)) {
    if (wp -> next == NULL)
      return;
    else
      wp = wp -> next;   /* run to right week (0 <= minsbetween < 10080) */
  }

  wp -> reqs += reqs;            /* and add to its requests */
  wp -> pages += pages;
  wp -> bytes += bytes;

}

/* and a function to co-ordinate all the date cataloguing */
void datehash(int year, int monthno, int date, int hr, int min,
	      long thistimecode, int reqs, int pages, double bytes)
{
  extern flag mq, dq, Dq, Wq, Hq;
  extern flag mback, Dback, Wback, Hback;
  extern int Wrows, Wrowsdone;
  extern struct timestruct firsttime, lasttime;
  extern struct monthly *firstm, *lastm;
  extern struct daily *firstD, *lastD;
  extern struct weekly *firstW, *lastW;
  extern struct hourly *firstH, *lastH;
  extern int monthlength[];
  extern int dreq[], hreq[], dpag[], hpag[];
  extern double dbytes[], hbytes[];

  int day;
  struct monthly *tempmp;
  struct daily *tempdp;
  struct weekly *tempwp, *tempwp2, *tempwp3;
  struct hourly *temphp;
  int i, j;

  if (mq) {

    if (year <= firsttime.year) {  /* then we might need a new lot of months */
      for (i = firsttime.year - year; i > 0; i--) {
	tempmp = firstm;
	firstm = (struct monthly *)xmalloc(sizeof(struct monthly));
	if (mback) {
	  tempmp -> next = firstm;
	  firstm -> next = NULL;
	}
	else
	  firstm -> next = tempmp;
	for (j = 0; j < 12; j++) {
	  firstm -> reqs[j] = 0;
	  firstm -> pages[j] = 0;
	  firstm -> bytes[j] = 0.0;
	}
      }
      firstm -> reqs[monthno] += reqs;    /* add to this month */
      firstm -> pages[monthno] += pages;  /* (whether or not newly created) */
      firstm -> bytes[monthno] += bytes;
    }

    else if (year >= lasttime.year) {     /* similarly */
      for (i = year - lasttime.year; i > 0; i--) {
	tempmp = lastm;
	lastm = (struct monthly *)xmalloc(sizeof(struct monthly));
	if (mback)
	  lastm -> next = tempmp;
	else {
	  tempmp -> next = lastm;
	  lastm -> next = NULL;
	}
	for (j = 0; j < 12; j++) {
	  lastm -> reqs[j] = 0;
	  lastm -> pages[j] = 0;
	  lastm -> bytes[j] = 0.0;
	}
      }
      lastm -> reqs[monthno] += reqs;
      lastm -> pages[monthno] += pages;
      lastm -> bytes[monthno] += bytes;
    }

    else   /* NB we will never get here if logfile in chron. order */
      addmonthlydata(year, monthno, reqs, pages, bytes);

  }   /* end if (mq) */

  if (Dq) {

    if (year * 12 + monthno <= firsttime.year * 12 + firsttime.monthno) {
      /* then we might need a new lot of days */
      for (i = (firsttime.year - year) * 12 +
	   firsttime.monthno - monthno; i > 0; i--) {
	tempdp = firstD;
	firstD = (struct daily *)xmalloc(sizeof(struct daily));
	if (Dback) {
	  tempdp -> next = firstD;
	  firstD -> next = NULL;
	}
	else
	  firstD -> next = tempdp;
	for (j = 0; j < 31; j++) {
	  firstD -> reqs[j] = 0;
	  firstD -> pages[j] = 0;
	  firstD -> bytes[j] = 0.0;
	}
      }
      firstD -> reqs[date - 1] += reqs;
      firstD -> pages[date - 1] += pages;
      firstD -> bytes[date - 1] += bytes;
    }
    
    else
      if (year * 12 + monthno >= lasttime.year * 12 + lasttime.monthno) {
	for (i = (year - lasttime.year) * 12 - lasttime.monthno + monthno;
	     i > 0; i--) {
	  tempdp = lastD;
	  lastD = (struct daily *)xmalloc(sizeof(struct daily));
	  if (Dback)
	    lastD -> next = tempdp;
	  else {
	    tempdp -> next = lastD;
	    lastD -> next = NULL;
	  }
	  for (j = 0; j < 31; j++) {
	    lastD -> reqs[j] = 0;
	    lastD -> pages[j] = 0;
	    lastD -> bytes[j] = 0.0;
	  }
	}
	lastD -> reqs[date - 1] += reqs;
	lastD -> pages[date - 1] += pages;
	lastD -> bytes[date - 1] += bytes;
      }

      else
	adddailydata(year, monthno, date, reqs, pages, bytes);

  }   /* end if (mq) */

  if (Hq) {

    if (year * /* 12 * 31 */ 372 + monthno * 31 + date <=
	firsttime.year * 372 + firsttime.monthno * 31 + firsttime.date) {
      /* then we might need a new lot of hours */
      for (i = minsbetween(date, monthno, year, 0, 0, firsttime.date,
			   firsttime.monthno, firsttime.year, 0, 0) / 1440;
	   i > 0; i--) {
	temphp = firstH;
	firstH = (struct hourly *)xmalloc(sizeof(struct hourly));
	if (Hback) {
	  temphp -> next = firstH;
	  firstH -> next = NULL;
	}
	else
	  firstH -> next = temphp;
	for (j = 0; j < 24; j++) {
	  firstH -> reqs[j] = 0;
	  firstH -> pages[j] = 0;
	  firstH -> bytes[j] = 0.0;
	}
      }
      firstH -> reqs[hr] += reqs;
      firstH -> pages[hr] += pages;
      firstH -> bytes[hr] += bytes;
    }
    
    else
      if (year * 372 + monthno * 31 + date
	  >= lasttime.year * 372 + lasttime.monthno * 31 + lasttime.date) {
	for (i = minsbetween(lasttime.date, lasttime.monthno, lasttime.year,
			     0, 0, date, monthno, year, 0, 0) / 1440;
	     i > 0; i--) {
	  temphp = lastH;
	  lastH = (struct hourly *)xmalloc(sizeof(struct hourly));
	  if (Hback)
	    lastH -> next = temphp;
	  else {
	    temphp -> next = lastH;
	    lastH -> next = NULL;
	  }
	  for (j = 0; j < 24; j++) {
	    lastH -> reqs[j] = 0;
	    lastH -> pages[j] = 0;
	    lastH -> bytes[j] = 0.0;
	  }
	}
	lastH -> reqs[hr] += reqs;
	lastH -> pages[hr] += pages;
	lastH -> bytes[hr] += bytes;
      }

      else
	addhourlydata(year, monthno, date, hr, reqs, pages, bytes);

  }   /* end if (mq) */

  if (Wq) {
    if (thistimecode < firstW -> start.code) {
      if (Wrows <= 0 || Wrowsdone < Wrows) { /* new week needed; else bin it */
	while (thistimecode < firstW -> start.code &&
	       (Wrows <= 0 || Wrowsdone < Wrows)) {
	  Wrowsdone++;
	  tempwp = firstW;
	  firstW = (struct weekly *)xmalloc(sizeof(struct weekly));
	  if (Wback) {
	    tempwp -> next = firstW;
	    firstW -> next = NULL;
	  }
	  else
	    firstW -> next = tempwp;
	  firstW -> reqs = 0;
	  firstW -> pages = 0;
	  firstW -> bytes = 0.0;
	  firstW -> start = tempwp -> start;
	  firstW -> start.date -= 7;
	  if (firstW -> start.date <= 0) {
	    firstW -> start.monthno--;
	    if (firstW -> start.monthno == -1) {
	      firstW -> start.monthno = 11;
	      firstW -> start.year--;
	    }
	    firstW -> start.date = monthlength[firstW -> start.monthno] +
	      firstW -> start.date +
		ISLEAPFEB(firstW -> start.monthno, firstW -> start.year);
	  }
	  firstW -> start.code = timecode(firstW -> start.date,
					  firstW -> start.monthno,
					  firstW -> start.year, 0, 0);
	}
	if (thistimecode >= firstW -> start.code) {
	  firstW -> reqs += reqs;
	  firstW -> pages += pages;
	  firstW -> bytes += bytes;
	}
      }
    }

    else if (thistimecode >= lastW -> start.code) {
      while (minsbetween(lastW -> start.date, lastW -> start.monthno,
			 lastW -> start.year, 0, 0,  /* 10080m = 1w */
			 date, monthno, year, 0, 0) >= 10080) {
	tempwp = lastW;
	if (Wrows > 0 && (++Wrowsdone) > Wrows) {   /* then recycle firstW */
	  Wrowsdone--;
	  lastW = firstW;
	  if (Wback) {
	    /* want firstW = firstW -> prev; but prev isn't recorded. So... */
	    if (Wrows > 1) {   /* else firstW = lastW = onlyW */
	      for (tempwp2 = tempwp; tempwp2 -> next != NULL;
		   tempwp2 = tempwp2 -> next)
		tempwp3 = tempwp2;
	      firstW = tempwp3;
	    }
	    firstW -> next = NULL;
	  }
	  else if (Wrows > 1)   /* as above */
	    firstW = firstW -> next;
	}
	else
	  lastW = (struct weekly *)xmalloc(sizeof(struct weekly));
	if (Wback)
	  lastW -> next = tempwp;
	else {
	  tempwp -> next = lastW;
	  lastW -> next = NULL;
	}
	lastW -> reqs = 0;
	lastW -> pages = 0;
	lastW -> bytes = 0.0;
	lastW -> start = tempwp -> start;
	lastW -> start.date += 7;
	if (lastW -> start.date > monthlength[lastW -> start.monthno] +
	    ISLEAPFEB(lastW -> start.monthno, lastW -> start.year)) {
	  lastW -> start.date -= monthlength[lastW -> start.monthno] +
	    ISLEAPFEB(lastW -> start.monthno, lastW -> start.year);
	  lastW -> start.monthno++;
	  if (lastW -> start.monthno == 12) {
	    lastW -> start.monthno = 0;
	    lastW -> start.year++;
	  }
	}
	lastW -> start.code = timecode(lastW -> start.date,
				       lastW -> start.monthno,
				       lastW -> start.year, 0, 0);
      }
      lastW -> reqs += reqs;
      lastW -> pages += pages;
      lastW -> bytes += bytes;
    }
    
    else  /* again, only used if logfile not chronological */
      addweeklydata(year, monthno, date, reqs, pages, bytes);

  }   /* end if (Wq) */

  if (dq) {
    day = dayofdate(date, monthno, year);
    dreq[day] += reqs;
    dpag[day] += pages;
    dbytes[day] += bytes;
  }
  hreq[hr] += reqs;  /* no need to bother checking hq */
  hpag[hr] += pages;
  hbytes[hr] += bytes;
                            

  if (thistimecode < firsttime.code) {
    firsttime.date = date;
    firsttime.monthno = monthno;
    firsttime.year = year;
    firsttime.hr = hr;
    firsttime.min = min;
    firsttime.code = thistimecode;
  }

  if (thistimecode > lasttime.code) {
    lasttime.date = date;
    lasttime.monthno = monthno;
    lasttime.year = year;
    lasttime.hr = hr;
    lasttime.min = min;
    lasttime.code = thistimecode;
  }
}

/*** Now some routines to sort the various reports ready for printing ***/
/* First a simple bubblesort for the errors */
void errsort(int errorder[NO_ERRS])
{
  extern int errors[NO_ERRS];

  int i, j, tempint;

  for (i = 0; i < NO_ERRS; i++)
    errorder[i] = i;

  for (i = NO_ERRS - 2; i >= 0; i--) {
    for (j = 0; j <= i; j++) {
      if (errors[errorder[j]] < errors[errorder[j + 1]]) {
	tempint = errorder[j];
	errorder[j] = errorder[j + 1];
	errorder[j + 1] = tempint;
      }
    }
  }
}

double bytefloor(double bytes, char *str)
{
  double ans;
  int last;

  last = MAX((int)strlen(str) - 1, 0);

  if (str[last] == '%') {
    str[last] = '\0';
    ans = bytes * atof(str) / 100.0;
    str[last] = '%';
  }

  else if (str[last] == 'k' || str[last] == 'K') {
    str[last] = '\0';
    ans = atof(str) * 1024.0;
    str[last] = 'k';
  }
  else if (str[last] == 'M' || str[last] == 'm') {
    str[last] = '\0';
    ans = atof(str) * 1048576.0;
    str[last] = 'M';
  }
  else if (str[last] == 'G' || str[last] == 'g') {
    str[last] = '\0';
    ans = atof(str) * 1073741824.0;
    str[last] = 'G';
  }
  else if (str[last] == 'T' || str[last] == 't') {
    str[last] = '\0';
    ans = atof(str) * 1099511627776.0;
    str[last] = 'T';
  }

  else
    ans = atof(str);

  return(ans);
}

int reqfloor(int reqs, char *str)
{
  int ans;
  int last;

  last = MAX((int)strlen(str) - 1, 0);

  if (str[last] == '%') {
    str[last] = '\0';
    ans = (int)ceil((double)reqs * atof(str) / 100.0);
    str[last] = '%';
  }

  else
    ans = atoi(str);

  return(ans);
}

/* a function to sort the generic reports */
struct genstruct *gensort(struct genstruct **objhead, int hashsize,
			  int tot_reqs, int tot_pages, double tot_bytes,
			  int sortby, char *minreqstr, char *minpagestr,
			  char *minbytestr, struct include *includehead,
			  /* What to list. Currently only requests use this,
			     but could extend */
			  flag alphahost,  /* an alphabetical hostsort? */
			  int *maxreqs, int *maxpages, double *maxbytes,
			  int *maxlength)
{
  flag wantit = ON;    /* whether we want a particular item */
  /* NB This is what to list. *INCLUDE and *EXCLUDE are done in allaliases() */
  struct genstruct *sorthead;   /* build up the sort in this list
				(and return it at the end) */
  struct genstruct *p, *p2, *nextp, *lastp;
  int onlist;          /* the list we are processing */
  flag finished;       /* whether we've finished with a particular entry */
  double floorb = 0.0; /* floor for bytes (for byte sorting) */
  int floorr = 0;      /* floor for pages/requests (for other sorting) */
  int outnumber;       /* the number of items to be displayed */

  int i;

  /* first calculate the floor */

  if (sortby == BYBYTES)
    floorb = bytefloor(tot_bytes, minbytestr);
  else if (sortby == BYPAGES)
    floorr = reqfloor(tot_pages, minpagestr);
  else
    floorr = reqfloor(tot_reqs, minreqstr);

  outnumber = 0;
  sorthead = (struct genstruct *)xmalloc(sizeof(struct genstruct));
  sorthead -> name = NULL;           /* as marker */
  onlist = 0;                        /* the list we are on */
  p = objhead[0];                 /* starting at list 0 */
  for ( ; onlist < hashsize; p = nextp)  {
                                     /* run through all the objects */
    if (p -> name == NULL)    {   /* then finished this list */
      nextp = objhead[++onlist];  /* so look at the next list */
    }
    else {
      if (includehead != NULL)  /* o/wise wantit always stays ON */
	wantit = included(p -> name, p -> ispage, includehead);
      if ((sortby == BYBYTES && p -> bytes < floorb) ||
	  (sortby == BYPAGES && p -> pages < floorr) ||
	  (sortby != BYBYTES && sortby != BYPAGES && p -> reqs < floorr) ||
	  (p -> reqs == 0) || (!wantit)) {  /* we don't want it */
	nextp = p -> next;
      }
      else {
	outnumber++;
	*maxreqs = MAX(p -> reqs, *maxreqs);
	*maxpages = MAX(p -> pages, *maxpages);
	*maxbytes = MAX(p -> bytes, *maxbytes);
	if (alphahost) {  /* some special things for alphabetical host sort */
	  *maxlength = MAX((int)strlen(p -> name), *maxlength);
	  if (!isdigit(p -> name[(int)strlen(p -> name) - 1]))
	    reversehostname(p -> name);
	}
	if ((sorthead -> name == NULL) ||
	    (sortby == RANDOMLY) ||
	    (sortby == BYBYTES && p -> bytes > sorthead -> bytes) ||
	    (sortby == BYPAGES && p -> pages > sorthead -> pages) ||
	    (sortby == BYREQUESTS && p -> reqs > sorthead -> reqs) ||
	    (sortby == ALPHABETICAL && !alphahost &&
	     strcmp(p -> name, sorthead -> name) < 0) ||
	    (sortby == ALPHABETICAL && alphahost &&
	     hoststrcmp(p -> name, sorthead -> name) < 0)) {
	  /* if it's before the first item currently on the list, slot it in */
	  nextp = p -> next;   /* the next one we're going to look at */
	  p -> next = sorthead;
	  sorthead = p;
	}
	else {                   /* otherwise compare with the ones so far */
	  finished = OFF;
	  lastp = sorthead;
	  if (floorb < 0.0)
	    i = (int)ROUND(floorb);
	  else if (floorr < 0)
	    i = floorr;
	  else
	    i = 1;
	  for (p2 = sorthead -> next;
	       p2 -> name != NULL && (!finished) && (i++) != -1;
	       p2 = p2 -> next) {
	    if ((sortby == BYBYTES && p -> bytes > p2 -> bytes) ||
		(sortby == BYPAGES && p -> pages > p2 -> pages) ||
		(sortby == BYREQUESTS && p -> reqs > p2 -> reqs) ||
		(sortby == ALPHABETICAL && !alphahost &&
		 strcmp(p -> name, p2 -> name) < 0) ||
		(sortby == ALPHABETICAL && alphahost &&
		 hoststrcmp(p -> name, p2 -> name) < 0)) {
	  /* if p comes before p2 in the chosen ordering, slot it in */
	      nextp = p -> next;
	      p -> next = p2;
	      lastp -> next = p;
	      finished = ON;
	    }
	    lastp = p2;
	  }
	  if (!finished) {         /* we've reached the end of the list; */
	                           /* slot it in at the end */
	    nextp = p -> next;
	    p -> next = p2;
	    p2 -> name = NULL;
	    lastp -> next = p;
	  }
	}
      }
    }
    
    p = nextp;   /* so, on to the next one */

  }        /* end for running through all objects */

  if ((floorb < 0.0 && outnumber <= -(int)ROUND(floorb)) ||
      (floorr < 0 && outnumber <= -floorr)) {
                          /* -ve floor & there are at most that many objects */
    strcpy(minreqstr, "0");   /* signal to output() that we are printing all */
    strcpy(minpagestr, "0");
    strcpy(minbytestr, "0");
  }

  return(sorthead);

}    /* end gensort */

/*** Again, the domain report is a bit different because the
   structure is stored differently ***/

int domsort(void)
{
  extern int onumber;
  extern struct domain **ohead;
  extern int osortby;
  extern char *ominreqstr;
  extern char *ominpagestr;
  extern char *ominbytestr;
  extern double total_bytes;
  extern int total_succ_reqs, total_page_reqs;
  extern int omaxreqs, omaxpages;
  extern double omaxbytes;

  int i, j, k;
  int firstdom;  /* the numerical index of the first domain; we return this */
  int onextj;
  struct domain *p, *lastp;
  int floorr = 0;
  double floorb = 0.0;
  flag finished;

  if (osortby == BYBYTES)
    floorb = bytefloor(total_bytes, ominbytestr);
  else if (osortby == BYPAGES)
    floorr = reqfloor(total_page_reqs, ominpagestr);
  else
    floorr = reqfloor(total_succ_reqs, ominreqstr);

  onumber = 0;
  firstdom = DOMHASHSIZE - 2; /* start with unknown domains at front of list */
  if ((osortby == BYBYTES && (ohead[firstdom] -> reqs == 0 ||
			      ohead[firstdom] -> bytes < floorb)) ||
      (osortby == BYPAGES && ohead[firstdom] -> pages < floorr) ||
      (osortby != BYBYTES && osortby != BYPAGES &&
       ohead[firstdom] -> reqs < floorr) ||
      ((floorr < 0 || floorb < 0) && ohead[firstdom] -> reqs == 0))
    /* then we don't want it; set marker */
    ohead[firstdom] -> reqs = -1;
  omaxreqs = ohead[firstdom] -> reqs;
  omaxpages = ohead[firstdom] -> pages;
  omaxbytes = ohead[firstdom] -> bytes;
  ohead[firstdom] -> nexti = -1;
  j = DOMHASHSIZE - 1; /* the domain we are on; start with numerical domains */
  while (j >= 0) {              /* run through all the domains */
    p = ohead[j];
    onextj = p -> nexti;
                            /* the one we're going to look at after this one */
    if (!((osortby == BYBYTES && (p -> reqs == 0 || p -> bytes < floorb)) ||
	  (osortby == BYPAGES && p -> pages < floorr) ||
	  (osortby != BYBYTES && osortby != BYPAGES && p -> reqs < floorr) ||
	  ((floorr < 0 || floorb < 0) && p -> reqs == 0))) {
                                   /* else we don't want it */
      onumber++;
      omaxreqs = MAX(p -> reqs, omaxreqs);
      omaxpages = MAX(p -> pages, omaxpages);
      omaxbytes = MAX(p -> bytes, omaxbytes);
      if ((osortby == RANDOMLY) ||
	  (osortby == BYBYTES && p -> bytes > ohead[firstdom] -> bytes) ||
	  (osortby == BYPAGES && p -> pages > ohead[firstdom] -> pages) ||
	  (osortby == BYREQUESTS && p -> reqs > ohead[firstdom] -> reqs) ||
	  (osortby == ALPHABETICAL &&
	   strcmp(p -> id, ohead[firstdom] -> id) < 0)) {
	/* if it's before the first item currently on the list, slot it in */
	p -> nexti = firstdom;
	firstdom = j;
      }
      else {        /* otherwise compare with the ones so far */
	finished = OFF;
	lastp = ohead[firstdom];
	if (floorb < 0.0)
	  k = (int)ROUND(floorb);
	else if (floorr < 0)
	  k = floorr;
	else
	  k = 1;
	for (i = ohead[firstdom] -> nexti;
	     i >= 0 && (!finished) && (k++) != -1;
	     i = ohead[i] -> nexti) {
	  if ((osortby == BYBYTES && p -> bytes > ohead[i] -> bytes) ||
	      (osortby == BYPAGES && p -> pages > ohead[i] -> pages) ||
	      (osortby == BYREQUESTS && p -> reqs > ohead[i] -> reqs) ||
	      (osortby == ALPHABETICAL &&
	       strcmp(p -> id, ohead[i] -> id) < 0)) {
	    /* if domp comes before domp2 in the chosen ordering, slot it in */
	    p -> nexti = i;
	    lastp -> nexti = j;
	    finished = ON;
	  }
	  lastp = ohead[i];
	}
	if (!finished) {
	  p -> nexti = -1;     /* meaning, last item on the list */
	  lastp -> nexti = j;
	}
      }
    }
    
    j = onextj;   /* so, on to the next one */
	
  }        /* end while j >= 0 */

  if ((floorb < 0.0 && onumber <= -(int)ROUND(floorb)) ||
      (floorr < 0 && onumber <= -floorr)) {
    strcpy(ominreqstr, "0");
    strcpy(ominpagestr, "0");
    strcpy(ominbytestr, "0");
  }

  return(firstdom);

}   /* end domsort */

/*** Finally, sort subdomains into the domain report ***/

void subdomsort(void)
{
  extern int Onumber;
  extern struct domain **ohead, **Ohead;
  extern int osortby;
  extern char *Ominbytestr, *Ominpagestr, *Ominreqstr;
  extern double total_bytes;
  extern int total_succ_reqs, total_page_reqs;
  extern size_t Ohashsize;

  struct domain *Op, *Onextp, *p, *nextp;
  int floorr;
  double floorb;
  int onlist;
  int domcode;

  if (osortby == BYBYTES)
    floorb = bytefloor(total_bytes, Ominbytestr);
  else if (osortby == BYPAGES)
    floorr = reqfloor(total_page_reqs, Ominpagestr);
  else
    floorr = reqfloor(total_succ_reqs, Ominreqstr);

  onlist = 0;
  Onumber = 0;
  Op = Ohead[0];              /* starting at list 0 */
  for ( ; onlist < Ohashsize; Op = Onextp)  {  /* run thro' all subdoms */
    if (Op -> name == NULL) {           /* then finished this list */
      Onextp = Ohead[++onlist];         /* so look at the next list */
    }
    else if ((osortby == BYBYTES && Op -> bytes >= floorb) ||
	     (osortby == BYPAGES && Op -> pages >= floorr) ||
	     (osortby != BYBYTES && osortby != BYPAGES &&
	      Op -> reqs >= floorr)) {
      Onextp = Op -> next;
      Onumber++;
      domcode = hosttodomcode(Op -> id);
      if (domcode != DOMHASHSIZE - 2) {
	p = ohead[domcode];   /* now run through that domain's subdomains */
	while (p -> next -> name != NULL &&
	       hoststrcmp(p -> next -> revid, Op -> revid) < 0)
	  p = p -> next;  /* run to right place in alphabet */
	nextp = p -> next;  /* then slot it in */
	p -> next = Op;
	Op -> next = nextp;
      }
    }
    else
      Onextp = Op -> next;
  }
}
