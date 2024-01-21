/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** analog.c; the main function ***/

#include "analhea2.h"

int main(int argc, char **argv)
{
  extern char *commandname;    /* all global vars declared in init.c */
  extern struct loglist *logfilehead;
  extern struct stringlist *cachefilehead, *refloghead, *browloghead;
  extern struct stringlist *errloghead;
#ifndef NODNS
  extern char *dnsfile;
  extern flag dnsq;
  extern struct dnscache **dnshead;
  extern size_t dnshashsize;
  struct dnscache *dnsp, *dnsnextp;
#endif
  extern flag byq, browbyq, refbyq;
  extern flag filemaskq, hostmaskq, q7, warnq, anywarns;
  extern flag mq, hq, Hq, dq, Dq, Wq, sq, Sq, oq, iq, tq, rq, fq, Bq, bq, cq;
  extern flag eq;
  extern int osortby, rsortby, isortby, tsortby, Ssortby, fsortby;
  extern int bsortby, Bsortby;
  extern char bcols[], Bcols[], fcols[];
  extern char mgraph, dgraph, Dgraph, hgraph, Hgraph, Wgraph;
  extern char *rminreqstr, *iminreqstr, *tminreqstr, *Sminreqstr;
  extern char *fminreqstr, *bminreqstr, *Bminreqstr;
  extern char *rminpagestr, *iminpagestr, *tminpagestr, *Sminpagestr;
  extern char *fminpagestr, *bminpagestr, *Bminpagestr;
  extern char *rminbytestr, *iminbytestr, *tminbytestr, *Sminbytestr;
  extern char *fminbytestr, *bminbytestr, *Bminbytestr;
  extern int rmaxreqs, imaxreqs, tmaxreqs, Smaxreqs, fmaxreqs;
  extern int bmaxreqs, Bmaxreqs, eminreqs;
  extern int rmaxpages, imaxpages, tmaxpages, Smaxpages, fmaxpages;
  extern int bmaxpages, Bmaxpages;
  extern double rmaxbytes, imaxbytes, tmaxbytes, Smaxbytes, fmaxbytes;
  extern double bmaxbytes, Bmaxbytes;
  extern size_t rhashsize, ihashsize, thashsize, Shashsize, fhashsize;
  extern size_t bhashsize, Bhashsize;
  extern int Smaxlength;
  extern struct timestruct firsttime, lasttime, fromtime, totime, oldtime;
  extern int no_hosts, no_hosts7, no_new_hosts7;
  extern int no_urls, no_urls7;
  extern int cachereqs, cachereqs7, cachepages, cachepages7;
  extern int corrupt_lines, other_lines;
  extern double total_bytes, total_bytes7, total_ref_bytes, total_brow_bytes;
  extern int total_succ_reqs, total_fail_reqs, total_other_reqs;
  extern int total_succ_reqs7, total_fail_reqs7, total_other_reqs7;
  extern int total_page_reqs, total_page_reqs7;
  extern int total_good_refs, total_bad_refs, total_masked_refs;
  extern int total_good_brows, total_bad_brows, total_masked_brows;
  extern int total_ref_pages, total_brow_pages;
  extern struct weekly *firstW;
  extern struct genstruct **Shead, **rhead, **ihead, **thead, **fhead;
  extern struct genstruct **bhead, **Bhead, **Shead2, **rhead2, **fhead2;
  extern int errors[];
  extern int onumber;
  extern struct include *wanthosthead, *wantreqhead;
  extern int debug, progressfreq;
  extern int status[], status7[], statusnos[];

  FILE *lf;
  flag ispipe;           /* whether the currently open logfile is a pipe */
  struct loglist *logfilep;
  struct stringlist *otherlogp;
  char inputline[MAXLINELENGTH];  /* a particular input line */
  int linetype;          /* COMMON, NCSAOLD, WEBSTAR or CORRUPT */
  char hostn[MAXSTRINGLENGTH];
  int date, year, hr, min, monthno;
  long thistimecode;
  char fromurl[MAXSTRINGLENGTH];
  char browser[MAXSTRINGLENGTH];
  char errstr[MAXLINELENGTH];
  char filename[MAXSTRINGLENGTH];
  int code;
  size_t preflength;     /* length of filename prefix for this logfile */
  flag firstreq = TRUE;
  flag datemaskq;
  flag fwarn1 = OFF, fwarn2 = OFF, fwarn3 = OFF;
  flag bwarn1 = OFF, bwarn2 = OFF, bwarn3 = OFF, bwarn4 = OFF;
  /* have certain warnings been given? */
  double bytes;    /* long is not big enough; double has more sig. figs,
		      and copes with overflow automatically. */
  char bytestr[16];

  struct genstruct *rsorthead, *isorthead, *tsorthead, *Ssorthead;
  struct genstruct *fsorthead, *bsorthead, *Bsorthead;
  int firstdom, errorder[NO_ERRS];     /* heads for sorting */

  struct genstruct *hostp, *hostnextp, *urlp, *urlnextp;
  int onlist;

  flag wantthisone = TRUE;  /* whether we want to analyse a particular entry */
  flag issuccess;           /* whether an entry has a success status code */
  flag ispage = OFF;        /* whether it represents a page */
  flag last7q = OFF;        /* are we now in the last 7 days? */
  int rc;
  register int linesread;
  int nextreport;
  char *tempstr, *tempc;
  int i, tempint, tempint2;
  flag tempf;
  char tempchar, tempchar2;
  struct genstruct *tempgs;

  /*** Initialisation ***/

#ifdef MAC_EVENTS
  MacInit();
#endif
  initialise(argc, argv);
  linesread = 0;
  nextreport = progressfreq;
  datemaskq = (fromtime.code > -INFINITY || totime.code < INFINITY);

  /*** Now start scanning ***/

  for (logfilep = logfilehead; logfilep -> name[0] != '\0';
       logfilep = logfilep -> next) {  /* for each logfile */

    lf = fopenlog(logfilep -> name, "logfile", &ispipe);
    if (lf != NULL) {

      preflength = strlen(logfilep -> prefix);

      while(fgets(inputline, MAXLINELENGTH, lf) != NULL) {

	strcpy(filename, logfilep -> prefix);
	/* needed each line coz can get accidentally overwritten by /../ */

	linetype = CORRUPT;   /* paranoia :) */

	if ((rc = sscanf_common(inputline, hostn, &date, &monthno, &year, &hr,
				&min, filename + preflength, fromurl, browser,
				&code, bytestr, preflength)) >= 9) {
	  linetype = COMMON;
	  bytes = atof(bytestr);
	}
#ifdef WEBSTAR
	else if ((rc = sscanf_webstar(inputline, hostn, &date, &monthno, &year,
				      &hr, &min, filename + preflength,
				      fromurl, browser, &code, bytestr,
				      preflength)) >= 9) {
	  linetype = WEBSTARLINE;
	  bytes = atof(bytestr);
	}
#endif
#ifdef NETPRESENZ
	else if ((rc = sscanf_netpresenz(lf, inputline, hostn, &date, &monthno,
					 &year, &hr, &min,
					 filename + preflength, fromurl,
					 browser, &code, bytestr,
					 preflength)) >= 9) {
	  linetype = NETPRESENZLINE;
	  bytes = 0;
	  if (byq) {
	    fprintf(stderr, "%s: Warning: Netpresenz logs contain no bytes information:\n", commandname);
	    fprintf(stderr, "  will not report on bytes transferred.\n");
	    byq = OFF;
	    if (osortby == BYBYTES)
	      osortby = BYREQUESTS;
	    if (rsortby == BYBYTES)
	      rsortby = BYREQUESTS;
	    if (isortby == BYBYTES)
	      isortby = BYREQUESTS;
	    if (tsortby == BYBYTES)
	      tsortby = BYREQUESTS;
	    if (Ssortby == BYBYTES)
	      Ssortby = BYREQUESTS;
	    if (mgraph == 'b' || mgraph == 'B')
	      mgraph = 'r';
	    if (dgraph == 'b' || dgraph == 'B')
	      dgraph = 'r';
	    if (Dgraph == 'b' || Dgraph == 'B')
	      Dgraph = 'r';
	    if (hgraph == 'b' || hgraph == 'B')
	      hgraph = 'r';
	    if (Hgraph == 'b' || Hgraph == 'B')
	      Hgraph = 'r';
	    if (Wgraph == 'b' || Wgraph == 'B')
	      Wgraph = 'r';
	  }
	}
#endif
	else if ((rc = sscanf_ncsaold(inputline, hostn, &monthno, &date, &hr,
				      &min, &year, filename + preflength,
				      preflength)) == 7) {
	  linetype = NCSAOLD;
	  code = 200;
	  bytes = 0;
	  if (byq) {
	    fprintf(stderr, "%s: Warning: old style NCSA logs contain no bytes information:\n", commandname);
	    fprintf(stderr, "  will not report on bytes transferred.");
	    byq = OFF;
	    if (osortby == BYBYTES)
	      osortby = BYREQUESTS;
	    if (rsortby == BYBYTES)
	      rsortby = BYREQUESTS;
	    if (isortby == BYBYTES)
	      isortby = BYREQUESTS;
	    if (tsortby == BYBYTES)
	      tsortby = BYREQUESTS;
	    if (Ssortby == BYBYTES)
	      Ssortby = BYREQUESTS;
	    if (mgraph == 'b' || mgraph == 'B')
	      mgraph = 'r';
	    if (dgraph == 'b' || dgraph == 'B')
	      dgraph = 'r';
	    if (Dgraph == 'b' || Dgraph == 'B')
	      Dgraph = 'r';
	    if (hgraph == 'b' || hgraph == 'B')
	      hgraph = 'r';
	    if (Hgraph == 'b' || Hgraph == 'B')
	      Hgraph = 'r';
	    if (Wgraph == 'b' || Wgraph == 'B')
	      Wgraph = 'r';
	  }
	}
      
	if (linetype != CORRUPT) {
	
	  thistimecode = timecode(date, monthno, year, hr, min);
	  wantthisone = thistimecode >= fromtime.code &&
	    thistimecode <= totime.code;
	  if (wantthisone && hostmaskq) {  /* must hostmask before req. hash */
	    doaliashost(hostn);
	    wantthisone = included(hostn, UNSET, wanthosthead);
	  }
	  if (wantthisone) {
	    issuccess = (code <= 299 || code == 304);
	    /* Are we in the last 7 days? Check this every time in case */
	    /* logfile is not in chronological order */
	    if (q7)   /* if !q7, last7q stays off (for efficiency) */
	      last7q = (thistimecode > oldtime.code);
	
	    /* Request report. We always construct a (poss. silent) request
	       report in order to generate ispage etc. */
	    tempgs = hashadd(rhead2, rhashsize, filename, issuccess,
			     issuccess?bytes:0, UNSET,
			     last7q, &tempint, &tempint, &tempint,
			     OFF, filemaskq, UNSET,
			     (struct genstruct *)NULL, -1, 'r');
	    wantthisone = tempgs -> wanted;
	    ispage = tempgs -> ispage;
	  }
	  if (wantthisone) {
	    /* Hostname report/count. This time, we don't do one if the
	       domain report or hostmaskq is on and this is off, because
	       this takes up a lot of memory. */
	    if (sq == ON) {
	      if (hostmaskq)  /* then aliasing already done */
		hashadd(Shead2, Shashsize, hostn, issuccess, issuccess?bytes:0,
			issuccess && ispage, last7q, &tempint, &tempint,
			&tempint, ON, OFF, OFF, (struct genstruct *)NULL,
			-1, 'S');  /* and we know it's wanted */
	      else
		wantthisone = hashadd(Shead2, Shashsize, hostn, issuccess,
				      issuccess?bytes:0, issuccess && ispage,
				      last7q, &tempint, &tempint, &tempint,
				      OFF, OFF, OFF, (struct genstruct *)NULL,
				      -1, 'S') -> wanted;
	    }
	    else if (issuccess) {
	      if (!hostmaskq && (oq || sq == APPROX)) {
		doaliashost(hostn);
		wantthisone = included(hostn, UNSET, wanthosthead);
	      }
	      if /* still */ (wantthisone) {
		if (oq)
		  domhashadd(hostn, 1, ispage, bytes);
		if (sq == APPROX)
		  approxhosthashadd(hostn, last7q);
	      }
	    }
	  }

	  if (!wantthisone)
	    ++other_lines;
	  else {
	    /* add to the right status code total */
	    tempf = OFF;
	    for (i = 0; !tempf && i < NO_STATUS; i++) {
	      if (code <= statusnos[i]) {
		status[i]++;
		if (last7q)
		  status7[i]++;
		tempf = ON;
	      }
	    }

	    if (issuccess) {
	      if (ispage) {
		total_page_reqs++;
		total_page_reqs7 += last7q;
	      }
	      total_bytes += bytes;  /* NB only count bytes for successes */
	      if (last7q)
		total_bytes7 += bytes;

	      if (firstreq) {
		firstreq = FALSE;
		firsttime.date = date;
		firsttime.monthno = monthno;
		firsttime.year = year;
		firsttime.hr = hr;
		firsttime.min = min;
		firsttime.code = thistimecode;
		if (Wq)
		  firstW -> start = startofweek(firsttime);
		lasttime.date = date;
		lasttime.monthno = monthno;
		lasttime.year = year;
		lasttime.hr = hr;
		lasttime.min = min;
		lasttime.code = thistimecode;
	      }
	  
	      /* date cataloguing */

	      datehash(year, monthno, date, hr, min, thistimecode, 1, ispage,
		       bytes);

	    }    /* end if issuccess */
	
	    if (rc == 11) {  /* then do referrer and browser now (all codes) */
	      if (fq && fromurl[0] != '\0')
		addref(fromurl, filename, ispage, bytes, last7q, OFF);
	      if ((Bq || bq) && browser[0] != '\0')
		addbrowser(browser, ispage, bytes, last7q);
	    }

	  }  /* end if want this one */
	  
	}   /* end if linetype != CORRUPT */

	else {   /* line is corrupt */
	  ++corrupt_lines;
	  if (debug != 0)
	    fprintf(stderr, "C: %s", inputline);
	  if (strchr(inputline, '\n') == NULL) {
	    /* line corrupt by being too long; */
	    fscanf(lf, "%*[^\n]");              /* read to end of line */
	    if (debug != 0)
	      fprintf(stderr, "\n");
	  }
	}
  
	if ((++linesread) == nextreport) {
	  fprintf(stderr, "%s: %d lines read\n", commandname, linesread);
	  nextreport += progressfreq;
	}

#ifdef MAC_EVENTS
	if ((linesread % MAC_IDLE_FREQ) == 0)
	  MacIdle();
#endif

      }   /* end of reading this logfile */

      fcloselog(lf, logfilep -> name, "logfile", ispipe);

    }
  }    /*** End of main loop (for all logfiles) ***/

  /*** Now for the other logfiles. First cache files. ***/
  /* NB Some of this is shared with main loop and could probably be combined */

  min = 30;   /* reckon all cache entries at half past the hour */
  for (otherlogp = cachefilehead; otherlogp -> name[0] != '\0';
       otherlogp = otherlogp -> next) {  /* for each referrer log */
    
    lf = fopenlog(otherlogp -> name, "cache file", &ispipe);
    if (lf != NULL) {

      if (fgets(inputline, MAXLINELENGTH, lf) == NULL ||
	  sscanf(inputline, "CACHE type %c produced by analo%c", &tempchar,
		 &tempchar2) != 2 ||
	  tempchar2 != 'g' || (tempchar != '1' && tempchar != '2')) {
	if (warnq) {
	  fprintf(stderr, "%s: Warning: %s appears not to be a cache file: ignoring it\n", commandname, otherlogp -> name);
	  anywarns = ON;
	}
      }
      else {
	if (tempchar == '1') {
	  tempint2 = 0;  /* number of page requests then always zero */
	  if (warnq) {
	    fprintf(stderr, "%s: Warning: old style cache file %s contains no page information:\n",
		    commandname, otherlogp -> name);
	    fprintf(stderr, "  page counts will be too low\n");
	    anywarns = ON;
	  }
	}
	while (fgets(inputline, MAXLINELENGTH, lf) != NULL) {
	  if ((tempstr = strtok(inputline, ":")) != NULL) {
	    tempc = tempstr;
	    if (strlen(tempstr) != 10) {
	      if (warnq) {
		fprintf(stderr, "%s: Warning: ignoring corrupt line in cache file %s starting %s\n", commandname, otherlogp -> name, tempc);
		anywarns = ON;
	      }
	    }
	    else {
	      year = 1000 * (*tempstr - '0');
	      year += 100 * (*(++tempstr) - '0');
	      year += 10 * (*(++tempstr) - '0');
	      year += (*(++tempstr) - '0');
	      monthno = 10 * (*(++tempstr) - '0');
	      monthno += *(++tempstr) - '0' - 1;
	      date = 10 * (*(++tempstr) - '0');
	      date += *(++tempstr) - '0';
	      hr = 10 * (*(++tempstr) - '0');
	      hr += *(++tempstr) - '0';
	      tempf = OFF;
	      for ( ; hr < 24 && !tempf; hr++) {
		if ((tempstr = strtok((char *)NULL, ":")) == NULL) {
		  if (warnq) {
		    fprintf(stderr, "%s: Warning: missing data in cache file %s at line starting %s\n", commandname, otherlogp -> name, tempc);
		    anywarns = ON;
		  }
		  tempf = ON;
		}
		else if (tempstr[0] == '*')
		  tempf = ON;
		else {
		  tempint = atoi(tempstr);
		  if (tempchar == '2') {
		    if ((tempstr = strtok((char *)NULL, ":")) == NULL) {
		      if (warnq) {
			fprintf(stderr, "%s: Warning: missing data in cache file %s at line starting %s\n", commandname, otherlogp -> name, tempc);
			anywarns = ON;
		      }
		      tempf = ON;
		    }
		    else
		      tempint2 = atoi(tempstr);
		  }
		  if ((tempstr = strtok((char *)NULL, ":")) == NULL) {
		    if (warnq) {
		      fprintf(stderr, "%s: Warning: missing data in cache file %s at line starting %s\n", commandname, otherlogp -> name, tempc);
		      anywarns = ON;
		    }
		    tempf = ON;
		  }
		  else {
		    bytes = atof(tempstr);
		    thistimecode = timecode(date, monthno, year, hr, min);
		    if (thistimecode >= fromtime.code &&
			thistimecode <= totime.code) {
		      if (firstreq) {
			firstreq = FALSE;
			firsttime.date = date;
			firsttime.monthno = monthno;
			firsttime.year = year;
			firsttime.hr = hr;
			firsttime.min = min;
			firsttime.code = thistimecode;
			if (Wq)
			  firstW -> start = startofweek(firsttime);
			lasttime.date = date;
			lasttime.monthno = monthno;
			lasttime.year = year;
			lasttime.hr = hr;
			lasttime.min = min;
			lasttime.code = thistimecode;
		      }
		      if (q7)
			last7q = (thistimecode > oldtime.code);
		      total_bytes += bytes;
		      cachereqs += tempint;
		      cachepages += tempint2;
		      if (last7q) {
			total_bytes7 += bytes;
			cachereqs7 += tempint;
			cachepages7 += tempint2;
		      }
		      datehash(year, monthno, date, hr, min, thistimecode,
			       tempint, tempint2, bytes);
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
      fcloselog(lf, otherlogp -> name, "cache file", ispipe);
    }
  }

  /*** Now the referrer logs ***/

  if (fq) {

    for (otherlogp = refloghead; otherlogp -> name[0] != '\0';
	 otherlogp = otherlogp -> next) {  /* for each referrer log */

      lf = fopenlog(otherlogp -> name, "referrer log", &ispipe);
      if (lf != NULL) {

	while(fgets(inputline, MAXLINELENGTH, lf) != NULL) {
	  if (sscanf_referrer(inputline, &date, &monthno, &year, &hr, &min,
			     fromurl, filename) == 7) {
	    wantthisone = ON;
	    last7q = OFF;
	    if (datemaskq) {
	      if (date != 0) {
		thistimecode = timecode(date, monthno, year, hr, min);
		wantthisone = thistimecode > fromtime.code &&
		  thistimecode < totime.code;
		if (q7)
		  last7q = (thistimecode > oldtime.code);
	      }
	      else if (!fwarn1 && warnq) {
		fprintf(stderr, "%s: Warning: Referrer log contains lines with no date information;\n", commandname);
		fprintf(stderr, "  cannot apply FROM and TO commands to them.\n");
		fwarn1 = ON;
		anywarns = ON;
	      }
	    }
	    if (!fwarn2 && hostmaskq && wantthisone && warnq) {
	      fprintf(stderr, "%s: Warning: Referrer logs contain no host information;\n", commandname);
	      fprintf(stderr, "  cannot apply HOSTINCLUDE and HOSTEXCLUDE commands to it.\n");
	      fwarn2 = ON;
	      anywarns = ON;
	    }
	    if (!fwarn3) {
	      fwarn3 = ON;
	      for (tempc = fcols; *tempc != '\0' && *tempc != 'B' &&
		   *tempc != 'b'; tempc++)
		;
	      if (*tempc != '\0' || fsortby == BYBYTES) {
		if (warnq) {
		  fprintf(stderr, "%s: Warning: Referrer logs contain no bytes information;\n", commandname);
		  fprintf(stderr, "  cannot report on referrer bytes.\n");
		  anywarns = ON;
		}
		if (fsortby == BYBYTES)
		  fsortby = BYREQUESTS;
		refbyq = OFF;
	      }
	    }
	    if (wantthisone)
	      addref(fromurl, filename, UNSET, 0.0, last7q, filemaskq);
	    else
	      ++total_masked_refs;
	  }
	  else
	    ++total_bad_refs;

	if ((++linesread) == nextreport) {
	  fprintf(stderr, "%s: %d lines read\n", commandname, linesread);
	  nextreport += progressfreq;
	}

#ifdef MAC_EVENTS
	if ((linesread % MAC_IDLE_FREQ) == 0)
	  MacIdle();
#endif

	}
	
	fcloselog(lf, otherlogp -> name, "referrer log", ispipe);
      }
    }
  }

  /* Next the browser logs */

  if (bq || Bq) {

    for (otherlogp = browloghead; otherlogp -> name[0] != '\0';
	 otherlogp = otherlogp -> next) { 

      lf = fopenlog(otherlogp -> name, "browser log", &ispipe);
      if (lf != NULL) {

	while(fgets(inputline, MAXLINELENGTH, lf) != NULL) {
	
	  /* read in the date, if supplied */
	  if (*(tempstr = inputline) == '[') {
	    wantthisone = FALSE;  /* unless date is valid */
	    if (sscanf_date(++tempstr, &date, &monthno, &year, &hr, &min) ==
		5) {
	      tempstr += 20;
	      if (*tempstr == ']') {
		if (*(++tempstr) == ' ') {
		  tempstr++;
		  wantthisone = TRUE;
		}
	      }
	    }
	  }
	  else {
	    wantthisone = TRUE;
	    date = 0;   /* as marker */
	  }

	  if (wantthisone) {
	    last7q = OFF;
	    if (datemaskq) {
	      if (date != 0) {
		thistimecode = timecode(date, monthno, year, hr, min);
		wantthisone = thistimecode > fromtime.code &&
		  thistimecode < totime.code;
		if (q7)
		  last7q = (thistimecode > oldtime.code);
	      }
	      else if (!bwarn1 && warnq) {
		fprintf(stderr, "%s: Warning: Browser log contains lines with no date information;\n", commandname);
		fprintf(stderr, "  cannot apply FROM and TO commands to them.\n");
		bwarn1 = ON;
		anywarns = ON;
	      }
	    }
	    if (warnq) {  /* some boring warnings */
	      if (!bwarn2 && hostmaskq && wantthisone) {
		fprintf(stderr, "%s: Warning: Browser logs contain no host information;\n", commandname);
		fprintf(stderr, "  cannot apply HOSTINCLUDE and HOSTEXCLUDE commands to them.\n");
		bwarn2 = ON;
		anywarns = ON;
	      }
	      if (!bwarn3 && filemaskq && wantthisone) {
		fprintf(stderr, "%s: Warning: Browser logs contain no file information;\n", commandname);
		fprintf(stderr, "  cannot apply FILEINCLUDE and FILEEXCLUDE commands to them.\n");
		bwarn3 = ON;
		anywarns = ON;
	      }
	    }
	    if (!bwarn4) {
	      bwarn4 = ON;
	      tempf = OFF;
	      if (bq && (bsortby == BYBYTES || bsortby == BYPAGES))
		tempf = ON;
	      else if (Bq && (bsortby == BYBYTES || bsortby == BYPAGES))
		tempf = ON;
	      else if (bq) {
		for (tempc = bcols; *tempc != '\0' && *tempc != 'p' &&
		     *tempc != 'P' && *tempc != 'B' && *tempc != 'b';
		     tempc++)
		  ;
		if (*tempc != '\0')
		  tempf = ON;
	      }
	      if (!tempf && Bq) {
		for (tempc = Bcols; *tempc != '\0' && *tempc != 'p' &&
		     *tempc != 'P' && *tempc != 'B' && *tempc != 'b';
		     tempc++)
		  ;
		if (*tempc != '\0')
		  tempf = ON;
	      }
	      if (tempf) {
		if (warnq) {
		  fprintf(stderr, "%s: Warning: Browser logs contain no file information;\n", commandname);
		  fprintf(stderr, "  cannot report on browser page counts or bytes.\n");
		  anywarns = ON;
		}
		browbyq = OFF;
		if (bsortby == BYBYTES || bsortby == BYPAGES)
		  bsortby = BYREQUESTS;
		if (Bsortby == BYBYTES || Bsortby == BYPAGES)
		  Bsortby = BYREQUESTS;
	      }
	    }
	    if (wantthisone) {
	      if ((tempc = strchr(tempstr, '\n')) != NULL)
		*tempc = '\0';
	      strcpy(browser, tempstr);
	      addbrowser(browser, OFF, 0.0, last7q);
	    }
	    else   /* masked out */
	      ++total_masked_brows;

	  }
	  else    /* had bad date */
	    ++total_bad_brows;

	  if ((++linesread) == nextreport) {
	    fprintf(stderr, "%s: %d lines read\n", commandname, linesread);
	    nextreport += progressfreq;
	  }

#ifdef MAC_EVENTS
	  if ((linesread % MAC_IDLE_FREQ) == 0)
	    MacIdle();
#endif

	}

	fcloselog(lf, otherlogp -> name, "browser log", ispipe);

      }
    }
  }

  /* Finally the error logs */

  if (eq) {

    for (otherlogp = errloghead; otherlogp -> name[0] != '\0';
	 otherlogp = otherlogp -> next) {  /* for each logfile */

      lf = fopenlog(otherlogp -> name, "error log", &ispipe);
      if (lf != NULL) {

	while(fgets(inputline, MAXLINELENGTH, lf) != NULL) {
	
	  tempstr = inputline;

	  wantthisone = FALSE;
	  if (*tempstr == '[') {   /* others are non-httpd errors */
	    if (sscanf_olddate(++tempstr, &date, &monthno, &year, &hr, &min)
		== 5) {
	      tempstr += 24;
	      if (*tempstr == ']') {
		if (*(++tempstr) == ' ') {
		  tempstr++;
		  wantthisone = TRUE;
		}
	      }
	    }
	  }

	  if (wantthisone) {
	    if (datemaskq) {
	      thistimecode = timecode(date, monthno, year, hr, min);
	      wantthisone = (thistimecode > fromtime.code) &&
		(thistimecode < totime.code);
	    }
	    if (wantthisone) {
	      strcpy(errstr, tempstr);
	      adderr(errstr);
	    }
	  }

	  if ((++linesread) == nextreport) {
	    fprintf(stderr, "%s: %d lines read\n", commandname, linesread);
	    nextreport += progressfreq;
	  }

#ifdef MAC_EVENTS
	  if ((linesread % MAC_IDLE_FREQ) == 0)
	    MacIdle();
#endif

	}

	fcloselog(lf, otherlogp -> name, "error log", ispipe);
      }

    }
  }

  /* Now writing out the DNS cache */

#ifndef NODNS
  if (dnsq) {
    if (STREQ(dnsfile, "-") || STREQ(dnsfile, "stdin")) {
      lf = stdout;
      if (debug > 0)
	fprintf(stderr, "F: Opening stdout as DNS cache output file\n");
    }
    else if ((lf = fopen(dnsfile, "w")) == NULL) {
      if (warnq) {
	fprintf(stderr,
		"%s: Warning: failed to open DNS cache file %s for writing.\n",
		commandname, dnsfile);
	anywarns = ON;
      }
    }
    else if (debug > 0)
      fprintf(stderr, "F: Opening %s as DNS cache output file\n", dnsfile);
    if (lf != NULL) {
      onlist = 0;
      for (dnsp = dnshead[0]; onlist < dnshashsize; dnsp = dnsnextp) {
	                                /* run through hosts, as below */
	if (dnsp -> number == NULL) {
	  dnsnextp = dnshead[++onlist];
	}
	else {
	  if (isnumeric(dnsp -> number))
	    fprintf(lf, "%d %s %s\n", dnsp -> altimecode, dnsp -> number,
	            (dnsp -> alias == NULL)?"*":(dnsp -> alias));
	  dnsnextp = dnsp -> next;
	}
      }
      fclose(lf);
      if (debug > 0)
	fprintf(stderr, "F: Closing %s\n",
		(STREQ(dnsfile, "-") || STREQ(dnsfile, "stdin"))?"stdout":
		dnsfile);
    }
  }
#endif

  /* now start final accounting */

  for (i = 0; i < NO_STATUS; i++) {
    if (statusnos[i] <= 299 || statusnos[i] == 304) {
      total_succ_reqs += status[i];
      total_succ_reqs7 += status7[i];
    }
    else if (statusnos[i] <= 399) {
      total_other_reqs += status[i];
      total_other_reqs7 += status7[i];
    }
    else {
      total_fail_reqs += status[i];
      total_fail_reqs7 += status7[i];
    }
  }

  tempint = total_succ_reqs;
  total_succ_reqs += cachereqs;
  total_succ_reqs7 += cachereqs7;
  total_page_reqs += cachepages;
  total_page_reqs7 += cachepages7;

  if (tempint == 0) {      /* no logfile successes */
    if (cachereqs == 0) {  /* no cached successes either */
      mq = OFF;
      dq = OFF;
      Dq = OFF;
      Wq = OFF;
      hq = OFF;
      Hq = OFF;
      q7 = OFF;
    }
    oq = OFF;
    iq = OFF;
    tq = OFF;
    rq = OFF;
    Sq = OFF;
    cq = OFF;
  }

  else {   /* there are things to report from the main logfile */

    if (total_succ_reqs7 == 0)
      q7 = OFF;   /* just total_bytes no good in case (!byq) */

    /* Next do aliasing of hosts */

    if (sq == ON)
      allaliases(Shead2, Shead, Shashsize, &no_hosts, &no_hosts7,
		 &no_new_hosts7, 'S');

    /* Now the domain report. This is now easy because all the hostnames
       are already aliased etc. */

    if (oq && sq == ON) {
      onlist = 0;                          /* the list of files we are on */
      hostp = Shead[0];                    /* starting at list 0 */
      for ( ; onlist < Shashsize; hostp = hostnextp) {
                                           /* run through hosts */
	if (hostp -> name == NULL) {       /* then finished this list */
	  hostnextp = Shead[++onlist];     /* so start the next list */
	}
	else {
	  strcpy(hostn, hostp -> name);
	  domhashadd(hostn, hostp -> reqs, hostp -> pages, hostp -> bytes);
	  hostnextp = hostp -> next;
	}
      }
    }

    /* Now for aliasing filenames. */

    if (rq || iq || tq)
      allaliases(rhead2, rhead, rhashsize, &no_urls, &no_urls7, &tempint,
		 'r');

    /* Now the filetype report */

    if (tq) {
      onlist = 0;
      urlp = rhead[0];
      for ( ; onlist < rhashsize; urlp = urlnextp) {
                                         /* run through files */
	if (urlp -> name == NULL) {      /* then finished this list */
	  urlnextp = rhead[++onlist];    /* so start the next list */
	}
	else {
	  hashadd(thead, thashsize, urltoext(urlp -> name), urlp -> reqs,
		  urlp -> bytes, urlp -> pages, urlp -> last7, &tempint,
		  &tempint, &tempint, ON, OFF, OFF, (struct genstruct *)NULL,
		  -1, 't');
	  urlnextp = urlp -> next;
	}
      }
    }      

    /* Now the directory report. */

    if (iq) {
      onlist = 0;                        /* the list of files we are on */
      urlp = rhead[0];                   /* starting at list 0 */
      for ( ; onlist < rhashsize; urlp = urlnextp) {
                                         /* run through files */
	if (urlp -> name == NULL) {      /* then finished this list */
	  urlnextp = rhead[++onlist];  /* so start the next list */
	}
	else {
	  strcpy(filename, urlp -> name);
	  urltodir(filename);
	  hashadd(ihead, ihashsize, filename, urlp -> reqs, urlp -> bytes,
		  urlp -> pages, urlp -> last7, &tempint, &tempint, &tempint,
		  ON, OFF, OFF, (struct genstruct *)NULL, -1, 'i');
	  urlnextp = urlp -> next;
	}
      }
    }
	  
    /*** now for the checking and sorting ***/
    
    if (rq) {
      rsorthead = gensort(rhead, rhashsize, total_succ_reqs,
			  total_page_reqs, total_bytes, rsortby, rminreqstr,
			  rminpagestr, rminbytestr, wantreqhead,
			  OFF, &rmaxreqs, &rmaxpages, &rmaxbytes, &tempint);
      if (rsorthead -> name == NULL)
	rq = OFF;
    }

    if (iq) {
      isorthead = gensort(ihead, ihashsize, total_succ_reqs,
			  total_page_reqs, total_bytes, isortby, iminreqstr,
			  iminpagestr, iminbytestr, (struct include *)NULL,
			  OFF, &imaxreqs, &imaxpages, &imaxbytes, &tempint);
      if (isorthead -> name == NULL)
	iq = OFF;
    }

    if (tq) {
      tsorthead = gensort(thead, thashsize, total_succ_reqs,
			  total_page_reqs, total_bytes, tsortby, tminreqstr,
			  tminpagestr, tminbytestr,(struct include *) NULL,
			  OFF, &tmaxreqs, &tmaxpages, &tmaxbytes, &tempint);
    if (tsorthead -> name == NULL)
      tq = OFF;
    }

    if (Sq) {
      Ssorthead = gensort(Shead, Shashsize, total_succ_reqs,
			  total_page_reqs, total_bytes, Ssortby, Sminreqstr,
			  Sminpagestr, Sminbytestr, (struct include *)NULL,
			  Ssortby == ALPHABETICAL, &Smaxreqs, &Smaxpages,
			  &Smaxbytes, &Smaxlength);
      if (Ssorthead -> name == NULL)
	Sq = OFF;
    }

    if (oq) {
      firstdom = domsort();
      if (onumber == 0)
	oq = OFF;
      else
	subdomsort();
    }

  }    /* end else (there are things to report from the main logfile) */

  if (fq) {
    /* aliasing referrers */
    allaliases(fhead2, fhead, fhashsize, &tempint, &tempint, &tempint, 'f');
    fsorthead = gensort(fhead, fhashsize, total_good_refs, total_ref_pages,
			total_ref_bytes, fsortby, fminreqstr, fminpagestr,
			fminbytestr, (struct include *)NULL, OFF, &fmaxreqs,
			&fmaxpages, &fmaxbytes, &tempint);
    if (fsorthead -> name == NULL)
      fq = OFF;
  }

  if (bq) {
    bsorthead = gensort(bhead, bhashsize, total_good_brows,
			total_brow_pages, total_brow_bytes, bsortby,
			bminreqstr, bminpagestr, bminbytestr,
			(struct include *)NULL, OFF, &bmaxreqs, &bmaxpages,
			&bmaxbytes, &tempint);
    if (bsorthead -> name == NULL)
      bq = OFF;
  } 

  if (Bq) {
    Bsorthead = gensort(Bhead, Bhashsize, total_good_brows,
			total_brow_pages, total_brow_bytes, Bsortby,
			Bminreqstr, Bminpagestr, Bminbytestr,
			(struct include *)NULL, OFF, &Bmaxreqs, &Bmaxpages,
			&Bmaxbytes, &tempint);
    if (Bsorthead -> name == NULL)
      Bq = OFF;
  }

  if (eq) {
    errsort(errorder);
    if (errors[errorder[0]] < eminreqs)
      eq = OFF;
  }

  /*** Finally, do all the output ***/

  if (debug > 0) {
    fprintf(stderr, "S: successful requests: %d (%d)\n", total_succ_reqs,
	    total_succ_reqs7);
    fprintf(stderr, "S: requests from cache: %d (%d)\n", cachereqs,
	    cachereqs7);
    fprintf(stderr, "S: page requests from cache: %d (%d)\n", cachepages,
	    cachepages7);
    fprintf(stderr, "S: failed requests: %d (%d)\n", total_fail_reqs,
	    total_fail_reqs7);
    fprintf(stderr, "S: redirected requests: %d (%d)\n", total_other_reqs,
	    total_other_reqs7);
    fprintf(stderr, "S: successful page requests: %d (%d)\n", total_page_reqs,
	    total_page_reqs7);
    fprintf(stderr, "S: total bytes: %.0f (%.0f)\n", total_bytes,
	    total_bytes7);
    fprintf(stderr, "S: corrupt lines: %d\n", corrupt_lines);
    fprintf(stderr, "S: unwanted lines: %d\n", other_lines);
    fprintf(stderr, "S: good referrer lines: %d\n", total_good_refs);
    fprintf(stderr, "S: bad referrer lines: %d\n", total_bad_refs);
    fprintf(stderr, "S: unwanted referrer lines: %d\n", total_masked_refs);
    fprintf(stderr, "S: referrer lines for pages: %d\n", total_ref_pages);
    fprintf(stderr, "S: total referrer bytes: %.0f\n", total_ref_bytes);
    fprintf(stderr, "S: good browser lines: %d\n", total_good_brows);
    fprintf(stderr, "S: bad browser lines: %d\n", total_bad_brows);
    fprintf(stderr, "S: unwanted browser lines: %d\n", total_masked_brows);
    fprintf(stderr, "S: browser lines for pages: %d\n", total_brow_pages); 
    fprintf(stderr, "S: total browser bytes: %.0f\n", total_brow_bytes);
    anywarns = ON;
 }

  output(rsorthead, isorthead, tsorthead, Ssorthead, firstdom, fsorthead,
	 bsorthead, Bsorthead, errorder);

#ifdef MAC_EVENTS
  MacFini();
#endif

  return(OK);
   
}
