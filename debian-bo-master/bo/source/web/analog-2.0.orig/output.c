/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** output.c; the output function. ***/
/* See output2.c and output3.c for subsidiary functions */

#include "analhea2.h"

void output(struct genstruct *rsorthead, struct genstruct *isorthead,
	    struct genstruct *tsorthead, struct genstruct *Ssorthead,
	    int firstdom, struct genstruct *fsorthead,
	    struct genstruct *bsorthead, struct genstruct *Bsorthead,
	    int errorder[])
{
  extern char *outfile;
  extern char dayname[7][11];
  extern char monthname[12][12];
  extern int monthlength[12];
  extern char *hostname;
  extern char *logourl;
  extern char *hosturl;
  extern char *commandname;
  extern char *headerfile;
  extern char *footerfile;
  extern char reportorder[];
  extern flag byq, refbyq, browbyq, html2;
  extern flag mback, Dback, Wback, Hback;
  extern int mrows, Hrows, Drows;
  extern flag xq, dq, Dq, Wq, hq, Hq, mq, Sq, rq, oq, iq, tq, fq, bq, Bq, cq;
  extern int eq;
  extern int aq, lang, dialect, debug;
  extern struct include *linkhead, *reflinkhead;
  extern struct alias *routaliashead, *ioutaliashead, *Soutaliashead;
  extern struct alias *foutaliashead, *boutaliashead, *toutaliashead;
  extern struct timestruct firsttime, lasttime;
  extern time_t starttime, stoptime;
  extern int weekbeginson;
  extern struct monthly *firstm, *lastm;
  extern struct weekly *firstW, *lastW;
  extern struct daily *firstD, *lastD;
  extern struct hourly *firstH, *lastH;
  extern int dreq[], hreq[], dpag[], hpag[];
  extern double dbytes[], hbytes[];
  extern int munit, Wunit, hunit, Hunit, dunit, Dunit;
  extern double total_bytes, total_ref_bytes, total_brow_bytes;
  extern int total_succ_reqs, total_page_reqs, total_good_refs;
  extern int total_good_brows, total_ref_pages, total_brow_pages;
  extern int pagewidth;
  extern int Ssortby, rsortby, isortby, tsortby, fsortby, bsortby, Bsortby;
  extern char *Sminreqstr, *rminreqstr, *iminreqstr, *fminreqstr;
  extern char *tminreqstr, *bminreqstr, *Bminreqstr;
  extern char *Sminpagestr, *rminpagestr, *iminpagestr, *fminpagestr;
  extern char *tminpagestr, *bminpagestr, *Bminpagestr;
  extern char *Sminbytestr, *rminbytestr, *iminbytestr, *fminbytestr;
  extern char *tminbytestr, *bminbytestr, *Bminbytestr;
  extern char mgraph, dgraph, Dgraph, hgraph, Hgraph, Wgraph;
  extern char mcols[], dcols[], Dcols[], hcols[], Wcols[], Hcols[];
  extern char rcols[], icols[], tcols[], Scols[], fcols[], bcols[], Bcols[];
  extern char *imagedir, *baseurl;
  extern int imaxreqs, tmaxreqs, Smaxreqs, rmaxreqs, fmaxreqs, bmaxreqs;
  extern int Bmaxreqs;
  extern int imaxpages, tmaxpages, Smaxpages, rmaxpages, fmaxpages, bmaxpages;
  extern int Bmaxpages;
  extern double imaxbytes, tmaxbytes, Smaxbytes, rmaxbytes, fmaxbytes;
  extern double bmaxbytes, Bmaxbytes;
  extern char *presep;

  FILE *outf;       /* the output file */
  int fieldwidth;   /* Width we require to print #reqs in */
  int pfieldwidth, bfieldwidth;  /* #pages and #bytes ditto */
  char bprefix[2];  /* kilo, Mega, etc. */
  int graphwidth;   /* the width left for a graph after columns written */
  struct monthly *mp;
  struct daily *dp;
  struct weekly *wp;
  struct hourly *hp;
  int maxreq, maxpages;       /* within a particular date report */
  double maxbytes;
  double bdivider;
  int year, monthno, date, hr;
  flag finished;
  flag ispipe;
  char langstr[MAXSTRINGLENGTH], langstr2[MAXSTRINGLENGTH];
  char langstr3[MAXSTRINGLENGTH], langstr4[MAXSTRINGLENGTH];
  char langgender = 'm';  /* initialise so don't need to set in English */
  int i, j, firsti, lasti, tempint;
  char *ro;
  char templine[MAXLINELENGTH];
  FILE *tempf;

  bprefix[0] = '\0';
  bprefix[1] = '\0';

  if (STREQ(outfile, "stdout") || STREQ(outfile, "-")) {
    outf = stdout;
    if (debug > 0)
      fprintf(stderr, "F: Opening stdout as output file\n");
  }

  else if ((outf = fopen(outfile, "a")) == NULL) {
    fprintf(stderr, "%s: Error: failed to open output file %s for writing.\n",
	    commandname, outfile);
    exit(ERR);  /* shouldn't get here because also tested at the beginning */
  }             /* (unless it's vanished in the meantime or something) */

  else if (debug > 0)
      fprintf(stderr, "F: Opening %s as output file\n", outfile);

  if (aq == CACHE)
    fprintf(outf,
	    "CACHE type 2 produced by analog%s. Do not modify or delete!",
	    VERSION);
  else {
    if (aq == HTML) {
      fprintf(outf, "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n");
      fprintf(outf, "<html>\n<head>\n");
      fprintf(outf, "<meta name=\"GENERATOR\" content=\"analog%s\">\n",
	      VERSION);
      if (lang == ENGLISH)
	fprintf(outf, "<title>Web Server Statistics for ");
      else if (lang == FRENCH)
	fprintf(outf, "<title>Statistiques du Serveur Web pour ");
      else if (lang == GERMAN)
	fprintf(outf, "<title>WWW-Server Statistiken fuer ");
      else if (lang == SPANISH)
	fprintf(outf, "<title>Estad&iacute;sticas del Servidor Web de ");
      else if (lang == DANISH)
	fprintf(outf, "<title>Web Server Statistik for ");
      else /* lang == ITALIAN */
        fprintf(outf, "<title>Statistiche del Web Server per ");
      htmlfprintf(outf, hostname);
      fprintf(outf, "</title></head>\n");
      fprintf(outf, "<body>\n<h1><a NAME=\"Top\">");
      if (!STREQ(logourl, "none")) {
	fprintf(outf, "<IMG src=\"");
	htmlfprintf(outf, logourl);
	fprintf(outf, "\" alt=\"\"> ");
      }
      if (hosturl[0] == '-') {
	if (lang == ENGLISH)
	  fprintf(outf, "Web Server Statistics</a> for ");
	else if (lang == FRENCH)
	  fprintf(outf, "Statistiques du Serveur Web</a> pour ");
	else if (lang == GERMAN)
	  fprintf(outf, "WWW-Server Statistiken</a> f&uuml;r ");
	else if (lang == SPANISH)
	  fprintf(outf, "Estad&iacute;sticas del Servidor Web</a> de ");
	else if (lang == DANISH)
 	  fprintf(outf, "Web Server Statistik</a> for ");
        else /* lang == ITALIAN */
	  fprintf(outf, "Statistiche del Web Server</a> per ");
	htmlfprintf(outf, hostname);
      }
      else {
	if (lang == ENGLISH)
	  fprintf(outf, "Web Server Statistics</a> for <a HREF=\"");
	else if (lang == FRENCH)
	  fprintf(outf, "Statistiques du Serveur Web</a> pour <a HREF=\"");
	else if (lang == GERMAN)
	  fprintf(outf, "WWW-Server Statistiken</a> f&uuml;r <a HREF=\"");
	else if (lang == SPANISH)
	  fprintf(outf,
		  "Estad&iacute;sticas del Servidor Web</a> de <a HREF=\"");
	else if (lang == DANISH)
	  fprintf(outf, "Web Server Statistik</a> for <a HREF=\"");
        else /* lang == ITALIAN */
          fprintf(outf, "Statistiche del Web Server</a> per <a HREF=\"");
	htmlfprintf(outf, hosturl);
	fprintf(outf, "\">");
	htmlfprintf(outf, hostname);
	fprintf(outf, "</a>");
      }
      fprintf(outf, "</h1>\n\n");
    }
    else if (aq == ASCII) {
      if (lang == ENGLISH) {
	fprintf(outf, "Web Server Statistics for %s\n", hostname);
	fprintf(outf, "==========================");
      }
      else if (lang == FRENCH) {
	fprintf(outf, "Statistiques du Serveur Web pour %s\n", hostname);
	fprintf(outf, "=================================");
      }
      else if (lang == GERMAN) {
	fprintf(outf, "WWW-Server Statistiken fuer %s\n", hostname);
	fprintf(outf, "============================");
      }
      else if (lang == SPANISH) {
	fprintf(outf, "Estadisticas del Servidor Web de %s\n", hostname);
	fprintf(outf, "=================================");
      }
      else if (lang == DANISH) {
	fprintf(outf, "Web Server Statistik for %s\n", hostname);
	fprintf(outf, "===========================");
      }
      else {  /* lang == ITALIAN */
        fprintf(outf, "Statistiche del Web Server per %s\n", hostname);
        fprintf(outf, "===============================");
      }
      for (i = (int)strlen(hostname); i > 0; i--)
	fprintf(outf, "=");
      fprintf(outf, "\n");
    }
    
    /* insert header file  -- before top line for HTML, ASCII; after for PRE */
    
    if (!STREQ(headerfile, "none")) {
      tempf = fopenlog(headerfile, "header file", &ispipe);
      if (tempf != NULL) {
	if (aq == HTML)
	  fprintf(outf, "<hr>\n");
	else if (aq == ASCII)
	  fprintf(outf, "\n");
	
	while(fgets(templine, MAXLINELENGTH, tempf) != NULL)
	  fprintf(outf, "%s", templine);
	if (templine[(int)strlen(templine) - 1] != '\n')
	  fprintf(outf, "\n");
	fcloselog(tempf, headerfile, "header file", ispipe);
	
	if (aq == ASCII) {
	  for (i = 0; i < pagewidth; i++)
	    fprintf(outf, "-");
	}
	
	if (aq != PREFORMATTED)
	  fprintf(outf, "\n");

	html2 = OFF;
      }
    }

    if (aq == PREFORMATTED) {
      if (xq) {
	fprintf(outf, "x%sHN%s%s", presep, presep, hostname);
	if (hosturl[0] != '-')
	  fprintf(outf, "\nx%sHU%s%s", presep, presep, hosturl);
      }
    }

  }   /* else (aq != CACHE) */

  /* Summary statistics */

  if (xq)
    gensum(outf);
  else if (aq == ASCII)
    printf("\n");

  /* Now for the rest of the reports, in reportorder order */
    
  for (ro = reportorder; *ro != '\0'; ro++) {

    switch(*ro) {

    case 'm':    /* Monthly report */

      if (mq) {

	/* For forwards reports and (mrows > 0), must get the first month
	   correct (for backwards reports doesn't matter; can just count off
	   the correct number of months as we go (using j below). */

	/* NB This could be more efficient by not remembering months that
	   we're definitely not going to need (re-using those years in the
	   date hashing). Then we only ever need to move forward by at most
	   one year. We couldn't use firsttime for the earliest month recorded
	   though. */

	/* Another alternative would be to have TO times for each report
	   separately. (This would allow them for the general reports too).
	   It wouldn't, however, extend well to the weekly report. */

	if (!mback && mrows > 0) {  /* set year, monthno to 1st month wanted */
	  monthno = lasttime.year * 12 + lasttime.monthno - mrows + 1;
	  year = monthno / 12;
	  monthno %= 12;
	  if (year < firsttime.year ||
	      (year == firsttime.year && monthno <= firsttime.monthno)) {
	    mrows = 0;
	    monthno = 0;
	  }
	  else for (i = year - firsttime.year; i > 0; i--)
	    firstm = firstm -> next;    /* run firstm through enough years */
	}
	else
	  monthno = 0;

	if (aq != PREFORMATTED) {
	  maxreq = 0;
	  maxpages = 0;
	  maxbytes = 0.0;
	  finished = FALSE;
	  for (mp = mback?lastm:firstm; !finished; mp = mp -> next) {
	    for (i = (mp == firstm)?monthno:0; i < 12; i++) {
	      maxreq = MAX(maxreq, mp -> reqs[i]);
	      maxpages = MAX(maxpages, mp -> pages[i]);
	      maxbytes = MAX(maxbytes, mp -> bytes[i]);
	    }
	    if (mp == (mback?firstm:lastm))
	      finished = TRUE;
	  }

	  if (lang == ENGLISH) {
	    strcpy(langstr, "Monthly Report");
	    strcpy(langstr3, "   month");
	  }
	  else if (lang == FRENCH) {
	    strcpy(langstr, "Rapport Mensuel");
	    strcpy(langstr3, "     mois");
	  }
	  else if (lang == GERMAN) {
	    strcpy(langstr, "Monatsbericht");
	    strcpy(langstr3, "  Monate");
	  }
	  else if (lang == SPANISH) {
	    strcpy(langstr, "Informe Mensual");
	    strcpy(langstr3, "     mes");
	  }
	  else if (lang == DANISH) {
	    if (aq == HTML) {
	      strcpy(langstr, "M&aring;nedlig rapport");
	      strcpy(langstr3, "   m&aring;ned");
	    }
	    else {
	      strcpy(langstr, "Maanedlig rapport");
	      strcpy(langstr3, "  maaned");
	    }
          }
          else { /* lang == ITALIAN */
            strcpy(langstr, "Resoconto mesi");
	    strcpy(langstr3, "    mese");
	  }
	  datehead(outf, maxreq, maxpages, maxbytes, mcols, &mgraph, "Monthly",
		   langstr, langstr, langstr3, 'm', &munit, &fieldwidth,
		   &pfieldwidth, &bfieldwidth, &graphwidth, &bdivider);
	}

	if (mback && mrows > 0)
	  j = mrows;
	else
	  j = -1;
	finished = FALSE;
	if (mback || mrows <= 0)     /* else 'year' already calculated */
	  year = (mback?(&lasttime):(&firsttime)) -> year;
	/* really (mback?lasttime:firsttime).year but some compilers choke */
	for (mp = mback?lastm:firstm; !finished; mp = mp -> next) {
	  if (mp == firstm) {
	    if (!mback && mrows > 0)
	      firsti = monthno;
	    else
	      firsti = firsttime.monthno;
	    if (mback)
	      finished = TRUE;
	  }
	  else
	    firsti = 0;
	  if (mp == lastm) {
	    lasti = lasttime.monthno;
	    if (!mback)
	      finished = TRUE;
	  }
	  else
	    lasti = 11;
	  for (i = mback?lasti:firsti; (mback?(i >= firsti):(i <= lasti)) &&
	       j != 0; i += mback?(-1):1) {  /* thro months in chosen order */
	    if (aq == PREFORMATTED)
	      precols(outf, mcols, 'm', byq, ON);
	    else
	      fprintf(outf, "%s %d: ", monthname[i], year);
	    dateline(outf, mp -> reqs[i], mp -> pages[i], mp -> bytes[i],
		     mcols, mgraph, fieldwidth, pfieldwidth, bfieldwidth,
		     munit, bdivider);
	    if (aq == PREFORMATTED)
	      fprintf(outf, "%d%s%d\n", year, presep, i + 1);
	    j--;
	  }
	  year += mback?(-1):1;
	}
	
	if (aq == ASCII)
	  asciiline(outf);
	else if (aq == HTML)
	  fprintf(outf, "</tt></pre>");
	
      }

      break;
      
    case 'W':      /* Weekly report */

      if (Wq) {

	/* For the weekly report, we needn't worry about Wrows at this stage.
	   Becuase there is only one week per structure, we never allocate
	   more than are needed. */

	if (aq != PREFORMATTED) {
	  maxreq = 0;
	  maxpages = 0;
	  maxbytes = 0.0;
	  finished = FALSE;
	  for (wp = Wback?lastW:firstW; !finished; wp = wp -> next) {
	    maxreq = MAX(maxreq, wp -> reqs);
	    maxpages = MAX(maxpages, wp -> pages);
	    maxbytes = MAX(maxbytes, wp -> bytes);
	    if (wp == (Wback?firstW:lastW))
	      finished = TRUE;
	  }

	  if (lang == ENGLISH) {
	    strcpy(langstr, "Weekly Report");
	    strcpy(langstr3, "week beg.");
	  }
	  else if (lang == FRENCH) {
	    strcpy(langstr, "Rapport Hebdomadaire");
	    strcpy(langstr3, " sem. com.");
	  }
	  else if (lang == GERMAN) {
	    strcpy(langstr, "Wochenbericht");
	    strcpy(langstr3, "Woch.beg.");
	  }
	  else if (lang == SPANISH) {
	    strcpy(langstr, "Informe Semanal");
	    strcpy(langstr3, "sem. com.");
	  }
	  else if (lang == DANISH) {
	    strcpy(langstr, "Ugentlig rapport");
	    strcpy(langstr3, "uge start.");
	  }
          else { /* lang == ITALIAN */
            strcpy(langstr, "Resoconto settimane");
            strcpy(langstr3, "sett.com.");
	  } 
	  datehead(outf, maxreq, maxpages, maxbytes, Wcols, &Wgraph, "Weekly",
		   langstr, langstr, langstr3, 'W', &Wunit, &fieldwidth,
		   &pfieldwidth, &bfieldwidth, &graphwidth, &bdivider);
	}

	finished = FALSE;
	for (wp = Wback?lastW:firstW; !finished; wp = wp -> next) {
	  if (aq == PREFORMATTED)
	    precols(outf, Wcols, 'W', byq, ON);
	  else
	    fprintf(outf, "%2d/%s/%02d: ", wp -> start.date,
		    monthname[wp -> start.monthno], wp -> start.year % 100);
	  dateline(outf, wp -> reqs, wp -> pages, wp -> bytes, Wcols, Wgraph,
		   fieldwidth, pfieldwidth, bfieldwidth, Wunit, bdivider);
	  if (aq == PREFORMATTED)
	    fprintf(outf, "%d%s%d%s%d\n", wp -> start.year, presep,
		    wp -> start.monthno + 1, presep, wp -> start.date);
	  if (wp == (Wback?firstW:lastW))
	    finished = TRUE;
	  
	}     /* end running through weeks */

	if (aq == ASCII)
	  asciiline(outf);
	else if (aq == HTML)
	  fprintf(outf, "</tt></pre>");
	
      }   /* end if Wq */

      break;

    case 'd':      /* Daily summary */

      if (dq) {
	
	if (aq != PREFORMATTED) {
	  maxreq = 0;
	  maxpages = 0;
	  maxbytes = 0.0;
	  for (i = 0; i <= 6; i++) {
	    maxreq = MAX(maxreq, dreq[i]);
	    maxpages = MAX(maxpages, dpag[i]);
	    maxbytes = MAX(maxbytes, dbytes[i]);
	  }
	
	  if (lang == ENGLISH) {
	    strcpy(langstr, "Daily Summary");
	    strcpy(langstr2, "Daily Summary");
	    strcpy(langstr3, "day");
	  }
	  else if (lang == FRENCH) {
	    strcpy(langstr, "Resume Quotidien");
	    strcpy(langstr2, "R&eacute;sum&eacute; Quotidien");
	    strcpy(langstr3, "jou");
	  }
	  else if (lang == GERMAN) {
	    strcpy(langstr, "Tagesuebersicht");
	    strcpy(langstr2, "Tages&uuml;bersicht");
	    strcpy(langstr3, "Tag");
	  }
	  else if (lang == SPANISH) {
	    strcpy(langstr, "Resumen Diario");
	    strcpy(langstr2, "Resumen Diario");
	    if (aq == HTML)
	      strcpy(langstr3, "d&iacute;a");
	    else
	      strcpy(langstr3, "dia");
	  }
	  else if (lang == DANISH) {
	    strcpy(langstr, "Daglig oversigt");
	    strcpy(langstr2, "Daglig oversigt");
	    strcpy(langstr3, "dag");
          }
          else { /* lang == ITALIAN */
            strcpy(langstr, "Sommario giorni della settimana");
            strcpy(langstr2, "Sommario giorni della settimana");
            strcpy(langstr3, "gio");
	  }
	  datehead(outf, maxreq, maxpages, maxbytes, dcols, &dgraph, "Daily",
		   langstr, langstr2, langstr3, 'd', &dunit, &fieldwidth,
		   &pfieldwidth, &bfieldwidth, &graphwidth, &bdivider);
	}

	for(i = 0; i <= 6; i++) {
	  j = (weekbeginson + i) % 7;
	  if (aq == PREFORMATTED)
	    precols(outf, dcols, 'd', byq, ON);
	  else
	    fprintf(outf, "%s: ", dayname[j]);
	  dateline(outf, dreq[j], dpag[j], dbytes[j], dcols, dgraph,
		   fieldwidth, pfieldwidth, bfieldwidth, dunit, bdivider);
	  if (aq == PREFORMATTED)
	    fprintf(outf, "%s\n", dayname[j]);
	}
	
	if (aq == ASCII)
	  asciiline(outf);
	else if (aq == HTML)
	  fprintf(outf, "</tt></pre>");
	
      }

      break;
      
    case 'D':       /* Full daily report */

      if (Dq) {

	/* For the daily report, we adopt a slightly different strategy to that
	   for the monthly report above. Rather than calculate the first day
	   explicitly, we move along the months until there are only the right
	   number of days left. */

	if (!Dback && Drows > 0) {
	  tempint = minsbetween(1, firsttime.monthno, firsttime.year, 0, 0,
             lasttime.date, lasttime.monthno, lasttime.year, 0, 0) / 1440 + 1;
	  /* = days between start of first month, and last date, inc. */
	  monthno = firsttime.monthno;
	  year = firsttime.year;
	  if (tempint - firsttime.date < Drows) {
	    Drows = 0;
	    date = 1;
	  }
	  else {
	    while ((tempint -= monthlength[monthno] +
		    ISLEAPFEB(monthno, year)) > Drows) {
	      firstD = firstD -> next;
	      if (++monthno == 12) {
		monthno = 0;
		year++;
	      }
	    }      
	    date = tempint + monthlength[monthno] + ISLEAPFEB(monthno, year) -
	      Drows + 1;  /* when at right month, find initial date wanted */
	  }
	}
	else
	  date = 1;

	if (aq != PREFORMATTED) {
	  maxreq = 0;
	  maxpages = 0;
	  maxbytes = 0.0;
	  finished = FALSE;
	  for (dp = Dback?lastD:firstD; !finished; dp = dp -> next) {
	    for (i = (dp == firstD)?(date - 1):0; i < 31; i++) {
	      maxreq = MAX(maxreq, dp -> reqs[i]);
	      maxpages = MAX(maxpages, dp -> pages[i]);
	      maxbytes = MAX(maxbytes, dp -> bytes[i]);
	    }
	    if (dp == (Dback?firstD:lastD))
	      finished = TRUE;
	  }

	  if (lang == ENGLISH) {
	    strcpy(langstr, "Daily Report");
	    strcpy(langstr3, "     date");
	  }
	  else if (lang == FRENCH) {
	    strcpy(langstr, "Rapport Quotidien");
	    strcpy(langstr3, "      date");  /* NB 1 char longer than Eng */
	  }
	  else if (lang == GERMAN) {
	    strcpy(langstr, "Tagesbericht");
	    strcpy(langstr3, "    Datum");
          }
	  else if (lang == SPANISH) {
	    strcpy(langstr, "Informe Diario");
	    strcpy(langstr3, "    fecha");
	  }
	  else if (lang == DANISH) {
	    strcpy(langstr, "Daglig rapport");
	    strcpy(langstr3, "     dato");
	  }
          else { /* lang == ITALIAN */
            strcpy(langstr, "Resoconto giorni");
            strcpy(langstr3, "     data");
	  }
	  datehead(outf, maxreq, maxpages, maxbytes, Dcols, &Dgraph,
		   "FullDaily", langstr, langstr, langstr3, 'D', &Dunit,
		   &fieldwidth, &pfieldwidth, &bfieldwidth, &graphwidth,
		   &bdivider);
	}
	
	if (Dback && Drows > 0)
	  j = Drows;
	else
	  j = -1;
	finished = FALSE;
	if (Dback || Drows <= 0) {  /* o/wise year and monthno already found */
	  year = (Dback?(&lasttime):(&firsttime)) -> year;
	  monthno = (Dback?(&lasttime):(&firsttime)) -> monthno;
	}
	for (dp = Dback?lastD:firstD; !finished; dp = dp -> next) {
	  if (dp == firstD) {
	    if (!Dback && Drows > 0)
	      firsti = date - 1;
	    else
	      firsti = firsttime.date - 1;
	    if (Dback)
	      finished = TRUE;
	  }
	  else
	    firsti = 0;
	  if (dp == lastD) {
	    lasti = lasttime.date - 1;
	    if (!Dback)
	      finished = TRUE;
	  }
	  else
	    lasti = monthlength[monthno] + ISLEAPFEB(monthno, year) - 1;
	  for (i = Dback?lasti:firsti; (Dback?(i >= firsti):(i <= lasti)) &&
	       j != 0; i += Dback?(-1):1) { /* thro days in chosen order */
	    if (aq == PREFORMATTED)
	      precols(outf, Dcols, 'D', byq, ON);
	    else if (lang == ENGLISH && dialect == US_ENGLISH)
	      fprintf(outf, "%s/%2d/%02d: ", monthname[monthno], i + 1,
		      year % 100);
	    else if (lang == GERMAN)
	      fprintf(outf, "%2d.%s %02d: ", i + 1, monthname[monthno],
		      year % 100);
	    else
	      fprintf(outf, "%2d/%s/%02d: ", i + 1, monthname[monthno],
		      year % 100);
	    dateline(outf, dp -> reqs[i], dp -> pages[i], dp -> bytes[i],
		     Dcols, Dgraph, fieldwidth, pfieldwidth, bfieldwidth,
		     Dunit, bdivider);
	    if (aq == PREFORMATTED)
	      fprintf(outf, "%d%s%d%s%d\n", year, presep, monthno + 1, presep,
		      i + 1);
	    else if (((dayofdate(i + 1, monthno, year) + (!Dback)) % 7 ==
		    weekbeginson) && !(finished && i == (Dback?firsti:lasti)))
	      fprintf(outf, "\n");
                         /* extra blank line after each week (not last) */
	    j--;
	  }

	  if (Dback) {
	    if ((--monthno) == -1) {
	      monthno = 11;
	      --year;
	    }
	  }
	  else {
	    if ((++monthno) == 12) {
	      monthno = 0;
	      ++year;
	    }
	  }
	  
	}     /* end running through dp's */

	if (aq == ASCII)
	  asciiline(outf);
	else if (aq == HTML)
	  fprintf(outf, "</tt></pre>");
	
      }   /* end if Dq */

      break;

    case 'H':       /* Full hourly report */

      /* This is essentially like the daily report above */

      if (Hq) {

	if (!Hback && Hrows > 0) {
	  tempint = minsbetween(firsttime.date, firsttime.monthno,
	      firsttime.year, 0, 0, lasttime.date, lasttime.monthno,
	      lasttime.year, lasttime.hr, 0) / 60 + 1;
	  /* = hrs between start of first day, and last hr, inc. */
	  date = firsttime.date;
	  monthno = firsttime.monthno;
	  year = firsttime.year;
	  if (tempint - firsttime.hr < Hrows) {
	    Hrows = 0;
	    hr = 0;
	  }
	  else {
	    for ( ; tempint - Hrows > 23; tempint -= 24) {
	      firstH = firstH -> next;
	      if (++date > monthlength[monthno] + ISLEAPFEB(monthno, year)) {
		date = 1;
		if (++monthno == 12) {
		  monthno = 0;
		  year++;
		}
	      }
	    }
	    hr = tempint - Hrows;
	  }
	}
	else
	  hr = 0;

	if (aq != PREFORMATTED) {
	  maxreq = 0;
	  maxpages = 0;
	  maxbytes = 0.0;
	  finished = FALSE;
	  for (hp = Hback?lastH:firstH; !finished; hp = hp -> next) {
	    for (i = ((hp == firstH)?hr:0); i < 24; i++) {
	      maxreq = MAX(maxreq, hp -> reqs[i]);
	      maxpages = MAX(maxpages, hp -> pages[i]);
	      maxbytes = MAX(maxbytes, hp -> bytes[i]);
	    }
	    if (hp == (Hback?firstH:lastH))
	      finished = TRUE;
	  }

	  if (lang == ENGLISH) {
	    strcpy(langstr, "Hourly Report");
	    strcpy(langstr2, "Hourly Report");
	    strcpy(langstr3, "     date:hr");
	  }
	  else if (lang == FRENCH) {
	    strcpy(langstr, "Rapport Horaire");
	    strcpy(langstr2, "Rapport Horaire");
	    strcpy(langstr3, "      date:hr"); /* NB 1 char longer than Eng */
	  }
	  else if (lang == GERMAN) {
	    strcpy(langstr, "Stuendlicher Bericht");
	    strcpy(langstr2, "St&uuml;ndlicher Bericht");
	    strcpy(langstr3, "    Datum:St");
	  }
	  else if (lang == SPANISH) {
	    strcpy(langstr, "Informe Horario");
	    strcpy(langstr2, "Informe Horario");
	    strcpy(langstr3, "    fecha:hr");
	  }
	  else if (lang == DANISH) {
	    strcpy(langstr, "Time rapport");
	    strcpy(langstr2, "Time rapport");
	    strcpy(langstr3, "     dato:kl");
	  }
          else { /* lang == ITALIAN */
            strcpy(langstr, "Resoconto ore");
            strcpy(langstr2, "Resoconto ore");
            strcpy(langstr3, "     data:hh");  /* NB 1 char longer than Eng */
	  }
	  if (aq != CACHE)
	    datehead(outf, maxreq, maxpages, maxbytes, Hcols, &Hgraph,
		     "FullHourly", langstr, langstr2, langstr3, 'H',
		     &Hunit, &fieldwidth, &pfieldwidth, &bfieldwidth,
		     &graphwidth, &bdivider);
	}

	if (Hback && Hrows > 0)
	  j = Hrows;
	else
	  j = -1;
	finished = FALSE;
	if (Hback || Hrows <= 0) {
	  year = (Hback?(&lasttime):(&firsttime)) -> year;
	  monthno = (Hback?(&lasttime):(&firsttime)) -> monthno;
	  date = (Hback?(&lasttime):(&firsttime)) -> date;
	}
	for (hp = Hback?lastH:firstH; !finished; hp = hp -> next) {
	  if (hp == firstH) {
	    if (!Hback && Hrows > 0)
	      firsti = hr;
	    else
	      firsti = firsttime.hr;
	    if (Hback)
	      finished = TRUE;
	  }
	  else
	    firsti = 0;
	  if (hp == lastH) {
	    lasti = lasttime.hr;
	    if (!Hback)
	      finished = TRUE;
	  }
	  else
	    lasti = 23;
	  for (i = Hback?lasti:firsti; (Hback?(i >= firsti):(i <= lasti)) &&
	       j != 0; i += Hback?(-1):1) {  /* through hrs in chosen order */
	    if (aq == CACHE) {
	      if (i == 0 || (hp == firstH && i == firsti))
		fprintf(outf, "\n%d%02d%02d%02d", year, monthno + 1, date, i);
	      fprintf(outf, ":%d:%d:%.0f", hp -> reqs[i], hp -> pages[i],
		      hp -> bytes[i]);
	    }
	    else {
	      if (aq == PREFORMATTED)
		precols(outf, Hcols, 'H', byq, ON);
	      else if (lang == ENGLISH && dialect == US_ENGLISH)
		fprintf(outf, "%s/%2d/%02d:%02d: ", monthname[monthno], date,
			year % 100, i);
	      else if (lang == GERMAN)
		fprintf(outf, "%2d.%s %02d:%02d: ", date, monthname[monthno],
			year % 100, i);
	      else
		fprintf(outf, "%2d/%s/%02d:%02d: ", date, monthname[monthno],
			year % 100, i);
	      dateline(outf, hp -> reqs[i], hp -> pages[i], hp -> bytes[i],
		       Hcols, Hgraph, fieldwidth, pfieldwidth, bfieldwidth,
		       Hunit, bdivider);
	      if (aq == PREFORMATTED)
		fprintf(outf, "%d%s%d%s%d%s%d\n", year, presep, monthno + 1,
			presep, date, presep, i);
	      else if (i == (Hback?0:23) && !finished)
		fprintf(outf, "\n");
	      /* extra blank line after each day (not last) */
	      j--;
	    }
	  }

	  if (Hback) {
	    if ((--date) == 0) {
	      if ((--monthno) == -1) {
		monthno = 11;
		--year;
	      }
	      date = monthlength[monthno] + ISLEAPFEB(monthno, year);
	    }
	  }
	  else {
	    if ((++date) > monthlength[monthno] + ISLEAPFEB(monthno, year)) {
	      if ((++monthno) == 12) {
		monthno = 0;
		++year;
	      }
	      date = 1;
	    }
	  }
	  
	}     /* end running through hp's */

	if (aq == CACHE)
	  fprintf(outf, ":*\n");
	else if (aq == ASCII)
	  asciiline(outf);
	else if (aq == HTML)
	  fprintf(outf, "</tt></pre>");
	
      }   /* end if Hq */
    
      break;

    case 'h': /* Hourly summary */

      if (hq) {

	if (aq != PREFORMATTED) {
	  maxreq = 0;
	  maxpages = 0;
	  maxbytes = 0.0;
	  for (i = 0; i <= 23; i++) {
	    maxreq = MAX(maxreq, hreq[i]);
	    maxpages = MAX(maxpages, hpag[i]);
	    maxbytes = MAX(maxbytes, hbytes[i]);
	  }

	  if (lang == ENGLISH) {
	    strcpy(langstr, "Hourly Summary");
	    strcpy(langstr2, "Hourly Summary");
	    strcpy(langstr3, "hr");
	  }
	  else if (lang == FRENCH) {
	    strcpy(langstr, "Resume Horaire");
	    strcpy(langstr2, "R&eacute;sum&eacute; Horaire");
	    strcpy(langstr3, "hr");
	  }
	  else if (lang == GERMAN) {
	    strcpy(langstr, "Stuendliche Uebersicht");
	    strcpy(langstr2, "St&uuml;ndliche &Uuml;bersicht");
	    strcpy(langstr3, "St");
	  }
	  else if (lang == SPANISH) {
	    strcpy(langstr, "Resumen Horario");
	    strcpy(langstr2, "Resumen Horario");
	    strcpy(langstr3, "hr");
	  }
	  else if (lang == DANISH) {
	    strcpy(langstr, "Time oversigt");
	    strcpy(langstr2, "Time oversigt");
	    strcpy(langstr3, "kl");
	  }
          else { /* lang == ITALIAN */
            strcpy(langstr, "Sommario ore del giorno");
            strcpy(langstr2, "Sommario ore del giorno");
            strcpy(langstr3, "hh");
	  }
	  datehead(outf, maxreq, maxpages, maxbytes, hcols, &hgraph, "Hourly",
		   langstr, langstr2, langstr3, 'h', &hunit, &fieldwidth,
		   &pfieldwidth, &bfieldwidth, &graphwidth, &bdivider);
	}

	for(i = 0; i <= 23; i++) {
	  if (aq == PREFORMATTED)
	    precols(outf, hcols, 'h', byq, ON);
	  else
	    fprintf(outf, "%2d: ", i);
	  dateline(outf, hreq[i], hpag[i], hbytes[i], hcols, hgraph,
		   fieldwidth, pfieldwidth, bfieldwidth, hunit, bdivider);
	  if (aq == PREFORMATTED)
	    fprintf(outf, "%d\n", i);
	}
	
	if (aq == ASCII)
	  asciiline(outf);
	else if (aq == HTML)
	  fprintf(outf, "</tt></pre>");
	
      }
      
      break;

    case 'o':    /* Domain report */

      if (oq)
	domout(outf, firstdom);
	
      break;

    case 'S':    /* Host report */

      if (Sq) {
	if (lang == ENGLISH) {
	  strcpy(langstr, "Host Report");
	  strcpy(langstr2, "host");
	  strcpy(langstr3, "hosts");
	  strcpy(langstr4, "host");
	}
	else if (lang == GERMAN) {
	  strcpy(langstr, "Host-Bericht");
	  strcpy(langstr2, "Hosts");
	  strcpy(langstr3, "Hosts");
	  strcpy(langstr4, "Host");
	  langgender = 'n';
	}
        else if (lang == ITALIAN) {
          strcpy(langstr, "Resoconto host");
          strcpy(langstr2, "host");
          strcpy(langstr3, "host");
          strcpy(langstr4, "host");
          langgender = 'n';  /* special code to mean: use _gli_, not _i_ */
	}
	else if (lang == SPANISH && aq == HTML) {
	  strcpy(langstr, "Informe de <i>Hosts</i>");
	  /* NB Italics, as 'host' is not Spanish */
	  strcpy(langstr2, "<i>host</i>");
	  strcpy(langstr3, "<i>hosts</i>");
	  strcpy(langstr4, "host");
	  langgender = 'm';
	}
	else if (lang == SPANISH) { /* and aq != HTML */
	  strcpy(langstr, "Informe de Hosts");
	  strcpy(langstr2, "host");
	  strcpy(langstr3, "hosts");
	  strcpy(langstr4, "host");
	  langgender = 'm';
	}
	else if (lang == DANISH) {
	  strcpy(langstr, "Host rapport");
	  strcpy(langstr2, "host");
	  strcpy(langstr3, "hosts");
	  strcpy(langstr4, "host");
	}
	else if (aq == HTML) { /* and FRENCH */
	  strcpy(langstr, "Rapport des h&ocirc;tes");
	  strcpy(langstr2, "h&ocirc;te");
	  strcpy(langstr3, "h&ocirc;tes");
	  strcpy(langstr4, "h&ocirc;te");
	  langgender = 'm';
	}
	else { /* lang == FRENCH and aq != HTML */
	  strcpy(langstr, "Rapport des hotes");
	  strcpy(langstr2, "hote");
	  strcpy(langstr3, "hotes");
	  strcpy(langstr4, "hote");
	  langgender = 'm';
	}
 	genout(outf, Ssorthead, total_succ_reqs, total_page_reqs, total_bytes,
	       Ssortby, Sminreqstr, Sminpagestr, Sminbytestr, Smaxreqs,
	       Smaxpages, Smaxbytes, Scols, "Host", langstr, langstr2,
	       langstr3, langstr4, langgender, 'S', Ssortby == ALPHABETICAL,
	       byq, ON, NULL, Soutaliashead, "");
      }
      break;
    
    case 'i':   /* Directory report */
   
      if (iq) {
	if (lang == ENGLISH) {
	  strcpy(langstr, "Directory Report");
	  strcpy(langstr2, "directory");
	  strcpy(langstr3, "directories");
	  strcpy(langstr4, "directory");
	}
	else if (lang == GERMAN) {
	  strcpy(langstr, "Verzeichnis-Bericht");
	  strcpy(langstr2, "Verzeichnisses");
	  strcpy(langstr3, "Verzeichnisse");
	  strcpy(langstr4, "Verzeichnis");
	  langgender = 'n';
	}
        else if (lang == ITALIAN) {
          strcpy(langstr," Resoconto direttori");
          strcpy(langstr2, "direttorio");
          strcpy(langstr3, "direttori");
          strcpy(langstr4, "direttorio");
          langgender = 'm';
        }
	else if (lang == SPANISH) {
	  strcpy(langstr, "Informe de Directorios");
	  strcpy(langstr2, "directorio");
	  strcpy(langstr3, "directorios");
	  strcpy(langstr4, "directorio");
	  langgender = 'm';
	}
	else if (lang == DANISH) {
	  strcpy(langstr, "Directory rapport");
	  strcpy(langstr2, "directory");
	  strcpy(langstr3, "directories");
	  strcpy(langstr4, "directory");
	}
	else if (aq == HTML) { /* and FRENCH */
	  strcpy(langstr, "Rapport des r&eacute;pertoires");
	  strcpy(langstr2, "r&eacute;pertoire");
	  strcpy(langstr3, "r&eacute;pertoires");
	  strcpy(langstr4, "r&eacute;pertoire");
	  langgender = 'm';
	}
	else { /* lang == FRENCH and aq != HTML */
	  strcpy(langstr, "Rapport des repertoires");
	  strcpy(langstr2, "repertoire");
	  strcpy(langstr3, "repertoires");
	  strcpy(langstr4, "repertoire");
	  langgender = 'm';
	}
	genout(outf, isorthead, total_succ_reqs, total_page_reqs, total_bytes,
	       isortby, iminreqstr, iminpagestr, iminbytestr, imaxreqs,
	       imaxpages, imaxbytes, icols, "Directory", langstr, langstr2,
	       langstr3, langstr4, langgender, 'i', FALSE, byq, ON, NULL,
	       ioutaliashead, "");
      }
      break;

    case 't':   /* File type report */
   
      if (tq) {
	if (lang == ENGLISH) {
	  strcpy(langstr, "File Type Report");
	  strcpy(langstr2, "extension");
	  strcpy(langstr3, "extensions");
	  strcpy(langstr4, "extension");
	}
	else if (lang == FRENCH) {
	  strcpy(langstr, "Rapport des Types de Fichier");
	  strcpy(langstr2, "extension");
	  strcpy(langstr3, "extensions");
	  strcpy(langstr4, "extension");
	  langgender = 'f';
	}
	else if (lang == GERMAN) {
	  strcpy(langstr, "Dateityp-Bericht");
	  strcpy(langstr2, "Extension");
	  strcpy(langstr3, "Extensions");
	  strcpy(langstr4, "Extension");
	  langgender = 'f';
	}
        else if (lang == ITALIAN) {
          strcpy(langstr, "Resoconto tipi di file");
          strcpy(langstr2, "estensione");
          strcpy(langstr3, "estensioni");
          strcpy(langstr4, "estensione");
          langgender = 'f';
        }
	else if (lang == DANISH) {
	  strcpy(langstr, "Filtype rapport");
	  strcpy(langstr2, "endelse");
	  strcpy(langstr3, "endelser");
	  strcpy(langstr4, "endelse");
	  langgender = 'f';
	}
	else if (aq == HTML) { /* and SPANISH */
	  strcpy(langstr, "Informe de Tipos de Fichero");
	  strcpy(langstr2, "extensi&oacute;n");
	  strcpy(langstr3, "extensi&oacute;nes");
	  strcpy(langstr4, "extensi&oacute;n");
	  langgender = 'f';
	}
	else { /* lang == SPANISH and aq != HTML */
	  strcpy(langstr, "Informe de Tipos de Fichero");
	  strcpy(langstr2, "extension");
	  strcpy(langstr3, "extensiones");
	  strcpy(langstr4, "extension");
	  langgender = 'f';
	}
	genout(outf, tsorthead, total_succ_reqs, total_page_reqs,
	       total_bytes, tsortby, tminreqstr, tminpagestr, tminbytestr,
	       tmaxreqs, tmaxpages, tmaxbytes, tcols, "FileType", langstr,
	       langstr2, langstr3, langstr4, langgender, 't', FALSE, byq,
	       ON, NULL, toutaliashead, "");
      }
      break;

    case 'r':    /* Request report */
      
      if (rq) {
	if (lang == ENGLISH) {
	  strcpy(langstr, "Request Report");
	  strcpy(langstr2, "requested file");
	  strcpy(langstr3, "requested files");
	  strcpy(langstr4, "file");
	}
	else if (lang == GERMAN) {
	  strcpy(langstr, "Anfrage-Bericht");
	  strcpy(langstr2, "verlangten Datei");
	  strcpy(langstr3, "verlangten Dateien");
	  strcpy(langstr4, "Datei");
	  langgender = 'f';
	}
        else if (lang == ITALIAN) {
          strcpy(langstr, "Resoconto richieste");
          strcpy(langstr2, "file richiesto");
          strcpy(langstr3, "file richiesti");
          strcpy(langstr4, "file");
          langgender = 'm';
        }
	else if (lang == SPANISH) {
	  strcpy(langstr, "Informe de Peticiones");
	  strcpy(langstr2, "fichero solicitado");
	  strcpy(langstr3, "ficheros solicitados");
	  strcpy(langstr4, "fichero");
	  langgender = 'm';
	}
	else if (lang == DANISH) {
	  if (aq == HTML)
	    strcpy(langstr, "Foresp&oslash;rgselsrapport");
	  else
	    strcpy(langstr, "Forespoergselsrapport");
          strcpy(langstr2, "forespurgt fil");
          strcpy(langstr3, "forespurgte filer");
          strcpy(langstr4, "fil");
	}
	else if (aq == HTML) { /* and FRENCH */
	  strcpy(langstr, "Rapport des requ&ecirc;tes");
	  strcpy(langstr2, "fichier demand&eacute;");
	  strcpy(langstr3, "fichiers demand&eacute;s");
	  strcpy(langstr4, "fichier");
	  langgender = 'm';
	}
	else { /* lang == FRENCH and aq != HTML */
	  strcpy(langstr, "Rapport des requetes");
	  strcpy(langstr2, "fichier demande");
	  strcpy(langstr3, "fichiers demandes");
	  strcpy(langstr4, "fichier");
	  langgender = 'm';
	}
	genout(outf, rsorthead, total_succ_reqs, total_page_reqs, total_bytes,
	       rsortby, rminreqstr, rminpagestr, rminbytestr, rmaxreqs,
	       rmaxpages, rmaxbytes, rcols, "Request", langstr, langstr2,
	       langstr3, langstr4, langgender, 'r', FALSE, byq, ON,
	       (aq == HTML)?linkhead:NULL, routaliashead, baseurl);
      }
      break;

    case 'f':    /* Referrer report */

      if (fq) {
	if (lang == ENGLISH) {
	  strcpy(langstr, "Referrer Report");
	  strcpy(langstr2, "referring URL");
	  strcpy(langstr3, "referring URLs");
	  strcpy(langstr4, "URL");
	}
	else if (lang == GERMAN) {
	  strcpy(langstr, "Verweis-Bericht");
	  strcpy(langstr2, "verweisenden URL");
	  strcpy(langstr3, "verweisenden URLs");
	  strcpy(langstr4, "URL");
	  langgender = 'f';
	}
	else if (lang == FRENCH) {
	  strcpy(langstr, "Rapport des Appelants");
	  strcpy(langstr2, "URL appelant");
	  strcpy(langstr3, "URL appelants");
	  strcpy(langstr4, "URL");
	  langgender = 'm';
	}
	else if (lang == SPANISH) {
	  strcpy(langstr, "Informe de Remitentes");
	  strcpy(langstr2, "URL remitente");
	  strcpy(langstr3, "URLs remitentes");
	  strcpy(langstr4, "URL");
	  langgender = 'm';
	}
	else if (lang == DANISH) {
	  strcpy(langstr, "Refererende adresserapport");
	  strcpy(langstr2, "refererende URL");
	  strcpy(langstr3, "refererende URLs");
	  strcpy(langstr4, "URL");
	  langgender = 'm';
	}
        else { /* lang == ITALIAN */
          strcpy(langstr, "Resoconto provenienze");
          strcpy(langstr2, "URL di provenienza");
          strcpy(langstr3, "URL di provenienza");
          strcpy(langstr4, "URL");
          langgender = 'f';
        }
	genout(outf, fsorthead, total_good_refs, total_ref_pages,
	       total_ref_bytes, fsortby, fminreqstr, fminpagestr, fminbytestr,
	       fmaxreqs, fmaxpages, fmaxbytes, fcols, "Referrer", langstr,
	       langstr2, langstr3, langstr4, langgender, 'f', FALSE, refbyq,
	       ON, (aq == HTML)?reflinkhead:NULL, foutaliashead, "");
      }
      break;

    case 'b':    /* Browser summary */

      if (bq) {
	if (lang == ENGLISH) {
	  strcpy(langstr, "Browser Summary");
	  strcpy(langstr2, "browser");
	  strcpy(langstr3, "browsers");
	  strcpy(langstr4, "browser");
	}
	else if (lang == GERMAN) {
	  if (aq == HTML)
	    strcpy(langstr, "Browser-&Uuml;bersicht");
	  else
	    strcpy(langstr, "Browser-Uebersicht");
	  strcpy(langstr2, "Browsers");
	  strcpy(langstr3, "Browser");
	  strcpy(langstr4, "Browser");
	  langgender = 'n';
	}
	else if (lang == FRENCH) {
	  if (aq == HTML)
	    strcpy(langstr, "R&eacute;sum&eacute; des navigateurs");
	  else
	    strcpy(langstr, "Resume des navigateurs");
	  strcpy(langstr2, "navigateur");
	  strcpy(langstr3, "navigateurs");
	  strcpy(langstr4, "navigateur");
	  langgender = 'm';
	}
	else if (lang == SPANISH && aq == HTML) {
    	  strcpy(langstr, "Resumen de <i>Browsers</i>");
	  strcpy(langstr2, "<i>browser</i>");
	  strcpy(langstr3, "<i>browsers</i>");
	  strcpy(langstr4, "browser");
	  langgender = 'm';
	}
	else if (lang == SPANISH) {  /* and aq != HTML */
	  strcpy(langstr, "Resumen de Browsers");
	  strcpy(langstr2, "browser");
	  strcpy(langstr3, "browsers");
	  strcpy(langstr4, "browser");
	  langgender = 'm';
	}
	else if (lang == DANISH) {
	  strcpy(langstr, "Browser oversigt");
	  strcpy(langstr2, "browser");
	  strcpy(langstr3, "browsere");
	  strcpy(langstr4, "browser");
	  langgender = 'm';
	}
        else { /* lang == ITALIAN */
          strcpy(langstr, "Sommario browser");
          strcpy(langstr2, "browser");
          strcpy(langstr3, "browser");
          strcpy(langstr4, "browser");
          langgender = 'm';
        }
	genout(outf, bsorthead, total_good_brows, total_brow_pages,
	       total_brow_bytes, bsortby, bminreqstr, bminpagestr,
	       bminbytestr, bmaxreqs, bmaxpages, bmaxbytes, bcols, "Browser",
	       langstr, langstr2, langstr3, langstr4, langgender, 'b', FALSE,
	       browbyq, browbyq, NULL, boutaliashead, "");
      }
      break;
      
    case 'B':    /* Full browser report */

      if (Bq) {
	if (lang == ENGLISH) {
	  strcpy(langstr, "Browser Report");
	  strcpy(langstr2, "browser");
	  strcpy(langstr3, "browsers");
	  strcpy(langstr4, "browser");
	}
	else if (lang == GERMAN) {
	  strcpy(langstr, "Browser-Bericht");
	  strcpy(langstr2, "Browsers");
	  strcpy(langstr3, "Browser");
	  strcpy(langstr4, "Browser");
	  langgender = 'n';
	}
	else if (lang == FRENCH) {
	  strcpy(langstr, "Rapport des navigateurs");
	  strcpy(langstr2, "navigateur");
	  strcpy(langstr3, "navigateurs");
	  strcpy(langstr4, "navigateur");
	  langgender = 'm';
	}
        else if (lang == ITALIAN) {
          strcpy(langstr, "Resoconto browser");
          strcpy(langstr2, "browser");
          strcpy(langstr3, "browser");
          strcpy(langstr4, "browser");
          langgender = 'm';
        }
	else if (lang == DANISH) {
	  strcpy(langstr, "Browser rapport");
	  strcpy(langstr2, "browser");
	  strcpy(langstr3, "browsere");
	  strcpy(langstr4, "browser");
	  langgender = 'm';
	}
	else if (aq == HTML) { /*  and SPANISH */
	  strcpy(langstr, "Informe de <i>Browsers</i>");
	  strcpy(langstr2, "<i>browser</i>");
	  strcpy(langstr3, "<i>browsers</i>");
	  strcpy(langstr4, "browser");
	  langgender = 'm';
	}
	else { /* lang == SPANISH and aq != HTML */
	  strcpy(langstr, "Informe de Browsers");
	  strcpy(langstr2, "browsers");
	  strcpy(langstr3, "browser");
	  strcpy(langstr4, "browser");
	  langgender = 'm';
	}
	genout(outf, Bsorthead, total_good_brows, total_brow_pages,
	       total_brow_bytes, Bsortby, Bminreqstr, Bminpagestr,
	       Bminbytestr, Bmaxreqs, Bmaxpages, Bmaxbytes, Bcols,
	       "FullBrowser", langstr, langstr2, langstr3, langstr4,
	       langgender, 'B', FALSE, browbyq, browbyq, NULL, NULL, "");
      }
      break;

    case 'c':

      if (cq)
	statusout(outf);
      break;

    case 'e':

      if (eq)
	errout(outf, errorder);
      break;

    }    /* end switch */
  }      /* end for ro */

  /*** Bit at the bottom of the page ***/

  if (aq != CACHE) {
    if (aq != PREFORMATTED) {
      if (!aq) {
	if (lang == ENGLISH)
	  fprintf(outf, "\n\n<hr>\n<i>This analysis was produced by <a HREF=\"http://www.statslab.cam.ac.uk/~sret1/analog/\">analog%s</a>.\n", VERSION);
	else if (lang == FRENCH)
	  fprintf(outf, "\n\n<hr>\n<i>Ces statistiques sont produites par <a HREF=\"http://www.statslab.cam.ac.uk/~sret1/analog/\">analog%s</a>.\n", VERSION);
	else if (lang == GERMAN)
	  fprintf(outf, "\n\n<hr>\n<i>Diese Statistik wurde mit <a HREF=\"http://www.statslab.cam.ac.uk/~sret1/analog/\">analog%s</a> erstellt.\n", VERSION);
	else if (lang == SPANISH)
	  fprintf(outf, "\n\n<hr>\n<i>Estad&iacute;sticas generadas por <a HREF=\"http://www.statslab.cam.ac.uk/~sret1/analog/\">analog%s</a>.\n", VERSION);
	else if (lang == DANISH)
	  fprintf(outf, "\n\n<hr>\n<i>Denne analyse blev produceret af <a HREF=\"http://www.statslab.cam.ac.uk/~sret1/analog/\">analog%s</a>.\n", VERSION);
        else /* lang == ITALIAN */
          fprintf(outf, "\n\n<hr>\n<i>Queste statistiche sono state prodotte da <a HREF=\"http://www.statslab.cam.ac.uk/~sret1/analog/\">analog%s</a>.\n", VERSION);
      }
      else if (lang == ENGLISH)
	fprintf(outf, "This analysis was produced by analog%s.\n", VERSION);
      else if (lang == FRENCH)
	fprintf(outf, "Ces statistiques sont produites par analog%s.\n",
		VERSION);
      else if (lang == GERMAN)
        fprintf(outf, "Diese Statistik wurde mit analog%s erstellt.\n",
		VERSION);
      else if (lang == SPANISH)
	fprintf(outf, "Estadisticas generadas por analog%s.\n",
		VERSION);
      else if (lang == DANISH)
	fprintf(outf, "Denne analyse blev produceret af analog%s.\n", VERSION);
      else /* lang == ITALIAN */
        fprintf(outf, "Queste statistiche sono state prodotte da analog%s.\n", VERSION);

      time(&stoptime);
  
      stoptime -= starttime;   /* so now measures elapsed time */

      if (stoptime == 0) {
	if (aq == HTML) {
	  if (lang == ENGLISH)
	    fprintf(outf,
		    "<br><b>Running time:</b> Less than 1 second.</i>\n");
	  else if (lang == FRENCH)
	    fprintf(outf, "<br><b>Temps d'ex&eacute;cution :</b> Moins d'une seconde.</i>\n");
	  else if (lang == GERMAN)
	    fprintf(outf, "<br><b>Laufzeit:</b> Unter einer Sekunde.</i>\n");
	  else if (lang == SPANISH)
	    fprintf(outf, "<br><b>Tiempo de ejecuci&oacute;n:</b> Menos de 1 segundo.</i>\n");
	  else if (lang == DANISH)
	    fprintf(outf, "<br><b>Analysetid:</b> Under 1 sekund.</i>\n");
          else /* lang == ITALIAN */
            fprintf(outf, "<br><b>Tempo di esecuzione:</b> Meno di un secondo.</i>\n");
	}
	else if (lang == ENGLISH)
	  fprintf(outf, "Running time: Less than 1 second.\n");
	else if (lang == FRENCH)
	  fprintf(outf, "Temps d'execution : Moins d'une seconde.\n");
	else if (lang == GERMAN)
	  fprintf(outf, "Laufzeit: Unter einer Sekunde.\n");
	else if (lang == SPANISH)
	  fprintf(outf, "Tiempo d ejecucion: Menos de 1 segundo.\n");
	else if (lang == DANISH)
	  fprintf(outf, "Analysetid: Under 1 sekund.\n");
        else /* lang == ITALIAN */
          fprintf(outf, "Tempo di esecuzione: Meno di un secondo.\n");
      }

      else if (stoptime < 60) {
	if (aq == HTML) {
	  if (lang == ENGLISH)
	    fprintf(outf, "<br><b>Running time:</b> %ld second%s.</i>\n",
		    stoptime, (stoptime == 1)?"":"s");
	  else if (lang == FRENCH)
	    fprintf(outf,
		    "<br><b>Temps d'ex&eacute;cution :</b> %ld seconde%s.</i>\n",
		    stoptime, (stoptime == 1)?"":"s");
	  else if (lang == GERMAN)
	    fprintf(outf, "<br><b>Laufzeit:</b> %ld Sekunde%s.</i>\n",
                    stoptime, (stoptime == 1)?"":"n");
	  else if (lang == SPANISH)
	    fprintf(outf, "<br><b>Tiempo de ejecuci&oacute;n:</b> %ld segundo%s.</i>\n",
                    stoptime, (stoptime == 1)?"":"s");
	  else if (lang == DANISH)
	    fprintf(outf, "<br><b>Analysetid:</b> %ld sekund%s.</i>\n",
		    stoptime, (stoptime == 1)?"":"er");
          else /* lang == ITALIAN */
            fprintf(outf,
		    "<br><b>Tempo di esecuzione:</b> %ld second%s.</i>\n",
                    stoptime, (stoptime == 1)?"o":"i");
	}
	else if (lang == ENGLISH)
	  fprintf(outf, "Running time: %ld second%s.\n",
		  stoptime, (stoptime == 1)?"":"s");
	else if (lang == FRENCH)
	  fprintf(outf, "Temps d'execution : %ld seconde%s.\n",
		  stoptime, (stoptime == 1)?"":"s");
	else if (lang == GERMAN)
	  fprintf(outf, "Laufzeit: %ld Sekunde%s.\n",
		  stoptime, (stoptime == 1)?"":"n");
	else if (lang == SPANISH)
	  fprintf(outf, "Tiempo de ejecucion: %ld segundo%s.\n",
		  stoptime, (stoptime == 1)?"":"s");
	else if (lang == DANISH)
	  fprintf(outf, "Analysetid: %ld sekund%s.\n",
		  stoptime, (stoptime == 1)?"":"er");
        else /* lang == ITALIAN */
          fprintf(outf, "Tempo di esecuzione: %ld second%s.</i>\n",
                  stoptime, (stoptime == 1)?"o":"i");                  
      }
  
      else {
	if (aq == HTML) {
	  if (lang == ENGLISH)
	    fprintf(outf, "<br><b>Running time:</b> %ld minute%s, %ld second%s.</i>\n",
		    stoptime / 60, (stoptime / 60 == 1)?"":"s",
		    stoptime % 60, (stoptime % 60 == 1)?"":"s");
	  else if (lang == FRENCH)
	    fprintf(outf, "<br><b>Temps d'ex&eacute;cution :</b> %ld minute%s, %ld seconde%s.</i>\n",
		    stoptime / 60, (stoptime / 60 == 1)?"":"s",
		    stoptime % 60, (stoptime % 60 == 1)?"":"s");
	  else if (lang == GERMAN)
	    fprintf(outf, "<br><b>Laufzeit:</b> %ld Minute%s, %ld Sekunde%s.</i>\n",
                    stoptime / 60, (stoptime / 60 == 1)?"":"n",
                    stoptime % 60, (stoptime % 60 == 1)?"":"n");
	  else if (lang == SPANISH)
	    fprintf(outf, "<br><b>Tiempo de ejecuci&oacute;n:</b> %ld minuto%s, %ld segundo%s.</i>\n",
		    stoptime / 60, (stoptime / 60 == 1)?"":"s",
		    stoptime % 60, (stoptime % 60 == 1)?"":"n");
	  else if (lang == DANISH)
	    fprintf(outf, "<br><b>Analysetid:</b> %ld minut%s, %ld sekund%s.</i>\n",
		    stoptime / 60, (stoptime / 60 == 1)?"":"ter",
		    stoptime % 60, (stoptime % 60 == 1)?"":"er");
          else /* lang == ITALIAN */
            fprintf(outf, "<br><b>Tempo di esecuzione:</b> %ld minut%s, %ld second%s.</i>\n",
                    stoptime / 60, (stoptime / 60 == 1)?"o":"i",
                    stoptime % 60, (stoptime % 60 == 1)?"o":"i");
	}
	else if (lang == ENGLISH)
	  fprintf(outf, "Running time: %ld minute%s, %ld second%s.\n",
		  stoptime / 60, (stoptime / 60 == 1)?"":"s",
		  stoptime % 60, (stoptime % 60 == 1)?"":"s");
	else if (lang == FRENCH)
	  fprintf(outf, "Temps d'execution : %ld minute%s, %ld seconde%s.\n",
		  stoptime / 60, (stoptime / 60 == 1)?"":"s",
		  stoptime % 60, (stoptime % 60 == 1)?"":"s");
	else if (lang == GERMAN)
	  fprintf(outf, "Laufzeit: %ld Minute%s, %ld Sekunde%s.\n",
                  stoptime / 60, (stoptime / 60 == 1)?"":"n",
                  stoptime % 60, (stoptime % 60 == 1)?"":"n");
	else if (lang == SPANISH)
	  fprintf(outf, "Tiempo de ejecucion: %ld minuto%s, %ld segundo%s.\n",
                  stoptime / 60, (stoptime / 60 == 1)?"":"s",
                  stoptime % 60, (stoptime % 60 == 1)?"":"s");
	else if (lang == DANISH)
	  fprintf(outf, "Analysetid: %ld minut%s, %ld sekund%s.\n",
		  stoptime / 60, (stoptime / 60 == 1)?"":"ter",
		  stoptime % 60, (stoptime % 60 == 1)?"":"er");
        else /* lang == ITALIAN */
          fprintf(outf, "Tempo di esecuzione: %ld minut%s, %ld second%s.\n",
		  stoptime / 60, (stoptime / 60 == 1)?"o":"i",
		  stoptime % 60, (stoptime % 60 == 1)?"o":"i");
      }
  
      if (!aq && (mq || Wq || dq || Dq || hq || oq || Sq || iq || rq)) {
	gotos(outf, '\0');
      }
    }     /* end if aq != PREFORMATTED */

    /* Finally, insert footer file */

    if (!STREQ(footerfile, "none")) {
      tempf = fopenlog(footerfile, "footer file", &ispipe);
      if (tempf != NULL) {
	if (aq == HTML)
	  fprintf(outf, "<hr>\n");
	else if (aq == ASCII) {
	  for (i = 0; i < pagewidth; i++)
	    fprintf(outf, "-");
	  fprintf(outf, "\n\n");
	}
	fflush(stdout);

	while(fgets(templine, MAXLINELENGTH, tempf) != NULL)
	  fprintf(outf, "%s", templine);
	if (templine[(int)strlen(templine) - 1] != '\n')
	  fprintf(outf, "\n");
	fcloselog(tempf, footerfile, "footer file", ispipe);

	if (aq == HTML || aq == ASCII)
	  fprintf(outf, "\n");

	html2 = OFF;
      }
    }

    if (aq == HTML) {
      if (html2) {
	fprintf(outf,
		"<P> <A HREF=\"http://www.webtechs.com/html-val-svc/\">\n");
	fprintf(outf, "<IMG SRC=\"");
	htmlfprintf(outf, imagedir);
	fprintf(outf, "html2.gif\"\n");
	fprintf(outf, "ALT=\"HTML 2.0 Conformant!\"></A>\n");
      }
      fprintf(outf, "\n</body>\n</html>\n");
    }
  }      /* end if aq != CACHE */

  fclose(outf);
  if (debug > 0)
    fprintf(stderr, "F: Closing %s\n", STREQ(outfile, "-")?"stdout":outfile);

}
