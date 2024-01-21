/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** formgen.c; generate a form for the analog form interface ***/

#include "analhea2.h"

void genopts(FILE *outf, char name[17], char plural[16], int sortby,
	     char codeletter)
{
  fprintf(outf, "\n<h3>%s options</h3>\n\n", name);
  fprintf(outf, "Sort the %s \n<select name=%cs size=1>\n", name, codeletter);
  fprintf(outf, "<option value=2%s>alphabetically (*)\n",
	  (sortby==ALPHABETICAL)?" selected":"");
  fprintf(outf, "<option value=1%s>by volume of traffic (+)\n",
	  (sortby==BYBYTES)?" selected":"");
  fprintf(outf, "<option value=0%s>by number of requests (*)\n",
	  (sortby==BYREQUESTS)?" selected":"");
  if (codeletter != 'B' && codeletter != 'b' && codeletter != 'f') {
    fprintf(outf, "<option value=4%s>by requests for pages (*)\n",
	    (sortby==BYPAGES)?" selected":"");
  }
  fprintf(outf, "</select>\n");
  fprintf(outf, "<br>(*) Include all %s with at least\n", plural);
  fprintf(outf,
	  "<input type=TEXT name=\"%ca\" maxlength=6 size=6> requests%s\n",
	  codeletter,
	  (codeletter != 'B' && codeletter != 'b' && codeletter != 'f')?"/page requests":"");
  fprintf(outf, "<b>or</b> the top ");
  fprintf(outf, "<input type=TEXT name=\"%cb\" maxlength=3 size=3> %s.\n",
	  codeletter, plural);
  fprintf(outf, "<br>(+) Include all %s with at least\n", plural);
  fprintf(outf, "<input type=TEXT name=\"%cc\" maxlength=6 size=6> bytes\n",
	  codeletter);
  fprintf(outf, "transferred <b>or</b> the top ");
  fprintf(outf, "<input type=TEXT name=\"%cd\" maxlength=3 size=3> %s.\n",
	  codeletter, plural);
}

void repchoice(FILE *outf, char name[MAXSTRINGLENGTH], char codeletter,
	       flag qq)
{
  fprintf(outf, "<BR><INPUT TYPE=radio NAME=%cq VALUE=1%s> [On] ",
	  codeletter, qq?" checked":"");
  fprintf(outf, "<INPUT TYPE=radio NAME=%cq VALUE=0%s> [Off] %s\n",
	  codeletter, qq?"":" checked", name);
}

void formgen(void)
{
  extern flag xq, mq, Wq, dq, Dq, hq, Hq, oq, Sq, iq, rq, tq, Bq, bq, fq, eq, cq;
  extern char reportorder[];
  extern int osortby, isortby, Ssortby, rsortby, fsortby, bsortby, Bsortby, tsortby;
  extern int dirlevel;
  extern char *hostname, *hosturl, *logourl, *commandname;
  extern char *outfile;
  extern struct timestruct fromtime, totime;

  char *ro;
  FILE *outf;

  if (STREQ(outfile, "stdout"))
    outf = stdout;

  else if ((outf = fopen(outfile, "w")) == NULL) {
    fprintf(stderr, "%s: Error: failed to open output file %s for writing.\n",
	    commandname, outfile);
    exit(ERR);  /* shouldn't get here because also tested at the beginning */
  }

  fprintf(outf, "<html><head>\n");
  fprintf(outf, "<title>Analog form interface</title>\n");
  fprintf(outf, "</head>\n\n");
  fprintf(outf, "<body>\n");
  if (hosturl[0] == '-')
    fprintf(outf, "<h1><img src=\"%s\" alt=\"\"> Analog form interface for %s</h1>\n",
	   logourl, hostname);
  else
    fprintf(outf, "<h1><img src=\"%s\" alt=\"\"> Analog form interface for <a HREF=\"%s\">%s</a></h1>\n",
	   logourl, hosturl, hostname);    
  fprintf(outf, "<form action=\""FORMPROG"\" method=\"GET\">\n");
  fprintf(outf, "<hr><i>You can set any options you like below. But they should all be set to\n");
  fprintf(outf, "sensible default values, so you can ignore any of them too.</i>\n");
  fprintf(outf, "<hr>\n<h2>1. Report choices</h2>\n");
  fprintf(outf, "<i>See <a href=\"http://www.statslab.cam.ac.uk/~sret1/analog/\">the analog\n");
  fprintf(outf, "home page</a> for the meanings of the various reports.</i>\n\n");
  fprintf(outf, "<p>\n<h3>Which reports do you want to see?</h3>\n");
  repchoice(outf, "General statistics", 'x', xq);
  
  for (ro = reportorder; *ro != '\0'; ro++) {
    switch(*ro) {
    case 'b':
      repchoice(outf, "Browser summary", 'b', bq);
      break;
    case 'B':
      repchoice(outf, "Browser report", 'B', Bq);
      break;
    case 'c':
      repchoice(outf, "Status code report", 'c', cq);
      break;
    case 'd':
      repchoice(outf, "Daily summary", 'd', dq);
      break;
    case 'D':
      repchoice(outf, "Daily report", 'D', Dq);
      break;
    case 'e':
      repchoice(outf, "Error report", 'e', eq);
      break;
    case 'f':
      repchoice(outf, "Referrer report", 'f', fq);
      break;
    case 'h':
      repchoice(outf, "Hourly summary", 'h', hq);
      break;
    case 'H':
      repchoice(outf, "Hourly report", 'H', Hq);
      break;
    case 'i':
      repchoice(outf, "Directory report", 'i', iq);
      break;
    case 'm':
      repchoice(outf, "Monthly report", 'm', mq);
      break;
    case 'o':
      repchoice(outf, "Domain report", 'o', oq);
      break;
    case 'r':
      repchoice(outf, "Request report", 'r', rq);
      break;
    case 'S':
      repchoice(outf, "Host report", 'S', Sq);
      break; 
    case 't':
      repchoice(outf, "File type report", 't', tq);
      break;
    case 'W':
      repchoice(outf, "Weekly report", 'W', Wq);
      break;
    }
  }

  fprintf(outf, "<p><i>You can now run the program: <input type=submit value=\"Produce statistics\">\n");
  fprintf(outf, "<br>Or you can fill in the options below for individual reports.\n");
  fprintf(outf, "You can fill in the </i>(+)<i> or </i>(*)<i> in each section according to which sort method you select.\n");
  fprintf(outf, "You can use <input type=TEXT name=\"vo\" maxlength=4 size=4 value=\"10M\"> bytes to mean 10 Megabytes etc.</i>\n");

  fprintf(outf, "<hr>\n<h2>2. Detailed report options</h2>\n");
  for (ro = reportorder; *ro != '\0'; ro++) {
    switch(*ro) {
    case 'b':
      genopts(outf, "Browser summary", "browsers", bsortby, 'b');
      break;
    case 'B':
      genopts(outf, "Browser report", "browsers", Bsortby, 'B');
      break;
    case 'f':
      genopts(outf, "Referrer report", "referring URLs", fsortby, 'f');
      break;
    case 'i':
      genopts(outf, "Directory report", "directories", isortby, 'i');
      fprintf(outf, "<br>Print directories to depth ");
      fprintf(outf, "<input type=text name=\"ie\" maxlength=1 size=1 value=%d>\n", dirlevel);
      break;
    case 'o':
      genopts(outf, "Domain report", "domains", osortby, 'o');
      break;
    case 'r':
      genopts(outf, "Request report", "requests", rsortby, 'r');
      fprintf(outf, "<br>Show\n");
      fprintf(outf, "<select name=\"rt\" size=1>\n");
      fprintf(outf, "<option value=\"f\" selected>all files\n");
      fprintf(outf, "<option value=\"p\">pages only\n");
      fprintf(outf, "</select>\n; include links to\n");
      fprintf(outf, "<select name=\"rl\" size=1>\n");
      fprintf(outf, "<option value=\"f\">all files\n");
      fprintf(outf, "<option value=\"p\" selected>pages only\n");
      fprintf(outf, "<option value=\"n\">no files\n");
      fprintf(outf, "</select>\n");
      break;
    case 'S':
      genopts(outf, "Host report", "hosts", Ssortby, 'S');
      break;
    case 't':
      genopts(outf, "File type report", "extensions", tsortby, 't');
      break;
    }
  }


  fprintf(outf, "<hr>\n\n<h2>3. Analysing only part of the logfile</h2>\n");
  fprintf(outf, "<b>Only certain dates</b>\n");
  fprintf(outf, "<br>You can analyse only the requests from certain dates.\n");
  fprintf(outf, "Enter the range of dates below in the from <i>yymmdd</i>;\n");
  fprintf(outf, "e.g., 950701 for 1st July 1995 (or fill in just one box\n");
  fprintf(outf, "to limit the range of dates on just one side).");
  fprintf(outf, "<br>From <input type=TEXT name=\"fr\" maxlength=10 size=6");
  if (fromtime.code > -INFINITY)
    fprintf(outf, " value=\"%02d%02d%02d\"", fromtime.year % 100,
	   fromtime.monthno + 1, fromtime.date);
  fprintf(outf, ">\nto <input type=TEXT name=\"to\" maxlength=10 size=6");
  if (totime.code < INFINITY)
    fprintf(outf, " value=\"%02d%02d%02d\"", totime.year % 100,
	   totime.monthno + 1, totime.date);
  fprintf(outf, ">\n\n");

  fprintf(outf, "<p><b>Only certain requested files</b>\n");
  fprintf(outf, "<br>Only look at the following requested files (list, separated by commas; can contain wild character *)\n");
  fprintf(outf, "<br><input type=TEXT name=\"fy\" size=60>\n");
  fprintf(outf, "<br>Ignore the following files\n");
  fprintf(outf, "<br><input type=TEXT name=\"fi\" size=60>\n");

  fprintf(outf, "<hr><h2>4. Layout</h2>\n");

  fprintf(outf, "<b>Your organisation's name</b> (for the title of the page)\n");
  fprintf(outf, "<input type=TEXT name=\"or\" value=\"%s\" size=60>\n",
	 hostname);

  fprintf(outf, "<p><b>Your organisation's home page</b> (leave blank for none)\n");
  fprintf(outf, "<br>URL: <input type=TEXT name=\"ho\" value=\"%s\" size=60>\n",
         hosturl);

  fprintf(outf, "<hr><input type=hidden name=\"TZ\" value=\"%s\">\n",
         getenv("TZ")!=NULL?getenv("TZ"):"");  /* gets it twice, but easy */
  fprintf(outf, "<input type=submit value=\"Produce statistics\"> ");
  fprintf(outf, "<input type=reset value=\"Reset form\">\n");
  fprintf(outf, "</form>\n");
  fprintf(outf, "<P> <A HREF=\"http://www.webtechs.com/html-val-svc/\">\n");
  fprintf(outf, "<IMG SRC=\"http://www.webtechs.com/html-val-svc/images/valid_html.gif\" ALT=\"HTML 2.0 Compliant!\"></A>\n");
  fprintf(outf, "</body></html>\n");

}
