/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** output3.c; output functions. ***/
/* See also output.c and output2.c */

#include "analhea2.h"

/*** The first function prints the "goto" line; links to all reports except
     possibly one (the one we're on). If called gotos('\0') won't omit one.
     If called gotos('z') will omit 'Top'. ***/

void gotos(FILE *outf, char c)
{
  extern char reportorder[];
  extern flag bq, Bq, cq, dq, Dq, eq, fq, hq, Hq, iq, tq, mq, oq, rq, Sq, Wq;
  extern flag xq;
  extern int lang;

  char *i;
  char langstr[MAXSTRINGLENGTH];

  if (xq) {  /* NB Have already tested aq == HTML */
    if (lang == ENGLISH)
      strcpy(langstr, "Go To");
    else if (lang == FRENCH)
      strcpy(langstr, "Autres choix");
    else if (lang == GERMAN)
      strcpy(langstr, "Andere Statistiken");
    else if (lang == SPANISH)
      strcpy(langstr, "Ir a");
    else if (lang == DANISH)
      strcpy(langstr, "G&aring; til");
    else /* lang == ITALIAN */
      strcpy(langstr, "Altre statistiche");
    fprintf(outf, "\n\n<p>(<b>%s</b>", langstr);

    if (c != 'z') {
      if (lang == ENGLISH)
	strcpy(langstr, "Top");
      else if (lang == FRENCH)
	strcpy(langstr, "Sommet");
      else if (lang == GERMAN)
	strcpy(langstr, "Anfang");
      else if (lang == SPANISH)
	strcpy(langstr, "Inicio");
      else if (lang == DANISH)
	strcpy(langstr, "Top");
      else /* lang == ITALIAN */
        strcpy(langstr, "Inizio");
      fprintf(outf, "%s: <a HREF=\"#Top\">%s</a>",
	      (lang == FRENCH)?" ":"", langstr);
    }

    for (i = reportorder; *i != '\0'; i++) {
      if (c != *i) {   /* o/wise we don't want this one */
	switch(*i) {
	case 'b':
	  if (bq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Browser summary");
	    else if (lang == FRENCH)
	      strcpy(langstr, "R&eacute;sum&eacute; des navigateurs");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Browser-&Uuml;bersicht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Resumen de <i>Browsers</i>");
	      /* NB Italics, as browser is not Spanish */
	    else if (lang == DANISH)
	      strcpy(langstr, "Browser oversigt");
            else /* lang == ITALIAN */
              strcpy(langstr, "Sommario browser");
	    fprintf(outf, "%s: <a HREF=\"#Browser\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'B':
	  if (Bq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Browser report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport des navigateurs");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Browser-Bericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe de <i>Browsers</i>");
	    else if (lang == DANISH)
	      strcpy(langstr, "Browser rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto browser");
	    fprintf(outf, "%s: <a HREF=\"#FullBrowser\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'c':
	  if (cq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Status code report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport des statuts");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Statuscode-Bericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe de C&oacute;digos de Estado");
	    else if (lang == DANISH)
	      strcpy(langstr, "Statuskode rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto codici di stato");
	    fprintf(outf, "%s: <a HREF=\"#Status\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'd':
	  if (dq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Daily summary");
	    else if (lang == FRENCH)
	      strcpy(langstr, "R&eacute;sum&eacute; quotidien");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Tages&uuml;bersicht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Resumen Diario");
	    else if (lang == DANISH)
	      strcpy(langstr, "Daglig oversigt");
            else /* lang == ITALIAN */
              strcpy(langstr, "Sommario giorni della settimana");
	    fprintf(outf, "%s: <a HREF=\"#Daily\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'D':
	  if (Dq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Daily report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport quotidien");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Tagesbericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe Diario");
	    else if (lang == DANISH)
	      strcpy(langstr, "Daglig rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto giorni");
	    fprintf(outf, "%s: <a HREF=\"#FullDaily\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'e':
	  if (eq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Error report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport des erreurs");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Fehlerbericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe de Errores");
	    else if (lang == DANISH)
	      strcpy(langstr, "Fejlrapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto errori");
	    fprintf(outf, "%s: <a HREF=\"#Error\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'f':
	  if (fq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Referrer report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport des appelants");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Verweis-Bericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe de Remitentes");
	    else if (lang == DANISH)
	      strcpy(langstr, "Refererende adresserapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto provenienze");
	    fprintf(outf, "%s: <a HREF=\"#Referrer\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'H':
	  if (Hq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Hourly report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport horaire");
	    else if (lang == GERMAN)
	      strcpy(langstr, "St&uuml;ndlicher Bericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe Horario");
	    else if (lang == DANISH)
	      strcpy(langstr, "Time rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto ore");
	    fprintf(outf, "%s: <a HREF=\"#FullHourly\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'h':
	  if (hq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Hourly summary");
	    else if (lang == FRENCH)
	      strcpy(langstr, "R&eacute;sum&eacute; horaire");
	    else if (lang == GERMAN)
	      strcpy(langstr, "St&uuml;ndliche &Uuml;bersicht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Resumen Horario");
	    else if (lang == DANISH)
	      strcpy(langstr, "Time oversigt");
            else /* lang == ITALIAN */
              strcpy(langstr, "Sommario ore del giorno");
	    fprintf(outf, "%s: <a HREF=\"#Hourly\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'i':
	  if (iq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Directory report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport des r&eacute;pertoires");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Verzeichnisbericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe de Directorios");
	    else if (lang == DANISH)
	      strcpy(langstr, "Directory rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto direttori");
	    fprintf(outf, "%s: <a HREF=\"#Directory\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'm':
	  if (mq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Monthly report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport mensuel");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Monatsbericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe Mensual");
	    else if (lang == DANISH)
	      strcpy(langstr, "M&aring;nedlig rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto mesi");
	    fprintf(outf, "%s: <a HREF=\"#Monthly\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'o':
	  if (oq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Domain report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport des domaines");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Dom&auml;nen-Bericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe de Dominios");
	    else if (lang == DANISH)
	      strcpy(langstr, "Dom&aelig;ne rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto domini");
	    fprintf(outf, "%s: <a HREF=\"#Domain\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'r':
	  if (rq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Request report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport des requ&ecirc;tes");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Anfrage-Bericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe de Peticiones");
	    else if (lang == DANISH)
	      strcpy(langstr, "Foresp&oslash;rgselsrapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto richieste");
	    fprintf(outf, "%s: <a HREF=\"#Request\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'S':
	  if (Sq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Host report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport des h&ocirc;tes");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Host-Bericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe de <i>Hosts</i>");
	    else if (lang == DANISH)
	      strcpy(langstr, "Host rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto host");
	    fprintf(outf, "%s: <a HREF=\"#Host\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 't':
	  if (tq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "File type report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport des types de fichier");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Dateityp-Bericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe de Tipos de Fichero");
	    else if (lang == DANISH)
	      strcpy(langstr, "Filtype rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto tipi di file");
	    fprintf(outf, "%s: <a HREF=\"#FileType\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;
	case 'W':
	  if (Wq) {
	    if (lang == ENGLISH)
	      strcpy(langstr, "Weekly report");
	    else if (lang == FRENCH)
	      strcpy(langstr, "Rapport hebdomadaire");
	    else if (lang == GERMAN)
	      strcpy(langstr, "Wochenbericht");
	    else if (lang == SPANISH)
	      strcpy(langstr, "Informe Semanal");
	    else if (lang == DANISH)
	      strcpy(langstr, "Ugentlig rapport");
            else /* lang == ITALIAN */
              strcpy(langstr, "Resoconto settimane");
	    fprintf(outf, "%s: <a HREF=\"#Weekly\">%s</a>",
		    (lang == FRENCH)?" ":"", langstr);
	  }
	  break;

	}   /* end switch */
      }     /* end if this i wanted */
    }       /* end for i */

    fprintf(outf, ")\n");

  }         /* end if xq */
}           /* end function gotos() */

/*** Next, to print strings with HTML reserved characters translated ***/

void htmlputc(char c, FILE *outf)
{
  if (c == '<')
    fprintf(outf, "&lt;");
  else if (c == '>')
    fprintf(outf, "&gt;");
  else if (c == '&')
    fprintf(outf, "&amp;");
  else if (c == '"')
    fprintf(outf, "&quot;");
  else
    putc(c, outf);
}

void htmlfprintf(FILE *outf, char string[MAXSTRINGLENGTH])
{
  extern flag html2;

  char *c;

  for (c = string; *c != '\0'; c++) {
    if (*c == '\\' && *(c + 1) != '\0') {
      html2 = OFF;
      putc(*(++c), outf);
    }
    else
      htmlputc(*c, outf);
  }
}

/*** Now a little routine to find the correct divider for large numbers of
     bytes. Also sets bprefix[0] as a side effect. ***/

double finddivider(double bytes, char *bprefix)
{
  extern flag rawbytes;

  double bdivider;

  if (rawbytes)
    bdivider = 1.0;
  else
    for (bdivider = 1; bytes / bdivider >= 999999.5;
	 bdivider *= 1024)
      ;  /* run bdivider to right multiplier */

  if (bdivider == 1.0)
    *bprefix = '\0';
  else if (bdivider == 1024.0)
    *bprefix = 'k';
  else if (bdivider == 1048576.0)
    *bprefix = 'M';
  else if (bdivider == 1073741824.0)
    *bprefix = 'G';
  else if (bdivider == 1099511627776.0)
    *bprefix = 'T';
  else       /* 10^6 terabytes should be enough. Just about. */
    *bprefix = '?';

  return(bdivider);
}

/*** print a line across the page, assuming ASCII mode ***/

void asciiline(FILE *outf)
{
  extern int pagewidth;

  int i;

  for (i = 0; i < pagewidth; i++)
    fprintf(outf, "-");
  fprintf(outf, "\n\n");
}

/*** a barchart bar, length n, within <pre><tt> ***/

void barplot(FILE *outf, int n)
{
  extern int aq;
  extern flag graphical;
  extern char *imagedir;
  extern char markchar;

  int i, k;
  flag first = TRUE;

  if (aq || !graphical) {
    for ( ; n > 0; n--)
      fprintf(outf, "%c", markchar);
  }

  else {
    for (k = 32; k >= 1; k /= 2) {
      while (n >= k) {
	fprintf(outf, "<img src=\"");
	htmlfprintf(outf, imagedir);
	fprintf(outf, "bar%d.gif\" alt=\"", k);
	if (first) {
	  for (i = n; i > 0; i--)
	    htmlputc(markchar, outf);
	  first = FALSE;
	}
	fprintf(outf, "\">");
	n -= k;
      }
    }
  }
}

/*** Left hand two columns in OUTPUT PREFORMATTED ***/
/* NB: Check (aq == PREFORMATTED) elsewhere */

void precols(FILE *outf, char *wantcols, char codeletter, flag byq, flag pageq)
{
  extern char *presep;

  char *cols;

  fprintf(outf, "%c%s", codeletter, presep);
  for (cols = wantcols; *cols != '\0'; cols++) {
    switch(*cols) {
    case 'R':
    case 'r': 
      fprintf(outf, "%c", *cols);
      break;
    case 'P':
    case 'p':
      if (pageq)
	fprintf(outf, "%c", *cols);
      break;
    case 'b':
    case 'B':
      if (byq)
	fprintf(outf, "%c", *cols);
      break;
    }
  }
  fprintf(outf, "%s", presep);
}

/*** Two functions to print R,r,P,p,B,b cols given various parameters ***/

void printcolheads(FILE *outf, char *wantcols, int fieldwidth, int pfieldwidth,
		   int bfieldwidth, char bprefix[2], char name[20], char type,
		   flag byq, flag pageq, flag name1st)
{   /* assume aq != PREFORMATTED already tested */
  extern int lang;

  char *cols;
  char langstr[MAXSTRINGLENGTH], langstr2[MAXSTRINGLENGTH];
  int i;
  char *tempc;
  flag tempflag;

  if (name1st)
    fprintf(outf, "%s: ", name);
  for (cols = wantcols; *cols != '\0'; cols++) {
    switch(*cols) {
    case 'R':
      /* lang: number of requests and number of occurrences. 5 characters */
      if (lang == GERMAN) {
	strcpy(langstr, "#Anf.");
	strcpy(langstr2, " Anz.");
      }
      else if (lang == ITALIAN) {
        strcpy(langstr, "#rich");
        strcpy(langstr2, "#casi");
      }
      else if (lang == SPANISH) {
	strcpy(langstr, "N.pet");
	strcpy(langstr2, "N.ocu");
      }
      else if (lang == DANISH) {
	strcpy(langstr, "#henv");
	strcpy(langstr2, "#tilf");
      }
      else { /* lang == ENGLISH or FRENCH */
	strcpy(langstr, "#reqs");
	strcpy(langstr2, "#occs");
      }
      for (i = 5; i < fieldwidth; i++)
	fprintf(outf, " ");
      if (type == 'o')
	fprintf(outf, " %s : ", langstr);
      else if (type == 'c' || type == 'e')
	fprintf(outf, "%s: ", langstr2);
      else
	fprintf(outf, "%s: ", langstr);
      break;
    case 'r':
      /* lang: percentage requests and occurrences. 6 characters */
      if (lang == GERMAN) {
	strcpy(langstr, " %Anf.");
	strcpy(langstr2, " %Anz.");
      }
      else if (lang == ITALIAN) {
        strcpy(langstr, " %rich");
        strcpy(langstr2, " %casi");
      }
      else if (lang == SPANISH) {
	strcpy(langstr, "% pet");
	strcpy(langstr2, "% ocu");
      }
      else if (lang == DANISH) {
	strcpy(langstr, " %henv");
	strcpy(langstr2, " %tilf");
      }
      else { /* lang == ENGLISH or FRENCH */
	strcpy(langstr, " %reqs");
	strcpy(langstr2, " %occs");
      }
      if (type == 'o')
	fprintf(outf, " %s : ", langstr);
      else if (type == 'c' || type == 'e')
	fprintf(outf, "%s: ", langstr2);
      else
	fprintf(outf, "%s: ", langstr);
      break;
    case 'P':
      if (pageq) {
	/* lang: number of pages. 5 characters */
	if (lang == GERMAN)
	  strcpy(langstr, "Seit.");
	else if (lang == ITALIAN)
	  strcpy(langstr, "#pag.");
	else if (lang == SPANISH)
	  strcpy(langstr, " Pgs.");
	else if (lang == DANISH)
	  strcpy(langstr, "sider");
	else  /* lang == ENGLISH or FRENCH */
	  strcpy(langstr, "pages");
        
	for (i = 5; i < pfieldwidth; i++)
	  fprintf(outf, " ");
	if (type == 'o')
	  fprintf(outf, " %s : ", langstr);
	else
	  fprintf(outf, "%s: ", langstr);
      }
      break;
    case 'p':
      if (pageq) {
	/* lang: percentage pages. 6 characters */
	if (lang == GERMAN)
	  strcpy(langstr, "%Seit");
	else if (lang == ITALIAN)
	  strcpy(langstr, " %pag.");
	else if (lang == SPANISH)
	  strcpy(langstr, "% Pgs.");
	else if (lang == DANISH)
	  strcpy(langstr, "%sider");
	else  /* lang == ENGLISH or FRENCH */
	  strcpy(langstr, "%pages");
	if (type == 'o')
	  fprintf(outf, " %s : ", langstr);
	else
	  fprintf(outf, "%s: ", langstr);
      }
      break;
    case 'B':
      if (byq) {
	/* lang: bytes. 5 characters. Compensating space below if < 5. */
	if (lang == FRENCH)
	  strcpy(langstr, "octs");
        else if (lang == ITALIAN)
          strcpy(langstr, "byte");
	else if (lang == GERMAN && bprefix[0] == '\0')
	  strcpy(langstr, "Bytes");
	else /* other languages */
	  strcpy(langstr, "bytes");
	for (i = 6; i < bfieldwidth; i++)
	  fprintf(outf, " ");
	if (type == 'o')
	  fprintf(outf, " %s%s%s : ", 
                  ((lang == FRENCH) || (lang == ITALIAN))?" ":"",
		  (bprefix[0] == '\0')?" ":bprefix, langstr);
	else
	  fprintf(outf, "%s%s%s: ", 
                  ((lang == FRENCH) || (lang == ITALIAN))?" ":"",
		  (bprefix[0] == '\0')?" ":bprefix, langstr);
      }
      break;
    case 'b':
      if (byq) {
	if (lang == ENGLISH || lang == SPANISH || lang == DANISH)
	  strcpy(langstr, "%bytes");
	else if (lang == FRENCH)
	  strcpy(langstr, " %octs");
	else if (lang == GERMAN)
	  strcpy(langstr, "%Bytes");
        else /* lang == ITALIAN */
          strcpy(langstr, " %byte");
	if (type == 'o')
	  fprintf(outf, " %s : ", langstr);
	else
	  fprintf(outf, "%s: ", langstr);
      }
      break;
    }
  }
  if (!name1st)
    fprintf(outf, "%s", name);
  fprintf(outf, "\n");

  if (name1st) {
    tempflag = ON;
    for (tempc = name; *tempc != '\0'; tempc++) {
      if (*tempc == '&')
	tempflag = OFF;    /* only print one dash for e.g. &eacute; */
      else if (*tempc == ';')
	tempflag = ON;
      if (tempflag)
	fprintf(outf, "-");
    }
    fprintf(outf, "  ");
  }
  for (cols = wantcols; *cols != '\0'; cols++) {
    switch(*cols) {
    case 'R':
      for (i = 1; i <= fieldwidth + 2 * (type == 'o'); i++)
	fprintf(outf, "-");
      fprintf(outf, "  ");
      break;
    case 'r':
      fprintf(outf, "%s------  ", (type == 'o')?"--":"");
      break;
    case 'P':
      if (pageq) {
	for (i = 1; i <= pfieldwidth + 2 * (type == 'o'); i++)
	  fprintf(outf, "-");
	fprintf(outf, "  ");
      }
      break;
    case 'p':
      if (pageq)
	fprintf(outf, "%s------  ", (type == 'o')?"--":"");
      break;
    case 'B':
      if (byq) {
	for (i = 1; i <= bfieldwidth + 2 * (type == 'o'); i++)
	  fprintf(outf, "-");
	fprintf(outf, "  ");
      }
      break;
    case 'b':
      if (byq)
	fprintf(outf, "%s------  ", (type == 'o')?"--":"");
      break;
    }
  }
  if (!name1st) {
    tempflag = ON;
    for (tempc = name; *tempc != '\0'; tempc++) {
      if (*tempc == '&')
	tempflag = OFF;    /* only print one dash for e.g. &eacute; */
      else if (*tempc == ';')
	tempflag = ON;
      if (tempflag)
	fprintf(outf, "-");
    }
  }
  fprintf(outf, "\n");
}

void printcols(FILE *outf, char *wantcols, int reqs, int pages, double bytes,
	       int fieldwidth, int pfieldwidth, int bfieldwidth,
	       double bdivider, int totreqs, int totpages, double totbytes,
	       char type, flag byq, flag pageq)
{
  extern char *presep;
  extern char repsepchar, decpoint;
  extern flag aq;

  char *cols;
  double pc;
  int pc1, pc2;
  int k;

  for (cols = wantcols; *cols != '\0'; cols++) {
    switch(*cols) {
    case 'R':
      if (aq == PREFORMATTED)
	fprintf(outf, "%d%s", reqs, presep);
      else {
	if (type == 'o')
	  fprintf(outf, " ");
	else if (type == 'O')
	  fprintf(outf, "(");
	int3printf(outf, reqs, repsepchar, fieldwidth);
	if (type == 'o')
	  fprintf(outf, " : ");
	else if (type == 'O')
	  fprintf(outf, "): ");
	else
	  fprintf(outf, ": ");
      }
      break;
    case 'r':
      if (totreqs == 0)
	pc = 0;
      else
	pc = (reqs + 0.0) / ((totreqs + 0.0) / 10000);
      pc1 = ((int)(pc + 0.5)) / 100;     /* whole no. of %reqs */
      pc2 = ((int)(pc + 0.5)) % 100;     /* remaining 100ths. */
      if (aq != PREFORMATTED) {
	if (type == 'o')
	  fprintf(outf, " ");
	else if (type == 'O')
	  fprintf(outf, "(");
      }
      if (pc1 == 100) {
	if (aq == PREFORMATTED)
	  fprintf(outf, "100.00");
	else
	  fprintf(outf, "  100%%");
      }
      else if (pc1 > 0 || pc2 > 0) {
	if (aq == PREFORMATTED)
	  fprintf(outf, "%d.%02d", pc1, pc2);
	else
	  fprintf(outf, "%2d%c%02d%%", pc1, decpoint, pc2);
      }
      else if (aq == PREFORMATTED)
	fprintf(outf, "0.00");
      else
	fprintf(outf, "      ");
      if (aq == PREFORMATTED)
	fprintf(outf, "%s", presep);
      else if (type == 'o')
	fprintf(outf, " : ");
      else if (type == 'O')
	fprintf(outf, "): ");
      else
	fprintf(outf, ": ");
      break;
    case 'P':
      if (pageq) {
	if (aq == PREFORMATTED)
	  fprintf(outf, "%d%s", pages, presep);
	else {
	  if (type == 'o')
	    fprintf(outf, " ");
	  else if (type == 'O')
	    fprintf(outf, "(");
	  int3printf(outf, pages, repsepchar, pfieldwidth);
	  if (type == 'o')
	    fprintf(outf, " : ");
	  else if (type == 'O')
	    fprintf(outf, "): ");
	  else
	    fprintf(outf, ": ");
	}
      }
      break;
    case 'p':
      if (pageq) {
	if (totpages == 0)
	  pc = 0;
	else
	  pc = (pages + 0.0) / ((totpages + 0.0) / 10000);
	pc1 = ((int)(pc + 0.5)) / 100;
	pc2 = ((int)(pc + 0.5)) % 100;
	if (aq != PREFORMATTED) {
	  if (type == 'o')
	    fprintf(outf, " ");
	  else if (type == 'O')
	    fprintf(outf, "(");
	}
	if (pc1 == 100) {
	  if (aq == PREFORMATTED)
	    fprintf(outf, "100.00");
	  else
	    fprintf(outf, "  100%%");
	}
	else if (pc1 > 0 || pc2 > 0) {
	  if (aq == PREFORMATTED)
	    fprintf(outf, "%d.%02d", pc1, pc2);
	  else
	    fprintf(outf, "%2d%c%02d%%", pc1, decpoint, pc2);
	}
	else if (aq == PREFORMATTED)
	  fprintf(outf, "0.00");
	else
	  fprintf(outf, "      ");
	if (aq == PREFORMATTED)
	  fprintf(outf, "%s", presep);
	else if (type == 'o')
	  fprintf(outf, " : ");
	else if (type == 'O')
	  fprintf(outf, "): ");
	else
	  fprintf(outf, ": ");
      }
      break;
    case 'B':
      if (byq) {
	if (aq == PREFORMATTED)
	  fprintf(outf, "%.0f", bytes);
	else if (bytes / bdivider > 0.5) {
	  if (type == 'o')
	    fprintf(outf, " ");
	  else if (type == 'O')
	    fprintf(outf, "(");
	  double3printf(outf, ROUND(bytes / bdivider), repsepchar,
			bfieldwidth);
	  if (type == 'o')
	    fprintf(outf, " ");
	  else if (type == 'O')
	    fprintf(outf, ")");
	}
	else for (k = 0; k < bfieldwidth + 2 * (type == 'o' || type == 'O');
		  k++)
	  fprintf(outf, " ");
	fprintf(outf, "%s", (aq == PREFORMATTED)?presep:": ");
      }
      break;
    case 'b':
      if (byq) {
	if (totbytes < 0.5)
	  pc = 0;
	else
	  pc = bytes / (totbytes / 10000);
	pc1 = ((int)(pc + 0.5)) / 100;    /* whole no. of %bytes */
	pc2 = ((int)(pc + 0.5)) % 100;    /* remaining 100ths. */
	if (aq != PREFORMATTED) {
	  if (type == 'o')
	    fprintf(outf, " ");
	  else if (type == 'O')
	    fprintf(outf, "(");
	}
	if (pc1 == 100) {
	  if (aq == PREFORMATTED)
	    fprintf(outf, "100.00");
	  else
	    fprintf(outf, "  100%%");
	}
	else if (pc1 > 0 || pc2 > 0) {
	  if (aq == PREFORMATTED)
	    fprintf(outf, "%d.%02d", pc1, pc2);
	  else
	    fprintf(outf, "%2d%c%02d%%", pc1, decpoint, pc2);
	}
	else if (aq == PREFORMATTED)
	  fprintf(outf, "0.00");
	else
	  fprintf(outf, "      ");
	if (aq == PREFORMATTED)
	  fprintf(outf, "%s", presep);
	else if (type == 'o')
	  fprintf(outf, " : ");
	else if (type == 'O')
	  fprintf(outf, "): ");
	else
	  fprintf(outf, ": ");
      }
      break;
    }
  }
}


/*** A nasty header bit. Return rough floor -- accurate if negative. ***/
/* (NB: good enough to use total_bytes in place of total_brow_bytes etc. */
int whatincluded(FILE *outf, int sortby, char *minreqstr, char *minpagestr,
		 char *minbytestr, char singular[27], char plural[29],
		 flag subdoms, char gender)
{
  extern double total_bytes;
  extern int total_succ_reqs, total_page_reqs;
  extern int aq, lang;

  int genfloor;
  int tempint;
  char tempc;

  if (sortby == BYBYTES) {
    if (minbytestr[0] == '-') {
      genfloor = (int)bytefloor(total_bytes, minbytestr);
      if (genfloor == -1) {
	if (lang == ENGLISH)
	  fprintf(outf, "Printing the first %s", singular);
	else if (lang == FRENCH)
	  fprintf(outf, "Affichage d%s premi%s %s",
		  (gender == 'm')?"u":"e la",
		  (gender == 'm')?"er":((aq == HTML)?"&egrave;re":"ere"),
		  singular);
	else if (lang == GERMAN)
	  fprintf(outf, "Ausgabe de%s erste%s %s",
		  (gender == 'f')?"r":"s", (gender == 'f')?"r":"n",
		  singular);
	else if (lang == SPANISH)
	  fprintf(outf, "Mostrando %s primer%s %s",
		  (gender == 'm')?"el":"la",
		  (gender == 'm')?"":"a",
		  singular);
	else if (lang == DANISH)
	  fprintf(outf, "Udskriver det f&oslash;rste %s", singular);
	else /* lang == ITALIAN */
	  fprintf(outf, "Elenco del%s prim%s %s",
		  (gender == 'f')?"la":"", (gender == 'f')?"a":"o",
		  singular);
      }
      else {
	if (lang == ENGLISH)
	  fprintf(outf, "Printing the first %d %s", -genfloor, plural);
	else if (lang == FRENCH)
	  fprintf(outf, "Affichage des %d premi%ss %s", -genfloor,
		  (gender == 'm')?"er":((aq == HTML)?"&egrave;re":"ere"),
		  plural);
	else if (lang == GERMAN)
	  fprintf(outf, "Ausgabe der ersten %d %s", -genfloor, plural);
	else  if (lang == SPANISH)
	  fprintf(outf, "Mostrando l%ss %d primer%ss %s",
		  (gender == 'm')?"o":"a",
		  -genfloor, (gender == 'm')?"o":"a", plural);
	else if (lang == DANISH)
	  fprintf(outf, "Udskriver de f&oslash;rste %d %s", -genfloor, plural);
        else /* lang == ITALIAN */
          fprintf(outf, "Elenco de%s prim%s %d %s",
                 (gender == 'f')?"lle":"i", (gender == 'f')?"e":"i",
                 -genfloor, plural);
      }
    }  /* end if minbytestr[0] == '-' */
    else {
      if (lang == ENGLISH)
	fprintf(outf, "Printing all %s", plural);
      else if (lang == FRENCH)
	fprintf(outf, "Affichage de tou%ss les %s", (gender == 'f')?"te":"",
		plural);
      else if (lang == GERMAN)
	fprintf(outf, "Ausgabe aller %s", plural);
      else if (lang == SPANISH)
	fprintf(outf, "Mostrando tod%ss l%ss %s", (gender == 'f')?"a":"o",
		(gender == 'f')?"a":"o", plural);
      else if (lang == DANISH)
	fprintf(outf, "Udskriver alle %s", plural);
      else /* lang == ITALIAN */
        /* 'f'->le   'm'->i   'n'->gli  */
        fprintf(outf, "Elenco di tutt%s %s%s %s",
               (gender == 'f')?"e":"i",
               (gender == 'n')?"gl":"",
               (gender == 'f')?"le":"i",
               plural); 
      genfloor = (int)(ceil(bytefloor(total_bytes, minbytestr)));
      if (genfloor > 0) {
	if (lang == ENGLISH)
	  fprintf(outf, " with at least ");
	else if (lang == FRENCH)
	  fprintf(outf, " avec au moins ");
	else if (lang == GERMAN)
	  fprintf(outf, " mit mindestens ");
	else if (lang == SPANISH)
	  fprintf(outf, " con al menos ");
	else if (lang == DANISH)
	  fprintf(outf, " med mindst ");
        else /* lang == ITALIAN */
          fprintf(outf, " con almeno ");
	tempint = MAX((int)strlen(minbytestr) - 1, 0);
	if (minbytestr[tempint] == '%') {
	  minbytestr[tempint] = '\0';
	  doublefprintf(outf, atof(minbytestr));
	  if (lang == GERMAN)
	    fprintf(outf, "%% Anteil an gesendeten Daten");
          else if (lang == ITALIAN)
            fprintf(outf, "%% del traffico");
	  else if (lang == SPANISH)
	    fprintf(outf, "%% del tr%sfico", (aq == HTML)?"&aacute;":"a");
	  else if (lang == DANISH)
            fprintf(outf, "%% trafik");
	  else /* lang == FRENCH || ENGLISH */
	    fprintf(outf, "%% %s traffic", (lang == FRENCH)?"du":"of the");
	}
	else if (minbytestr[tempint] == 'k' || minbytestr[tempint] == 'M' ||
		 minbytestr[tempint] == 'G' || minbytestr[tempint] == 'T') {
	  tempc = minbytestr[tempint];
	  minbytestr[tempint] = '\0';
	  doublefprintf(outf, atof(minbytestr));
	  if (lang == ENGLISH)
	    fprintf(outf, " %cbytes of traffic", tempc);
	  else if (lang == FRENCH)
	    fprintf(outf, " %coctets du traffic", tempc);
	  else if (lang == GERMAN)
	    fprintf(outf, " %cbytes gesendeten Daten", tempc);
	  else if (lang == SPANISH)
	    fprintf(outf, " %cbytes de tr%sfico", tempc,
		    (aq == HTML)?"&aacute;":"a");
	  else if (lang == DANISH)
	    fprintf(outf, " %cbytes trafik", tempc);
          else /* lang == ITALIAN */
            fprintf(outf, " %cbyte di traffico", tempc);
	}
	else {
	  doublefprintf(outf, atof(minbytestr));
	  if (lang == ENGLISH)
	    fprintf(outf, " bytes of traffic");
	  else if (lang == FRENCH)
	    fprintf(outf, " octets du traffic");
	  else if (lang == GERMAN)
	    fprintf(outf, " Bytes gesendeten Daten");
	  else if (lang == SPANISH)
	    fprintf(outf, " bytes de tr%sfico", (aq == HTML)?"&aacute;":"a");
	  else if (lang == DANISH)
	    fprintf(outf, " bytes trafik");
          else /* lang == ITALIAN */
            fprintf(outf, " byte di traffico");
	}
      }
    }      /* end if minbytestr[0] != '-' */
    if (subdoms)
      fprintf(outf, ".\n");
    else if (lang == ENGLISH)
      fprintf(outf, ",%ssorted by amount of traffic.\n",
              (genfloor > 0)?"\n  ":" ");
    else if (lang == FRENCH)
      fprintf(outf, ",%stri%s%s%s par le taux de traffic.\n",
	      (genfloor > 0)?"\n  ":" ", (aq == HTML)?"&eacute;":"e",
	      (gender == 'f')?"e":"", (genfloor == -1)?"":"s");
    else if (lang == GERMAN)
      fprintf(outf, ",%ssortiert nach Menge gesendeter Daten.\n",
              (genfloor > 0)?"\n  ":" ");
    else if (lang == SPANISH)
      fprintf(outf, ",%sordenados por tasa de tr%sfico.\n",
              (genfloor > 0)?"\n  ":" ", (aq == HTML)?"&aacute;":"a");
    else if (lang == DANISH)
      fprintf(outf, ",%ssorteret efter m%sngde trafik.\n",
              (genfloor > 0)?"\n  ":" ", (aq == HTML)?"&aelig;":"ae");
    else /* lang == ITALIAN */
      fprintf(outf, ",%sin ordine di traffico.\n",
              (genfloor > 0)?"\n  ":" ");
  }        /* end if sortby BYBYTES */
  else if (sortby == BYPAGES) {
    genfloor = reqfloor(total_page_reqs, minpagestr);
    if (minpagestr[0] == '-') {
      if (genfloor == -1) {
	if (lang == ENGLISH)
	  fprintf(outf, "Printing the first %s", singular);
	else if (lang == FRENCH)
	  fprintf(outf, "Affichage d%s premi%s %s",
		  (gender == 'm')?"u":"e la",
		  (gender == 'm')?"er":((aq == HTML)?"&egrave;re":"ere"),
		  singular);
	else if (lang == GERMAN)
	  fprintf(outf, "Ausgabe de%s erste%s %s",
		  (gender == 'f')?"r":"s", (gender == 'f')?"r":"n",
		  singular);
	else if (lang == SPANISH)
	  fprintf(outf, "Mostrando %s primer%s %s",
		  (gender == 'm')?"el":"la",
		  (gender == 'm')?"":"a",
		  singular);
	else if (lang == DANISH)
	  fprintf(outf, "Udskriver den f%srste %s",
		  (aq == HTML)?"&oslash;":"oe", singular);
        else /* lang == ITALIAN */
          fprintf(outf, "Elenco del%s prim%s %s",
                  (gender == 'f')?"la":"", (gender == 'f')?"a":"o",
                  singular);
      }
      else {
	if (lang == ENGLISH)
	  fprintf(outf, "Printing the first %d %s", -genfloor, plural);
	else if (lang == FRENCH)
	  fprintf(outf, "Affichage des %d premi%ss %s", -genfloor,
		  (gender == 'm')?"er":((aq == HTML)?"&egrave;re":"ere"),
		  plural);
	else if (lang == GERMAN)
	  fprintf(outf, "Ausgabe der ersten %d %s", -genfloor, plural);
	else if (lang == SPANISH)
	  fprintf(outf, "Mostrando l%ss %d prirmer%ss %s",
		  (gender == 'm')?"o":"a",  -genfloor, (gender == 'm')?"o":"a",
		  plural);
	else if (lang == DANISH)
	  fprintf(outf, "Udskriver de f%srste %d %s",
		  (aq == HTML)?"&oslash;":"oe", -genfloor, plural);
        else /* lang == ITALIAN */
          fprintf(outf, "Elenco de%s prim%s %d %s",
		  (gender == 'f')?"lle":"i", (gender == 'f')?"e":"i",
		  -genfloor, plural);
      }
    }    /* end if minpagestr[0] == '-' */
    else {
      if (lang == ENGLISH)
	fprintf(outf, "Printing all %s", plural);
      else if (lang == FRENCH)
	fprintf(outf, "Affichage de tou%ss les %s", (gender == 'f')?"te":"",
		plural);
      else if (lang == GERMAN)
	fprintf(outf, "Ausgabe aller %s", plural);
      else if (lang == SPANISH)
	fprintf(outf, "Mostrando tod%ss l%ss %s", (gender == 'f')?"a":"o",
		(gender == 'f')?"a":"o", plural);
      else if (lang == DANISH)
	fprintf(outf, "Udskriver alle %s", plural);
      else /* lang == ITALIAN */
        /* 'f'->le   'm'->i   'n'->gli  */
        fprintf(outf, "Elenco di tutt%s %s%s %s",
               (gender == 'f')?"e":"i",
               (gender == 'n')?"gl":"",
               (gender == 'f')?"le":"i",
               plural); 
      if (genfloor > 0) {
	if (lang == ENGLISH)
	  fprintf(outf, " with at least ");
	else if (lang == FRENCH)
	  fprintf(outf, " avec au moins ");
	else if (lang == GERMAN)
	  fprintf(outf, " mit mindestens ");
	else if (lang == SPANISH)
	  fprintf(outf, " con al menos ");
	else if (lang == DANISH)
	  fprintf(outf, " med mindst ");
        else /* lang == ITALIAN */
          fprintf(outf, " con almeno ");
	tempint = MAX((int)strlen(minpagestr) - 1, 0);
	if (minpagestr[tempint] == '%') {
	  minpagestr[tempint] = '\0';
	  doublefprintf(outf, atof(minpagestr));
	  if (lang == ENGLISH)
	    fprintf(outf, "%% of the requests for pages");
	  else if (lang == FRENCH)
	    fprintf(outf, "%% des requ%stes pour pages",
		    (aq == HTML)?"&ecirc;":"e");
	  else if (lang == GERMAN)
	    fprintf(outf, "%% Anteil an Seiten-Anfragen");
	  else if (lang == SPANISH)
	    fprintf(outf, "%% de las peticiones de p%sginas",
		    (aq == HTML)?"&aacute;":"a");
	  else if (lang == DANISH)
	    fprintf(outf, "%% sideforesp%srgsler",
		    (aq == HTML)?"&oslash;":"oe");
          else /* lang == ITALIAN */
            fprintf(outf, "%% delle richieste di pagine");
	}
	else if (lang == ENGLISH)
	  fprintf(outf, "%d request%s", atoi(minpagestr),
		  (atoi(minpagestr) == 1)?" for a page":"s for pages");
	else if (lang == FRENCH)
	  fprintf(outf, "%d requ%ste%s", atoi(minpagestr),
		  (aq == HTML)?"&ecirc;":"e",
		  (atoi(minpagestr) == 1)?" sur une page":"s sur des pages");
	else if (lang == GERMAN)
	  fprintf(outf, "%d Seiten-Anfrage%s", atoi(minreqstr),
                  (atoi(minpagestr) == 1)?"":"n");
	else  if (lang == SPANISH)
	  fprintf(outf, "%d petici%s", atoi(minreqstr),
                  (atoi(minpagestr) == 1)?((aq == HTML)?"&oacute;n de una p&aacutegina":"on de una pagina"):
		  ((aq == HTML)?"ones de p&aacute;ginas":"ones de paginas"));
	else if (lang == DANISH)
	  fprintf(outf, "%d sideforesp%srgs%s", atoi(minpagestr),
		  (aq == HTML)?"&oslash":"oe",
		  (atoi(minpagestr) == 1)?"el":"ler");
        else /* lang == ITALIAN */
          fprintf(outf, "%d richiest%s", atoi(minpagestr),
                  (atoi(minpagestr) == 1)?"a di pagina":"e di pagine");
      }
    }      /* end if minpagestr[0] != '-' */
    if (subdoms)
      fprintf(outf, ".\n");
    else if (lang == ENGLISH)
      fprintf(outf, ",%ssorted by number of page requests.\n",
	      (genfloor > 0)?"\n  ":" ");
    else if (lang == FRENCH)
      fprintf(outf, ",\n  tri%s%s%s par le nombre de requ%stes sur des pages.\n",
	      (aq == HTML)?"&eacute;":"e", (gender == 'f')?"e":"",
	      (genfloor == -1)?"":"s", (aq == HTML)?"&ecirc;":"e");
    else if (lang == GERMAN)
      fprintf(outf, ",\n  sortiert nach Anzahl der Seiten-Anfragen.\n");
    else if (lang == SPANISH)
      fprintf(outf, ",%sordenad%s%s por el n%smero de peticiones.\n",
	      (genfloor > 0)?"\n  ":" ", (gender == 'f')?"a":"o",
	      (genfloor == -1)?"":"s", (aq == HTML)?"&uacute;":"u");
    else if (lang == DANISH)
      fprintf(outf, ",%ssorteret efter antal sideforesp%srgsler.\n",
	      (genfloor > 0)?"\n  ":" ", (aq == HTML)?"&oslash;":"oe");
    else /* lang == ITALIAN */
      fprintf(outf, ",\n  in ordine di numero di pagine richieste.\n");
  }   /* end if sortby BYPAGES */
  else {   /* sortby not BYBYTES or BYPAGES */
    genfloor = reqfloor(total_succ_reqs, minreqstr);
    if (minreqstr[0] == '-') {
      if (genfloor == -1) {
	if (lang == ENGLISH)
	  fprintf(outf, "Printing the first %s", singular);
	else if (lang == FRENCH)
	  fprintf(outf, "Affichage d%s premi%s %s",
		  (gender == 'm')?"u":"e la",
		  (gender == 'm')?"er":((aq == HTML)?"&egrave;re":"ere"),
		  singular);
	else if (lang == GERMAN)
	  fprintf(outf, "Ausgabe de%s erste%s %s",
		  (gender == 'f')?"r":"s", (gender == 'f')?"r":"n",
		  singular);
	else if (lang == SPANISH)
	  fprintf(outf, "Mostrando %s primer%s %s",
		  (gender == 'm')?"el":"la",
		  (gender == 'm')?"":"a",
		  singular);
	else if (lang == DANISH)
	  fprintf(outf, "Udskriver den f%srste %s",
		  (aq == HTML)?"&oslash;":"oe", singular);
        else /* lang == ITALIAN */
          fprintf(outf, "Elenco del%s prim%s %s",
                  (gender == 'f')?"la":"", (gender == 'f')?"a":"o",
                  singular);
      }
      else {
	if (lang == ENGLISH)
	  fprintf(outf, "Printing the first %d %s", -genfloor, plural);
	else if (lang == FRENCH)
	  fprintf(outf, "Affichage des %d premi%ss %s", -genfloor,
		  (gender == 'm')?"er":((aq == HTML)?"&egrave;re":"ere"),
		  plural);
	else if (lang == GERMAN)
	  fprintf(outf, "Ausgabe der ersten %d %s", -genfloor, plural);
	else if (lang == SPANISH)
	  fprintf(outf, "Mostrando l%ss %d primer%ss %s",
		  (gender == 'm')?"o":"a", -genfloor,
		  (gender == 'm')?"o":"a", plural);
	else if (lang == DANISH)
	  fprintf(outf, "Udskriver de f%srste %d %s",
		  (aq == HTML)?"&oslash;":"oe", -genfloor, plural);
        else /* lang == ITALIAN */
          fprintf(outf, "Elenco de%s prim%s %d %s",
                 (gender == 'f')?"lle":"i", (gender == 'f')?"e":"i",
                 -genfloor, plural);
     }
    }    /* end if minreqstr[0] == '-' */
    else {
      if (lang == ENGLISH)
	fprintf(outf, "Printing all %s", plural);
      else if (lang == FRENCH)
	fprintf(outf, "Affichage de tou%ss les %s", (gender == 'f')?"te":"",
		plural);
      else if (lang == GERMAN)
	fprintf(outf, "Ausgabe aller %s", plural);
      else if (lang == SPANISH)
	fprintf(outf, "Mostrando tod%ss l%ss %s", (gender == 'f')?"a":"o",
		(gender == 'f')?"a":"o", plural);
      else if (lang == DANISH)
	fprintf(outf, "Udskriver alle %s", plural);
      else /* lang == ITALIAN */
        /* 'f'->le   'm'->i   'n'->gli  */
        fprintf(outf, "Elenco di tutt%s %s%s %s",
               (gender == 'f')?"e":"i",
               (gender == 'n')?"gl":"",
               (gender == 'f')?"le":"i",
               plural); 
       
      if (genfloor > 0) {
	if (lang == ENGLISH)
	  fprintf(outf, " with at least ");
	else if (lang == FRENCH)
	  fprintf(outf, " avec au moins ");
	else if (lang == GERMAN)
	  fprintf(outf, " mit mindestens ");
	else if (lang == SPANISH)
	  fprintf(outf, " con al menos ");
	else if (lang == DANISH)
	  fprintf(outf, " med mindst ");
        else /* lang == ITALIAN */
          fprintf(outf, " con almeno ");
	tempint = MAX((int)strlen(minreqstr) - 1, 0);
	if (minreqstr[tempint] == '%') {
	  minreqstr[tempint] = '\0';
	  doublefprintf(outf, atof(minreqstr));
	  if (lang == ENGLISH)
	    fprintf(outf, "%% of the requests");
	  else if (lang == FRENCH)
	    fprintf(outf, "%% des requ%stes", (aq == HTML)?"&ecirc;":"e");
	  else if (lang == GERMAN)
	    fprintf(outf, "%% Anteil an Anfragen");
	  else if (lang == SPANISH)
	    fprintf(outf, "%% de las peticiones");
	  else if (lang == DANISH)
	    fprintf(outf, "%% af foresp%srgslerne",
		    (aq == HTML)?"&oslash;":"oe");
          else /* lang == ITALIAN */
            fprintf(outf, "%% delle richieste");
	}
	else if (lang == ENGLISH)
	  fprintf(outf, "%d request%s", atoi(minreqstr),
		  (atoi(minreqstr) == 1)?"":"s");
	else if (lang == FRENCH)
	  fprintf(outf, "%d requ%ste%s", atoi(minreqstr),
		  (aq == HTML)?"&ecirc;":"e", (atoi(minreqstr) == 1)?"":"s");
	else if (lang == GERMAN)
	  fprintf(outf, "%d Anfrage%s", atoi(minreqstr),
                  (atoi(minreqstr) == 1)?"":"n");
	else if (lang == SPANISH)
	  fprintf(outf, "%d petici%s", atoi(minreqstr),
                  (atoi(minreqstr) == 1)?((aq == HTML)?"&oacute;n":"on"):"ones");
	else if (lang == DANISH)
	  fprintf(outf, "%d foresp%srgs%s", atoi(minreqstr),
		  (aq == HTML)?"&oslash;":"oe",
		  (atoi(minreqstr) == 1)?"el":"ler");
        else /* lang == ITALIAN */
          fprintf(outf, "%d richiest%s", atoi(minreqstr),
                  (atoi(minpagestr) == 1)?"a":"e");
      }
    }      /* end if minreqstr[0] != '-' */
    if (subdoms)
      fprintf(outf, ".\n");
    else if (lang == ENGLISH) {
      if (sortby == BYREQUESTS)
	fprintf(outf, ",%ssorted by number of requests.\n",
		(genfloor > 0)?"\n  ":" ");
      else if (sortby == ALPHABETICAL)
	fprintf(outf, ",%ssorted alphabetically.\n",
		(genfloor > 0)?"\n  ":" ");
      else
	fprintf(outf, ", unsorted.\n");
    }
    else if (lang == FRENCH) {
      if (sortby == BYREQUESTS)
	fprintf(outf, ",%stri%s%s%s par le nombre de requ%stes.\n",
		(genfloor > 0)?"\n  ":" ", (aq == HTML)?"&eacute;":"e",
		(gender == 'f')?"e":"", (genfloor == -1)?"":"s",
		(aq == HTML)?"&ecirc;":"e");
      else if (sortby == ALPHABETICAL)
	fprintf(outf, ",%stri%s%s%s par ordre alphab%stique.\n",
		(genfloor > 0)?"\n  ":" ", (aq == HTML)?"&eacute;":"e",
		(gender == 'f')?"e":"", (genfloor == -1)?"":"s",
		(aq == HTML)?"&eacute;":"e");
      else
	fprintf(outf, ", non tri%s%s%s.\n", (aq == HTML)?"&eacute;":"e",
		(gender == 'f')?"e":"", (genfloor == -1)?"":"s");
    }
    else if (lang == GERMAN) {
      if (sortby == BYREQUESTS)
        fprintf(outf, ",%ssortiert nach Anzahl der Anfragen.\n",
                (genfloor > 0)?"\n  ":" ");
      else if (sortby == ALPHABETICAL)
        fprintf(outf, ",%salphabetisch sortiert.\n",
                (genfloor > 0)?"\n  ":" ");
      else
        fprintf(outf, ", unsortiert.\n");
    }
    else if (lang == SPANISH) {
      if (sortby == BYREQUESTS)
        fprintf(outf, ",%sordenad%s%s por el n%smero de peticiones.\n",
                (genfloor > 0)?"\n  ":" ", (gender == 'f')?"a":"o",
		(genfloor == -1)?"":"s", (aq == HTML)?"&uacute;":"u");
      else if (sortby == ALPHABETICAL)
        fprintf(outf, ",%sen orden alfab%stico.\n",
                (genfloor > 0)?"\n  ":" ", (aq == HTML)?"&eacute;":"e");
      else
        fprintf(outf, ", desordenad%s%s.\n", (gender == 'f')?"a":"o",
		(genfloor == -1)?"":"s");
    }
    else if (lang == DANISH) {
      if (sortby == BYREQUESTS)
	fprintf(outf, ",%ssorteret efter antal foresp%srgsler.\n",
		(genfloor > 0)?"\n  ":" ", (aq == HTML)?"&oslash;":"oe");
      else if (sortby == ALPHABETICAL)
	fprintf(outf, ",%ssorteret alfabetisk.\n",
		(genfloor > 0)?"\n  ":" ");
      else
	fprintf(outf, ", usorteret.\n");
    }
    else {   /* lang == ITALIAN */
      if (sortby == BYREQUESTS)
        fprintf(outf, ",%sin ordine di numero di richieste.\n",
                (genfloor > 0)?"\n  ":" ");
      else if (sortby == ALPHABETICAL)
        fprintf(outf, ",%sin ordine alfabetico.\n",
                (genfloor > 0)?"\n  ":" ");
      else
        fprintf(outf, ", in ordine casuale.\n");
    }


  }   /* end if sortby not bybytes or bypages */

  return(genfloor);

}

/* The same without the printing (just to return the right value). Could
   undoubtedly be done more cleanly, with a bit more work. */

int whatincludednop(int sortby, char *minreqstr, char *minpagestr,
		    char *minbytestr)
{
  extern double total_bytes;
  extern int total_succ_reqs, total_page_reqs;

  int genfloor;

  if (sortby == BYBYTES) {
    if (minbytestr[0] == '-')
      genfloor = (int)bytefloor(total_bytes, minbytestr);
    else
      genfloor = (int)(ceil(bytefloor(total_bytes, minbytestr)));
  }
  else if (sortby == BYPAGES)
    genfloor = reqfloor(total_page_reqs, minpagestr);
  else
    genfloor = reqfloor(total_succ_reqs, minreqstr);

  return(genfloor);

}
