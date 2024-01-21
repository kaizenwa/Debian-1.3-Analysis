/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** output2.c; output functions. ***/
/* See also output.c and output3.c */

#include "analhea2.h"

/*** Generic output function for generic objects ***/

void genout(FILE *outf, struct genstruct *sorthead, int tot_reqs,
	    int tot_pages, double tot_bytes, int sortby, char *minreqstr,
	    char *minpagestr, char *minbytestr, int max_reqs, int max_pages,
	    double max_bytes, char *wantcols, char anchor[10], char title[36],
	    char singular[22], char plural[24], char colhead[24], char gender,
	    char codeletter, flag alphahost, flag byq, flag pageq,
	    struct include *links, struct alias *aka,
	    char baseurl[MAXSTRINGLENGTH])
/* lang: note for making arguments to this function: singular and plural are
   passed to whatincluded(). colhead is often the same as singular, but is
   for the column heading. So in "including the first requested page", the
   singular is "requested page" and the colhead is just "page"; or in German,
   the singular is in the genitive, and the colhead in the nominative. */
{
  extern int pagewidth;
  extern int dirlevel;
  extern int Smaxlength;
  extern int aq, lang;
  extern flag rawbytes;
  extern char repsepchar;

  struct genstruct *p;
  int fieldwidth, pfieldwidth, bfieldwidth, graphwidth;
  int genfloor;
  double bdivider;
  char bprefix[2];
  char akaname[MAXSTRINGLENGTH];
  char *cols;
  int i, j, tempint;
  char *tempc;
  
  bprefix[0] = '\0';
  bprefix[1] = '\0';

  if (aq != PREFORMATTED) {

    if (!aq) {
      fprintf(outf, "\n\n<hr>\n<h2><a NAME=\"%s\">%s</a></h2>\n\n", anchor,
	      title);
      gotos(outf, codeletter);
      fprintf(outf, "<p>");
    }
    else {
      fprintf(outf, "%s\n", title);
      for (tempc = title; *tempc != '\0'; tempc++)
	fprintf(outf, "-");
      fprintf(outf, "\n");
    }
    
    genfloor = whatincluded(outf, sortby, minreqstr, minpagestr, minbytestr,
			    singular, plural, FALSE, gender);
    if (codeletter == 'i') {
      if (!aq)
	fprintf(outf, "<br>");
      if (lang == ENGLISH)
	fprintf(outf, "Printing directories to depth %d.\n", dirlevel);
      else if (lang == FRENCH)
	fprintf(outf,
		"Affichage des r%spertoires %s une profondeur de %d.\n",
		(aq == HTML)?"&eacute;":"e", (aq == HTML)?"&agrave;":"a",
		dirlevel);
      else if (lang == GERMAN)
	fprintf(outf, "Ausgabe der Verzeichnisse bis Tiefe %d.\n", dirlevel);
      else if (lang == SPANISH)
	fprintf(outf, "Mostrando directorios hasta el nivel %d.\n", dirlevel);
      else if (lang == DANISH)
	fprintf(outf, "Udskriver directories til niveau %d.\n", dirlevel);
      else /* lang == ITALIAN */
        fprintf(outf, "Sottodirettori riportati fino al livello %d di profondit%s.\n",
                dirlevel, (aq == HTML)?"&agrave;":"a");
   }

    if (aq)
      fprintf(outf, "\n");
    else
      fprintf(outf, "<pre>");

    tempint = 10000;
    for (fieldwidth = 5; max_reqs / tempint >= 10; fieldwidth++)
      tempint *= 10;
    if (repsepchar != '\0' && max_reqs >= 10000)
      fieldwidth = fieldwidth + ((fieldwidth - 1) / 3);

    if (pageq) {
      tempint = 10000;
      for (pfieldwidth = 5; max_pages / tempint >= 10; pfieldwidth++)
	tempint *= 10;
      if (repsepchar != '\0' && max_pages >= 10000)
	pfieldwidth = pfieldwidth + ((pfieldwidth - 1) / 3);
    }

    if (byq) {
      bdivider = finddivider(max_bytes, bprefix);
      if (rawbytes) {
	tempint = 100000;
	for (bfieldwidth = 6; max_bytes / tempint >= 10; bfieldwidth++)
	  tempint *= 10;
      }
      else
	bfieldwidth = 6;
      if (repsepchar != '\0' && max_bytes / bdivider >= 99999.5)
	bfieldwidth = bfieldwidth + ((bfieldwidth - 1) / 3);
    }

    printcolheads(outf, wantcols, fieldwidth, pfieldwidth, bfieldwidth,
		  bprefix, colhead, codeletter, byq, pageq, FALSE);

    if (alphahost) {
      graphwidth = pagewidth;
      for (cols = wantcols; *cols != '\0'; cols++) {
	switch(*cols) {
	case 'R':
	  graphwidth -= fieldwidth + 2;
	  break;
	case 'P':
	  graphwidth -= pfieldwidth + 2;
	  break;
	case 'B':
	  graphwidth -= bfieldwidth + 2;
	  break;
	case 'r':
	case 'p':
	case 'b':
	  graphwidth -= 8;
	  break;
	}
      }
      graphwidth = MIN(graphwidth, Smaxlength);
    }
  }

  else /* aq == PREFORMATTED */
    genfloor = whatincludednop(sortby, minreqstr, minpagestr, minbytestr);

  if (genfloor < 0)
    j = genfloor;
  else j = 1;

  for(p = sorthead; p -> name != NULL && (j++) != 0;
      p = p -> next) {

    if (aq == PREFORMATTED)
      precols(outf, wantcols, codeletter, byq, pageq);

    printcols(outf, wantcols, p -> reqs, p -> pages, p -> bytes, fieldwidth,
	      pfieldwidth, bfieldwidth, bdivider, tot_reqs, tot_pages,
	      tot_bytes, codeletter, byq, pageq);

    if (alphahost && !isdigit(p -> name[0])) {  /* we've swapped the names */
      reversehostname(p -> name);
      strcpy(akaname, p -> name);
      if (aka != NULL)
	doaliaslist(akaname, aka);
      /* Also in that case right align names */
      if (aq != PREFORMATTED) {
	for (i = graphwidth - (int)strlen(akaname); i > 0; i--)
	  fprintf(outf, " ");
      }
    }
    else {
      strcpy(akaname, p -> name);
      if (aka != NULL)
	doaliaslist(akaname, aka);
    }

    if (links != NULL && included(p -> name, p -> ispage, links)) {
      fprintf(outf, "<a HREF=\"");
      htmlfprintf(outf, baseurl);
      htmlfprintf(outf, p -> name);
      fprintf(outf, "\">");
      htmlfprintf(outf, akaname);
      fprintf(outf, "</a>");
    }
    else   /* (the usual case for most reports) */
      if (aq == HTML)
	htmlfprintf(outf, akaname);
      else
	fprintf(outf, "%s", akaname);
    fprintf(outf, "\n");

  }
      
  if (aq == ASCII)
    asciiline(outf);
  else if (aq == HTML)
    fprintf(outf, "</pre>");
	
}

/*** The domain report is similar to the generic ones. It differs in that
     the domains are stored in a different structure, and that subdomains
     must be printed. ***/

void domout(FILE *outf, int firstdom)
{
  extern struct domain **ohead;
  extern int aq, lang;
  extern flag byq, rawbytes;
  extern int osortby;
  extern char *ominbytestr, *ominpagestr, *ominreqstr;
  extern char *Ominbytestr, *Ominpagestr, *Ominreqstr;
  extern int omaxreqs, omaxpages;
  extern double omaxbytes;
  extern int Onumber;
  extern char ocols[];
  extern double total_bytes;
  extern int total_succ_reqs, total_page_reqs;
  extern char *presep;
  extern char repsepchar;

  int ofloor;

  struct domain *p;
  double bdivider;
  char bprefix[2];
  int fieldwidth, pfieldwidth, bfieldwidth;
  char langstr[MAXSTRINGLENGTH], langstr2[MAXSTRINGLENGTH];
  int i, j, tempint;
  char *tempp;

  bprefix[0] = '\0';
  bprefix[1] = '\0';

  if (aq != PREFORMATTED) {

    if (lang == ENGLISH) {
      strcpy(langstr, "Domain Report");
      strcpy(langstr2, "-------------");
    }
    else if (lang == FRENCH) {
      strcpy(langstr, "Rapport des Domaines");
      strcpy(langstr2, "--------------------");
    }
    else if (lang == SPANISH) {
      strcpy(langstr, "Informe de Dominios");
      strcpy(langstr2, "-------------------");
    }
    else if (lang == ITALIAN) {
      strcpy(langstr, "Resoconto domini");
      strcpy(langstr2, "----------------");
    }
    else if (lang == DANISH && aq == HTML) {
      strcpy(langstr, "Dom&aelig;ne rapport");
      strcpy(langstr2, "--------------");
    }
    else if (lang == DANISH) { /* not HTML */
      strcpy(langstr, "Domaene rapport");
      strcpy(langstr2, "--------------");
    }	
    else if (aq == HTML) { /* lang == GERMAN */
      strcpy(langstr, "Dom&auml;nen-Bericht");
      strcpy(langstr2, "---------------");
    }
    else { /* GERMAN and not HTML */
      strcpy(langstr, "Domaenen-Bericht");
      strcpy(langstr2, "----------------");
    }
    if (!aq) {
      fprintf(outf,
	      "\n\n<hr>\n<h2><a NAME=\"Domain\">%s</a></h2>\n\n", langstr);
      gotos(outf, 'o');
    }
    else
      fprintf(outf, "%s\n%s\n", langstr, langstr2);
  
    if (!aq)
      fprintf(outf, "<p>");

    if (lang == ENGLISH) {
      strcpy(langstr, "domain");
      strcpy(langstr2, "domains");
    }
    else if (lang == FRENCH) {
      strcpy(langstr, "domaine");
      strcpy(langstr2, "domaines");
    }
    else if (lang == SPANISH) {
      strcpy(langstr, "dominio");
      strcpy(langstr2, "dominios");
    }
    else if (lang == ITALIAN) {
      strcpy(langstr, "dominio");
      strcpy(langstr2, "domini");
    }
    else if (lang == DANISH && aq == HTML) {
      strcpy(langstr, "dom&aelig;ne");
      strcpy(langstr2, "dom&aelig;ner");
    }
    else if (lang == DANISH) { /* not HTML */
      strcpy(langstr, "domaene");
      strcpy(langstr2, "domaener");
    }
    else if (aq == HTML) { /* lang == GERMAN */
      strcpy(langstr, "Dom&auml;ne");
      strcpy(langstr2, "Dom&auml;nen");
    }
    else { /* GERMAN and not HTML */
      strcpy(langstr, "Domaene");
      strcpy(langstr2, "Domaenen");
    }

    ofloor = whatincluded(outf, osortby, ominreqstr, ominpagestr, ominbytestr,
			  langstr, langstr2, FALSE, 'm');
    if (Onumber > 0) {
      if (!aq)
	fprintf(outf, "<br>");
      if (lang == ENGLISH) {
	strcpy(langstr, "requested subdomain");
	strcpy(langstr2, "requested subdomains");
      }
      else if (lang == SPANISH) {
	strcpy(langstr, "subdominio solicitado");
	strcpy(langstr2, "subdominios solicitados");
      }
      else if (lang == ITALIAN) {
        strcpy(langstr, "sottodominio richiesto");
        strcpy(langstr2, "sottodomini richiesti");
      }
      else if (lang == FRENCH && aq == HTML) {
	strcpy(langstr, "sous-domaine demand&eacute;");
	strcpy(langstr2, "sous-domaines demand&eacute;s");
      }
      else if (lang == FRENCH) { /* and not HTML */
	strcpy(langstr, "sous-domaine demande");
	strcpy(langstr2, "sous-domaines demandes");
      }
      else if (lang == DANISH && aq == HTML) {
	strcpy(langstr, "forespurgte sub-dom&aelig;ne");
	strcpy(langstr2, "forespurgte sub-dom&aelig;ner");
      }
      else if (lang == DANISH) { /* not HTML */
	strcpy(langstr, "forespurgte sub-domaene");
	strcpy(langstr2, "forspurgte sub-domaener");
      }
      else if (aq == HTML) { /* lang == GERMAN */
	strcpy(langstr, "verlangter Unter-Dom&auml;ne");
	strcpy(langstr2, "verlangten Unter-Dom&auml;nen");
      }
      else { /* GERMAN and not HTML */
	strcpy(langstr, "verlangter Unter-Domaene");
	strcpy(langstr2, "verlangten Unter-Domaenen");
      }
      whatincluded(outf, osortby, Ominreqstr, Ominpagestr, Ominbytestr,
		   langstr, langstr2, TRUE, 'm');
    }

    if (aq)
      fprintf(outf, "\n");
    else
      fprintf(outf, "<pre>");
  
    tempint = 10000;
    for (fieldwidth = 5; omaxreqs / tempint >= 10; fieldwidth++)
      tempint *= 10;
    if (repsepchar != '\0' && omaxreqs >= 10000)
      fieldwidth = fieldwidth + ((fieldwidth - 1) / 3);

    tempint = 10000;
    for (pfieldwidth = 5; omaxpages / tempint >= 10; pfieldwidth++)
      tempint *= 10;
    if (repsepchar != '\0' && omaxpages >= 10000)
      pfieldwidth = pfieldwidth + ((pfieldwidth - 1) / 3);
  
    if (byq) {
      bdivider = finddivider(omaxbytes, bprefix);
      if (rawbytes) {
	tempint = 100000;
	for (bfieldwidth = 6; omaxbytes / tempint >= 10; bfieldwidth++)
	  tempint *= 10;
      }
      else
	bfieldwidth = 6;
      if (repsepchar != '\0' && omaxbytes / bdivider >= 99999.5)
	bfieldwidth = bfieldwidth + ((bfieldwidth - 1) / 3);
    }
  
    if (lang == ENGLISH)
      strcpy(langstr, "domain");
    else if (lang == FRENCH)
      strcpy(langstr, "domaine");
    else if (lang == ITALIAN || lang == SPANISH)
      strcpy(langstr, "dominio");
    else if (lang == DANISH && aq == HTML)
      strcpy(langstr, "dom&aelig;ne");
    else if (lang == DANISH) /* not HTML */
      strcpy(langstr, "domaene");
    else if (aq == HTML) /* and lang == GERMAN */
      strcpy(langstr, "Dom&auml;ne");
    else  /* GERMAN and not HTML */
      strcpy(langstr, "Domaene");

    printcolheads(outf, ocols, fieldwidth, pfieldwidth, bfieldwidth, bprefix,
		  langstr, (Onumber > 0)?'o':'\0', byq, ON, FALSE);

  }

  else /* aq == PREFORMATTED */
    ofloor = whatincludednop(osortby, ominreqstr, ominpagestr, ominbytestr);

  if (ofloor < 0)
    j = ofloor;    
  else j = 1;

  for (i = firstdom; i >= 0 && (j++) != 0; i = ohead[i] -> nexti) {

    if (!(i == DOMHASHSIZE - 2 && ohead[i] -> reqs == -1)) {

      if (aq == PREFORMATTED)
	precols(outf, ocols, 'o', byq, ON);

      printcols(outf, ocols, ohead[i] -> reqs, ohead[i] -> pages,
		ohead[i] -> bytes, fieldwidth, pfieldwidth, bfieldwidth,
		bdivider, total_succ_reqs, total_page_reqs, total_bytes,
		(Onumber > 0)?'o':'\0', byq, ON);
      
      if (ohead[i] -> id[0] == '*')
	/* flagged domains, not real domain names */
	fprintf(outf, "[%s]\n", ohead[i] -> name);
      else if (ohead[i] -> name[0] == '?')
	/* real domain, but don't print name */
	fprintf(outf, ".%s\n", ohead[i] -> id);
      else if (aq == PREFORMATTED)
	fprintf(outf, ".%s%s%s\n", ohead[i] -> id, presep, ohead[i] -> name);
      else if (aq == ASCII)
	fprintf(outf, ".%s (%s)\n", ohead[i] -> id, ohead[i] -> name);
      else {  /* aq == HTML */
	fprintf(outf, ".%s (", ohead[i] -> id);
	htmlfprintf(outf, ohead[i] -> name);
	fprintf(outf, ")\n");
      }
      
      /* Now print its subdomains too. */
      
      for (p = ohead[i] -> next; p -> name != NULL;
	   p = p -> next) {

	if (aq == PREFORMATTED)
	  precols(outf, ocols, 'O', byq, ON);

	printcols(outf, ocols, p -> reqs, p -> pages, p -> bytes, fieldwidth,
		  pfieldwidth, bfieldwidth, bdivider, total_succ_reqs,
		  total_page_reqs, total_bytes, 'O', byq, ON);

	if (aq != PREFORMATTED) {
	  tempp = p -> id;
	  while ((tempp = strchr(tempp, '.')) != NULL) {
	    fprintf(outf, "  "); 
	    /* print two spaces for each dot in name */
	    tempp++;
	  }
	  if (i == DOMHASHSIZE - 1)
	    fprintf(outf, "  ");  /* + 2 more for numerical domains */
	}
	
	fprintf(outf, "%s", p -> id);
	
	if (p -> name[0] != '?') {   /* print name */
	  if (aq == PREFORMATTED)
	    fprintf(outf,"%s%s", presep, p -> name);
	  else if (aq == ASCII)
	    fprintf(outf, " (%s)", p -> name);
	  else {
	    fprintf(outf, " (");
	    htmlfprintf(outf, p -> name);
	    fprintf(outf, ")");
	  }
	}
	
	fprintf(outf, "\n");
	
      }    /* end for domp */
	
    }

  }   /* end for (i = running over domains) */
    
  if (aq == ASCII)
    asciiline(outf);
  else if (aq == HTML)
    fprintf(outf, "</pre>");
	
}

/*** The date reports aren't quite generic enough to combine completely,
     but we can go a long way towards it. ***/
/*** First a function for printing out the headers of a report and finding
     the fieldwidths etc.; then one for printing out each individual line. ***/

void datehead(FILE *outf, int maxreq, int maxpages, double maxbytes,
	      char *wantcols, char *graphtype, char anchor[11],
	      char title[21], char htmltitle[31], char colhead[13],
	      char codeletter, int *unit, int *fieldwidth, int *pfieldwidth,
	      int *bfieldwidth, int *graphwidth, double *bdivider)
     /* NB: colhead: inc. leading spaces. */
     /* The last 5 args are returned altered */
{
  extern int aq, lang;
  extern flag byq, rawbytes, graphical;
  extern int pagewidth;
  extern char *imagedir;
  extern char markchar, sepchar, repsepchar;

  char *cols;
  char bprefix[2];
  char langstr[MAXSTRINGLENGTH];
  int i, j, tempint;
  char *tempc;

  bprefix[0] = '\0';
  bprefix[1] = '\0';

  if (*graphtype == 'b')
    *graphtype = 'B';
  if (*graphtype == 'p')
    *graphtype = 'P';

  if (!aq) {
    fprintf(outf, "<hr>\n<h2><a NAME=\"%s\">%s</a></h2>\n", anchor, htmltitle);
	  gotos(outf, codeletter);
  }
  else {
    fprintf(outf, "%s\n", title);
    for (tempc = title; *tempc != '\0'; tempc++)
      fprintf(outf, "-");
    fprintf(outf, "\n");
  }
    
  tempint = 10000;
  for (*fieldwidth = 5; maxreq / tempint >= 10; (*fieldwidth)++)
    tempint *= 10;   /* so fieldwidth is log_10(maxreq), but >= 5 */
  if (repsepchar != '\0' && maxreq >= 10000)
    *fieldwidth = *fieldwidth + ((*fieldwidth - 1) / 3);

  tempint = 10000;
  for (*pfieldwidth = 5; maxpages / tempint >= 10; (*pfieldwidth)++)
    tempint *= 10;
  if (repsepchar != '\0' && maxpages >= 10000)
    *pfieldwidth = *pfieldwidth + ((*pfieldwidth - 1) / 3);
	
  if (byq) {
    *bdivider = finddivider(maxbytes, bprefix);
    if (rawbytes || (*graphtype == 'B' && *unit > 0)) {
      tempint = 100000;
      for (*bfieldwidth = 6; maxbytes / tempint >= 10; (*bfieldwidth)++)
	tempint *= 10;
    }
    else
      *bfieldwidth = 6;
    if (repsepchar != '\0' && maxbytes / *bdivider >= 99999.5)
      *bfieldwidth = *bfieldwidth + ((*bfieldwidth - 1) / 3);
  }

  if (*unit <= 0) {   /* (o/wise just use the given amount) */

    /* Calculate the graphwidth */
    *graphwidth = pagewidth - (int)strlen(colhead) - 2;
    for (cols = wantcols; *cols != '\0'; cols++) {
      switch(*cols) {
      case 'R':
	*graphwidth -= *fieldwidth + 2;
	break;
      case 'P':
	*graphwidth -= *pfieldwidth + 2;
	break;
      case 'B':
	*graphwidth -= *bfieldwidth + 2;
	break;
      case 'r':
      case 'p':
      case 'b':
	*graphwidth -= 8;
	break;
      }
    }
    *graphwidth = MAX(*graphwidth, MINGRAPHWIDTH);  /* must be >= MGW wide */
	                                  
    if (*graphtype == 'B')
      *unit = (maxbytes - 1) / (*bdivider * *graphwidth);
    else if (*graphtype == 'P')
      *unit = (maxpages - 1) / *graphwidth;
    else   /* graphtype assumed to be 'R' */
      *unit = (maxreq - 1) / *graphwidth;
                   	        /* except we want a 'nice' amount, so ... */
	             /* (Nice amount is 1, 1.5, 2, 2.5, 3, 4, 5, 6, 8 * 10^n */

    j = 0;
    while (*unit > 24) {
      *unit /= 10;
      j++;
    }
    if (*unit == 6)
      *unit = 7;
    else if (*unit == 8)
      *unit = 9;
    else if (*unit >= 20)
      *unit = 24;
    else if (*unit >= 15)
      *unit = 19;
    else if (*unit >= 10)
      *unit = 14;
    (*unit)++;
    for (i = 0; i < j; i++) {
      *unit *= 10;
    }

  }     /* end if (*unit <= 0) */

  else if (*graphtype == 'B') {   /* o/wise unit doesn't make sense */
    *bdivider = 1;
    bprefix[0] = '\0';
  }

  if (!aq)
    fprintf(outf, "\n<p>");
  if (aq == HTML && graphical) {
    if (lang == ENGLISH)
      strcpy(langstr, "Each unit");
    else if (lang == FRENCH)
      strcpy(langstr, "Chaque unit&eacute;");
    else if (lang == GERMAN)
      strcpy(langstr, "Jede Einheit");
    else if (lang == SPANISH)
      strcpy(langstr, "Cada unidad");
    else if (lang == DANISH)
      strcpy(langstr, "Hver enhed");
    else /* lang == ITALIAN */
      strcpy(langstr, "Ogni unit&agrave;");
    fprintf(outf, "%s (<tt><img src=\"", langstr);
    htmlfprintf(outf, imagedir);
    fprintf(outf, "bar1.gif\" alt=\"");
    htmlputc(markchar, outf);
    if (lang == ENGLISH)
      strcpy(langstr, "represents");
    else if (lang == FRENCH)
      strcpy(langstr, "repr&eacute;sente");
    else if (lang == GERMAN)
      strcpy(langstr, "entspricht");
    else if (lang == SPANISH)
      strcpy(langstr, "representa");
    else if (lang == ITALIAN)
      strcpy(langstr, "rappresenta");
    else if (aq == HTML) /* DANISH */
      strcpy(langstr, "repr&aelig;senterer");
    else /* DANISH, not HTML */
      strcpy(langstr, "repraesenterer");

    fprintf(outf, "\"></tt>) %s ", langstr);
    int3printf(outf, *unit, sepchar, 0);
    if (*graphtype == 'B') {
      if (lang == ENGLISH)
	fprintf(outf, " %sbyte%s, or part thereof.\n\n", bprefix,
		(*unit == 1)?"":"s");
      else if (lang == FRENCH)
	fprintf(outf, " %soctet%s, ou une partie du moins.\n\n", bprefix,
		(*unit == 1)?"":"s");
      else if (lang == GERMAN)
	fprintf(outf, " %s%syte%s, oder einem Teil dessen.\n\n", bprefix,
                (bprefix[0] == '\0')?"B":"b", (*unit == 1)?"":"s");
      else if (lang == SPANISH)
	fprintf(outf, " %sbyte%s, o fracci&oacute;n.\n\n", bprefix,
		(*unit == 1)?"":"s");
      else if (lang == DANISH)
	fprintf(outf, " %sbyte%s, eller en del heraf.\n\n", bprefix,
		(*unit == 1)?"":"s");
      else /* lang == ITALIAN */
        fprintf(outf, " %sbyte, o frazione.\n\n", bprefix);
    }
    else if (*graphtype == 'P') {
      if (lang == ENGLISH)
	fprintf(outf, " request%s.\n\n",
		(*unit == 1)?" for a page":"s for pages, or part thereof");
      else if (lang == FRENCH)
	fprintf(outf, " requ&ecirc;te%s.\n\n",
		(*unit == 1)?" sur une page":
		"s sur des pages, ou une partie du moins");
      else if (lang == GERMAN)
	fprintf(outf, " Seiten-Anfrage%s.\n\n",
		(*unit == 1)?"":"n, oder einem Teil dessen");
      else if (lang == SPANISH)
	fprintf(outf, " petici%s.\n\n",
		(*unit == 1)?"&oacute;n de una p&aacute;gina":
		"ones de p&aacute;ginas, o fracci&oacute;n");
      else if (lang == DANISH)
	fprintf(outf, " sideforesp&oslash;rgs%s.\n\n",
		(*unit == 1)?"el":"ler, eller en del heraf");
      else /* lang == ITALIAN */
        fprintf(outf, " richiest%s.\n\n",
                (*unit == 1)?"a di pagina":
                "e di pagine, o frazione");
    }
    else if (lang == ENGLISH)
      fprintf(outf, " request%s.\n\n", (*unit == 1)?"":"s, or part thereof");
    else if (lang == FRENCH)
      fprintf(outf, " requ&ecirc;te%s.\n\n",
	      (*unit == 1)?"":"s, ou une partie du moins");
    else if (lang == GERMAN)
      fprintf(outf, " Anfrage%s.\n\n",
	      (*unit == 1)?"":"n, oder einem Teil dessen");
    else if (lang == SPANISH)
      fprintf(outf, " petici%s.\n\n",
	      (*unit == 1)?"&oacute;n":"ones, o fracci&oacute;n");
    else if (lang == DANISH)
      fprintf(outf, " foresp&oslash;rgs%s.\n\n", (*unit == 1)?"el":"ler, eller en del heraf");
    else /* lang == ITALIAN */
      fprintf(outf, " richiest%s.\n\n", (*unit == 1)?"a":"e, o frazione");
  }
  else {
    if (lang == ENGLISH)
      fprintf(outf, "\nEach unit (%c) represents ", markchar);
    else if (lang == FRENCH)
      fprintf(outf, "\nChaque unit%s (%c) repr%ssente ",
	      (aq == HTML)?"&eacute;":"e", markchar,
	      (aq == HTML)?"&eacute;":"e");
    else if (lang == GERMAN)
      fprintf(outf, "\nJede Einheit (%c) entspricht ", markchar);
    else if (lang == SPANISH)
      fprintf(outf, "\nCada unidad (%c) representa ", markchar);
    else if (lang == DANISH)
      fprintf(outf, "\nHver enhed (%c) repr%senterer ",
      		  markchar, (aq == HTML)?"&aelig;":"ae");
    else /* lang == ITALIAN */
      fprintf(outf, "\nOgni unit%s (%c) rappresenta ",
              (aq == HTML)?"&agrave;":"a", markchar);
              
    int3printf(outf, *unit, sepchar, 0);
    if (*graphtype == 'B') {
      if (lang == ENGLISH)
	fprintf(outf, " %sbyte%s, or part thereof.\n\n", bprefix,
		(*unit == 1)?"":"s");
      else if (lang == FRENCH)
	fprintf(outf, " %soctet%s, ou une partie du moins.\n\n", bprefix,
		(*unit == 1)?"":"s");
      else if (lang == GERMAN)
	fprintf(outf, " %s%syte%s, oder einem Teil dessen.", bprefix,
                (bprefix[0] == '\0')?"B":"b", (*unit == 1)?"":"s");
      else if (lang == SPANISH)
	fprintf(outf, " %sbyte%s, o fracci%sn.", bprefix,
		(*unit == 1)?"":"s", (aq == HTML)?"&oacute":"o");
      else if (lang == DANISH)
	fprintf(outf, " %sbyte%s, eller en del heraf.\n\n", bprefix,
		(*unit == 1)?"":"s");
      else /* lang == ITALIAN */
        fprintf(outf, " %sbyte, o frazione.\n\n", bprefix);
  
    }
    else if (*graphtype == 'P') {
      if (lang == ENGLISH)
	fprintf(outf, " request%s.\n\n",
		(*unit == 1)?" for a page":"s for pages, or part thereof");
      else if (lang == FRENCH)
	fprintf(outf, " requ%ste%s.\n\n", (aq == HTML)?"&ecirc;":"e",
		(*unit == 1)?" sur une page":
		"s sur des pages, ou une partie du moins");
      else if (lang == GERMAN)
	fprintf(outf, " Seiten-Anfrage%s.\n\n",
		(*unit == 1)?"":"n, oder einem Teil dessen");
      else if (lang == SPANISH)
	fprintf(outf, " petici%s.\n\n",
		(*unit == 1)?((aq == HTML)?" &oacute;n de una p&aacute;gina":"on de una pagina"):
		((aq == HTML)?"ones de p&aacute;ginas, o fracci&oacute;n":"ones de paginas, o fraccion"));
      else if (lang == DANISH)
	fprintf(outf, " sideforesp%srgs%s.\n\n", (aq == HTML)?"&oslash;":"oe",
		(*unit == 1)?"el": "ler, eller en del heraf");
      else /* lang == ITALIAN */
        fprintf(outf, " richiest%s.\n\n",
                (*unit == 1)?"a di pagina":
                "e di pagine, o frazione");
    }
    else if (lang == ENGLISH)
      fprintf(outf, " request%s.\n\n", (*unit == 1)?"":"s, or part thereof");
    else if (lang == FRENCH)
      fprintf(outf, " requ%ste%s.\n\n", (aq == HTML)?"&ecirc;":"e",
	      (*unit == 1)?"":"s, ou une partie du moins");
    else if (lang == GERMAN)
      fprintf(outf, " Anfrage%s.\n\n",
	      (*unit == 1)?"":"n, oder einem Teil dessen");
    else if (lang == SPANISH)
      fprintf(outf, " petici%s.\n\n",
	      (*unit == 1)?((aq == HTML)?"&oacute;n":"on"):
	      ((aq == HTML)?"ones, o fracci&oacute;n":"ones, o fraccion"));
    else if (lang == DANISH)
      fprintf(outf, " foresp%srgs%s.\n\n", (aq == HTML)?"&oslash;":"oe",
	      (*unit == 1)?"el":"ler, eller en del heraf");
    else /* lang == ITALIAN */
      fprintf(outf, " richiest%s.\n\n", (*unit == 1)?"a":"e, o frazione");

  }
  if (!aq)
    fprintf(outf, "<pre width=%d><tt>\n", pagewidth);
	
  printcolheads(outf, wantcols, *fieldwidth, *pfieldwidth, *bfieldwidth,
		bprefix, colhead, codeletter, byq, ON, TRUE);

}

/* As promised, each separate line. We print name of date in output() though */

void dateline(FILE *outf, int reqs, int pages, double bytes, char *wantcols,
	      char graphtype, int fieldwidth, int pfieldwidth, int bfieldwidth,
	      int unit, double bdivider)
{
  extern double total_bytes;
  extern int total_succ_reqs, total_page_reqs;
  extern flag aq, byq;

  printcols(outf, wantcols, reqs, pages, bytes, fieldwidth, pfieldwidth,
	    bfieldwidth, bdivider, total_succ_reqs, total_page_reqs,
	    total_bytes, '\0', byq, ON);

  if (aq != PREFORMATTED) {
    if (graphtype == 'B')
      barplot(outf, (int)(ceil(bytes / (unit * bdivider))));
    else if (graphtype == 'P')
      barplot(outf, (pages == 0)?0:((pages - 1) / unit) + 1);
    else
      barplot(outf, (reqs == 0)?0:((reqs - 1) / unit) + 1);
    fprintf(outf, "\n");  /* PREFORMATTED has more before the \n */
  }
}

/*** The status code report (very simple) ***/

void statusout(FILE *outf)
{
  extern int status[], statusnos[];
  extern char statusstrs[NO_STATUS][MAXSTATUSLENGTH];
  extern char ccols[];
  extern int aq, lang;
  extern char repsepchar;

  int fieldwidth;
  int maxreqs = 0;
  char langstr[MAXSTRINGLENGTH];
  int i;

  if (aq != PREFORMATTED) {
    for (i = 0; i < NO_STATUS; i++)
      maxreqs = MAX(maxreqs, status[i]);

    if (lang == ENGLISH)
      strcpy(langstr, "Status Code Report");
    else if (lang == FRENCH)
      strcpy(langstr, "Rapport des Statuts");
    else if (lang == GERMAN)
      strcpy(langstr, "Statuscode-Bericht");
    else if (lang == ITALIAN)
      strcpy(langstr, "Resoconto codici di stato");
    else if (lang == DANISH)
      strcpy(langstr, "Statuskode rapport");
    else if (aq == HTML) /* and lang == SPANISH */
        strcpy(langstr, "Informe de C&oacute;digos de Estado");
    else /* lang == SPANISH && aq != HTML */
      strcpy(langstr, "Informe de Codigos de Estado");
    if (aq == HTML) {
      fprintf(outf, "\n\n<hr>\n<h2><a NAME=\"Status\">%s</a></h2>\n\n",
	      langstr);
      gotos(outf, 'c');
      fprintf(outf, "<pre>");
    }
    else {
      fprintf(outf, "%s\n", langstr);
      for (i = (int)strlen(langstr); i > 0; i--)
         fprintf(outf, "-");
      fprintf(outf, "\n\n");
    }
    i = 10000;
    for (fieldwidth = 5; maxreqs / i >= 10; fieldwidth++)
      i *= 10;
    if (repsepchar != '\0' && maxreqs > 10000)
      fieldwidth = fieldwidth + ((fieldwidth - 1) / 3);

    if (lang == GERMAN)
      strcpy(langstr, "Nr. Beschreibung");
    else if (lang == ITALIAN)
      strcpy(langstr, "no. descrizione");
    else if (lang == FRENCH || lang == ENGLISH)
      strcpy(langstr, "no. description");
    else if (lang == DANISH)
      strcpy(langstr, "nr. beskrivelse");
    else if (aq == HTML) /* and SPANISH */
        strcpy(langstr, "nr. descripci&oacute;n");
    else /* SPANISH, not HTML */
      strcpy(langstr, "nr. descripcion");
    printcolheads(outf, ccols, fieldwidth, 0, 0, "", langstr, 'c', FALSE,
		  FALSE, FALSE);
  }

  for (i = 0; i < NO_STATUS; i++) {
    if (status[i] > 0) {
      if (aq == PREFORMATTED)
	precols(outf, ccols, 'c', OFF, OFF);
      printcols(outf, ccols, status[i], 0, 0.0, fieldwidth, 0, 0, 0.0, 1, 1,
		1.0, 'c', OFF, OFF);
      if (aq == PREFORMATTED)
	fprintf(outf, "%d\n", statusnos[i]);
      else if (statusstrs[i][0] == '[')
	fprintf(outf, "    %s\n", statusstrs[i]);
      else
	fprintf(outf, "%d %s\n", statusnos[i], statusstrs[i]);
    }
  }

  if (aq == ASCII)
    asciiline(outf);
  else if (aq == HTML)
    fprintf(outf, "</pre>");
}

/*** The error report ***/

void errout(FILE *outf, int errorder[NO_ERRS])
{
  extern int errors[NO_ERRS];
  extern char errs[NO_ERRS][MAXERRLENGTH];
  extern int eminreqs;
  extern char ecols[];
  extern int aq, lang;
  extern char repsepchar;

  int fieldwidth;
  char langstr[MAXSTRINGLENGTH], langstr2[MAXSTRINGLENGTH];
  int i;

  if (lang == ENGLISH) {
    strcpy(langstr, "Error Report");
    strcpy(langstr2, "------------");
  }
  else if (lang == FRENCH) {
    strcpy(langstr, "Rapport des Erreurs");
    strcpy(langstr2, "-------------------");
  }
  else if (lang == GERMAN) {
    strcpy(langstr, "Fehlerbericht");
    strcpy(langstr2, "-------------");
  }
  else if (lang == SPANISH) {
    strcpy(langstr, "Informe de Errores");
    strcpy(langstr2, "------------------");
  }
  else if (lang == DANISH) {
    strcpy(langstr, "Fejlrapport");
    strcpy(langstr2, "-----------");
  }
  else { /* lang == ITALIAN */
    strcpy(langstr, "Resoconto errori");
    strcpy(langstr2, "----------------");
  }

  if (aq != PREFORMATTED) {
    if (aq == HTML) {
      fprintf(outf, "\n\n<hr>\n<h2><a NAME=\"Error\">%s</a></h2>\n\n",
	      langstr);
      gotos(outf, 'e');
      fprintf(outf, "<p>");
    }
    else /* not HTML */
      fprintf(outf, "%s\n%s\n", langstr, langstr2);

    if (eminreqs == 0) {
      if (lang == ENGLISH)
	fprintf(outf, "Printing all possible errors, ");
      else if (lang == FRENCH)
	fprintf(outf, "Affichage de toutes les erreurs possibles, ");
      else if (lang == SPANISH)
	fprintf(outf, "Mostrando todos los errores posibles, ");
      else if (lang == ITALIAN)
        fprintf(outf, "Elenco di tutti gli errori possibili, ");
      else if (lang == DANISH)
	fprintf(outf, "Udskriver alle mulige fejl, ");
      else if (aq == HTML)  /* and GERMAN */
	fprintf(outf, "Ausgabe aller m&ouml;glichen Fehler, ");
      else  /* GERMAN and not HTML */
	fprintf(outf, "Ausgabe aller moeglichen Fehler, ");
    }
    
    else if (lang == ENGLISH)
      fprintf(outf, "Printing all errors with at least %d occurrence%s,\n  ",
	      eminreqs, (eminreqs == 1)?"":"s");
    else if (lang == FRENCH)
      fprintf(outf,
	      "Affiche toutes les erreurs avec au moins %d occurence%s,\n  ",
	      eminreqs, (eminreqs == 1)?"":"s");
    else if (lang == GERMAN)
      fprintf(outf, "Ausgabe aller Fehler die mindestens %d mal aufgetreten sind,\n  ",
              eminreqs);
    else if (lang == SPANISH)
      fprintf(outf,
	      "Mostrando todos los errores con al menos %d ocurrencia%s,\n  ",
	      eminreqs, (eminreqs == 1)?"":"s");
    else if (lang == DANISH)
      fprintf(outf, "Udskriver alle fejl med mindst %d forekomst%s,\n  ",
	      eminreqs, (eminreqs == 1)?"":"er");
    else /* lang == ITALIAN */
      fprintf(outf, "Elenco di tutti gli errori possibili accaduti almeno %d volt%s,\n  ",
              eminreqs, (eminreqs == 1)?"a":"e" );
          
    if (lang == ENGLISH)
      fprintf(outf, "sorted by number of occurrences.");
    else if (lang == FRENCH)
      fprintf(outf, "tri%ses par le nombre d'occurences.",
	      (aq == HTML)?"&eacute;":"e");
    else if (lang == GERMAN)
      fprintf(outf, "sortiert nach H%sufigkeit.",
	      (aq == HTML)?"&auml;":"ae");
    else if (lang == SPANISH)
      fprintf(outf, "ordenados por n%smero de ocurrencias.",
	      (aq == HTML)?"&uacute;":"u");
    else if (lang == DANISH)
      fprintf(outf, "sorteret efter antal forekomster.");
    else /* lang == ITALIAN */
      fprintf(outf, "  in ordine di frequenza.");
  
    if (aq)
      fprintf(outf, "\n\n");
    else
      fprintf(outf, "<pre>");

    i = 10000;
    for (fieldwidth = 5; errors[errorder[0]] / i >= 10; fieldwidth++)
      i *= 10;
    if (repsepchar != '\0' && errors[errorder[0]] >= 10000)
      fieldwidth = fieldwidth + ((fieldwidth - 1) / 3);

    if (lang == ENGLISH)
      strcpy(langstr, "error type");
    else if (lang == FRENCH)
      strcpy(langstr, "type d'erreur");
    else if (lang == GERMAN)
      strcpy(langstr, "Fehlertyp");
    else if (lang == SPANISH)
      strcpy(langstr, "tipo de error");
    else if (lang == DANISH)
      strcpy(langstr, "fejltype");
    else /* lang == ITALIAN */
      strcpy(langstr, "tipo di errore");
  
    printcolheads(outf, ecols, fieldwidth, 0, 0, "", langstr, 'e', FALSE,
		  FALSE, FALSE);
  }

  for (i = 0; i < NO_ERRS && errors[errorder[i]] >= eminreqs; i++) {
    if (aq == PREFORMATTED)
      precols(outf, ecols, 'e', FALSE, FALSE);
    printcols(outf, ecols, errors[errorder[i]], 0, 0.0, fieldwidth, 0, 0, 0.0,
	      1, 1, 1.0, 'e', OFF, OFF);
    fprintf(outf, "%s\n",
	    (errs[errorder[i]][0] == '\0')?"[unknown]":errs[errorder[i]]);
  }

  if (aq == ASCII)
    asciiline(outf);
  else if (aq == HTML)
    fprintf(outf, "</pre>");
}

/*** And the general summary ***/

void gensum(FILE *outf)
{
  extern int corrupt_lines, other_lines;
  extern int no_urls, no_hosts, no_urls7, no_hosts7, no_new_hosts7;
  extern double total_bytes, total_bytes7;
  extern int total_succ_reqs, total_fail_reqs, total_other_reqs;
  extern int total_succ_reqs7, total_fail_reqs7, total_other_reqs7;
  extern int total_page_reqs, total_page_reqs7;
  extern flag mq, Wq, dq, Dq, hq, oq, Sq, iq, rq, q7, byq;
  extern int sq, aq, lang, dialect;
  extern char sepchar, *presep;
  extern struct timestruct starttimec, totime, firsttime, lasttime, oldtime;
  extern char dayname[7][11];
  extern char monthname[12][12];

  int totalmins;    /* between first and last entries analysed */
  char langstr[MAXSTRINGLENGTH], langstr2[MAXSTRINGLENGTH];
  double bdivider;
  char bprefix[2];  /* kilo, Mega, etc. */

  bprefix[0] = '\0';
  bprefix[1] = '\0';

  if (aq == HTML)
    fprintf(outf, "<hr>");

  if (aq == PREFORMATTED)
    fprintf(outf, "\nx%sPS%s%d%s%d%s%d%s%d%s%d", presep,
	    presep, starttimec.year, presep, starttimec.monthno + 1,
	    presep, starttimec.date, presep, starttimec.hr, presep,
	    starttimec.min);
  else if (lang == ENGLISH && dialect == NONE)
    fprintf(outf,
	    "\nProgram started at %s-%02d-%s-%d %02d:%02d local time.\n",
	    dayname[dayofdate(starttimec.date, starttimec.monthno,
			      starttimec.year)], starttimec.date,
	    monthname[starttimec.monthno], starttimec.year,
	    starttimec.hr, starttimec.min);
  else if (lang == ENGLISH && dialect == US_ENGLISH)
    fprintf(outf, "\nProgram started at %s-%s-%02d-%d %02d:%02d local time.\n",
	    dayname[dayofdate(starttimec.date, starttimec.monthno,
			      starttimec.year)],
	    monthname[starttimec.monthno], starttimec.date, starttimec.year,
	    starttimec.hr, starttimec.min);
  else if (lang == GERMAN) {
    if (aq == HTML)
      strcpy(langstr, "<b>Programmstart:</b>");
    else
      strcpy(langstr, "Programmstart:");
    fprintf(outf, "\n%s %s, %d. %s %d %02d:%02d Ortszeit.\n", langstr,
	    dayname[dayofdate(starttimec.date, starttimec.monthno,
			      starttimec.year)], starttimec.date,
	    monthname[starttimec.monthno], starttimec.year,
	    starttimec.hr, starttimec.min);
  }
  else if (lang == ITALIAN)
    fprintf(outf, "\nProgramma attivato il giorno %s %02d %s %d alle %02d:%02d ora locale.\n",
            dayname[dayofdate(starttimec.date, starttimec.monthno,
			      starttimec.year)], starttimec.date,
            monthname[starttimec.monthno], starttimec.year,
            starttimec.hr, starttimec.min);
  else if (lang == SPANISH)
    fprintf(outf,
	    "\nComienzo del programa el %s %02d %s %d %02d:%02d hora local.\n",
	    dayname[dayofdate(starttimec.date, starttimec.monthno,
			      starttimec.year)], starttimec.date,
	    monthname[starttimec.monthno], starttimec.year,
	    starttimec.hr, starttimec.min);
  else if (lang == DANISH)
    fprintf(outf, "\nProgram start: %s, %02d. %s, %d %02d:%02d lokaltid.\n",
	    dayname[dayofdate(starttimec.date, starttimec.monthno,
			      starttimec.year)],
	    starttimec.date, monthname[starttimec.monthno], starttimec.year,
	    starttimec.hr, starttimec.min);
  else { /* lang == FRENCH */
    if (aq == HTML)
      strcpy(langstr, "Programme lanc&eacute; le");
    else
      strcpy(langstr, "Programme lance le");
    fprintf(outf, "\n%s %s %02d %s %d %02d:%02d heure locale.\n", langstr,
	    dayname[dayofdate(starttimec.date, starttimec.monthno,
			      starttimec.year)], starttimec.date,
	    monthname[starttimec.monthno], starttimec.year,
	    starttimec.hr, starttimec.min);
  }

  if (firsttime.code > oldtime.code)
    q7 = OFF;

  if (total_succ_reqs > 0) {
    totalmins = minsbetween(firsttime.date, firsttime.monthno,
			    firsttime.year, firsttime.hr, firsttime.min,
			    lasttime.date, lasttime.monthno, lasttime.year,
			    lasttime.hr, lasttime.min) + 1;
    if (aq == HTML)
      fprintf(outf, "<br>");
    if (aq == PREFORMATTED) {
      fprintf(outf, "\nx%sFR%s%d%s%d%s%d%s%d%s%d", presep, presep,
	      firsttime.year, presep, firsttime.monthno + 1, presep,
	      firsttime.date, presep, firsttime.hr, presep, firsttime.min);
      fprintf(outf, "\nx%sLR%s%d%s%d%s%d%s%d%s%d", presep, presep,
	      lasttime.year, presep, lasttime.monthno + 1, presep,
	      lasttime.date, presep, lasttime.hr, presep, lasttime.min);
      if (q7)
	fprintf(outf, "\nx%sL7%s%d%s%d%s%d%s%d%s%d", presep, presep,
		oldtime.year, presep, oldtime.monthno + 1, presep,
		oldtime.date, presep, oldtime.hr, presep, oldtime.min);
    }
    else if (lang == ENGLISH && dialect == NONE)
      fprintf(outf, "Analysed requests from %s-%02d-%s-%d %02d:%02d to %s-%02d-%s-%d %02d:%02d\n  (%.1f days).\n\n",
	      dayname[dayofdate(firsttime.date,
				firsttime.monthno, firsttime.year)],
	      firsttime.date, monthname[firsttime.monthno], firsttime.year,
	      firsttime.hr, firsttime.min,
	      dayname[dayofdate(lasttime.date, lasttime.monthno,
				lasttime.year)],
	      lasttime.date, monthname[lasttime.monthno], lasttime.year,
	      lasttime.hr, lasttime.min, (double)totalmins / 1440.0);
    else if (lang == ENGLISH && dialect == US_ENGLISH)
      fprintf(outf, "Analyzed requests from %s-%s-%02d-%d %02d:%02d to %s-%s-%02d-%d %02d:%02d\n  (%.1f days).\n\n",
	      dayname[dayofdate(firsttime.date,
				firsttime.monthno, firsttime.year)],
	      monthname[firsttime.monthno], firsttime.date, firsttime.year,
	      firsttime.hr, firsttime.min,
	      dayname[dayofdate(lasttime.date, lasttime.monthno,
				lasttime.year)],
	      monthname[lasttime.monthno], lasttime.date, lasttime.year,
	      lasttime.hr, lasttime.min, (double)totalmins / 1440.0);
    else if (lang == GERMAN) {
      if (aq == HTML)
	strcpy(langstr, "<b>Auswertungszeitraum:</b>");
      else
	strcpy(langstr, "Auswertungszeitraum:");
      fprintf(outf, "%s %s, %d. %s %d %02d:%02d bis %s, %d. %s %d %02d:%02d\n  (%.1f Tage).\n\n",
	      langstr, dayname[dayofdate(firsttime.date,
					 firsttime.monthno, firsttime.year)],
	      firsttime.date, monthname[firsttime.monthno], firsttime.year,
	      firsttime.hr, firsttime.min,
	      dayname[dayofdate(lasttime.date, lasttime.monthno,
				lasttime.year)],
	      lasttime.date, monthname[lasttime.monthno], lasttime.year,
	      lasttime.hr, lasttime.min, (double)totalmins / 1440.0);
    }
    else if (lang == ITALIAN)
      fprintf(outf, "Analizzate le richieste da %s %02d %s %d %02d:%02d a %s %02d %s %d %02d:%02d\n  (%.1f giorni).\n\n",
	      dayname[dayofdate(firsttime.date,
				firsttime.monthno, firsttime.year)],
	      firsttime.date, monthname[firsttime.monthno], firsttime.year,
	      firsttime.hr, firsttime.min,
	      dayname[dayofdate(lasttime.date, lasttime.monthno,
				lasttime.year)],
	      lasttime.date, monthname[lasttime.monthno], lasttime.year,
	      lasttime.hr, lasttime.min, (double)totalmins / 1440.0);
    else if (lang == SPANISH) {
      if (aq == HTML) {
	strcpy(langstr, "An&aacute;lisis de peticiones desde el");
	strcpy(langstr2, "al");
      }
      else {
	strcpy(langstr, "Analisis de peticiones desde el");
	strcpy(langstr2, "al");
      }
      fprintf(outf, "%s %s %02d %s %d %02d:%02d %s %s %02d %s %d %02d:%02d\n  (%.1f d%sas).\n\n",
	      langstr, dayname[dayofdate(firsttime.date,
					 firsttime.monthno, firsttime.year)],
	      firsttime.date, monthname[firsttime.monthno], firsttime.year,
	      firsttime.hr, firsttime.min, langstr2,
	      dayname[dayofdate(lasttime.date, lasttime.monthno,
				lasttime.year)],
	      lasttime.date, monthname[lasttime.monthno], lasttime.year,
	      lasttime.hr, lasttime.min, (double)totalmins / 1440.0,
	      (aq == HTML)?"&iacute;":"i");
    }
    else if (lang == DANISH) {
      if (aq == HTML)
	strcpy(langstr, "Analyserede foresp&oslash;rgsler fra");
      else
	strcpy(langstr, "Analyserede forespoergelser fra");
      fprintf(outf, "%s %s, %02d. %s %d %02d:%02d til\n  %s, %d. %s %d %02d:%02d (%.1f dage).\n\n",
	      langstr, dayname[dayofdate(firsttime.date,
					 firsttime.monthno, firsttime.year)],
	      firsttime.date, monthname[firsttime.monthno], firsttime.year,
	      firsttime.hr, firsttime.min,
	      dayname[dayofdate(lasttime.date, lasttime.monthno,
				lasttime.year)],
	      lasttime.date, monthname[lasttime.monthno], lasttime.year,
	      lasttime.hr, lasttime.min, (double)totalmins / 1440.0);
    }
    else { /* lang == FRENCH */
      if (aq == HTML) {
	strcpy(langstr, "Requ&ecirc;tes analys&eacute;es de");
	strcpy(langstr2, "&agrave;");
      }
      else {
	strcpy(langstr, "Requetes analysees de");
	strcpy(langstr2, "a");
      }
      fprintf(outf, "%s %s %02d %s %d %02d:%02d %s %s %02d %s %d %02d:%02d\n  (%.1f jours).\n\n",
	      langstr, dayname[dayofdate(firsttime.date,
					 firsttime.monthno, firsttime.year)],
	      firsttime.date, monthname[firsttime.monthno], firsttime.year,
	      firsttime.hr, firsttime.min, langstr2,
	      dayname[dayofdate(lasttime.date, lasttime.monthno,
				lasttime.year)],
	      lasttime.date, monthname[lasttime.monthno], lasttime.year,
	      lasttime.hr, lasttime.min, (double)totalmins / 1440.0);
	      
    }


  }

  if (aq == HTML) {
    if (lang == ENGLISH)
      fprintf(outf, "<p><b>Total successful requests:</b> ");
    else if (lang == FRENCH)
      fprintf(outf,
	      "<p><b>Nombre total de requ&ecirc;tes effectu&eacute;es :</b> ");
    else if (lang == GERMAN)
      fprintf(outf, "<p><b>Erfolgreich bearbeitete Anfragen:</b> ");
    else if (lang == SPANISH)
      fprintf(outf,
	      "<p><b>N&uacute;mero total de peticiones efectuadas:</b> ");
    else if (lang == DANISH)
      fprintf(outf, "<p><b>Totalt antal opfyldte foresp&oslash;rgsler:</b> ");
    else /* lang == ITALIAN */
      fprintf(outf, "<p><b>Totale richieste soddisfatte:</b> ");
  }
  else if (aq == ASCII) {
    if (lang == ENGLISH)
      fprintf(outf, "Total successful requests: ");
    else if (lang == FRENCH)
      fprintf(outf, "Nombre total de requetes effectuees : ");
    else if (lang == GERMAN)
      fprintf(outf, "Erfolgreich bearbeitete Anfragen: ");
    else if (lang == SPANISH)
      fprintf(outf, "Numero total de peticiones efectuadas: ");
    else if (lang == DANISH)
      fprintf(outf, "Totalt antal opfyldte forespoergsler: ");
    else /* lang == ITALIAN */
      fprintf(outf, "Totale richieste soddisfatte: ");
  }
  else   /* aq == PREFORMATTED */
    fprintf(outf, "\nx%sSR%s", presep, presep);
  int3printf(outf, total_succ_reqs, sepchar, 0);
  if (q7) {
    if (aq == PREFORMATTED)
      fprintf(outf, "\nx%sS7%s%d", presep, presep, total_succ_reqs7);
    else {
      fprintf(outf, " (");
      int3printf(outf, total_succ_reqs7, sepchar, 0);
      fprintf(outf, ")");
    }
  }
  if (totalmins > 30 && aq != PREFORMATTED) {
    if (aq == HTML) {
      if (lang == ENGLISH)
	fprintf(outf, "\n<br><b>Average successful requests per day:</b> ");
      else if (lang == FRENCH)
	fprintf(outf, "\n<br><b>Moyenne par jour des requ&ecirc;tes effectu&eacute;es :</b> ");
      else if (lang == GERMAN)
	fprintf(outf, "\n<br><b>Durchschnittlich bearbeitete Anfragen pro Tag:</b> ");
      else if (lang == SPANISH)
	fprintf(outf,
		"\n<br><b>N&uacute;mero medio de peticiones diarias:</b> ");
      else if (lang == DANISH)
	fprintf(outf, "\n<br><b>Antal opfyldte foresp&oslash;rgsler pr. dag (gennemsnit):</b> ");
      else /* lang == ITALIAN */
        fprintf(outf,
		"\n<br><b>Media giornaliera di richieste soddisfatte:</b> ");
    }
    else if (lang == ENGLISH)
      fprintf(outf, "\nAverage successful requests per day: ");
    else if (lang == FRENCH)
      fprintf(outf, "\nMoyenne par jour des requetes effectuees : ");
    else if (lang == GERMAN)
      fprintf(outf, "\nDurchschnittlich bearbeitete Anfragen pro Tag: ");
    else if (lang == SPANISH)
      fprintf(outf, "\nNumero medio de peticiones diarias: ");
      else if (lang == DANISH)
	fprintf(outf,
		"\nAntal opfyldte forespoergsler pr. dag (gennemsnit): ");
    else /* lang == ITALIAN */
      fprintf(outf, "\nMedia giornaliera di richieste soddisfatte: ");
      
    if (total_succ_reqs < 2)
      fprintf(outf, "0");
    else
      double3printf(outf, ROUND((double)(total_succ_reqs - 1)) * 1440.0 / (totalmins + 0.0), sepchar, 0);
    if (q7) {
      fprintf(outf, " (");
      int3printf(outf, total_succ_reqs7 / 7, sepchar, 0);
      fprintf(outf, ")");
    }
  }
  if (total_page_reqs > 0) {
    if (aq == HTML) {
      if (lang == ENGLISH)
	fprintf(outf, "\n<br><b>Total successful requests for pages:</b> ");
      else if (lang == FRENCH)
	fprintf(outf, "\n<br><b>Nombre total de requ&ecirc;tes sur des pages effectu&eacute;es :</b> ");
      else if (lang == GERMAN)
	fprintf(outf,
		"\n<br><b>Erfolgreich bearbeitete Seiten-Anfragen:</b> ");
      else if (lang == SPANISH)
	fprintf(outf, "\n<br><b>N&uacute;mero total de peticiones de p&aacute;ginas:</b> ");
      else if (lang == DANISH)
	fprintf(outf, "\n<br><b>Totalt antal opfyldte foresp&oslash;rgsler for sider:</b> ");
      else /* lang == ITALIAN */
        fprintf(outf, "\n<br><b>Totale richieste di pagine soddisfatte:</b> ");
    }
    else if (aq == ASCII) {
      if (lang == ENGLISH)
	fprintf(outf, "\nTotal successful requests for pages: ");
      else if (lang == FRENCH)
	fprintf(outf, "\nNombre total de requetes pour pages effectuees : ");
      else if (lang == GERMAN)
	fprintf(outf, "\nErfolgreich bearbeitete Anfragen fuer Seiten: ");
      else if (lang == SPANISH)
	fprintf(outf, "\nNumero total de peticiones de paginas: ");
      else if (lang == DANISH)
	fprintf(outf, "\nTotalt antal opfyldte forespoergsler for sider: ");
      else /* lang == ITALIAN */
        fprintf(outf, "\nTotale richieste di pagine soddisfatte: ");
    }
    else   /* aq == PREFORMATTED */
      fprintf(outf, "\nx%sPR%s", presep, presep);
    int3printf(outf, total_page_reqs, sepchar, 0);
    if (q7) {
      if (aq == PREFORMATTED)
	fprintf(outf, "\nx%sP7%s%d", presep, presep, total_page_reqs7);
      else {
	fprintf(outf, " (");
	int3printf(outf, total_page_reqs7, sepchar, 0);
	fprintf(outf, ")");
      }
    }
  }
  if (total_fail_reqs > 0) {
    if (aq == HTML) {
      if (lang == ENGLISH)
	fprintf(outf, "\n<br><b>Total failed requests:</b> ");
      else if (lang == FRENCH)
	fprintf(outf,
		"\n<br><b>Nombre de requ&ecirc;tes rat&eacute;es :</b> ");
      else if (lang == GERMAN)
	fprintf(outf, "\n<br><b>Fehlgeschlagene Anfragen:</b> ");
      else if (lang == SPANISH)
	fprintf(outf, "\n<br><b>N&uacute;mero de peticiones fallidas:</b> ");
      else if (lang == DANISH)
	fprintf(outf,
		"\n<br><b>Totalt antal uopfyldte foresp&oslash;rgsler:</b> ");
      else /* lang == ITALIAN */
        fprintf(outf, "\n<br><b>Totale richieste fallite:</b> ");
    }
    else if (aq == ASCII) {
      if (lang == ENGLISH)
	fprintf(outf, "\nTotal failed requests: ");
      else if (lang == FRENCH)
	fprintf(outf, "\nNombre de requetes ratees : ");
      else if (lang == GERMAN)
	fprintf(outf, "\nFehlgeschlagene Anfragen: ");
      else if (lang == SPANISH)
	fprintf(outf, "\nNumero de peticiones fallidas: ");
      else if (lang == DANISH)
	fprintf(outf, "\nTotalt antal uopfyldte forespoergsler: ");
      else /* lang == ITALIAN */
        fprintf(outf, "\nTotale richieste fallite: ");
    }
    else   /* aq == PREFORMATTED */
      fprintf(outf, "\nx%sFL%s", presep, presep);
    int3printf(outf, total_fail_reqs, sepchar, 0);
    if (q7) {
      if (aq == PREFORMATTED)
	fprintf(outf, "\nx%sF7%s%d", presep, presep, total_fail_reqs7);
      else {
	fprintf(outf, " (");
	int3printf(outf, total_fail_reqs7, sepchar, 0);
	fprintf(outf, ")");
      }
    }
  }
  if (total_other_reqs > 0) {
    if (aq == HTML) {
      if (lang == ENGLISH)
	fprintf(outf, "\n<br><b>Total redirected requests:</b> ");
      else if (lang == FRENCH)
	fprintf(outf,
		"\n<br><b>Nombre de requ&ecirc;tes redirig&eacute;es :</b> ");
      else if (lang == GERMAN)
	fprintf(outf, "\n<br><b>Umgeleitete Anfragen:</b> ");
      else if (lang == SPANISH)
	fprintf(outf,
		"\n<br><b>N&uacute;mero de peticiones redirigidas:</b> ");
      else if (lang == DANISH)
	fprintf(outf, "\n<br><b>Totalt antal omdirigerede foresp&oslash;rgsler:</b> ");
      else /* lang == ITALIAN */
        fprintf(outf, "\n<br><b>Totale richieste reindirizzate:</b> ");
    }
    else if (aq == ASCII) {
      if (lang == ENGLISH)
	fprintf(outf, "\nTotal redirected requests: ");
      else if (lang == FRENCH)
	fprintf(outf, "\nNombre de requetes redirigees : ");
      else if (lang == GERMAN)
	fprintf(outf, "\nUmgeleitete Anfragen: ");
      else if (lang == SPANISH)
	fprintf(outf, "\nNumero de peticiones redirigidas: ");
      else if (lang == DANISH)
	fprintf(outf, "\nTotalt antal omdirigerede forespoergsler: ");
      else /* lang == ITALIAN */
        fprintf(outf, "\nTotale richieste reindirizzate: ");
    }
    else   /* aq == PREFORMATTED */
      fprintf(outf, "\nx%sRR%s", presep, presep);
    int3printf(outf, total_other_reqs, sepchar, 0);
    if (q7) {
      if (aq == PREFORMATTED)
	fprintf(outf, "\nx%sR7%s%d", presep, presep, total_other_reqs7);
      else {
	fprintf(outf, " (");
	int3printf(outf, total_other_reqs7, sepchar, 0);
	fprintf(outf, ")");
      }
    }
  }
  if (rq) {   /* These data are not collected o/wise */
    if (aq == HTML) {
      if (lang == ENGLISH)
	fprintf(outf, "\n<br><b>Number of distinct files requested:</b> ");
      else if (lang == FRENCH)
	fprintf(outf,
		"\n<br><b>Nombre de fichiers distincts appel&eacute;s :</b> ");
      else if (lang == GERMAN)
	fprintf(outf,
		"\n<br><b>Anzahl unterschiedlicher verlangter Dateien:</b> ");
      else if (lang == SPANISH)
	fprintf(outf, "\n<br><b>N&uacute;mero de ficheros diferentes solicitados:</b> ");
      else if (lang == DANISH)
	fprintf(outf, "\n<br><b>Antal enkelte filer forespurgt:</b> ");
      else /* lang == ITALIAN */
        fprintf(outf, "\n<br><b>Numero di file distinti richiesti:</b> ");
    }
    else if (aq == ASCII) {
      if (lang == ENGLISH)
	fprintf(outf, "\nNumber of distinct files requested: ");
      else if (lang == FRENCH)
	fprintf(outf, "\nNombre de fichiers distincts appeles : ");
      else if (lang == GERMAN)
	fprintf(outf, "\nAnzahl unterschiedlicher verlangter Dateien: ");
      else if (lang == SPANISH)
	fprintf(outf, "\nNumero de ficheros diferentes solicitados: ");
      else if (lang == DANISH)
	fprintf(outf, "\nAntal enkelte filer forespurgt: ");
      else /* lang == ITALIAN */
        fprintf(outf, "\nNumero di file distinti richiesti: ");
    }
    else   /* aq == PREFORMATTED */
      fprintf(outf, "\nx%sNF%s", presep, presep);
    int3printf(outf, no_urls, sepchar, 0);
    if (q7) {
      if (aq == PREFORMATTED)
	fprintf(outf, "\nx%sN7%s%d", presep, presep, no_urls7);
      else {
	fprintf(outf, " (");
	int3printf(outf, no_urls7, sepchar, 0);
	fprintf(outf, ")");
      }
    }
  }
  if ((sq == ON || sq == APPROX) && no_hosts > 0) {
    if (aq == HTML) {
      if (lang == ENGLISH)
	fprintf(outf, "\n<br><b>%sumber of distinct hosts served:</b> ",
		(sq == ON)?"N":"Approximate n");
      else if (lang == FRENCH)
	fprintf(outf,
		"\n<br><b>Nombre%s d'invit&eacute;s distincts servis :</b> ",
		(sq == ON)?"":" approximatif");
      else if (lang == GERMAN)
	fprintf(outf, "\n<br><b>%sAnzahl unterschiedlicher anfragender Hosts:</b> ", 
                (sq == ON)?"":"Ungef&auml;hre ");
      else if (lang == SPANISH)
	fprintf(outf, "\n<br><b>N&uacute;mero%s de <i>hosts</i> diferentes atendidos:</b> ", (sq == ON)?"":"aproximado ");
      else if (lang == DANISH)
	fprintf(outf, "\n<br><b>%sAntal enkelte hosts betjent:</b> ",
		(sq == ON)?"":"ca. ");
      else /* lang == ITALIAN */
        fprintf(outf, "\n<br><b>Numero%s di host distinti serviti:</b> ",
                (sq == ON)?"":" approssimato");
    }
    else if (aq == ASCII) {
      if (lang == ENGLISH) 
	fprintf(outf, "\n%sumber of distinct hosts served: ",
		(sq == ON)?"N":"Approximate n");
      else if (lang == FRENCH)
	fprintf(outf, "\nNombre%s d'invites distincts servis : ",
		(sq == ON)?"":" approximatif");
      else if (lang == GERMAN)
	fprintf(outf, "\n%sAnzahl unterschiedlicher anfragender Hosts: ",
		(sq == ON)?"":"Ungefaehre ");
      else if (lang == SPANISH)
	fprintf(outf, "\nNumero%s de hosts diferentes atendidos: ",
		(sq == ON)?"":"aproximado ");
      else if (lang == DANISH)
	fprintf(outf, "\n%sAntal enkelte hosts betjent: ",
		(sq == ON)?"":"ca. ");
      else /* lang == ITALIAN */
        fprintf(outf, "\nNumero%s di host distinti serviti: ",
                (sq == ON)?"":" approssimato");
    }
    else   /* aq == PREFORMATTED */
      fprintf(outf, "\nx%s%cH%s", presep, (sq == ON)?'N':'A', presep);
    int3printf(outf, no_hosts, sepchar, 0);
    if (q7) {
      if (aq == PREFORMATTED)
	fprintf(outf, "\nx%s%c7%s%d", presep, (sq == ON)?'H':'A', presep,
		no_hosts7);
      else {
	fprintf(outf, " (");
	int3printf(outf, no_hosts7, sepchar, 0);
	fprintf(outf, ")");
      }
      if (aq == HTML) {
	if (lang == ENGLISH)
	  fprintf(outf, "\n<br><b>%sumber of new hosts served in last 7 days:</b> ",
		  (sq == ON)?"N":"Approximate n");
	else if (lang == FRENCH)
	  fprintf(outf, "\n<br><b>Nombre%s de nouveaux invit&eacute;s les 7 derniers jours :</b> ",
		  (sq == ON)?" approximatif":"");
	else if (lang == GERMAN)
	  fprintf(outf, "\n<br><b>%sAnzahl neu anfragender Hosts in den letzten 7 Tagen:</b> ",
		  (sq == ON)?"":"Ungef&auml;hre ");
	else  if (lang == SPANISH)
	  fprintf(outf, "\n<br><b>N&uacute;mero%s de <i>hosts</i> nuevos atendidos en los &uacute;ltimos 7 d&iacute;as:</b> ",
		  (sq == ON)?" aproximado":"");
	else if (lang == DANISH)
	  fprintf(outf, "\n<br><b>%sAntal nye hosts de sidste 7 dage:</b> ",
		  (sq == ON)?"":"ca. ");
        else /* lang == ITALIAN */
          fprintf(outf, "\n<br><b>Numero%s di nuovi host serviti negli ultimi 7 giorni:</b> ",
                  (sq == ON)?"":" approssimato");
      }
      else if (aq == ASCII) {
	if (lang == ENGLISH)
	  fprintf(outf, "\n%sumber of new hosts served in last 7 days: ",
		  (sq == ON)?"N":"Approximate n");
	else if (lang == FRENCH)
	  fprintf(outf,
		  "\nNombre%s de nouveaux invites les 7 derniers jours : ",
		  (sq == ON)?" approximatif":"");
	else if (lang == GERMAN)
	  fprintf(outf, "\n%sAnzahl neu anfragender Hosts in den letzten 7 Tagen: ",
		  (sq == ON)?"":"Ungefaehre ");
	else if (lang == SPANISH)
	  fprintf(outf, "\nNumero%s de hosts nuevos atendidos en los ultimos 7 dias: ",
		  (sq == ON)?" aproximado":"");
	else if (lang == DANISH)
	  fprintf(outf, "\n%sAntal nye hosts de sidste 7 dage: ",
		  (sq == ON)?"":"ca. ");
        else /* lang == ITALIAN */
          fprintf(outf, "\nNumero%s di nuovi host serviti negli ultimi 7 giorni: ",
                  (sq == ON)?"":" approssimato");
              
      }
      else   /* aq == PREFORMATTED */
	fprintf(outf, "\nx%s%cV%s", presep, (sq == ON)?'N':'A', presep);
      int3printf(outf, no_new_hosts7, sepchar, 0);
    }
  }
  if (corrupt_lines > 0) {
    if (aq == HTML) {
      if (lang == ENGLISH)
	fprintf(outf, "\n<br><b>Corrupt logfile lines:</b> ");
      else if (lang == FRENCH)
	fprintf(outf,
		"\n<br><b>Lignes invalides dans le fichier log :</b> ");
      else if (lang == GERMAN)
	fprintf(outf, "\n<br><b>Unlesbare Zeilen in der Logdatei:</b> ");
      else if (lang == SPANISH)
	fprintf(outf, "\n<br><b>L&iacute;neas inv&aacute;lidas en el fichero de log:</b> ");
      else if (lang == DANISH)
	fprintf(outf, "\n<br><b>Ugyldige logfil linier:</b> ");
      else /* lang == ITALIAN */
        fprintf(outf, "\n<br><b>Linee del log file non valide:</b> ");
    }
    else if (aq == ASCII) {
      if (lang == ENGLISH)
	fprintf(outf, "\nCorrupt logfile lines: ");
      else if (lang == FRENCH)
	fprintf(outf, "\nLignes invalides dans le fichier log : ");
      else if (lang == GERMAN)
	fprintf(outf, "\nUnlesbare Zeilen in der Logdatei: ");
      else if (lang == SPANISH)
	fprintf(outf, "\nLineas invalidas en el fichero de log: ");
      else if (lang == DANISH)
	fprintf(outf, "\nUgyldige logfil linier: ");
      else /* lang == ITALIAN */
        fprintf(outf, "\nLinee del log file non valide: ");
    }
    else   /* aq == PREFORMATTED */
      fprintf(outf, "\nx%sCL%s", presep, presep);
    int3printf(outf, corrupt_lines, sepchar, 0);
  }
  if (other_lines > 0) {
    if (aq == HTML) {
      if (lang == ENGLISH)
	fprintf(outf, "\n<br><b>Unwanted logfile entries:</b> ");
      else if (lang == FRENCH)
	fprintf(outf, "\n<br><b>Entr&eacute;es non desir&eacute;es dans le fichier log :</b> ");
      else if (lang == GERMAN)
	fprintf(outf, "\n<br><b>Nichtverwendete Eintr&auml;ge in der Logdatei:</b> ");
      else if (lang == SPANISH)
	fprintf(outf, "\n<br><b>Entradas no deseadas en el fichero de log:</b> ");
      else if (lang == DANISH)
	fprintf(outf, "\n<br><b>U&oslash;nskede logfil indf&oslash;rsler:</b> ");
      else /* lang == ITALIAN */
        fprintf(outf, "\n<br><b>Registrazioni nel log file non desiderate:</b> ");
    }
    else if (aq == ASCII) {
      if (lang == ENGLISH)
	fprintf(outf, "\nUnwanted logfile entries: ");
      else if (lang == FRENCH)
	fprintf(outf, "\nEntrees non desirees dans le fichier log : ");
      else if (lang == GERMAN)
	fprintf(outf, "\nNichtverwendete Eintraege in der Logdatei: ");
      else if (lang == SPANISH)
	fprintf(outf, "\nEntradas no deseadas en el fichero de log: ");
      else if (lang == DANISH)
	fprintf(outf, "\nUoenskede logfil indfoersler: ");
      else /* lang == ITALIAN */
        fprintf(outf, "\nRegistrazioni nel log file non desiderate: ");
    }
    else   /* aq == PREFORMATTED */
      fprintf(outf, "\nx%sUL%s", presep, presep);
    int3printf(outf, other_lines, sepchar, 0);
  }
  if (byq) {
    if (aq == HTML) {
      if (lang == ENGLISH)
	fprintf(outf, "\n<br><b>Total data transferred:</b> ");
      else if (lang == FRENCH)
	fprintf(outf, "\n<br><b>Quantit&eacute; de traffic total :</b> ");
      else if (lang == GERMAN)
	fprintf(outf, "\n<br><b>Menge verschickter Daten:</b> ");
      else if (lang == SPANISH)
	fprintf(outf, "\n<br><b>Tr&aacute;fico total:</b> ");
      else if (lang == DANISH)
	fprintf(outf,
		"\n<br><b>Total m&aelig;ngde data overf&oslash;rt:</b> ");
      else /* lang == ITALIAN */
        fprintf(outf, "\n<br><b>Quantit&agrave; di traffico totale:</b> ");
    }
    else if (aq == ASCII) {
      if (lang == ENGLISH)
	fprintf(outf, "\nTotal data transferred: ");
      else if (lang == FRENCH)
	fprintf(outf, "\nQuantite de traffic total : ");
      else if (lang == GERMAN)
	fprintf(outf, "\nMenge verschickter Daten: ");
      else if (lang == SPANISH)
	fprintf(outf, "\nTrafico total: ");
      else if (lang == DANISH)
	fprintf(outf, "\nTotal maengde data overfoert: ");
      else /* lang == ITALIAN */
        fprintf(outf, "\nQuantita di traffico totale: ");
    }
    else   /* aq == PREFORMATTED */
      fprintf(outf, "\nx%sBT%s", presep, presep);
    bdivider = finddivider(total_bytes, bprefix);
    double3printf(outf, ROUND(total_bytes / bdivider), sepchar, 0);
    if (aq != PREFORMATTED)
      fprintf(outf, " %s%s%s", bprefix, 
              (lang == FRENCH)?"octet":"byte",
              (lang == ITALIAN)?"":"s");
    if (q7) {
      if (aq == PREFORMATTED)
	fprintf(outf, "\nx%sB7%s", presep, presep);
      else
	fprintf(outf, " (");
      bdivider = finddivider(total_bytes7, bprefix);
      double3printf(outf, ROUND(total_bytes7 / bdivider), sepchar, 0);
      if (aq != PREFORMATTED)
	fprintf(outf, " %s%s%s)", bprefix, 
                (lang == FRENCH)?"octet":"byte",
                (lang == ITALIAN)?"":"s");
    }
    if (totalmins > 30 && aq != PREFORMATTED) {
      if (aq == HTML) {
	if (lang == ENGLISH)
	  fprintf(outf, "\n<br><b>Average data transferred per day:</b> ");
	else if (lang == FRENCH)
	  fprintf(outf, "\n<br><b>Taux de traffic moyen par jour :</b> ");
	else if (lang == GERMAN)
	  fprintf(outf, "\n<br><b>Durchschnittliche Menge verschickter Daten pro Tag:</b> ");
	else if (lang == SPANISH)
	  fprintf(outf, "\n<br><b>Tr&aacute;fico medio por d&iacute;a:</b> ");
	else if (lang == DANISH)
	  fprintf(outf,
		  "\n<br><b>Overf&oslash;rte data pr. dag (gennemsnit):</b> ");
        else /* lang == ITALIAN */
          fprintf(outf, "\n<br><b>Traffico medio giornaliero:</b> ");
      }
      else if (lang == ENGLISH)
	fprintf(outf, "\nAverage data transferred per day: ");
      else if (lang == FRENCH)
	fprintf(outf, "\nTaux de traffic moyen par jour : ");
      else if (lang == GERMAN)
	fprintf(outf,
		"\nDurchschnittliche Menge verschickter Daten pro Tag: ");
      else if (lang == SPANISH)
	fprintf(outf, "\nTrafico medio por dia: ");
      else if (lang == DANISH)
	  fprintf(outf, "\nOverfoerte data pr. dag (gennemsnit): ");
      else /* lang == ITALIAN */
        fprintf(outf, "\nTraffico medio giornaliero: ");
  
      bdivider = finddivider((total_bytes * 1440) / (totalmins + 0.0),
			     bprefix);
      double3printf(outf, ROUND((total_bytes * 1440) / (totalmins + 0.0) / bdivider), sepchar, 0);
      fprintf(outf, " %s%s%s", bprefix,
              (lang == FRENCH)?"octet":"byte",
              (lang == ITALIAN)?"":"s");
      if (q7) {
	fprintf(outf, " (");
	bdivider = finddivider(total_bytes7 / 7.0, bprefix);
	double3printf(outf, ROUND(total_bytes7 / 7.0 / bdivider), sepchar, 0);
	fprintf(outf, " %s%s%s)", bprefix,
                (lang == FRENCH)?"octet":"byte",
                (lang == ITALIAN)?"":"s");
      }
    }
  }
  if (q7 && aq != PREFORMATTED) {
    if (aq == HTML)
      fprintf(outf, "\n<br>");
    else
      fprintf(outf, "\n");
    if (lang == ENGLISH)
      fprintf(outf, "(Figures in parentheses refer to the ");
    else if (lang == FRENCH)
      fprintf(outf, "(Les valeurs entre parenth%sses repr%ssentent les ",
	      (aq == HTML)?"&egrave;":"e", (aq == HTML)?"&eacute;":"e");
    else if (lang == GERMAN)
      fprintf(outf, "(Zahlen in Klammern beziehen sich auf die ");
    else if (lang == SPANISH)
      fprintf(outf, "(Los valores entre par%sntesis se refieren a los ",
	      (aq == HTML)?"&eacute;":"e");
    else if (lang == DANISH)
      fprintf(outf, "(Tallene i parentes refererer til ");
    else /* lang == ITALIAN */
      fprintf(outf, "(I valori in parentesi si riferiscono a");
      
    if (starttimec.code > totime.code) {
      if (lang == ENGLISH)
	strcpy(langstr, "days to");
      else if (lang == FRENCH)
	strcpy(langstr, "jours au");
      else if (lang == GERMAN)
	strcpy(langstr, "Tage bis");
      else if (lang == ITALIAN)
        strcpy(langstr, "giorni fino al");
      else if (lang == DANISH)
	strcpy(langstr, "dage til");
      else if (aq == HTML)  /* and lang == SPANISH */
	strcpy(langstr, "d&iacute;as hasta el");
      else /* SPANISH, not HTML */
	strcpy(langstr, "dias hasta el");
      fprintf(outf, "%s7 %s %02d-%s-%4d).", 
              (lang == ITALIAN)?"i ":"", langstr, totime.date,
	      monthname[totime.monthno], totime.year);
    }
    else if (lang == ENGLISH)
      fprintf(outf, "last 7 days).");
    else if (lang == FRENCH)
      fprintf(outf, "7 derniers jours).");
    else if (lang == GERMAN)
      fprintf(outf, "letzten 7 Tage).");
    else if (lang == SPANISH)
      fprintf(outf, "%sas).",
	      (aq == HTML)?"&uacute;ltimos 7 d&iacute;":"ultimos 7 di");
    else if (lang == DANISH)
      fprintf(outf, "de sidste 7 dage).");
    else /* lang == ITALIAN */
      fprintf(outf, "gli ultimi 7 giorni).");
  
  }
  
  if (aq == HTML && (mq || Wq || dq || Dq || hq || oq || Sq || iq || rq))
    gotos(outf, 'z');
  else if (aq == ASCII) {
    fprintf(outf, "\n");
    asciiline(outf);
  }
  else if (aq == PREFORMATTED)
    fprintf(outf, "\n");
}
