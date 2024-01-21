/*
** Copyright 1993 by Bruno Pages
**
** Permission to use, copy, and distribute for non-commercial purposes,
** is hereby granted without fee, providing that the above copyright
** notice appear in all copies and that both the copyright notice and this
** permission notice appear in supporting documentation.
** The software may be modified for your own purposes, but modified versions
** may not be distributed.
** This software is provided "as is" without any expressed or implied warranty.
**
**
*/


#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "justify.h"

#define MAX_WIDTH_DPY 10000 /*!!!*/

/* Les codes de fonction du fichier dtex */

#define Part		1
#define Chapter		2
#define Section		3
#define SubSection	4
#define SubSubSection	5
#define BeginItemize	6
#define EndItemize	7
#define BeginVerbatim	8
#define EndVerbatim	11
#define BeginCenter	12
#define EndCenter	14
#define BeginTabular	15
#define EndTabular	16
#define Index		17
#define IndeX		18
#define Accent		19

#define MaxCode		19

/* espace non proportionnel */

#define ConstantSpace (MaxCode + 1)

/* Donnees d'entree */

static XFontStruct * OutFont;
static int Width;
static FILE * fpdtex;


/* Les buffers xcoral */

static Buf * TextBuf;
static Buf * IndexBuf;
static Buf * TblBuf;

/* Memorisation des titres et numeros de ligne de la table des mat et index */

static int IndexNEntry;
static int IndexTabSize;
static int * IndexLineNumbers;
static char ** IndexNames = 0;

static int TblNEntry;
static int TblTabSize;
static int * TblLineNumbers;
static char ** TblNames = 0;

/* La ligne en cours de fabrication */

#define LINELENGTH 1024
static unsigned char Line[LINELENGTH];
static unsigned char * pLine;
static int LineWidth;

/* Largeurs */

static int charwidth[256];
static int SpaceWidth;
static int MinusWidth;
static int EqualWidth;
static int PipeWidth;
static int ConstantSpaceWidth;
static int ConstantSpaceNspaces;
static int ItemizeMarge;

/* */

static int LineNumber;
static int TblLineNumber;
static int PartNumber;
static int ChapterNumber;
static int SectionNumber;
static int SubSectionNumber;

static int Center = 0;
static int Itemize = 0;

void (*Functions[MaxCode + 1])();

/* */

FCT(static void, justifier_line_and_flush,()	);
FCT(static void, justifier_line,()		);
FCT(static void, inserer,(unsigned int)	);

extern FCT ( char *, stringdup,(char *)		);

#ifdef UNIXWARE
int strcasecmp ();

int
strcasecmp(s1, s2)
    register char *s1, *s2;
{
    register int c1, c2;

    while (*s1 && *s2) {
        c1 = tolower(*s1);
        c2 = tolower(*s2);
        if (c1 != c2)
            return (c1 - c2);
        s1++;
        s2++;
    }
    return (int) (*s1 - *s2);
}
#endif


static void inserer_space()
{
  inserer(' ');
}


static void flush_line()
{
  unsigned char * p;

  for (p = Line; p != pLine; p += 1)
    if (*p == ConstantSpace)
      InsertNchar(TextBuf, " ", 1);
    else
      InsertNchar(TextBuf, (char *) p, 1);

  LineWidth = 0;
  pLine = Line;
  LineNumber += 1;
  InsertNchar(TextBuf, "\n", 1);
}

static void insert_empty_lines(n)
     int n;
{
  if (pLine != Line)
    justifier_line_and_flush();
  else {
    /* Regarde combien de lignes vide sont deja ecrites */
    int deja = 0;
    char * cur = TextBuf->l_cur - 1;
    char * begin = TextBuf->top;

    while ((cur > begin) && (*--cur == '\n'))
      deja += 1;

    if ((n -= deja) <= 0)
      return;
  }

  LineNumber += n;
  
  while (n--)
    InsertNchar(TextBuf, "\n", 1);
}

static int round(x, y)
     int x, y;
{
  return ((x + y/2) / y);
}

static void fBeginVerbatim()
{
  unsigned int c;
  
  while ((c = fgetc(fpdtex)) != EndVerbatim)
    switch (c) {
    case '\n':
      justifier_line_and_flush();
      break;
    case '\t':
      c = round((pLine - Line + 7) % 8 * charwidth['_'], SpaceWidth);
      LineWidth += c * SpaceWidth;
      while (c--) *pLine++ = ConstantSpace;
      break;
    case ' ':
      {
	int n = 1;
	
	while ((c = fgetc(fpdtex)) == ' ') n += 1;
	ungetc(c, fpdtex);
	n = round(n * charwidth['_'], SpaceWidth);
	LineWidth += n * SpaceWidth;
	while (n--) *pLine++ = ConstantSpace;
      }
      break;
    default:
      LineWidth += charwidth[*pLine++ = c];
    }
}

static void new_tbl(lgn, title)
     int lgn;
     char * title;
{
  InsertNchar(TblBuf, title, strlen(title));

  if (TblNEntry == TblTabSize) {
    char ** newtblname = (char **) malloc(TblTabSize * 2 * sizeof(char *));
    int * newlgn = (int *) malloc(TblTabSize * 2 * sizeof(int));

    memcpy((char *) newtblname, (char *) TblNames,
	   TblTabSize * sizeof(char *));
    memcpy((char *) newlgn, (char *) TblLineNumbers, TblTabSize * sizeof(int));
    free(TblNames);
    free(TblLineNumbers);
    TblNames = newtblname;
    TblLineNumbers = newlgn;
    TblTabSize *= 2;
  }

  TblNames[TblNEntry] = title;
  TblLineNumbers[TblNEntry++] = lgn;
  TblLineNumber = lgn;
}

char * PartName[] = { "", "I", "II", "III", "iV", "V" };

static void fPart()
{
  int lg = 3;
  int width;
  int i;
  unsigned char title[64];
  int marge, margeminus;
  int nminus, nspace;

  insert_empty_lines(5);
  
  title[0] = '|';
  title[1] = title[2] = ' ';
  while (title[lg++] = fgetc(fpdtex));
  title[lg - 1] = ' ';
  title[lg++] = ' ';
  title[lg++] = '|';
  title[lg] = 0;
  
  marge = round(Width - (width = XTextWidth(OutFont, (char *) title, lg)),
		SpaceWidth * 2);

  nminus = round(width - 2 * PipeWidth, MinusWidth);
  nspace = round(width - 2 * PipeWidth, SpaceWidth);
  margeminus = round(Width - nminus * MinusWidth, SpaceWidth * 2);
  for (i = margeminus; i--; ) InsertNchar(TextBuf, " ", 1);
  for (i = nminus; i--; ) InsertNchar(TextBuf, "-", 1);
  InsertNchar(TextBuf, "\n", 1);
  
  for (i = marge; i--; ) InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, "|", 1);
  for (i = nspace; i--; ) InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, "|\n", 2);
  
  for (i = marge; i--; ) InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, (char *) title, lg);
  InsertNchar(TextBuf, "\n", 1);
  
  for (i = marge; i--; ) InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, "|", 1);
  for (i = nspace; i--; ) InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, "|\n", 2);

  for (i = margeminus; i--; ) InsertNchar(TextBuf, " ", 1);
  for (i = nminus; i--; ) InsertNchar(TextBuf, "-", 1);
  InsertNchar(TextBuf, "\n\n\n\n", 4);

  title[lg - 3] = 0;

  {
    char * t = (char *) malloc(lg - 4 + 7);
    
    PartNumber += 1;
    sprintf(t, "%s  %s\n", PartName[PartNumber], title + 3);
    new_tbl(LineNumber, t);
  }
  
  LineNumber += 8;
  ChapterNumber = SectionNumber = SubSectionNumber = 0;
}

static void fChapter()
{
  int c;
  int lg = 3;
  int width, twidth, chwidth;
  int marge, nminus, inwidth;
  int i;
  char title[64];
  char chapter[16];

  insert_empty_lines(3);
  
  title[0] = '|';
  title[1] = title[2] = ' ';
  while (title[lg++] = fgetc(fpdtex));
  title[lg - 1] = ' ';
  title[lg++] = ' ';
  title[lg] = 0;
  
  sprintf(chapter, "|  Chapter %d  ", ++ChapterNumber);

  chwidth = XTextWidth(OutFont, chapter, strlen(chapter));
  twidth = XTextWidth(OutFont, title, lg);
  width = (chwidth > twidth) ? chwidth : twidth;

  marge = round(MinusWidth, SpaceWidth);
  nminus = round(width - marge * SpaceWidth * 2 +  PipeWidth, MinusWidth);
  inwidth = round(width - PipeWidth, SpaceWidth);
    
  for (i = marge; i--; ) InsertNchar(TextBuf, " ", 1);
  for (i = nminus; i--; ) InsertNchar(TextBuf, "-", 1);
  InsertNchar(TextBuf, "\n", 1);
  
  InsertNchar(TextBuf, "|", 1);
  for (i = inwidth; i--; ) InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, "|\n", 2);

  InsertNchar(TextBuf, chapter, strlen(chapter));
  for (i = round(width - chwidth, SpaceWidth); i--; )
    InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, "|\n", 2);
  
  InsertNchar(TextBuf, "|", 1);
  for (i = inwidth; i--; ) InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, "|\n", 2);

  InsertNchar(TextBuf, title, strlen(title));
  for (i = round(width - twidth, SpaceWidth); i--; )
    InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, "|\n", 2);
  
  InsertNchar(TextBuf, "|", 1);
  for (i = inwidth; i--; ) InsertNchar(TextBuf, " ", 1);
  InsertNchar(TextBuf, "|\n", 2);

  for (i = marge; i--; ) InsertNchar(TextBuf, " ", 1);
  for (i = nminus; i--; ) InsertNchar(TextBuf, "-", 1);
  InsertNchar(TextBuf, "\n\n", 2);

  title[lg - 2] = 0;

  {
    char * t = (char *) malloc(lg - 3 + 4 + 8);
    
    sprintf(t, "%s %d  %s\n", PartName[PartNumber], ChapterNumber, title + 3);
    new_tbl(LineNumber, t);
  }
  
  LineNumber += 8;
  SectionNumber = SubSectionNumber = 0;
}

static int souligner(strlen, cwidth, lastchar)
     int strlen;
     int cwidth;
     int lastchar;
{
  int res = strlen / cwidth;
  char c = lastchar;

  return ((strlen % cwidth) > XTextWidth(OutFont, &c, 1) / 3) ? res + 1 : res;
}

static void fSection()
{
  int c;
  int i;
  char title[64];

  insert_empty_lines(2);
  
  sprintf(title, "%d.%d       ", ChapterNumber, ++SectionNumber);
  for (i = 8; title[i] = fgetc(fpdtex); )
    i += 1;
  
  InsertNchar(TextBuf, title, i);InsertNchar(TextBuf, "\n", 1);
  for (i = souligner(XTextWidth(OutFont, title, i), EqualWidth, title[i-1]);
       i--; )
    InsertNchar(TextBuf, "=", 1);
  InsertNchar(TextBuf, "\n\n", 2);

  {
    char * t = (char *) malloc(strlen(title) + 2 + 4);

    sprintf(t, "%s %d.%d  %s\n",
	    PartName[PartNumber], ChapterNumber, SectionNumber, title + 8);
    new_tbl(LineNumber, t);
  }
  
  LineNumber += 3;
  SubSectionNumber = 0;
}

static void fSubSection()
{
  int c;
  int i;
  char title[64];

  insert_empty_lines(2);
  
  sprintf(title, "%d.%d.%d  ",
	  ChapterNumber, SectionNumber, ++SubSectionNumber);
  for (i = strlen(title); title[i] = fgetc(fpdtex); )
    i += 1;
  
  InsertNchar(TextBuf, title, i); InsertNchar(TextBuf, "\n", 1);
  for (i = souligner(XTextWidth(OutFont, title, i), MinusWidth, title[i-1]);
       i--; )
    InsertNchar(TextBuf, "-", 1);
  InsertNchar(TextBuf, "\n\n", 2);

  {
    char * t = (char *) malloc(strlen(title) + 2 + 4);

    sprintf(t, "%s %s\n", PartName[PartNumber], title);
    new_tbl(LineNumber, t);
  }
  
  LineNumber += 3;
}

static void fSubSubSection()
{
  char title[64];
  int i = 0;

  insert_empty_lines(1);
  
  while (title[i] = fgetc(fpdtex))
    i += 1;
  if (i) {
    InsertNchar(TextBuf, title, i);
    InsertNchar(TextBuf, "\n", 1);

    for (i = round(XTextWidth(OutFont, title, i) + SpaceWidth,
		   XTextWidth(OutFont, "- ", 2));
	 i--; )
      InsertNchar(TextBuf, "- ", 2);

    InsertNchar(TextBuf, "\n\n", 2);
    TblLineNumber = LineNumber;
    LineNumber += 3;
  }
}

static void fBeginItemize()
{
  int c;
  
  Itemize += 1;
  Width -= ItemizeMarge;
  while ((c = fgetc(fpdtex)) != EndItemize)
    inserer(c);
  Itemize -= 1;
  Width += ItemizeMarge;
}

static void fBeginCenter()
{
  int c;

  insert_empty_lines(1);
  
  Center += 1;
  while ((c = fgetc(fpdtex)) != EndCenter)
    inserer(c);
  inserer('\n');
  Center -= 1;
  
  insert_empty_lines(1);
}

static void inserer_spaces(n)
     int n;
{
  if (n > 0) {
    LineWidth += n * SpaceWidth;
    while (n--) *pLine++ = ' ';
  }
}

static void fBeginTabular()
{
  char profile[16];
  int cwidth[16];
  int sizecont = 512, icont;
  char * contents = (char *) malloc(sizecont);
  int iprof, icwidth;
  int c;
  int saveWidth = Width;

  Width = MAX_WIDTH_DPY;

  /* nbre de col et les | */
  iprof = 0;
  while (profile[iprof] = fgetc(fpdtex))
    cwidth[iprof++] = 0;

  /* memorisation */
  icont = icwidth = 0;
  while ((c = fgetc(fpdtex)) != EndTabular)
    switch (c) {
    case 0:
    case '\n':
      *pLine++ = c;
      if ((icont + pLine - Line) >= sizecont) {
	char * newcont = (char *) malloc(sizecont * 2);
	
	memcpy(newcont, contents, sizecont);
	free(contents);
	contents = newcont;
	sizecont *= 2;
      }
      memcpy(contents + icont, (char *) Line, pLine - Line);
      icont += pLine - Line;
      
      if (cwidth[icwidth] < LineWidth)
	cwidth[icwidth] = LineWidth;
      icwidth = (c) ? 0 : icwidth + 1;
      pLine = Line;
      LineWidth = 0;
      break;
    default:
      inserer(c);
    }

  /* ecriture */
  {
    int larg = 0;
    
    LineWidth = 0;
    icwidth = iprof = 0;
    for (c = 0; c != icont; ) {
      int ch;

      if (profile[iprof] == '|') {
	inserer('|');
	inserer_spaces(2 * ConstantSpaceNspaces);
	iprof += 1;
	larg += 2 * ConstantSpaceWidth + PipeWidth;
      }

      switch (profile[iprof++]) {
      default:
	fprintf(stderr, "tabular{..%c..}\n", profile[iprof - 1]);
      case 'l':
	while (((ch = contents[c++]) != '\n') && ch)
	  if (ch == ConstantSpace) {
	    *pLine++ = ch;
	    LineWidth += SpaceWidth;
	  }
	  else
	    inserer(ch);
	larg += cwidth[icwidth++];
	inserer_spaces(round(larg - LineWidth, SpaceWidth));
      }
      inserer_spaces(2 * ConstantSpaceNspaces);
      larg += 2 * ConstantSpaceWidth;
      if (ch == '\n') {
	LineWidth = larg;
	if (profile[iprof] == '|') {
	  inserer('|');
	}
	if (Center) {
	  Width = saveWidth;

	  justifier_line();
	  Width = MAX_WIDTH_DPY;
	}
	else
	  justifier_line();
	
	larg = 0;
	icwidth = 0;
	iprof = 0;
      }
    }
  }

  free(contents);

  Width = saveWidth;
}

static void findex(ln)
    int ln;
{
  char name[64];

  {
    char * pname = name;
    
    while (*pname++ = fgetc(fpdtex));
  }
  if (IndexNEntry == IndexTabSize) {
    int * newln = (int *) malloc(IndexTabSize * sizeof(int) * 2);
    char ** newnames = (char **) malloc(IndexTabSize * sizeof(char *) * 2);

    memcpy((char *) newln, (char *) IndexLineNumbers,
	   IndexTabSize * sizeof(int));
    memcpy((char *) newnames, (char *) IndexNames,
	   IndexTabSize * sizeof(char *));
    free(IndexLineNumbers);
    free(IndexNames);
    IndexLineNumbers = newln;
    IndexNames = newnames;
    IndexTabSize *= 2;
  }
  IndexLineNumbers[IndexNEntry] = ln;
  IndexNames[IndexNEntry++] = stringdup(name);
}

static void fIndex()
{
  findex(LineNumber);
}

static void fIndeX()
{
  findex(TblLineNumber);
}


static void sortnames(low, high)
     int low, high;
{
  if (low < high) {
    int lo = low;
    int hi = high + 1;
    char * name = IndexNames[low];

    for (;;) {
      while ((++lo < hi) && (strcasecmp(IndexNames[lo], name) <= 0));
      while (strcasecmp(IndexNames[--hi], name) > 0);
      if (lo < hi) {
	char * aux;
	int laux;

	aux = IndexNames[lo];
	IndexNames[lo] = IndexNames[hi];
	IndexNames[hi] = aux;
	laux = IndexLineNumbers[lo];
	IndexLineNumbers[lo] = IndexLineNumbers[hi];
	IndexLineNumbers[hi] = laux;
      }
      else
	break;
    }
    {
      char * aux;
      int laux;
      
      aux = IndexNames[low];
      IndexNames[low] = IndexNames[hi];
      IndexNames[hi] = aux;
      laux = IndexLineNumbers[low];
      IndexLineNumbers[low] = IndexLineNumbers[hi];
      IndexLineNumbers[hi] = laux;
    }
    sortnames(low, hi - 1);
    sortnames(hi + 1, high);
  }
}

static void fAccent()
{
  int ch = fgetc(fpdtex);
  int codebase;

  switch(ch) {
  case 'a':
  case 'A':
    codebase = ch + 127;
    break;
  case 'u':
  case 'U':
    codebase = ch + 132;
    break;
  case 'c':
    fgetc(fpdtex);
    inserer(231);
    return;
  case 'C':
    fgetc(fpdtex);
    inserer(199);
    return;
  default:
    codebase = ch + 131;
  }

  switch(fgetc(fpdtex)) {
  case '`':
    inserer(codebase);
    break;
  case '\'':
    inserer(codebase + 1);
    break;
  case '^':
    inserer(codebase + 2);
    break;
  default:			/* '~' */
    inserer(codebase + 3);
    break;
  }
}

static void justifier_line_and_flush()
{
  if (pLine == Line)
    /* Ligne vide */
    flush_line();
  else
    do {
      unsigned char * p = pLine;
      
      justifier_line();
      if (pLine == p)
	/* Ligne ne pouvant tenir */
	flush_line();
    } while (pLine != Line);
}

static void justifier_line()
{
  if (Center) {
    int nspace = (Width - LineWidth) / SpaceWidth / 2;

    while (nspace-- > 0) InsertNchar(TextBuf, " ", 1);
    flush_line();
  }
  else {
    if (Itemize) {
      int i;
      
      for (i = (Itemize * ItemizeMarge) / SpaceWidth; i--; )
	InsertNchar(TextBuf, " ", 1);
    }
    if (LineWidth <= Width)
	flush_line();
    else {
      unsigned char * psup = pLine;
      int futurelwidth;

      /* On n'ecrira pas a cette ligne a partir du dernier blanc */
      {
	int width = LineWidth;
      
	do 
	  if (--psup == Line)
	      /* ne peut rien faire : on debordera */
	      return;
	  else
	    width -= charwidth[*psup];
	while ((*psup != ' ') || (width > Width));
      
	futurelwidth = LineWidth - width - SpaceWidth;
	LineWidth = width;
      }

      /* insersion de blancs */
      {
	int nspace = (Width - LineWidth) / SpaceWidth;
	int nplace = 0;
	unsigned char * pl = Line;

	if (Itemize)
	  while (strchr(" *+-", *pl))
	    pl += 1;
      
	while (pl != psup) if (*pl++ == ' ') nplace += 1;

	pl = Line;
	if (Itemize)
	  while (strchr(" *+-", *pl))
	    InsertNchar(TextBuf, (char *) pl++, 1);
      
	for (; pl != psup; pl += 1)
	  switch (*pl) {
	  case ConstantSpace:
	    InsertNchar(TextBuf, " ", 1);
	    break;
	  case ' ':
	    if (nspace) {
	      int n  = (nspace + nplace - 1) / nplace;
	      
	      nspace -= n;
	      nplace -= 1;
	      while (n--) InsertNchar(TextBuf, " ", 1);
	    }
	    InsertNchar(TextBuf, " ", 1);
	    break;
	  default:
	    InsertNchar(TextBuf, (char *) pl, 1);
	  }
      }
      
      InsertNchar(TextBuf, "\n", 1);
      LineNumber += 1;

      /* Recopie la fin de ligne en debut */
      {
	unsigned char * p = Line;
      
	if (psup != pLine) psup += 1;
	while (psup != pLine)
	    *p++ = *psup++;
	pLine = p;
      }
    
      LineWidth = futurelwidth;
    }
  }
}


static void inserer(c)
     unsigned int c;
{
#ifdef __osf__
  c &= 0xff ;
#endif

  if (c >= ' ') {
    if ((c != ' ') ||
	((pLine != Line) && (*(pLine - 1) != ' '))) {
      *pLine ++ = c;
      if ((LineWidth += charwidth[c]) > Width)
	justifier_line();
    }
  }
  else
    (Functions[c])();
}


static void init_justifier()
{
  static int init_faites = 0;

  LineNumber = 1;
  PartNumber = 0;
  ChapterNumber = 0;
  SectionNumber = 0;
  SubSectionNumber = 0;

  IndexNEntry = 0;
  TblNEntry = 0;
  
  {
    int i;
    unsigned char c;
  
    for (i = 0; i != 256; i += 1) {
      unsigned char c = i;
      
      charwidth[c] = XTextWidth(OutFont, (char *) &c, 1);
    }
  }

  SpaceWidth = charwidth[' '];
  MinusWidth = charwidth['-'];
  EqualWidth = charwidth['='];
  PipeWidth = charwidth['|'];
  ConstantSpaceNspaces = round(charwidth['_'], SpaceWidth);
  ConstantSpaceWidth = ConstantSpaceNspaces * SpaceWidth;
  charwidth[ConstantSpace] = ConstantSpaceWidth;
  ItemizeMarge = MinusWidth * 4;

  if (! init_faites) {
    init_faites = 1;
    Functions[Part] = fPart;
    Functions[Chapter] = fChapter;
    Functions[Section] = fSection;
    Functions[SubSection] = fSubSection;
    Functions[SubSubSection] = fSubSubSection;
    Functions[BeginItemize] = fBeginItemize;
    Functions[BeginVerbatim] = fBeginVerbatim;
    Functions[BeginCenter] = fBeginCenter;
    Functions[BeginTabular] = fBeginTabular;
    Functions[Index] = fIndex;
    Functions[IndeX] = fIndeX;
    Functions[Accent] = fAccent;

    Functions['\t'] = inserer_space;
    Functions['\n'] = justifier_line_and_flush;

    IndexTabSize = 64;
    IndexLineNumbers = (int *) malloc(IndexTabSize * sizeof(int));
    IndexNames = (char **) malloc(IndexTabSize * sizeof(char *));

    TblTabSize = 64;
    TblLineNumbers = (int *) malloc(TblTabSize * sizeof(int));
    TblNames = (char **) malloc(TblTabSize * sizeof(char *));
  }
}

/*
 */

int FillManText(font, width, textbuf, indexbuf, tblbuf,
		 textnlig, indexnlig, tblnlig)
     XFontStruct * font;
     int width;
     Buf * textbuf;
     Buf * indexbuf;
     Buf * tblbuf;
     int * textnlig;
     int * indexnlig;
     int * tblnlig;
{
  Width = width;
  OutFont = font;
  TextBuf = textbuf;
  IndexBuf = indexbuf;
  TblBuf = tblbuf;

  fpdtex = 0;
  
  {
    char * smaclib = (char *) getenv ("XCORAL_SMACLIB");
    
    if (smaclib) {
      char * filename = (char *) malloc(strlen(smaclib) + 10);

      sprintf(filename, "%s/man.dtex", smaclib);
      fpdtex = fopen(filename, "r");
      free(filename);
    }
  }
  
  if (! fpdtex) {
    char * filename = (char *) malloc(strlen(XCORAL_LIB_DIR) + 10);

    sprintf(filename, "%s/man.dtex", XCORAL_LIB_DIR);
    if (! (fpdtex = fopen(filename, "r"))) {
      free(filename);
      fprintf(stderr, "Error : cannot open %s\n", filename);
      return 0;
    }
    free(filename);
  }
  
  init_justifier();
  
  {
    int c;
    
    /* Saute les \n initiaux */
    while ((c = fgetc(fpdtex)) == '\n')
      ;

    /* Justifie */
  
    pLine = Line;
    do
      inserer(c); 
    while ((c = fgetc(fpdtex)) != EOF);
    flush_line();
  }

  fclose(fpdtex);

  /* trie la table des index */
  sortnames(0, IndexNEntry - 1);

  {
    int i;

    for (i = 0; i != IndexNEntry; i += 1) {
      InsertNchar(IndexBuf, IndexNames[i], strlen(IndexNames[i]));
      InsertNchar(IndexBuf, "\n", 1);
    }
  }

  *textnlig = LineNumber;
  *indexnlig = IndexNEntry + 1;		/* + 1 : la derniere ligne vide */
  *tblnlig = TblNEntry + 1;		/* + 1 : la derniere ligne vide */

  return 1;
}

int GetLineManFromIndex(index_line)
     int index_line;
{
  return IndexLineNumbers[index_line - 1];
}

int GetLineManFromToc(index_line)
     int index_line;
{
  return TblLineNumbers[index_line - 1];
}

static int lower_char(c)
    int c;
{
  return ((c >= 'A') && (c <= 'Z'))
    ? c - 'A' + 'a'
    : c;
}

int FirstIndexLineBeginningWith(code)
    int code;
{
  int i;
  
  code = lower_char(code);
    
  for (i = 0; i != IndexNEntry; i += 1)
    if (lower_char(*(IndexNames[i])) >= code)
      return (lower_char(*(IndexNames[i])) == code) ? i + 1 : 0;
  
  return 0;
}

