/*@z24.c:Print Service:PrintInit()@*******************************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.08)                       */
/*  COPYRIGHT (C) 1991, 1996 Jeffrey H. Kingston                             */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@cs.usyd.edu.au)                                */
/*  Basser Department of Computer Science                                    */
/*  The University of Sydney 2006                                            */
/*  AUSTRALIA                                                                */
/*                                                                           */
/*  This program is free software; you can redistribute it and/or modify     */
/*  it under the terms of the GNU General Public License as published by     */
/*  the Free Software Foundation; either version 1, or (at your option)      */
/*  any later version.                                                       */
/*                                                                           */
/*  This program is distributed in the hope that it will be useful,          */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/*  GNU General Public License for more details.                             */
/*                                                                           */
/*  You should have received a copy of the GNU General Public License        */
/*  along with this program; if not, write to the Free Software              */
/*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                */
/*                                                                           */
/*  FILE:         z24.c                                                      */
/*  MODULE:       Print Service                                              */
/*  EXTERNS:      PrintInit(), PrintBeforeFirst(), PrintBetween(),           */
/*                PrintWord(), PrintAfterLast(), CoordTranslate(),           */
/*                CoordRotate(), CoordScale(), SaveGraphicState(),           */
/*                RestoreGraphicState(), PrintGraphicObject(),               */
/*                DefineGraphicNames(), PrintGraphicInclude()                */
/*                                                                           */
/*  This module implements the PostScript back end.                          */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define	StartUpResource	"LoutStartUp"
#define DEFAULT_XHEIGHT 500
#define	NO_FONT		0		/* actually stolen from z37.c        */
#define	NO_COLOUR	0

#define printnum(x, fp)							\
{ char buff[20];  register int i, y;					\
  if( x >= 0 )  y = x;							\
  else { y = -x; putc(CH_MINUS, fp); }					\
  i = 0;								\
  do { buff[i++] = numtodigitchar(y % 10);				\
     } while( (y = (y / 10)) > 0 );					\
  do { --i; putc(buff[i], fp);						\
     } while( i );							\
}

static FILE	*out_fp;		/* output file                       */

/* these variables used by PLAINTEXT back end only                           */
static int	hsize;			/* horizontal size of page in chars  */
static int	vsize;			/* vertical size of page in chars    */
static FULL_CHAR *page;			/* the page (two-dim array of chars) */

/* these variables used by POSTSCRIPT back end only                          */
static FONT_NUM		currentfont;	/* font of most recent atom          */
static COLOUR_NUM	currentcolour;	/* colour of most recent atom        */
static short		currentxheight2;/* half xheight in current font      */
static BOOLEAN		cpexists;	/* true if a current point exists    */
static LENGTH		currenty;	/* if cpexists, its y coordinate     */
static int		wordcount;	/* atoms printed since last newline  */
static int		pagecount;	/* total number of pages printed     */
static BOOLEAN		prologue_done;	/* TRUE after prologue is printed    */
static OBJECT		needs;		/* Resource needs of included EPSFs  */
static OBJECT		supplied;	/* Resources supplied by this file   */


/*****************************************************************************/
/*                                                                           */
/*  PrintInit(file_ptr)                                                      */
/*                                                                           */
/*  Initialise this module.  Output is to go to FILE file_ptr.               */
/*                                                                           */
/*****************************************************************************/

void PrintInit(FILE *file_ptr)
{ debug0(DGP, D, "PrintInit()");
  out_fp = file_ptr;  prologue_done = FALSE;
  currentfont = NO_FONT;
  currentcolour = NO_COLOUR;
  cpexists = FALSE;
  wordcount = pagecount = 0;
  New(needs, ACAT);
  New(supplied, ACAT);
  debug0(DGP, D, "PrintInit returning.");
}


/*@::PrintBeforeFirst@********************************************************/
/*                                                                           */
/*  PrintBeforeFirst(h, v, label)                                            */
/*                                                                           */
/*  This procedure is called just before starting to print the first         */
/*  component of the output.  Its size is h, v, and label is the page        */
/*  label to attach to the %%Page comment.                                   */
/*                                                                           */
/*  If BackEnd is PLAINTEXT, this procedure obtains a two-dimensional array  */
/*  of characters large enough to hold the first component, and clears it.   */
/*                                                                           */
/*  If BackEnd is POSTSCRIPT, this procedure generates the PostScript        */
/*  prologue, augmented with any @PrependGraphic or @SysPrependGraphic       */
/*  files specified by the user.  The following PostScript operators are     */
/*  defined:                                                                 */
/*                                                                           */
/*      scale_factor  fnt       scale and set font                           */
/*      x_coordinate  x         move to x_coordinate, current y coordinate   */
/*      string        s         show string                                  */
/*      number        in        result is number inches                      */
/*      number        cm        result is number centimetres                 */
/*      number        pt        result is number points                      */
/*      number        sp        result is number spaces                      */
/*      number        vs        result is number vspaces                     */
/*      number        ft        result is number font-sizes                  */
/*                                                                           */
/*  as well as LoutGraphic, for use with the @Graphic operator:              */
/*                                                                           */
/*      xsize ysize xmark ymark fr vs sp LoutGraphic -                       */
/*                                                                           */
/*  Define xmark, ymark, xsize, ysize to be the positions of                 */
/*  these features of x, and define symbols ft, vs and sp                    */
/*  to be the current font size, line separation, and space width.           */
/*                                                                           */
/*****************************************************************************/

void PrintBeforeFirst(LENGTH h, LENGTH v, FULL_CHAR *label)
{ FILE_NUM fnum;  int i, j;
  debug2(DGP, DD, "PrintBeforeFirst(%d, %d)", h, v);

  switch( BackEnd )
  {
    case PLAINTEXT:

      /* get a new page[] and clear it */
      hsize = ceiling(h, PlainCharWidth);
      vsize = ceiling(v, PlainCharHeight);
      debug2(DGP, DD, "  PlainCharWidth: %d;  PlainCharHeight: %d",
	PlainCharWidth, PlainCharHeight);
      ifdebug(DMA, D, DebugRegisterUsage(MEM_PAGES, 1,
	hsize * vsize * sizeof(FULL_CHAR)));
      debug2(DGP, DD, "  PrintBeforeFirst allocating %d by %d", hsize, vsize);
      page = (FULL_CHAR *) malloc(hsize * vsize * sizeof(FULL_CHAR));
      for( i = 0;  i < vsize;  i++ )
	for( j = 0;  j < hsize;  j++ )
	  page[i*hsize + j] = ' ';
      break;


    case POSTSCRIPT:

      /* print header comments for PostScript DSC 3.0 output */
      if( Encapsulated )
        fprintf(out_fp, "%%!PS-Adobe-3.0 EPSF-3.0\n");
      else
        fprintf(out_fp, "%%!PS-Adobe-3.0\n");
      fprintf(out_fp, "%%%%Creator: %s\n", LOUT_VERSION);
      fprintf(out_fp, "%%%%CreationDate: %s", TimeString());
      fprintf(out_fp, "%%%%DocumentData: Binary\n");
      fprintf(out_fp, "%%%%DocumentNeededResources: (atend)\n");
      fprintf(out_fp, "%%%%DocumentSuppliedResources: (atend)\n");
      fprintf(out_fp, "%%%%Pages: (atend)\n");
      fprintf(out_fp, "%%%%BoundingBox: 0 0 %d %d\n", h/PT, v/PT);
      fprintf(out_fp, "%%%%EndComments\n\n");

      /* print procedure definitions part of header */
      fprintf(out_fp, "%%%%BeginProlog\n");
      fprintf(out_fp, "%%%%BeginResource: procset %s\n", StartUpResource);
      fprintf(out_fp, "/m  { 3 1 roll moveto show } bind def\n");
      fprintf(out_fp, "/s  { exch currentpoint exch pop moveto show } bind def\n");
      fprintf(out_fp, "/k  { exch neg 0 rmoveto show } bind def\n");
      fprintf(out_fp, "/ul { gsave setlinewidth dup 3 1 roll\n");
      fprintf(out_fp, "      moveto lineto stroke grestore } bind def\n");
      fprintf(out_fp, "/in { %d mul } def\n", IN);
      fprintf(out_fp, "/cm { %d mul } def\n", CM);
      fprintf(out_fp, "/pt { %d mul } def\n", PT);
      fprintf(out_fp, "/em { %d mul } def\n", EM);
      fprintf(out_fp, "/sp { louts mul } def\n");
      fprintf(out_fp, "/vs { loutv mul } def\n");
      fprintf(out_fp, "/ft { loutf mul } def\n");
      fprintf(out_fp, "/dg {           } def\n\n");

      fputs("/LoutGraphic {\n",					  out_fp);
      fputs("  /louts exch def\n",				  out_fp);
      fputs("  /loutv exch def\n",				  out_fp);
      fputs("  /loutf exch def\n",				  out_fp);
      fputs("  /ymark exch def\n",				  out_fp);
      fputs("  /xmark exch def\n",				  out_fp);
      fputs("  /ysize exch def\n",				  out_fp);
      fputs("  /xsize exch def\n} def\n\n",			  out_fp);

      /* print definition used by Lout output to recode fonts                */
      /* adapted from PostScript Language Reference Manual (2nd Ed), p. 275  */
      /* usage: /<fullname> <encodingvector> /<originalname> LoutRecode -    */

      fputs("/LoutFont\n",                                            out_fp);
      fputs("{ findfont exch scalefont setfont\n",                    out_fp);
      fputs("} bind def\n\n",					      out_fp);

      fputs("/LoutRecode {\n",                                        out_fp);
      fputs("  { findfont dup length dict begin\n",                   out_fp);
      fputs("    {1 index /FID ne {def} {pop pop} ifelse} forall\n",  out_fp);
      fputs("    /Encoding exch def\n",                               out_fp);
      fputs("    currentdict end definefont pop\n",                   out_fp);
      fputs("  }\n",                                                  out_fp);
      fputs("  stopped pop\n",                                         out_fp);
      fputs("} bind def\n\n",                                         out_fp);

      /* print definitions used by Lout output when including EPSF files     */
      /* copied from PostScript Language Reference Manual (2nd Ed.), p. 726  */

      fputs("/BeginEPSF {\n",					  out_fp);
      fputs("  /LoutEPSFState save def\n",			  out_fp);
      fputs("  /dict_count countdictstack def\n",		  out_fp);
      fputs("  /op_count count 1 sub def\n",			  out_fp);
      fputs("  userdict begin\n",				  out_fp);
      fputs("  /showpage { } def\n",				  out_fp);
      fputs("  0 setgray 0 setlinecap\n",			  out_fp);
      fputs("  1 setlinewidth 0 setlinejoin\n",			  out_fp);
      fputs("  10 setmiterlimit [] 0 setdash newpath\n",	  out_fp);
      fputs("  /languagelevel where\n",				  out_fp);
      fputs("  { pop languagelevel\n",				  out_fp);
      fputs("    1 ne\n",					  out_fp);
      fputs("    { false setstrokeadjust false setoverprint\n",	  out_fp);
      fputs("    } if\n",					  out_fp);
      fputs("  } if\n",						  out_fp);
      fputs("} bind def\n\n",					  out_fp);

      fputs("/EndEPSF {\n",					  out_fp);
      fputs("  count op_count sub { pop } repeat\n",		  out_fp);
      fputs("  countdictstack dict_count sub { end } repeat\n",	  out_fp);
      fputs("  LoutEPSFState restore\n",			  out_fp);
      fputs("} bind def\n",					  out_fp);

      fputs("%%EndResource\n\n",				  out_fp);

      /* print encoding vectors as resources */
      MapPrintEncodings(out_fp);

      /* print prepend files (assumed to be organized as DSC 3.0 Resources) */
      for( fnum=FirstFile(PREPEND_FILE);  fnum!=NO_FILE;  fnum=NextFile(fnum) )
      { FULL_CHAR buff[MAX_BUFF];  FILE *fp;
        if( (fp = OpenFile(fnum, FALSE, FALSE)) == null )
          Error(24, 1, "cannot open %s file %s",
	    WARN, PosOfFile(fnum), KW_PREPEND, FileName(fnum));
        else if( StringFGets(buff, MAX_BUFF, fp) == NULL )
          Error(24, 2, "%s file %s is empty",
	    WARN, PosOfFile(fnum), KW_PREPEND, FileName(fnum));
        else
        {
          if( StringBeginsWith(buff, AsciiToFull("%%BeginResource:")) )
	  { OBJECT tmp;
	    tmp = MakeWord(WORD, &buff[strlen("%%BeginResource:")], no_fpos);
	    Link(supplied, tmp);
	  }
	  else
	    Error(24, 3, "%s file %s lacks PostScript BeginResource comment",
	      WARN, PosOfFile(fnum), KW_PREPEND, FileName(fnum));
          StringFPuts(buff, out_fp);
          fprintf(out_fp, "%% %s file %s\n", KW_PREPEND, FileName(fnum));
          while( StringFGets(buff, MAX_BUFF, fp) != NULL )
	    StringFPuts(buff, out_fp);
	  fprintf(out_fp, "\n");
	  fclose(fp);
        }
      }

      fputs("%%EndProlog\n\n", out_fp);
      fputs("%%BeginSetup\n", out_fp);
      FontPrintPageSetup(out_fp);
      fputs("%%EndSetup\n\n", out_fp);
      fprintf(out_fp, "%%%%Page: %s %d\n", label, ++pagecount);
      fprintf(out_fp, "%%%%BeginPageSetup\n");
      FontPrintPageResources(out_fp);
      FontAdvanceCurrentPage();
      fprintf(out_fp, "/pgsave save def\n");
      fprintf(out_fp, "%.4f dup scale %d setlinewidth\n", 1.0 / PT, PT/2);
      fprintf(out_fp, "%%%%EndPageSetup\n\n");
      break;

 } /* end switch */
 prologue_done = TRUE;
} /* end PrintBeforeFirst */


/*@::PrintBetween(), EightBitsToPrintForm[]@**********************************/
/*                                                                           */
/*  PrintBetween(h, v, label)                                                */
/*                                                                           */
/*  Start a new output component, of size h by v; label is the page label    */
/*  to attach to the %%Page comment.                                         */
/*                                                                           */
/*****************************************************************************/

void PrintBetween(LENGTH h, LENGTH v, FULL_CHAR *label)
{ int new_hsize, new_vsize, i, j, jmax;
  debug2(DGP, DD, "PrintBetween(%d, %d)", h, v);

  switch( BackEnd )
  {
    case PLAINTEXT:

      /* print the page that has just ended */
      ifdebug(DGP, D,
	putc('+', out_fp);
	for( j = 0;  j < hsize;  j++ )  putc('-', out_fp);
	putc('+', out_fp);
	putc('\n', out_fp);
      );
      for( i = vsize - 1;  i >= 0;  i-- )
      { ifdebug(DGP, D, putc('|', out_fp));
	for( jmax = hsize-1;  jmax >= 0 && page[i*hsize+jmax] == ' ';  jmax--);
	ifdebug(DGP, D, jmax = hsize - 1);
	for( j = 0;  j <= jmax;  j++ )
	  putc(page[i*hsize + j], out_fp);
        ifdebug(DGP, D, putc('|', out_fp));
	putc('\n', out_fp);
      }
      ifdebug(DGP, D,
	putc('+', out_fp);
	for( j = 0;  j < hsize;  j++ )  putc('-', out_fp);
	putc('+', out_fp);
	putc('\n', out_fp);
      );

      /* separate the page from the next one with a form-feed if required */
      if( PlainFormFeed ) putc('\f', out_fp);

      /* if page size has changed, get a new page[] array */
      new_hsize = ceiling(h, PlainCharWidth);
      new_vsize = ceiling(v, PlainCharHeight);
      if( new_hsize != hsize || new_vsize != vsize )
      {
        ifdebug(DMA, D, DebugRegisterUsage(MEM_PAGES, -1,
	  -hsize * vsize * sizeof(FULL_CHAR)));
	free(page);
	hsize = new_hsize;
	vsize = new_vsize;
        debug2(DGP, DD, "  PrintBetween allocating %d by %d", hsize, vsize);
        ifdebug(DMA, D, DebugRegisterUsage(MEM_PAGES, 1,
	  hsize * vsize * sizeof(FULL_CHAR)));
        page = (FULL_CHAR *) malloc(hsize * vsize * sizeof(FULL_CHAR));
      }

      /* clear page[] for the new page just beginning */
      for( i = 0;  i < vsize;  i++ )
	for( j = 0;  j < hsize;  j++ )
	  page[i*hsize + j] = ' ';
      break;


    case POSTSCRIPT:

      fprintf(out_fp, "\npgsave restore\nshowpage\n");
      cpexists = FALSE;
      currentfont = NO_FONT;
      currentcolour = NO_COLOUR;
      if( Encapsulated )
      { PrintAfterLast();
        Error(24, 4, "truncating -EPS document at end of first page",
	  FATAL, no_fpos);
      }
      fprintf(out_fp, "\n%%%%Page: %s %d\n", label, ++pagecount);
      fprintf(out_fp, "%%%%BeginPageSetup\n");
      FontPrintPageResources(out_fp);
      fprintf(out_fp, "/pgsave save def\n");
      FontPrintPageSetup(out_fp);
      FontAdvanceCurrentPage();
      fprintf(out_fp, "%.4f dup scale %d setlinewidth\n", 1.0 / PT, PT/2);
      fprintf(out_fp, "%%%%EndPageSetup\n");
      wordcount = 0;
      break;

  } /* end switch */
} /* end PrintBetween */


/*@::EightBitToPrintForm()@***************************************************/
/*                                                                           */
/*  static char *EightBitToPrintForm[]                                       */
/*                                                                           */
/*  Given 8-bit character i, returns a string of characters that will be     */
/*  interpreted by PostScript as character i when read within a string.      */
/*                                                                           */
/*      CHAR_OUT==1    Printable ASCII literal, others as escape sequences   */
/*      CHAR_OUT==2    Printable ISO-LATIN-1 literal, others escaped         */
/*                                                                           */
/*****************************************************************************/

static char *EightBitToPrintForm[] = {
#if CHAR_OUT==0
    "",      "\\001", "\\002", "\\003", "\\004", "\\005", "\\006", "\\007",
    "\\010", "\\011", "\\012", "\\013", "\\014", "\\015", "\\016", "\\017",
    "\\020", "\\021", "\\022", "\\023", "\\024", "\\025", "\\026", "\\027",
    "\\030", "\\031", "\\032", "\\033", "\\034", "\\035", "\\036", "\\037",
    " ",     "!",     "\"",    "#",     "$",     "%",     "&",     "'",
    "\\(",   "\\)",   "*",     "+",     ",",     "-",     ".",     "/",
    "0",     "1",     "2",     "3",     "4",     "5",     "6",     "7",
    "8",     "9",     ":",     ";",     "<",     "=",     ">",     "?",
    "@",     "A",     "B",     "C",     "D",     "E",     "F",     "G",
    "H",     "I",     "J",     "K",     "L",     "M",     "N",     "O",
    "P",     "Q",     "R",     "S",     "T",     "U",     "V",     "W",
    "X",     "Y",     "Z",     "[",     "\\\\",  "]",     "^",     "_",
    "`",     "a",     "b",     "c",     "d",     "e",     "f",     "g",
    "h",     "i",     "j",     "k",     "l",     "m",     "n",     "o",
    "p",     "q",     "r",     "s",     "t",     "u",     "v",     "w",
    "x",     "y",     "z",     "{",     "|",     "}",     "~",     "\\177",
    "\\200", "\\201", "\\202", "\\203", "\\204", "\\205", "\\206", "\\207",
    "\\210", "\\211", "\\212", "\\213", "\\214", "\\215", "\\216", "\\217",
    "\\220", "\\221", "\\222", "\\223", "\\224", "\\225", "\\226", "\\227",
    "\\230", "\\231", "\\232", "\\233", "\\234", "\\235", "\\236", "\\237",
    "\\240", "\\241", "\\242", "\\243", "\\244", "\\245", "\\246", "\\247",
    "\\250", "\\251", "\\252", "\\253", "\\254", "\\255", "\\256", "\\257",
    "\\260", "\\261", "\\262", "\\263", "\\264", "\\265", "\\266", "\\267",
    "\\270", "\\271", "\\272", "\\273", "\\274", "\\275", "\\276", "\\277",
    "\\300", "\\301", "\\302", "\\303", "\\304", "\\305", "\\306", "\\307",
    "\\310", "\\311", "\\312", "\\313", "\\314", "\\315", "\\316", "\\317",
    "\\320", "\\321", "\\322", "\\323", "\\324", "\\325", "\\326", "\\327",
    "\\330", "\\331", "\\332", "\\333", "\\334", "\\335", "\\336", "\\337",
    "\\340", "\\341", "\\342", "\\343", "\\344", "\\345", "\\346", "\\347",
    "\\350", "\\351", "\\352", "\\353", "\\354", "\\355", "\\356", "\\357",
    "\\360", "\\361", "\\362", "\\363", "\\364", "\\365", "\\366", "\\367",
    "\\370", "\\371", "\\372", "\\373", "\\374", "\\375", "\\376", "\\377"
#else
#if CHAR_OUT==1
    "",      "\\001", "\\002", "\\003", "\\004", "\\005", "\\006", "\\007",
    "\\010", "\\011", "\\012", "\\013", "\\014", "\\015", "\\016", "\\017",
    "\\020", "\\021", "\\022", "\\023", "\\024", "\\025", "\\026", "\\027",
    "\\030", "\\031", "\\032", "\\033", "\\034", "\\035", "\\036", "\\037",
    " ",     "!",     "\"",    "#",     "$",     "%",     "&",     "'",
    "\\(",   "\\)",   "*",     "+",     ",",     "-",     ".",     "/",
    "0",     "1",     "2",     "3",     "4",     "5",     "6",     "7",
    "8",     "9",     ":",     ";",     "<",     "=",     ">",     "?",
    "@",     "A",     "B",     "C",     "D",     "E",     "F",     "G",
    "H",     "I",     "J",     "K",     "L",     "M",     "N",     "O",
    "P",     "Q",     "R",     "S",     "T",     "U",     "V",     "W",
    "X",     "Y",     "Z",     "[",     "\\\\",  "]",     "^",     "_",
    "`",     "a",     "b",     "c",     "d",     "e",     "f",     "g",
    "h",     "i",     "j",     "k",     "l",     "m",     "n",     "o",
    "p",     "q",     "r",     "s",     "t",     "u",     "v",     "w",
    "x",     "y",     "z",     "{",     "|",     "}",     "~",     "\\177",
    "\\200", "\\201", "\\202", "\\203", "\\204", "\\205", "\\206", "\\207",
    "\\210", "\\211", "\\212", "\\213", "\\214", "\\215", "\\216", "\\217",
    "\220",  "\221",  "\222",  "\223",  "\224",  "\225",  "\226",  "\227",
    "\230",  "\\231", "\232",  "\233",  "\\234", "\235",  "\236",  "\237",
    "\240",  "\241",  "\242",  "\243",  "\244",  "\245",  "\246",  "\247",
    "\250",  "\251",  "\252",  "\253",  "\254",  "\255",  "\256",  "\257",
    "\260",  "\261",  "\262",  "\263",  "\264",  "\265",  "\266",  "\267",
    "\270",  "\271",  "\272",  "\273",  "\274",  "\275",  "\276",  "\277",
    "\300",  "\301",  "\302",  "\303",  "\304",  "\305",  "\306",  "\307",
    "\310",  "\311",  "\312",  "\313",  "\314",  "\315",  "\316",  "\317",
    "\320",  "\321",  "\322",  "\323",  "\324",  "\325",  "\326",  "\327",
    "\330",  "\331",  "\332",  "\333",  "\334",  "\335",  "\336",  "\337",
    "\340",  "\341",  "\342",  "\343",  "\344",  "\345",  "\346",  "\347",
    "\350",  "\351",  "\352",  "\353",  "\354",  "\355",  "\356",  "\357",
    "\360",  "\361",  "\362",  "\363",  "\364",  "\365",  "\366",  "\367",
    "\370",  "\371",  "\372",  "\373",  "\374",  "\375",  "\376",  "\377"
#else
If you are trying to compile this you have the wrong CHAR_OUT value!
#endif
#endif
};


/*****************************************************************************/
/*                                                                           */
/*  KernLength(fnum, ch1, ch2, res)                                          */
/*                                                                           */
/*  Set res to the kern length between ch1 and ch2 in font fnum, or 0 if     */
/*  none.                                                                    */
/*                                                                           */
/*****************************************************************************/

#define KernLength(fnum, mp, ch1, ch2, res)				\
{ int ua_ch1 = mp[ch1];							\
  int ua_ch2 = mp[ch2];							\
  int i = finfo[fnum].kern_table[ua_ch1], j;				\
  if( i == 0 )  res = 0;						\
  else									\
  { FULL_CHAR *kc = finfo[fnum].kern_chars;				\
    for( j = i;  kc[j] > ua_ch2;  j++ );				\
    res = (kc[j] == ua_ch2) ?						\
      finfo[fnum].kern_sizes[finfo[fnum].kern_value[j]] : 0;		\
  }									\
} /* end KernLength */


/*@::PrintWord()@*************************************************************/
/*                                                                           */
/*  PrintWord(x, hpos, vpos)                                                 */
/*                                                                           */
/*  Print non-empty word x; its marks cross at the point (hpos, vpos).       */
/*                                                                           */
/*****************************************************************************/

void PrintWord(OBJECT x, int hpos, int vpos)
{ FULL_CHAR *p, *q, *a, *b, *lig, *unacc, *acc;
  int i, h, v, ksize;  char command;  MAPPING m;

  debug5(DGP, DD, "PrintWord( %s, %d, %d ) font %d colour %d", string(x),
	hpos, vpos, word_font(x), word_colour(x));

  switch( BackEnd )
  {
    case PLAINTEXT:

      h = ((float) hpos / PlainCharWidth) + 0.5;
      v = ((float) vpos / PlainCharHeight) + 0.5;
      p = &page[v*hsize + h];
      for( i = 0;  string(x)[i] != '\0';  i++ )
	*p++ = string(x)[i];
      break;


    case POSTSCRIPT:

      /* if font is different to previous word then print change */
      if( word_font(x) != currentfont )
      { currentfont = word_font(x);
        currentxheight2 = FontHalfXHeight(currentfont);
        fprintf(out_fp, "%hd %s", FontSize(currentfont, x),
          FontName(currentfont));
        if( ++wordcount >= 5 )
        { fputs("\n", out_fp);
          wordcount = 0;
        }
        else fputs(" ", out_fp);
      }

      /* if colour is different to previous word then print change */
      if( word_colour(x) != currentcolour )
      { currentcolour = word_colour(x);
	if( currentcolour > 0 )
	{ fprintf(out_fp, "%s", ColourCommand(currentcolour));
          if( ++wordcount >= 5 )
          { fputs("\n", out_fp);
            wordcount = 0;
          }
          else fputs(" ", out_fp);
	}
      }

      /* move to coordinate of x */
      debug1(DGP, DDD, "  currentxheight2 = %d", currentxheight2);
      vpos = vpos - currentxheight2;
      if( cpexists && currenty == vpos )
      { printnum(hpos, out_fp);
        command = 's';
      }
      else
      { currenty = vpos;
        printnum(hpos, out_fp);
        fputs(" ", out_fp);
        printnum(currenty, out_fp);
        command = 'm';
        cpexists = TRUE;
      }

      /* convert ligature sequences into ligature characters */
      lig = finfo[word_font(x)].lig_table;
      p = q = string(x);
      do
      { 
        /* check for missing glyph (lig[] == 1) or ligatures (lig[] > 1) */
        if( lig[*q++ = *p++] )
        {
          if( lig[*(q-1)] == 1 ) continue;
          else
          { a = &lig[ lig[*(p-1)] + MAX_CHARS ];
            while( *a++ == *(p-1) )
            { b = p;
              while( *a == *b && *(a+1) != '\0' && *b != '\0' )  a++, b++;
              if( *(a+1) == '\0' )
              { *(q-1) = *a;
                p = b;
                break;
              }
              else
              { while( *++a );
                a++;
              }
            }
          }
        }
      } while( *p );
      *q = '\0';

      /* show string(x) */
      fputs("(", out_fp);
      p = string(x);
      fputs(EightBitToPrintForm[*p], out_fp);
      m = font_mapping(finfo[word_font(x)].font_table);
      unacc = MapTable[m]->map[MAP_UNACCENTED];
      acc   = MapTable[m]->map[MAP_ACCENT];
      for( p++;  *p;  p++ )
      { KernLength(word_font(x), unacc, *(p-1), *p, ksize);
        if( ksize != 0 )
        { fprintf(out_fp, ")%c %d(", command, -ksize);
          ++wordcount;
          command = 'k';
        }
	fputs(EightBitToPrintForm[*p], out_fp);
      }
      if( ++wordcount >= 5 )
      { fprintf(out_fp, ")%c\n", command);
        wordcount = 0;
      }
      else fprintf(out_fp, ")%c ", command);
      break;

  } /* end switch */
  debug0(DGP, DDD, "PrintWord returning");
} /* end PrintWord */


/*****************************************************************************/
/*                                                                           */
/*  PrintUnderline(fnum, xstart, xstop, ymk)                                 */
/*                                                                           */
/*  Draw an underline suitable for font fnum, from xstart to xstop at the    */
/*  appropriate distance below mark ymk.                                     */
/*                                                                           */
/*****************************************************************************/

void PrintUnderline(FONT_NUM fnum, LENGTH xstart, LENGTH xstop, LENGTH ymk)
{

  debug4(DGP, DD, "PrintUnderline(fnum %d, xstart %s, xstop %s, ymk %s )",
    fnum, EchoLength(xstart), EchoLength(xstop), EchoLength(ymk));

  switch( BackEnd )
  {
    case PLAINTEXT:

      /* do nothing */
      break;


    case POSTSCRIPT:

      fprintf(out_fp, "%d %d %d %d ul\n", xstart, xstop,
	ymk - finfo[fnum].underline_pos, finfo[fnum].underline_thick);
      break;

  }
  debug0(DGP, DD, "PrintUnderline returning.");
} /* end PrintUnderline */


/*@::PrintAfterLast(), CoordTranslate()@**************************************/
/*                                                                           */
/*  PrintAfterLast()                                                         */
/*                                                                           */
/*  Clean up this module and close output stream.                            */
/*                                                                           */
/*****************************************************************************/

void PrintAfterLast(void)
{ OBJECT x, link;  BOOLEAN first_need;  int i, j, jmax;
  if( prologue_done )
  { 
    switch( BackEnd )
    {
      case PLAINTEXT:

        /* print the page that has just ended (exists since prologue_done) */
	ifdebug(DGP, D,
	  putc('+', out_fp);
	  for( j = 0;  j < hsize;  j++ )  putc('-', out_fp);
	  putc('+', out_fp);
	  putc('\n', out_fp);
	);
        for( i = vsize - 1;  i >= 0;  i-- )
        { ifdebug(DGP, D, putc('|', out_fp));
	  for( jmax = hsize-1;  jmax >= 0 && page[i*hsize+jmax] == ' ';  jmax--);
	  ifdebug(DGP, D, jmax = hsize - 1);
	  for( j = 0;  j <= jmax;  j++ )
	    putc(page[i*hsize + j], out_fp);
          ifdebug(DGP, D, putc('|', out_fp));
	  putc('\n', out_fp);
        }
	ifdebug(DGP, D,
	  putc('+', out_fp);
	  for( j = 0;  j < hsize;  j++ )  putc('-', out_fp);
	  putc('+', out_fp);
	  putc('\n', out_fp);
	);
	break;
    

      case POSTSCRIPT:

        fprintf(out_fp, "\npgsave restore\nshowpage\n");
        fprintf(out_fp, "\n%%%%Trailer\n");

        /* print resource requirements (DSC 3.0 version) - fonts */
        first_need = FontNeeded(out_fp);

        /* print resource requirements (DSC 3.0 version) - included EPSFs  */
        for( link = Down(needs); link != needs; link = NextDown(link) )
        { Child(x, link);
          assert(is_word(type(x)), "PrintAfterLast: needs!" );
          fprintf(out_fp, "%s %s",
	    first_need ? "%%DocumentNeededResources:" : "%%+", string(x));
          first_need = FALSE;
        }

	/* print resources supplied */
	fprintf(out_fp,
	  "%%%%DocumentSuppliedResources: procset %s\n", StartUpResource);
	for( link = Down(supplied);  link != supplied;  link = NextDown(link) )
	{ Child(x, link);
	  fprintf(out_fp, "%%%%+ %s", string(x));
	}
        MapPrintResources(out_fp);

        fprintf(out_fp, "%%%%Pages: %d\n", pagecount);
        fprintf(out_fp, "%%%%EOF\n");
	break;

    } /* end switch */
  } /* end if prologue_done */
} /* end PrintAfterLast */


/*****************************************************************************/
/*                                                                           */
/*  CoordTranslate(xdist, ydist)                                             */
/*                                                                           */
/*  Translate coordinate system by the given x and y distances.              */
/*                                                                           */
/*****************************************************************************/

void CoordTranslate(LENGTH xdist, LENGTH ydist)
{ debug2(DRS,D,"CoordTranslate(%s, %s)",
    EchoLength(xdist), EchoLength(ydist));
  assert( BackEnd == POSTSCRIPT, "CoordTranslate: BackEnd!" );
  fprintf(out_fp, "%d %d translate\n", xdist, ydist);
  cpexists = FALSE;
  currentfont = NO_FONT;
  currentcolour = NO_COLOUR;
  debug0(DRS, D, "CoordTranslate returning.");
} /* end CoordTranslate */

/*@::CoordRotate(), CoordScale(), SaveGraphicsState(), etc.@******************/
/*                                                                           */
/*  CoordRotate(amount)                                                      */
/*                                                                           */
/*  Rotate coordinate system by given amount (in internal DG units)          */
/*                                                                           */
/*****************************************************************************/

void CoordRotate(LENGTH amount)
{ debug1(DRS, D, "CoordRotate(%.1f degrees)", (float) amount / DG);
  assert( BackEnd == POSTSCRIPT, "CoordRotate: BackEnd!" );
  fprintf(out_fp, "%.4f rotate\n", (float) amount / DG);
  cpexists = FALSE;
  currentfont = NO_FONT;
  currentcolour = NO_COLOUR;
  debug0(DRS, D, "CoordRotate returning.");
} /* end CoordRotate */


/*****************************************************************************/
/*                                                                           */
/*  CoordScale(ratio, dim)                                                   */
/*                                                                           */
/*  Scale coordinate system by ratio in the given dimension.                 */
/*                                                                           */
/*****************************************************************************/

void CoordScale(float hfactor, float vfactor)
{
#if DEBUG_ON
  char buff[20];
#endif
  assert( BackEnd == POSTSCRIPT, "CoordScale: BackEnd!" );
  ifdebug(DRS, D, sprintf(buff, "%.3f, %.3f", hfactor, vfactor));
  debug1(DRS, D, "CoordScale(%s)", buff);
  fprintf(out_fp, "%.4f %.4f scale\n", hfactor, vfactor);
  cpexists = FALSE;
  currentfont = NO_FONT;
  currentcolour = NO_COLOUR;
  debug0(DRS, D, "CoordScale returning.");
} /* end CoordScale */


/*****************************************************************************/
/*                                                                           */
/*  SaveGraphicState()                                                       */
/*                                                                           */
/*  Save current coord system on stack for later restoration.                */
/*                                                                           */
/*****************************************************************************/

void SaveGraphicState(void)
{ debug0(DRS, D, "SaveGraphicState()");
  assert( BackEnd == POSTSCRIPT, "SaveGraphicState: BackEnd!" );
  fprintf(out_fp, "gsave\n");
  debug0(DRS, D, "SaveGraphicState returning.");
} /* end SaveGraphicState */


/*****************************************************************************/
/*                                                                           */
/*  RestoreGraphicState()                                                    */
/*                                                                           */
/*  Restore previously saved coordinate system.  NB we normally assume that  */
/*  no white space is needed before any item of output, but since this       */
/*  procedure is sometimes called immediately after PrintGraphicObject(),    */
/*  which does not append a concluding space, we prepend one here.           */
/*                                                                           */
/*****************************************************************************/

void RestoreGraphicState(void)
{ debug0(DRS, D, "RestoreGraphicState()");
  assert( BackEnd == POSTSCRIPT, "RestoreGraphicState: BackEnd!" );
  fprintf(out_fp, "\ngrestore\n");
  cpexists = FALSE;
  currentfont = NO_FONT;
  currentcolour = NO_COLOUR;
  debug0(DRS, D, "RestoreGraphicState returning.");
} /* end RestoreGraphicState */


/*@::PrintGraphicObject(), DefineGraphicNames()@******************************/
/*                                                                           */
/*  PrintGraphicObject(x)                                                    */
/*                                                                           */
/*  Print object x on out_fp                                                 */
/*                                                                           */
/*****************************************************************************/

void PrintGraphicObject(OBJECT x)
{ OBJECT y, link;
  assert( BackEnd == POSTSCRIPT, "PrintGraphicObject: BackEnd!" );
  switch( type(x) )
  {
    case WORD:
    case QWORD:
    
      StringFPuts(string(x), out_fp);
      break;
	

    case ACAT:
    
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	if( type(y) == GAP_OBJ )
	{ if( vspace(y) > 0 )  fputs("\n", out_fp);
	  else if( hspace(y) > 0 ) fputs(" ", out_fp);
	}
	else if( is_word(type(y)) || type(y) == ACAT )  PrintGraphicObject(y);
	else if( type(y) != WIDE && !is_index(type(y)) )
		/* @Wide, indexes are sometimes inserted by Manifest */
	{ Error(24, 5, "error in left parameter of %s",
	    WARN, &fpos(x), KW_GRAPHIC);
	  debug1(DGP, D, "  type(y) = %s, y =", Image(type(y)));
	  ifdebug(DGP, D, DebugObject(y));
	}
      }
      break;


    default:
    
      Error(24, 6, "error in left parameter of %s", WARN, &fpos(x), KW_GRAPHIC);
      debug1(DGP, D, "  type(x) = %s, x =", Image(type(x)));
      ifdebug(DGP, D, DebugObject(x));
      break;
  }
} /* end PrintGraphicObject */


/*****************************************************************************/
/*                                                                           */
/*  DefineGraphicNames(x)                                                    */
/*                                                                           */
/*  Generate PostScript for xsize, ysize etc. names of graphic object.       */
/*                                                                           */
/*****************************************************************************/

void DefineGraphicNames(OBJECT x)
{ assert( type(x) == GRAPHIC, "PrintGraphic: type(x) != GRAPHIC!" );
  assert( BackEnd == POSTSCRIPT, "DefineGraphicNames: BackEnd!" );
  debug1(DRS, D, "DefineGraphicNames( %s )", EchoObject(x));
  debug1(DRS, DD, "  style = %s", EchoStyle(&save_style(x)));

  /* if font is different to previous word then print change */
  if( font(save_style(x)) != currentfont )
  { currentfont = font(save_style(x));
    if( currentfont > 0 )
    { currentxheight2 = FontHalfXHeight(currentfont);
      fprintf(out_fp, "%hd %s ", FontSize(currentfont, x),
        FontName(currentfont));
    }
  }

  /* if colour is different to previous word then print change */
  if( colour(save_style(x)) != currentcolour )
  { currentcolour = colour(save_style(x));
    if( currentcolour > 0 )
    { fprintf(out_fp, "%s ", ColourCommand(currentcolour));
    }
  }

  fprintf(out_fp, "%d %d %d %d %d %d %d LoutGraphic\n",
    size(x, COLM), size(x, ROWM), back(x, COLM), fwd(x, ROWM),
    currentfont <= 0 ? 12*PT : FontSize(currentfont, x),
    width(line_gap(save_style(x))), width(space_gap(save_style(x))));

  debug0(DRS, D, "DefineGraphicNames returning.");
} /* end DefineGraphicNames */


/*@::PrintGraphicInclude()@***************************************************/
/*                                                                           */
/*  PrintGraphicInclude(x, colmark, rowmark)                                 */
/*                                                                           */
/*  Print graphic include file, with appropriate surrounds.  This code       */
/*  closely follows the PostScript Language Reference Manual, 2n ed.,        */
/*  pages 733-5, except we do not clip the included EPSF.                    */
/*                                                                           */
/*  Note to porters: Version 3.0 of the EPSF standard is not compatible      */
/*  with previous versions.  Thus, this output may crash your system.        */
/*  If you can find out which comment line(s) are causing the trouble,       */
/*  you can add to procedure strip_out to strip them out during the          */
/*  file inclusion step.  e.g. on my system %%EOF causes problems, so I      */
/*  strip it out.                                                            */
/*                                                                           */
/*  May 1994: I've just discovered that %%Trailer causes problems for        */
/*  the mpage Unix utility, so now I'm stripping it out as well.             */
/*                                                                           */
/*****************************************************************************/
#define	SKIPPING	0
#define	READING_DNR	1
#define FINISHED	2

static BOOLEAN strip_out(FULL_CHAR *buff)
{ if( StringBeginsWith(buff, AsciiToFull("%%EOF"))     )  return TRUE;
  if( StringBeginsWith(buff, AsciiToFull("%%Trailer")) )  return TRUE;
  return FALSE;
} /* end strip_out */

void PrintGraphicInclude(OBJECT x, LENGTH colmark, LENGTH rowmark)
{ OBJECT y, full_name;  FULL_CHAR buff[MAX_BUFF];
  FILE *fp;  int state;  BOOLEAN compressed;
  debug0(DRS, D, "PrintGraphicInclude(x)");
  assert( BackEnd == POSTSCRIPT, "PrintGraphicInclude: BackEnd!" );
  assert(type(x)==INCGRAPHIC || type(x)==SINCGRAPHIC, "PrintGraphicInclude!");
  assert(sparec(constraint(x)), "PrintGraphicInclude: sparec(constraint(x))!");

  /* open the include file and get its full path name */
  Child(y, Down(x));
  fp = OpenIncGraphicFile(string(y), type(x), &full_name,&fpos(y),&compressed);
  assert( fp != NULL, "PrintGraphicInclude: fp!" );

  /* if font is different to previous word then print change */
  if( font(save_style(x)) != currentfont )
  { currentfont = font(save_style(x));
    currentxheight2 = FontHalfXHeight(currentfont);
    fprintf(out_fp, "%hd %s\n", FontSize(currentfont, x), FontName(currentfont));
  }

  /* if colour is different to previous word then print change */
  if( colour(save_style(x)) != currentcolour )
  { currentcolour = colour(save_style(x));
    if( currentcolour > 0 )
    { fprintf(out_fp, "%s\n", ColourCommand(currentcolour));
    }
  }

  /* generate appropriate header code */
  fprintf(out_fp, "BeginEPSF\n");
  CoordTranslate(colmark - back(x, COLM), rowmark - fwd(x, ROWM));
  CoordScale( (float) PT, (float) PT );
  CoordTranslate(-back(y, COLM), -back(y, ROWM));
  fprintf(out_fp, "%%%%BeginDocument: %s\n", string(full_name));

  /* copy through the include file, except divert resources lines to needs */
  /* and strip out some comment lines that cause problems                  */
  state = (StringFGets(buff, MAX_BUFF, fp) == NULL) ? FINISHED : SKIPPING;
  while( state != FINISHED ) switch(state)
  {
    case SKIPPING:

      if( StringBeginsWith(buff, AsciiToFull("%%DocumentNeededResources:")) &&
	  !StringContains(buff, AsciiToFull("(atend)")) )
      { y = MakeWord(WORD, &buff[StringLength("%%DocumentNeededResources:")],
	      no_fpos);
        Link(needs, y);
	state = (StringFGets(buff,MAX_BUFF,fp)==NULL) ? FINISHED : READING_DNR;
      }
      else
      { if( StringBeginsWith(buff, AsciiToFull("%%LanguageLevel:")) )
	  Error(24, 7, "ignoring LanguageLevel comment in %s file %s",
	    WARN, &fpos(x), KW_INCGRAPHIC, string(full_name));
	if( StringBeginsWith(buff, AsciiToFull("%%Extensions:")) )
	  Error(24, 8, "ignoring Extensions comment in %s file %s",
	    WARN, &fpos(x), KW_INCGRAPHIC, string(full_name));
	if( !strip_out(buff) )  StringFPuts(buff, out_fp);
	state = (StringFGets(buff, MAX_BUFF, fp) == NULL) ? FINISHED : SKIPPING;
      }
      break;

    case READING_DNR:

      if( StringBeginsWith(buff, AsciiToFull("%%+")) )
      {	x = MakeWord(WORD, &buff[StringLength(AsciiToFull("%%+"))], no_fpos);
	Link(needs, x);
	state = (StringFGets(buff,MAX_BUFF,fp)==NULL) ? FINISHED : READING_DNR;
      }
      else
      { if( !strip_out(buff) )  StringFPuts(buff, out_fp);
	state = (StringFGets(buff, MAX_BUFF, fp) == NULL) ? FINISHED : SKIPPING;
      }
      break;
  }

  /* wrapup */
  DisposeObject(full_name);
  fclose(fp);
  if( compressed )  StringRemove(AsciiToFull(LOUT_EPS));
  fprintf(out_fp, "%%%%EndDocument\nEndEPSF\n");
  wordcount = 0;
  debug0(DRS, D, "PrintGraphicInclude returning.");
} /* end PrintGraphicInclude */
