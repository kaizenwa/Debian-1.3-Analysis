/*@z01.c:Supervise:StartSym, AllowCrossDb, Encapsulated, etc.@****************/
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
/*  FILE:         z01.c                                                      */
/*  MODULE:       Supervise                                                  */
/*  EXTERNS:      main(), StartSym, GalleySym, ForceGalleySym, InputSym,     */
/*                PrintSym, AllowCrossDb, Encapsulated                       */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  MemCheck - check this memory location                                    */
/*                                                                           */
/*****************************************************************************/

POINTER MemCheck = 0;


/*****************************************************************************/
/*                                                                           */
/*  StartSym        the symbol table entry for \Start (overall scope)        */
/*  GalleySym       the symbol table entry for @Galley                       */
/*  ForceGalleySym  the symbol table entry for @ForceGalley                  */
/*  InputSym        the symbol table entry for @LInput                       */
/*  PrintSym        the symbol table entry for \Print (root target)          */
/*  OptGallSym      the symbol table entry for @OptGall (optimal galley rec) */
/*  FilterInSym     the symbol table entry for @FilterIn                     */
/*  FilterOutSym    the symbol table entry for @FilterOut                    */
/*  FilterErrSym    the symbol table entry for @FilterErr                    */
/*                                                                           */
/*****************************************************************************/

OBJECT StartSym, GalleySym, ForceGalleySym, InputSym, PrintSym, OptGallSym,
       FilterInSym, FilterOutSym, FilterErrSym;


/*****************************************************************************/
/*                                                                           */
/*  CommandOptions      Command-line options (ACAT of objects)               */
/*                                                                           */
/*****************************************************************************/

OBJECT CommandOptions;


/*****************************************************************************/
/*                                                                           */
/*  AllowCrossDb        Allow references to OldCrossDb and NewCrossDb        */
/*  Encapsulated        Produce a one-page encapsulated PostScript file      */
/*  Kern                Do kerning                                           */
/*  SafeExecution       Execute safely, i.e. prohibit system() calls         */
/*                                                                           */
/*****************************************************************************/

BOOLEAN AllowCrossDb;
BOOLEAN Encapsulated;
BOOLEAN Kern;
BOOLEAN SafeExecution;


/*****************************************************************************/
/*                                                                           */
/*  BackEnd		POSTSCRIPT or PLAINTEXT                              */
/*  PlainCharWidth      if PLAINTEXT, the width of each character            */
/*  PlainCharHeight     if PLAINTEXT, the height of each character           */
/*  PlainFormFeed       if PLAINTEXT, TRUE if separate components with \f.   */
/*  InitializeAll       TRUE if this is an initializing run.                 */
/*  MsgCat              category for locale-specific messages                */
/*                                                                           */
/*****************************************************************************/

int BackEnd;
LENGTH PlainCharWidth, PlainCharHeight;
BOOLEAN PlainFormFeed;
BOOLEAN InitializeAll;
#if LOCALE_ON
nl_catd MsgCat;
#endif


/*****************************************************************************/
/*                                                                           */
/*  static OBJECT load(xstr, xpredefined, xleft, xright, xindef, xprec)      */
/*                                                                           */
/*  Load a predefined operator with these attributes into the symbol table.  */
/*  If the operator has parameters, load symbols for those also.             */
/*                                                                           */
/*****************************************************************************/

static OBJECT load(FULL_CHAR *xstr, unsigned xpre,
BOOLEAN xleft, BOOLEAN xright, BOOLEAN xindef, unsigned char xprec)
{ OBJECT s;
  s = InsertSym(xstr, LOCAL, no_fpos, xprec, xindef, FALSE, xpre,
	StartSym, nilobj);
  if( xleft )  InsertSym( AsciiToFull("pa"), LPAR, no_fpos, DEFAULT_PREC,
    FALSE, FALSE, 0, s, nilobj);
  if( xright )  InsertSym( AsciiToFull("pb"), RPAR, no_fpos, DEFAULT_PREC,
    FALSE, FALSE, 0, s, nilobj);
  if( xleft && xright )  right_assoc(s) = TRUE;
  return s;
} /* end load */


/*@::GetArg(), main()@********************************************************/
/*                                                                           */
/*  GetArg(argv, argc, i)                                                    */
/*                                                                           */
/*  Get the next argument from the command line and return it.               */
/*  Return NULL if it isn't there.                                           */
/*                                                                           */
/*****************************************************************************/

static FULL_CHAR *GetArg(char *argv[], int argc, int *i)
{ if( !StringEqual(AsciiToFull(argv[*i]+2), STR_EMPTY) )
    return AsciiToFull(argv[*i]+2);
  else if( *i < argc-1 && *argv[*i + 1] != CH_HYPHEN )
    return AsciiToFull(argv[(*i)++ +1]);
  else
    return (FULL_CHAR *) NULL;
} /* end GetArg */


/*****************************************************************************/
/*                                                                           */
/*  main(argc, argv)                                                         */
/*                                                                           */
/*  Read command line, initialise everything, read definitions, read         */
/*  galleys, clean up and exit.                                              */
/*                                                                           */
/*****************************************************************************/

int main(int argc, char *argv[])
{ int i, len;  FULL_CHAR *arg;
  OBJECT t, y, res, s;			/* current token, parser output      */
  BOOLEAN stdin_seen;			/* TRUE when stdin file seen         */
  int source_file_count;		/* number of source files in command */
  FULL_CHAR *cross_db;			/* name of cross reference database  */
  FULL_CHAR *outfile;			/* name of output file               */
  FILE *out_fp;
  long MemCheckLong;
  FULL_CHAR oname[MAX_BUFF], oval[MAX_BUFF], buff[MAX_BUFF], *p;
  int bp;  OBJECT z;

  /* set locale if that's what we are doing */
#if LOCALE_ON
  char catname[MAX_BUFF], *loc;
  loc = setlocale(LC_MESSAGES, "");
  if( loc == (char *) NULL )
  { Error(1, 6, "unable to initialize locale", WARN, no_fpos);
    loc = "C";
  }
  sprintf(catname, "%s/%s/LC_MESSAGES/errors.%s", LOCALE_DIR, loc, loc);
  MsgCat = catopen(catname, 0);
#endif

  /* initialise various modules, add current directory to search paths */
  BackEnd = POSTSCRIPT;
  PlainCharWidth = PLAIN_WIDTH;
  PlainCharHeight = PLAIN_HEIGHT;
  PlainFormFeed = FALSE;
  InitializeAll = FALSE;
  AllowCrossDb = TRUE;
  Encapsulated = FALSE;
  SafeExecution = FALSE;
  Kern = TRUE;
  MemInit();
  InitSym();
  LexInit();
  InitFiles();
  AddToPath(SOURCE_PATH,   STR_EMPTY);
  AddToPath(DATABASE_PATH, STR_EMPTY);
  AddToPath(INCLUDE_PATH,  STR_EMPTY);

  /* read command line */
  stdin_seen = FALSE;
  cross_db = CROSS_DB;
  outfile = STR_STDOUT;
  source_file_count = 0;
  New(CommandOptions, ACAT);
  for( i = 1;  i < argc;  i++ )
  {
    if( *argv[i] == CH_HYPHEN ) switch( *(argv[i]+1) )
    {
      case CH_FLAG_OUTFILE:
     
	/* read name of output file */
	if( (outfile = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 7, "usage: -o <filename>", FATAL, no_fpos);
	break;


      case CH_FLAG_SUPPRESS:
     
	/* suppress references to OldCrossDb and NewCrossDb */
	AllowCrossDb = FALSE;
	break;


      case CH_FLAG_NOKERN:
     
	/* suppress kerning */
	Kern = FALSE;
	break;


      case CH_FLAG_CROSS:
     
	/* read name of cross reference database */
	if( (cross_db = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 8, "usage: -c <filename>", FATAL, no_fpos);
	break;


      case CH_FLAG_ERRFILE:
     
	/* read log file name */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 9, "usage: -e <filename>", FATAL, no_fpos);
	ErrorInit(arg);
	break;


      case CH_FLAG_EPSFIRST:
     
	/* -EPS produces encapsulated PostScript output */
	if( !StringEqual(AsciiToFull(argv[i]+1), STR_EPS) )
	  Error(1, 10, "usage: -EPS", FATAL, no_fpos);
	Encapsulated = TRUE;
	break;


      case CH_FLAG_DIRPATH:
     
	/* add directory to database and sysdatabase paths */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 11, "usage: -D <directoryname>", FATAL, no_fpos);
	AddToPath(DATABASE_PATH, arg);
	AddToPath(SYSDATABASE_PATH, arg);
	break;


      case CH_FLAG_ENCPATH:
     
	/* add directory to character mapping path */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 12, "usage: -C <directoryname>", FATAL, no_fpos);
	AddToPath(MAPPING_PATH, arg);
	break;


      case CH_FLAG_FNTPATH:
     
	/* add directory to font path */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 13, "usage: -F <directoryname>", FATAL, no_fpos);
	AddToPath(FONT_PATH, arg);
	break;


      case CH_FLAG_HYPPATH:
     
	/* add directory to hyph path */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 14, "usage: -H <directoryname>", FATAL, no_fpos);
	AddToPath(HYPH_PATH, arg);
	break;


      case CH_FLAG_INCPATH:
     
	/* add directory to include and sysinclude paths */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 15, "usage: -I <directoryname>", FATAL, no_fpos);
	AddToPath(INCLUDE_PATH, arg);
	AddToPath(SYSINCLUDE_PATH, arg);
	break;


      case CH_FLAG_INCLUDE:
     
	/* read sysinclude file and strip any .lt suffix */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 16, "usage: -i <filename>", FATAL, no_fpos);
	len = StringLength(arg) - StringLength(SOURCE_SUFFIX);
	if( len >= 0 && StringEqual(&arg[len], SOURCE_SUFFIX) )
	  StringCopy(&arg[len], STR_EMPTY);
	debug0(DFS, D, "  calling DefineFile from main (1)");
	DefineFile(arg, STR_EMPTY, no_fpos, SOURCE_FILE, SYSINCLUDE_PATH);
	break;


      case CH_FLAG_HYPHEN:
     
	/* declare hyphenation file */
	if( FirstFile(HYPH_FILE) != NO_FILE )
	  Error(1, 17, "two -h options illegal", FATAL, no_fpos);
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 18, "usage: -h <filename>", FATAL, no_fpos);
	debug0(DFS, D, "  calling DefineFile from main (2)");
	DefineFile(arg, STR_EMPTY, no_fpos, HYPH_FILE, INCLUDE_PATH);
	DefineFile(arg, HYPH_SUFFIX, no_fpos, HYPH_PACKED_FILE, INCLUDE_PATH);
	break;


      case CH_FLAG_VERSION:
     
	fprintf(stderr, "%s\n", LOUT_VERSION);
	fprintf(stderr, "%-28s %s\n",
	  "Basser Lout written by:", "Jeffrey H. Kingston (jeff@cs.usyd.edu.au)");
	fprintf(stderr, "%-28s %s\n",
	  "Free source available from:", "ftp://ftp.cs.usyd.edu.au/jeff/lout");
	fprintf(stderr, "%-28s %s %s\n",
	  "This executable compiled:", __TIME__, __DATE__);
	fprintf(stderr, "%-28s %s\n", "System include directory:", INCL_DIR);
	fprintf(stderr, "%-28s %s\n", "System database directory:", DATA_DIR);
	fprintf(stderr, "Database index files created afresh automatically:%s\n",
	  USE_STAT ? " yes" : " no");
	exit(0);
	break;


      case CH_FLAG_FFPLAIN:

	PlainFormFeed = TRUE;
	/* NB NO BREAK */


      case CH_FLAG_PLAIN:
     
	BackEnd = PLAINTEXT;
	if( *(argv[i]+2) != '\0' )
	{ float len1, len2;  FULL_CHAR units1, units2;
	  debug1(DGP, DD, "  command line %s", argv[i]+2);
	  if( sscanf(argv[i]+2, "%f%c%f%c",&len1,&units1,&len2,&units2) != 4 )
	  { Error(1, 19, "usage: lout -%c<length><length>",
	      FATAL, no_fpos, *(argv[i]+1));
	  }
	  switch( units1 )
	  {
	    case CH_UNIT_CM:	PlainCharWidth = len1 * CM; break;
	    case CH_UNIT_IN:	PlainCharWidth = len1 * IN; break;
	    case CH_UNIT_PT:	PlainCharWidth = len1 * PT; break;
	    case CH_UNIT_EM:	PlainCharWidth = len1 * EM; break;

	    default:	Error(1, 20, "lout -%c: units must be c, i, p, or m",
				  FATAL, no_fpos, *(argv[i]+1));
				break;
	  }
	  switch( units2 )
	  {
	    case CH_UNIT_CM:	PlainCharHeight = len2 * CM; break;
	    case CH_UNIT_IN:	PlainCharHeight = len2 * IN; break;
	    case CH_UNIT_PT:	PlainCharHeight = len2 * PT; break;
	    case CH_UNIT_EM:	PlainCharHeight = len2 * EM; break;

	    default:	Error(1, 21, "lout -%c: units must be c, i, p, or m",
				  FATAL, no_fpos, *(argv[i]+1));
				break;
	  }
	}
	break;


      case CH_FLAG_INITALL:

	InitializeAll = TRUE;
	AllowCrossDb = FALSE;
	break;


      case CH_FLAG_USAGE:
     
	Error(1, 22, "usage: lout [ -i <filename> ] files", WARN, no_fpos);
	exit(0);
	break;


      case CH_FLAG_DEBUG:
     
	debug_init(AsciiToFull(argv[i]));
	break;


      case 'M':

	sscanf(argv[i], "-M%ld", &MemCheckLong);
	MemCheck = (POINTER) MemCheckLong;
	fprintf(stderr, "checking memory location %ld\n", (long) MemCheck);
	break;

      case '\0':
     
	/* read stdin as file name */
	if( stdin_seen )
	  Error(1, 23, "standard input specified twice", FATAL, no_fpos);
	stdin_seen = TRUE;
	debug0(DFS, D, "  calling DefineFile from main (3)");
	DefineFile(STR_STDIN, STR_EMPTY, no_fpos, SOURCE_FILE, SOURCE_PATH);
	break;


      case CH_FLAG_OPTION:

	/* read command-line document option */
	if( sscanf(argv[i]+2, "%[^{ ] { %[^}] }", oname, oval) != 2 ||
	  StringLength(oname) == 0 || StringLength(oval) == 0 )
	  Error(1, 24, "error in command-line option %s", FATAL, no_fpos,
	    argv[i]+2);
	y = MakeWord(WORD, oname, no_fpos);
	Link(CommandOptions, y);
	New(y, ACAT);
	Link(CommandOptions, y);
	bp = 0;
	for( p = oval;  *p != '\0';  p++ )  switch( *p )
	{
	  case ' ':
	  case '\t':
	  case '\n':
	  case '{':
	  case '}':

	    if( bp > 0 )
	    { buff[bp++] = '\0';
	      if( Down(y) != y ) 
	      { OBJECT g;
		New(g, GAP_OBJ);
		hspace(g) = 1;  vspace(g) = 0;
		FposCopy(fpos(g), *no_fpos);
		Link(y, g);
	      }
	      z = MakeWord(WORD, buff, no_fpos);
	      Link(y, z);
	      bp = 0;
	    }
	    break;


	  default:

	    buff[bp++] = *p;
	    break;
	}
	if( bp > 0 )
	{ buff[bp++] = '\0';
	  z = MakeWord(WORD, buff, no_fpos);
	  Link(y, z);
	}
	if( Down(y) == y )
	  Error(1, 25, "error in command-line option %s", FATAL, no_fpos,
	    argv[i]+2);
	break;


      case CH_FLAG_SAFE:

	/* ensure safe execution by disabling system calls */
	SafeExecution = TRUE;
	break;

      default:
     
	Error(1, 26, "unknown command line flag %s", FATAL, no_fpos, argv[i]);
	break;

    }
    else
    {   /* argument is source file, strip any .lout suffix and define it */
	arg = AsciiToFull(argv[i]);
	len = StringLength(arg) - StringLength(SOURCE_SUFFIX);
	if( len >= 0 && StringEqual(&arg[len], SOURCE_SUFFIX) )
	  StringCopy(&arg[len], STR_EMPTY);
	debug0(DFS, D, "  calling DefineFile from main (4)");
	DefineFile(AsciiToFull(argv[i]), STR_EMPTY, no_fpos,
	    SOURCE_FILE, SOURCE_PATH);
	source_file_count++;
    }
  } /* for */

  /* start timing if required */
  ifdebug(DPP, D, ProfileOn("main"));

  /* open output file, or stdout if none specified, and initialize printer */
  if( StringEqual(outfile, STR_STDOUT) )  out_fp = stdout;
  else if( (out_fp = StringFOpen(outfile, WRITE_TEXT)) == null )
    Error(1, 27, "cannot open output file %s", FATAL, no_fpos, outfile);
  FontInit();
  ColourInit();
  LanguageInit();
  PrintInit(out_fp);

  /* append default directories to file search paths */
  AddToPath(FONT_PATH,         AsciiToFull(FONT_DIR));
  AddToPath(HYPH_PATH,         AsciiToFull(HYPH_DIR));
  AddToPath(MAPPING_PATH,      AsciiToFull(MAPS_DIR));
  AddToPath(SYSDATABASE_PATH,  AsciiToFull(DATA_DIR));
  AddToPath(DATABASE_PATH,     AsciiToFull(DATA_DIR));
  AddToPath(SYSINCLUDE_PATH,   AsciiToFull(INCL_DIR));
  AddToPath(INCLUDE_PATH,      AsciiToFull(INCL_DIR));

  /* use stdin if no source files were mentioned */
  if( source_file_count == 0 )
  { debug0(DFS, D, "  calling DefineFile from main (5)");
    DefineFile(STR_STDIN, STR_EMPTY, no_fpos, SOURCE_FILE, SOURCE_PATH);
  }

  /* load predefined symbols into symbol table */
  StartSym       = nilobj;  /* Not a mistake */
  StartSym       = load(KW_START,        0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  GalleySym      = load(KW_GALLEY,       0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  ForceGalleySym = load(KW_FORCE_GALLEY, 0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  InputSym       = load(KW_INPUT,        0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  PrintSym       = load(KW_PRINT,        0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  FilterInSym    = load(KW_FILTERIN,     0, FALSE,  FALSE,  FALSE, NO_PREC     );
  FilterOutSym   = load(KW_FILTEROUT,    0, FALSE,  FALSE,  FALSE, NO_PREC     );
  FilterErrSym   = load(KW_FILTERERR,    0, FALSE,  FALSE,  FALSE, NO_PREC     );
  OptGallSym     = load(KW_OPTGALL,      0, FALSE,  TRUE,   FALSE, DEFAULT_PREC);


  load(KW_BEGIN,       BEGIN,          FALSE,  FALSE,  FALSE, BEGIN_PREC  );
  load(KW_END,         END,            FALSE,  FALSE,  FALSE, END_PREC    );
  load(KW_ENV,         ENV,            FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_CLOS,        CLOS,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_LVIS,        LVIS,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_LUSE,        LUSE,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_LBR,         LBR,            FALSE,  FALSE,  FALSE, LBR_PREC    );
  load(KW_RBR,         RBR,            FALSE,  FALSE,  FALSE, RBR_PREC    );
  load(KW_INCLUDE,     INCLUDE,        FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SYSINCLUDE,  SYS_INCLUDE,    FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_PREPEND,     PREPEND,        FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SYSPREPEND,  SYS_PREPEND,    FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_DATABASE,    DATABASE,       FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SYSDATABASE, SYS_DATABASE,   FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_USE,         USE,            FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_NOT_REVEALED,NOT_REVEALED,   FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_CASE,        CASE,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_YIELD,       YIELD,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_BACKEND,     BACKEND,        FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_XCHAR,       XCHAR,          FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_FONT,        FONT,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_SPACE,       SPACE,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_YUNIT,       YUNIT,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ZUNIT,       ZUNIT,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_BREAK,       BREAK,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_UNDERLINE,   UNDERLINE,      FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_COLOUR,      COLOUR,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_COLOR,       COLOUR,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_LANGUAGE,    LANGUAGE,       TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_CURR_LANG,   CURR_LANG,      FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_COMMON,      COMMON,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_RUMP,        RUMP,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_INSERT,      INSERT,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_NEXT,        NEXT,           FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_OPEN,        OPEN,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_TAGGED,      TAGGED,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_WIDE,        WIDE,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HIGH,        HIGH,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HSHIFT,      HSHIFT,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VSHIFT,      VSHIFT,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ONE_COL,     ONE_COL,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ONE_ROW,     ONE_ROW,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HSCALE,      HSCALE,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VSCALE,      VSCALE,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HCOVER,      HCOVER,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VCOVER,      VCOVER,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_SCALE,       SCALE,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HCONTRACT,   HCONTRACT,      FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VCONTRACT,   VCONTRACT,      FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HEXPAND,     HEXPAND,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VEXPAND,     VEXPAND,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_PADJUST,     PADJUST,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HADJUST,     HADJUST,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VADJUST,     VADJUST,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ROTATE,      ROTATE,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_INCGRAPHIC,  INCGRAPHIC,     FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_SINCGRAPHIC, SINCGRAPHIC,    FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_GRAPHIC,     GRAPHIC,        TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_CROSS,       CROSS,          TRUE,   TRUE,   FALSE, CROSSOP_PREC);
  load(KW_NULL,        NULL_CLOS,      FALSE,  FALSE,  TRUE,  NO_PREC     );
  load(KW_PAGE_LABEL,  PAGE_LABEL,     FALSE,  TRUE,   TRUE,  DEFAULT_PREC);

#define setcat(s, mk, jn)  has_mark(s)=mk, has_join(s)=jn

  s=load(KW_VCAT_NN, VCAT, TRUE, TRUE, FALSE, VCAT_PREC); setcat(s,FALSE,FALSE);
  s=load(KW_VCAT_MN, VCAT, TRUE, TRUE, FALSE, VCAT_PREC); setcat(s,TRUE, FALSE);
  s=load(KW_VCAT_NJ, VCAT, TRUE, TRUE, FALSE, VCAT_PREC); setcat(s,FALSE,TRUE);
  s=load(KW_VCAT_MJ, VCAT, TRUE, TRUE, FALSE, VCAT_PREC); setcat(s,TRUE, TRUE);
  s=load(KW_HCAT_NN, HCAT, TRUE, TRUE, FALSE, HCAT_PREC); setcat(s,FALSE,FALSE);
  s=load(KW_HCAT_MN, HCAT, TRUE, TRUE, FALSE, HCAT_PREC); setcat(s,TRUE, FALSE);
  s=load(KW_HCAT_NJ, HCAT, TRUE, TRUE, FALSE, HCAT_PREC); setcat(s,FALSE,TRUE);
  s=load(KW_HCAT_MJ, HCAT, TRUE, TRUE, FALSE, HCAT_PREC); setcat(s,TRUE, TRUE);
  s=load(KW_ACAT_NJ, ACAT, TRUE, TRUE, FALSE, ACAT_PREC); setcat(s,FALSE,TRUE);
  s=load(KW_ACAT_MJ, ACAT, TRUE, TRUE, FALSE, ACAT_PREC); setcat(s,TRUE, TRUE);

  /* intialize current time and load @Moment symbol */
  InitTime();

  /* initialize filter module */
  FilterInit();

  /* initialise scope chain to <StartSym> */
  PushScope(StartSym, FALSE, FALSE);

  /* initialise lexical analyser */
  LexPush(FirstFile(SOURCE_FILE), 0, SOURCE_FILE);

  /* process input files */
  InitParser(cross_db);
  t = NewToken(BEGIN, no_fpos, 0, 0, BEGIN_PREC, StartSym);
  res = Parse(&t, StartSym, TRUE, TRUE);
  debug0(DGT, D, "calling TransferEnd(res) from main()");
  DisposeObject(CommandOptions);
  TransferEnd(res);
  TransferClose();

  /* close various modules */
  PrintAfterLast();
  CrossClose();
  CloseFiles();

  /* remove any leftover filter temporary files */
  FilterScavenge(TRUE);

  /* check for unbalanced error blocks */
  CheckErrorBlocks();

  /* wrapup */
  ifdebug(DST, D, CheckSymSpread() );
  ifdebug(ANY, D, DeleteEverySym() );
  debug0(DMA, D, "at end of run:");
  ifdebug(DMA, D, DebugMemory() );
  ifdebug(DPP, D, ProfileOff("main"));
  ifdebug(DPP, D, ProfilePrint());

#if LOCALE_ON
  catclose(MsgCat);
#endif

  exit(0);
  return 0;
} /* end main */
