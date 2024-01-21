/******************************************************************************
** $Id: main.c,v 2.24 1996/02/22 21:43:51 gerd Exp gerd $
**=============================================================================
** 
** This file is part of BibTool.
** It is distributed under the GNU General Public License.
** See the file COPYING for details.
** 
** (c) 1996 Gerd Neugebauer
** 
** Net: gerd@informatik.uni-koblenz.de
** 
******************************************************************************/

#include "tex_aux.h"
#include "bibtool.h"
#include "error.h"
#include "key.h"
#include "parse.h"
#include "pxfile.h"
#include "print.h"
#include "rewrite.h"
#include "rsc.h"
#include "entry.h"
#include "macros.h"
#include "type.h"
#include "s_parse.h"
#include "sbuffer.h"
#include "version.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int main _ARG((int argc,char *argv[]));	   /* main.c                 */
 static void keep _ARG((Record rec));		   /* main.c                 */
 static void process_stored_records _ARG((int verbose));/* main.c            */
 static void read_loop _ARG((void (*fct)_ARG((Record))));/* main.c           */
 static void store _ARG((Record rec));		   /* main.c                 */
 static void update_crossref _ARG((Record rec,Record first_rec));/* main.c   */
 void printchar _ARG((char c));			   /* main.c                 */
 void save_input_file _ARG((char *file));	   /* main.c                 */
 void save_macro_file _ARG((char * file));	   /* main.c                 */
 void save_output_file _ARG((char * file));	   /* main.c                 */
 void usage _ARG((int full));			   /* main.c                 */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

 extern char * sym_crossref;

/*---------------------------------------------------------------------------*/

#ifndef __STDC__
#ifndef HAS_getenv

 char * getenv _ARG((char *name));		   /* main.c                 */

/*-----------------------------------------------------------------------------
** Dummy funtion returning NULL to indicate failure.
**___________________________________________________			     */
char * getenv(name)				   /*			     */
  register char *name;				   /*			     */
{ return (char*)0;				   /*			     */
}						   /*------------------------*/
#endif
#endif


#ifndef OptionLeadingCharacter
#define OptionLeadingCharacter '-'
#endif

 static char *use[] =
  { "bibtool [options] [%co outfile] [[%ci] infile] ...\n",
    "\n\tOptions:\n",
    "\t%cA<c>\t\tKind of disambiguating keystrings: <c>=0|a|A\n",
    "\t%cd\t\tCheck double entries\n",
    "\t%cf <format>\tKey generation enabled (formated key)\n",
    "\t%cF\t\tKey generation enabled with formated key\n",
    "\t%ch\t\tPrint this help info and exit\n",
    "\t[%ci] infile\tSpecify input file. If %ci omitted it may not start\n",
    "\t\t\twith a %c. If absent stdin is taken to read from.\n",
    "\t\t\tMultiple input files may be given.\n",
    "\t%ck\t\tKey generation enabled.\n",
    "\t%cK\t\tKey generation enabled (long key).\n",
    "\t%cm macfile\tDump macros to macfile. - is stdout\n",
    "\t%cM macfile\tDump used macros to macfile. - is stdout\n",
    "\t%co outfile\tSpecify output file as next argument\n",
    "\t\t\tIf absent stdout is taken to write to.\n",
    "\t%cq\t\tQuiet mode. No warnings.\n",
    "\t%cr resource\tLoad resource file (several are possible).\n",
    "\t%cR\t\tLoad default resource file here.\n",
    "\t%cs\t\tSort.\n",
    "\t%cS\t\tSort reverse.\n",
    "\t%cv\t\tEnable verbose mode\n",
    "\t%cV\t\tPrint version\n",
    "\t%cx file\t\tExtract from aux file.\n",
    "\t%cX <regex>\tExtract regular expression.\n",
    "\t%c- <rsc>\tEvaluate one resource command <rsc>.\n",
    "\t%c@\t\tPrint statistics (short).\n",
    "\t%c#\t\tPrint statistics.\n",
#ifdef SYMBOL_DUMP
    "\t%c$\t\tSymbol table output (debugging only)\n",
#endif
    0L,
    "Gerd Neugebauer $Date: 1996/02/22 21:43:51 $",
    "gerd@informatik.uni-koblenz.de"
  };

/*-----------------------------------------------------------------------------
** Function:	usage()
** Purpose:	Print the version number and
**		a short description of the command line options.
**
** Arguments:
**	full	Boolean. If FALSE only the versin is displayed.
** Returns:	nothing
**___________________________________________________			     */
void usage(full)				   /*			     */
  int           full;				   /*                        */
{ register char **cpp;				   /*			     */
						   /*			     */
  ErrPrintF("BibTool Vers. %s (C) 1996 Gerd Neugebauer\n\n",version);/*	     */
						   /*			     */
  if ( full )					   /*                        */
  { for ( cpp=use; *cpp; cpp++ )		   /*			     */
    { ErrPrintF2(*cpp,				   /*			     */
		 OptionLeadingCharacter,	   /*			     */
		 OptionLeadingCharacter);	   /*			     */
    }						   /*                        */
  }						   /*                        */
}						   /*------------------------*/


#define SkipWarning	      WARNING("*** Skiping to next '@'")
#define UnknownWarning(X)     WARNING2("Unknown flag ignored: ",X)
#define NoSFileWarning	      WARNING("Missing select option. Flag ignored.")
#define NoFileError(X)	      WARNING3("File ",X," not found.")
#define MissingPattern	      WARNING("Missing pattern.")
#define MissingResource	      WARNING("Missing resource.")
#define NoRscError(X)	      WARNING3("Resource file ",X," not found.")

/*****************************************************************************/
/***			 Input File Pipe Section			   ***/
/*****************************************************************************/

#define InputFilePipeIncrement 8

 static char **input_files;
 static int  input_file_size = 0;
 static int  input_file_ptr  = 0;

#define InputPipeIsFull		(input_file_ptr >= input_file_size)
#define InputPipeIsEmpty	(input_file_ptr == 0)
#define PushToInputPipe(FILE)	input_files[input_file_ptr++] = FILE
#define ForAllInputFiles(FILE)	for (FILE=input_files;			\
				     FILE<&input_files[input_file_ptr];	\
				     FILE++)

/*-----------------------------------------------------------------------------
** Function:	save_input_file()
** Purpose:	The input file pipe is a dynamic array of strings
**		This function is called to push an string into the pipe.
**		If neccesary the array has to be allocated or enlarged.
**		This is done in larger junks to avoid lots of calls to
**		realloc().
** Arguments:
**	file	File name to save.
** Returns:	nothing
**___________________________________________________			     */
void save_input_file(file)			   /*			     */
  char *file;					   /*			     */
{						   /*			     */
  if ( file == NULL )				   /*			     */
  { WARNING("Missing input file name. Flag ignored.");/*		     */
    return;					   /*			     */
  }						   /*			     */
  if ( *file == '-' && *(file+1) == '\0' )	   /*			     */
  { file = NULL; }				   /*			     */
						   /*			     */
  if ( InputPipeIsFull )			   /* No space left?	     */
  { input_file_size += InputFilePipeIncrement;	   /*			     */
						   /*			     */
    if ( InputPipeIsEmpty			   /* Try to enlarge array   */
	? NULL==(input_files=			   /*			     */
		 (char**)malloc(sizeof(char*)	   /*			     */
				*(size_t)input_file_size))/*		     */
	: NULL==(input_files=			   /*			     */
		 (char**)realloc((char*)input_files,/*			     */
				 sizeof(char*)	   /*			     */
				 *(size_t)input_file_size))/*		     */
	)					   /*			     */
    { OUT_OF_MEMORY("input file pipe.");	   /*		             */
    }						   /*			     */
  }						   /*			     */
  PushToInputPipe(file);			   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***			   Output File Section				   ***/
/*****************************************************************************/

 static char *output_file = NULL;		   /*			     */

/*-----------------------------------------------------------------------------
** Function:	save_output_file()
** Purpose:	Simply feed the output file name into the static variable.
**		This function is useful since it can be called from rsc.c
** Arguments:
**	file	File name to save
** Returns:	nothing
**___________________________________________________			     */
void save_output_file(file)			   /*			     */
  char * file;					   /*			     */
{ if ( output_file != NULL )			   /*			     */
  { WARNING2("Output file redefined: ",file); }	   /*			     */
  output_file = file;				   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***			   Macro File Section				   ***/
/*****************************************************************************/

 static char *macro_file = NULL;		   /*			     */

/*-----------------------------------------------------------------------------
** Function:	save_macro_file()
** Purpose:	Simply feed the macro file name into the static variable.
**		This function is useful since it can be called from rsc.c
** Arguments:
**	file	File name to save
** Returns:	nothing
**___________________________________________________			     */
void save_macro_file(file)			   /*			     */
  char * file;					   /*			     */
{ if ( macro_file != NULL )			   /*			     */
  { WARNING2("Macro file redefined: ",file); }	   /*			     */
  macro_file = file;				   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***				    MAIN				   ***/
/*****************************************************************************/

#define Toggle(X) X = !(X)

/*-----------------------------------------------------------------------------
** Function:	main()
** Purpose:	
**		Perform Initializations.
**		Evaluate command line arguments.
**		Run the main loop.
** Arguments:
**	argc	Number of arguments
**	argv	Array of arguments
** Returns:	
**___________________________________________________			     */
int main(argc,argv)				   /*			     */
  int		argc;				   /* Argument count	     */
  char		*argv[];			   /* Argument values	     */
{ register int	i;				   /*			     */
  register char *ap;				   /*			     */
  int		need_rsc = TRUE;		   /*			     */
  void		(*fct)();			   /* Function pointer	     */
#ifdef EMTEX_LIKE_PATH
  static StringBuffer *sb_rsc;			   /*                        */
  static StringBuffer *sb_bibtex;		   /*                        */
 						   /*                        */
  { char *emtexdir = getenv(EMTEXDIR);		   /*                        */
    if ( emtexdir == NULL ) emtexdir = EMTEXTDIR_DEFAULT;/*                  */
    sb_rsc = sbopen();				   /*                        */
    sbputs(".;",sb_rsc);			   /*                        */
    sbputs(emtexdir,sb_rsc);			   /*                        */
    sbputs(EMTEXT_RESOURCE,sb_rsc);		   /*                        */
    rsc_v_rsc = sbflush(sb_rsc);		   /*                        */
    sb_bibtex = sbopen();			   /*                        */
    sbputs(".;",sb_bibtex);			   /*                        */
    sbputs(emtexdir,sb_bibtex);			   /*                        */
    sbputs(EMTEXT_BIBTEX,sb_bibtex);		   /*                        */
    rsc_v_bibtex = sbflush(sb_bibtex);		   /*                        */
  }						   /*                        */
#endif
						   /*			     */
  init_type();					   /*			     */
  init_symbols();				   /*			     */
  init_entries();				   /*			     */
  init_read();					   /*			     */
  init_key(0);					   /*			     */
  set_rsc_path(rsc_v_rsc);			   /*                        */
						   /*			     */
  for ( i=1; i<argc; i++ )			   /*			     */
  { if ( *(ap=argv[i]) != OptionLeadingCharacter ) /*			     */
    { save_input_file(argv[i]);			   /*			     */
    }						   /*			     */
    else					   /*			     */
    { switch ( *++ap )				   /*			     */
      { case 'A': set_base(ap+1);	    break; /* disambiguation	     */
	case 'd': Toggle(rsc_double_check); break; /* double entries	     */
	case 'f': add_format(argv[++i]);	   /*	!!! no break !!!     */
	case 'F': rsc_make_key = TRUE;	    break; /* key generation	     */
	case 'h': usage(TRUE); return 1;	   /* print help	     */
	case 'i': save_input_file(argv[++i]); break;/*			     */
	case 'K': add_format("long");	    break; /* key generation	     */
	case 'k': add_format("short");	    break; /* key generation	     */
	case 'M':				   /* print macro table	     */
	case 'm': rsc_all_macs = (*ap=='m');	   /* print macro table	     */
	  save_macro_file(argv[++i]);		   /*			     */
	  break;				   /*			     */
	case 'o':				   /* output file	     */
	  save_output_file(argv[++i]);	    break; /*			     */
	case 'q': Toggle(rsc_quiet);	    break; /* quiet		     */
	case 'r':				   /* resource file	     */
	  if ( ++i < argc && load_rsc(argv[i]) )   /*			     */
	  {  NoRscError(argv[i]); }		   /*                        */
	  else need_rsc = FALSE;		   /*			     */
	  break;				   /*			     */
	case 'R': need_rsc = search_rsc();  break; /* default resource file  */
	case 's': Toggle(rsc_sort); break;	   /* sort		     */
	case 'S':				   /*			     */
	  Toggle(rsc_sort);			   /*			     */
	  Toggle(rsc_sort_reverse);		   /*			     */
	  break;				   /* sort		     */
	case 'v': Toggle(rsc_verbose);	    break; /* verbose		     */
	case 'V': usage(FALSE);	            break; /* version		     */
	case 'x':				   /* extract		     */
	  rsc_all_macs = FALSE;			   /*                        */
	  if ( ++i < argc )			   /*			     */
	  { read_aux(argv[i],*++ap=='v'); }	   /*			     */
	  else		    { NoSFileWarning; }	   /*			     */
	  break;				   /*			     */
	case 'X':				   /* extract pattern	     */
	  rsc_all_macs = FALSE;			   /*                        */
	  if ( ++i < argc ) { save_regex(argv[i]); }/*			     */
	  else		    { MissingPattern; }	   /*			     */
	  break;				   /*			     */
	case '#': Toggle(rsc_cnt_all);	    break; /* print full statistics  */
	case '@': Toggle(rsc_cnt_used);	    break; /* print short statistics */
	case '-':				   /* extended command	     */
	  if ( *++ap )	      { use_rsc(ap);	  }/*			     */
	  else if ( ++i<argc ){ use_rsc(argv[i]); }/*			     */
	  else		      { MissingResource;  }/*			     */
	  break;				   /*			     */
#ifdef SYMBOL_DUMP
	case '$': Toggle(rsc_dump_symbols); break; /* print symbol table     */
#endif
	default:				   /*			     */
	  UnknownWarning(ap);			   /*			     */
	  usage(TRUE);				   /*                        */
	  return 1;		   		   /*			     */
      }						   /*			     */
    }						   /*			     */
  }						   /*			     */
						   /*			     */
  if ( need_rsc ) { (void)search_rsc(); }	   /*			     */
						   /*			     */
  if ( InputPipeIsEmpty )			   /* If no input file given */
  { save_input_file("-"); }			   /*  then read from stdin  */
						   /*			     */
  if ( output_file != NULL )			   /* If output file is given*/
  { (void)tell(output_file); }			   /*  then use it.	     */
						   /*			     */
  if	  (  rsc_sort				   /*			     */
	  || rsc_sort_reverse ) fct = store;	   /*			     */
  else if (  rsc_double_check			   /*			     */
	  || rsc_make_key     ) fct = keep;	   /*			     */
  else				fct = print_record;/*			     */
						   /*			     */
  init_macros();				   /*			     */
  init_read();					   /*			     */
  init_key(1);					   /*			     */
						   /*			     */
  for ( i=0; i<input_file_ptr; i++ )		   /* For all input files    */
  {						   /*			     */
    if ( !see_bib(input_files[i]) )		   /* Try to open bib file   */
    { NoFileError(input_files[i]); }		   /*			     */
    else					   /*			     */
    { if ( rsc_verbose )			   /* If desired print an    */
      { VerbosePrint2("Reading ",filename); }	   /*	open message.	     */
						   /*			     */
      read_loop(fct);				   /* Process the whole file.*/
						   /*			     */
      (void)seen();				   /* Close the file.	     */
						   /*			     */
      if ( rsc_verbose )			   /* If desired print a     */
      { VerbosePrint2("Done with ",filename); }	   /*	close message.	     */
    }						   /*			     */
  }						   /*			     */
						   /*			     */
  process_stored_records(rsc_verbose);		   /* Maybe something's left.*/
						   /*			     */
  (void)told();					   /* Close output stream.   */
						   /*			     */
  if ( macro_file != NULL )			   /*			     */
  { if ( rsc_verbose )				   /*			     */
    { VerbosePrint1("Writing macros"); }	   /*			     */
    dump_mac(macro_file,rsc_all_macs);		   /* Write the strings.     */
  }						   /*			     */
						   /*			     */
  if ( rsc_cnt_all || rsc_cnt_used )		   /*			     */
  { entry_statistics(rsc_cnt_all); }		   /* Write statistics.	     */
						   /*			     */
#ifdef SYMBOL_DUMP
  if ( rsc_dump_symbols ) sym_dump();		   /* Write symbols.	     */
#endif
  return 0;					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	read_loop()
** Purpose:	Read one file until EOF and do what's to be done.
** Arguments:
**	fct
** Returns:	nothing
**___________________________________________________			     */
static void read_loop(fct)			   /*			     */
  void		  (*fct)_ARG((Record));		   /* Function pointer	     */
{ register int	  type;				   /*			     */
  Record rec;				   	   /*			     */
						   /*			     */
  while ( (type=parse()) != EndOfFile )		   /*			     */
  { if ( type < 0 )				   /* Errors give rise to    */
    { SkipWarning; }				   /*  a warning.	     */
    else if ( IsSpecialRecord(type) )		   /* STRING/PREAMBLE/COMMENT*/
    { print_record(get_master_record());	   /* are written at once    */
      ++EntryUsed(type);			   /*			     */
    }						   /*			     */
    else					   /*			     */
    {						   /*			     */
      rec = get_master_record();		   /*			     */
						   /*			     */
      if ( rsc_make_key ) make_key(rec);	   /* Maybe make a new key   */
      else		  mark_key(rec);	   /*  or remember the old   */
						   /*			     */
      if (   rsc_select	!= TRUE			   /* selection enabled	and  */
	  || lookup_item(rec) )	   		   /* record selected	     */
      {						   /*  then		     */
	++EntryUsed(type);			   /*			     */
	rewrite_record(rec);			   /*			     */
	(*fct)(rec);				   /* Do what's needed to be */
						   /*  done.		     */
	if ( rsc_verbose ) { ErrC('+'); FlushErr; }/*			     */
      }						   /*			     */
      else if ( rsc_verbose )			   /*			     */
      { ErrC('.'); FlushErr; }			   /* Just to see something. */
    }						   /*			     */
  }						   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***			      Sorting Section				   ***/
/*****************************************************************************/

 static Record sort_rec = RecNULL;

#define RecLess(A,B)							\
	((rsc_sort_reverse? -strcmp(RecordKey(A),RecordKey(B))		\
			  :  strcmp(RecordKey(A),RecordKey(B))) < 0)

/*-----------------------------------------------------------------------------
** The sorting uses a variant of insertion sort.
** Thus the major task is performed in this function, which is called to store
** a record.
** The Record structure contains two pointers to the same type. With those
** pointers a double linked list is established. sort_rec is either NULL ---
** if no record is stored --- or it is a pointer to some element in the
** ORDERED list:
** 
**    +------+	 +------+   +------+   +------+	  +------+   +------+	
** -->|Record|-->|Record|-->|Record|-->|Record|-->|Record|-->|Record|-->
** <--|	     |<--|	|<--|	   |<--|      |<--|	 |<--|	    |<--
**    +------+	 +------+   +------+   +------+	  +------+   +------+	
**			       ^
**		sort_rec_______|
** 
** With this picture in mind it's obvious what's to be done.
** There are two major cases. To find the right position to insert the new
** record
** - either the pointer has to be moved leftward
** - or	    the pointer has to be moved rightward
** 
** Special cases have to be considered if the new record is to be inserted
** before the first or after the last element in the list.
** 
** The rest is simply legwork.
**
** Well, a word on the complexity.
** If the input is almost sorted (correct or in reverse order) then the 
** algorithm is linear.
** In the worst case the algorithm is quadratic. This worst case looks as
** follows:
** A record has to be inserted which is smaller than the least element in the
** list. Afterwards one which is greater than the largest.
** 
** NOTE: CURRENTLY THE ALGORITHM IS NOT STABLE:
**	 Records with the same key may get mixed in the sorted list.
**___________________________________________________			     */
static void store(rec)				   /*			     */
  register Record rec;				   /*			     */
{ register Record rp;				   /*			     */
						   /*			     */
  rec = copy_record(rec);			   /* Make a private copy.   */
						   /*			     */
  make_sort_key(rec);				   /*			     */
						   /*			     */
  if ( sort_rec == RecNULL )			   /* List is empty	     */
  { sort_rec = rec; return; }			   /* Just remember the rec. */
						   /*			     */
  rp	   = sort_rec;				   /*			     */
  sort_rec = rec;				   /*			     */
  if ( RecLess(rec,rp) )			   /*			     */
  { while ( PrevRecord(rp) != RecNULL )		   /* Move leftward.	     */
    { rp = PrevRecord(rp);			   /*			     */
      if ( !RecLess(rec,rp) )			   /*			     */
      { PrevRecord(rec)		   = rp;	   /* Insert		     */
	NextRecord(rec)		   = NextRecord(rp);/*			     */
	PrevRecord(NextRecord(rec))= rec;	   /*			     */
	NextRecord(rp)		   = rec;	   /*			     */
	return;					   /*			     */
      }						   /*			     */
    }						   /*			     */
    PrevRecord(rp)  = rec;			   /* Insert before the first*/
    NextRecord(rec) = rp;			   /*			     */
  }						   /*			     */
  else						   /*			     */
  { while ( NextRecord(rp) != RecNULL )		   /* Move rightward.	     */
    { rp = NextRecord(rp);			   /*			     */
      if ( RecLess(rec,rp) )			   /*			     */
      { NextRecord(rec)		   = rp;	   /* Insert		     */
	PrevRecord(rec)		   = PrevRecord(rp);/*			     */
	NextRecord(PrevRecord(rec))= rec;	   /*			     */
	PrevRecord(rp)		   = rec;	   /*			     */
	return;					   /*			     */
      }						   /*			     */
    }						   /*			     */
    NextRecord(rp)  = rec;			   /* Insert after the last. */
    PrevRecord(rec) = rp;			   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	keep()
** Purpose:	Just push a copy of the given record to the list.
** Arguments:
**	rec	Record to push
** Returns:	nothing
**___________________________________________________			     */
static void keep(rec)				   /*			     */
  register Record rec;				   /*			     */
{						   /*			     */
  rec = copy_record(rec);			   /* Make a private copy.   */
						   /*			     */
  make_sort_key(rec);				   /*			     */
						   /*			     */
  if ( sort_rec == RecNULL )			   /* List is empty	     */
  { sort_rec = rec; }				   /* Just remember the rec. */
  else						   /*			     */
  { NextRecord(sort_rec) = rec;			   /*			     */
    PrevRecord(rec)	 = sort_rec;		   /*			     */
    sort_rec		 = rec;			   /*			     */
  }						   /*			     */
}						   /*------------------------*/


#define equal_records(R1,R2) RecordKey(R1) == RecordKey(R2)

/*-----------------------------------------------------------------------------
** Function:	process_stored_records()
** Purpose:	This function performs actions for sorting after all records
**		have been read.
**		It simply goes to the beginning and prints them all.
** Arguments:
**	verbose	boolean
** Returns:	nothing
**___________________________________________________			     */
static void process_stored_records(verbose)	   /*			     */
  int		  verbose;			   /*			     */
{ register Record rec;				   /*			     */
  Record	  first_rec;			   /*			     */
						   /*			     */
  if ( (rec=sort_rec) == RecNULL ) return;	   /* No records to process. */
						   /*			     */
  if ( verbose ) { VerbosePrint1("Writing records"); }/*		     */
						   /*			     */
  while( PrevRecord(rec) != RecNULL )		   /*			     */
    rec = PrevRecord(rec);			   /* rewind		     */
						   /*			     */
  first_rec = rec;				   /*			     */
						   /*			     */
  print_record(rec);				   /* print the first	     */
  rec = NextRecord(rec);			   /*			     */
						   /*			     */
  while ( rec != RecNULL )			   /* loop towards end	     */
  {						   /*  and print it.	     */
    if (   IsRecordXREF(rec)			   /*			     */
	&& rsc_make_key	      )			   /*			     */
    { update_crossref(rec,first_rec); }		   /*			     */
						   /*			     */
    if (   rsc_double_check			   /*			     */
	&& equal_records(PrevRecord(rec),rec) )	   /*			     */
    { print_commented(rec);			   /*			     */
      WARNING3("Possible double entries discovered: \n***\t\t",/*	     */
	       RecordKey(PrevRecord(rec)),	   /*			     */
	       RecordKey(rec));			   /*			     */
    }						   /*			     */
    else { print_record(rec); }			   /*			     */
    rec = NextRecord(rec);			   /*			     */
  }						   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	update_crossref()
** Purpose:	
**		
**
** Arguments:
**	rec
**	first_rec
** Returns:	nothing
**___________________________________________________			     */
static void update_crossref(rec,first_rec)	   /*			     */
  Record	  rec;				   /*			     */
  register Record first_rec;			   /*			     */
{ register char	  **hp;				   /*			     */
  int		  i;				   /*			     */
  char		  *ref, *s;			   /*			     */
						   /*			     */
  for ( i=RecordFree(rec), hp = RecordHeap(rec);   /* search crossref field  */
	i>0 && *hp != sym_crossref;		   /*			     */
	i-=2, hp += 2	 )			   /*			     */
  { }						   /*			     */
						   /*			     */
  if ( i <= 0 )					   /*			     */
  { DebugPrint1("*** No crossref found.");	   /*			     */
    return;					   /*			     */
  }						   /*			     */
						   /*			     */
  s = *++hp; s++;				   /*			     */
  (void)sp_open(s);				   /* Try to extract	     */
  if ( (ref = SParseSymbol(&s)) == NULL )	   /*  the crossref as symbol*/
  { return; }					   /*			     */
						   /*			     */
  while (   first_rec != RecNULL		   /* Search the referenced  */
	 && RecordOldKey(first_rec) != ref )	   /*  record.		     */
  { first_rec = NextRecord(first_rec); }	   /*			     */
						   /*			     */
  if ( first_rec == RecNULL )			   /*			     */
  { ERROR2("Crossref not found:",ref);		   /*			     */
    return;					   /*			     */
  }						   /*			     */
  ref = *RecordHeap(first_rec);			   /* Get the new key.	     */
						   /*			     */
  if ( (s = malloc(strlen(ref)+3)) == NULL )	   /* allocate temp memory   */
  { OUT_OF_MEMORY("update_crossref()"); }	   /*		             */
						   /*			     */
  (void)sprintf(s,				   /* construct new crossref */
		(**hp=='"'?"\"%s\"":"{%s}"),	   /*			     */
		ref);				   /*			     */
						   /*			     */
  *hp = symbol(s);				   /* store new crossref     */
  free(s);					   /* free temp memory	     */
}						   /*------------------------*/


#ifdef DEBUG
/*-----------------------------------------------------------------------------
** Function:	printchar()
** Purpose:	For debugging of regex.c
** Arguments:
**	c
** Returns:	nothing
**___________________________________________________			     */
void printchar(c)				   /*                        */
  char c;					   /*                        */
{ ErrC(c);					   /*                        */
}						   /*------------------------*/
#endif
