/* This is a simple demo program for the S-Lang interpreter.  It would 
 * take very little work to make this demo *really* useful.
 */

#include "config.h"

#include <stdio.h>
#include <math.h>
#include <string.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "slang.h"

#if !defined (__GO32__) && !defined(msdos) && !defined (__EMX__)
# define USE_ANSI_COLOR
#endif

static void help (void)
{
   puts("ALL statements MUST be terminated with a ';' character, e.g., quit();\n");
   puts("Available functions:");
   puts("  cos, sin, tan, atan, acos, asin, exp, log, sqrt, fabs, log10, pow, PI, E");
   puts("  print -- prints string, e.g. print(\"hello world!\\n\");");
   puts("\nas well as intrinsic S-Lang functions.");
   puts("See S-Lang language documentation for further details.\n");
   SLang_run_hooks ("calc_help", NULL, NULL);
}      

/* The following three functions will be callable from the interpreter */
static void quit_calc (void)
{
   SLang_reset_tty ();
#ifdef MALLOC_DEBUG
   SLmalloc_dump_statistics ();
#endif
   exit (SLang_Error);
}


static void print (char *s)
{
   fputs(s, stdout); fflush (stdout);
}

static void error(char *s)
{
   fprintf(stderr, "%s\n", s);
   SLang_Error = INTRINSIC_ERROR;
}

/* Now here is a table that provides the link between the above functions and
   the S-Lang interpreter */
SLang_Name_Type Calc_Intrinsics[] =
{
   MAKE_INTRINSIC(".print", print, VOID_TYPE, 1),
   MAKE_INTRINSIC(".quit", quit_calc, VOID_TYPE, 0),
   MAKE_INTRINSIC(".help", help, VOID_TYPE, 0),
   MAKE_INTRINSIC(".error", error, VOID_TYPE, 1),
   SLANG_END_TABLE
};




/* forward declarations */
static int take_input (void);
static int calc_open_readline (SLang_Load_Type *);
static int calc_close_readline (SLang_Load_Type *);

int main (int argc, char **argv)
{
   if (!init_SLang() 		       /* basic interpreter functions */
       || !init_SLmath() 	       /* sin, cos, etc... */
#ifdef __unix__
       || !init_SLunix()
#endif
       || !init_SLfiles()	       /* file i/o */
       /* || !init_SLmatrix()	*/       /* matrix manipluation */
       || !SLang_add_table(Calc_Intrinsics, "Calc"))   /* calc specifics */
     {
	fprintf(stderr, "Unable to initialize S-Lang.\n");
	exit(1);
     }

   if (SLang_init_tty (-1, 0, 1))
     {
	fprintf(stderr, "Unable to initialize tty.");
	exit (-1);
     }
   
   SLang_set_abort_signal (NULL);
   
   SLang_Traceback = 1;
   SLang_load_file("calc.sl");
   
   while (--argc && !SLang_Error)
     {
	argv++;
	SLang_load_file (*argv);
     }

   
   fputs("Type 'help();' for help and a list of available functions.\n", stdout);
   fputs("Note also that statements end in a ';'\n", stdout);
   fputs("\nIt is also important to note that most binary operators +, -, *, /,\n", stdout);
   fputs("as well as the '=' sign must be surrounded by spaces!\n", stdout);
   fputs("\nType `quit;' to exit this program.\n", stdout);
   
   SLang_User_Open_Slang_Object = calc_open_readline;
   SLang_User_Close_Slang_Object = calc_close_readline;
   
   while(1)
     {
	if (SLang_Error) 
	  {
	     SLang_doerror (NULL);
	     SLang_restart(1);
	  }
	
	SLKeyBoard_Quit = SLang_Error = 0;
	if (take_input () && (SLang_Last_Key_Char == SLang_RL_EOF_Char))
	  break;
     }
   
   SLang_reset_tty ();
   return (SLang_Error);
}


/* For a detailed explanation of all of this, see slang/demo/useropen.c */

static char *read_using_readline (SLang_Load_Type *x)
{
   int n;
   SLang_RLine_Info_Type *rli;
   
   rli = (SLang_RLine_Info_Type *) x->ptr;
   *rli->buf = 0;
   
   if (x->top_level) rli->prompt = "Calc> ";
   else rli->prompt = "_Calc> ";
   n = SLang_read_line (rli);
   
   putc ('\n', stdout);  fflush (stdout);
   if (n <= 0) return NULL;
   if ((n == 1) && (*rli->buf == SLang_RL_EOF_Char)) return "quit;";
   
   SLang_rline_save_line (rli);
   return (char *) rli->buf;
}

static SLang_RLine_Info_Type Calc_RLI;
static unsigned char Calc_RLI_Buf[256];

static int calc_open_readline (SLang_Load_Type *x)
{
   switch (x->type)
     {
      case 'r':
	
	x->ptr = (char *) &Calc_RLI;
	x->read = read_using_readline;
	
	if (Calc_RLI.buf != NULL)
	  {
	     /* everything else already initialized. Just return. */
	     return 0;
	  }
	
	Calc_RLI.buf = Calc_RLI_Buf;
	Calc_RLI.buf_len = 255;
	Calc_RLI.tab = 8;
	Calc_RLI.edit_width = 79;
	Calc_RLI.dhscroll = 20;
	Calc_RLI.prompt = "Calc> ";
	Calc_RLI.getkey = SLang_getkey;
#ifdef USE_ANSI_COLOR
	Calc_RLI.flags = SL_RLINE_USE_ANSI;
#endif
	SLang_init_readline (&Calc_RLI);
	return 0;
	
      default:
	/* unrecognized-- let S-lang handle it */
	return SL_OBJ_UNKNOWN;
     }
}

static int calc_close_readline (SLang_Load_Type *x)
{
   switch (x->type)
     {
      case 'r':
	/* Nothing to do */
	return 0;
	
      default:
	/* unrecognized-- let S-lang handle it */
	return SL_OBJ_UNKNOWN;
     }
}

static int take_input ()
{
   SLang_Load_Type x;
   memset ((char *) &x, 0, sizeof (SLang_Load_Type));
   x.type = 'r';		       /* readline type */
   x.read = read_using_readline;       /* function to call to perform the read */
   return SLang_load_object (&x);
}
