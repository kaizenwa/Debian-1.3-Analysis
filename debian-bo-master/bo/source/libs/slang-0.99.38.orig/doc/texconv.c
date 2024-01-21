#include <stdio.h>
#include <time.h>

FILE *Input_Fp = stdin;
FILE *Output_Fp = stdout;

static int begin_parse (void);

/* --------------  Output routines ----------------------------------*/

static int Quiet_Mode;

static void msg_error (char *err, int severity)
{
   if (Quiet_Mode && !severity) return;
   if (severity == 0) fprintf (stderr, "Not understood: %s\n", err);
   else fprintf (stderr, "%s\n", err);
}

static void usage (void)
{
   fputs ("\
Usage: texconv [-hlp] [-quiet] input-file-name output-file-name\n\
   -hlp : generate a VMS style hlp file\n\
   -quiet: suppress warnings\n\
", stderr);
   exit (-1);
}

static int Hlp_Mode = 0;
static int Section_Num;
static int SubSection_Num;
static int SubSubSection_Num;

static int This_Column = 1;
static int Doing_Section;
static int Output_Ok;
static int No_Output;

static void my_putc (int x) 
{
   if (No_Output || (Output_Ok == 0)) return;
   
   if (Doing_Section && Hlp_Mode && (x == ' ')) x = '_';
   putc(x, Output_Fp);
   if (x == '\n') 
     {
	if (Doing_Section && !Hlp_Mode)
	  {
	     x = Doing_Section;
	     while (This_Column--) putc (x, Output_Fp);
	     putc ('\n', Output_Fp);
	  }
	Doing_Section = 0;
	This_Column = 1;
     }
   else This_Column++;
}

static void my_print (char *line)
{
   while (*line) my_putc ((int) *line++);
}


/* --------------------- routines to handle LaTeX commands ------------- */
static int bf_fun (char *tok)
{
   return begin_parse ();
}

static int em_fun (char *tok)
{
   return begin_parse ();
}

static int verb_fun (char *tok)
{
   int ch1, ch;
   
   ch1 = getc (Input_Fp);
   while ((ch1 != (ch = getc (Input_Fp))) && (ch != EOF))
     {
	my_putc (ch);
     }
   return 0;
}

static int swallow_argument (char *tok)
{
   int ch;
   while (((ch = getc (Input_Fp)) != EOF) && (ch != '{'));
   No_Output++;
   begin_parse ();
   No_Output--;
   return 0;
}

static int swallow_double_argument (char *tok)
{
   swallow_argument (tok);
   return swallow_argument (tok);
}

static int parbox_cmd (char *tok)
{
   int ch;
   while (((ch = getc (Input_Fp)) != EOF) && (ch != '{') && (ch != '['));
   if (ch == '[')
     while (((ch = getc (Input_Fp)) != EOF) && (ch != ']'));
   return swallow_argument (tok);
}

static int section_fun (char *tok)
{
   int ch;
   char line[256];
   Section_Num++;
   SubSection_Num = SubSubSection_Num = 0;
   
   if (Hlp_Mode) my_print ("\n1");
   else
     {
	sprintf (line, "\n%d ", Section_Num);
	my_print (line);
     }
   
   while (((ch = getc (Input_Fp)) != EOF) && (ch != '{'));
   Doing_Section = '=';
   begin_parse ();
   return 0;
}

static int subsection_fun (char *tok)
{
   char line[256];
   int ch;
   
   SubSection_Num++;
   SubSubSection_Num = 0;
   
   if (Hlp_Mode) my_print ("\n2");
   else
     {
	sprintf (line, "\n%d.%d ", Section_Num, SubSection_Num);
	my_print (line);
     }
   
   while (((ch = getc (Input_Fp)) != EOF) && (ch != '{'));
   Doing_Section = '-';
   begin_parse ();

   my_putc ('\n');
   return 0;
}

static int subsubsection_fun (char *tok)
{
   char line[256];
   int ch;
   SubSubSection_Num++;
   
   if (Hlp_Mode) my_print ("\n3");
   else
     {
	sprintf (line, "\n%d.%d.%d ", Section_Num, SubSection_Num, SubSubSection_Num);
	my_print (line);
     }
   
   while (((ch = getc (Input_Fp)) != EOF) && (ch != '{'));
   Doing_Section = '.';
   begin_parse ();
   
   my_putc ('\n');
   return 0;
}


static int extract_token (char *token)
{
   int ch;
   
   while (ch = getc (Input_Fp), (ch <= ' ') && (ch != EOF));
   
   while ((ch > ' ') && (ch != '}') && (ch != EOF))
     {
	*token++ = (char) ch;
	ch = getc (Input_Fp);
     }
   *token = 0;
   if (ch == EOF) 
     {
	msg_error ("Unexpected end of file.", 1);
     }
   
   return ch;
}



static int begin_fun (char *tok)
{
   int ch;
   char token[80], *t;
   char *verb = "end{verbatim}";
   
   while (ch = getc (Input_Fp), (ch != '{') && (ch != EOF));
   if (ch == EOF)
     {
	msg_error ("Unexpected end of file.", 1);
	return ch;
     }
   
   ch = extract_token (token);
   
   while ((ch != '}') && (ch != EOF)) ch = getc (Input_Fp);
   
   if (!strcmp (token, "document")) Output_Ok = 1;
   if (!strcmp (token, "verbatim"))
     {
	if (Output_Ok == 0) return 0;
	
	while ((ch = getc (Input_Fp)) != EOF)
	  {
	     if (ch == '\\')
	       {
		  char *t1 = verb;
		  t = verb;
		  while (((ch = getc (Input_Fp)) != EOF) && *t)
		    {
		       if ((char) ch == *t) t++;
		       else break;
		    }
		  
		  if (ch == EOF)
		    {
		       msg_error ("Unexpected end of file.", 1);
		       break;
		    }
		  
		  if ((*t == 0) && (ch <= ' ')) break;  /* verbatim finished */
		  
		  my_putc ('\\');
		  while (t1 < t) 
		    {
		       my_putc (*t1);
		       t1++;
		    }
		  ungetc (ch, Input_Fp);
	       }
	     else
	       my_putc (ch);
	  }
     }
   my_putc ('\n');
   return ch;
}

static int end_fun (char *tok)
{
   int ch;
   while (ch = getc (Input_Fp), (ch != '{') && (ch != EOF));
   
   if (ch == EOF)
     {
	msg_error ("Unexpected end of file.", 1);
	return EOF;
     }
   
   /* Later I will parse what we are ending */
   while ((ch != '}') && (ch != EOF)) ch = getc (Input_Fp);
   
   my_putc ('\n');
   return 0;
}

static int item_fun (char *tok)
{
   /* Later I will worry about actually indenting it. */
   my_print ("\n  *  ");
   return 0;
}

static int verbatim_fun (char *t)
{
   int ch;
   char *verb = "\\ev";
   
   if (Output_Ok == 0) return 0;
   while ((ch = getc (Input_Fp)) != EOF)
     {
	if (ch == '\n')
	  {
	     char *t1 = verb;
	     t = verb;
	     while (((ch = getc (Input_Fp)) != EOF) && *t)
	       {
		  if ((char) ch == *t) t++;
		  else break;
	       }
	     if (ch == EOF)
	       {
		  msg_error ("Unexpected end of file.", 1);
		  break;
	       }
		  
	     my_putc ('\n');
	     if ((*t == 0) && (ch <= ' ')) break;  /* verbatim finished */
		  
	     while (t1 < t) 
	       {
		  my_putc (*t1);
		  t1++;
	       }
	     ungetc (ch, Input_Fp);
	  }
	else my_putc (ch);
     }
   my_putc ('\n');
   return 0;
}


static int quote_fun (char *tok)
{
   int c;
   while (((c = getc (Input_Fp)) != EOF) && (c != '{'));
   my_putc ('`');
   begin_parse ();
   my_putc ('\'');
   return 0;
}

static int today_fun (char *tok)
{
    char *the_time;
    time_t c;

    c = time((time_t *) 0);
    the_time = (char *) ctime(&c);
   /* returns the form Sun Sep 16 01:03:52 1985\n\0 */
   the_time[24] = '\0';
   my_print (the_time);
   return 0;
}

   
typedef struct 
{
   char name[30];
   int (*fun)(char *);
   int type;
}
Token_List_Type;

Token_List_Type Token_List [] = 
{
   /* local extensions */
   {"bv", verbatim_fun, 0},
   {"key", quote_fun, 0},
   {"var", quote_fun, 0},
   {"exmp", quote_fun, 0},
   {"jed", (int (*)(char *)) "JED", 1},
   {"slang", (int (*)(char *)) "S-Lang", 1},
   {"MYBOX", NULL, 0},
	
   
   /* partially implemented */
   {"bf", bf_fun, 0},
   {"em", em_fun, 0},
   {"tt", em_fun, 0},
   {"it", em_fun, 0},
   {"sc", em_fun, 0},
   {"section", section_fun, 0},
   {"begin", begin_fun, 0},
   {"end", end_fun, 0},
   {"subsection", subsection_fun, 0},
   {"subsubsection", subsubsection_fun, 0},
   {"item", item_fun, 0},
   {"hspace", swallow_argument, 0},
   {"vspace", swallow_argument, 0},
   {"pagestyle", swallow_argument, 0},
   {"thispagestyle", swallow_argument, 0},
   {"verb", verb_fun, 0},
   {"today", today_fun, 0},
         
   {"times", (int (*)(char *)) "x", 1},
   {"bullet", (int (*)(char *)) " * ", 1},
   {"ldots", (int (*)(char *)) "...", 1},
   {"cdot", (int (*)(char *)) ".", 1},
   {"TeX", (int (*)(char *)) "TeX", 1},
   {"LaTeX", (int (*)(char *)) "LaTeX", 1},

   /* Not implemented (ignored) */
   {"bigskip", NULL, 0},
   {"par", NULL, 0},
   {"newline", NULL, 0},
   {"smallskip", NULL, 0},
   {"baselinestretch", NULL, 0},
   {"cleardoublepage", NULL, 0},
   {"pagenumbering", swallow_argument, 0},
   {"appendix", NULL, 0},
   {"newpage", NULL, 0},
   {"pagebreak", NULL, 0},
   {"parbox", parbox_cmd, 0},
   {"textwidth", NULL, 0},
   {"mbox", NULL, 0},
   {"hfill", NULL, 0},
   {"large", NULL, 0},
   {"huge", NULL, 0},
   {"documentstyle", NULL, 0},
   {"newcommand", NULL, 0},
   {"underline", NULL, 0},
   {"thepage", NULL, 0},
   {"def", NULL, 0},
   {"vfill", NULL, 0},
   {"tableofcontents", NULL, 0},
   {"hbox", NULL, 0},
   {"cal", NULL, 0},
   {"nopagebreak", NULL, 0},
   {"footnotesize", NULL, 0},
   {"Huge", NULL, 0},
   {"Large", NULL, 0},
   {"LARGE", NULL, 0},
   {"normalsize", NULL, 0},
   {"renewcommand", NULL, 0},
   {"small", NULL, 0},
   {"chapter", NULL, 0},
   {"evensidemargin", NULL, 0},
   {"oddsidemargin", NULL, 0},
   {"headsep", NULL, 0},
   {"topmargin", NULL, 0},
   {"fbox", NULL, 0},
   {"centerline", NULL, 0},
   {"kill", NULL, 0},
   {"parindent", NULL, 0},
   {"parskip", NULL, 0},
   {"vskip", NULL, 0},
   {"noindent", NULL, 0},
   {"hangindent", NULL, 0},
   {"break", NULL, 0},
   {"circ", NULL, 0},
     
     
   {"setlength", swallow_double_argument, 0},
   {"input", swallow_argument, 0},
   {"textheight", swallow_argument, 0},

   
   {"", NULL}
};
	
   
static int alpha_token (int ch)
{
   char token[80], *t;
   Token_List_Type *tl;
   char ch1, ch2;
   
   t = token + 1;
   *token = (char) ch;
   while ((ch = getc (Input_Fp)) != EOF)
     {
	if (((ch | 0x20) >= 'a') && ((ch | 0x20) <= 'z')) *t++ = (char) ch;
	else break;
     }
   *t = 0;
   
   if (ch == EOF) return -1;
   if (ch != ' ') ungetc (ch, Input_Fp);
   
   tl = Token_List;
   ch2 = *token;
   
   while ((ch1 = tl->name[0]) != 0)
     {
	if ((ch1 == ch2) && !strcmp (tl->name, token))
	  {
	     if (tl->fun != NULL) 
	       {
		  if (tl->type == 0) 
		    {
		       if ((Output_Ok == 0) && (tl->fun != begin_fun)) 
		       	 return 0;
		       return (*tl->fun) (token);
		    }
		  
		  my_print ((char *) tl->fun);
	       }
	     
	     return 0;
	  }
	tl++;
     }
   my_putc ('\\');
   my_print (token);
   my_putc (' ');
   msg_error (token, 0);
   return 0;
}


static void non_alpha_token (int ch)
{
   int ch1;
   
   ch1 = getc (Input_Fp);
   switch (ch)
     {
      case '\'':
	
	switch (ch1)
	  {
	     /* ISO-Latin 1 */
	   case 'A': ch1 = 193; break;
	   case 'E': ch1 = 201; break;
	   case 'I': ch1 = 205; break;
	   case 'O': ch1 = 211; break;
	   case 'U': ch1 = 218; break;
	   case 'Y': ch1 = 221; break;
	   case 'a': ch1 = 225; break;
	   case 'e': ch1 = 233; break;
	   case 'i': ch1 = 237; break;
	   case 'o': ch1 = 243; break;
	   case 'u': ch1 = 250; break;
	   case 'y': ch1 = 253; break;
	  }
	my_putc (ch1);
	break;
	
      case '{':
      case '}':
      case '%':
      case '$':
      case '&':
      case '_':
      case '^':
	my_putc (ch);
	
	/* drop */
      default:
	ungetc (ch1, Input_Fp);
	break;
     }
}

static int begin_parse (void)
{
   int ch;
   while ((ch = getc (Input_Fp)) != EOF)
     {
	/* Handle most common case */
	if ((ch >= 'a') && (ch <= 'z')) my_putc (ch);
	else if (ch == '%')
	  {
	     while ((ch = getc (Input_Fp)) != EOF)
	       {
		  if (ch == '\n') break;
	       }
	     if (ch == EOF) break;
	  }
	else if (ch == '\\')
	  {
	     if ((ch = getc (Input_Fp)) == EOF) break;
   
	     if (((ch | 0x20) >= 'a') && ((ch | 0x20) <= 'z'))
	       {
		  alpha_token (ch);
	       }
	     else non_alpha_token (ch);
	  }
	else if (ch == '{') 
	  {
	     if (Output_Ok) begin_parse ();	       /* new level */
	  }
	else if (ch == '}')
	  {
	     if (Output_Ok) return ch;		       /* end level */
	  }
	else if (ch != '$') my_putc (ch);
     }
   return ch;
}

int main (int argc, char **argv)
{
   argv++; argc--;
   
   while (argc)
     {
	if (!strcmp (*argv, "-hlp")) 
	  {
	     Hlp_Mode = 1;
	     argv++;
	     argc--;
	  }
	else if (!strcmp (*argv, "-quiet"))
	  {
	     Quiet_Mode = 1;
	     argv++;
	     argc--;
	  }
	else break;
     }
   
   if ((argc < 1) || (argc > 2)) usage ();
   if ((Input_Fp = fopen (*argv, "r")) == NULL)
     {
	fprintf (stderr, "Unable to open input file: %s\n", *argv);
	exit (-1);
     }
   argv++; argc--;
   
   if (argc) if ((Output_Fp = fopen (*argv, "w")) == NULL)
     {
	fprintf (stderr, "Unable to open output file: %s\n", *argv);
	exit (-1);
     }
   
   if (EOF != begin_parse ()) 
     {
	fprintf (stderr, "Parse Error.\n");
	return -1;
     }
   return 0;
}



	       
	
	       
