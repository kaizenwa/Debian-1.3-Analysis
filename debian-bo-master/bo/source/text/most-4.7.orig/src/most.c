#define __MOST_VERSION__ "4.7.0"

/* 
 *		Most ---- a more/less paging type program.
 * 
 *  Copyright (C) 1991, 1996 by John E. Davis. (davis@space.mit.edu)
 * 
 *		    The following disclaimer from GNU emacs.
 * 
 *				  NO WARRANTY
 * 
 * BECAUSE THIS PROGRAM IS LICENSED FREE OF CHARGE, WE PROVIDE ABSOLUTELY NO
 * WARRANTY, TO THE EXTENT PERMITTED BY APPLICABLE STATE LAW.  EXCEPT WHEN
 * OTHERWISE STATED IN WRITING, JOHN E. DAVIS AND/OR OTHER PARTIES PROVIDE THIS
 * PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND
 * PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE,
 * YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
 * 
 * IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW WILL JOHN E. DAVIS, AND/OR ANY
 * OTHER PARTY WHO MAY MODIFY AND REDISTRIBUTE THIS PROGRAM AS PERMITTED BELOW,
 * BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY LOST PROFITS, LOST MONIES, OR
 * OTHER SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR
 * INABILITY TO USE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
 * RENDERED INACCURATE OR LOSSES SUSTAINED BY THIRD PARTIES OR A FAILURE OF THE
 * PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS) THIS PROGRAM, EVEN IF YOU HAVE
 * BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES, OR FOR ANY CLAIM BY ANY OTHER
 * PARTY.
 * 
 *			 GENERAL PUBLIC LICENSE TO COPY
 * 
 * 1. You may copy and distribute verbatim copies of this source file as you
 * receive it, in any medium, provided that you conspicuously and appropriately
 * publish on each copy a valid copyright notice "Copyright (C) 1990 John E.
 * Davis"; and include following the copyright notice a verbatim copy of the
 * above disclaimer of warranty and of this License.  You may charge a
 * distribution fee for the physical act of transferring a copy.
 * 
 * 2. You may modify your copy or copies of this source file or
 * any portion of it, and copy and distribute such modifications under
 * the terms of Paragraph 1 above, provided that you also do the following:
 * 
 *   a) cause the modified files to carry prominent notices stating
 *   that you changed the files and the date of any change; and
 * 
 *   b) cause the whole of any work that you distribute or publish,
 *   that in whole or in part contains or is a derivative of this
 *   program or any part thereof, to be licensed at no charge to all
 *   third parties on terms identical to those contained in this
 *   License Agreement (except that you may choose to grant more extensive
 *   warranty protection to some or all third parties, at your option).
 * 
 *   c) You may charge a distribution fee for the physical act of
 *   transferring a copy, and you may at your option offer warranty
 *   protection in exchange for a fee.
 * 
 * Mere aggregation of another unrelated program with this program (or its
 * derivative) on a volume of a storage or distribution medium does not bring
 * the other program under the scope of these terms.
 * 
 * 3. You may copy and distribute this program (or a portion or derivative
 * of it, under Paragraph 2) in object code or executable form under the terms
 * of Paragraphs 1 and 2 above provided that you also do one of the following:
 * 
 *   a) accompany it with the complete corresponding machine-readable
 *   source code, which must be distributed under the terms of
 *   Paragraphs 1 and 2 above; or,
 * 
 *   b) accompany it with a written offer, valid for at least three
 *   years, to give any third party free (except for a nominal
 *   shipping charge) a complete machine-readable copy of the
 *   corresponding source code, to be distributed under the terms of
 *   Paragraphs 1 and 2 above; or,
 * 
 *   c) accompany it with the information you received as to where the
 *   corresponding source code may be obtained.  (This alternative is
 *   allowed only for noncommercial distribution and only if you
 *   received the program in object code or executable form alone.)
 * 
 * For an executable file, complete source code means all the source code for
 * all modules it contains; but, as a special exception, it need not include
 * source code for modules which are standard libraries that accompany the
 * operating system on which the executable file runs.
 * 
 * 4. You may not copy, sublicense, distribute or transfer this program
 * except as expressly provided under this License Agreement.  Any attempt
 * otherwise to copy, sublicense, distribute or transfer this program is void and
 * your rights to use the program under this License agreement shall be
 * automatically terminated.  However, parties who have received computer
 * software programs from you with this License Agreement will not have
 * their licenses terminated so long as such parties remain in full compliance.
 * 
 * In other words, you are welcome to use, share and improve this program.
 * You are forbidden to forbid anyone else to use, share and improve
 * what you give them.   Help stamp out software-hoarding! 
 */
#include "config.h"

#include <stdio.h>
#include <ctype.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <errno.h>
#include <stdarg.h>

#include <slang.h>
#include "jdmacros.h"

#include "most.h"
#include "search.h"
#include "window.h"
#include "file.h"
#include "sysdep.h"
#include "keym.h"
#include "display.h"



int Most_S_Opt = 0;		       /* squeeze liness */
int Most_A_Opt = 1;		       /* automatically choose -b if necessary */
int Most_V_Opt = 0;		       /* display control chars */
int Most_B_Opt = 0;		       /* display Binary File */
int Most_T_Opt = 0;		       /* display tab as ^I-- valid only with V option */
int Most_D_Opt = 0;		       /* delete file mode  (see ':D')  */
int Most_K_Opt = 0;		       /* Display 8 bit unformatted (Kanji) */
int Most_Z_Opt = 0;		       /* Gunzip on the fly */
int Most_Want_Exit;
int Most_Secure_Mode;
int Most_Captive_Mode;
static int  Most_Starting_Line;
char *Most_Program;	/* Program Name (argv[0]) */

char *Most_Version = __MOST_VERSION__ ;


#ifdef VMS
# ifndef isalpha
#  define isalpha(x) \
   (((x >= 'A') && (x <= 'Z'))||((x >= 'a') && (x <= 'z')) ? 1 : 0)
# endif
#endif


static void usage (void)
{
   fprintf(stderr,"MOST version S%s Usage:\n", Most_Version);
   fprintf(stderr, "most [-1bckstvw] [+/string] [+line number] [+s] [+d] file...\n");
   fputs(" where: -1:  assume VT100 terminal. (VMS only)\n", stderr); 
   fputs("        -b:  Startup in binary mode.\n", stderr);
   fputs("        -c:  Make searches case sensitive.\n", stderr);
   fputs("        -k:  Kanji mode.\n", stderr);
   fputs("        -s:  Squeeze out excess blank lines.\n", stderr);
   fputs("        -t:  Display tabs as ^I.  If this option is immediately followed\n", stderr);
   fputs("               by an integer, the integer sets the tab width.\n", stderr);
   fputs("        -v:  Do not interpret backspace formatting characters.\n", stderr);
   fputs("        -w:  Wrap lines.\n", stderr);
   fputs("        -z:  No gunzip-on-the-fly.\n", stderr);
   fputs("        +/string:\n", stderr);
   fputs("             Search for string\n", stderr);
   fputs("        +line number\n", stderr);
   fputs("             Start up at specified line number.\n", stderr);
   fputs("        +d:  Allow file deletion.\n", stderr);
   fputs("        +s:  Secure Mode-- no edit, cd, shell, and reading files not\n", stderr);
   fputs("               already listed on the command line.\n", stderr);
   fprintf(stderr, "\nExample: most -ct4 +82 keymap.c\n");
   fputs(" makes searches case sensitive, sets tabwidth to 4, and displays the file\n", stderr);
   fputs(" keymap.c starting at line 82.\n", stderr);
}

	 
   
static void do_extended_switches(char *str)
{
   int i;
   char ch;
   char numstr [256];
    
   i = 0;
   ch = *(++str);
   if ( ch == '/')
      {
	 strcpy (Most_Search_Str,++str);
	 return;
      }
   
   if (ch >= '0' && ch <= '9')
     {
	while ((ch >= '0' && ch <= '9') && (i < 10))
	  {
	     numstr[i++] = ch;
	     ch = *(++str);
	  }
	numstr[i] = '\0';
	if (1 == sscanf (numstr,"%d", &i))
	  Most_Starting_Line = i;
	return;
     }
   
   if (isalpha(ch))
     {
	while (isalpha(ch))
	  {
	     switch (ch)
	       {
		case 'D':
		case 'd':
		  Most_D_Opt = 1;   /* delete file mode */
		  break;
		case 'S':
		case 's':
		  Most_Secure_Mode = 1;
		  break;
		default:
		  fprintf(stderr,"%s invalid extended option %c ignored.\n",
			  Most_Program, ch);
	       }
	     ch = *(++str);
	  }

	return;
     }
   
   fprintf(stderr,"%s: switch '+%s' not valid.\n", Most_Program, str);
   exit (1);
}

/* if non-zero, assume terminal is only a generic vt100 */
static int assume_vt100 = 0;

static void do_switches(char *str)
{
   char ch;
   if (*str == '-') str++;
    while ((ch = *str++) != '\0')
      {
	 switch (ch)
	   {
	    default:
	      fprintf(stderr,"%s: invalid switch %c ignored.\n",
		      Most_Program, ch);
	      break;
	      
	    case 'C':
	    case 'c':
	      Most_Case_Sensitive = 1;
	      break;
	    case 's':
	    case 'S':
	      Most_S_Opt = 1; break;
	    case 'V':
	    case 'v':
	      Most_V_Opt = 1;  /* verbose-- convert control chars to '^' 'ch' */
	      break;
	    case 'W':
	    case 'w':  Most_W_Opt = 1; break;
	      
	    case 'K':		       /* Kanji option */
	    case 'k':  
	      Most_K_Opt = 1; break;
	      
	    case 'B':
	    case 'b':
	      Most_B_Opt = 1;  /* Binary display 8 bit */
	      break;
	      
	    case 'z':
	    case 'Z':
	      Most_Z_Opt = 1;  /* NO Gunzip-on-the-fly */
 	      break;
 	      
	    case 't':
	    case 'T': /* expand tabs to '^I'; meaningful only with 'v' */
	      ch = *str;
	      if ((ch <= '9') && (ch >= '0'))
		{
		   str++;
		   Most_Tab_Width = (int) ch - '0';
		   if (Most_Tab_Width == 0) Most_T_Opt = 1;
		}
	      else Most_T_Opt = 1;
	      break;
	      
	    case 'n': case 'N':
	      /* could be the Gopher Naive user switch --- ignored. */
	      break;
	    case '1': assume_vt100 = 1;
	      break;
	   }
      }
}

void most_exit_error(char *fmt,...)
{
   va_list ap;
   
   most_reset_tty ();
   most_reset_display();
   if (fmt != NULL) 
     {
	va_start (ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end (ap);
	putc ('\n', stderr);
     }
#ifdef MALLOC_DEBUG
   SLmalloc_dump_statistics ();
#endif
   exit(1);
}

static void play_cat(char *file)
{
   char buf[4096 * 4];
   int n;
   FILE *fp;
   
   if (file == NULL) fp = stdin; 
   else
     {
	fp = fopen(file, "r");
	if (fp == NULL) return;
     }
   
   while ((n = fread(buf, 1, 4096 * 4, fp)) > 0)
     {
	int m;
	m = fwrite (buf, 1, n, stdout);
	if (m != n)
	  {
	     fprintf (stderr, "fwrite returned %d, errno = %d\n", 
		      m, errno);
	     exit (1);
	  }
     }
}



void most_initialize_most (void)
{
   Most_S_Opt = 0;
   Most_A_Opt = 1;
   Most_V_Opt = 0;
   Most_B_Opt = 0;
   Most_T_Opt = 0;
   Most_D_Opt = 0;
   Most_K_Opt = 0;
   Most_W_Opt = 0;
   
   Most_Selective_Display = 0;
   *Most_Search_Str = 0;   Most_Search_Dir = 1;
   Most_Top_Win = Most_Win = NULL;
   Most_Buf = NULL;
   Most_Eob = NULL;
   Most_Beg = NULL;
   Most_Captive_Mode = Most_Secure_Mode = 0;
   Most_Want_Exit = 0;
}

   
static void do_most (char *file, int start)
{
   int piped, row, col;
    
   most_get_cdir(Most_C_Dir);

   row = col = 0;
   if (file[0] == '\0') piped = 1; else piped = 0;
    
   if ((-1 == most_find_file (file))
       && (Most_Num_Files == 1))
     most_exit_error ("%s: failed to open for reading.", file);

   most_init_display ();

   most_goto_line(start);

   Most_Curs_Offset = Most_C_Pos - Most_Beg;

   if (*Most_Search_Str && ((row = most_search(Most_C_Pos, 1, &col)) > 0))
     {
	most_goto_line(row);
     }
    else
      {
	 row = Most_C_Line;
	 col = 1;
      }
   
   most_window_buffer();
   Most_Curs_Row = Most_Win->curs_line = row - Most_C_Line + 1;
   Most_Win->curs_offset = Most_Curs_Offset;
   Most_Curs_Col = Most_Win->curs_col = col;
   most_redraw_window();
   most_update_status();

   while (Most_Want_Exit == 0)
     {
	most_execute_key();
     } 
}


void most_exit_most (void)
{
   if (Most_Want_Exit) return;
   Most_Want_Exit = 1;
   most_clear_minibuffer ();
   most_reset_tty ();
   most_reset_display ();
   most_free_windows ();
#ifdef MALLOC_DEBUG
   SLmalloc_dump_statistics ();
#endif
}


int most (int argc, char **argv)
{
   char file[MAX_PATHLEN], *switches;
   int file_i = 0, quit,i,piped,a_opt;
   unsigned long context;

#ifdef VMS
   char filename[256];
#else
   int j;
#endif

   
   Most_Program = argv[0];
   piped = 0;
   
   switches = getenv ("MOST_PROMPT");
   if ((switches != NULL) && (*switches != 0)) Most_Global_Msg = switches;
   
   switches = getenv("MOST_SWITCHES");
   if (switches !=  NULL)  do_switches(switches);
          
   
    i = 1;
    if (argc > 1)
      {
          quit = 0;
          while ((!quit) && (i < argc))
            {
                if (argv[i][0] == '-')
                  do_switches(argv[i++]);
                else if (argv[i][0] == '+')
                  do_extended_switches(argv[i++]);
                else quit = 1;
            }
      }

    
   if (i == argc)
     {
	if (isatty(0))   /* 1 if stdin is a terminal, 0 otherwise */
	  {
	     usage ();
	     return 0;
	  }
	/* assume input is from stdin */
	file[0] = '\0';  /* tells most this is stdin */
	piped = 1;
	if (!isatty(fileno(stdout))) 
	  {
	     play_cat(NULL);
	     return 0;
	  }
      }
   else
     strcpy(file,argv[i]);

   if (!isatty(fileno(stdout)))
     {
	while (i < argc) play_cat(argv[i++]);
	exit(0);
     }

   Most_Num_Files = 0;
   context = 0;

   SLtt_get_terminfo();
   SLtt_Ignore_Beep = 1;
   SLtt_Use_Ansi_Colors = 0;
   most_setup_colors ();
   most_init_tty ();
   most_init_keymaps ();

    if (Most_B_Opt) Most_A_Opt = 0;   /* explicit b overrides a */
    a_opt = Most_A_Opt;

    if (!piped)
      {
          file_i = i;
#ifdef VMS
          while(i < argc)
            {
	       if (Most_Num_Files >= MOST_MAX_FILES) break;
                if (argv[i][0] == '.') strcpy(file,"*"); else *file = 0;
                strcat(file, most_unix2vms(argv[i++]));
                while (most_expand_file_name(file,filename))
                  {
                      Most_File_Ring[Most_Num_Files] = (char*) MOSTMALLOC(strlen(filename) + 1);
                      strcpy(Most_File_Ring[Most_Num_Files++], filename);
                  }
            }
	 if (Most_Num_Files) strcpy(file,Most_File_Ring[0]);
	 else fputs("%%MOST-W-NOFILES, no files found\n", stderr);
#else
	 Most_Num_Files = argc - i;
	 if (Most_Num_Files > MOST_MAX_FILES) 
	   {
	      Most_Num_Files = MOST_MAX_FILES;
	      argc = Most_Num_Files + i;
	   }
	 
	 j = 0;
	 while (i < argc)
	   {
	      Most_File_Ring[j++] = argv[i++];
	   }
#endif
      }

   if (Most_Num_Files || piped) do_most(file, Most_Starting_Line);
   else if (Most_Num_Files == 0) 
     fprintf(stderr,"File %s not found\n", argv[file_i]);
   
   most_exit_most ();
   return (0);
}
