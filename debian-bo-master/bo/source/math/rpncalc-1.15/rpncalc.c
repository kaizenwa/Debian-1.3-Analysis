/***************************************************************************
 * rpncalc.c								   *
 *									   *
 * A little RPN (Reverse Polish Notation) calculator, rudimentary          *
 * emulating a HP 28S. 					                   *
 * 								           *
 * rpncalc is (c) David Frey, 1993, 1994, 1995, 1996, 1997		   *
 *								           * 
 * This program is free software; you can redistribute it and/or modify it *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation; either version 2 of the License, or (at your  *
 * option) any later version.                                              *
 *									   * 
 * This program is distributed in the hope that it will be useful, but     *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *									   * 
 * You should have received a copy of the GNU General Public License       *
 * along with this program; if not, write to the Free Software Foundation, *
 * Inc., 59 Temple Place Suite 330, Boston, MA 02111-1307 USA.             *
 ***************************************************************************/

/* $Id: rpncalc.c,v 1.4 1997/01/19 19:09:00 david Rel $
 * $Log: rpncalc.c,v $
 * Revision 1.4  1997/01/19 19:09:00  david
 * New command `char' displays character code of a character; help now
 * displays the commands out of the cmdtab[] array.
 *
 * Revision 1.4  1997/01/19 18:19:23  david
 * Version 1.2: new command `char' displays character code etc of a character,
 * help now displays the commands out of the cmdtab[] array.
 *
 * Revision 1.2  1996/09/13 20:21:29  david
 * lclint additions
 *
 * Revision 1.1  1996/07/13 20:53:44  david
 * Added operator completion, renaming due to linting
 *
 * Revision 1.0  1995/12/31 18:14:13  david
 * Initial revision
 * */ 

#include <stdio.h>
#include <stdlib.h> 
#include <unistd.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

#include <signal.h> 
#include <locale.h>

#ifdef HAVE_GETOPT_LONG
#include <getopt.h>
#endif

#ifdef HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include "cmds.h"
#include "utils.h"
#include "stack.h"
#include "rpncalc.h"

extern int pushtostack;
extern int errno;
extern int optind;

char *progname;

#ifndef linux
extern double pow10(double y);
extern double pow2(double y);
#endif

/* forward declarations for the table below */
double help(void);
double warranty(void);

/* The idea of the following was taken out of the Bison Manual p. 31 */
/* Sorting and lsearch out of iX 11/1995 */
struct cmd 
{
  const char *fname;
  short argno;			/* 0..2: no of arguments */
  short pushrestostack;
  double  (*fnct)();
};

struct cmd cmdtab[] =
{
  { "show"    , 0, 0, showstack },
  { "push"    , 1, 0, push      },
  { "pop"     , 0, 0, pop       },
  { "pick"    , 1, 1, pick      },
  { "swap"    , 0, 0, swap      },
  { "over"    , 0, 0, over      },
  { "roll"    , 0, 0, roll      },
  { "dup"     , 1, 0, dupel     },
  { "dup2"    , 1, 0, dupel2    },
  { "dupn"    , 2, 0, dupn      },
  { "drop"    , 0, 0, drop      },
  { "dropn"   , 1, 0, dropn     },
  { "depth"   , 0, 1, depth     },
  { "prec"    , 1, 0, prec      },
  { "clear"   , 0, 0, clear     },
  { "chs"     , 1, 1, chs       },
  { "+"       , 2, 1, plus      },
  { "-"       , 2, 1, minus     },
  { "*"       , 2, 1, multiply  },
  { "/"       , 2, 1, divide    },
  { "^"       , 2, 1, pow       },
  { "inv"     , 1, 1, inv       },
  { "sqrt"    , 1, 1, sqrt      }, 
  { "sqr"     , 1, 1, sqr       },
  { "sin"     , 1, 1, sin       },
  { "cos"     , 1, 1, cos       },
  { "tan"     , 1, 1, tan       },
  { "asin"    , 1, 1, asin      },
  { "acos"    , 1, 1, acos      },
  { "atan"    , 1, 1, atan      },
  { "atan2"   , 2, 1, atan2     },
  { "sinh"    , 1, 1, sinh      },
  { "cosh"    , 1, 1, cosh      },
  { "tanh"    , 1, 1, tanh      }, 
  { "asinh"   , 1, 1, asinh     },
  { "acosh"   , 1, 1, acosh     },
  { "atanh"   , 1, 1, atanh     },
  { "ln"      , 1, 1, log       },
  { "log"     , 1, 1, log10     },
  { "ld"      , 1, 1, log2      },
  { "exp"     , 1, 1, exp       },
  { "alog"    , 1, 1, pow10     },
  { "shl"     , 1, 1, pow2      },
  { "j0"      , 1, 1, j0        },
  { "j1"      , 1, 1, j1        },
  { "jn"      , 2, 1, jn        },
  { "y0"      , 1, 1, y0        },
  { "y1"      , 1, 1, y1        },
  { "yn"      , 2, 1, yn        },
  { "erf"     , 1, 1, erf       },
  { "erfc"    , 1, 1, erfc      },
  { "lgamma"  , 1, 1, lgamma    },
  { "abs"     , 1, 1, fabs      },
  { "ceil"    , 1, 1, ceil      },
  { "fact"    , 1, 1, fact      },
  { "!"       , 1, 1, fact      },
  { "mod"     , 2, 1, mod       },
  { "div"     , 2, 1, idiv      },
  { "gcd"     , 2, 1, gcd       },
  { "sum"     , 0, 1, sum       },	/* special case, takes all elements */
  { "prod"    , 0, 1, prod      },	/* special case, takes all elements */
  { "hex"     , 0, 0, sethex    },
  { "dec"     , 0, 0, setdec    },
  { "oct"     , 0, 0, setoct    },
  { "char"    , 0, 0, setchar   },
  { "and"     , 2, 1, and       },
  { "&"       , 2, 1, and       },
  { "or"      , 2, 1, or        },
  { "|"       , 2, 1, or        },
  { "xor"     , 2, 1, xor       },	/* ^ for XOR would be irritating */
  { "not"     , 1, 1, not       },
  { "~"       , 1, 1, not       },
  { "pi"      , 0, 1, pi        },
  { "e"       , 0, 1, e         },
  { "help"    , 0, 0, help      },
  { "?"       , 0, 0, help      },
  { "warranty", 0, 0, warranty  },
};

#define NCMDTAB (sizeof cmdtab / sizeof cmdtab[0])

#ifdef HAVE_GETOPT_LONG
struct option const long_options[] =
{
  {"help",    no_argument, 0, 'h'},
  {"help",    no_argument, 0, '?'},
  {"version", no_argument, 0, 'v'},
  {(char *)0, 0,           0, (char)0}
};
#endif

double help(void)
{
  int i;

  printf("The following operations and constants are recognized:\n\n");
  printf("  ");
  for(i=0;i<NCMDTAB;i++) {
    printf("%s%*c",cmdtab[i].fname,8-strlen(cmdtab[i].fname),' ');
    if ((i%8)==7) printf("\n  ");
  }
  printf("\n\n");
  printf("Delimiters are ',', ';', space, tab, and newline.\n");

  return 0.0; /* dummy value */
}

double warranty(void)
{
#define COPYRIGHT "\
This program is free software; you can redistribute it and/or modify\n\
it under the terms of the GNU General Public License as published by\n\
the Free Software Foundation; either version 2 of the License, or\n\
(at your option) any later version.\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program; if not, write to the Free Software Foundation,\n\
Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n"

  printf(COPYRIGHT);
  return 0.0; /* dummy value */
}

static int cmp(const void *f1, const void *f2)
/* (String-)Compare the two function names, cf. iX 11/95 p 212 */
{
  return strcmp(((const struct cmd *)f1)->fname,
		((const struct cmd *)f2)->fname);
}

/* Evaluate the command line. The work is done in this procedure ! */
static int eval(char line[])
{
  int quit;		/* flags */
  char delimiters[] = " ,;\t\f\n";
  char *cmnd, *rest;		 /* one command line */
  double op1, op2, res;		 /* operands */

  cmnd = strtok(line, delimiters); res = 0; pushtostack=1;
  quit = ((cmnd == NULL) ||
  	  (strncmp(cmnd, "quit", 4) == 0) || (strncmp(cmnd,"q",1) == 0) || 
	  (strncmp(cmnd, "exit", 4) == 0));

  while (!quit) {
      /* Is it a number ? It yes, push it onto the stack. */      

      /* First assume it's a double */
      op1 = strtod(cmnd, &rest);
      if (strcmp(rest, "") == 0) {
	(void)push(op1); /* It is a float */
      }
      else {
	/* converting number according to the rules of the C programming
	 * language, e.g 0=octal, 0x=hexadecimal
	 */
	op1 = strtol(cmnd, &rest, 0);
	if (strcmp(rest, "") == 0) {
	  (void)push(op1); /* it is an integer */
	}
	else {
	  struct cmd dummykey;
	  struct cmd *f; 

	  /* the following bsearch-trick is from iX 11/95 p 212 */
	  dummykey.fname=cmnd;
   	  f=(struct cmd *)bsearch(&dummykey, cmdtab, NCMDTAB,
				   sizeof cmdtab[0], cmp);
	  if (f == NULL) {
	    fprintf(stderr,"%s: unknown command `%s'.\n",progname, cmnd);
	  }
	  else {
	    errno=0;
	    switch (f->argno) {
	      case 0: res=(*f->fnct)(); break;
	      case 1: op1=pop(); res=(*f->fnct)(op1); break;
	      case 2: op2=pop(); op1=pop(); res=(*f->fnct)(op1,op2); break;
	    }
	    if (errno != 0) { perror(NULL); }
	    else {
	      if (f->pushrestostack && pushtostack) (void)push(res);
	    }
	  }
	}
      }
      cmnd = strtok(NULL, delimiters);
      quit = ((cmnd == NULL) ||
  	      (strncmp(cmnd, "quit", 4) == 0) || (strncmp(cmnd,"q",1) == 0) || 
  	      (strncmp(cmnd, "exit", 4) == 0));

  }
    
  /* return quit; */
  return (cmnd != NULL);
}

/* print a short usage statement. Indicate that the argument must be quoted. */
static void usage(void)
{
  fprintf(stderr, "usage: %s [-h][-v] [\"expression\"]\n", progname);
}

#ifdef HAVE_READLINE
/* the initialisation code was taken from:
     fileman.c -- A tiny application which demonstrates how to use the
     GNU Readline library.  This application interactively allows users
     to manipulate files and their modes. 
 */

/* Attempt to complete on the contents of TEXT.  START and END show the
   region of TEXT that contains the word to complete.  We can use the
   entire line in case we want to do some simple parsing.  Return the
   array of matches, or NULL if there aren't any. */

char **rpncalc_completion(char *text, int start, int end)
{
  return completion_matches (text, command_generator);
}

/* Generator function for command completion.  STATE lets us know whether
   to start from scratch; without any state (i.e. STATE == 0), then we
   start at the top of the list. */
char *command_generator (char *text, int state)
{
  static int list_index, len;
  const char *name;

  /* If this is a new word to complete, initialize now.  This includes
     saving the length of TEXT for efficiency, and initializing the index
     variable to 0. */
  if (!state) { list_index=0; len=strlen (text); }

  /* Return the next name which partially matches from the command list. */
  while (list_index < NCMDTAB) {
    name = cmdtab[list_index].fname; list_index++;

    if (strncmp (name, text, len) == 0) return (dupstr(name));
  }

  /* If no names matched, then return NULL. */
  return ((char *)NULL);
}

/* Tell the GNU Readline library how to complete.  We want to try to complete
   on command names if this is the first word in the line. */
void initialize_readline(int interactive)
{
  /* Allow conditional parsing of the ~/.inputrc file. */
  rl_readline_name = "rpncalc";

  /* Tell the completer that we want a crack first. */
  if (interactive) {
    rl_attempted_completion_function = (const CPPFunction *)rpncalc_completion;
  }
  else {
    rl_bind_key('\t', rl_insert()); 
    /* Suppress key binding of TAB in batch mode */
  }
}

#endif

int main(int argc, char *argv[])
{
  int c, i;
  char *line=NULL;	/* entire line  */
  int quit;			/* should we quit ? */

  progname=strrchr(argv[0],'/');
  if (progname==NULL) progname=argv[0];
  else progname++;

#ifdef HAVE_GETOPT_LONG
  while ((c = getopt_long(argc, argv, "h?v",
			  long_options, (int *) 0)) != EOF) {
#else
  while ((c = getopt(argc, argv, "h?v")) != EOF) {
#endif
    switch (c) {
      case 0  : break;
      case 'h':
      case '?': usage(); return 0;
      case 'v': fprintf(stderr,"%s version 1.0\n", progname); return 1;
      default : break;
    }
  }

  setlocale(LC_CTYPE, ""); /* No "," and such BS for decimal points */

  fprintf(stderr,"%s version %s. Copyright (c) 1993, 1997 David Frey.\n",
	  progname, VERSION);
  fprintf(stderr,"This is free software with ABSOLUTELY NO WARRANTY.\n");

  signal(SIGFPE,SIG_IGN); /* ignore floating point exceptions. */

  /* Sort the command table, from iX 11/95 p 212 */
  qsort(cmdtab, NCMDTAB, sizeof cmdtab[0], cmp);

  if (argc > optind) {
    short int l;

    l=0; for(i=optind;i<argc;i++) l += (strlen(argv[optind])+1);
    line=(char *)xmalloc(l+1);
    strcpy(line, argv[optind]);
    for (i = optind+1; i < argc; i++) {
      strcat(line, " "); strcat(line, argv[i]);
    }

    quit=eval(line); (void)showstack();
    free(line); line=NULL;
  }
  else
  {
    int interactive;

    interactive=isatty(0); line=NULL;

#ifdef HAVE_READLINE    
    initialize_readline(interactive);
#endif
    if (interactive) {
      fprintf(stderr,"For details, type `warranty'.\n");
      fprintf(stderr,"Type `quit' to quit and `?' to get a summary.\n");

#ifdef HAVE_READLINE    
      using_history();
#endif
    }

    do {
      line=getline();
      quit=(line == NULL);
      if (!quit) {
	/* Skip white spaces */
	while (*line != '\0' && isspace(*line)) line++;
	if (*line != '\0') {
#ifdef HAVE_READLINE          
	  add_history(line);
#endif
	  quit=eval(line);
	  if (!quit) (void)showstack();
	}
      }
      free(line); line=NULL; /* line was malloc'd by readline/getline. */
    }
    while (!quit);
#ifdef HAVE_READLINE 
    remove_history();
#endif    
  }  
  return 0;
}						 /* main */
