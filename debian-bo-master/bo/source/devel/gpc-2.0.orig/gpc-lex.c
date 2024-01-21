/* Gnu Pascal compiler lexical analyzer
   Copyright (C) 1989, Free Software Foundation, Inc.

This file is part of GNU GCC.

GNU GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 * Author: Jukka Virtanen <jtv@hut.fi>
 * This file is a modification of the GCC c-lex.c
 */

#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#include "config.h"
#include "tree.h"
#include "input.h"
#include "c-tree.h"
#include "flags.h"
#include "gpc-defs.h"
#include "gpc-parse.h"

#ifdef MULTIBYTE_CHARS
#include <stdlib.h>
#include <locale.h>
#endif

#ifndef errno
extern int errno;
#endif

/* Parser variables */
extern int yydebug;
YYSTYPE	yylval;			/*  the semantic value of the */
				/*  lookahead symbol          */

/* File used for outputting assembler code.  */
extern FILE *asm_out_file;

/* This is NULL_TREE before the start of the program header is parsed.
   I use this to recognize the keyword "Module" (and all other reserved
   words.)
 */
extern tree main_program_name;

int recognize_known_ids = 1;

tree lastiddecl;
int  last_id_value;

/* List of types and structure classes of the current declaration */
tree current_declspecs;

char *input_filename;		/* file being read */
char *main_input_filename;	/* top-level source file */

char *token_buffer;		/* Pointer to token buffer.
				   Actual allocated length is maxtoken + 2.  */

int lineno;			/* current line number in file being read */

FILE *finput;			/* input file.
				   Normally a pipe from the preprocessor.  */

/* lexical analyzer */

/* The reserved word recognition is done with
   Doug Schmidt's gperf program in O(1) time.

   The structure resword is defined in this file.
 */
#include "gpc-gperf.c"

static int maxtoken;		/* Current nominal length of token buffer */

/* Nonzero if end-of-file has been seen on input.  */
static int end_of_file;

int is_pascal_source = TRUE;

int check_newline ();

/* Nonzero tells yylex to ignore \ in string constants.  */
static int ignore_escape_flag = 0;

void
init_lex ()
{
  /* Make identifier nodes long enough for the language-specific slots.  */
  set_identifier_size (sizeof (struct lang_identifier));

  /* Start it at 0, because check_newline is called at the very beginning
     and will increment it to 1.  */
  lineno = 0;

#ifdef MULTIBYTE_CHARS
  /* Change to the native locale for multibyte conversions.  */
  setlocale (LC_CTYPE, "");
#endif

  maxtoken = 40;
  token_buffer = (char *)xmalloc (maxtoken + 2);
}

void
reinit_parse_for_function ()
{
}

/*
 * Process compiler directives.  NAME is given in low-case.
 * Not much work since most directives are already handled by preprocessor.
 * Return 0 if rest of directive shall be skipped, otherwise 1.
 */
static int
process_directive (name, comment_begin_char)
     register char *name;
     char comment_begin_char;
{
  register char plusminus;
  char c, d;
  int cont;
  if (pedantic)
    warning ("ISO Pascal does not define compiler directives%s",
             name[0] == 'p' && ! name [1] ? " :-)" : "" );
  	/* "pedantic" has been switched using a non-ISO feature. :-) */
  /*
   * @@@ Implement "long" compiler directives
   * such as (*$setlimit:128,borland-pascal,char-escapes,P-*)
   */
  if (strlen (name) != 1)   /* already handled by preprocessor */
    return 0;
  plusminus = getc (finput);
  switch (name [0])
    {
      case 'b':  /* Boolean complete evaluation */
        flag_short_circuit = (plusminus == '-');
        break;
      case 'c':  /* C-numbers */
        flag_c_numbers = (plusminus == '+');
        break;
      case 'e':  /* C char escape sequences */
        flag_c_escapes = (plusminus == '+');
        break;
      case 'l':  /* Lazy I/O */
        flag_lazy_io = (plusminus == '+');
        break;
      case 'n':  /* Nested comments */
        flag_nested_comments = (plusminus == '+');
        break;
      case 'p':  /* Be "pedantic" */
        pedantic = (plusminus == '+');
        break;
      case 'w':  /* Warnings */
        inhibit_warnings = (plusminus == '-');
        break;
      case 'x':
        flag_extended_syntax = (plusminus == '+');
        if (flag_extended_syntax)
          enable_keyword ("Operator");
        else
          disable_keyword ("Operator");
        break;
      case 'm':  /* Message */
        cont = 1;
        c = plusminus;
        do
          {
            switch (c)
              {
                case '}':
                  if (comment_begin_char == '{' || ! flag_nested_comments)
                    {
                      cont = 0;
                      break;
                    }
                case '*':
                  d = getc (finput);
                  ungetc (d, finput);
                  if (d == ')'
                      && (comment_begin_char != '{' || ! flag_nested_comments))
                    {
                      cont = 0;
                      break;
                    }
                default:
                  fprintf (stderr, "%c", c);
              }
            if (cont)
              c = getc (finput);
          }
        while (cont);
        ungetc (c, finput);
        fprintf (stderr, "\n");
        return 0; 
      case 'i':  /* Include */
      case 'd':  /* Define  */
        return 0;   /* already handled by preprocessor */
      default:  /* Preprocessor has already warned about undefined directive */
        return 1;
    }
  if (plusminus != '+' && plusminus != '-')
    warning ("compiler directive $%s not followed by `+' or `-'", name);
  return 1;
}

/*
 * Skip white space, including comments,
 * and dispatch compiler directives
 */
static int
skip_white_space (c)
     register int c;
{
  register int comment_is_directive;
  register char comment_begin_char, *dnp;
  char directive_name [256];

  for (;;)
    {
      switch (c)
	{
	case '(':
	  c = getc (finput);
	  if (c != '*')
	    {
	      ungetc (c, finput);
	      return '(';
	    }
          /* else: fall through */

	/* If flag_nested_comments is set, { is not equal to (* and
           } is not equal to *). This allows comment nesting like this
	    { (* foo (* bar *) }
	   and
	    (* {lk{{ ihmeess{ puhuko ru}tsia *)

	   But it violates the standard...
	 */
        case '{':
          comment_begin_char = c;
	  c = getc (finput);
          if (c == '$')
            {
              comment_is_directive = 1;
              c = getc (finput);
            }
          else
            comment_is_directive = 0;
	  while (1)
            {
	      if (c == '}'
                  && (comment_begin_char == '{' || ! flag_nested_comments))
		break;
	      else if (c == '*'
                       && (comment_begin_char != '{' || ! flag_nested_comments))
                {
		  while (c == '*')
		    c = getc (finput);
		  if (c != ')')
                    continue;
		  else
                    break;
	        }
              else if (c == '\n')
		lineno++;
	      else if (c == EOF) 
                {
		  error ("unterminated comment");
		  break;
	        }
              else if (comment_is_directive)
                {
                  dnp = directive_name;
                  while (c == ' ')
                    c = getc (finput);
                  while ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
                    {
                      if (c >= 'A' && c <= 'Z')
                        *dnp++ = c + ('a' - 'A');
                      else
                        *dnp++ = c;
                      c = getc (finput);
                    }
                  *dnp = 0;
                  while (c == ' ')
                    c = getc (finput);
                  ungetc (c, finput);
                  comment_is_directive = process_directive (directive_name,
                                                            comment_begin_char);
                  if (comment_is_directive)
                    {
                      c = getc (finput);
                      while (c == ' ')
                        c = getc (finput);
                      if (c != ',')
                        {
                          comment_is_directive = 0;
                          ungetc (c, finput);
                        }
                    }
                }
	      c = getc (finput);
	  }
	  if (c != EOF)
	    c = getc (finput);
	  break;

	case '\n':
	  c = check_newline ();
	  break;

	case ' ':
	case '\t':
	case '\f':
	case '\r':
	case '\b':
	  c = getc (finput);
	  break;

	case '\\':
	  if (flag_c_escapes) {
	      c = getc (finput);
	      if (c == '\n')
		  lineno++;
	      else
		  error ("stray '\\' in program");
	      c = getc (finput);
	      break;
	  } else
	      return (c);

	default:
	  return (c);
	}
    }
}

/* Make the token buffer longer, preserving the data in it.
   P should point to just beyond the last valid character in the old buffer.
   The value we return is a pointer to the new buffer
   at a place corresponding to P.  */

static char *
extend_token_buffer (p)
     char *p;
{
  int offset = p - token_buffer;

  maxtoken = maxtoken * 2 + 10;
  token_buffer = (char *) xrealloc (token_buffer, maxtoken + 2);
  if (token_buffer == 0)
    fatal ("virtual memory exceeded");

  return token_buffer + offset;
}

/* At the beginning of a line, increment the line number
   and handle a #line directive immediately following  */

int
check_newline ()
{
  register int c;
  register int token;

  lineno++;

  /* Read first nonwhite char on the line.  */

  c = getc (finput);
  while (c == ' ' || c == '\t')
    c = getc (finput);

  if (c != '#')
    {
      /* If not #, return it so caller will use it.  */
      return c;
    }

  /* Read first nonwhite char after the `#'.  */

  c = getc (finput);
  while (c == ' ' || c == '\t')
    c = getc (finput);

  /* If a letter follows, then if the word here is `line', skip
     it and ignore it; otherwise, ignore the line, with an error
     if the word isn't `pragma', `ident', `define', or `undef'.  */

  if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    {
      if (c == 'p')
	{
	  if (getc (finput) == 'r'
	      && getc (finput) == 'a'
	      && getc (finput) == 'g'
	      && getc (finput) == 'm'
	      && getc (finput) == 'a'
	      && ((c = getc (finput)) == ' ' || c == '\t' || c == '\n'))
	    {
	      if (pedantic)
		warning("ISO Pascal does not allow #pragma directive - ignored");
#if 0 /* jtv@hut.fi: No pragma code yet */
#ifdef HANDLE_SYSV_PRAGMA
	      return handle_sysv_pragma (finput, c);
#endif /* HANDLE_SYSV_PRAGMA */
#ifdef HANDLE_PRAGMA
	      HANDLE_PRAGMA (finput);
#endif /* HANDLE_PRAGMA */
#endif /* 0 */
	      goto skipline;
	    }
	}

      else if (c == 'd')
	{
	  if (getc (finput) == 'e'
	      && getc (finput) == 'f'
	      && getc (finput) == 'i'
	      && getc (finput) == 'n'
	      && getc (finput) == 'e'
	      && ((c = getc (finput)) == ' ' || c == '\t' || c == '\n'))
	    {
	      if (pedantic)
		warning("ISO Pascal does not allow #define directive");
#ifdef DWARF_DEBUGGING_INFO
	      if ((debug_info_level == DINFO_LEVEL_VERBOSE)
		  && (write_symbols == DWARF_DEBUG))
	        dwarfout_define (lineno, get_directive_line (finput));
#endif /* DWARF_DEBUGGING_INFO */
	      goto skipline;
	    }
	}
      else if (c == 'u')
	{
	  if (getc (finput) == 'n'
	      && getc (finput) == 'd'
	      && getc (finput) == 'e'
	      && getc (finput) == 'f'
	      && ((c = getc (finput)) == ' ' || c == '\t' || c == '\n'))
	    {
	      if (pedantic)
		warning("ISO Pascal does not allow #undef directive");
#ifdef DWARF_DEBUGGING_INFO
	      if ((debug_info_level == DINFO_LEVEL_VERBOSE)
		  && (write_symbols == DWARF_DEBUG))
	        dwarfout_undef (lineno, get_directive_line (finput));
#endif /* DWARF_DEBUGGING_INFO */
	      goto skipline;
	    }
	}
      else if (c == 'l')
	{
	  if (getc (finput) == 'i'
	      && getc (finput) == 'n'
	      && getc (finput) == 'e'
	      && ((c = getc (finput)) == ' ' || c == '\t'))
	    goto linenum;
	}
      else if (c == 'i')
	{
	  if (getc (finput) == 'd'
	      && getc (finput) == 'e'
	      && getc (finput) == 'n'
	      && getc (finput) == 't'
	      && ((c = getc (finput)) == ' ' || c == '\t'))
	    {
	      if (pedantic)
		warning("ISO Pascal does not allow #ident directive");

	      /* #ident.  The pedantic warning is now in cccp.c.  */

	      /* Here we have just seen `#ident '.
		 A string constant should follow.  */

	      while (c == ' ' || c == '\t')
		c = getc (finput);

	      /* If no argument, ignore the line.  */
	      if (c == '\n')
		return c;

	      ungetc (c, finput);
	      token = yylex ();
	      if (token != STRING_LITERAL
		  || TREE_CODE (yylval.ttype) != STRING_CST)
		{
		  error ("invalid #ident");
		  goto skipline;
		}

	      if (!flag_no_ident)
		{
#ifdef ASM_OUTPUT_IDENT
		  ASM_OUTPUT_IDENT (asm_out_file, TREE_STRING_POINTER (yylval.ttype));
#endif
		}

	      /* Skip the rest of this line.  */
	      goto skipline;
	    }
	}

      error ("undefined or invalid # directive");
      goto skipline;
    }

linenum:
  /* Here we have either `#line' or `# <nonletter>'.
     In either case, it should be a line number; a digit should follow.  */

  while (c == ' ' || c == '\t')
    c = getc (finput);

  /* If the # is the only nonwhite char on the line,
     just ignore it.  Check the new newline.  */
  if (c == '\n')
    return c;

  /* Something follows the #; read a token.  */

  ungetc (c, finput);
  token = yylex ();

  if (token == UNSIGNED_INTEGER
      && TREE_CODE (yylval.ttype) == INTEGER_CST)
    {
      int old_lineno = lineno;
      int used_up = 0;
      /* subtract one, because it is the following line that
	 gets the specified number */

      int l = TREE_INT_CST_LOW (yylval.ttype) - 1;

      /* Is this the last nonwhite stuff on the line?  */
      c = getc (finput);
      while (c == ' ' || c == '\t')
	c = getc (finput);
      if (c == '\n')
	{
	  /* No more: store the line number and check following line.  */
	  lineno = l;
	  return c;
	}
      ungetc (c, finput);

      /* More follows: it must be a string constant (filename).  */

      /* Read the string constant, but don't treat \ as special.  */
      ignore_escape_flag = 1;

      /* The #line directives are generated by cpp. */
      is_pascal_source = FALSE;

      token = yylex ();

      is_pascal_source = TRUE;
      ignore_escape_flag = 0;

      if (token != STRING_LITERAL || TREE_CODE (yylval.ttype) != STRING_CST)
	{
	  error ("invalid #line");
	  goto skipline;
	}

      input_filename
	= (char *) permalloc (TREE_STRING_LENGTH (yylval.ttype) + 1);
      strcpy (input_filename, TREE_STRING_POINTER (yylval.ttype));
      lineno = l;

      /* Each change of file name
	 reinitializes whether we are now in a system header.  */
      in_system_header = 0;

      if (main_input_filename == 0)
	main_input_filename = input_filename;

      /* Is this the last nonwhite stuff on the line?  */
      c = getc (finput);
      while (c == ' ' || c == '\t')
	c = getc (finput);
      if (c == '\n')
	{
	  /* Update the name in the top element of input_file_stack.  */
	  if (input_file_stack)
	    input_file_stack->name = input_filename;

	  return c;
	}
      ungetc (c, finput);

      token = yylex ();
      used_up = 0;

      /* `1' after file name means entering new file.
	 `2' after file name means just left a file.  */

      if (token == UNSIGNED_INTEGER
	  && TREE_CODE (yylval.ttype) == INTEGER_CST)
	{
	  if (TREE_INT_CST_LOW (yylval.ttype) == 1)
	    {
	      /* Pushing to a new file.  */
	      struct file_stack *p
		= (struct file_stack *) xmalloc (sizeof (struct file_stack));
	      input_file_stack->line = old_lineno;
	      p->next = input_file_stack;
	      p->name = input_filename;
	      input_file_stack = p;
	      input_file_stack_tick++;
#ifdef DWARF_DEBUGGING_INFO
	      if (debug_info_level == DINFO_LEVEL_VERBOSE
		  && write_symbols == DWARF_DEBUG)
		dwarfout_start_new_source_file (input_filename);
#endif /* DWARF_DEBUGGING_INFO */

	      used_up = 1;
	    }
	  else if (TREE_INT_CST_LOW (yylval.ttype) == 2)
	    {
	      /* Popping out of a file.  */
	      if (input_file_stack->next)
		{
		  struct file_stack *p = input_file_stack;
		  input_file_stack = p->next;
		  free (p);
		  input_file_stack_tick++;
#ifdef DWARF_DEBUGGING_INFO
		  if (debug_info_level == DINFO_LEVEL_VERBOSE
		      && write_symbols == DWARF_DEBUG)
		    dwarfout_resume_previous_source_file (input_file_stack->line);
#endif /* DWARF_DEBUGGING_INFO */
		}
	      else
		error ("#-lines for entering and leaving files don't match");

	      used_up = 1;
	    }
	}

      /* Now that we've pushed or popped the input stack,
	 update the name in the top element.  */
      if (input_file_stack)
	input_file_stack->name = input_filename;

      /* If we have handled a `1' or a `2',
	 see if there is another number to read.  */
      if (used_up)
	{
	  /* Is this the last nonwhite stuff on the line?  */
	  c = getc (finput);
	  while (c == ' ' || c == '\t')
	    c = getc (finput);
	  if (c == '\n')
	    return c;
	  ungetc (c, finput);

	  token = yylex ();
	  used_up = 0;
	}

      /* `3' after file name means this is a system header file.  */

      if (token == UNSIGNED_INTEGER
	  && TREE_CODE (yylval.ttype) == INTEGER_CST
	  && TREE_INT_CST_LOW (yylval.ttype) == 3)
	in_system_header = 1;
    }
  else
    error ("invalid #-line");

  /* skip the rest of this line.  */
 skipline:
  if (c == '\n')
    return c;
  while ((c = getc (finput)) != EOF && c != '\n');
  return c;
}

#define ENDFILE -1  /* token that represents end-of-file */

/* Read an escape sequence, returning its equivalent as a character,
   or store 1 in *ignore_ptr if it is backslash-newline.  */

static int
readescape (ignore_ptr)
     int *ignore_ptr;
{
  register int c = getc (finput);
  register int code;
  register unsigned count;
  unsigned firstdig = 0;
  int nonnull;

  switch (c)
    {
    case 'x':
      if (warn_traditional)
	warning ("the meaning of `\\x' varies with -traditional");

      if (flag_traditional)
	return c;

      code = 0;
      count = 0;
      nonnull = 0;
      while (1)
	{
	  c = getc (finput);
	  if (!(c >= 'a' && c <= 'f')
	      && !(c >= 'A' && c <= 'F')
	      && !(c >= '0' && c <= '9'))
	    {
	      ungetc (c, finput);
	      break;
	    }
	  code *= 16;
	  if (c >= 'a' && c <= 'f')
	    code += c - 'a' + 10;
	  if (c >= 'A' && c <= 'F')
	    code += c - 'A' + 10;
	  if (c >= '0' && c <= '9')
	    code += c - '0';
	  if (code != 0 || count != 0)
	    {
	      if (count == 0)
		firstdig = code;
	      count++;
	    }
	  nonnull = 1;
	}
      if (! nonnull)
	error ("\\x used with no following hex digits");
      else if (count == 0)
	/* Digits are all 0's.  Ok.  */
	;
      else if ((count - 1) * 4 >= TYPE_PRECISION (integer_type_node)
	       || (count > 1
		   && ((1 << (TYPE_PRECISION (integer_type_node) - (count - 1) * 4))
		       <= firstdig)))
	pedwarn ("hex escape out of range");
      return code;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':
      code = 0;
      count = 0;
      while ((c <= '7') && (c >= '0') && (count++ < 3))
	{
	  code = (code * 8) + (c - '0');
	  c = getc (finput);
	}
      ungetc (c, finput);
      return code;

    case '\\': case '\'': case '"':
      return c;

    case '\n':
      lineno++;
      *ignore_ptr = 1;
      return 0;

    case 'n':
      return TARGET_NEWLINE;

    case 't':
      return TARGET_TAB;

    case 'r':
      return TARGET_CR;

    case 'f':
      return TARGET_FF;

    case 'b':
      return TARGET_BS;

    case 'a':
      if (warn_traditional)
	warning ("the meaning of `\\a' varies with -traditional");

      if (flag_traditional)
	return c;
      return TARGET_BELL;

    case 'v':
#if 0 /* Vertical tab is present in common usage compilers.  */
      if (flag_traditional)
	return c;
#endif
      return TARGET_VT;

    case 'e':
    case 'E':
      pedwarn ("non-ANSI-standard escape sequence, `\\%c'", c);
      return 033;

    case '?':
      return c;

      /* `\(', etc, are used at beginning of line to avoid confusing Emacs.  */
    case '(':
    case '{':
    case '[':
      /* `\%' is used to prevent SCCS from getting confused.  */
    case '%':
      if (pedantic)
	pedwarn ("non-ANSI escape sequence `\\%c'", c);
      return c;
    }
  if (c >= 040 && c < 0177)
    pedwarn ("unknown escape sequence `\\%c'", c);
  else
    pedwarn ("unknown escape sequence: `\\' followed by char code 0x%x", c);
  return c;
}

void
yyerror (string)
	char *string;
{
  char buf[200];

  strcpy (buf, string);

  /* We can't print string and character constants well
     because the token_buffer contains the result of processing escapes.  */
  if (end_of_file)
    strcat (buf, " at end of input");
  else if (token_buffer[0] == 0)
    strcat (buf, " at null character");
  else if (token_buffer[0] == '"')
    strcat (buf, " before string constant");
  else if (token_buffer[0] == '\'')
    strcat (buf, " before character constant");
  else if (token_buffer[0] < 040 || (unsigned char) token_buffer[0] >= 0177)
    sprintf (buf + strlen (buf), " before character 0%o",
	     (unsigned char) token_buffer[0]);
  else
    strcat (buf, " before `%s'");

  error (buf, token_buffer);
}

/* if we scan tokens:
     INTEGER..      : we must return INTEGER now, and TWODOTS maybe next time.
     INTEGER.DIGIT  : we have a floating point number
     INTEGER.others : we have an error

   The next thing is used to avoid two ungetc() calls (only one is guaranteed
   to work) and return the token TWODOTS next time yylex() is called.
 */
static int have_DOTS = 0;

static int nextchar = -1;

/* Skips all of the white space at the current location in the input file.
   Must use and reset nextchar if it has the next character.  */

void
position_after_white_space ()
{
  register int c;

  if (nextchar != -1)
    c = nextchar, nextchar = -1;
  else
    c = getc (finput);

  ungetc (skip_white_space (c), finput);
}

static char *exp_store_loc;

static void
store_exp (what)
     int what;
{
  if (what > 9)
    store_exp (what/10);

  *exp_store_loc++ = (what%10)+'0';
}
     
/* Pack the valid floating point number starting from start
   by skipping the leading fractional zeroes if the mantissa
   is zero. Return the adjust value.
 */
static int
compress_float (start, end)
     char *start;
     char **end;
{
  int adjust  = 0;
  char *first = start;

  while (*first == '0')
    first++;
  if (*first == '.')
    adjust = -1;
  else
    return 0;	/* Nonzero mantissa */

  first++;	/* Skip '.' */

  while (*first == '0')
    {
      first++;
      adjust--;
    }
  
  if (! *first)	/* A zero */
    return 0;

  *start++ = *first++;
  *start++ = '.';

  if (! *first)
    *start++ = '0';
  else
    while (*start++ = *first++);

  *--start = '\000';
  *end = start;
  
  return adjust;
}

/* if the declaration found has its PASCAL_REDEFINABLE_DECL attribute
   bit set, it has the "real" type of the node in it's TREE_TYPE field
   (if it is a predefined type name, it has TYPE_DECL node there;
   others have integer_type_node (to prevent crashes in gpc-decl.c)
   and they have the actual value to return in DECL_INITIAL (decl)

   But this only returns the TREE_TYPE field or NULL_TREE if it is not
   a type.
 */
tree
check_if_predefined_type (id)
     tree id;
{
  tree pdef_type = lookup_name (id);
  
  if (pdef_type != 0 
      && TREE_CODE (pdef_type) == VAR_DECL
      && PASCAL_REDEFINABLE_DECL (pdef_type))
    if (TREE_TYPE (pdef_type) != NULL_TREE          /* type specified and */
	&& DECL_INITIAL (pdef_type) == NULL_TREE)   /* not a constant */
      pdef_type = TREE_TYPE (pdef_type);
    else
      pdef_type = NULL_TREE;

  return pdef_type;
}

char *
which_language (id)
     int id;
{
  switch (id) {
  case PASCAL_ISO:     return "ISO-7185 Standard Pascal";
  case PASCAL_EXTEND:  return "ISO-10206 Extended Pascal";
  case PASCAL_OBJECT:  return "Object Extensions to ISO-10206 Extended Pascal";
  case PASCAL_BORLAND: return "Borland Pascal";
  case PASCAL_GNU:     return "Gnu Extensions to ISO-10206 Extended Pascal";
  default:	       return "Unknown reserved id level in which_language()";
  }
}

/* Enable/disable keywords.  Using this mechanism we can avoid a lot of
 * shift/reduce and reduce/reduce conflicts:  Keywords will simply fall
 * through and become a new_identifier if not applied in a certain context.
 */
void
enable_keyword (kw)
     char *kw;
{
  register struct resword *resword_ptr;
  if (yydebug)
    fprintf (stderr, "Keyword \"%s\" enabled\n", kw);
  resword_ptr = is_reserved_word (kw, strlen (kw));
  if (! resword_ptr)
    abort ();
  resword_ptr->disabled = 0;
}

void
disable_keyword (kw)
     char *kw;
{
  register struct resword *resword_ptr;
  if (yydebug)
    fprintf (stderr, "Keyword \"%s\" disabled\n", kw);
  resword_ptr = is_reserved_word (kw, strlen (kw));
  if (! resword_ptr)
    abort ();
  resword_ptr->disabled = 1;
}

/* Set to -1 if you want a report of all underscored without -pedantic */
static int note_underscore = ~1;

int
yylex ()
{
  register int c;
  register char *p;
  register int value;
  int wide_flag = 0;
  int extended_resword;
  int keep_silent = FALSE;
  struct resword *resword_ptr;

  int check_known_ids = recognize_known_ids;
  int recognize_known_ids = 1;
  int underscore_rep = 0;

  if (nextchar >= 0)
    c = nextchar, nextchar = -1;
  else
    c = getc (finput);

  /* Effectively do c = skip_white_space (c)
     but do it faster in the usual cases.  */
  while (1)
    switch (c)
      {
      case ' ':
      case '\t':
      case '\f':
      case '\v':
      case '\r':
      case '\b':
	c = getc (finput);
	break;

      case '\n':
      case '(':
      case '{':
      case '\\':
	c = skip_white_space (c);
      default:
	goto found_nonwhite;
      }
 found_nonwhite:

  token_buffer[0] = c;
  token_buffer[1] = 0;

  switch (c)
    {
    case EOF:
      end_of_file = 1;
      token_buffer[0] = 0;
      value = ENDFILE;
      break;

    case '$':
      {
        if (dollars_in_ident)
          goto letter;
        else
          {
            int ch = getc (finput);
            ungetc (ch, finput);
            switch (ch)
              { 
                case '0':  case '1':  case '2':  case '3':  case '4':
                case '5':  case '6':  case '7':  case '8':  case '9':
                case 'A':  case 'B':  case 'C':  case 'D':  case 'E':  case 'F':
                case 'a':  case 'b':  case 'c':  case 'd':  case 'e':  case 'f':
                  goto digit;
                default:
                  return '$';
              }
          }
      }

#ifdef notyet
      /* Not supported in string_constant yet */
    case 'L':
      /* Capital L may start a wide-string or wide-character constant.  */
      {
	int ch = getc (finput);
	if (ch == '\'')
	  {
	    wide_flag = 1;
	    c = ch;
	    goto string_constant;
	  }
	if (ch == '"')
	  {
	    wide_flag = 1;
	    c = ch;
	    if (pedantic)
	      warning ("ISO Pascal does not allow double quoted strings");
	    goto string_constant;
	  }
	ungetc (ch, finput);
      }
      goto letter;
#endif /* notyet */

    case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
    case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
    case 'K':  case 'L':  case 'M':  case 'N':  case 'O':
    case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
    case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
    case 'Z':
    case 'a':  case 'b':  case 'c':  case 'd':  case 'e':
    case 'f':  case 'g':  case 'h':  case 'i':  case 'j':
    case 'k':  case 'l':  case 'm':  case 'n':  case 'o':
    case 'p':  case 'q':  case 'r':  case 's':  case 't':
    case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
    case 'z':
    case '_':
    letter:
      {
	char prev = '\000';
	p = token_buffer;
	while (isalnum (c) || c == '_' || c == '$')
	  {
	    if (c == '_')
	      {
		if (pedantic && (note_underscore & 1))
		  underscore_rep |= 1;

		if (! prev && (note_underscore & 2))
		  underscore_rep |= 2;

		if (prev == '_' && (note_underscore & 4))
		  underscore_rep |= 4;
	      }
	    if (p >= token_buffer + maxtoken)
	      p = extend_token_buffer (p);

	    if (p == token_buffer)
	      {
		if (islower (c))
		  c = toupper (c); /* all words start with a CAPITAL letter */
	      }
	    else if (isupper (c))
	      c = tolower(c);	   /* Leave only the first letter upper case */

	    *p++ = prev = c;
	    c = getc (finput);
	  }
	if (prev == '_' && (note_underscore & 8))
	  underscore_rep |= 8;
      }
      *p = 0;
      nextchar = c;
      
      value = IDENTIFIER;
      yylval.itype = 0;
      
      /* Recognize reserved words */
      
      extended_resword = 0;
      resword_ptr = is_reserved_word (token_buffer, p - token_buffer);
      if (resword_ptr
	  && resword_ptr->disabled == 0)
	{
	  if (resword_ptr->iclass <= flag_what_pascal)
	      value = (int) resword_ptr->token;
	  else
	    {
	      /* Don't warn about Extended pascal keywords when compiling
		 a module */
	      keep_silent = !main_program_name;

	      extended_resword = (int) resword_ptr->token;
	    }
	}

      /* If we did not find a keyword, look for an identifier declaration */
      
      if (value == IDENTIFIER)
	{
          yylval.ttype = get_identifier (token_buffer);
	  lastiddecl   = lookup_name (yylval.ttype);
	  last_id_value  = value;
	  
	  /* if the declaration found has its PASCAL_REDEFINABLE_DECL attribute
	     bit set, it has the "real" type of the node in it's TREE_TYPE field
	     (if it is a predefined type name, it has TYPE_DECL node there;
	     others have integer_type_node (to prevent crashes in gpc-decl.c)
	     and they have the actual value to return in
	     DECL_INITIAL (decl) */

	  /* Report reserved words (in some other language level)
	   *  that are used as identifiers if pedantic option is given.
	   */
	  if (pedantic
	      && extended_resword
	      && !keep_silent
	      && !resword_ptr->informed)
	    {
	      resword_ptr->informed = 1;
	      warning ("`%s' is a reserved word in %s",
		       IDENTIFIER_POINTER (yylval.ttype),
		       which_language (resword_ptr->iclass));
	    }

	  if (check_known_ids)
	    {
	      /* @@@  HP Snake fails pvs 046 if I don't check
		 that it actually is a VAR_DECL. So something sets
		 the redef_decl bit when it should not be set???
		 (for a component_ref at line 22 of 046 -> signal 11)
	       */
	      if (lastiddecl != 0 
		  && TREE_CODE (lastiddecl) == VAR_DECL
		  && PASCAL_REDEFINABLE_DECL (lastiddecl))
		if (TREE_TYPE (lastiddecl) != NULL_TREE && /* type specified &*/
		    DECL_INITIAL (lastiddecl) == NULL_TREE)   /* not a constant */
		  lastiddecl = TREE_TYPE (lastiddecl);
		else
		  value = TREE_INT_CST_LOW (DECL_INITIAL (lastiddecl));
	    }
	    
	  /* @@@@@@ Check what effect this has for redeclaring extensions!!! */
	  if (lastiddecl == 0
	      && value == IDENTIFIER
	      && extended_resword)
	    value = extended_resword;

	  /* Remember what is returned, parser needs it because of the
	   * magic done with redeclaring words
	   */
	  last_id_value = value;

	  if (underscore_rep && value == IDENTIFIER && lastiddecl == NULL_TREE)
	    {
	      if (underscore_rep & 1)
		warning ("ISO Pascal does not allow underscores in identifiers");
	      if (underscore_rep & 2)
		warning ("Identifiers should not start with an underscore");
	      if (underscore_rep & 4)
		warning ("Identifiers should not contain two adjacent underscores");
	      if (underscore_rep & 8)
		warning ("Identifiers should not end with an underscore");
	      
	      /* Give only one each/compilation */
	      note_underscore &= ~underscore_rep;
	    }
	}

      /*
       * Special guest: PROTECTED.  
       * It is no keyword when a ',' or ':' follows.
       * With this hack we can handle ISO 7185 as well as ISO 10206.
       */
      if (value == PROTECTED)
        {
          nextchar = skip_white_space (nextchar);
          if (nextchar == ',' || nextchar == ':')
            value = IDENTIFIER;
        }
      
      break;

    case '(':
      c = getc (finput);
      if (c == '.') {
	  c = getc (finput);
	  ungetc (c, finput);
	  if (c == '.') {	/* Allow all params to be unknown (...) */
	      have_DOTS = 1;
	      value = '(';
	  } else {
	      token_buffer[1] = '.';
	      token_buffer[2] = 0;
	      value = LBRACKET;
	  }
      } else {
	  ungetc (c, finput);
	  value = '(';
      }
      break;

    case '.':
      if (have_DOTS == 0)
	  c = getc (finput);
      else
	  have_DOTS = 0;
      switch (c) {
      case '.':
	  token_buffer[1] = '.';
	  c = getc (finput);
	  if (c == '.') {
	      token_buffer[2] = '.';
	      token_buffer[3] = 0;
	      value = ELLIPSIS;
	  } else {
	      ungetc (c, finput);
	      token_buffer[2] = 0;
	      value = TWODOTS;
	  }
	  break;
      case ')':
	  value = RBRACKET;
	  token_buffer[1] = c;
	  token_buffer[2] = 0;
	  break;
      default:
	  ungetc (c, finput);
	  value = '.';
	  break;
      }
      break;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
    digit:
      {
	int baseflag = 0;
	int base = 10;
	int count = 0;
	int largest_digit = 0;
	int numdigits = 0;
	/* for multi-precision arithmetic,
	   we store only 8 live bits in each short,
	   giving us 64 bits of reliable precision */
	short shorts[8];
	int floatflag = 0;  /* Set 1 if we learn this is a floating constant */

	for (count = 0; count < 8; count++)
	  shorts[count] = 0;

        if (c == '$')
          {
            baseflag = 1;
            base = 16;
            c = getc (finput);
            if (pedantic)
              warning ("ISO Pascal does not define `$hex'");
          }

	p = token_buffer;
	*p++ = c;

	if (c == '0' && flag_c_numbers) /* allow c style 0octal, 0xhex etc */
	  {
	    *p++ = (c = getc (finput));
	    if ((c == 'x') || (c == 'X'))
	      {
		base = 16;
		*p++ = (c = getc (finput));
	      }
	    else
	      {
		base = 8;
		numdigits++;
	      }
	  }

	/* Read all the digits.  */

	while (isalnum (c) || (c == '.' && floatflag == 0) ||
	       (c == '#' && baseflag++ == 0)) {
	    if (isdigit (c)) {
		c = c - '0';
	    } else if (c == '.') {
		c = getc (finput);
		if (c == '.') {
		    have_DOTS = 1;
		    ungetc (c, finput); /* only peek at character after . */
		    break;	/* INTEGER.. is not floating point */
		} else {
		    if (base != 10)
			warning("floating constants must be in radix 10");
		    base = 10;
		    if (! isdigit(c)) {
		      if (c == ')')		/* '.)' == ']' */
			{
			  nextchar = '.'; /* we just missed one */
			  break;	  /* Do ungetc() later  */
			}
		      ungetc (c, finput);
		      error("ISO Pascal requires a digit after decimal point");
		      c = '0'; /* assume zero was present */
		    }
		    *p++ = c;
		    c -= '0';
		    floatflag = 1;
		}
	    } else if (c == '#') {
		if (pedantic)
		    warning ("ISO Pascal does not define `radix#value'");
		/* Using Extended Pascals way to input values in different
		 * bases:
		 *     base#value
		 * Base may be in range 2..36
		 */
		base = (shorts[3]<<24) + (shorts[2]<<16) + (shorts[1]<<8) + shorts[0];
		if ((base < 2 || base > 36) ||
		    shorts[7] | shorts[6] | shorts[5] | shorts[4]) {
		    warning ("base value out of range; assuming base 10");
		    base = 10;
		}
	        for (count = 0; count < 8; count++)
		    shorts[count] = 0;
		largest_digit = 0;
		numdigits = 0;
		*p++ = (c = getc (finput));
		continue;
	    } else if (base <= 10) {
		if (base == 10 && (c&~040) == 'E') {
		    floatflag = 1;
		    break;   /* start of exponent */
		}
		error ("nondigits in number whose radix <= 10");
		c = 0;
	    } else if (c >= 'a') {
		c = c - 'a' + 10;
	    } else {
		c = c - 'A' + 10;
	    }
	    if (c >= largest_digit)
		largest_digit = c;
	    numdigits++;
	    
	    if (floatflag == 0)
		for (count = 0; count < 8; count++) {
		    (shorts[count] *= base);
		    if (count) {
			shorts[count] += (shorts[count-1] >> 8);
			shorts[count-1] &= (1<<8)-1;
		    } else shorts[0] += c;
		}
    
	    if (p >= token_buffer + maxtoken - 3)
		p = extend_token_buffer (p);
	    *p++ = (c = getc (finput));
	}

	if (c == '.' && have_DOTS == 0)
	    error("Only one decimal point in floating point number allowed");

	if (numdigits == 0)
	  error ("numeric constant with no digits");

	if (largest_digit >= base)
	  error ("numeric constant contains digits beyond the radix");

	/* Remove terminating char from the token buffer 
	   and delimit the string */
	*--p = 0;

	if (floatflag)
	  {
	    tree type = double_type_node;
	    char f_seen = 0;
	    char l_seen = 0;
	    int esign   = 1;
	    int expon   = 0;
	    char *temp  = p;

	    /* Compress out the leading zeros by adjusting the exponent */
	    int adjust_exp = compress_float (token_buffer, &temp);

	    p = temp;

	    /* Read explicit exponent if any, and put it in tokenbuf.  */

	    if ((c == 'e') || (c == 'E'))
	      {
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = getc (finput);
		if ((c == '+') || (c == '-'))
		  {
		    *p++ = c;
		    if (c == '-')
		      esign = -1;
		    c = getc (finput);
		  }
		if (! isdigit (c))
		  error ("floating constant exponent has no digits");
	        while (isdigit (c))
		  {
		    expon = 10*expon+c-'0';
		    c = getc (finput);
		  }
		expon = esign*expon+adjust_exp;
		expon *= esign;
		exp_store_loc = p;
		store_exp (expon);
		p = exp_store_loc;
	      }
	    else if (adjust_exp)
	      {
		*p++ = 'E';
		if (adjust_exp < 0)
		  {
		    *p++ = '-';
		    adjust_exp = -adjust_exp;
		  }
		exp_store_loc = p;
		store_exp (adjust_exp);
		p = exp_store_loc;
	      }

	    *p = 0;
	    yylval.ttype = build_real (type,
				       REAL_VALUE_ATOF (token_buffer, DFmode));

	    if (isalnum (c)) {
		error ("garbage at end of number");
		while (isalnum (c)) {
		    if (p >= token_buffer + maxtoken - 3)
			p = extend_token_buffer (p);
		    *p++ = c;
		    c = getc (finput);
		}
	    }

	    ungetc (c, finput);
	    *p = 0;

	    TREE_TYPE (yylval.ttype) = type;
	    value = UNSIGNED_REAL;
	  }
	else
	  {
	    tree type;

	    if (have_DOTS == 0) { /* already did ungetc for have_DOTS */
		if (isalnum (c)) {
		    error ("garbage at end of number");
		    while (isalnum (c)) {
			if (p >= token_buffer + maxtoken - 3)
			    p = extend_token_buffer (p);
			*p++ = c;
			c = getc (finput);
		    }	
		}
		ungetc (c, finput);
	    }
	    *p = 0;

	    /* This is simplified by the fact that our constant
	       is always positive.  */
	    yylval.ttype
	      = build_int_2 ((shorts[3]<<24) + (shorts[2]<<16) + (shorts[1]<<8) + shorts[0],
			     (shorts[7]<<24) + (shorts[6]<<16) + (shorts[5]<<8) + shorts[4]);
    
	    if (int_fits_type_p (yylval.ttype, integer_type_node))
	      type = integer_type_node;
	    else if (int_fits_type_p (yylval.ttype, unsigned_type_node))
              type = unsigned_type_node;
            else if (int_fits_type_p (yylval.ttype, long_long_integer_type_node))
              type = long_long_integer_type_node;
            else
              {
                /* should be effectively unreachable */
	        error ("integer constant does not fit in integer type");
	        type = integer_type_node;
	      }
	    TREE_TYPE (yylval.ttype) = type;
	    value = UNSIGNED_INTEGER;
	  }
	break;
      }

    case '"' :
	if (pedantic && is_pascal_source)
	   warning ("ISO Pascal does not allow double quoted strings");
	/* FALLTHROUGH */
    case '\'':
    string_constant:
      {
	char quote_char = c;
	c = getc (finput);    /* first char */
	p = token_buffer + 1;
	if (quote_char == '"')
	    value = STRING_LITERAL;
	else
	    value = CHAR_LITERAL; /* Let's all WIRTH together 3 times! */

	if (pedantic && wide_flag)
	  warning ("ISO Pascal does not allow wide %s",
		   (value == STRING_LITERAL) ? "strings" : "chars");

	while (1) {
	    int num;
	    while(c != quote_char && c != EOF) {
		if (!ignore_escape_flag && flag_c_escapes && c == '\\') {
		    int ignore = 0;
		    c = readescape (&ignore);
		    if (ignore)
			goto skipnewline;
		} else if (c == '\n') {
		    if (pedantic)
			if (value == CHAR_LITERAL) /* not accurate :-) */
			    warning ("ISO Pascal forbids newline in char constant");
		        else
			    warning ("string not closed before end of line");
		    lineno++;
		}
		if (p == token_buffer + maxtoken)
		    p = extend_token_buffer (p);
		*p++ = c;
	    skipnewline:
		c = getc (finput);
	    }
	    if (c == EOF)
	      {
		error ("String not terminated before end of file");
		break;
	      }
	    /* Do we have a closing quote? */
	    num = 0;
	    do {
		if (p == token_buffer + maxtoken)
		    p = extend_token_buffer (p);
		if (num & 1)
		    *p++ = c;
		num++;
		c = getc (finput);
	    } while (c == quote_char);
	    *p = 0;

	    if (p - token_buffer > 2)
		value = STRING_LITERAL;

	    if (num & 1)
	      { /* string/char constant has terminated */
		if (c != EOF)
		  ungetc (c, finput);
		break; /* while (1) */
	      }

	    /* String did not terminate, continue reading it */
	}

	if (p - token_buffer == 1)
	  if (value == CHAR_LITERAL || pedantic)
	    {
	      if (pedantic)
		warning ("Empty string literal%s", (value == CHAR_LITERAL) ? " in ''" : "");
	      value = STRING_LITERAL;
	    }

	if (value == CHAR_LITERAL) {
	    char code = token_buffer[1];
	    /* If char type is signed, sign-extend the constant.  */
	    if (TREE_UNSIGNED (char_type_node)
		|| ((code >> (BITS_PER_UNIT - 1)) & 1) == 0)
	       yylval.ttype = build_int_2(code & ((1 << BITS_PER_UNIT) - 1), 0);
	    else
	       yylval.ttype = build_int_2(code | ((-1) << BITS_PER_UNIT), -1);

	    TREE_TYPE (yylval.ttype) = char_type_node;
        } else { /* string constant */
	    yylval.ttype = build_string (p - token_buffer, token_buffer + 1);
	    TREE_TYPE (yylval.ttype) = char_array_type_node;
	}
	break;
      }      
      
    case '&':
      {
	char c1; 
	token_buffer[ 1 ] = c1 = getc (finput);
        if (c1 == '&')
	  {
	    value = ANDAND; 		/* To take address of a label */
	    token_buffer[ 2 ] = '\000';
	  }
	else
	  {
	    value = '&';
	    ungetc (c1, finput);
	    token_buffer[ 1 ] = '\000';
	  }
      }

    case '+':
    case '-':
    case '<':
    case '>':
    case '*':
    case '/':
    case '=':
    case ':':
      {
	register int c1;

	token_buffer[1] = c1 = getc (finput);
	token_buffer[2] = 0;

	if (c1 == '=') {
	    switch (c) {
	    case ':': value = ASSIGN; goto done;
	    case '<': value = LTE; yylval.code = LE_EXPR; goto done;
	    case '>': value = GTE; yylval.code = GE_EXPR; goto done;
	    }
	} else if (c == '<' && c1 == '>') {
	    value = NEQ; yylval.code = NE_EXPR; goto done;
	} else if (c == '>' && c1 == '<') {
	    value = SYMMETRIC_DIFF; goto done;
	} else if (c == '=' && c1 == '>') {
	    value = RENAME; goto done;
	} else if (c == '*' && c1 == '*') {
	    value = EXPON; goto done;
	} else if (c1 == '>') {
	    switch (c) {
	    case '+': value = CEILPLUS; goto done;
	    case '-': value = CEILMINUS; goto done;
	    case '*': value = CEILMULT; goto done;
	    case '/': value = CEILRDIV; goto done;
            }
	} else if (c1 == '<') {
	    switch (c) {
	    case '+': value = FLOORPLUS; goto done;
	    case '-': value = FLOORMINUS; goto done;
	    case '*': value = FLOORMULT; goto done;
	    case '/': value = FLOORRDIV; goto done;
            }
	}
	ungetc (c1, finput);
	token_buffer[1] = 0;

	switch (c) {
	  case '=':
	    yylval.code = EQ_EXPR; break;
	  case '+':
	    yylval.code = PLUS_EXPR; break;
	  case '-':
	    yylval.code = MINUS_EXPR; break;
	  case '*':
	    yylval.code = MULT_EXPR; break;
	  case '/':
	    yylval.code = RDIV_EXPR; break;
	  case '<':
	    yylval.code = LT_EXPR; break;
	  case '>':
	    yylval.code = GT_EXPR; break;
	}	
	value = c;
	goto done;
      }

    default:
      value = c;
    }

done:
  return value;
}
