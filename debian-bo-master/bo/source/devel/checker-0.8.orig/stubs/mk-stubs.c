/* Make stubs from a .aux file.
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
/* TODO:
 * o handle pointers to a function
 * o add an include when a structure/union/enum is used
 * o recognize standards typedef
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

extern char *optarg;
extern int optind;

#define BUFLEN 2048
#define MAXARGS 20

#define CHKR_PREFIX "chkr$"

/* Flags for arguments */
#define FL_DOTS		1	/* `...' */
#define FL_POINTER	2
#define FL_CONST	4

/* Action */
#define ACTION_EMIT_STUBS 0
#define ACTION_EMIT_DEFINE 1
#define ACTION_EMIT_TESTS 2
#define ACTION_EMIT_FILES 3

char *st_args[MAXARGS];
char *progname;

/* If not null, emit aliases when possible.  */
int alias_flag;

/* If not null, enclose stubs between ifdef/endif HAVE_funcname.  */
int ifdef_flag;

/* Action to do.  See ACTION_EMIT_*.  */
int action_flag;

/* If set, do not emit stubs for static declarations.  */
int static_flag;

/* This structure describe an argument.  */
struct arg
{
  char *type;		/* the type (egs: int; const char *) */
  char *reduced_type;	/* reduced type: without volatile, const */
  char *name;		/* the name (eg: 'a' for 'int a') */
  char *orig;		/* the contents in the .X line */
  int flags;
};

#define SKIP_BLANKS(c)		\
do				\
{				\
  while (isspace(*c))		\
    c++;			\
}				\
while(0)

#define REWIND_BLANKS(c)	\
do				\
{				\
  while (isspace(*c))		\
    c--;			\
}				\
while(0)

#define IS_C_CHAR(c) (isalnum(c) || (c) == '_' || (c) == '$')

/* Find the left parenthesis.  Levels are carefuly handled.  */
char *
find_left_paren (char *c)
{
 int level = 0;
 while (*c)
   {
     if (*c == '(' && level == 0)
       return c;
     if (*c == ')')
       level++;
     else if (*c == '(')
       level--;
     c--;
   }
 return NULL;
}

/* Find the next `,'.  Parenthesises and braces are carefuly handled.  */
char *
find_next_comma (char *c)
{
 int level = 0;
 while (*c)
   {
     if (*c == ',' && level == 0)
       return c;
     if (*c == '(' || *c == '{')
       level++;
     else if (*c == ')' || *c == '}')
       level--;
     c++;
   }
 return NULL;
}

/* Initialize the table of default argument names.  */
void
init_st_args (void)
{
  char proto[] = "arg123";
  int i;
  for (i = 0; i < MAXARGS; i++)
    {
      sprintf (proto, "arg%d", i);
      st_args[i] = strdup(proto);
    }
}

/* Remove the ending `*' of IN (if it exists).  */
char *
remove_asterik (char *in)
{
  static char buffer[BUFLEN];
  int len;
  
  strcpy (buffer, in);
  len = strlen (buffer);
  if (len && buffer[len - 1] == '*')
    {
      len--;
      buffer[len] = '\0';
      while (len > 0 && isspace (buffer[len - 1]))
        {
          buffer[len - 1] = '\0';
          len--;
        }
    }
  return buffer;
}

/* Back-copy.  */
void
my_memmove (char *dest, char *src, int len)
{
  for (; len > 0; len--)
    *dest++ = *src++;
}

/* Remove `const' and `volatile' from ORI.  */
char *
reduce_decl (char *ori, int *flags)
{
  static char buffer[BUFLEN];
  int len;
  char *b;
  char *e;
  
  strcpy (buffer, ori);
  len = strlen (buffer);
  while ((b = strstr (buffer, "const")))
    {
      my_memmove (b, b + 5, len - (b - buffer));
      *flags |= FL_CONST;
    }
  while ((b = strstr (buffer, "volatile")))
    my_memmove (b, b + 8, len - (b - buffer));
  len = strlen (buffer);
  for (b = buffer, e = buffer; *b; b++)
    if (isspace (*b))
      {
        /* Skip initial blanks.  */
        if (e == buffer)
          continue;
          
        *e++ = ' ';
        while (isspace (*b))
          b++;
        b--;
      }
    else
      *e++ = *b;
  *e = '\0';
  return buffer;
}

/* Normalize the array of arguments.  The result is a global flag.  */
int
fill_args (struct arg *args, int type)
{
  int flags = 0;
  int l;
  int i;
  char *b;
  static char buffer[BUFLEN];
  char *e;
  
  b = buffer;
  
  for (i = 0; args->orig; args++, i++)
    {
      args->flags = 0;
      
      /* Remove the blanks at the end.  */
      l = strlen (args->orig);
      if (l)
        {
          e = args->orig + l - 1;
          if (isspace (*e))
            {
              REWIND_BLANKS (e);
              *e = '\0';
            }
        }
        
      /* Set name.  */
      if (!strcmp (args->orig, "void"))
        {
          args->name = "";	/* no arguments */
          args->reduced_type = "void";
          args->type = "void";
          continue;
        }
      else if (!strcmp (args->orig, "..."))
        {
          args->name = "";
          args->reduced_type = "...";
          args->type = "...";
          args->flags |= FL_DOTS;
          flags |= FL_DOTS;
          continue;
        }
      if (type == 'C')
        {
          /* A prototype doesn't have argument names. */
          args->name = st_args[i];
          args->type = args->orig;
        }
      else
        {
          l = strlen (args->orig);
          e = args->orig + l - 1;
          for (l = 0; IS_C_CHAR (*e); l++)
            e--;
          if (isspace (*e))
            {
              args->name = e + 1;
              e--;
              REWIND_BLANKS (e);
              e++;
            }
          else
            {
              e++;
              strcpy (b, e);
              args->name = b;
              b += l + 1;
            }
          *e = '\0';
          args->type = args->orig;
        }
        
      e = reduce_decl (args->type, &args->flags);
      strcpy (b, e);
      args->reduced_type = b;
      b += strlen (e) + 1;
      
      l = strlen (args->reduced_type);
      if (l && args->reduced_type[l-1] == '*')
        {
          args->flags |= FL_POINTER;
          flags |= FL_POINTER;
        }
    }
  return flags;
}

void
usage (void)
{
  printf("Usage: %s [-a] [-d] [-t action] [-i file] [-o file]\n"
  	 "-a		Uses aliases\n"
  	 "-i input	Set the input file\n"
  	 "-o output	Set the output file\n"
  	 "-d		Enclose stubs in #ifdef/#endif\n"
  	 "-t define	Emit defines list\n"
  	 "-t notdef	Emit warnings file\n"
  	 "-t files	Emit files file\n", progname);
  exit(1);
}

int
main (int argc, char *argv[])
{
  FILE *f_in;
  FILE *f_out;
  char buffer[BUFLEN];
  char func_name[BUFLEN];
  char *file_name;
  int lineno;
  int static_decl;
  char *first_comment;
  char *b;
  char *e;
  char *file;
  int type;
  char *paren;
  char *begin;
  struct arg args[MAXARGS];
  int argn;
  int flags;
  int i;
  
  progname = argv[0];
  alias_flag = 0;
  ifdef_flag = 0;
  action_flag = ACTION_EMIT_STUBS;
  static_flag = 0;
  
  f_in = stdin;
  f_out = stdout;
  
  while ((i = getopt (argc, argv, "i:o:adt:s")) != -1)
    switch(i)
      {
    case 'i':
        if (f_in != stdin)
          {
            fprintf (stderr, "Option `-i' already used\n");
            fclose (f_in);
          }
        f_in = fopen (optarg, "r");
        if (!f_in)
          {
            fprintf (stderr, "Can't open `%s'\n", optarg);
            exit (1);
          }
        break;
    case 'o':
        if (f_out != stdout)
          {
            fprintf (stderr, "Option `-o' already used\n");
            fclose (f_out);
          }
        f_out = fopen (optarg, "w");
        if (!f_out)
          {
            fprintf (stderr, "Can't open `%s'\n", optarg);
            exit (1);
          }
        break;
    case 'a':
        alias_flag = 1;
        break;
    case 'd':
    	ifdef_flag = 1;
    	break;
    case 't':
        if (!strcasecmp ("define", optarg))
          action_flag = ACTION_EMIT_DEFINE;
        else if (!strcasecmp ("notdef", optarg))
    	  action_flag = ACTION_EMIT_TESTS;
    	else if (!strcasecmp ("files", optarg))
    	  action_flag = ACTION_EMIT_FILES;
    	break;
    case 's':
    	static_flag = 1;
    	break;
    case '?':
        usage ();
      }
  
  /* The first line is just a comment.  */
  if (fgets (buffer, BUFLEN, f_in) == NULL)
    exit (0);
  else
    {
      first_comment = strdup (buffer);
      fprintf (f_out, "%s", buffer);
    }
    
  init_st_args ();
  
  lineno = 1;
  while (fgets (buffer, BUFLEN, f_in) != NULL)
    {
      lineno++;
      static_decl = 0;
      if (buffer[0] != '/' || buffer[1] != '*' || buffer[2] != ' ')
        {
          fprintf (stderr, "Hmm, the line %d doesn't begin with a comment\n", lineno);
          continue;
        }
      file = buffer + 3;
      e = strchr (file, ':');
      if (!e)
        {
          fprintf (stderr, "Hmm, the line %d hasn't a file name\n", lineno);
          continue;
        }
      e = strchr(e + 1, ':');
      if (!e)
        {
          fprintf (stderr, "Hmm, the line %d hasn't a line number\n", lineno);
          continue;
        }
      *e = '\0';
      e++;
      if (*e != 'O' && *e != 'N')
        {
          fprintf (stderr, "Hmm, the line %d doesn't have a O or a N\n", lineno);
          continue;
        }
      e++;
      if (*e != 'C' && *e != 'F')
        {
          fprintf (stderr, "Hmm, the line %d doesn't have a C or a F\n", lineno);
          continue;
        }
      type = *e;
      e++;
      if (e[0] != ' ' || e[1] != '*' || e[2] != '/' || e[3] != ' ')
        {
          fprintf (stderr, "Hmm, the line %d doesn't end with a comment\n", lineno);
          continue;
        }
      b = e + 4;
      SKIP_BLANKS (b);
      begin = b;
      e = strchr (b, '\n');
      if (type == 'C')
        {
          if (!e || e[-1] != ';' || e[-2] != ')')
            {
              fprintf (stderr, "Hmm, the line %d doesn't have a ');\\n'\n", lineno);
              continue;
            }
          else
            e -= 2;
        }
      else if (type == 'F')
        {
          if (!e || e[-1] != '/' && e[-2] != '*')
            {
              fprintf (stderr, "Hmm, the line %d doesn't have an ending comment\n", lineno);
              continue;
            }
          e -= 2;
          /* Search the beginning of the comment.  */
          while (e > begin && (e[1] != '*' || e[0] != '/'))
            e--;
          if (e[0] != '/' || e[1] != '*')
            {
              fprintf (stderr, "Hmm, I can't find the beginning of the last comment on line %d", lineno);
              continue;
            }
          if (e[-1] != ' ' || e[-2] != ';' || e[-3] != ')')
            {
              fprintf (stderr, "Hmm, I can't find the end of prototype in line %d", lineno);
              continue;
            }
          e -= 3;
        }
      if (!strncmp (begin, "static", 6))
        {
          if (static_flag)
            {
              fprintf (stderr, "static declaration are not converted (%d)\n", lineno);
              continue;
            }
          static_decl = 1;
        }
      else if (strncmp (begin, "extern", 6))
        {
          fprintf (stderr, "Hmm, the line %d doesn't begin with static or extern\n", lineno);
          continue;
        }
        
      /* skip `extern' or `static'.  */
      begin += 6;
      SKIP_BLANKS (begin);
      
      b = e;
      if (*b != ')')
        abort ();
      
      /* Find the open parenthesis.  */
      for (i = 0; *b == ')'; i++)
        {
          if (i == 1)
            b--;
          paren = find_left_paren (b - 1);
          *paren = '\0';
          *b = '\0';
          b = paren - 1;
          REWIND_BLANKS (b);
          b[1] = '\0';
        }
      
      /* Find the function name.  */
      while (IS_C_CHAR (*b))
        b--;
      b++;
      if (action_flag == ACTION_EMIT_DEFINE)
        {
          fprintf (f_out, "#define HAVE_%s\n", b);
          continue;
        }
      else if (action_flag == ACTION_EMIT_TESTS)
        {
          fprintf (f_out, "#ifndef HAVE_%s\n", b);
          fprintf (f_out, "#warning No definition for \"%s\"\n", b);
          fprintf (f_out, "#endif /* !HAVE_%s*/\n", b);
          continue;
        }
      else if (action_flag == ACTION_EMIT_FILES)
        {
          char *f;
          
          f = strstr (file, "include/");
          if (!f)
            continue;
          f += 8;
          for (e = f; *e && *e != ':'; e++)
            if (*e == '/')
              *e = '_';
          e[-2] = '\0';	/* remove the `.h'  */
          fprintf (f_out, "s-%s-%s.o\n", f, b);
          continue;
        }
      else
        strcpy (func_name, b);
      
      /* Isolate the type of the function.  */
      *b = '\0';
      b--;
      REWIND_BLANKS (b);
      b[1] = '\0';
      
      /* Now each argument is parsed.  */
      argn = 0;
      b = paren;
      do
        {
          /* The `,' is turned into a \0 */
          if (argn)
            *b = '\0';
          b = b + 1;
          SKIP_BLANKS (b);
          args[argn].orig = b;
          argn++;
        }
      while ((b = find_next_comma (b)));
      args[argn].orig = NULL;
      
      /* Normalize the array.  */
      flags = fill_args (args, type);
      
      if (ifdef_flag)
        fprintf (f_out, "#ifdef HAVE_%s\n", func_name);
        
      if (static_decl)
        fprintf (f_out, "/* This was a static declaration.  */\n");
      
      fprintf (f_out, "/* From `%s'.  */\n", file);
      
      if (!(flags & (FL_DOTS | FL_POINTER)) && alias_flag)
        {
          fprintf (f_out, "#ifdef ALIASES\n");
          fprintf (f_out, "/* This function doesn't require a stub, so I use an alias:\n");
          fprintf (f_out, "\t%s %s(", begin, func_name);
          for (i = 0; args[i].orig; i++)
            {
              fprintf (f_out, "%s", args[i].type);
              if (args[i + 1].orig)
                fprintf (f_out, ", ");
            }
          fprintf (f_out, ");\n*/\n");
          fprintf (f_out, "  asm(\".stabs \\\"_%s%s\\\",11,0,0,0\\n\\\"\n", CHKR_PREFIX, func_name);
          fprintf (f_out, "      \".stabs \\\"_%s\\\",1,0,0,0\");\n", func_name);
          fprintf (f_out, "#else /* ALIASES */\n");
        }
      if (1)
        {
          /* Function declaration.  */
          fprintf (f_out, "%s\n%s%s (", begin, CHKR_PREFIX, func_name);
          for(i = 0; args[i].orig; i++)
            {
              fprintf (f_out, "%s %s", args[i].type, args[i].name);
              if (args[i + 1].orig)
                fprintf (f_out, ", ");
            }
          fprintf (f_out, ")\n{\n");
          
          /* Write the checking functions.  */
          if (flags & FL_POINTER)
            {
              fprintf (f_out, "  /* This function requires a stub */\n");
              for (i = 0; args[i].orig; i++)
                if (args[i].flags & FL_POINTER)
                  fprintf (f_out, "  stubs_chkr_check_addr (%s, sizeof (%s), CHKR_%s, \"%s\");\n",
                          args[i].name,
                          remove_asterik (args[i].reduced_type),
                          args[i].flags & FL_CONST ? "RO" : "XX",
                          args[i].name);
            }              
          if (flags & FL_DOTS)
            fprintf (f_out, "  /* This function must be handled by the user */\n");
            
          /* Write the builtin_jump.  */
          fprintf (f_out, "#if USE_BI_JUMP\n  __builtin_jump (%s);\n#else\n", func_name);
          
          /* Write a direct call.  */
          if (strcmp (begin, "void"))
            fprintf (f_out, "  return ");
          else
            fprintf (f_out, "  ");
          fprintf (f_out, "%s (", func_name);
          for (i = 0; args[i].orig; i++)
            {
              fprintf (f_out, "%s", args[i].name);
              if (args[i + 1].orig)
                fprintf (f_out, ", ");
            }
          fprintf (f_out, ");\n");
          
          /* Write an indirect call.  */
          if (strcmp (begin, "void"))
            fprintf (f_out, "  {\n    %s res;\n    res = %s (", begin, func_name);
          else
            fprintf (f_out, "  %s (", func_name);
          for (i = 0; args[i].orig; i++)
            {
              fprintf (f_out, "%s", args[i].name);
              if (args[i + 1].orig)
                fprintf (f_out, ", ");
            }
          fprintf (f_out, ");\n");
          if (strcmp (begin, "void"))
            fprintf (f_out, "    return res;\n  }\n");
          fprintf (f_out, "#endif /* !USE_BI_JUMP */\n}\n");
        }
        
      if (!(flags & (FL_DOTS | FL_POINTER)) && alias_flag)
        fprintf (f_out, "#endif /* !ALIASES */\n");
        
      if (ifdef_flag)
        fprintf (f_out, "#endif /* HAVE_%s */\n", func_name);
        
      fputc ('\n', f_out);
    }
    
 if (f_in != stdin)
   fclose (f_in);
 if (f_out != stdout)
   fclose (f_out);
 return 0;
}
