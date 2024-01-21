/* General purpose program for Checker
   Copyright 1994, 1995 Tristan Gingold
		  Written July 1994 by Tristan Gingold

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
*/
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

/* Possible macros:
 *  HAVE_BFD: bfd facilities available.  bfd is used to interpret the output,
              to read the symbol table.
 *  GCC_COMPILE: Checker on linux-i386, your program must be compiled with a
              special as.
 *  GCCCHECKER: GCC-Checker, which use -fcheck-memory-usage.
 *  SIMCHECKER: Checker on Solaris2-sparc.  */

/* If we can use BFD, use it!  */ 
#ifdef HAVE_BFD
#include <bfd.h>
#include <getopt.h>
#endif

/* If checker can run GCC, define NEED_GCC.  */
#if defined (GCC_COMPILE) || defined (GCCCHECKER)
#define NEED_GCC
#endif

/* Defined by version.c.  */
extern char *version;
extern char *target;

/* The maximum line length.  */
#define MAXLEN 2048

#ifdef NEED_GCC
/* Where gcc can be found.  */
#ifndef GCC_PATH
#define GCC_PATH "/usr/bin/gcc"
#endif

/* Where g++ can be found.  */
#ifndef GXX_PATH
#define GXX_PATH "/usr/bin/g++"
#endif
#endif

/* Where the libraries of Checker are.  */
#ifndef CHECKER_PATH
#define CHECKER_PATH "/usr/local/lib/checker"
#endif

/* Where the include files of Checker are.  */
#ifndef CHECKER_INCLUDE_PATH
#define CHECKER_INCLUDE_PATH "/usr/local/checker/include"
#endif

/* The basename of argv[0].  */
char *program_name;

/* Give some tips and exit.  */
void give_tips (void) __attribute__ ((noreturn));
void
give_tips (void)
{
  fprintf (stderr, "Try `%s --help' for more informations.\n", program_name);
  exit (1);
}

/* Display the current version.  */
void
disp_version (int argc, char *argv[])
{
  printf ("Checker version %s for `%s'.\n", version, target);
}

/* Display the help.  */
void
help (int argc, char *argv[])
{
  puts ("Usage: checker COMMAND [OPTIONS] [ARGUMENTS]");
  puts ("Options and arguments can be set according to the commands:\n");
  puts ("checker -v");
  puts ("checker --version");
  puts ("  output version information and exit. No options.\n");
  puts ("checker -h");
  puts ("checker --help");
  puts ("  display help and exit. No options.\n");
#ifdef NEED_GCC
  puts ("checker gcc [GCC_ARGUMENTS]...");
  puts ("checker -gcc [GCC_ARGUMENTS]...");
  puts ("  compile with GNU CC for Checker.\n");
  puts ("checker g++ [G++_ARGUMENTS]...");
  puts ("checker -g++ [G++_ARGUMENTS]...");
  puts ("  compile with GNU G++ for Checker.\n");
#endif
#ifdef HAVE_BFD 
  puts ("checker -i [OPTION]... [program [file]]");
  puts ("checker --interpret [OPTION]... [program [file]]");
  puts ("  interpret the FILE (or stdin) produced by PROGRAM (or a.out).");
  puts ("  -o, --output OUTFILE  Set the output file.");
  puts ("  -r, --realpath        Give the real path.");
  puts ("  -b, --basename        Give the basename.");
#endif
#ifdef SIMCHECKER
  puts ("checker your_program [args]");
  puts ("  run your program through Checker.");
#endif
}

#ifdef HAVE_BFD
/* Interpret a file:
   replace each line   "$       pc=0xAAAAAAAA" 
   by		       "        pc=0xAAAAAAAA in function() at file:line"
   The informations are taken from the binary file.  */
void
interpret (int argc, char *argv[])
{
 FILE *in = stdin;
 FILE *out = stdout;
 bfd *abfd;
 asection *textsection;
 unsigned int storage_needed;
 asymbol **symbol_table;
 unsigned int number_of_symbols;
 char *filename;
 char *target = NULL;
 char buffer[MAXLEN];
 bfd_vma offset;
 int i;
 char *srcname;
 char *srcname1;
 char *functionname;
 unsigned int line;
 
 static int path_flag = 0;
 static struct option interpret_opts[]={
 { "output", 1, 0, 'o'},
 { "realpath", 0, &path_flag, 1},
 { "basename", 0, &path_flag, 2},
 { 0, 0, 0, 0}};

 /* Getopt will emit a message in case of error. */ 
 opterr = 1;
 
 /* Read the arguments. */
 while (1)
   {
     i = getopt_long (argc, argv, "o:rb", interpret_opts, &i);
     if (i == -1)
       break;
     switch (i)
       {
         case 0:	/* 'realpath' & 'basename' */
           break;
         case 'o':	/* -o: set the output file. */
           if (strcmp ("-", optarg) == 0)
             out = stdout;
           else
             {
               out = fopen (optarg, "w");
               if (out == NULL)
                 {
                   fprintf (stderr, "%s: can't open %s", program_name, optarg);
                   perror ("");
                   exit (2);
                 }
             }
           break;
         case 'r':	/* -r: give the realpath */
           path_flag = 1;
           break;
         case 'b':	/* -b: give the basename. */
           path_flag = 2;
           break;
         default:	/* error */
           exit(2);
       }
   }
   
 /* PROGRAM can follow. */ 
 if (optind < argc)
   filename = argv[optind++];
 else
   filename = "a.out";	/* by default */
   
 /* FILE can follow. */
 if (optind < argc)
   {
     if (strcmp ("-", argv[optind]) == 0)
       in = stdin;
     else
       {
         in = fopen (argv[optind], "r");
         if (in == NULL)
           {
             fprintf (stderr, "%s: can't open file %s", program_name, argv[optind]);
             perror ("");
             exit (2);
           }
       }
     optind++;
   }
 
 if (optind != argc)
   {
     fprintf (stderr, "Too many arguments\n");
     give_tips ();
   }
 
 /* Open the PROGRAM file.  */    
 abfd = bfd_openr (filename, target);
 if (abfd == NULL)
   {
     fprintf (stderr, "%s: ", program_name);
     bfd_perror (filename);
     exit (2);
   }
 /* PROGRAM must be an executable.  */
 if (bfd_check_format (abfd, bfd_object) == false)
   {
     fprintf (stderr, "%s: %s is not an object file\n", program_name, filename);
     exit (2);
   }
 
 /* Use the ".text" section.  */
 textsection = bfd_get_section_by_name (abfd, ".text");
 
 /* Read the symbol table.  */
 storage_needed = bfd_get_symtab_upper_bound (abfd);
 if (storage_needed == 0)
   {
     fprintf (stderr, "%s: no symbols!\n", program_name);
     exit (2);
   }
 symbol_table = (asymbol**) malloc (storage_needed);
 if (symbol_table == (asymbol**)0)
   {
     fprintf (stderr, "%s: virtual memory exhausted\n", program_name);
     exit (3);
   }
 number_of_symbols = bfd_canonicalize_symtab (abfd, symbol_table);
 
 /* Read lines of the file.  */
 while (fgets (buffer, MAXLEN -2, in))
   {
     if (buffer[0] == '$')
       {
         /* The format is: "$\tpc=0x%08x\n" */
         /* Read the pc */
         offset = 0;
         for (i = 7; buffer[i] != '\n'; i++) 
           {
             offset <<= 4;	/*  *16  */
             if (buffer[i] >= '0' && buffer[i] <= '9')
               offset += buffer[i] - '0';
             else if (buffer[i] >= 'A' && buffer[i] <= 'F')
               offset += buffer[i] - 'A' + 10;
             else if (buffer[i] >= 'a' && buffer[i] <= 'f')
               offset += buffer[i] - 'a' + 10;
             else break;
           }

	 /* Find the symbols for offset.  */
         if (bfd_find_nearest_line (abfd, textsection, symbol_table, offset,
         			    &srcname, &functionname, &line) == false)
           {
             fprintf (stderr, "%s: ", program_name);
             bfd_perror (filename);
             exit (2);
           }
         /* The srcname can be processed.  */
         switch (path_flag)
           {
             case 0:
               srcname1 = srcname;
               break;
             case 1:
               /* SRCNAME is processed only if it begins with a '/', since
                  it might not belong to the current directory.  */
               if (srcname[0] == '/')
                 {
                   /* Replace by the real path: follow the links, simplify.  */
                   realpath (srcname, buffer);
                   srcname1 = buffer;
                 }
               else
                 srcname1 = srcname;
               break;
             case 2:
               /* Replace by the basename.  */
               srcname1 = srcname + strlen (srcname);
               while (srcname1 != srcname && srcname1[-1] != '/')
                 srcname1--;
               break;
             default:
               srcname1 = srcname;
           }
         fprintf (out, "\tpc=0x%08x in %s() at %s:%d\n",
                       (unsigned int)offset, functionname, srcname1, line);
       }
     else
       {
         /* Nothin to do. */
         fputs (buffer, out);
       }
   }   
   
 /* Close the files.  */
 bfd_close (abfd);
 if (in != stdin)
   fclose (in);
 if (out != stdout)
   fclose (out);
 return;
}
#endif /* HAVE_BFD */


#ifdef NEED_GCC

/* When called with gcc, checker is a wrapper like g++.  It inserts args at
   the begining of the line command and libraries at the end.  */
#ifdef GCC_COMPILE
/* Args to insert, for gcc and g++.  */
char *added_args_for_gcc[] = { "checkergcc", "-B" CHECKER_PATH "/", 0};
char *added_args_for_gpp[] = { "checkerg++", "-B" CHECKER_PATH "/", 0};
char *added_libs_for_gcc[] = { 0};
char *added_libs_for_gxx[] = { 0};
#else
#ifdef GCCCHECKER
char *added_args_for_gcc[] = { 
	"checkergcc", "-fcheck-memory-usage",
	"-g",
	"-I" CHECKER_INCLUDE_PATH,
	"-D__CHECKER__",
	"-DMALLOC_0_RETURNS_NULL",
	"-L" CHECKER_PATH, 0};
char *added_args_for_gxx[] = { 
	"checkerg++", "-fcheck-memory-usage",
	"-g",
	"-I" CHECKER_INCLUDE_PATH,
	"-D__CHECKER__",
	"-DMALLOC_0_RETURNS_NULL",
	"-L" CHECKER_PATH, 0};
char *added_libs_for_gcc[] = { "-lchecker", 0};
char *added_libs_for_gxx[] = { "-lchecker", 0};
#endif
#endif

/* Compile with gcc.  */
void
compile (char *added_args[], char *added_libs[], char *prog, char *prog1,
	 int argc, char *argv[])
{
  int i,j;

  /* The new argument array.  */
  char **newargv;

  /* If true, libraries are inserted.  */
  int library = 1;

  /* If true, be verbose.  */
  int verbose = 0;

  /* Number of items in added_{args, libs}.  */
  int nbr_added_args;
  int nbr_added_libs;

  /* Compute the number of items.  */
  for (i = 0; added_args[i]; i++)
    ;
  nbr_added_args = i;

  for (i = 0; added_libs[i]; i++)
    ;
  nbr_added_libs = i;

  /* Alloc space for the array.  */
  newargv = (char **) alloca ((argc + nbr_added_args + 2 * nbr_added_libs)
  			      * sizeof (char*));

  /* Check if we have to insert the libraries.  */
  for (i = 0; i < argc; i++)
    if (argv[i][0] == '-')
      {
        /* The option -nostdlib, -c, -S, -E, -M and -MM prevent from linking.
           Note also that 'gcc -v' prevent also from linking.  */
        if (strcmp (argv[i], "nostdlib") == 0)
          library = 0;
        else if (argv[i][1] && argv[i][2] == 0)
          {
            if (strchr ("cSEM", argv[i][1]))
              library = 0;
            else if (argv[i][1] == 'v')
              {
                verbose = 1;
                if (argc == 2)
                  library = 0;
              }
          }
        else if (strcmp (argv[i], "-MM") == 0)
          library = 0;
      }
   /* Don't had the library if gcc is invoked without any args.  */
   if (argc == 1)
     library = 0;

  /* Insert the (added) args.  */
  for (i = 0; i < nbr_added_args; i++)
    newargv[i] = added_args[i];

  /* Copy the line args.  Also add a NULL (contained in argv).  */
  for (i = 1; i < argc + 1; i++)
    {
      /* Also insert the library before *the first* `-l' arg.  */
      if (library == 1 && argv[i] && argv[i][0] == '-' && argv[i][1] == 'l')
        {
          for (j = 0; j < nbr_added_libs; j++)
            newargv[nbr_added_args + i - 1 + j] = added_libs[j];
          nbr_added_args += nbr_added_libs;
          library = 2;
        }

      /* Copy one arg.  */
      newargv[nbr_added_args + i - 1] = argv[i];
    }

  /* Insert the library.  */
  if (library)
    {
      for (i = 0; i < nbr_added_libs; i++)
        newargv[nbr_added_args + argc + i - 1] = added_libs[i];
      newargv[nbr_added_args + argc + nbr_added_libs - 1] = (char *) 0;
    }

  /* Verbose.  */
  if (verbose)
    {
      for (i = 0; newargv[i]; i++)
        {
          fputs (newargv[i], stdout);
          putchar (' ');
        }
      putchar ('\n');
   }

  execv (prog, newargv);
  execvp (prog1, newargv);
  perror ("gcc not found!");
  exit (1);
}

/* Compile with gcc.  */
void
gcc (int argc, char *argv[])
{
  compile (added_args_for_gcc, added_libs_for_gcc,
  	   GCC_PATH, "gcc",
  	   argc, argv);
}

/* Compile with g++.  */
void
gplusplus (int argc, char *argv[])
{
  compile (added_args_for_gxx, added_libs_for_gxx,
  	   GXX_PATH, "g++",
  	   argc, argv);
}
#endif /* NEED_GCC */

#ifdef SIMCHECKER
void
run_simchecker (char **argv)
{
  struct stat statbuf;
  char *preload = "LD_PRELOAD=" CHECKER_PATH "/checker.so.1";
  char *preloaded_file = preload + 11;
  
  if (getenv ("LD_PRELOAD"))
    {
      fprintf (stderr, "LD_PRELOAD variable is already set.  Unset it before running Checker.\n");
      exit (2);
    }
  if (stat (preloaded_file, &statbuf) == -1)
    {
      fprintf (stderr, "Can't stat the shared library `%s': ", preloaded_file);
      perror (NULL);
      exit (2);
    }
  if (stat (argv[1], &statbuf) == -1)
    {
      fprintf (stderr, "Can't stat your program `%s': ", argv[1]);
      perror (NULL);
      exit (2);
    }
  if (   ((statbuf.st_mode & S_ISUID) && geteuid () != statbuf.st_uid)
      || ((statbuf.st_mode & S_ISGID) && getegid () != statbuf.st_gid))
    {
      fprintf (stderr, "Checker can't work with the set[ug]id program `%s'\n", argv[1]);
      fprintf (stderr, "Please, do `su' and try again.\n");
      exit (2);
    }
  putenv (preload);
  execv (argv[1], argv + 1);
  perror ("exec failed");
  exit (2);
}
#endif /* SIMCHECKER */

/* Describe which function to call according to flags.  */
struct flags
{
  char short_flag;
  char *long_flag;
  void (*function)(int, char **);
};

struct flags commands[] = {
{ 'h', "--help",	&help},
{ 'v', "--version",	&disp_version},
#ifdef HAVE_BFD
{ 'i', "--interpret",	&interpret},
#endif
#ifdef NEED_GCC
{  0,  "gcc",		&gcc},
{  0,  "-gcc",		&gcc},
{  0,  "g++",		&gplusplus},
{  0,  "-g++",		&gplusplus},
#endif
{  0,   (char*)0 }};

int
main (int argc, char *argv[])
{
 struct flags *aflag;
 
 /* Extract the basename of argv[0].  */
 program_name = argv[0] + strlen (argv[0]);
 while (program_name != argv[0] && program_name[-1] != '/')
   program_name--;

#ifdef NEED_GCC
 if (strcmp ("checkergcc", program_name) == 0)
   gcc (argc, argv);
 
 if (strcmp ("checkerg++", program_name) == 0)
   gplusplus (argc, argv);
#endif /* NEED_GCC */
   
 if (argc < 2)
   {
     fprintf (stderr, "Usage: %s command [args]\n", program_name);
     give_tips ();
   }
 
 if (argv[1][0] == '-' && argv[1][1] != '\0' && argv[1][2] == '\0')
   {
     /* A short flag.  */
     for (aflag = commands; aflag->long_flag; aflag++)
       {
         if (argv[1][1] == aflag->short_flag)
           {
             (*aflag->function)(argc - 1, &argv[1]);
             exit (0);
           }
       }
   }
 else
   {
     /* Something else.  */
     for (aflag = commands; aflag->long_flag; aflag++)
       {
         if (strcmp (argv[1], aflag->long_flag) == 0)
           {
             (*aflag->function)(argc - 1, &argv[1]);
             exit (0);
           }
       }
   }
   
#ifdef SIMCHECKER
 /* `checker prog [args]'.  PROG must not begin with a `-'.  */
 if (argv[1] && argv[1][1] != '-')
   run_simchecker (argv);
#endif /* SIMCHECKER */

 fprintf (stderr, "%s: unknown command %s\n", argv[0], argv[1]);
 give_tips ();
 return 1;
}
