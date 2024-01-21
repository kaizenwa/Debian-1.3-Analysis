/* Basic module support for the GPC
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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
 *
 */

#include "config.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "tree.h"
#include "c-tree.h"
#include "input.h"
#include "rtl.h"
#include "obstack.h"
#include "flags.h"

#include "gpc-defs.h"

#ifdef __EMX__
#include <process.h>
#include <stdlib.h>
#include <errno.h>
#endif /* __EMX__ */

#ifdef USG
#define vfork fork
#endif /* USG */

extern tree identifier_output;
extern tree identifier_input;
extern tree global_input_file_node;
extern tree global_output_file_node;

/* Allocates global object for the module list.
 * find_module().
 */
extern struct obstack permanent_obstack;

/* We are currently compiling this module */

module_t module_list = NULL_MODULE;
module_t current_module;

/* A list of all exported names in all modules seen so far.
 *
 * TREE_LIST of exported interfaces we know at the current point of
 * compilation: TREE_VALUE is an IDENTIFIER_NODE of an exported
 * interface name TREE_PURPOSE is a TREE_LIST chain of the names (?
 * decls?) exported by this interface */
tree exported_interface_list;

/* Required module interfaces StandardInput and StandardOutput */
tree standard_interface_input;
tree standard_interface_output;

/* Export "all" mark */
tree export_all;

/* List of the contents of a gpi file */
tree gpi_contents;

/* Flag for debugging the gpi mechanism */
int flag_debug_gpi = 0;

/* Nonzero means do an AutoMake */
int flag_automake = 0;

/* Options to pass to child gpc processes in AutoMake */
char* automake_gpc_options = NULL;

/* Name of a file to store information about targets in */
char* automake_temp_filename = NULL;

/* No comment. :-) */
int we_are_loading_a_gpi_file = 0;


/* Scan the program/module parameter list for entries of FILE_TYPE.
 * If this is at top level, and they are variables of file type,
 * flag the files as external.
 *
 * Since the order of declarations are relaxed, this is checked
 * before every routine.
 *
 * All nodes already handled are marked.
 */
void
associate_external_objects (external_name_list)
     tree external_name_list;
{
  tree link;

  if (external_name_list && top_level_p (current_module->main_program))
    for (link = external_name_list;
	 link;
	 link = TREE_CHAIN (link))
      
      {
	tree id = TREE_VALUE (link);

	if (id == identifier_output)
	  current_module->output_file_node = global_output_file_node;
	else if (id == identifier_input)
	  current_module->input_file_node = global_input_file_node;
	else if (! TREE_PURPOSE (link))
	  {
	    tree name = lookup_name (id);
	    
	    if (name
		&& TREE_CODE (name) == VAR_DECL
		&& TREE_CODE (TREE_TYPE (name)) == FILE_TYPE)
	      {
		PASCAL_EXTERNAL_OBJECT (name) = 1;
		TREE_PURPOSE (link) = error_mark_node;
	      }
	  }
      }
}

/* Check if all the names in program/module param list have been declared
 *
 * If not, give a warning.
 */
void
check_external_objects (idlist)
     tree idlist;
{
  char *what  = current_module->main_program ? "program" : "module";

  for (; idlist; idlist = TREE_CHAIN (idlist))
    {
      tree id = TREE_VALUE (idlist);

      if (id != identifier_output && id != identifier_input)
	{
	  tree name = lookup_name (id);

	  if (name == NULL_TREE)
	    warning ("Identifier `%s' in %s heading is undefined",
		     IDENTIFIER_POINTER (id), what);
	  else if (TREE_CODE (name) != VAR_DECL
		   || TREE_CODE (TREE_TYPE (name)) != FILE_TYPE)
	    warning ("Identifier `%s' in %s heading is not a variable of file type",
		     IDENTIFIER_POINTER (id), what);
	}
    }
}

/* Possibly add the `static' qualifier.
 */
tree
maybe_make_static (qualifiers)
     tree qualifiers;
{
  int make_static = 1;

  /* @@@@ Need to verify if the function was declared with a
   *      FORWARD directive.
   */
  if (qualifiers)
    {
      tree scan = qualifiers;
      for (; scan; scan = TREE_CHAIN (scan))
	if (TREE_VALUE (scan) == extern_id)
	  {
	    make_static = FALSE;
	    break;
	  }
    }
  

  if (make_static)
    qualifiers = chainon (qualifiers, build_tree_list (NULL_TREE, static_id));

  return qualifiers;
}

/* Locates a module by it's NAME.
 * If CREATE is nonzero, create a new module if old not found
 */
module_t
find_module (name, create)
     tree name;
     int create;
{
  module_t curr;
  
  for (curr = module_list; curr; curr = curr->next)
    {
      if (curr->name == name)
	return curr;
    }
  
  if (create)
    {
      curr = (module_t) obstack_alloc (&permanent_obstack,
				       sizeof (struct module));
      /* Initialize */
      bzero ((void *)curr, sizeof (struct module));
      curr->name  = name;
      curr->next  = module_list;
      module_list = curr;
    }
  return curr;
}



void
initialize_module (id)
     tree id;
{
  current_module = find_module (id, 1);
}

/* NAME is an IDENTIFIER_NODE of the exported interface name.
 * EXPORT_LIST:
 *   TREE_LIST
 *       TREE_PURPOSE: Export renaming (new name) or NULL_TREE.
 *       TREE_VALUE  : IDENTIFIER_NODE of the exported name
 */
void
export_interface (name, export_list)
     tree name;
     tree export_list;
{
  tree exported       = exported_interface_list;
  tree exported_names = NULL_TREE;
  tree nscan, new_export_list;

  for (; exported; exported = TREE_CHAIN (exported))
    if (TREE_VALUE (exported) == name)
      {
	error ("Interface `%s' has already been exported",
	       IDENTIFIER_POINTER (name));
	return;
      }

  if (export_list == export_all)
    exported_names = build_tree_list (NULL_TREE, NULL_TREE);
  else
    for (nscan = export_list; nscan; nscan = TREE_CHAIN (nscan))
      {
        tree value = build_tree_list (NULL_TREE, TREE_VALUE (nscan));

        if (TREE_PURPOSE (nscan))
	  warning ("Export renaming is not yet implemented; exporting `%s'",
		   IDENTIFIER_POINTER (TREE_VALUE (nscan)));

        exported_names = chainon (exported_names, value);
      }
  
  current_module->exports
    = chainon (current_module->exports,
	       build_tree_list (exported_names, name));

  new_export_list = build_tree_list (exported_names, name);
  exported_interface_list = chainon (exported_interface_list, new_export_list);
  if (export_list == export_all)
    current_module->autoexport = chainon (current_module->autoexport,
                                          build_tree_list (NULL_TREE,
                                                           new_export_list));
}

int
name_exported_p (name)
     tree name;
{
  tree scan = current_module->exports;
  for (; scan; scan = TREE_CHAIN (scan))
    {
      tree id_chain = TREE_PURPOSE (scan);

      for (; id_chain; id_chain = TREE_CHAIN(id_chain))
	if (name == TREE_VALUE (id_chain))
	  return 1;
    }
  return 0;
}

/*
 * Export names of a Borland Pascal Unit;
 * intended to handle also "export foo = all" clauses
 * and PXSC modules
 */
void
handle_autoexport (name)
     tree name;
{
  tree e;
  if (! we_are_loading_a_gpi_file)
    for (e = current_module->autoexport; e; e = TREE_CHAIN (e))
      {
        tree nscan, p = TREE_PURPOSE (TREE_VALUE (e));
        for (nscan = p; nscan; nscan = TREE_CHAIN (nscan))
          {
            if (TREE_VALUE (nscan) == name)
              break;
          }
        if (!nscan)
          p = chainon (p, module_export_clause (name, NULL_TREE, 0));
      }
}



/*
 *  The AutoMake facility
 *
 *  When invoked with an --automake option, gpc can do an
 *  implicit `make' for preprocessing, compiling and linking.
 *  A Makefile is not needed; everything is extracted from
 *  the Pascal source.
 *
 *  Read AUTOMAKE.DOC for more information about this.
 *
 *  - PG
 */

/*
 * For the AutoMake facility we need to execute gpc as a child
 * process.  We do it here by copying pexecute() from gcc.c to
 * make it easier to keep track of modifications in gcc, and by
 * providing an easier execute_child() interface function which
 * does not use pipes.
 */

/*
 * Definitions from gcc.c
 */

extern int execv (), execvp ();

/* If a stage of compilation returns an exit status >= 1,
   compilation of that file ceases.  */

#define MIN_FATAL_STATUS 1

#ifndef WIFSIGNALED
#define WIFSIGNALED(S) (((S) & 0xff) != 0 && ((S) & 0xff) != 0x7f)
#endif

#ifndef WIFEXITED
#define WIFEXITED(S) (((S) & 0xff) == 0)
#endif

#ifndef WEXITSTATUS
#define WEXITSTATUS(S) (((S) & 0xff00) >> 8)
#endif

/*
 * Provide a function to report an error if a child's execution fails
 */
static void
perror_exec (name)
     char *name;
{
  error ("installation problem, cannot exec `%s'", name);
}

#ifdef __EMX__
static int pipes_supported = 0;
#endif /* __EMX__ */

/* Unmodified copied part from gcc.c starts here. */

/* stdin file number.  */
#define STDIN_FILE_NO 0

/* stdout file number. */
#define STDOUT_FILE_NO 1

/* value of `pipe': port index for reading.  */
#define READ_PORT 0

/* value of `pipe': port index for writing.  */
#define WRITE_PORT 1

/* Pipe waiting from last process, to be used as input for the next one.
   Value is STDIN_FILE_NO if no pipe is waiting
   (i.e. the next command is the first of a group).  */

static int last_pipe_input;

/* Fork one piped subcommand.  FUNC is the system call to use
   (either execv or execvp).  ARGV is the arg vector to use.
   NOT_LAST is nonzero if this is not the last subcommand
   (i.e. its output should be piped to the next one.)  */

#ifdef __MSDOS__

#include <process.h>
static int
pexecute (search_flag, program, argv, not_last)
     int search_flag;
     char *program;
     char *argv[];
     int not_last;
{
#ifdef __GO32__
  int i = (search_flag ? spawnv : spawnvp) (1, program, argv);
#else
  char *scmd, *rf;
  FILE *argfile;
  int i, el = search_flag ? 0 : 4;

  scmd = (char *)malloc (strlen (program) + strlen (temp_filename) + 6 + el);
  rf = scmd + strlen(program) + 2 + el;
  sprintf (scmd, "%s%s @%s.gp", program,
	   (search_flag ? "" : ".exe"), temp_filename);
  argfile = fopen (rf, "w");
  if (argfile == 0)
    pfatal_with_name (rf);

  for (i=1; argv[i]; i++)
    {
      char *cp;
      for (cp = argv[i]; *cp; cp++)
	{
	  if (*cp == '"' || *cp == '\'' || *cp == '\\' || isspace (*cp))
	    fputc ('\\', argfile);
	  fputc (*cp, argfile);
	}
      fputc ('\n', argfile);
    }
  fclose (argfile);

  i = system (scmd);

  remove (rf);
#endif
  
  if (i == -1)
    {
      perror_exec (program);
      return MIN_FATAL_STATUS << 8;
    }
  return i << 8;
}

#endif

/* include for Unix-like environments but not for Dos-like environments */
#if !defined(__MSDOS__) && !defined(OS2) &&(defined(__CYGWIN32__) || !defined(_WIN32))

#ifdef __EMX__

static int
pexecute (search_flag, program, argv, not_last)
     int search_flag;
     char *program;
     char *argv[];
     int not_last;
{
  int pid;
  int pdes[2], org_stdin, org_stdout;
  int input_desc = last_pipe_input;
  int output_desc = STDOUT_FILE_NO;

  /* If this isn't the last process, make a pipe for its output,
     and record it as waiting to be the input to the next process.  */

  if (not_last)
    {
      if (pipe (pdes) < 0)
	pfatal_with_name ("pipe");
      output_desc = pdes[WRITE_PORT];
      last_pipe_input = pdes[READ_PORT];
    }
  else
    last_pipe_input = STDIN_FILE_NO;

  if (pipes_supported && input_desc != STDIN_FILE_NO)
    {
      org_stdin = dup (STDIN_FILE_NO);
      dup2 (input_desc, STDIN_FILE_NO);
      close (input_desc); 
    }
  if (pipes_supported && output_desc != STDOUT_FILE_NO)
    {
      org_stdout = dup (STDOUT_FILE_NO);
      dup2 (output_desc, STDOUT_FILE_NO);
      close (output_desc);
    }
  pid = (search_flag ? spawnv : spawnvp) (P_NOWAIT, program,
                                          (char const * const *)argv);
  if (pipes_supported && input_desc != STDIN_FILE_NO)
    {
      dup2 (org_stdin, STDIN_FILE_NO);
      close (org_stdin);
    }
  if (pipes_supported && output_desc != STDOUT_FILE_NO)
    {
      dup2 (org_stdout, STDOUT_FILE_NO);
      close (org_stdout);
    }

  if (pid == -1)
    {
      perror_exec (program);
      exit (-1);
    }

  return pid;
}

#else /* not __EMX__ */

static int
pexecute (search_flag, program, argv, not_last)
     int search_flag;
     char *program;
     char *argv[];
     int not_last;
{
  int (*func)() = (search_flag ? execv : execvp);
  int pid;
  int pdes[2];
  int input_desc = last_pipe_input;
  int output_desc = STDOUT_FILE_NO;
  int retries, sleep_interval;

  /* If this isn't the last process, make a pipe for its output,
     and record it as waiting to be the input to the next process.  */

  if (not_last)
    {
      if (pipe (pdes) < 0)
	pfatal_with_name ("pipe");
      output_desc = pdes[WRITE_PORT];
      last_pipe_input = pdes[READ_PORT];
    }
  else
    last_pipe_input = STDIN_FILE_NO;

  /* Fork a subprocess; wait and retry if it fails.  */
  sleep_interval = 1;
  for (retries = 0; retries < 4; retries++)
    {
      pid = vfork ();
      if (pid >= 0)
	break;
      sleep (sleep_interval);
      sleep_interval *= 2;
    }

  switch (pid)
    {
    case -1:
#ifdef vfork
      pfatal_with_name ("fork");
#else
      pfatal_with_name ("vfork");
#endif
      /* NOTREACHED */
      return 0;

    case 0: /* child */
      /* Move the input and output pipes into place, if nec.  */
      if (input_desc != STDIN_FILE_NO)
	{
	  close (STDIN_FILE_NO);
	  dup (input_desc);
	  close (input_desc);
	}
      if (output_desc != STDOUT_FILE_NO)
	{
	  close (STDOUT_FILE_NO);
	  dup (output_desc);
	  close (output_desc);
	}

      /* Close the parent's descs that aren't wanted here.  */
      if (last_pipe_input != STDIN_FILE_NO)
	close (last_pipe_input);

      /* Exec the program.  */
      (*func) (program, argv);
      perror_exec (program);
      exit (-1);
      /* NOTREACHED */
      return 0;

    default:
      /* In the parent, after forking.
	 Close the descriptors that we made for this child.  */
      if (input_desc != STDIN_FILE_NO)
	close (input_desc);
      if (output_desc != STDOUT_FILE_NO)
	close (output_desc);

      /* Return child's process number.  */
      return pid;
    }
}

#endif /* not __EMX__ */
#endif /* ! __MSDOS__ && ! OS2 && && (__CYGWIN32___ || ! _WIN32) */

#if defined(OS2)

static int
pexecute (search_flag, program, argv, not_last)
     int search_flag;
     char *program;
     char *argv[];
     int not_last;
{
  return (search_flag ? spawnv : spawnvp) (1, program, argv);
}
#endif /* OS2 */

#if defined (_WIN32) && !defined (__CYGWIN32__)

#include <process.h>
/* ??? Why are these __spawnv{,p} and not _spawnv{,p}?  */
extern int __spawnv ();
extern int __spawnvp ();

static int
pexecute (search_flag, program, argv, not_last)
     int search_flag;
     char *program;
     char *argv[];
     int not_last;
{
  return (search_flag ? __spawnv : __spawnvp) (1, program, argv);
}
#endif /* _WIN32 */

/* Unmodified copied part from gcc.c ends here. */

/* The following execute_child () function is inspired by
 * execute () of gcc.c, so keep track of changes there!
 */
static int
execute_child (argv)
     char *argv[];
{
  int status;
  int pid;
  last_pipe_input = STDIN_FILE_NO;
  if (version_flag)  /* i.e. gpc was invoked with --verbose */
    {
      char **j;
      fprintf (stderr, "GPC AutoMake: ");
      for (j = argv; *j; j++)
        fprintf (stderr, " %s", *j);
      fprintf (stderr, "\n");
    }
  pid = pexecute (0, argv [0], argv, 0);
#ifdef __MSDOS__
  status = pid;
#else
#if defined (_WIN32) && !defined (__CYGWIN32__)
  pid = cwait (&status, pid, WAIT_CHILD);
#else
  pid = wait (&status);
#endif
#endif
#ifdef __EMX__
  while (pid < 0 && errno == EINTR)
    pid = wait (&status);
#endif /* __EMX__ */
  if (pid < 0)
    abort ();
  if (WIFSIGNALED (status)
      || WIFEXITED (status) && WEXITSTATUS (status) >= MIN_FATAL_STATUS)
    {
      /* An error message already has been put out by the child gpc process. */
      return -1;
    }
  else
    return 0;
}

/*
 * Add an object file to the list in the AutoMake temporary file.
 * The contents of that file will be passed to the linker.
 */
void
add_to_automake_temp_file (object_filename)
     char *object_filename;
{
  if (automake_temp_filename && object_filename)
    {
       FILE *automake_temp_file = fopen (automake_temp_filename, "at");
       if (automake_temp_file)
         {
           fprintf (automake_temp_file, "%s\n", object_filename);
           fclose (automake_temp_file);
         }
    }
}

/*
 * Pass the AutoMake options to a child process
 */
int pass_automake_gpc_options (argv, i)
     char **argv;
     int i;
{
  char recursive_options [] = "--automake";
  if (automake_gpc_options)
    {
      /* Pass the options specified behind --automake */
      char *p = save_string (automake_gpc_options);
      argv [i++] = p;
      while (*p)
        {
          if (*p == ' ')
            {
              *p = 0;
              p++;
              argv [i++] = p;
            }
          else
            p++;
        } 
      /* Pass the --automake specification itself */
      p = concat ( "=\"", automake_gpc_options, "\"");
      argv [i++] = concat (recursive_options, p, "");
    }
  else
    argv [i++] = save_string (recursive_options);
  if (automake_temp_filename)
    argv [i++] = concat ("--amtmpfile=", automake_temp_filename, "");
  return i;
}

/*
 * Check whether a module must be recompiled (for AutoMake).
 */
int
module_must_be_recompiled (gpi_filename, module_filename)
     char *gpi_filename;
     char *module_filename;
{
  struct stat gpi_status, source_status;
  char *argv [256], *dep_filename, *p;
  int i;
  FILE *dep_file;

  /* no source found --> cannot recompile */
  if (access (module_filename, 4) != 0)
    return 0;

  /* gpi older than source --> recompile
   */
  stat (gpi_filename, &gpi_status);
  stat (module_filename, &source_status);
  if (gpi_status.st_mtime < source_status.st_mtime)
    return 1;

  /* Run the preprocessor with -M for creating a dependency file.
   * The name of that file is derived from module_filename by 
   * replacing `.p' or `.pas' with `.d' (like gcc does with the 
   * -MD option).
   */
  dep_filename = xmalloc (strlen (module_filename) + 3);
  strcpy (dep_filename, module_filename);
  p = dep_filename + strlen (dep_filename);
  do
    p--;
  while (p >= dep_filename && *p != '.');
  *p = 0;
  strcat (dep_filename, ".d");
  i = 0;
  argv [i++] = "gpc";
  i = pass_automake_gpc_options (argv, i);
  argv [i++] = "-M";
  argv [i++] = module_filename;
  argv [i++] = "-o";
  argv [i++] = dep_filename;
  argv [i++] = NULL;
  execute_child (argv);

  /* Parse the `.d' file and check if the object file or the gpi file
   * is older than one of the source files.  If yes --> recompile.
   */
  dep_file = fopen (dep_filename, "rt");
  if (dep_file)
    {
      char source_filename [256], *object_filename = NULL;
      while (! feof (dep_file))
        {
          fscanf (dep_file, "%s", source_filename);
          if (strcmp (source_filename, "\\") != 0)
            {
              p = source_filename + strlen (source_filename) - 1;
              if (*p == ':')
                {
                  /* This is the name of the object file.
                   * If it doesn't exist, we must compile.
                   * If it is older than the gpi file,
                   * update the time.
                   */
                  *p = 0;
                  object_filename = save_string (source_filename);
                  if (access (object_filename, 4) != 0)
                    {
                      fclose (dep_file);
                      unlink (dep_filename);
                      return 1;
                    }
                  stat (object_filename, &source_status);
                  if (gpi_status.st_mtime > source_status.st_mtime)
                    gpi_status.st_mtime = source_status.st_mtime;
                }
              else
                {
                  /* This is the name of a source file.
                   * If it is younger than the gpi file,
                   * recompile.
                   */
                  stat (source_filename, &source_status);
                  if (gpi_status.st_mtime < source_status.st_mtime)
                    {
                      fclose (dep_file);
                      unlink (dep_filename);
                      return 1;
                    }
                }
            }
        }
      fclose (dep_file);
      unlink (dep_filename);
      /* Recompile not needed.  Tell the linker about the object file. */
      if (object_filename)
        add_to_automake_temp_file (object_filename);
    }
  /* Otherwise: no further check possible */

  /* All possible checks passed --> no recompilation.
   */
  return 0;
}

/*
 * Compile a module during an AutoMake.
 */
int
compile_module (filename)
     char *filename;
{
  char *argv [256];
  if (access (filename, 4) == 0)
    {
      int result, i = 0;
      char *object_filename, *p;
 
      /* Tell the linker about the object file */
      object_filename = xmalloc (strlen (filename) + 3);
      strcpy (object_filename, filename);
      p = object_filename + strlen (filename) - 1;
      while (p > object_filename && *p != '.')
        p--;
      p++;
      *p = 0;
      strcat (object_filename, "o");
      add_to_automake_temp_file (object_filename);

      argv [i++] = "gpc";
      i = pass_automake_gpc_options (argv, i);
      argv [i++] = "-c";
      argv [i++] = filename;
      argv [i++] = NULL;
      return execute_child (argv);
    }
  else
    return -1;
}



/*
 *  GPI file handling -- storing and retreiving tree nodes in an
 *  implementation-dependend (but not *too* implementation-dependend ;-)
 *  "GNU Pascal Interface" binary file.
 *
 *  For an introduction, read the file GPI.DOC.
 *
 *  - PG
 */

/* Store a string in a binary stream
 */
void
store_string (f, s)
     FILE *f;
     const char *s;
{
  unsigned char l = strlen (s);
  fwrite (&l, 1, 1, f);
  if (l > 0)
    fwrite (s, l, 1, f);
}

/* Load a string out of a binary stream
 */
void
load_string (f, s)
     FILE *f;
     char *s;
{
  unsigned char l;
  fread (&l, sizeof (unsigned char), 1, f);
  if (l > 0)
    fread (s, l, 1, f);
  s [l] = 0;
}

/* Store a tree node's flags in a stream
 */
void
store_flags (t, f)
     tree t;
     FILE *f;
{
  fwrite ((tree *) t + 2, 4, 1, f);
}

/* Load a tree node's flags out of a stream
 */
void
load_flags (t, f)
     tree t;
     FILE *f;
{
  fread ((tree *) t + 2, 4, 1, f);
}

/* Store a *_TYPE tree node's extra flags in a stream
 */
void
store_type_flags (t, f)
     tree t;
     FILE *f;
{
  fwrite (&TYPE_UID (t), sizeof (unsigned) + 4 + sizeof (int), 1, f);
}

/* Load a *_TYPE tree node's extra flags out of a stream
 */
void
load_type_flags (t, f)
     tree t;
     FILE *f;
{
  fread (&TYPE_UID (t), sizeof (unsigned) + 4 + sizeof (int), 1, f);
}

/* Store a *_DECL tree node's extra flags in a stream
 */
void
store_decl_flags (t, f)
     tree t;
     FILE *f;
{
  fwrite (&DECL_UID (t), sizeof (unsigned) + 4, 1, f);
  fwrite (&DECL_FRAME_SIZE (t), sizeof (int), 1, f);
}

/* Load a *_DECL tree node's extra flags out of a stream
 */
void
load_decl_flags (t, f)
     tree t;
     FILE *f;
{
  fread (&DECL_UID (t), sizeof (unsigned) + 4, 1, f);
  fread (&DECL_FRAME_SIZE (t), sizeof (int), 1, f);
}

/*
 * Store (parts of) a tree (containing a declaration) in a stream.
 * (A generalized version of this should go into tree.c.)
 * In order to save disk space, try to store only those fields 
 * of each tree node which are relevant in a module interface.
 */
void
store_tree (t, s, depth)
     tree t;
     FILE *s;
     int depth;
{
  unsigned char code;

  if (!t)
    {
      if (flag_debug_gpi)
        fprintf (stderr, "storing (%d): NULL_TREE\n", depth);
      code = (unsigned char) LAST_AND_UNUSED_TREE_CODE;
      fwrite (&code, 1, 1, s);
      return;
    }

  #define STORE_SPECIAL(n,node)                               \
  if (t == node)                                              \
    {                                                         \
      code = (unsigned char) LAST_AND_UNUSED_TREE_CODE + n;   \
      fwrite (&code, 1, 1, s);                                \
      if (flag_debug_gpi)                                     \
        {                                                     \
          char msg[] = GPC_MAKE_STRING (node);                \
          fprintf (stderr, "storing (%d): %s\n", depth, msg); \
        }                                                     \
      return;                                                 \
    }

  STORE_SPECIAL (1, error_mark_node);
  STORE_SPECIAL (2, short_integer_type_node);
  STORE_SPECIAL (3, integer_type_node);
  STORE_SPECIAL (4, long_integer_type_node);
  STORE_SPECIAL (5, long_long_integer_type_node);
  STORE_SPECIAL (6, short_unsigned_type_node);
  STORE_SPECIAL (7, unsigned_type_node);
  STORE_SPECIAL (8, long_unsigned_type_node);
  STORE_SPECIAL (9, long_long_unsigned_type_node);
  STORE_SPECIAL (10, boolean_type_node);
  STORE_SPECIAL (11, boolean_false_node);
  STORE_SPECIAL (12, boolean_true_node);
  STORE_SPECIAL (13, ptrdiff_type_node);
  STORE_SPECIAL (14, unsigned_char_type_node);
  STORE_SPECIAL (15, signed_char_type_node);
  STORE_SPECIAL (16, char_type_node);
  STORE_SPECIAL (17, wchar_type_node);
  STORE_SPECIAL (18, signed_wchar_type_node);
  STORE_SPECIAL (19, unsigned_wchar_type_node);
  STORE_SPECIAL (20, float_type_node);
  STORE_SPECIAL (21, double_type_node);
  STORE_SPECIAL (22, long_double_type_node);
  STORE_SPECIAL (23, complex_integer_type_node);
  STORE_SPECIAL (24, complex_float_type_node);
  STORE_SPECIAL (25, complex_double_type_node);
  STORE_SPECIAL (26, complex_long_double_type_node);
  STORE_SPECIAL (27, intQI_type_node);
  STORE_SPECIAL (28, intHI_type_node);
  STORE_SPECIAL (29, intSI_type_node);
  STORE_SPECIAL (30, intDI_type_node);
  STORE_SPECIAL (31, unsigned_intQI_type_node);
  STORE_SPECIAL (32, unsigned_intHI_type_node);
  STORE_SPECIAL (33, unsigned_intSI_type_node);
  STORE_SPECIAL (34, unsigned_intDI_type_node);
  STORE_SPECIAL (35, void_type_node);
  STORE_SPECIAL (36, ptr_type_node);
  STORE_SPECIAL (37, const_ptr_type_node);
  STORE_SPECIAL (38, string_type_node);
  STORE_SPECIAL (39, const_string_type_node);
  STORE_SPECIAL (40, char_array_type_node);
  STORE_SPECIAL (41, int_array_type_node);
  STORE_SPECIAL (42, wchar_array_type_node);
  STORE_SPECIAL (43, default_function_type);
  STORE_SPECIAL (44, double_ftype_double);
  STORE_SPECIAL (45, double_ftype_double_double);
  STORE_SPECIAL (46, int_ftype_int);
  STORE_SPECIAL (47, long_ftype_long);
/*
  STORE_SPECIAL (48, float_ftype_float);
  STORE_SPECIAL (49, ldouble_ftype_ldouble);
*/
  STORE_SPECIAL (50, void_ftype_ptr_ptr_int);
  STORE_SPECIAL (51, int_ftype_ptr_ptr_int);
  STORE_SPECIAL (52, void_ftype_ptr_int_int);
  STORE_SPECIAL (53, string_ftype_ptr_ptr);
  STORE_SPECIAL (54, int_ftype_string_string);
  STORE_SPECIAL (55, int_ftype_cptr_cptr_sizet);
  STORE_SPECIAL (56, integer_zero_node);
  STORE_SPECIAL (57, null_pointer_node);
  STORE_SPECIAL (58, integer_one_node);

  #undef STORE_SPECIAL

  if (depth > 0)
    {
      tree c;
      int count = 0;
      for (c = gpi_contents; c; c = TREE_CHAIN (c))
        {
          if (t == TREE_VALUE (c))
            {
              if (flag_debug_gpi)
                {
                  fprintf (stderr, "storing (%d, via gpi_contents):\n", depth);
                  debug_tree (t);
                }
              code = 0xFF;
              fwrite (&code, 1, 1, s);
              fwrite (&count, sizeof (int), 1, s);
              return;
            }
          count++;
        }
    }
  if (flag_debug_gpi)
    {
      fprintf (stderr, "storing (%d):\n", depth);
      debug_tree (t);
    }
  code = (unsigned char) TREE_CODE (t);
  fwrite (&code, 1, 1, s);
  switch (TREE_CODE (t))
    {
      case IDENTIFIER_NODE:
        {
          store_string (s, IDENTIFIER_POINTER (t));
          store_flags (t, s);
          if (depth == 0)
            store_tree (IDENTIFIER_GLOBAL_VALUE (t), s, depth + 1);
          break;
        }

      case TREE_LIST:
        {
          store_tree (TREE_VALUE (t), s, depth + 1);
          store_tree (TREE_CHAIN (t), s, depth + 1);
          store_flags (t, s);
          break;
        }

      case VOID_TYPE:
      case REAL_TYPE:
      case COMPLEX_TYPE:
      case ENUMERAL_TYPE:
      case BOOLEAN_TYPE:
      case CHAR_TYPE:
      case SET_TYPE:
      case LANG_TYPE:
        {
          store_flags (t, s);
          store_type_flags (t, s);
          store_tree (TYPE_SIZE (t), s, depth + 1);
          break;
        }

      case INTEGER_TYPE:
        {
          store_flags (t, s);
          store_type_flags (t, s);
          store_tree (TYPE_SIZE (t), s, depth + 1);
          store_tree (TYPE_MIN_VALUE (t), s, depth + 1);
          store_tree (TYPE_MAX_VALUE (t), s, depth + 1);
          break;
        }
      
      case POINTER_TYPE:
      case REFERENCE_TYPE:
        {
          store_flags (t, s);
          store_type_flags (t, s);
          store_tree (TYPE_SIZE (t), s, depth + 1);
          gpi_contents = chainon (gpi_contents, build_tree_list (NULL_TREE, t));
          store_tree (TREE_TYPE (t), s, depth + 1);
          return;
          /* Pointers and references might cause infinite */
          /* loops in the gpi file otherwise.             */
        }

      case FILE_TYPE:
        {
          /* Since FILE_TYPE is not documented in tree.def, */
          /* I only can guess about its structure.          */
          store_flags (t, s);
          store_type_flags (t, s);
          store_tree (TREE_TYPE (t), s, depth + 1);
          break;
        }

      case ARRAY_TYPE:
        {
          store_tree (TREE_TYPE (t), s, depth + 1);
          store_tree (TYPE_DOMAIN (t), s, depth + 1);
          store_flags (t, s);
          store_type_flags (t, s);
          break;
        }

      case RECORD_TYPE:
      case UNION_TYPE:
        {
          tree f;
          char is_object_type = 0;
          store_flags (t, s);
          store_type_flags (t, s);
          store_tree (TYPE_SIZE (t), s, depth + 1);
          if (IS_OBJECT_TYPE (t))
            {
              is_object_type = 1;
              fwrite (&is_object_type, 1, 1, s);
              store_tree (TYPE_LANG_SPECIFIC (t)->elts [0], s, depth + 1);
              store_tree (TYPE_LANG_SPECIFIC (t)->elts [1], s, depth + 1);
            }
          else
            fwrite (&is_object_type, 1, 1, s);
          gpi_contents = chainon (gpi_contents, build_tree_list (NULL_TREE, t));
          for (f = TYPE_FIELDS (t); f; f = TREE_CHAIN (f))
            {
              store_tree (f, s, depth + 1);
              store_tree (DECL_FIELD_BITPOS (f), s, depth + 1);
            }
          store_tree (NULL_TREE, s, depth + 1);  /* end of field decls */
          return;
          /* To save disk space, cut here too -- not */
          /* only at POINTER_TYPE and REFRENCE_TYPE. */
        }

      case FUNCTION_TYPE:
        {
          store_flags (t, s);
          store_type_flags (t, s);
          store_tree (TREE_TYPE (t), s, depth + 1);
          store_tree (TYPE_ARG_TYPES (t), s, depth + 1);
          store_tree (TYPE_SIZE (t), s, depth + 1);
          break;
        }

      case INTEGER_CST:
        {
          fwrite (&TREE_INT_CST_LOW (t), sizeof (HOST_WIDE_INT), 1, s);
          fwrite (&TREE_INT_CST_HIGH (t), sizeof (HOST_WIDE_INT), 1, s);
          store_tree (TREE_TYPE (t), s, depth + 1);
          store_flags (t, s);
          break;
        }

      case REAL_CST:
        {
          store_flags (t, s);
          fwrite (&TREE_REAL_CST (t), sizeof (REAL_VALUE_TYPE), 1, s);
          store_tree (TREE_TYPE (t), s, depth + 1);
          break;
        }

      case COMPLEX_CST:
        {
          store_flags (t, s);
          store_tree (TREE_REALPART (t), s, depth + 1);
          store_tree (TREE_IMAGPART (t), s, depth + 1);
          store_tree (TREE_TYPE (t), s, depth + 1);
          break;
        }

      case STRING_CST:
        {
          store_flags (t, s);
          fwrite (&TREE_STRING_LENGTH (t), sizeof (int), 1, s);
          fwrite (TREE_STRING_POINTER (t), TREE_STRING_LENGTH (t) + 1, 1, s );
          store_tree (TREE_TYPE (t), s, depth + 1);
          break;
        }

      case FUNCTION_DECL:
        {
          store_tree (DECL_NAME (t), s, depth + 1);  /* name of function */
          store_tree (DECL_ASSEMBLER_NAME (t), s, depth + 1);
          store_tree (TREE_TYPE (TREE_TYPE (t)), s, depth + 1);  /* return type */
          store_tree (TYPE_ARG_TYPES (TREE_TYPE (t)), s, depth + 1);  /* argument types */
          break;
        }

      case LABEL_DECL:
      case PARM_DECL:
      case RESULT_DECL:
        {
          store_tree (DECL_NAME (t), s, depth + 1);  /* name of function */
          store_tree (TREE_TYPE (t), s, depth + 1);
          store_flags (t, s);
          break;
        }

      case FIELD_DECL:
        {
          store_flags (t, s);
          store_decl_flags (t, s);
          store_tree (DECL_NAME (t), s, depth + 1);
          store_tree (TREE_TYPE (t), s, depth + 1);
          store_tree (DECL_SIZE (t), s, depth + 1);
          break;
        }

      case CONST_DECL:
      case TYPE_DECL:
        {
          store_flags (t, s);
          store_tree (DECL_NAME (t), s, depth + 1);
          store_tree (TREE_TYPE (t), s, depth + 1);
          store_tree (DECL_INITIAL (t), s, depth + 1);
          break;
        }

      case VAR_DECL:
        {
          store_tree (DECL_NAME (t), s, depth + 1);
          store_tree (DECL_ASSEMBLER_NAME (t), s, depth + 1);
          store_tree (TREE_TYPE (t), s, depth + 1);
          /* don't store DECL_INITIAL */
          break;
        }
    }
  gpi_contents = chainon (gpi_contents, build_tree_list (NULL_TREE, t));
}

/*
 * Load (parts of) a tree (containing a declaration) out of a stream.
 * (A generalized version of this should go into tree.c.)
 */
tree
load_tree (s, depth)
     FILE *s;
     int depth;
{
  unsigned char code;
  tree t = NULL_TREE;

  fread (&code, 1, 1, s);
  if (code == LAST_AND_UNUSED_TREE_CODE)
    {
      if (flag_debug_gpi)
        fprintf (stderr, "loaded (%d): NULL_TREE\n", depth);
      return NULL_TREE;
    }
  push_obstacks_nochange ();
  end_temporary_allocation ();
  switch (code)
    {
      #define LOAD_SPECIAL(n,node)                                \
      case LAST_AND_UNUSED_TREE_CODE + n:                         \
        {                                                         \
          t = node;                                               \
          if (flag_debug_gpi)                                     \
            {                                                     \
              char msg[] = GPC_MAKE_STRING (node);                \
              fprintf (stderr, "loaded (%d): %s\n", depth, msg);  \
            }                                                     \
          return t;                                               \
        }

      LOAD_SPECIAL (1, error_mark_node);
      LOAD_SPECIAL (2, short_integer_type_node);
      LOAD_SPECIAL (3, integer_type_node);
      LOAD_SPECIAL (4, long_integer_type_node);
      LOAD_SPECIAL (5, long_long_integer_type_node);
      LOAD_SPECIAL (6, short_unsigned_type_node);
      LOAD_SPECIAL (7, unsigned_type_node);
      LOAD_SPECIAL (8, long_unsigned_type_node);
      LOAD_SPECIAL (9, long_long_unsigned_type_node);
      LOAD_SPECIAL (10, boolean_type_node);
      LOAD_SPECIAL (11, boolean_false_node);
      LOAD_SPECIAL (12, boolean_true_node);
      LOAD_SPECIAL (13, ptrdiff_type_node);
      LOAD_SPECIAL (14, unsigned_char_type_node);
      LOAD_SPECIAL (15, signed_char_type_node);
      LOAD_SPECIAL (16, char_type_node);
      LOAD_SPECIAL (17, wchar_type_node);
      LOAD_SPECIAL (18, signed_wchar_type_node);
      LOAD_SPECIAL (19, unsigned_wchar_type_node);
      LOAD_SPECIAL (20, float_type_node);
      LOAD_SPECIAL (21, double_type_node);
      LOAD_SPECIAL (22, long_double_type_node);
      LOAD_SPECIAL (23, complex_integer_type_node);
      LOAD_SPECIAL (24, complex_float_type_node);
      LOAD_SPECIAL (25, complex_double_type_node);
      LOAD_SPECIAL (26, complex_long_double_type_node);
      LOAD_SPECIAL (27, intQI_type_node);
      LOAD_SPECIAL (28, intHI_type_node);
      LOAD_SPECIAL (29, intSI_type_node);
      LOAD_SPECIAL (30, intDI_type_node);
      LOAD_SPECIAL (31, unsigned_intQI_type_node);
      LOAD_SPECIAL (32, unsigned_intHI_type_node);
      LOAD_SPECIAL (33, unsigned_intSI_type_node);
      LOAD_SPECIAL (34, unsigned_intDI_type_node);
      LOAD_SPECIAL (35, void_type_node);
      LOAD_SPECIAL (36, ptr_type_node);
      LOAD_SPECIAL (37, const_ptr_type_node);
      LOAD_SPECIAL (38, string_type_node);
      LOAD_SPECIAL (39, const_string_type_node);
      LOAD_SPECIAL (40, char_array_type_node);
      LOAD_SPECIAL (41, int_array_type_node);
      LOAD_SPECIAL (42, wchar_array_type_node);
      LOAD_SPECIAL (43, default_function_type);
      LOAD_SPECIAL (44, double_ftype_double);
      LOAD_SPECIAL (45, double_ftype_double_double);
      LOAD_SPECIAL (46, int_ftype_int);
      LOAD_SPECIAL (47, long_ftype_long);
/*
      LOAD_SPECIAL (48, float_ftype_float);
      LOAD_SPECIAL (49, ldouble_ftype_ldouble);
*/
      LOAD_SPECIAL (50, void_ftype_ptr_ptr_int);
      LOAD_SPECIAL (51, int_ftype_ptr_ptr_int);
      LOAD_SPECIAL (52, void_ftype_ptr_int_int);
      LOAD_SPECIAL (53, string_ftype_ptr_ptr);
      LOAD_SPECIAL (54, int_ftype_string_string);
      LOAD_SPECIAL (55, int_ftype_cptr_cptr_sizet);
      LOAD_SPECIAL (56, integer_zero_node);
      LOAD_SPECIAL (57, null_pointer_node);
      LOAD_SPECIAL (58, integer_one_node);

      #undef LOAD_SPECIAL

      case 0xFF:
        {
          tree c = gpi_contents;
          int count;
          fread (&count, sizeof (int), 1, s);
          while (count-- && c)
            c = TREE_CHAIN (c);
          if (!c)
            {
              error ("invalid gpi file");
              t = error_mark_node;
            }
          else
            t = TREE_VALUE (c);

          if (flag_debug_gpi)
            {
              fprintf (stderr, "loaded (%d, via gpi_contents):\n", depth);
              debug_tree (t);
            }

          return t;
        }

      case IDENTIFIER_NODE:
        {
          char id [256];
          load_string (s, id);
          t = get_identifier (id);
          load_flags (t, s);
          if (depth == 0)
            IDENTIFIER_GLOBAL_VALUE (t) = load_tree (s, depth + 1);
          break;
        }

      case TREE_LIST:
        {
          tree value, chain;
          value = load_tree (s, depth + 1);
          chain = load_tree (s, depth + 1);
          t = chainon (build_tree_list (NULL_TREE, value), chain);
          load_flags (t, s);
          break;
        }

      case VOID_TYPE:
      case REAL_TYPE:
      case COMPLEX_TYPE:
      case ENUMERAL_TYPE:
      case BOOLEAN_TYPE:
      case CHAR_TYPE:
      case SET_TYPE:
      case LANG_TYPE:
        {
          t = make_node (code);
          load_flags (t, s);
          load_type_flags (t, s);
          TYPE_SIZE (t) = load_tree (s, depth + 1);
          break;
        }

      case INTEGER_TYPE:
        {
          t = make_node (code);
          load_flags (t, s);
          load_type_flags (t, s);
          TYPE_SIZE (t) = load_tree (s, depth + 1);
          TYPE_MIN_VALUE (t) = load_tree (s, depth + 1);
          TYPE_MAX_VALUE (t) = load_tree (s, depth + 1);
          break;
        }
      
      case POINTER_TYPE:
      case REFERENCE_TYPE:
        {
          t = make_node (code);
          load_flags (t, s);
          load_type_flags (t, s);
          TYPE_SIZE (t) = load_tree (s, depth + 1);
          gpi_contents = chainon (gpi_contents, build_tree_list (NULL_TREE, t));
          TREE_TYPE (t) = load_tree (s, depth + 1);
          return t;
          /* Pointers and references might cause infinite */
          /* loops in the gpi file otherwise.             */
        }

      case FILE_TYPE:
        {
          /* Since FILE_TYPE is not documented in tree.def, */
          /* I only can guess about its structure.          */
          t = make_node (code);
          load_flags (t, s);
          load_type_flags (t, s);
          TREE_TYPE (t) = load_tree (s, depth + 1);
          break;
        }

      case ARRAY_TYPE:
        {
          tree type, domain;
          type = load_tree (s, depth + 1);
          domain = load_tree (s, depth + 1);
          t = build_array_type (type, domain);
          load_flags (t, s);
          load_type_flags (t, s);
          break;
        }

      case RECORD_TYPE:
      case UNION_TYPE:
        {
          tree f, f0 = NULL_TREE;
          tree current_type_name_save = current_type_name;
          char is_object_type;
          t = make_node (code);
          load_flags (t, s);
          load_type_flags (t, s);
          TYPE_SIZE (t) = load_tree (s, depth + 1);
          fread (&is_object_type, 1, 1, s);
          if (is_object_type)
            {
              TYPE_LANG_SPECIFIC (t) = allocate_type_lang_specific ();
              TYPE_LANG_SPECIFIC (t)->len = -2;
              current_type_name = load_tree (s, depth + 1);
              TYPE_LANG_SPECIFIC (t)->elts [0] = current_type_name;
              TYPE_LANG_SPECIFIC (t)->elts [1] = load_tree (s, depth + 1);
            }
          gpi_contents = chainon (gpi_contents, build_tree_list (NULL_TREE, t));
          do
            {
              f = load_tree (s, depth + 1);
              if (f)
                {
                  /* Don't use chainon: danger of circularity */
                  /* if t0 was loaded via gpi_contents.       */
                  if (f0)
                    TREE_CHAIN (f0) = f;
                  else
                    TYPE_FIELDS (t) = f;
                  /* Delete possibly invalid TREE_CHAIN info  */
                  /* if t was loaded via gpi_contents.        */
                  TREE_CHAIN (f) = NULL_TREE;
                  f0 = f;
                  DECL_CONTEXT (f) = t;
                  DECL_FIELD_BITPOS (f) = load_tree (s, depth + 1);
                }
            }
          while (f);
          current_type_name = current_type_name_save;  /* usually NULL_TREE */
          return t;
          /* To save disk space, cut here too -- not */
          /* only at POINTER_TYPE and REFRENCE_TYPE. */
        }

      case FUNCTION_TYPE:
        {
          t = make_node (code);
          load_flags (t, s);
          load_type_flags (t, s);
          TREE_TYPE (t) = load_tree (s, depth + 1);
          TYPE_ARG_TYPES (t) = load_tree (s, depth + 1);
          TYPE_SIZE (t) = load_tree (s, depth + 1);
          break;
        }

      case INTEGER_CST:
        {
          t = make_node (code);
          fread (&TREE_INT_CST_LOW (t), sizeof (HOST_WIDE_INT), 1, s);
          fread (&TREE_INT_CST_HIGH (t), sizeof (HOST_WIDE_INT), 1, s);
          TREE_TYPE (t) = load_tree (s, depth + 1);
          load_flags (t, s);
          break;
        }

      case REAL_CST:
        {
          t = make_node (code);
          load_flags (t, s);
          fread (&TREE_REAL_CST (t), sizeof (REAL_VALUE_TYPE), 1, s);
          TREE_TYPE (t) = load_tree (s, depth + 1);
          break;
        }

      case COMPLEX_CST:
        {
          t = make_node (code);
          load_flags (t, s);
          TREE_REALPART (t) = load_tree (s, depth + 1);
          TREE_IMAGPART (t) = load_tree (s, depth + 1);
          TREE_TYPE (t) = load_tree (s, depth + 1);
          break;
        }

      case STRING_CST:
        {
          t = make_node (code);
          load_flags (t, s);
          fread (&TREE_STRING_LENGTH (t), sizeof (int), 1, s);
          TREE_STRING_POINTER (t) = oballoc (TREE_STRING_LENGTH (t) + 9);
          fread (TREE_STRING_POINTER (t), TREE_STRING_LENGTH (t) + 1, 1, s );
          TREE_TYPE (t) = load_tree (s, depth + 1);
          break;
        }

      case FUNCTION_DECL:
        {
          tree name, full_name, asmname, type, args, d, heading, a, argdecls;
          int ellipsis = 0;

          name = load_tree (s, depth + 1);
          if (current_type_name)
            full_name = get_method_name (current_type_name, name);
          else
            full_name = name;
          asmname = load_tree (s, depth + 1);
          type = load_tree (s, depth + 1);
          args = load_tree (s, depth + 1);

          type = tree_cons (NULL_TREE, type, NULL_TREE);
          type = tree_cons (NULL_TREE, extern_id, type);
          pushlevel (0);
          clear_parm_order ();
          declare_parm_level (1);
          for (a = args; a; a = TREE_CHAIN (a))
            if (TREE_VALUE (a) == void_type_node)
              ellipsis = 1;
            else
              {
                d = build_tree_list (NULL_TREE, TREE_VALUE (a));
                d = build_tree_list (build_tree_list (d,
                                       get_unique_identifier ("parm", 0)),
                                     build_tree_list (NULL_TREE, NULL_TREE));
                push_parm_decl (d);
              }
          argdecls = get_parm_info (ellipsis);
          poplevel (0, 0, 0);
          heading = build_nt (CALL_EXPR, full_name, argdecls, NULL_TREE);
          if (!asmname || full_name == asmname)
            grok_directive (type, heading,
                            build_tree_list (NULL_TREE, d_external), 1);
          else
            {
              DECL_ASSEMBLER_NAME (d_asmname) = 
                build_string (IDENTIFIER_LENGTH (asmname),
                              IDENTIFIER_POINTER (asmname));
              grok_directive (type, heading,
                              build_tree_list (NULL_TREE, d_asmname), 1);
            }
          t = lookup_name (full_name);
          if (current_type_name)
            {
              /* Normally, t will be ignored since grok_directive already
               * has done the job of an external function declaration.  Only
               * if this is a method (i.e. current_type_name != NULL_TREE),
               * a copy of t with `name' instead of `full_name' will go as
               * a field into the object (a RECORD_TYPE node).
               */
              DECL_NAME (t) = name;
            }
          break;
        }

      case LABEL_DECL:
      case PARM_DECL:
      case RESULT_DECL:
        {
          t = make_node (code);
          DECL_NAME (t) = load_tree (s, depth + 1);
          TREE_TYPE (t) = load_tree (s, depth + 1);
          DECL_CONTEXT (t) = current_function_decl;
          load_flags (t, s);
          break;
        }

      case FIELD_DECL:
        {
          t = make_node (code);
          load_flags (t, s);
          load_decl_flags (t, s);
          DECL_NAME (t) = load_tree (s, depth + 1);
          TREE_TYPE (t) = load_tree (s, depth + 1);
          DECL_SIZE (t) = load_tree (s, depth + 1);
          break;
        }

      case CONST_DECL:
      case TYPE_DECL:
        {
          t = make_node (code);
          load_flags (t, s);
          DECL_NAME (t) = load_tree (s, depth + 1);
          TREE_TYPE (t) = load_tree (s, depth + 1);
          DECL_INITIAL (t) = load_tree (s, depth + 1);
          DECL_CONTEXT (t) = current_function_decl;
          break;
        }

      case VAR_DECL:
        {
          /* Re-build the tree node rather than load it */
          tree name, asmname, type;
          name = load_tree (s, depth + 1);
          asmname = load_tree (s, depth + 1);
          type = load_tree (s, depth + 1);
          if (asmname == name)
            declare_vars (build_tree_list (NULL_TREE, name),
                          type, NULL_TREE, NULL_TREE, 1);
          else
            declare_vars (build_tree_list (NULL_TREE, name),
                          type, NULL_TREE,
                          build_tree_list (asmname, extern_id), 1);
          t = lookup_name (name);
          break;
        }
    }
  pop_obstacks ();

  if (flag_debug_gpi && code < LAST_AND_UNUSED_TREE_CODE)
    {
      fprintf (stderr, "loaded (%d):\n", depth);
      debug_tree (t);
    }

  gpi_contents = chainon (gpi_contents, build_tree_list (NULL_TREE, t));
  return t;
}

/*
 * Create gpi files (GNU Pascal Interface) containing
 * precompiled export Interfaces of a Unit or Module
 */
void
create_gpi_files ()
{
  tree escan;
  for (escan = current_module->exports; escan; escan = TREE_CHAIN (escan))
    {
      FILE *gpi_file;
      char *gpi_file_name, *p;
      char gpi_header [] = "GNU Pascal Unit/Module Interface\n";
      tree name = TREE_VALUE (escan), nscan, iscan;

      gpi_file_name = concat (IDENTIFIER_POINTER (name), ".gpi", "");
      for (p = gpi_file_name; *p; p++)
        *p = tolower (*p);
      gpi_file = fopen (gpi_file_name, "wb");
      if (flag_debug_gpi)
        fprintf (stderr, "storing GPI header: %s", gpi_header);
      fwrite (gpi_header, 33, 1, gpi_file);
      if (flag_debug_gpi)
        fprintf (stderr, "storing Module source file name: %s\n", main_input_filename);
      store_string (gpi_file, main_input_filename);
      gpi_contents = NULL_TREE;
      /* Store names of interfaces imported by this module */
      if (flag_debug_gpi)
        fprintf (stderr, "storing names of imported interfaces:\n");
      for (iscan = current_module->imports; iscan;
           iscan = TREE_CHAIN (iscan))
        {
          /* Store them as strings, not as tree nodes. */
          /* We don't yet want to use the gpi_contents mechanism. */
          tree iname = TREE_VALUE (iscan);
          if (!iname || TREE_CODE (iname) != IDENTIFIER_NODE)
            abort ();
          if (flag_debug_gpi)
            fprintf (stderr, "  %s\n", IDENTIFIER_POINTER (iname));
          store_string (gpi_file, IDENTIFIER_POINTER (iname));
        }
      store_string (gpi_file, "");
      store_tree (TREE_VALUE (escan), gpi_file, 0);
      for (nscan = TREE_PURPOSE (escan); nscan; nscan = TREE_CHAIN (nscan))
        if (TREE_VALUE (nscan))  /* may be NULL_TREE with autoexports */
          store_tree (TREE_VALUE (nscan), gpi_file, 0);
      store_tree (NULL_TREE, gpi_file, 0);
      fclose (gpi_file);
    }
}

/*
 * Open a gpi file for reading and process the header.
 * Do an AutoMake, if necessary and/or requested.
 * The file is closed via a normal fclose ().
 */
FILE *
gpi_open (name, depth)
     char *name;
     int depth;
{
  FILE *s = NULL;
  char module_filename [256];
  if (access (name, 4) == 0)
    s = fopen (name, "rb");
  if (s)
    {
      char gpi_loaded_string [256];
      tree import_list = NULL_TREE;

      fread (gpi_loaded_string, 33, 1, s);
      gpi_loaded_string [33] = 0;
      if (flag_debug_gpi)
        fprintf (stderr, "loaded GPI header: %s", gpi_loaded_string);
      load_string (s, module_filename);
      if (flag_debug_gpi)
        {
          fprintf (stderr, "loaded Module source file name: %s\n",
                           module_filename);
          fprintf (stderr, "loaded names of imported interfaces:\n");
        }
      do
        {
          /* Store names of imported interfaces for later use */
          load_string (s, gpi_loaded_string);
          if (gpi_loaded_string [0])
            {
              if (flag_debug_gpi)
                fprintf (stderr, "  %s\n", gpi_loaded_string);
              import_list = chainon (import_list,
                                     build_tree_list (NULL_TREE,
                                       get_identifier (gpi_loaded_string)));
            }
        }
      while (gpi_loaded_string [0]);
      if (flag_automake && depth == 0)
        if (module_must_be_recompiled (name, module_filename))
          {
            fclose (s);
            if (flag_debug_gpi)
              fprintf (stderr, "recompiling: %s --> %s\n",
                               module_filename, name);
            compile_module (module_filename);
            /* Module has been compiled (or not).  Reload the GPI file. */
            s = gpi_open (name, depth + 1);
          }
        else
          {
            /* No recompilation needed.  Also check modules used by this one. */
            FILE *u;
            tree imported, i_name;
            char *u_name, *p;

            for (imported = import_list; imported;
                 imported = TREE_CHAIN (imported))
              {
                i_name = TREE_VALUE (imported);
                u_name = concat (IDENTIFIER_POINTER (i_name), ".gpi", "");
                for (p = u_name; *p; p++)
                  *p = tolower (*p);
                u = gpi_open (u_name, 0);
                if (u)
                  fclose (u);
              }
          }
    }
  else if (flag_automake && depth == 0)
    {
      /* GPI file not found.  Try to compile the module by deriving
       * the file name of its source from the GPI file name.
       */
      char *p;
      strcpy (module_filename, name);
      /* Cut the extension `.gpi' */
      p = module_filename + strlen (module_filename);
      do
        p--;
      while (p >= module_filename && *p != '.');
      *p = 0;
      strcat (module_filename, ".p");  /* First, try extension `.p' */
      if (compile_module (module_filename) != 0)
        {
          strcat (module_filename, "as");  /* Now try extension `.pas' */
          compile_module (module_filename);
        }
      /* Module has been compiled (or not). */
      /* Try again to load the GPI file.    */
      s = gpi_open (name, depth + 1);
    }
  return s;
}

/*
 * Try to load a gpi file and to extract an exported module interface.
 */
tree
load_gpi_file (name)
     char *name;
{
  FILE *gpi_file;
  char *gpi_file_name, *p;
  tree export;
  we_are_loading_a_gpi_file = 1;

  gpi_file_name = concat (name, ".gpi", "");
  for (p = gpi_file_name; *p; p++)
    *p = tolower (*p);
  gpi_file = gpi_open (gpi_file_name, 0);
  if (gpi_file)
    {
      tree value, purpose, name;
      gpi_contents = NULL_TREE;
      value = load_tree (gpi_file, 0);
      purpose = NULL_TREE;
      do
        {
          name = load_tree (gpi_file, 0);
          if (name)
            purpose = chainon (purpose, build_tree_list (NULL_TREE, name));
        }
      while (name);
      fclose (gpi_file);
      export = build_tree_list (purpose, value);
    }
  else
    export = NULL_TREE;

  we_are_loading_a_gpi_file = 0;
  return export;
}



/*
 * NAME is an IDENTIFIER_NODE of the interface name
 * IMPORT_QUALIFIER:
 *     NULL_TREE if no qualifiers given,
 *     TREE_LIST:
 *       TREE_PURPOSE:
 *           NULL_TREE  -> no restricted_import_option given
 *           !NULL_TREE -> restricted_import_option (== ONLY) given
 *	 TREE_VALUE:
 *           TREE_LIST
 *             TREE_PURPOSE: imported <name> from the interface
 *             TREE_VALUE:
 *		   NULL_TREE: name has not been renamed
 *		   identifier_node: new name of the renamed <name>
 * ONLY_QUALIFIED is 0 if unqualified references are allowed;
 *                   1 if qualified references are mandatory.
 */
void
import_interface (interface, import_qualifier, only_qualified)
     tree interface;
     tree import_qualifier;
     long only_qualified;
{
  tree exported_name_list;
  tree exported;

  current_module->imports = chainon (current_module->imports,
				     build_tree_list (NULL_TREE,
						      interface));

  if (standard_interface_input == interface)
    {
      current_module->input_file_node = global_input_file_node;
      exported_name_list = build_tree_list (NULL_TREE, identifier_input);
    }
  else if (standard_interface_output == interface)
    {
      current_module->output_file_node = global_output_file_node;
      exported_name_list = build_tree_list (NULL_TREE, identifier_output);
    }
  else
    {
      for (exported = exported_interface_list;
	   exported;
	   exported = TREE_CHAIN (exported))
	if (TREE_VALUE (exported) == interface)
	  break;

      if (! exported)
        {
          exported = load_gpi_file (IDENTIFIER_POINTER (interface));
          if (exported)
            exported_interface_list = chainon (exported_interface_list,
                                               exported);
          else
	    {
	      error ("No exported interface matching `%s'",
		     IDENTIFIER_POINTER (interface));
	      return;
	    }
        }

      exported_name_list = TREE_PURPOSE (exported);
    }

  if (only_qualified)
    warning ("QUALIFIED not yet supported; it is ignored");

  /* EXPORTED is now the correct interface */
  if (import_qualifier)
    {
      tree inames = TREE_VALUE (import_qualifier);
      if (TREE_PURPOSE (import_qualifier))
	warning ("Restricted import option `ONLY' not yet supported; it is ignored");
      for (; inames; inames = TREE_CHAIN (inames))
	{
	  tree in = TREE_PURPOSE (inames);
	  tree echeck = exported_name_list;
	  int found = FALSE;

	  if (TREE_VALUE (inames))
	    warning ("Renaming of imported interface `%s' to `%s' is not yet supported",
		     IDENTIFIER_POINTER (in), IDENTIFIER_POINTER (TREE_VALUE (inames)));
	  for (; !found && echeck; echeck = TREE_CHAIN (echeck))
	    if (TREE_VALUE (echeck) == in)
	      found = TRUE;

	  if (! found)
	    error ("Interface `%s' does not export `%s'",
		   IDENTIFIER_POINTER (interface), IDENTIFIER_POINTER (in));
	}
    }
}

/*
 * Only exportable names allowed as the NAME here:
 *	  constant_name
 *	| type_name
 *	| schema_name
 *	| procedure_name
 *	| function_name
 *	| variable_name
 *	| PROTECTED variable_name
 */
tree
module_export_clause (name, renamed, protected)
     tree name;
     tree renamed;
     int  protected;
{
  tree rval = build_tree_list (renamed, name);

  if (protected)
    warning ("PROTECTED exports are not yet implemented; handled as a normal exports");

  return rval;
}

tree
module_export_range (low, high)
     tree low;
     tree high;
{
  /* @@@@@@ exported ranges are not yet supported */
  warning ("Exported ranges are not yet supported; ABORTING");
  abort ();
}

