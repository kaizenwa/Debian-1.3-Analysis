/* symctl.c controls which symbols are exported (global) 
   changes by Tristan Gingold (C) 1993, 1994, 1995 */
/* strip certain symbols from a rel file.
   Copyright (C) 1986, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>

#include <fcntl.h>
#include <string.h>
#include <elf.h>

/* Some systems, such as Linux, don't define that ! */
#ifndef ELF32_ST_INFO
#define ELF32_ST_INFO(b,t) (((b) << 4) + ((t) & 0xf))
#endif

/* Number of files specified in the command line. */
int number_of_files;

/* The exit status */
int errs;

/* Each specified file has a file_entry structure for it.
   These are contained in the vector which file_table points to.  */

struct file_entry
{
  char *filename;
  unsigned int file_size;
  unsigned int mapped;

  /* Things obtained from the file's header.  */
  unsigned char *contents;
  unsigned int shnum;
  Elf32_Shdr *shdr;
  Elf32_Ehdr *hdr;
  char *shstr;
  
  char *symstr[2];
  Elf32_Sym *symtab[2];
  unsigned int nbrsym[2];
};

struct file_entry *file_table;

struct symbol_h
{
  char *name;
  int action;
  struct symbol_h *next;
};

struct symbol_h *exports;

/* Descriptor on which current file is open.  */
int input_desc;

/* Stream for writing that file using stdio.  */
FILE *outstream;

/* export only those symbols */
#define UNCHANGE 0
#define EXPORT 1
#define HIDE 2
#define NOTDEF 3
int default_exp = HIDE;
int allow_debug;
int verbose;
int setcheckerflag;

int scramble_flag;

/* The name this program was run with. */
char *program_name;

char *xmalloc ();
int file_open ();
int read_entry_symbols ();
int read_file_symbols ();
int read_header ();
void count_file_symbols ();
void error (int status, int errnum, char *message, ...);
void file_close ();
void rewrite_file_symbols ();
void strip_file ();
void usage ();
void read_rules(char *);
int is_sym_exported(char *sym, int defined);

void
main (int argc, char *argv[])
{
  int c;
  int i;
  struct file_entry *p;

  program_name = argv[0];
  errs = 0;
  exports = NULL;
  scramble_flag = 0;

  while ((c = getopt (argc, argv, "vs")) != EOF) 
    {
      switch (c)
	{
	case  0 :
	  break;
	case 'v':
	  verbose = 1;
	  break;
	case 's':
	  scramble_flag = 1;
	  break;
	default:
	  usage ();
	}
    }

  /* Read the export rules file */
  if (argc > optind)
    {
      read_rules (argv[optind]);
      optind++;
    }

  number_of_files = argc - optind;
  
  if (!number_of_files)
    usage ();
  
  p = file_table = (struct file_entry *) xmalloc (number_of_files * sizeof (struct file_entry));

  /* Now fill in file_table */
  for (i = 0; i < number_of_files; i++)
    {
       p->filename = argv[optind + i];
       p->contents = 0;
       p++;
    }

  for (i = 0; i < number_of_files; i++)
    strip_file (&file_table[i]);
  
  if (errs)
    exit (1);
  else  
    exit (0);
}

int delayed_signal;

void
delay_signal (int signo)
{
  delayed_signal = signo;
  signal (signo, delay_signal);
}

/* process one input file */

void
strip_file (struct file_entry *entry)
{
  int val;
  int sigint_handled = 0;
  int sighup_handled = 0;
  int sigterm_handled = 0;

  val = file_open (entry);
  if (val < 0)
    return;

  /* Effectively defer handling of asynchronous kill signals.  */
  delayed_signal = 0;
  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    sigint_handled = 1, signal (SIGINT, delay_signal);
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    sighup_handled = 1, signal (SIGHUP, delay_signal);
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    sigterm_handled = 1, signal (SIGTERM, delay_signal);

  /* Change the file.  */
  if (entry->symtab[0])
    rewrite_file_symbols (0, entry);
  if (entry->symtab[1])
    rewrite_file_symbols (1, entry);

  file_close (entry);

  /* Effectively undefer handling.  */
  if (sigint_handled)
    signal (SIGINT, SIG_DFL);
  if (sighup_handled)
    signal (SIGHUP, SIG_DFL);
  if (sigterm_handled)
    signal (SIGTERM, SIG_DFL);

  /* Handle any signal that came in while they were deferred.  */
  if (delayed_signal)
    kill (getpid (), delayed_signal);
}

/** Convenient functions for operating on one or all files being processed.  */

/* Close the file that is now open.  */

void
file_close (struct file_entry *entry)
{
  if (input_desc != -1)
    {
      if (entry->mapped)
        munmap (entry->contents, entry->file_size);
      else
        {
          lseek (input_desc, 0, SEEK_SET);
          if (write (input_desc, entry->contents, entry->file_size) != entry->file_size)
            error (0, errno, "%s", entry->filename);
        }
      close (input_desc);
    }
  input_desc = -1;
}

/* Open the file specified by 'entry', and return a descriptor,
   or -1 if the file cannot be opened or is not in rel format.
   The descriptor is also saved in input_desc.  */

int
file_open (struct file_entry *entry)
{
  int desc;

  desc = open (entry->filename, O_RDWR, 0);

  if (desc > 0)
    {
      input_desc = desc;
      if (read_header (desc, entry) < 0)
	{
	  close (desc);
	  return -1;
	}
      return desc;
    }

  error (0, errno, "%s", entry->filename);
  return -1;
}


/* Read a file's header and fill in various fields of a file's entry.
   Return -1 on failure, 0 if successful.  */

int
read_header (int desc, struct file_entry *entry)
{
  struct stat st;
  int i;
  
  if (fstat (desc, &st) != 0)
    {
      error (0, 0, "%s: cannot stat", entry->filename);
      return -1;
    }
  entry->file_size = st.st_size;
  entry->contents = mmap (0, st.st_size, PROT_READ | PROT_WRITE, MAP_SHARED, desc, 0);
  if (entry->contents == (unsigned char*)-1)
    {
      entry->contents = xmalloc (st.st_size);
      if (read (desc, entry->contents, st.st_size) != st.st_size)
        {
          error (0, 0, "%s: can neither mmap nor malloc", entry->filename);
          return -1;
        }
      entry->mapped = 0;
    }
  else
    entry->mapped = 1;
  entry->hdr = (Elf32_Ehdr*) entry->contents;
  entry->symtab[0] = entry->symtab[1] = (Elf32_Sym* )0;
  if (!strncmp (entry->hdr->e_ident, ELFMAG, SELFMAG))
    {
      entry->shnum = entry->hdr->e_shnum;
      entry->shdr = (Elf32_Shdr*) (entry->contents + entry->hdr->e_shoff);
      entry->shstr = entry->contents + entry->shdr[entry->hdr->e_shstrndx].sh_offset;
      for (i = 0; i < entry->shnum; i++)
        switch (entry->shdr[i].sh_type)
          {
        case SHT_SYMTAB:
          if (!strcmp (".symtab", entry->shstr + entry->shdr[i].sh_name))
            {
              entry->symtab[0] = (Elf32_Sym*) (entry->contents + entry->shdr[i].sh_offset);
              entry->symstr[0] = entry->contents + entry->shdr[entry->shdr[i].sh_link].sh_offset;
              entry->nbrsym[0] = entry->shdr[i].sh_size / entry->shdr[i].sh_entsize;
            }
          break;
        case SHT_DYNSYM:
          if (!strcmp (".dynsym", entry->shstr + entry->shdr[i].sh_name))
            {
              entry->symtab[1] = (Elf32_Sym*) (entry->contents + entry->shdr[i].sh_offset);
              entry->symstr[1] = entry->contents + entry->shdr[entry->shdr[i].sh_link].sh_offset;
              entry->nbrsym[1] = entry->shdr[i].sh_size / entry->shdr[i].sh_entsize;
            }
          break;
        default:
          }
	return 0;
    }

  error (0, 0, "%s: not an executable or object file", entry->filename);
  return -1;
}

/* Copy into NEWSYMS the symbol entries to be preserved.
   Count them in sym_written_count.

   We record, for each symbol written, its symbol number in the resulting file.
   This is so that the relocation can be updated later.
   Since the symbol names will not be needed again,
   this index goes in the `n_strx' field.
   If a symbol is not written, -1 is stored there.  */

void
rewrite_file_symbols (int n, struct file_entry *entry)
{
  Elf32_Sym *sym;

  sym = entry->symtab[n];
  for (sym = entry->symtab[n]; sym < entry->symtab[n] + entry->nbrsym[n]; sym++)
    if (   ELF32_ST_TYPE(sym->st_info) == STT_OBJECT
        || ELF32_ST_TYPE(sym->st_info) == STT_FUNC
        /* ||  ELF32_ST_TYPE(sym->st_info) == STT_NOTYPE*/ )
      {
        /* Don't change the common symbols.  */
        if (sym->st_shndx == SHN_COMMON)
          continue;
        switch (is_sym_exported (entry->symstr[n] + sym->st_name, 1))
          {
          case HIDE:
            if (ELF32_ST_BIND (sym->st_info) == STB_GLOBAL)
              {
                if (scramble_flag)
                  {
                    char *name = entry->symstr[n] + sym->st_name;
                    name[0] = '$';	/* Why not */
                  }
                else
                  sym->st_info = ELF32_ST_INFO(STB_LOCAL, ELF32_ST_TYPE(sym->st_info));
              }
            break;
          case EXPORT:
            if (ELF32_ST_BIND (sym->st_info) == STB_LOCAL)
              sym->st_info = ELF32_ST_INFO (STB_GLOBAL, ELF32_ST_TYPE(sym->st_info));
            break;
          case UNCHANGE:
            break;
          default:
          }
      }
    else if (ELF32_ST_TYPE(sym->st_info) == STT_NOTYPE
             && sym->st_shndx == SHN_UNDEF
             && sym->st_name)
      {
        if (n == 0 && is_sym_exported (entry->symstr[n] + sym->st_name, 0) != NOTDEF)
          {
            fprintf(stderr, "warning: %s is not registered as an undefined symbol.\n", entry->symstr[n] + sym->st_name);
            errs++;
          }
      }
}

/* Like malloc but get fatal error if memory is exhausted.  */

char *
xmalloc (unsigned size)
{
  /* Some implementations of malloc get unhappy if size==0.
   * Given that the code sometimes "wants" a 0-length array,
   * it seems cleaner to put a work-around here
   * than clutter up the code logic in various other places. */
  char *result = malloc (size==0 ? 1 : size);

  if (!result)
    error (1, 0, "virtual memory exhausted");
  return result;
}

char *
symbol_dup (char *s)
{
  char *res;
  
  res = xmalloc (strlen (s) + 1);
  if (*s == '*')
    strcpy (res, s + 1);
  else
    strcpy (res, s);
  return res;
}

/* read the rules file. The grammar is simple:
   Each line can be either a comment (an '#' at the first col) or a control
   line.
   A control line is:
    export sym
    hide sym
    notdef sym
    end
    default is export
    default is hide
    default is unchange
   where 'sym' is a symbol name
 */
void
read_rules (char *filename)
{
 FILE *file;
 int i,len;
 static char line[2048];
 int argc;
 char *argv[5];
 struct symbol_h *ptr;
 
 if ((file = fopen (filename,"r")) == (FILE*)0)
   error (1, errno, "Can't open %s", filename);   
   
 while (fgets (line,2048,file) != NULL)
   {
     /* skipf comment line */
     if (line[0] == '#')
       continue;
     /* parse line */
     argc=0;
     len = strlen (line);
     for (i = 0; i < len;)
       {
         /* skipf blanks */
         if (line[i] == ' ' || line[i] == '\n')
           {
             i++;
             continue;
           }
         argv[argc++] = &line[i];
         /* search the end of the arg */
         while (line[i] != '\0' && line[i] != ' ' && line[i] != '\n')
           i++;
         /* end of the arg */
         line[i++] = '\0';
       }
     if (*argv[0] == '\n')
       continue;
     if (argc == 2 && strcmp (argv[0], "export") == 0)
       {
         ptr = (struct symbol_h*) malloc (sizeof (struct symbol_h));
         ptr->name = symbol_dup (argv[1]);
         ptr->action = EXPORT;
         ptr->next = exports;
         exports = ptr;
         continue;
       }
     if (argc == 2 && strcmp (argv[0], "hide") == 0)
       {
         ptr = (struct symbol_h*) malloc (sizeof (struct symbol_h));
         ptr->name = symbol_dup (argv[1]);
         ptr->action = HIDE;
         ptr->next = exports;
         exports = ptr;
         continue;
       }
     if (argc == 2 && strcmp (argv[0], "notdef") == 0)
       {
         ptr = (struct symbol_h*) malloc (sizeof (struct symbol_h));
         ptr->name = symbol_dup (argv[1]);
         ptr->action = NOTDEF;
         ptr->next = exports;
         exports = ptr;
         continue;
       }
     if (argc == 3 && strcmp (argv[0], "allow") == 0 
   		 && strcmp (argv[1], "debug") == 0
   		 && strcmp (argv[2], "symbols")==0)
       {
         allow_debug=1;
         continue;
       }
     if (argc == 3 && strcmp (argv[0], "default") == 0
   		 && strcmp (argv[1], "is") == 0)
       {
         if (strcmp (argv[2], "hide") == 0)
           {
             default_exp = HIDE;
             continue;
           }
         if (strcmp (argv[2], "export") == 0)
           {
             default_exp = EXPORT;
             continue;
           }
         if (strcmp (argv[2], "unchange") == 0)
           {
             default_exp = UNCHANGE;
             continue;
           }
         error (0, 0, "Default mode '%s' is unknown", argv[2]);
       }
     if (argc == 2 
         && strcmp (argv[0], "set") == 0
         && strcmp (argv[1], "checkerflag") == 0)
       continue;
     if (argc == 1 && strcmp (argv[0], "end") == 0)
       break;
     error (0, 0, "Unknown command: %s %s", argv[0], argv[1]);
   }
 fclose (file);
}

int
is_sym_exported (char *sym, int defined)
{
 struct symbol_h *ptr;
 char res;

 if (sym == (char*)0 || *sym == '\0')
   return UNCHANGE;
    
 if (!defined)
   {
     for (ptr = exports; ptr; ptr = ptr->next)
       {
         if (strcmp (sym, ptr->name) == 0)
           {
             res = ptr->action;
             if (verbose)
               fprintf (stderr, "%s is found to %d\n", sym, res);
	     return res == NOTDEF ? NOTDEF : UNCHANGE;
           }
       }
     if (verbose)
       fprintf (stderr, "%s is not found\n",sym);
     return UNCHANGE;
   }
 else
   {
     for (ptr = exports; ptr; ptr = ptr->next)
       {
	 if (strcmp (sym, ptr->name) == 0)
	   {
	     res = ptr->action;
	     if (verbose)
	       fprintf (stderr, "%s is found to %d\n", sym, res);
	     return res;
	   }
       }
     if (verbose)
       fprintf (stderr, "%s is not found\n", sym);
     return default_exp;
   }
}

void usage ()
{
  fprintf (stderr, "Usage: %s [-vs] rule-file obj-file\n", program_name);
  exit (1);
}


/* error.c -- error handler for noninteractive utilities
   Copyright (C) 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* David MacKenzie */
/* Heavy destorying by T. Gingold */

/* Print the program name and error message MESSAGE, which is a printf-style
   format string with optional args.
   If ERRNUM is nonzero, print its corresponding system error message.
   Exit with status STATUS if it is nonzero. */
/* VARARGS */
void error (int status, int errnum, char *message, ...)
{
  extern char *program_name;
  va_list args;

  fprintf (stderr, "%s: ", program_name);
  va_start (args, message);
  vfprintf (stderr, message, args);
  va_end (args);
  if (errnum)
    fprintf (stderr, ": %s", strerror (errnum));
  putc ('\n', stderr);
  fflush (stderr);
  if (status)
    exit (status);
}
