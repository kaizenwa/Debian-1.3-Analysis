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
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
extern int errno;
char *sys_errlist[];
extern int optind;
extern char *optarg;

#include <string.h>

#ifdef COFF_ENCAPSULATE
#include "a.out.encap.h"
#else
/* On native BSD systems, use the system's own a.out.h.  */
#include <a.out.h>
#endif

void free(void*);

#ifdef nounderscore
#define LPREFIX '.'
#else
#define LPREFIX 'L'
#endif

#if defined (sun) && defined (sparc)
/* On the sparc, the name of the relocation info structure is
   different (on SunOS4, "struct relocation_info" does not exist).
   The meaning of the r_index field is the same as r_symbolnum
   in normal relocation_info's for external symbols.  Fortunately,
   we only use the field for external symbols.  */
typedef struct reloc_info_sparc *relocation_info_ptr;
#define RELOCATION_INFO_SYMBOL_NUM(ri) (ri)->r_index
#else /* not Sun and sparc.  */
typedef struct relocation_info *relocation_info_ptr;
#define RELOCATION_INFO_SYMBOL_NUM(ri) (ri)->r_symbolnum
#endif /* not Sun and sparc.  */

/* If BSD or HP-UX, we can use `ftruncate' and `rename'.  */
  
#if !defined(USG) || defined(hpux)
#define HAVE_FTRUNCATE
#define HAVE_RENAME
#endif /* !USG || hpux */

struct symbol_h
{
  char *name;
  int action;
  struct symbol_h *next;
};

struct symbol_h *exports;

/* Number of nlist entries that are for local symbols. */
int local_sym_count;

/* Number of nlist entries that are for local symbols
   whose names don't start with L. */
int non_L_local_sym_count;

/* Number of nlist entries for debugger info.  */
int debugger_sym_count;

/* Number of global symbols referenced or defined.  */
int global_sym_count;

/* Total number of symbols to be preserved in the current file.  */
int nsyms;

/* Number of files specified in the command line. */
int number_of_files;

/* Kinds of files understood.  */
enum file_type { IS_UNKNOWN, IS_A_OUT };

/* Each specified file has a file_entry structure for it.
   These are contained in the vector which file_table points to.  */

struct file_entry
{
  char *filename;
  enum file_type filetype;	/* what kind of file it is */

  /* Things obtained from the file's header.  */
  long int trel_offset;		/* offset to text relocation */
  unsigned int trel_size;	/* size of text relocation */
  long int drel_offset;		/* offset to data relocation */
  unsigned int drel_size;	/* size of data relocation */
  long int syms_offset;		/* offset to the symbol table */
  unsigned int syms_size;	/* size of the symbol table */
  long int strs_offset;		/* offset to the string table */
  unsigned int strs_size;	/* size of the string table */

  int ss_size;			/* size, in bytes, of symbols_and_strings data */
  struct nlist *symbols_and_strings;

  /* offset of the symtab_command in a mach-O file's header */
  long int symtab_cmd_offset;
};

struct file_entry *file_table;

/* Descriptor on which current file is open.  */
int input_desc;

/* Stream for writing that file using stdio.  */
FILE *outstream;

enum locals_action
{
  locals_undef,
  locals_start_L,		/* discard locals starting with L */
  locals_all			/* discard all locals */
};

/* Which local symbols to remove. */
enum locals_action discard_locals;

/* export only those symbols */
#define UNCHANGE 0
#define EXPORT 1
#define HIDE 2
#define NOTDEF 3
int nbr_T_export;
char **T_export;
int default_exp = HIDE;
int allow_debug;
int verbose;
int chkrstrip = 0;
char *symbol_format = "_%s";
#ifdef CHECKERFLAG
int setcheckerflag;
#endif

/* The name this program was run with. */
char *program_name;

char *malloc ();

char *concat ();
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
int is_sym_exported(struct nlist *n, char *sym);


void main (int argc, char *argv[])
{
  int c;
  int i;
  struct file_entry *p;

  program_name = argv[0];

  discard_locals = locals_undef;
  exports = NULL;

  while ((c = getopt (argc, argv, "f:cvxX")) != EOF) 
    {
      switch (c)
	{
	case  0 :
	  break;
	case 'x':
	  discard_locals = locals_all;
	  break;
	case 'X':
	  discard_locals = locals_start_L;
	  break;
	case 'v':
	  verbose = 1;
	  break;
	case 'c':
	  chkrstrip = 1;
	  break;
	case 'f':
	  symbol_format = optarg;
	  break;
	default:
	  usage ();
	}
    }

  /* Read the export rules file */
  if (!chkrstrip && argc > optind)
    {
      read_rules(argv[optind]);
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
       p->filetype = IS_UNKNOWN;
       p->trel_offset = p->trel_size = 0;
       p->drel_offset = p->drel_size = 0;
       p->syms_offset = p->syms_size = 0;
       p->strs_offset = p->strs_size = 0;
       p->symbols_and_strings = 0;
       p->symtab_cmd_offset = 0;
       p++;
    }

  for (i = 0; i < number_of_files; i++)
    strip_file (&file_table[i]);
  exit (0);
}

int delayed_signal;

void delay_signal (int signo)
{
  delayed_signal = signo;
  signal (signo, delay_signal);
}

/* process one input file */

void strip_file (struct file_entry *entry)
{
  int val;
  int sigint_handled = 0;
  int sighup_handled = 0;
  int sigterm_handled = 0;

  local_sym_count = 0;
  non_L_local_sym_count = 0;
  debugger_sym_count = 0;
  global_sym_count = 0;

  val = file_open (entry);
  if (val < 0)
    return;

  /* Read in the existing symbols unless we are discarding everything.  */
  if (read_file_symbols (entry) < 0)
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

  rewrite_file_symbols (entry);
  free (entry->symbols_and_strings);

  file_close ();

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

void file_close ()
{
  if (input_desc != -1)
    close (input_desc);
  input_desc = -1;
}

/* Open the file specified by 'entry', and return a descriptor,
   or -1 if the file cannot be opened or is not in rel format.
   The descriptor is also saved in input_desc.  */

int file_open (struct file_entry *entry)
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

/* Validate file ENTRY and read its symbol and string sections into core.
   Return 0 if ok, -1 if error. */

int read_file_symbols (struct file_entry* entry)
{
  if (read_entry_symbols (input_desc, entry) < 0)
    {
      file_close ();
      return -1;
    }
  count_file_symbols (entry);
  return 0;
}

/* Read a file's header and fill in various fields of a file's entry.
   Return -1 on failure, 0 if successful.  */

int read_header (int desc, struct file_entry *entry)
{
  int len;

  {
    struct exec hdr;

    lseek (desc, 0, 0);
#ifdef HEADER_SEEK_FD
    /* Skip the headers that encapsulate our data in some other format
       such as COFF.  */
    HEADER_SEEK_FD (desc);
#endif
    len = read (desc, (char *) &hdr, sizeof (struct exec));
    if (len == sizeof (struct exec) && !N_BADMAG (hdr))
      {
	entry->filetype = IS_A_OUT;
#ifdef N_TRELOFF
	entry->trel_offset = N_TRELOFF (hdr);
#else
#ifdef N_DATOFF
	entry->trel_offset = N_DATOFF (hdr) + hdr.a_data;
#else
	entry->trel_offset = N_TXTOFF (hdr) + hdr.a_text + hdr.a_data;
#endif
#endif
	entry->trel_size = hdr.a_trsize;
#ifdef N_DRELOFF
	entry->drel_offset = N_DRELOFF (hdr);
#else
	entry->drel_offset = entry->trel_offset + entry->trel_size;
#endif
	entry->drel_size = hdr.a_drsize;
	entry->syms_offset = N_SYMOFF(hdr);
	entry->syms_size = hdr.a_syms;
	entry->strs_offset = N_STROFF(hdr);
	lseek(desc, entry->strs_offset, 0);
	if (read (desc, (char *) &entry->strs_size, sizeof entry->strs_size)
	    != sizeof entry->strs_size)
	  {
	    error (0, errno, "%s: cannot read string table size",
		   entry->filename);
	    return -1;
	  }
	return 0;
      }
  }

  error (0, 0, "%s: not an executable or object file", entry->filename);
  return -1;
}

/* Read the symbols and strings of file ENTRY into core.
   Assume it is already open, on descriptor DESC.
   Return -1 on failure, 0 if successful.  */

int read_entry_symbols (int desc, struct file_entry *entry)
{
  entry->ss_size = entry->syms_size + entry->strs_size;
  entry->symbols_and_strings = (struct nlist *) xmalloc (entry->ss_size);

  lseek (desc, entry->syms_offset, 0);
  if (entry->ss_size != read (desc, entry->symbols_and_strings, entry->ss_size))
    {
      error (0, errno, "%s: premature end of file in symbols/strings",
	     entry->filename);
      return -1;
    }
  return 0;
}

/* Count the number of symbols of various categories in the file of ENTRY.  */

void count_file_symbols (struct file_entry *entry)
{
  struct nlist *p, *end = entry->symbols_and_strings + entry->syms_size / sizeof (struct nlist);
  char *name_base = entry->syms_size + (char *) entry->symbols_and_strings;

  for (p = entry->symbols_and_strings; p < end; p++)
    if (p->n_type & N_EXT)
      global_sym_count++;
    else if (p->n_un.n_strx && !(p->n_type & (N_STAB | N_EXT)))
      {
	if ((p->n_un.n_strx + name_base)[0] != LPREFIX)
	  non_L_local_sym_count++;
	local_sym_count++;
      }
    else
      debugger_sym_count++;
}

void modify_relocation ();
void write_file_syms ();

/* Total size of string table strings allocated so far */
int strtab_size;

/* Vector whose elements are the strings to go in the string table */
char **strtab_vector;

/* Index in strtab_vector at which the next string will be stored */
int strtab_index;

int sym_written_count;

int assign_string_table_index (char *name)
{
  int index = strtab_size;

  strtab_size += strlen (name) + 1;
  strtab_vector[strtab_index++] = name;

  return index;
}

void rewrite_file_symbols (struct file_entry *entry)
{
  int i;
  struct nlist *newsyms;

  /* Calculate number of symbols to be preserved.  */

  nsyms = global_sym_count;
  if (discard_locals == locals_start_L)
    nsyms += non_L_local_sym_count;
  else if (discard_locals == locals_undef)
   nsyms += local_sym_count;

  nsyms += debugger_sym_count;

  strtab_vector = (char **) xmalloc (nsyms * sizeof (char *));
  strtab_index = 0;

  strtab_size = 4;

  /* Accumulate in 'newsyms' the symbol table to be written.  */

  newsyms = (struct nlist *) xmalloc (nsyms * sizeof (struct nlist));

  sym_written_count = 0;

  /* Write into newsyms the symbols we want to keep.  */
  write_file_syms (entry, newsyms);

  if (sym_written_count != nsyms)
    {
      fprintf (stderr, "written = %d, expected = %d\n",
	       sym_written_count, nsyms);
      abort ();
    }

  /* Modify the symbol-numbers in the relocation in the file,
     to preserve its meaning */
  modify_relocation (input_desc, entry);

#ifndef	HAVE_FTRUNCATE
  {
    int size = entry->syms_offset, mode;
    int old_desc;
    char *renamed;
    char *copy_buffer = (char *)xmalloc (size);
    struct stat statbuf;

    renamed = (char *) xmalloc(strlen(entry->filename) + 3);
    strcpy(renamed, entry->filename);
    {
      char *file_p, *renamed_p;
      file_p = strrchr(entry->filename, '/');
      file_p = file_p ? ++file_p : entry->filename;
      renamed_p = renamed + (file_p - entry->filename);
      *renamed_p++ = '~';
      while (*renamed_p++ = *file_p++)
	;
      *renamed_p++ = '~';
      *renamed_p = '\0';
    }

    lseek (input_desc, 0, 0);
    if (read (input_desc, copy_buffer, size) != size)
      {
	error (0, errno, "%s: cannot read up to symbol table",
	       entry->filename);
	return;
      }
    mode = fstat (input_desc, &statbuf) ? 0666 : statbuf.st_mode;
    if (rename (entry->filename, renamed))
      {
	error (0, errno, "%s", entry->filename);
	return;
      }
    old_desc = input_desc;
    input_desc = open (entry->filename, O_RDWR | O_CREAT | O_TRUNC, mode);
    if (input_desc < 0)
      {
	error (0, errno, "%s", entry->filename);
	return;
      }
    if (write (input_desc, copy_buffer, size) != size)
      error (0, errno, "%s", entry->filename);
    if (unlink (renamed))
      error (0, errno, "%s", renamed);
    if (close (old_desc))
      error (0, errno, "%s", renamed);
    free (copy_buffer);
    free (renamed);
  }
#endif /* not HAVE_FTRUNCATE */

  /* Now write contents of NEWSYMS into the file. */

  lseek (input_desc, entry->syms_offset, 0);
  write (input_desc, newsyms, nsyms * sizeof (struct nlist));
  free (newsyms);

  /* Now write the string table.  */

  {
    char *strvec = (char *) xmalloc (strtab_size);
    char *p;

    *((long *) strvec) = strtab_size;

    p = strvec + sizeof (long);

    for (i = 0; i < strtab_index; i++)
      {
	int len = strlen (strtab_vector[i]);
	strcpy (p, strtab_vector[i]);
	*(p+len) = 0;
	p += len + 1;
      }

    write (input_desc, strvec, strtab_size);
    free (strvec);
  }

  /* Adjust file to be smaller */

#ifdef HAVE_FTRUNCATE
  if (ftruncate (input_desc, lseek (input_desc, 0L, 1)) < 0)
    error (0, errno, "%s", entry->filename);
#endif

  /* Write new symbol table size into file header.  */

  if (entry->filetype == IS_A_OUT)
    {
      struct exec hdr;

      lseek (input_desc, 0L, 0);
#ifdef HEADER_SEEK_FD
      HEADER_SEEK_FD (input_desc);
#endif
      read (input_desc, (char *) &hdr, sizeof hdr);
      hdr.a_syms = nsyms * sizeof (struct nlist);
#ifdef CHECKERFLAG
      if (setcheckerflag)
        N_SET_FLAGS(hdr, 0x40);
#endif
      lseek (input_desc, -(long) sizeof hdr, 1);
      write (input_desc, (char *) &hdr, sizeof hdr);
    }

  free (strtab_vector);
}

/* Copy into NEWSYMS the symbol entries to be preserved.
   Count them in sym_written_count.

   We record, for each symbol written, its symbol number in the resulting file.
   This is so that the relocation can be updated later.
   Since the symbol names will not be needed again,
   this index goes in the `n_strx' field.
   If a symbol is not written, -1 is stored there.  */

void write_file_syms (struct file_entry *entry, struct nlist *newsyms)
{
  struct nlist *p = entry->symbols_and_strings;
  struct nlist *end = p + entry->syms_size / sizeof (struct nlist);
  char *string_base = (char *) end;   /* address of start of file's string table */
  struct nlist *outp = newsyms;

  for (; p < end; p++)
    {
      int write=1;

      if (chkrstrip)
        {
          if (p->n_type & N_STAB)
            {
              switch(p->n_type & ~N_EXT)
              {
                case 0x24:	/* N_FUN */
                case 0x44:	/* N_SLINE */
                case 0x64:	/* N_SO */
                case 0x84:	/* N_SOL */
                  write = 1;
                  break;
                default:
                  write = 0;
              }
            }
        }
      else
        {
          if (p->n_type == N_EXT && p->n_value == 0 ) /* imported symbol */
            {
              if (is_sym_exported(p, p->n_un.n_strx + string_base) != NOTDEF)
                fprintf(stderr, "warning: %s is not registered as an undefined symbol.\n",
            			p->n_un.n_strx + string_base);
            }
          else
            if (is_sym_exported(p, p->n_un.n_strx + string_base) == HIDE)
              p->n_type &= ~N_EXT;
        }
     if (write == 0)
       nsyms--;
#if 0
      else if (p->n_un.n_strx && !(p->n_type & (N_STAB | N_EXT)))
	/* ordinary local symbol */
	write = (discard_locals != locals_all)
		&& !(discard_locals == locals_start_L &&
		     (p->n_un.n_strx + string_base)[0] == LPREFIX);
      else
	/* debugger symbol */
	write = 1;
#endif

      if (write)
	{
	  if (p->n_un.n_strx)
	    p->n_un.n_strx = assign_string_table_index (p->n_un.n_strx + string_base);

	  *outp++ = *p;

	  p->n_un.n_strx = sym_written_count++;
	}
      else p->n_un.n_strx = -1;
    }
}

/* Read in ENTRY's relocation, alter the symbolnums in it,
   and write it out again.  */

void modify_relocation (int desc, struct file_entry *entry)
{
  relocation_info_ptr reloc, p, end;
  int size;
  struct nlist *sym_base = (struct nlist *) entry->symbols_and_strings;
  int losing = 0;
  long int offsets[2];
  unsigned int sizes[2];
  int i;

  offsets[0] = entry->trel_offset;
  sizes[0] = entry->trel_size;
  offsets[1] = entry->drel_offset;
  sizes[1] = entry->drel_size;

  for (i = 0; i < 2; ++i)
    {
      size = sizes[i];
      reloc = (relocation_info_ptr) xmalloc (size);
      lseek (desc, offsets[i], 0);
      read (desc, reloc, size);

      p = reloc;
      end = (relocation_info_ptr) (size + (char *) reloc);
      while (p < end)
	{
	  if (p->r_extern)
	    {
	      int newnum = (sym_base == 0 ? -1
			    :((sym_base + RELOCATION_INFO_SYMBOL_NUM(p))
			      -> n_un.n_strx));
	      if (newnum < 0)
		{
		  if (losing == 0)
		    error (0, 0, "%s: warning: file is now unlinkable",
			   entry->filename);
		  losing = 1;
		}
	      RELOCATION_INFO_SYMBOL_NUM(p) = newnum;
	    }
	  p++;
	}

      lseek (desc, offsets[i], 0);
      write (desc, reloc, size);
      free ((char *) reloc);
    }
}

/* Return a newly-allocated string whose contents 
   concatenate those of S1, S2, S3.  */

char *concat (char *s1, char *s2, char *s3)
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

/* Like malloc but get fatal error if memory is exhausted.  */

char *xmalloc (unsigned size)
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

#ifndef HAVE_RENAME
int rename (char *from, char *to)
{
  unlink (to);
  if (link (from, to) < 0 || unlink (from) < 0)
    return -1;
  else
    return 0;
}
#endif

char *
symbol_dup (char *s)
{
  static int format_length;
  char *res;
  
  if (!format_length)
    format_length = strlen (symbol_format) - 1;
    
  res = xmalloc (strlen (s) + format_length);
  if (*s == '*')
    strcpy (res, s + 1);
  else
    sprintf (res, symbol_format, s);
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
void read_rules(char *filename)
{
 FILE *file;
 int i,len;
 static char line[2048];
 int argc;
 char *argv[5];
 struct symbol_h *ptr;
 
 if( (file=fopen(filename,"r")) == (FILE*)0)
   error(1,errno,"Can't open %s",filename);   
   
 while (fgets(line,2048,file) != NULL)
   {
     /* skipf comment line */
     if (line[0] == '#')
       continue;
     /* parse line */
     argc=0;
     len = strlen(line);
     for (i=0; i < len;)
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
     if (argc == 2 && strcmp(argv[0], "export") == 0)
       {
         ptr = (struct symbol_h*)malloc(sizeof(struct symbol_h));
         ptr->name = symbol_dup (argv[1]);
         ptr->action = EXPORT;
         ptr->next = exports;
         exports = ptr;
         continue;
       }
     if (argc == 2 && strcmp(argv[0], "hide") == 0)
       {
         ptr = (struct symbol_h*)malloc(sizeof(struct symbol_h));
         ptr->name = symbol_dup (argv[1]);
         ptr->action = HIDE;
         ptr->next = exports;
         exports = ptr;
         continue;
       }
     if (argc == 2 && strcmp(argv[0], "notdef") == 0)
       {
         ptr = (struct symbol_h*)malloc(sizeof(struct symbol_h));
         ptr->name = symbol_dup (argv[1]);
         ptr->action = NOTDEF;
         ptr->next = exports;
         exports = ptr;
         continue;
       }
     if (argc == 3 && strcmp(argv[0], "allow") == 0 
   		 && strcmp(argv[1], "debug") == 0
   		 && strcmp(argv[2], "symbols")==0)
       {
         allow_debug=1;
         continue;
       }
#ifdef CHECKERFLAG
     if (argc == 2 && strcmp(argv[0], "set") == 0
    		 && strcmp(argv[1], "checkerflag") == 0)
       {
         setcheckerflag = 1;
         continue;
       }
#endif
     if (argc == 3 && strcmp(argv[0], "default") == 0
   		 && strcmp(argv[1], "is") == 0)
       {
         if (strcmp(argv[2], "hide") == 0)
           {
             default_exp = HIDE;
             continue;
           }
         if (strcmp(argv[2], "export") == 0)
           {
             default_exp = EXPORT;
             continue;
           }
         if (strcmp(argv[2], "unchange") == 0)
           {
             default_exp = UNCHANGE;
             continue;
           }
         error(0,0,"Default mode '%s' is unknown", argv[2]);
       }
     if (argc == 1 && strcmp(argv[0], "end") == 0)
       break;
     error(0,0,"Unknown command: %s %s", argv[0], argv[1]);
   }
 fclose(file);
}

int is_sym_exported (struct nlist *n, char *sym)
{
 struct symbol_h *ptr;
 int res;

 if (sym == (char*)0 || *sym == '\0')
   return UNCHANGE;
    
 switch(n->n_type & (~N_EXT))
 {
   case 0:
#if 0
   	if (n->n_value == 0)
   	  return UNCHANGE;	/* undefined symbols */
#endif
      for (ptr = exports; ptr; ptr = ptr->next)
	{
	  if (strcmp(sym, ptr->name) == 0)
	  {
	    res = ptr->action;
	    if (verbose)
	      fprintf(stderr,"%s is found to %d\n", sym, res);
	    return res == NOTDEF ? NOTDEF : UNCHANGE;
	  }
	}
      if (verbose)
	fprintf(stderr,"%s is not found\n",sym);
      return UNCHANGE;

#ifndef N_SETB
#define N_SETB 0x1A
#define N_SETA 0x14
#define N_SETT 0x16
#define N_SETD 0x18
#define N_SETV 0x1C
#endif
   case N_ABS:
   case N_TEXT:
   case N_DATA:
   case N_BSS:
   case N_SETB:
   case N_SETV:
   case N_SETA:
   case N_SETT:
   case N_SETD:
      for (ptr = exports; ptr; ptr = ptr->next)
	{
	  if (strcmp(sym, ptr->name) == 0)
	  {
	    res = ptr->action;
	    if (verbose)
	      fprintf(stderr,"%s is found to %d\n", sym, res);
	    return res;
	  }
	}
      if (verbose)
  	fprintf(stderr,"%s is not found\n",sym);
      return default_exp;
   default:
      return UNCHANGE; /* don't change it */
 }
}

void usage ()
{
  fprintf (stderr, "Usage: %s [-vxX] [-f format] obj-file rule-file\n", program_name);
  fprintf (stderr, "or     %s -c obj-file\n", program_name);
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
/* Heavy destroying by T. Gingold */

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
    fprintf (stderr, ": %s", sys_errlist [errnum]);
  putc ('\n', stderr);
  fflush (stderr);
  if (status)
    exit (status);
}
