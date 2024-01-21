/* elf-symtab.c Functions to handle the symbol table of an elf exec file.
   Copyright 1995 Tristan Gingold
		  Written April 1995 by Tristan Gingold

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

#define NEED_MM
#include "checker.h"
#include "machine.h"
#include "message.h"
#include <sys/elf.h>
#include <link.h>
#include <fcntl.h>

#ifdef CHKR_SAVESTACK

/* Prototype for stabx.h.  */
static void prepare_symtab_file (void);

/* The prototype of the file, used by mktemp().  */
static char symtabfileproto[] = TMP_FILE;

/* This variable contains the temp file where the symbol table is stored.  */
static char *symtabfile;

/* Size of the temp file.  */
static unsigned int symtabfilesize;

/* The state of the symtab file.  */
static uint symtab_file_stat;
#define SYMTAB_NOT_PREPARED 0	/* Not prepared.  */
#define SYMTAB_ERROR 1		/* Error.  */
#define SYMTAB_TRY_AGAIN 2	/* Try again to prepare it.  */
#define SYMTAB_CREATED 3	/* Prepared.  */

/* Number of symbols in the temp file.  */
static size_t nsymbols;

/* A pointer to the symbol table when loaded in memory.  */
static Elf32_Sym *symbols;

/* A pointer to the string table when loaded in memory.  */
static char *string_table;

/* 0: not loaded; >= 1: loaded.  */
int symtab_available = 0;

/* True if no symtab available.  Set by parse-args.c.  */
extern int flag_no_symtab;

/* chkr_find_nearest_line use a cache.  */
struct symbol_entry
{
  PTR pc;
  uint line;
  char *funcname;
  char *filename;
  char *dirname;
};

/* Number of entries in this cache.  */
#define NBR_ENTRY 32

/* The cache.  */
static struct symbol_entry symbol_cache[NBR_ENTRY];

/* The last entry: new informations must be store in it.  */
static int entry_last;

#ifdef CHKR_PROFILE
/* Cache hit.  */
static int entry_found;

/* Not found in the cache.  */
static int entry_searched;
#endif

/* Load the symbols in memory.  */
void
chkr_load_symtab (void)
{
 int fd;
 
 /* no symtab or already loaded */ 
 if (flag_no_symtab || symtab_status != SYMTAB_NOT_LOADED)
   return;
 
 symtab_status = SYMTAB_ERROR;
 fd = prepare_symtab_file ();
 if (fd == -1)
    return;
    
 symbols = (Elf32_Sym*) (mmap ((char*)MM_SYM, symtabfilesize, PROT_READ,
           MAP_FIXED | MAP_FILE | MAP_PRIVATE, fd, 0));
 string_table = (char *) &symbols[nsymbols];
 close (fd);
 if (symbols != (Elf32_Sym*) MM_SYM)
   return;
 symtab_status = SYMTAB_LOADED;
}

/* NOTE: This comes from bfd/aoutx.h */
static void
chkr_find_nearest_line (PTR ptr, char **function_name,
	char **directory_name, char **main_file_name, uint *line)
{
  Elf32_Sym *p;
  int i;
  
  for (i = 0; i < NBR_ENTRY; i++)
    {
      if (symbol_cache[i].pc == ptr)
        {
          *line = symbol_cache[i].line;
          *function_name = symbol_cache[i].funcname;
          *directory_name = symbol_cache[i].dirname;
          *main_file_name = symbol_cache[i].filename;
#ifdef CHKR_PROFILE
	  entry_found++;
#endif
          return;
        }
      else
        if (!symbol_cache[i].pc)
          break;
    }
  
  *line = 0;
  *function_name = NULL;
  *directory_name = NULL;
  *main_file_name = NULL;
#ifdef CHKR_PROFILE
  entry_searched++;
#endif

  for (p = symbols; p < &symbols[nsymbols]; p++)
    if (ptr >= (PTR)p->st_value && ptr < (PTR)(p->st_value + p->st_size))
      {
        *function_name = (char*) p->st_name;
        break;
      }
      
  if (*function_name)
    {
      symbol_cache[entry_last].pc = ptr;
      symbol_cache[entry_last].line = 0;
      symbol_cache[entry_last].funcname = *function_name;
      symbol_cache[entry_last].filename = "<unknown>";
      symbol_cache[entry_last].dirname = "<unknown>";
      entry_last = (entry_last + 1) % NBR_ENTRY;
    }
}

void
chkr_show_addr (PTR ptr)
{
  char *function_name;
  char *directory_name;
  char *main_file_name;
  uint line;
  
  if (symtab_available == 0)
    chkr_printf (M_PC__NOSYMTAB, ptr);
  else
    {
      chkr_find_nearest_line (ptr, &function_name, &directory_name,
      				  &main_file_name, &line);
      if (!function_name)
        function_name = (char*) M_UNKNOWN;
                                  
      chkr_printf (M_PC___IN___AT_,
                  ptr, /* pc or ip */
                  function_name,
                  main_file_name ? main_file_name : M_UNKNOWN,
                  line);
    }
}

static int
compare_function_name (char *proto, char *func)
{
 int i;
 for (i = 0; proto[i]; i++)
   {
     if (proto[i] == '*')
       return 0;
     if (func[i] == '\0')
       return 1;
     if (func[i] != proto[i])
       return 1;
   }
 if (func[i] != '\0' && func[i] != '(')
   return 1;
 return 0;
}

int
same_history (char **funcs, int nbr_funcs)
{
 PTR *current_history; /* was [nbr_funcs+1]; */
 int n;
 char *function_name;
 char *directory_name;
 char *main_file_name;
 uint line;
 
 /* No symbols? -> we can't compare! */       
 if (flag_no_symtab)
   return 0;
   
 current_history = alloca ((nbr_funcs + 1) * sizeof (PTR));
 chkr_get_history (current_history, 0, nbr_funcs + 1);
 
 for (n = 0; current_history[n]; n++)
   ;
 if (n != nbr_funcs)
   return 0;
 chkr_load_symtab ();
 for (n = 0; current_history[n]; n++)
   {
      chkr_find_nearest_line (current_history[n], &function_name,
      	 	&directory_name, &main_file_name, &line);
      if (!function_name || compare_function_name (funcs[n], function_name) != 0)
        chkr_unload_symtab ();
   }
 
 return 1;
}

/* Show all the symbol table.  */
void
__chkr_dump_symtab (void)
{
  int i;
  Elf32_Sym *sp;

  if (symtab_available == 0)
    return;
  /* From objdump.c (binutils 1.9) */
  chkr_printf("%3s: %8s %8s %4s",
	  "#", "value", "size", "name");
  for (i = 0, sp = symbols; i < nsymbols; i++, sp++)
    {
      chkr_printf ("%3d: %8x %8x %s\n",
	      i,
	      (uint)sp->st_value,
	      (uint)sp->st_size,
	      (char*) sp->st_name);
    }
}

/* Prepare the symtab file: create it.  All unuseful symbols are forgotten.  */
static int
prepare_symtab_file (void)
{
 int fd;
 Elf32_Sym *sym, *sym1;
 Elf32_Sym *newsymbols;
 char *string_table1;			/* second string table */
 unsigned long int string_table_size;	/* size of the string table */
 int  str_index=0;
 int i;
 Elf32_Ehdr *ehdr;		/* Elf header. */
 Elf32_Shdr *shdr;		/* Section header. */
 Elf32_Shdr *symtab_shdr;	/* Section header for the symtab. */
 Elf32_Shdr *symtab_str_shdr;	/* Section header for the strings. */
 char *ehdr_str;
 caddr_t base;			/* When in core, base of the image. */
 struct stat statbuf;
 uint str_offset;
 uint str_offset_to_add;
 uint func_offset;

 /* Symtab never loaded or symtab temp file not saved.  */
 strcpy (symtabfileproto, TMP_FILE);
 symtabfile = chkr_mktemp (symtabfileproto);	/* make the temp file name */
 if (symtabfile == NULL)
   return -1;
 
 /* Open the binary file.  */
 fd = open (chkr_prog_path, O_RDONLY);
 if (fd == -1)
   return -1;
 
 /* We need to know the size of the file for mapping it.  */
 if (fstat (fd, &statbuf) == -1)
   {
     close (fd);
     return -1;
   }

 /* Map the file in memory. */
 base = mmap ((caddr_t)0, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);
 if (base == (caddr_t)-1)
   {
     close (fd);
     return -1;
   }
 ehdr = (Elf32_Ehdr*)base;

 close (fd);
   
 /* Section header.  */
 shdr = (Elf32_Shdr*)(base + ehdr->e_shoff);
 /* String table for the section header. */
 ehdr_str = base + shdr[ehdr->e_shstrndx].sh_offset;
 
 symtab_shdr = (Elf32_Shdr*)0;
 
 for (i = 0; i < ehdr->e_shnum; i++)
   if (strcmp (ehdr_str + shdr[i].sh_name, ".symtab") == 0)
     {
       symtab_shdr = &shdr[i];
       break;
     }
 if (!symtab_shdr)
   {
     munmap (base, statbuf.st_size);
     goto error;
   }
 symtab_str_shdr = &shdr[symtab_shdr->sh_link];

 /* Symbol table.  */
 symbols = (Elf32_Sym *) (base + symtab_shdr->sh_offset);
 
 /* Number of symtab entries.  */
 nsymbols = symtab_shdr->sh_size / sizeof (Elf32_Sym);
 
 string_table_size = symtab_str_shdr->sh_size;
 string_table = base + symtab_str_shdr->sh_offset;
 string_table1 = (char*) sys_malloc (string_table_size + 30); /* security */
 newsymbols = (Elf32_Sym *) sys_malloc (nsymbols * sizeof (Elf32_Sym));
 
 /* Prepare the second string table.  */
#define BAD_STR M_BAD_STR
 string_table1[str_index++] = '\0';	
 strcpy (&string_table1[str_index], BAD_STR);
 str_index += strlen(BAD_STR) + 1;	/* +1 is the '\0' */
 
 /* These are offset for the string tables.  */
 str_offset = 0;
 str_offset_to_add = 0;
 
 /* Offsets for the line numbers.  */
 func_offset = 0;
 
 /* Shift the symtab: forget unuseful symbols.  The second string table
    is initialised.  */
 for (sym = symbols, sym1 = newsymbols; sym < &symbols[nsymbols]; sym++ )
   if (ELF32_ST_TYPE (sym->st_info) == STT_FUNC)
     {
       *sym1 = *sym;
       if (sym1->st_name + str_offset > string_table_size) 
         sym1->st_name = 1;		/* 1 is BAD_STR */
       else if (sym1->st_name)
         {
           /* Copy the string. FIXME: try to see if it already exits.  */
           strcpy (string_table1 + str_index, string_table + sym1->st_name + str_offset);
           sym1->st_name = str_index;
           str_index += strlen(string_table1 + str_index) + 1;
         } 
       sym1++;
     }
 nsymbols = sym1 - newsymbols;
   
 /* Relocation.  */
 for (i = 0; i < nsymbols; i++)
   newsymbols[i].st_name = MM_SYM + nsymbols * sizeof(Elf32_Sym) 
     			        + newsymbols[i].st_name;

 /* Unmap the file.  */
 munmap (base, statbuf.st_size);
 
 /* Write the symtab to the temp file */
 fd = open (symtabfile, O_RDWR | O_CREAT, 0600);
 if (fd != -1)
   {
     /* This is a very temporary file.  */
     unlink (symtabfile);
     
     /* Write the symbol table.  */
     if (write (fd, (PTR)newsymbols, nsymbols * sizeof(Elf32_Sym)) !=
         nsymbols * sizeof (Elf32_Sym))
       goto error_bis;
       
     /* Write the string table.  */
     if (write (fd, string_table1, str_index) != str_index)
       goto error_bis;
       
     /* Write symtabfilesize.  */
     if (write (fd, (PTR)&symtabfilesize, sizeof(int)) != sizeof(int))
       goto error_bis;
       
     /* Write nsymbols.  */
     if (write (fd, (PTR)&nsymbols, sizeof(int)) != sizeof(int))
       goto error_bis;
     symtabfilesize = nsymbols * sizeof(Elf32_Sym) + str_index;
#if 0         
     /* This file will be removed at the end.  */
     atexit (chkr_remove_symtabfile);
#endif /* 0 */
     
     symbols = (Elf32_Sym*)0;
     symtab_file_stat = SYMTAB_CREATED;
     sys_free (string_table1);
     sys_free (newsymbols);
     return fd;
   }
   
   error_bis:
 symbols = (Elf32_Sym*)0;
 sys_free (string_table1);
 sys_free (newsymbols);
 return -1;
 
   error:	/* in case of error */
 symbols = (Elf32_Sym*)0;
 return -1;
}

#endif /* STACK */

#include "elf-libs.h"
