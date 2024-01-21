/* stabx.h Functions to handle stabs.
   Copyright 1993,1994,1995 Tristan Gingold
		  Written October 1993 by Tristan Gingold

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

/* The prototype of the file, used by mktemp(). */
static char symtabfileproto[] = TMP_FILE;

/* This variable contains the temp file where the symbol table is stored. */
static char *symtabfile;

/* Size of the temp file. */
static unsigned int symtabfilesize;

/* Number of symbols in the temp file. */
static size_t nsymbols;

/* A pointer to the symbol table when loaded in memory. */
static NList *symbols;

/* A pointer to the string table when loaded in memory. */
static char *string_table;

/* 0: not loaded; 1: loaded */
static enum { SYMTAB_NOT_LOADED, SYMTAB_LOADED, SYMTAB_ERROR }
   symtab_status = SYMTAB_NOT_LOADED;

/* True if no symtab available.  Set by parse-args.c */
extern int flag_no_symtab;

/* chkr_find_nearest_line use a cache. */
struct symbol_entry
{
  PTR pc;
  uint line;
  char *funcname;
  char *filename;
  char *dirname;
};

/* Number of entries in this cache. */
#define NBR_ENTRY 32

/* The cache */
static struct symbol_entry symbol_cache[NBR_ENTRY];

/* The last entry: new informations must be store in it. */
static int entry_last;

#ifdef CHKR_PROFILE
/* Cache hit */
static int entry_found;
/* Not found in the cache */
static int entry_searched;
#endif

/* Load the symbols in memory */
void
chkr_load_symtab (void)
{
 int fd;

 /* No symtab or already loaded.  */
 if (flag_no_symtab || symtab_status != SYMTAB_NOT_LOADED)
   return;
 
 /* Prepare to fail... */
 symtab_status = SYMTAB_ERROR;
 
 /* Prepare the symbole table file.  */
 fd = prepare_symtab_file ();
 if (fd == -1)
   return;
 
 /* Load into memory the symbol table */
 symbols = (NList *) (mmap ((char*)MM_SYM, symtabfilesize, PROT_READ,
               MAP_FIXED | MAP_FILE | MAP_PRIVATE, fd, 0));
 string_table = (char *)&symbols[nsymbols];
 close (fd);
 if (symbols != (NList *)MM_SYM)
   return;
 symtab_status = SYMTAB_LOADED;
}

/* NOTE: This comes from bfd/aoutx.h */
static void
chkr_find_nearest_line (PTR ptr, char **function_name,
	char **directory_name, char **main_file_name, uint *line)
{
  /* Run down the file looking for the filename, function and linenumber */
  char *current_file_name = NULL;
  char *line_file_name = NULL; /* Value of current_file_name at line number. */
  unsigned long low_line_vma = 0;
  unsigned long low_func_vma = 0;
  NList *func = 0;
  NList *p;
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
    {
    next:
      switch (p->n_type)
      {
      case N_SO:
	*main_file_name = current_file_name = p->n_un.n_name;
	/* Look ahead to next symbol to check if that too is an N_SO. */
	p++;
	if (p == &symbols[nsymbols])
	  break;
	if (p->n_type != (int)N_SO)
	  goto next;

	/* Found a second N_SO  First is directory; second is filename. */
	*directory_name = current_file_name;
	*main_file_name = current_file_name = p->n_un.n_name;
	break;
      case N_SOL:
	current_file_name = p->n_un.n_name;
	break;
      case N_SLINE:
	/* We'll keep this if it resolves nearer than the one we have already */
	if (p->n_value >= low_line_vma && p->n_value <= (unsigned long) ptr)
	  {
	    *line = p->n_desc;
	    low_line_vma = p->n_value;
	    line_file_name = current_file_name;
	  }
	break;
      case N_FUN:
	{
	  /* We'll keep this if it is nearer than the one we have already */
	  if (p->n_value >= low_func_vma && p->n_value <= (unsigned long) ptr)
	    {
	      low_func_vma = p->n_value;
	      func = p;
	    }
	}
	break;
      }
  }

  if (*line)
    *main_file_name = line_file_name;
  if (func)
    *function_name = func->n_un.n_name;
  if (func && *line)
    {
      symbol_cache[entry_last].pc = ptr;
      symbol_cache[entry_last].line = *line;
      symbol_cache[entry_last].funcname = func->n_un.n_name;
      symbol_cache[entry_last].filename = *main_file_name;
      symbol_cache[entry_last].dirname = *directory_name;
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
  
  if (symtab_status != SYMTAB_LOADED)
    chkr_printf (M_PC__NOSYMTAB, ptr);
  else
    {
      chkr_find_nearest_line (ptr, &function_name, &directory_name,
      				  &main_file_name, &line);
                                  
      chkr_printf (M_PC___IN___AT_,
                  ptr, /* pc or ip */
                  function_name ? function_name : M_UNKNOWN,
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
 current_history = alloca ((nbr_funcs + 1) * sizeof (PTR));
   
 /* No symbols? -> we can't compare! */       
 if (flag_no_symtab)
   return 0;
   
 chkr_get_history (current_history, 0, nbr_funcs + 1);
 for (n = 0; current_history[n]; n++)
   ;
 if (n != nbr_funcs)
   return 0;
 chkr_load_symtab();
 for (n = 0; current_history[n]; n++)
   {
      chkr_find_nearest_line (current_history[n], &function_name,
      	 	&directory_name, &main_file_name, &line);
      if (!function_name || compare_function_name (funcs[n], function_name) != 0)
        return 0;
   }
 
 return 1;
}

/* Show all the symbol table.  */
void
__chkr_dump_symtab (void)
{
  int i;
  NList *sp;

  if (symtab_status != SYMTAB_LOADED)
    return;
  /* From objdump.c (binutils 1.9) */
  chkr_printf("%3s: %4s %5s %4s %8s",
	  "#", "type", "other", "desc", "val");
  for (i = 0, sp = symbols; i < nsymbols; i++, sp++)
    {
      chkr_printf("%3d: %4x %5x %4x %8lx %s\n",
	      i,
	      sp->n_type & 0xff,
	      sp->n_other & 0xff,
	      sp->n_desc & 0xffff,
	      sp->n_value,
	      sp->n_un.n_name);
    }
}
