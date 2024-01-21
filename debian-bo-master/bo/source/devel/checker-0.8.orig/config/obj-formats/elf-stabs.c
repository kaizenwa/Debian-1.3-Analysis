/* elf-stabs.c Functions to handle the symbol table of an elf exec file.
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
#include <sys/stat.h>
#include <elf.h>
#include <link.h>
#include <fcntl.h>
#define NEED_MM
#include "checker.h"
#include "message.h"
#ifdef CHKR_SAVESTACK

/* Define nlist.  Solaris doesn't define it, so we need to do so.  */
typedef struct
{
  union
    {
      char *n_name;
      long n_strx;
    } n_un;
  unsigned char n_type;
  char n_other;
  short n_desc;
  unsigned long n_value;
} NList;

/* Common values of n_desc.  */
#include "stab.h"

/* Prototype for stabx.h.  */
static int prepare_symtab_file (void);

/* For stabs.  */
#include "stabx.h"

/* Return true if the symbol is useful
   useful symbols are:
        N_SLINE		line number in text segment
        N_SO		name of main source file
        N_SOL		name of source file
        N_FUN		function name
 */
/*#define GOODSYM(x) ( ((x)& ~N_EXT) == N_TEXT || ((x)& ~0x20) == 0x44 )*/
#define GOODSYM(x) ((x) == N_SO || (x) == N_SOL || (x) == N_SLINE || (x) == N_FUN)

/* Prepare the symtab file: create it.  All unuseful symbols are forgotten.  */
static int
prepare_symtab_file (void)
{
 int fd;
 NList *sym, *sym1;
 NList *newsymbols;
 char *string_table1;			/* second string table */
 unsigned long int string_table_size;	/* size of the string table */
 int  str_index=0;
 int i;
 Elf32_Ehdr *ehdr;		/* Elf header. */
 Elf32_Shdr *shdr;		/* Section header. */
 Elf32_Shdr *stabs_shdr;	/* Section header for the stabs. */
 Elf32_Shdr *stabs_str_shdr;	/* Section header for the strings. */
 char *ehdr_str;
 caddr_t base;			/* When in core, base of the image. */
 struct stat statbuf;
 uint str_offset;
 uint str_offset_to_add;
 uint func_offset;

 /* Symtab never loaded or symtab temp file not saved.  Make the temp file
    name.  */
 strcpy (symtabfileproto, TMP_FILE);
 symtabfile = chkr_mktemp (symtabfileproto);
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

 /* Map the file in memory.  */
 base = mmap ((caddr_t) 0, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);
 close (fd);
 if (base == (caddr_t) -1)
   return -1;

 ehdr = (Elf32_Ehdr*)  base;

 /* Section header.  */
 shdr = (Elf32_Shdr*)(base + ehdr->e_shoff);
 /* String table for the section header.  */
 ehdr_str = base + shdr[ehdr->e_shstrndx].sh_offset;
 
 stabs_shdr = (Elf32_Shdr*) 0;
 stabs_str_shdr = (Elf32_Shdr*) 0;
 
 for (i = 0; i < ehdr->e_shnum; i++)
   {
     if (strcmp (ehdr_str + shdr[i].sh_name, ".stab") == 0)
       stabs_shdr = &shdr[i];
     else if (strcmp (ehdr_str + shdr[i].sh_name, ".stabstr") == 0)
       stabs_str_shdr = shdr + i;
     else
       continue;
    if (stabs_shdr && stabs_str_shdr)
      break;
   }

 if (!stabs_shdr || !stabs_str_shdr)
   {
     munmap (base, statbuf.st_size);
     return -1;
   }

 /* Symbol table.  */
 symbols = (NList*) (base + stabs_shdr->sh_offset);
 
 /* Number of stabs.  */
 nsymbols = stabs_shdr->sh_size / sizeof (NList);
 
 string_table_size = stabs_str_shdr->sh_size;
 string_table = base + stabs_str_shdr->sh_offset;
 string_table1 = (char*) sys_malloc (string_table_size + 30); /* security */
 newsymbols = (NList *) sys_malloc (nsymbols * sizeof (NList));
 
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
   if (sym->n_type == 0 /* N_UNDF */)
     {
       /* The string indexes are not relocated when the executable is build.
        * Each string tables are concatenated.  To read the good string
        * table, we have to use an offset.  We know the size of each string
        * table by the first stab entry of each object files.  */
       str_offset += str_offset_to_add;
       str_offset_to_add = sym->n_value;
     }
   else if (GOODSYM (sym->n_type))
     {
       *sym1 = *sym;
       if (sym1->n_un.n_strx + str_offset > string_table_size) 
         sym1->n_un.n_strx = 1;		/* 1 is BAD_STR */
       else if (sym1->n_un.n_strx)
         {
           /* Copy the string. FIXME: try to see if it already exits. */
           strcpy(string_table1 + str_index, string_table + sym1->n_un.n_strx + str_offset);
           sym1->n_un.n_strx = str_index;
           /* Remove ':' of the function names.  */
           if (sym->n_type == N_FUN)
             {
               char *dp;
               dp = strchr (string_table1 + str_index, ':');
               if (dp)
                 *dp = '\0';
               func_offset = sym->n_value;
             }
           str_index += strlen (string_table1 + str_index) + 1;
         } 
       if (sym1->n_type == N_SLINE)
         sym1->n_value += func_offset;
       sym1++;
     }
 nsymbols = sym1 - newsymbols;
   
 /* Relocation.  */
 for (i = 0; i < nsymbols; i++)
   newsymbols[i].n_un.n_strx = MM_SYM + nsymbols * sizeof (NList) 
     			        + newsymbols[i].n_un.n_strx;

 /* Unmap the file.  */
 munmap (base, statbuf.st_size);
 
 /* Write the symtab to the temp file.  */
 fd = open (symtabfile, O_RDWR | O_CREAT, 0600);
 if (fd != -1)
   {
     /* This is a very temporary file.  */
     unlink (symtabfile);
     
     /* Write the symbol table.  */
     if (write (fd, (PTR) newsymbols, nsymbols * sizeof (NList)) !=
         nsymbols * sizeof (NList))
       goto error_bis;
       
     /* Write the string table.  */
     if (write (fd, string_table1, str_index) != str_index)
       goto error_bis;
       
     /* Write symtabfilesize.  */
     if (write (fd, (PTR) &symtabfilesize, sizeof (int)) != sizeof (int))
       goto error_bis;
       
     /* Write nsymbols.  */
     if (write (fd, (PTR) &nsymbols, sizeof (int)) != sizeof (int))
       goto error_bis;
       
     symtabfilesize = nsymbols * sizeof(NList) + str_index;
#if 0         
     /* This file will be removed at the end.  */
     atexit (chkr_remove_symtabfile);
#endif /* 0 */
     
     symbols = (NList*)0;
     sys_free (string_table1);
     sys_free (newsymbols);
     return fd;
   }
   
   error_bis:
 symbols = (NList*)0;
 sys_free (string_table1);
 sys_free (newsymbols);
 return -1;
}

#endif /* STACK */

#include "elf-libs.h"
 
