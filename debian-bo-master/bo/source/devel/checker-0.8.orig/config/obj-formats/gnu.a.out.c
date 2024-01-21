/* gnu.a.out.c Functions to handle the symbol table of a a.out exec file.
   from nlist.c and objdump.c Modifications by T.Gingold */

/* Copyright (C) 1991 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU C Library; see the file COPYING.  If not, write to
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  */

#define NEED_MM
#include "checker.h"
#include "message.h"
#include <a.out.h>
#include <stab.h>
#include <fcntl.h>
#ifdef SUNOS_LIBRARIES
#include <sys/types.h>
#include <link.h>
#endif
#ifdef CHKR_SAVESTACK

static int prepare_symtab_file (void);

typedef struct nlist NList;

#include "stabx.h"

/* The header structure of the executable file. */
static struct exec my_header;

/* Test if eheader is right.
 */
extern etext (void);
extern char edata, end;
static int
is_good_image (struct exec eheader)
{
  /* Check that this is really an executable:
     The magic number must be valid and
     there must be no relocation infos (on linux).  */
  if (N_BADMAG (eheader) || eheader.a_trsize || eheader.a_drsize)
    return 0;
     
#define PAGE_ROUND(a) (((unsigned)(a) + CHKR_PAGESIZE -1) & ~(CHKR_PAGESIZE - 1))     

  /* Check that this is not a bad file.  */
#ifndef N_MAGIC
#define N_MAGIC(x) x.a_magic
#endif  
  switch (N_MAGIC (eheader))
    {
#ifndef MDCHECKER
#ifdef N_TXTADDR
      case ZMAGIC:
      case NMAGIC:
      case OMAGIC:
        if (N_DATADDR (eheader) != PAGE_ROUND ((uint) &etext)
            || N_BSSADDR (eheader) != PAGE_ROUND ((uint) &edata)
            || N_BSSADDR (eheader) + PAGE_ROUND (eheader.a_bss) != PAGE_ROUND ((uint) &end)) 
          return 0;
        break;
#else /* N_TXTADDR */
      case ZMAGIC:
      case NMAGIC:
        if (eheader.a_text != (uint)&etext
     	    || eheader.a_text + eheader.a_data != PAGE_ROUND ((uint)&edata)
     	    || eheader.a_text + eheader.a_data + eheader.a_bss != PAGE_ROUND ((uint)&end))
     	  return 0;
     	break;
      case OMAGIC:
        if (eheader.a_text != (uint) &etext
            || eheader.a_text + eheader.a_data != (uint) &edata
            || eheader.a_text + eheader.a_data + eheader.a_bss != (uint) &end)
          return 0;
        break;
#endif /* !N_TXTADDR */
#else /* MDCHECKER */
      /* We can't check.  So we assume it is OK. */
      case ZMAGIC:
      case NMAGIC:
      case OMAGIC:
        return 1;
#endif /* MDCHECKER */
#ifdef QMAGIC
      case QMAGIC:
        /* Really easy! (I like it !!) */
        if (memcmp (&eheader, (struct exec*) CHKR_PAGESIZE, sizeof (struct exec)) != 0)
          return 0;
        break;
#endif /* QMAGIC */
      default:
        return 0;
    }
  /* seems OK */
  return 1;
}

/* Return true if the symbol is useful
   useful symbols are:
        N_SLINE		line number in text segment
        N_SO		name of main source file
        N_SOL		name of source file
        N_FUN		function name
 */
/*#define GOODSYM(x) (((x)& ~N_EXT) == N_TEXT || ((x)& ~0x20) == 0x44)*/
#define GOODSYM(x) ((x) == N_SO || (x) == N_SOL || (x) == N_SLINE || (x) == N_FUN)

/* Prepare the symtab file: create it.
 * All unuseful symbols are forgotten. */
static int
prepare_symtab_file (void)
{
 int fd;
 NList *sym, *sym1;
 char *string_table1;			/* second string table */
 unsigned long int string_table_size;	/* size of the string table */
 int  str_index=0;
 int i;

 symbols = (NList *) 0;
 string_table = (char *) 0;
 string_table1 = (char *) 0;
 
 /* Symtab never loaded or symtab temp file not saved.  */
 strcpy (symtabfileproto, TMP_FILE);
 symtabfile = chkr_mktemp (symtabfileproto);	/* make the temp file name */
 if (symtabfile == NULL)
   return -1;
   
 /* Open the binary file.  */
 fd = open (chkr_prog_path, O_RDONLY);
 if (fd == -1)
   return -1;
   
 /* Go to the symbol table.  */
 if (lseek (fd, N_SYMOFF (my_header), 0) < 0)
   {
     close (fd);
     return -1;
   }
   
 /* Allocate (stack) memory for loading the symbols.  */
 symbols = (NList *) sys_malloc (my_header.a_syms);
 nsymbols = my_header.a_syms / sizeof (symbols[0]);
 
 /* Load the symbols table.  */
 if (read (fd, (PTR) symbols, sizeof (symbols[0]) * nsymbols) != 
	sizeof (symbols[0]) * nsymbols)
   goto error;

#ifdef N_STROFF
 /* Go to the string table.  */
 if (lseek (fd, N_STROFF (my_header), 0) < 0)
   {
     close (fd);
     return -1;
   }
#endif

 /* Load the size of the strings table.  */
 if (read (fd, (PTR) &string_table_size, sizeof (string_table_size)) 
 				!= sizeof (string_table_size))
   goto error;
   
 /* Allocate space for the strings.  */
 string_table_size -= sizeof (string_table_size);
 string_table = (char *) sys_malloc (string_table_size);
 
 /* This second string table will contain a relocated and shorter copy 
    of the first string table.  */
 string_table1 = (char*) sys_malloc (string_table_size + 30); /* security */
 
 /* Load the strings.  */ 
 if (read (fd, (PTR) string_table, string_table_size) != string_table_size)
    goto error;
   
 /* Prepare the second string table.  */
#define BAD_STR M_BAD_STR
 string_table1[str_index++] = '\0';	
 strcpy (&string_table1[str_index], BAD_STR);
 str_index += strlen (BAD_STR) + 1;	/* +1 is the '\0' */
 
 /* Shift the symtab: forget unuseful symbols.  The second string table
    is initialised.  */
 for (sym = symbols, sym1 = symbols; sym < &symbols[nsymbols]; sym++ )
   if (GOODSYM (sym->n_type))
     {
       *sym1 = *sym;
       /* Taken from objdump.c (binutils 1.9).  */
       if (sym1->n_un.n_strx < 0 || sym1->n_un.n_strx > string_table_size) 
     	   sym1->n_un.n_strx = 1;		/* 1 is BAD_STR */
       else
         {
           char *str = &string_table1[str_index];
           /* Copy the string.  FIXME: try to see if it already exits.  */
           strcpy (str, &string_table[ sym1->n_un.n_strx 
         		- sizeof (string_table_size) ]);
           sym1->n_un.n_strx = str_index;
           /* Remove ':' of the function names.  */
           if (sym->n_type == N_FUN)
             {
               char *dp;
               dp = strchr (string_table1 + str_index, ':');
               if (dp)
                 *dp = '\0';
             }
           str_index += strlen (&string_table1[str_index]) + 1;
         } 
       sym1++;
     }
 nsymbols = sym1 - symbols;
   
 close (fd);		/* all is right */

 /* Relocation.  */
 for (i = 0; i < nsymbols; i++)
   symbols[i].n_un.n_name = (char*) (MM_SYM + nsymbols * sizeof(symbols[0]) 
       			        + symbols[i].n_un.n_strx);
       			        
 /* Write the symtab to the temp file.  */
 fd = open (symtabfile, O_RDWR | O_CREAT, 0600);
 if (fd != -1)
   {
     /* This is a very temporary file.  */
     unlink (symtabfile);
     
     /* Write the symbol table.  */
     if (write (fd, (PTR) symbols, nsymbols * sizeof (symbols[0])) !=
         nsymbols * sizeof (symbols[0]))
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
     symtabfilesize = nsymbols * sizeof (symbols[0]) + str_index;
#if 0         
     /* This file will be removed at the end.  */
     atexit (chkr_remove_symtabfile);
#endif /* 0 */
     
     sys_free (symbols);
     sys_free (string_table);
     sys_free (string_table1);
     symbols = (NList*)0;
     return fd;
   }
 
   error:	/* in case of error */
 if (symbols)
   sys_free (symbols);
 if (string_table)
   sys_free (string_table);
 if (string_table1)
   sys_free (string_table1);
 symbols = (NList*)0;
 return -1;
   error_bis:
 if (symbols)
   sys_free (symbols);
 if (string_table)
   sys_free (string_table);
 if (string_table1)
   sys_free (string_table1);
 symbols = (NList*)0;
 return -1;
}

/* Initialize the objects structure: seek etext, edata, end.
 * If LINKED is true, ETEXT are good for the program
 * eg: if checker is a module, ETEXT is for checker, not for your program.
 */
void
init_main_object (int linked, char *argv0, int nlibs, char **libs)
{
  struct exec hdr;
  char *path;
  int fd, i, n;
#ifdef SUNOS_LIBRARIES
  struct link_map *lm = (struct link_map *) libs;
#endif
  
  objects = (struct object*) sys_malloc ((2 * nlibs + 2) * sizeof (struct object));
  
  /* The main program.  */
  objects[0].path = chkr_prog_path ? chkr_prog_path : "Main program";
  path = chkr_prog_path ? chkr_prog_path : "/proc/self/exe";
  fd = open (path, O_RDONLY);
  if (fd == -1)
    {
      chkr_printf (M_CANT_OPEN_BINARY, path);
      chkr_abort ();
    }
  i = read (fd, (PTR)&my_header, sizeof (struct exec));
  if (i != sizeof (struct exec) || (linked && !is_good_image (my_header)) )
    {
       chkr_header (M_CANT_F_GOOD_INFO);
       chkr_abort ();
    }
  close (fd);

  objects[0].name = ".text";
  objects[0].org = my_header.a_entry;
  objects[0].end = (my_header.a_entry & ~(CHKR_PAGESIZE - 1)) + my_header.a_text;
  objects[0].rights = OBJECT_TEXT | OBJECT_READ | OBJECT_EXEC;
  objects[0].next = &objects[1];
  
  objects[1].path = objects[0].path;
  objects[1].name = ".data";
  objects[1].org = objects[0].end;
  objects[1].end = objects[0].end + my_header.a_data + my_header.a_bss;
  objects[1].rights = OBJECT_DATA | OBJECT_READ | OBJECT_WRIT;
  objects[1].next = (struct object *)0;
  
#ifndef MDCHECKER
   /* Perhaps the text segment is writable.  */
   if (N_MAGIC (my_header) == OMAGIC)
     is_text_writable = 1;	/* can write in the text segment */
#endif /* MDCHECKER */  
  
  /* The libraries */
  n = 1;
  for (i = 0; i < nlibs; i++)
    {
      daddr_t offset;

      /* The name.  */      
#ifdef SUNOS_LIBRARIES
      objects[n + 1].path = lm->lm_name;
#endif
#ifdef LINUX_AOUT_LIBRARIES
      objects[n + 1].path = (char *) sys_malloc (strlen (libs[i]) + 1);
      strcpy (objects[n + 1].path, libs[i]);
#endif

      /* Link it.  */
      objects[n].next = &objects[n+1];

#ifdef LINUX_AOUT_LIBRARIES
      /* Get info from the lib.  It can't fail.  */
      fd = open (name, O_RDONLY);
      read (fd, (PTR) &hdr, sizeof (struct exec));
      close (fd);
#endif

#ifdef SUNOS_LIBRARIES
      offset = (daddr_t) lm->lm_addr;
      hdr = * ((struct exec *) lm->lm_addr);
#endif

      objects[n+1].name = ".text";
      objects[n+1].org = hdr.a_entry + offset;
      objects[n+1].end = (hdr.a_entry & ~(CHKR_PAGESIZE - 1)) + hdr.a_text
      			 + offset;
      objects[n+1].rights = OBJECT_TEXT | OBJECT_READ | OBJECT_EXEC;
      objects[n+1].next = &objects[n+2];
  
      objects[n+2].path = objects[n+1].path;
      objects[n+2].name = ".data";
      objects[n+2].org = objects[n+1].end;
      objects[n+2].end = objects[n+1].end + hdr.a_data + hdr.a_bss;
      objects[n+2].rights = OBJECT_DATA | OBJECT_READ | OBJECT_WRIT;
      objects[n+2].next = (struct object *)0;
     
      n += 2;
#ifdef SUNOS_LIBRARIES
      lm = lm->lm_next;
#endif
    }
}
  
#endif /* CHKR_SAVESTACK */
