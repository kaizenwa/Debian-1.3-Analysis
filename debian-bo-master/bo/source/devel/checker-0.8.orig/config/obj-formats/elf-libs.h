/* elf-libs.h Functions to handle shared libraries on elf.
   Copyright 1995 Tristan Gingold
		  Written June 1995 by Tristan Gingold

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

/* Initialize the objects structure: seek etext, edata, end.
 * If LINKED is true, ETEXT are good for the program
 * eg: if checker is a module, ETEXT is for checker, not for your program.
 */
extern etext (void);
extern char *edata, *end;
#pragma weak _DYNAMIC
#ifndef __linux__
extern Elf32_Dyn _DYNAMIC[];
#endif
#ifdef __linux__
static Elf32_Dyn *dynamic_addr = (Elf32_Dyn *) &_DYNAMIC;
#else
static Elf32_Dyn *dynamic_addr = (Elf32_Dyn *) _DYNAMIC;
#endif

#if 0
/* Some incomplete systems, don't define these fields...  */
#ifndef PF_R
#define PF_X 1
#define PF_W 2
#define PF_R 4
#endif
#endif

/* The beginning of the '.text' segment.  */
#ifndef TEXT_BASE
#define TEXT_BASE 0
#endif

/* Called by ___chkr_init_checker to initialize the `objects' list according
   to the loaded shared libraries.  */
void
init_main_object (int linked, char *argv0, int nlibs, char **libs)
{
  Elf32_Dyn *dp;
  Elf32_Phdr *phdr;
  uint ldbase;
  uint num;
  struct link_map *lm;
  struct link_map *ldso_lm;
  struct object *obj;
  int j;
  
  /* If the file was staticaly linked, use the static info.  */
  /* dynamic_addr is &_DYNAMIC, but &_DYNAMIC doesn't work.  (GCC pitfall?) */
  if (!dynamic_addr)
    {
      if (flag_verbose)
        chkr_printf ("This file was staticaly linked.\n");
      
      /* Place from the .text and .data section of the program.  */
      objects = (struct object*) sys_malloc (2 * sizeof (struct object));
  
      /* The main program.  */
      objects[0].path = chkr_prog_path ? chkr_prog_path : "Main program";
      objects[0].name = ".text";
      objects[0].org = TEXT_BASE;
      objects[0].end = (uint) &etext;
      objects[0].rights = OBJECT_TEXT | OBJECT_READ | OBJECT_EXEC;
      objects[0].next = &objects[1];
  
      objects[1].path = objects[0].path;
      objects[1].name = ".data";
      objects[1].org = (uint) &etext;	/* bad */
      objects[1].end = (uint) &end;
      objects[1].rights = OBJECT_DATA | OBJECT_READ | OBJECT_WRIT;
      objects[1].next = (struct object *) 0;
      
      return;
    }
  
  /* Libs... */
  lm = (struct link_map*) 0;
  obj = (struct object*) 0;
  
#if defined(PLCHECKER) || defined (SIMCHECKER)
  phdr = (Elf32_Phdr *) libs;
  dp = (Elf32_Dyn *) 0;
  for (; phdr->p_type != PT_NULL; phdr++)
    if (phdr->p_type == PT_DYNAMIC)
      {
        dp = (Elf32_Dyn*)phdr->p_vaddr;
        break;
      }
  phdr = (Elf32_Phdr*) libs;
#else
  dp = dynamic_addr;
#endif

  /* Fetch the first opened link_map.  */
  ldbase = 0;
  for (; dp->d_tag; dp++)
    if (dp->d_tag == DT_DEBUG)
      {
        lm = ((struct r_debug *)(dp->d_un.d_ptr))->r_map;
        ldbase = ((struct r_debug *)(dp->d_un.d_ptr))->r_ldbase;
        break;		/* include myself */
      }
      
#ifdef ADD_LD_SO_IN_OBJECTS
   /* If ld.so is not in the linked list of DT_DEBUG, define this macro.  */
   if (lm)
     {
       ldso_lm = (struct link_map*) alloca (sizeof (struct link_map));
       ldso_lm->l_addr = ldbase;
       ldso_lm->l_next = (struct link_map*)0;
       ldso_lm->l_prev = ldso_lm;	/* To cheat later.  */
       ldso_lm->l_name = "ld.so";
     }
   else
#endif
     ldso_lm = (struct link_map*) 0;
       
       
   /* Gather info about the loaded segments.  */
   for (j = 0; j < 2; j++)
     {
       if (j == 1)
         lm = ldso_lm;
     
       for (; lm; lm = lm->l_next)
         {
           Elf32_Ehdr *ehdr;
           int i;
       
           ehdr = (Elf32_Ehdr*)lm->l_addr;
#if 1 /* def GCCCHECKER */
	   if (ehdr == (Elf32_Ehdr*)0)
	     {
	       phdr = (Elf32_Phdr*) libs;
	       num = nlibs;
	     }
	   else
#endif
	     {
	       phdr = (Elf32_Phdr*) (((char *)ehdr) + ehdr->e_phoff);
	       num = ehdr->e_phnum;
	     }
           for (i = 0; i < num; i++, phdr++)
             if (phdr->p_type == PT_LOAD)
               {
                 if (obj)
                   {
                     obj->next = (struct object*) sys_malloc(sizeof(struct object));
                     obj = obj->next;
                   }
                 else
                   objects = obj = (struct object*) sys_malloc(sizeof(struct object));
                 obj->next = (struct object*)0;
                 if (lm->l_prev)
                   /* For a shared library. */
                   obj->org = (uint)lm->l_addr + phdr->p_vaddr;
                 else
                   /* For the main file object. */
                   obj->org = (uint)phdr->p_vaddr;
                 obj->end = obj->org + phdr->p_memsz;
                 /* FIXME */
                 if (i == 1)
                   obj->end = (obj->end + 0xfff) & 0xfffff000;
                 obj->path = lm->l_name;
                 obj->rights = 0;
                 if (phdr->p_flags & PF_R)
                   obj->rights |= OBJECT_READ;
                 if (phdr->p_flags & PF_W)
                   obj->rights |= OBJECT_WRIT;
                 if (phdr->p_flags & PF_X)
                   obj->rights |= OBJECT_EXEC;
                 if (phdr->p_flags == (PF_R | PF_X))
                   {
                     obj->name = ".text";
                     obj->rights |= OBJECT_TEXT;
                     
                     /* Disable errors from ld.so */
                     if (j == 1)
                       register_disable_range (obj->org, obj->end);
                   }
                 else if (phdr->p_flags & (PF_R | PF_W))
                   {
                     obj->name = ".data";
                     obj->rights |= OBJECT_DATA;
                   }
                 else if (phdr->p_flags == PF_R)
                   obj->name = ".rodata";
                 else
                   obj->name = ".unknown";
               }
         }
     }
     
   /* Be sure the first two entry were initialized...  */ 
   if (!objects || !objects->next)
     {
       chkr_printf ("The first two entries of `objects' were not initialized.\n");
       chkr_abort ();
     }
     
   /* ... in the right order.  */
   if ((objects->rights & OBJECT_DATA) && (objects->next->rights & OBJECT_TEXT))
     {
       obj = objects->next;		/* .text */
       objects->next = obj->next;	/* .data->next = next */
       obj->next = obj;			/* .text->next = .data */
       objects = obj;			/* object = .text */
     }
   
   /* Be really sure !  */
   if (!(objects->rights & OBJECT_TEXT) || !(objects->next->rights & OBJECT_DATA))
     chkr_abort ();
   
   /* Set the path.  */
   objects->path = objects->next->path =
      chkr_prog_path ? chkr_prog_path : objects->path;
}
