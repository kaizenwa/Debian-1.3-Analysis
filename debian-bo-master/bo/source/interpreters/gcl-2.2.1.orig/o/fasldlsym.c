/*
 Copyright (C) 1994 M. Hagiya, W. Schelter, T. Yuasa

This file is part of GNU Common Lisp, herein referred to as GCL

GCL is free software; you can redistribute it and/or modify it under
the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
License for more details.

You should have received a copy of the GNU Library General Public License 
along with GCL; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/


#include <dlfcn.h>
#ifdef HAVE_AOUT
#include HAVE_AOUT
#endif
#ifdef HAVE_ELF
#include <elf.h>
#endif

/* cc -DVOL=volatile -G 0 -c foo.c ; ld  -shared foo.o -o jim.o ; cat foo.data >> jim.o */
int did_a_dynamic_load;

struct name_list
{ struct name_list *next;
  char name[1];
} ;
static struct name_list *loaded_files;

  
get_init_name(faslfile,init_fun)
     object faslfile;
     char *init_fun;
{
  object path = coerce_to_pathname(faslfile);
  char *p;
  int n;
  strcpy(init_fun,"init_");
  coerce_to_filename(path->pn.pn_name,init_fun+5);
  p = init_fun +5;
  while(*p)
    {if (*p == '-') *p = '_';
     p++;}
}

fasload(faslfile)
     object faslfile;
{ void *dlp ;
  int (*fptr)();
  char buf[200];
  static count=0;
  object memory;
  object data;
  char filename[MAXPATHLEN];
  object faslstream;
  coerce_to_filename(truename(faslfile), filename);
  if (count == 0)
    count = time(0);
  sprintf(buf,"/tmp/ufas%dxXXXXXX",count++);
  /* this is just to allow reloading in the same file twice.
   */
  mktemp(buf);
  symlink(filename,buf);
  { struct name_list *nl=
      (void *) malloc(strlen(buf)+sizeof(struct name_list));
    nl->next = loaded_files;
    loaded_files = nl;
    strcpy(nl->name,buf);
  }
  faslstream = open_stream(faslfile, smm_input, Cnil, sKerror);
#define MAKE_SHARED_LIB  
#ifdef MAKE_SHARED_LIB
  { struct filehdr fhdr;
    fread(&fhdr,1,sizeof(fhdr),faslstream->sm.sm_fp);
    fseek(faslstream->sm.sm_fp,0,0);
    if (IS_NOT_SHARED_OBJECT(fhdr,faslstream))
      { char com[600];
	unlink(buf);
	LD_SHARED(filename,buf);
	system(com);
      }
  }
#endif
  
  dlp = dlopen(buf,RTLD_NOW);
  if (dlp ==0)
    FEerror("Cant open for dynamic link ~a",1,make_simple_string(faslfile));
  
  fptr = (int (*)())dlsym(dlp, "init_code");
  if (fptr == 0)
    { /* maybe system-p compiled so init_filename */
      char init_fun[200];
      get_init_name(faslfile,init_fun);
      fptr = (int (*)())dlsym(dlp, init_fun);
      if (fptr == 0)
      FEerror("Cant find init_code in ~a",1,make_simple_string(faslfile));}
  
  SEEK_TO_END_OFILE(faslstream->sm.sm_fp);
  data = read_fasl_vector(faslstream);
  memory = alloc_object(t_cfdata);
  memory->cfd.cfd_self = NULL;
  memory->cfd.cfd_start = NULL;
  memory->cfd.cfd_size = 0;
  if(symbol_value(sLAload_verboseA)!=Cnil)	
    printf(" start address (dynamic) 0x%x ",fptr);
  call_init(0,memory,data,fptr);
  unlink(buf);
  did_a_dynamic_load = 1;
  return memory->cfd.cfd_size;
}

unlink_loaded_files()
{ while(loaded_files)
    { unlink(loaded_files->name);
      loaded_files= loaded_files->next;
    }
}
  
