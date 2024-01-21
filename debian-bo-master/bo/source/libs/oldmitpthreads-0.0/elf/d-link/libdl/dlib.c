/*
 * libdl.c
 * 
 * Functions required for dlopen et. al.
 */

#include <stdio.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <sys/mman.h>
#include "hash.h"
#include "string.h"
#include "linuxelf.h"

extern int _dl_error_number;
extern void * (*_dl_malloc_function)(size_t size);

static struct dyn_elf * handles;
static int do_fixup(struct elf_resolve * tpnt, int flag);

void * _dlopen(char * filename, int flag);
const char * _dlerror(void);
void * _dlsym(void *, char *);
int _dlclose(void *);

static const char * dl_error_names[] = {
	"",
	"File not found",
	"Unable to open /dev/zero",
	"Not an ELF file",
	"Not i386 binary",
	"Not an ELF shared library",
	"Unable to mmap file",
	"No dynamic section",
	"Unable to process RELA relocs",
	"Bad handle",
	"Unable to resolve symbol"
};
	
static void dl_cleanup()
{
	struct dyn_elf * rpnt, *rpnt1;
	
	for(rpnt = handles; rpnt; rpnt = rpnt1)
	{
		rpnt1 = rpnt->next_handle;
		dlclose(rpnt);
	}
}

void * _dlopen(char * libname, int flag)
{
	struct elf_resolve * tpnt;
	struct dyn_elf * rpnt;
	struct dyn_elf * dyn_chain;
	static int dl_init = 0;
	int (*dl_elf_init)(void);

	/* Have the dynamic linker use the regular malloc function now */
	if (!dl_init) {
		dl_init++;
		_dl_malloc_function = malloc;
		atexit(dl_cleanup);
	}

	/* Cover the trivial case first */
	if (!libname) return _dl_symbol_tables;

	if(!(tpnt = _dl_load_shared_library(NULL, libname)))
		return NULL;

	tpnt->usage_count++;
	dyn_chain = rpnt = 
	   (struct dyn_elf *) malloc(sizeof(struct dyn_elf));
	_dl_memset (rpnt, 0, sizeof(*rpnt));
	rpnt->dyn = tpnt;
	tpnt->symbol_scope = dyn_chain;
	
	rpnt->next_handle = handles;
	handles = rpnt;

	/*
	 * OK, we have the requested file in memory.  Now check for
	 * any other requested files that may also be required.
	 */
	  {
	    struct elf_resolve *tcurr;
	    struct elf_resolve * tpnt1;
	    struct dynamic * dpnt;
	    char * lpnt;

	    tcurr = tpnt;
	    do{
	      for(dpnt = (struct dynamic *) tcurr->dynamic_addr; dpnt->d_tag; dpnt++)
		{
	  
		  if(dpnt->d_tag == DT_NEEDED)
		    {
		      lpnt = tcurr->loadaddr + tcurr->dynamic_info[DT_STRTAB] + 
			dpnt->d_un.d_val;
		      if(!(tpnt1 = _dl_load_shared_library(_dl_symbol_tables, lpnt)))
			goto oops;

		      rpnt->next = 
		         (struct dyn_elf *) malloc(sizeof(struct dyn_elf));
		      _dl_memset (rpnt->next, 0, sizeof (*(rpnt->next)));
		      rpnt = rpnt->next;
		      tpnt1->usage_count++;
		      if (!tpnt1->symbol_scope) tpnt1->symbol_scope = dyn_chain;
		      rpnt->dyn = tpnt1;
		    };
		}
	      
	      tcurr = tcurr->next;
	    } while(tcurr);
	  }
	 
	/*
	 * OK, now attach the entire chain at the end
	 */

	rpnt->next = _dl_symbol_tables;

	if (do_fixup(tpnt, flag)) {
	  _dl_error_number = DL_NO_SYMBOL;
	  goto oops;
	}

	for(rpnt = dyn_chain; rpnt; rpnt = rpnt->next)
    	{
		tpnt = rpnt->dyn;
	      /* Apparently crt1 for the application is responsible for handling this.
	       * We only need to run the init/fini for shared libraries
	       */
 	      if (tpnt->libtype == elf_executable) continue;
	      if (tpnt->init_flag & INIT_FUNCS_CALLED) continue;
	      tpnt->init_flag |= INIT_FUNCS_CALLED;
      
	      if(tpnt->dynamic_info[DT_INIT]) {
		dl_elf_init = (int (*)(void)) (tpnt->loadaddr + 
					    tpnt->dynamic_info[DT_INIT]);
		(*dl_elf_init)();
	      }
   	}
	return (void *) dyn_chain;
oops:
	/* Something went wrong.  Clean up and return NULL. */
	dlclose (dyn_chain);
	return NULL;
}

static int do_fixup(struct elf_resolve * tpnt, int flag)
{
  int goof = 0;
  if(tpnt->next) goof += do_fixup(tpnt->next, flag);

  if(tpnt->dynamic_info[DT_REL]) {
    if (tpnt->init_flag & RELOCS_DONE) return goof;
    tpnt->init_flag |= RELOCS_DONE;
   
    goof += _dl_parse_relocation_information(tpnt, tpnt->dynamic_info[DT_REL],
					     tpnt->dynamic_info[DT_RELSZ], 0);
  }
  if(tpnt->dynamic_info[DT_JMPREL])
    {
      if (tpnt->init_flag & JMP_RELOCS_DONE) return goof;
      tpnt->init_flag |= JMP_RELOCS_DONE;
      
      if(flag == RTLD_LAZY)
	_dl_parse_lazy_relocation_information(tpnt, tpnt->dynamic_info[DT_JMPREL],
					      tpnt->dynamic_info[DT_PLTRELSZ], 0);
      else
	goof +=  _dl_parse_relocation_information(tpnt,
						  tpnt->dynamic_info[DT_JMPREL],
						  tpnt->dynamic_info[DT_PLTRELSZ], 0);
    };
  if(tpnt->dynamic_info[DT_RELA]) {
	/* FIXME - return some error code here */
	goof++;
  };
  return goof;
}

void * _dlsym(void * vhandle, char * name)
{	
	struct dyn_elf * handle;
	struct dyn_elf * rpnt;
	void *ret;

	/* First of all verify that we have a real handle
	of some kind.  Return NULL if not a valid handle. */

	handle = (struct dyn_elf *) vhandle;

	for(rpnt = handle; rpnt; rpnt = rpnt->next_handle)
		if(rpnt == handle) break;
	
	if (!rpnt) {
		_dl_error_number = DL_BAD_HANDLE;
		return NULL;
	}
		
	ret = _dl_find_hash(name, handle, 1, NULL, 0);
	if (!ret)
	{
		_dl_error_number = DL_NO_SYMBOL;
	}
	return ret;
}

int _dlclose(void * vhandle)
{
	struct dyn_elf * rpnt, *rpnt1;
	struct dyn_elf *spnt, *spnt1;
	struct elf_phdr * ppnt;
	struct elf_resolve * tpnt;
	int (*dl_elf_fini)(void);
	struct dyn_elf * handle;
	unsigned int end;
	int i = 0;

	handle = (struct dyn_elf *) vhandle;
	rpnt1 = NULL;
	for(rpnt = handles; rpnt; rpnt = rpnt->next_handle)
	{
		if(rpnt == handle) {
			break;
		}
		rpnt1 = rpnt;
	}
	
	if (!rpnt) {
		_dl_error_number = DL_BAD_HANDLE;
		return 1;
	}

	/* OK, this is a valid handle - now close out the file */
	for(spnt = handle; spnt; spnt = spnt1)
	{
	    spnt1 = spnt->next;

	    /* We appended the module list to the end - when we get back here, 
	     quit. The access counts were not adjusted to account for being here. */
	    if (spnt == _dl_symbol_tables) break;
	    if(spnt->dyn->usage_count==1 && spnt->dyn->libtype == loaded_file) {
		tpnt = spnt->dyn;
		/* Apparently crt1 for the application is responsible for handling this.
		 * We only need to run the init/fini for shared libraries
		 */

		if(tpnt->dynamic_info[DT_FINI]) {
		    dl_elf_fini = (int (*)(void)) (tpnt->loadaddr + 
						   tpnt->dynamic_info[DT_FINI]);
		    (*dl_elf_fini)();
		}	
	    }
	}
	if(rpnt1)
	    rpnt1->next_handle = rpnt->next_handle;
	else
	    handles = rpnt->next_handle;
	
	/* OK, this is a valid handle - now close out the file */
	for(rpnt = handle; rpnt; rpnt = rpnt1)
	  {
		rpnt1 = rpnt->next;

		/* We appended the module list to the end - when we get back here, 
		   quit. The access counts were not adjusted to account for being here. */
		if (rpnt == _dl_symbol_tables) break;

		rpnt->dyn->usage_count--;
		if(rpnt->dyn->usage_count == 0 && rpnt->dyn->libtype == loaded_file)
		{
			tpnt = rpnt->dyn;
		      /* Apparently crt1 for the application is responsible for handling this.
		       * We only need to run the init/fini for shared libraries
		       */
#if 0
/* We have to do this above, before we start closing objects.
Otherwise when the needed symbols for _fini handling are
resolved a coredump would occur. Rob Ryan (robr@cmu.edu)*/
		      if(tpnt->dynamic_info[DT_FINI]) {
			dl_elf_fini = (int (*)(void)) (tpnt->loadaddr + 
						    tpnt->dynamic_info[DT_FINI]);
			(*dl_elf_fini)();
		      }	
#endif
		      end = 0;
			for(i = 0, ppnt = rpnt->dyn->ppnt; 
				i < rpnt->dyn->n_phent; ppnt++, i++) {
				if (ppnt->p_type != PT_LOAD) continue;
				if (end < ppnt->p_vaddr + ppnt->p_memsz)
					end = ppnt->p_vaddr + ppnt->p_memsz;
			}
			munmap (rpnt->dyn->loadaddr, end);
			/* Next, remove rpnt->dyn from the loaded_module list */
			if (_dl_loaded_modules == rpnt->dyn)
			{
			  _dl_loaded_modules = rpnt->dyn->next;
			  if (_dl_loaded_modules)
			    _dl_loaded_modules->prev = 0;
			}
			else
			  for (tpnt = _dl_loaded_modules; tpnt; tpnt = tpnt->next)
				if (tpnt->next == rpnt->dyn) {
				  tpnt->next = tpnt->next->next;
				  if (tpnt->next) 
				    tpnt->next->prev = tpnt;
				  break;
				}
			free(rpnt->dyn);
		      }
		free(rpnt);
	  }
	return 0;
}

const char * _dlerror()
{
	const char * retval;
	if(!_dl_error_number) return NULL;
	retval = dl_error_names[_dl_error_number];
	_dl_error_number = 0;
	return retval;
}

/* Generate the correct symbols that we need. */
#ifndef IBCS_COMPATIBLE
#if 1
#pragma weak dlopen = _dlopen
#pragma weak dlerror = _dlerror
#pragma weak dlclose = _dlclose
#pragma weak dlsym = _dlsym
#else
__asm__(".weak dlopen;dlopen=_dlopen");
__asm__(".weak dlerror;dlerror=_dlerror");
__asm__(".weak dlclose;dlclose=_dlclose");
__asm__(".weak dlsym;dlsym=_dlsym");
#endif
#endif

/* This is a real hack.  We need access to the dynamic linker, but we
also need to make it possible to link against this library without any
unresolved externals.  We provide these weak symbols to make the link
possible, but at run time the normal symbols are accessed. */

static int foobar()
{
	fprintf(stderr,"libdl library not correctly linked\n");
	exit(1);
}

static int foobar1 = 0; /* Use as pointer */

#ifndef IBCS_COMPATIBLE
#if 1
#pragma weak _dl_find_hash = foobar
#pragma weak _dl_symbol_tables = foobar1
#pragma weak _dl_loaded_modules = foobar1
#pragma weak _dl_error_number = foobar1
#pragma weak _dl_load_shared_library = foobar
#pragma weak _dl_malloc_function = foobar1
#pragma weak _dl_parse_relocation_information = foobar
#pragma weak _dl_parse_lazy_relocation_information = foobar
#else
__asm__(".weak _dl_find_hash; _dl_find_hash = foobar");
__asm__(".weak _dl_symbol_tables; _dl_symbol_tables = foobar1");
__asm__(".weak _dl_loaded_modules; _dl_loaded_modules = foobar1");
__asm__(".weak _dl_error_number; _dl_error_number = foobar1");
__asm__(".weak _dl_load_shared_library; _dl_load_shared_library = foobar");
__asm__(".weak _dl_malloc_function; _dl_malloc_function = foobar1");
__asm__(".weak _dl_parse_relocation_information; _dl_parse_relocation_information = foobar");
__asm__(".weak _dl_parse_lazy_relocation_information; _dl_parse_lazy_relocation_information = foobar");
#endif
#endif

/*
 * Dump information to stderrr about the current loaded modules
 */
static char * type[] = {"Lib","Exe","Int","Mod"};

void _dlinfo()
{
	struct elf_resolve * tpnt;
	struct dyn_elf * rpnt, *hpnt;
	fprintf(stderr, "List of loaded modules\n");
	/* First start with a complete list of all of the loaded files. */
	for (tpnt = _dl_loaded_modules; tpnt; tpnt = tpnt->next)
		fprintf(stderr,"\t%8.8x %8.8x %8.8x %s %d %s\n", tpnt->loadaddr, tpnt,
			tpnt->symbol_scope,
			type[tpnt->libtype],
			tpnt->usage_count, 
			tpnt->libname);

	/* Next dump the module list for the application itself */
	fprintf(stderr,"\nModules for application (%x):\n", _dl_symbol_tables);
	for (rpnt = _dl_symbol_tables; rpnt; rpnt = rpnt->next)
		fprintf(stderr,"\t%8.8x %s\n", rpnt->dyn, rpnt->dyn->libname);

	for (hpnt = handles; hpnt; hpnt = hpnt->next_handle)
	{
		fprintf(stderr,"Modules for handle %x\n", hpnt);
		for(rpnt = hpnt; rpnt; rpnt = rpnt->next)
			fprintf(stderr,"\t%8.8x %s\n", rpnt->dyn, rpnt->dyn->libname);
	}
}

