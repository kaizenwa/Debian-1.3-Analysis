struct dyn_elf{
  struct elf_resolve * dyn;
  struct dyn_elf * next_handle;  /* Used by dlopen et al. */
  struct dyn_elf * next;
};
 
struct elf_resolve{
  struct elf_resolve * next;
  char * libname;
  enum {elf_lib, elf_executable,program_interpreter, loaded_file} libtype;
  struct dyn_elf * symbol_scope;
  unsigned short usage_count;
  unsigned short int init_flag;
  unsigned int nbucket;
  unsigned int * elf_buckets;
  /*
   * These are only used with ELF style shared libraries
   */
  unsigned int nchain;
  unsigned int * chains;
  char * loadaddr;
  unsigned int dynamic_info[24];

  unsigned int dynamic_addr;
  unsigned int dynamic_size;
  unsigned int n_phent;
  struct elf_phdr * ppnt;
};

#define COPY_RELOCS_DONE 1
#define RELOCS_DONE 2
#define JMP_RELOCS_DONE 4

extern struct dyn_elf     * _dl_symbol_tables;
extern struct elf_resolve * _dl_loaded_modules;

extern struct elf_resolve * _dl_check_hashed_files(char * libname);
extern struct elf_resolve * _dl_add_elf_hash_table(char * libname, 
					       char * loadaddr, 
					       unsigned int * dynamic_info, 
					       unsigned int dynamic_addr, 
					       unsigned int dynamic_size);
extern char * _dl_find_hash(char * name, struct dyn_elf * rpnt1, 
			unsigned int instr_addr, struct elf_resolve * f_tpnt);

extern int _dl_linux_dynamic_link(void);

#define SEND_STDERR(X) _dl_write(2, X, strlen(X));
extern int _dl_write(int, const char *, int);
extern char * _dl_library_path;
extern char * _dl_not_lazy;
extern char * _dl_strdup(const char *);
extern inline int _dl_symbol(char * name);
unsigned long _dl_elf_hash(const char * name);

extern inline int _dl_symbol(char * name)
{
  if(name[0] != '_' || name[1] != 'd' || name[2] != 'l' || name[3] != '_')
    return 0;
  return 1;
}

#define DL_ERROR_NOFILE 1
#define DL_ERROR_NONULL 2
#define DL_ERROR_NOTELF 3
#define DL_ERROR_NOTMAGIC 4
#define DL_ERROR_NOTDYN 5
#define DL_ERROR_MMAP_FAILED 6
#define DL_ERROR_NODYNAMIC 7
