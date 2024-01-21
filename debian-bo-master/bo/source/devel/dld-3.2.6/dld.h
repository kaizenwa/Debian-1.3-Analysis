/* Error codes */

#define DLD_ENOFILE	1	    /* cannot open file */
#define DLD_EBADMAGIC	2	    /* bad magic number */
#define DLD_EBADHEADER	3	    /* failure reading header */
#define DLD_ENOTEXT	4	    /* premature eof in text section */
#define DLD_ENOSYMBOLS	5	    /* premature end of file in symbols */
#define DLD_ENOSTRINGS	6	    /* bad string table */
#define DLD_ENOTXTRELOC	7	    /* premature eof in text relocation */
#define DLD_ENODATA	8	    /* premature eof in data section */
#define DLD_ENODATRELOC	9	    /* premature eof in data relocation */
#define DLD_EMULTDEFS	10	    /* multiple definitions of symbol */
#define DLD_EBADLIBRARY	11	    /* malformed library archive */
#define DLD_EBADCOMMON	12	    /* common block not supported */
#define DLD_EBADOBJECT	13	    /* malformed input file (not rel or
				       archive) */
#define DLD_EBADRELOC	14	    /* bad relocation info */
#define DLD_ENOMEMORY	15	    /* virtual memory exhausted */
#define DLD_EUNDEFSYM	16	    /* undefined symbol */

extern int dld_errno;		    /* error code returned by dld */
extern int dld_undefined_sym_count; /* # of undefined global symbols */
extern char* dld_last_symbol;       /* valid only if EMULTDEFS */

#ifdef __STDC__
# define P(s) s
#else
# define P(s) ()
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern int
dld_init P((char* path));	    /* initialize the dld routines */
extern int
dld_link P((char* filename));	    /* dynamically link and load an
				       object file */
extern unsigned long
dld_get_symbol P((char* id));	    /* return the address of the named
				       identifier */
extern unsigned long
dld_get_func P((char* func));	    /* return the address of the named
				       function */
extern unsigned long
dld_get_bare_symbol P((char *id));  /* same as dld_get_symbol except
				       that no underscore (_) is
				       prepended.  Use to locate
				       symbols defined by assembly
				       routines. */

extern int
dld_unlink_by_file P((char* path, int hard)); /* unlink a file */
extern int
dld_unlink_by_symbol P((char* id, int hard)); /* unlink the module
						 that define the given
						 symbol */

extern int
dld_function_executable_p P((char* func)); /* return true if the named
					      C function is executable */

extern char **
dld_list_undefined_sym P((void));   /* return an array of undefined
				       symbols */

extern char *
dld_find_executable P((char* command));	/* return the full path name
					   of the given executable
					   file. */

extern int
dld_create_reference P((char* name)); /* explicitly create a reference
					 to the given symbol. */

extern int
dld_define_sym P((char* name, unsigned int siye)); /* explicitly
						      define the value
						      for the given
						      symbol. */

extern void
dld_remove_defined_symbol P((char* name)); /* remove an explicitly
					      defined symbol */

extern void
dld_perror P((char* user_mesg));    /* print error messages. */

extern char
*dld_strerror P((int code));	    /* returns the error message */

extern void
dyn_load P((char * name));	    /* from gxxload.cc (c++) */
extern void
dyn_unload P((char * name));

#ifdef __cplusplus
}
#endif
