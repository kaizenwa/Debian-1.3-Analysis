/* manp.c */
extern char *cat_manpath(char *manp);
extern char *add_nls_manpath(char *manpathlist, const char *locale);
extern char *add_system_manpath(char *systems, char *manpathlist);
extern char *manpath(char *systems);
extern void create_pathlist(char *manp, char **mp);
extern char *get_mandb_manpath(void);
extern char *global_catpath(char *name);
extern int is_global_mandir(const char *dir);
