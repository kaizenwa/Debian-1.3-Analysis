extern void slrn_read_startup_file (char *);
extern void slrn_startup_initialize (void);
extern char *slrn_map_file_to_host (char *);
extern int Slrn_Autobaud;
extern char *Slrn_Score_File;
extern int Slrn_Scroll_By_Page;

extern int slrn_set_string_variable (char *, char *);
extern int slrn_set_integer_variable (char *, int);
extern int slrn_get_variable_value (char *, int *, char ***, int **);
extern int slrn_get_authorization (char *, char **, char **);
