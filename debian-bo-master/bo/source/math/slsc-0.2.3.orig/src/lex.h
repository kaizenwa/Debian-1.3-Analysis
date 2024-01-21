extern int yylex (void);
extern void yyerror (char *);
extern void initkbd (void);
extern void kbd_again (void);
extern void resetkbd (void);
extern int atocol (char *, int);
extern int yyparse (void);
extern int sc_atocol (char *, int);
extern char *slsc_strtof(char *, double *);
