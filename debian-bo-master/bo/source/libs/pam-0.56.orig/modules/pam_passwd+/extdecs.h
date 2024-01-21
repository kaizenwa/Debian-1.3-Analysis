
/* load.c */

void cleanup_load (struct _options *opt);
void loadsig (struct _options *opt, char *number);
void initpw (struct _options *opt);
int loadgecos (struct _options *opt, char *format);
void setvar (struct _options *opt, char *buf);
void loadlevel (struct _options *opt, char *line);
struct intvar *findiv (int c);

/* log.c */

void initlog (struct _options *opt);
void beginlog (struct _options *opt, const char *line);
void logfunc (struct _options *opt, unsigned int flag, const char *fmt, ...);
void plus_logout(struct _options *opt, int logno, const char *fmt, va_list ap);
void endlogging (struct _options *opt, int logno);
void paterr(struct _options *opt, const char *msg);
void sysyyerror (struct _options *opt, char *msg);

/* pattern.c */

void free_pattern (struct _options *opt);
int smatch (struct _options *opt, char *pat);
int match (struct _options *opt, char *str);

/* prog.c */

int mgets (struct _options *opt, char *buf, int n, struct _IO_FILE *fp);

int strfp (struct _options *opt, int neg, char *str,
	   char *name, struct _IO_FILE *(*fopn) (const char *, const char *),
	   int (*fclo) (struct _IO_FILE *));

int patfp(struct _options *opt, int neg, char *pat, char *name,
	  struct _IO_FILE *(*fopn) (const char *, const char *),
	  int (*fclo) (struct _IO_FILE *));

int patinfp (struct _options *opt, int neg, char *str, char *name,
	     struct _IO_FILE *(*fopn) (const char *, const char *),
	     int (*fclo) (struct _IO_FILE *));

int firstline (struct _options *opt, char *name, char *buf, int nbuf,
	       struct _IO_FILE *(*fopn) (const char *, const char *),
	       int (*fclo) (struct _IO_FILE *));

/* test.y */

void test_yyerror (const char *msg, void *yyparse_opt);
int passtest (struct _options *opt, char *buf);

/* util.c */

char *tonum (int n);
char *sfmt (char *ptr, int sgn, int n1, int n2, int num,
	    char *string, char *length);
char *nfmt (char *ptr, int sgn, char *string, char *length);
char *getcstring (char *from, char *to, int quo);
int findhost (char *name, int siznam);
int finddomain (char *name, int siznam);

/* verify.c */

void escape_password (struct _options *opt,
		      char *src_ptr,
		      char *dst_ptr);
int verify_password (struct _options *opt);

/* pam_passwd+.c */

void do_converse (struct _options *opt, int is_error, const char *str);
void _pam_log_error (const char *, ...);
