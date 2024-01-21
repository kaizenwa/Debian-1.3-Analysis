/*
 * Prototypes for functions in icont.
 */

novalue	addinvk			Params((char *name, int n));
novalue	addlfile		Params((char *name));
pointer	alloc			Params((unsigned int n));
novalue	alsolink		Params((char *name));
int	blocate			Params((word s));
struct	node *c_str_leaf	Params((int type,struct node *loc_model,
				   char *c));
novalue	codegen			Params((struct node *t));
novalue	constout		Params((FILE *fd));
novalue	dummyda			Params((noargs));
struct	fentry *flocate		Params((word id));
struct	fileparts *fparse	Params((char *s));
novalue	gencode			Params((noargs));
novalue	gentables		Params((noargs));
int	getdec			Params((noargs));
word	getid			Params((noargs));
long	getint			Params((int i, word *wp));
int	getlab			Params((noargs));
struct	lfile *getlfile		Params((struct lfile * *lptr));
int	getoct			Params((noargs));
int	getopc			Params((char * *id));
double	getreal			Params((noargs));
word	getrest			Params((noargs));
word	getstr			Params((noargs));
word	getstrlit		Params((int l));
struct	gentry *glocate		Params((word id));
novalue	gout			Params((FILE *fd));
struct	node *i_str_leaf	Params((int type,struct node *loc_model,char *c,
				   int d));
int	ilink			Params((char * *ifiles,char *outname));
novalue	install			Params((char *name,int flag,int argcnt));
word	instid			Params((char *s));
struct	node *int_leaf		Params((int type,struct node *loc_model,int c));
int	klookup			Params((char *id));
int	lexeql			Params((int l,char *s1,char *s2));
novalue	lfatal			Params((char *s1,char *s2));
novalue	linit			Params((noargs));
novalue	lmfree			Params((noargs));
novalue	loc_init		Params((noargs));
novalue	locinit			Params((noargs));
novalue	lout			Params((FILE *fd));
novalue	lwarn			Params((char *s1,char *s2,char *s3));
char	*makename		Params((char *dest,char *d,char *name,char *e));
novalue	newline			Params((noargs));
novalue	nfatal			Params((struct node *n,char *s1,char *s2));
novalue putconst		Params((int n,int flags,int len,word pc,
				   union xval *valp));
novalue	putfield		Params((word fname,struct gentry *gp,int fnum));
struct	gentry *putglobal	Params((word id,int flags,int nargs,
				   int procid));
char	*putid			Params((int len));
word	putident		Params((int len, int install));
int	putlit			Params((char *id,int idtype,int len));
int	putloc			Params((char *id,int id_type));
novalue	putlocal		Params((int n,word id,int flags,int imperror,
				   word procname));
novalue	quit			Params((char *msg));
novalue	quitf			Params((char *msg,char *arg));
novalue	readglob		Params((noargs));
unsigned int round2		Params((unsigned int n));
novalue	rout			Params((FILE *fd,char *name));
char	*salloc			Params((char *s));
novalue	scanrefs		Params((noargs));
novalue	sizearg			Params((char *arg,char * *argv));
int	smatch			Params((char *s,char *t));
pointer	tcalloc			Params((unsigned int m,unsigned int n));
novalue	tfatal			Params((char *s1,char *s2));
novalue	tmalloc			Params((noargs));
novalue	tmfree			Params((noargs));
novalue	tminit			Params((noargs));
int	trans			Params((char * *ifiles));
pointer trealloc		Params((pointer table, pointer tblfree,
                                  unsigned int *size, int unit_size,
                                  int min_units, char *tbl_name));
struct	node *tree1		Params((int type));
struct	node *tree2		Params((int type,struct node *loc_model));
struct	node *tree3		Params((int type,struct node *loc_model,struct node *c));
struct	node *tree4		Params((int type,struct node *loc_model,struct node *c,struct node *d));
struct	node *tree5		Params((int type,struct node *loc_model,
				   struct node *c,struct node *d,
				   struct node *e));
struct	node *tree6		Params((int type,struct node *loc_model,
				   struct node *c, struct node *d,
				   struct node *e,struct node *f));
struct node *buildarray		Params((struct node *a,struct node *lb,
					struct node *e, struct node *rb));
novalue	treeinit		Params((noargs));
novalue	tsyserr			Params((char *s));
novalue	writecheck		Params((int rc));
novalue	yyerror			Params((int tok,struct node *lval,int state));
int	yylex			Params((noargs));
int	yyparse			Params((noargs));

#ifdef MultipleRuns
novalue	tcodeinit		Params((noargs));
novalue yylexinit		Params((noargs));
#endif					/* MultipleRuns */


#ifdef DeBugTrans
novalue	cdump			Params((noargs));
novalue	gdump			Params((noargs));
novalue	ldump			Params((noargs));
#endif					/* DeBugTrans */

#ifdef DeBugLinker
novalue	idump			Params((char *c));
#endif					/* DeBugLinker */
