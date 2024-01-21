/*
 * mproto.h -- prototypes for functions common to several modules.
 */

char	*db_string	Params((noargs));
char	*makename	Params((char *dest,char *d,char *name,char *e));
char	*pathfind	Params((char *buf, char *path, char *name, char *extn));
char	*salloc	Params((char *s));
char	*spec_str	Params((char *s));
char	*str_install	Params((struct str_buf *sbuf));
int	cmp_pre	Params((char *pre1, char *pre2));
int	db_open	Params((char *s, char **lrgintflg));
int	db_tbl	Params((char *section, struct implement **tbl));
int	ppch	Params((noargs));
int	ppinit	Params((char *fname, int m4flag));
int	prt_i_str	Params((FILE *f, char *s, int len));
int	redirerr	Params((char *p));
int	smatch	Params((char *s,char *t));
int	tonum	Params((int c));
long	longwrite	Params((char *s,long len,FILE *file));
long	millisec	Params((noargs));
novalue clear_sbuf	Params((struct str_buf *sbuf));
novalue cset_init	Params((FILE *f, unsigned short *bv));
novalue	db_chstr	Params((char *s1, char *s2));
novalue	db_close	Params((noargs));
novalue	db_code	Params((struct implement *ip));
novalue	db_dscrd	Params((struct implement *ip));
novalue	db_err1	Params((int fatal, char *s1));
novalue	db_err2	Params((int fatal, char *s1, char *s2));
novalue free_stbl	Params((noargs));
novalue	getctime	Params((char *sbuf));
novalue	getitime	Params((struct cal_time *ct));
novalue	id_comment	Params((FILE *f));
novalue	init_sbuf	Params((struct str_buf *sbuf));
novalue	init_str	Params((noargs));
novalue	new_sbuf	Params((struct str_buf *sbuf));
novalue	nxt_pre Params((char *pre, char *nxt, int n));
novalue	ppdef	Params((char *name, char *value));
novalue	ppecho	Params((noargs));
pointer	alloc	Params((unsigned int n));
struct	fileparts *fparse	Params((char *s));
struct	il_code *new_il	Params((int il_type, int size));
struct	implement	*db_ilkup    Params((char *id, struct implement **tbl));
struct	implement	*db_impl     Params((int oper_typ));
unsigned short   *bitvect    Params((char *image, int len));

#ifndef SysMem
pointer	memcopy		Params((char *to, char* from, word n));
pointer	memfill		Params((char *to, int con, word n));
#endif					/* SysMem */

#ifndef SysOpt
#ifndef Linux
int	getopt	Params((int nargs, char **nargv, char *ostr));
#endif					/* Linux */
#endif					/* SysOpt */

#if IntBits == 16
long	lstrlen	Params((char *s));
novalue	lqsort	Params((char *base, int nel, int width, int (*cmp)()));
#endif					/* IntBits == 16 */

#define NewStruct(type)\
   (struct type *)alloc((unsigned int) sizeof (struct type))
