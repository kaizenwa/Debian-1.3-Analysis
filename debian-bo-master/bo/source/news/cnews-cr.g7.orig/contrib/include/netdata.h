#ifndef ARTBATCHSIZE
# define ARTBATCHSIZE 8192
#endif

struct netdata {
	FILE *nd_fp;		/* temporary file */
	sizeint nd_bytes;	/* total bytes */
	sizeint nd_spilled;	/* number of bytes spilled to nd_fp */
	sizeint nd_bufcnt;	/* number of bytes in nd_buf */
	char nd_buf[ARTBATCHSIZE];
};

extern char *net_readline(/* FILE *fp, sizeint *lenp */);
extern char *net_cmdread(/* FILE *fp, sizeint *lenp */);
extern int net_endwrite(/* FILE *fp */);
extern int net_writeline(/* char *cp, sizeint len, FILE *fp */);
extern int net_ackwrite(/* char *cp, sizeint len,char *str, FILE *fp */);
extern struct netdata *net_new(/* void */);
extern int net_getdata(/* FILE *fp, struct netdata *ndp */);
extern int net_timeout(/* int timeout */);
extern char *net_getreply(/* FILE *fp, sizeint *lenp,
			  void (* pfunc)(char *line, sizeint *lenp, char *arg),
			  char *arg */);
			  
#define NULLFUNC	((void (*)()) 0)
