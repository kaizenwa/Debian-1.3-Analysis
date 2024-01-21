typedef struct {
	char *sf_name;
	char *sf_tmp;
	FILE *sf_fp;
} SFILE;

extern SFILE *safeopen(/* char *fname, int makedirs */);
extern int safeclose(/* SFILE *sfp */);
extern void safeabort(/* SFILE *sfp */);

extern FILE *safefp(/* SFILE *sfp */);	/* really a macro */
#define safefp(sfp) ((sfp)->sf_fp)
