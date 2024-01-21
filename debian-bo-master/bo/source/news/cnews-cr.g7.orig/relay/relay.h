/* relaynews-specific declarations */

#define MINSHPTRS 30		/* initial value for sh_alloced */

/* All the information needed to describe an article as it is processed. */
struct article {
	statust a_status;	/* article status bits */
	struct headers h;	/* strictly from headers in input */
	char *a_haccum;		/* accumulated output headers, if any */
	char *a_hnext;		/* -> first free byte in a_haccum */
	short a_hpalloced;	/* indices in a_hptrs */
	short a_hpused;		/* indices currently in use */
	char **a_hptrs;		/* -> array of ptrs to lines in a_haccum */
	unsigned a_hbytesleft;	/* in a_haccum */
	char *a_files; /* names for history, added in filing, from h.h_ngs */
	char *a_tmpf;		/* temp link name or first spool dir link */
	FILE *a_artf;		/* stream corresponding to a_tmpf */
	boolean a_unlink;	/* true iff a_tmpf should be unlinked at end */
	boolean a_filed;	/* true iff article has been filed */
	boolean a_xref;		/* true iff Xref: header generated yet */
	boolean a_blvmax;	/* true iff a_unread is to be believed */
	long a_charswritten;	/* into spool directory, for batcher */
	long a_unread;		/* bytes of article input yet unread */
	long a_id;		/* article id #, unique within this batch */
	boolean a_badhdr;	/* true iff non-header is before blank line */
};

struct options {
	boolean	okrefusal;		/* okay to refuse articles? */
	char	*exclude;		/* site to exclude, for erik */
	boolean	histreject;		/* keep history of rejects? */
	long	staledays;		/* articles stale after this many days */
	boolean	genxref;		/* iff true, always generate Xref: */
	boolean	blvxref;		/* iff true, believe Xref: contents */
	char	*blvsite;		/* ... for this site only. */
	boolean	dupsokay;		/* iff true, allow duplicates ... */
	char	*dupsite;		/* ... from this site only. */
	char	*currdir;
};
extern struct options opts;

/* return name of at least one link, for printing in error messages, etc. */
#define spoolnm(art) ((art)->a_unlink? (art)->a_tmpf: (art)->a_files)

#define CONTROL "control"		/* control message pseudo-ng. */
#define JUNK "junk"			/* lost+found pseudo-ng. */

/* imports */
extern void artinit(), artfree();
extern statust loadcaches(), synccaches();
extern void ctlmsg();
extern char *hackhybrid();
extern void filedebug(), fileart();
extern void nnfclose();
extern boolean mkdirs();
extern void fulldisk();
extern statust trclose();
extern void transdebug(), transmit(), trcmd(), trbatch();
