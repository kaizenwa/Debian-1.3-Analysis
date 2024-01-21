/* ======================== Standard header files ====================== */

#ifdef vms

/* VAX VMS C flags references to undefined preprocessor symbols with 
irksome warning messages */

#ifndef DEBUG
#define DEBUG 0
#endif

#ifndef DEBUG_MALLOC
#define DEBUG_MALLOC 0
#endif

#ifndef DEBUG_MEMORY
#define DEBUG_MEMORY 0
#endif

#if !defined(__ALPHA)
#ifndef __cplusplus
#define __cplusplus 0
#endif
#endif

#ifndef __STDC__
#define __STDC__ 0
#endif

#ifndef __GNUC__
#define __GNUC__ 0
#endif

#define HAVE_STDLIB_H		1
#define HAVE_NETINET_IN_H	0
#define __NeXT__		0
#define __host_mips		0
#define __hppa			0
#define __sgi			0
#define ardent			0
#define c_plusplus		0
#define sun			0
#define ultrix			0
#endif /* vms */

#include <stdio.h>

#if (__STDC__ || __cplusplus || c_plusplus || HAVE_STDLIB_H)
#include <stdlib.h>
#endif /*  (__STDC__ || __cplusplus || c_plusplus || HAVE_STDLIB_H) */
#include <string.h>
#include <ctype.h>

#ifdef vms
#include <types.h>
#include <stat.h>
#else /* NOT vms */
#include <sys/types.h>
#include <sys/stat.h>
#endif /* vms */

#include <time.h>
#include <errno.h>
#include <limits.h>

#ifndef FILENAME_MAX		/* defined in all Standard C implementations */
#define FILENAME_MAX 1024	/* else use common UNIX value */
				/*  (DEC, NeXT, Sun: 1024, IBM, SGI: 255) */
#endif

#if FILENAME_MAX < 255		/* workaround for HP-UX bug, which sets it to 14! */
#undef FILENAME_MAX
#define FILENAME_MAX 1024
#endif

#ifndef  EXIT_FAILURE		/* defined in all Standard C implementations */
#define	EXIT_FAILURE	1
#endif

#ifndef EXIT_SUCCESS		/* defined in all Standard C implementations */
#define	EXIT_SUCCESS    0
#endif

/* ==================== Machine-specific definitions =================== */

#ifndef MOREPATH			/* can override at compile time */
#define MOREPATH		"/usr/ucb/more"	/* full path name */
#define MORE			"more"		/* argv[0] */
#endif /* MOREPATH */

#if __NeXT__
					/* NeXT lacks unistd.h and malloc.h */
#if __cplusplus
extern "C" {
					/* these routine are defined in */
					/* libc.h, but are absent from */
					/* the C++ runtime library, sigh... */
extern unsigned long htonl (unsigned long hostlong);
extern unsigned short htons (unsigned short hostshort);
extern unsigned long ntohl (unsigned long netlong);
extern unsigned short ntohs (unsigned short netshort);
#define htonl (htonl)
#define htons (htons)
#define ntohl (ntohl)
#define ntohs (ntohs)
};
#endif

#include </usr/include/bsd/libc.h>	/* NB: next absolute path to work */
					/* around broken g++ libc.h */
#else
#ifndef vms
#include <unistd.h>
#endif
#if defined(HAVE_MALLOC_H)
#include <malloc.h>
#endif /* defined(HAVE_MALLOC_H) */
#endif /* __NeXT__ */

#if (__STDC__ || __cplusplus || c_plusplus)
#define VOID	void
#else /* NOT (__STDC__ || __cplusplus || c_plusplus) */
#define VOID
#endif /* (__STDC__ || __cplusplus || c_plusplus) */

#ifdef bcopy
#undef bcopy
#endif
#define bcopy(source,target,length) \
	memcpy((char*)(target),(const char*)(source),length)

#if sun
#if __cplusplus
extern "C" int	_filbuf(FILE *);	/* missing from stdio.h */
#if !__GNUC__
int	_flsbuf(unsigned char,FILE *);	/* missing from stdio.h */
#endif /* !__GNUC__ */
extern "C" int	_flsbuf(unsigned int, FILE *); /* missing from stdio.h */
extern "C" char	*tempnam(const char *, const char *);
					/* not defined by acc's stdio.h */
extern "C" int	waitpid(int, int *, int); /* not defined by any Sun .h file */
#else /* NOT __cplusplus */
int	_filbuf(FILE *);		/* missing from stdio.h */
#if !__GNUC__
int	_flsbuf(unsigned char,FILE *);	/* missing from stdio.h */
#endif /* !__GNUC__ */
char	*tempnam(const char *, const char *); /* not defined by acc's stdio.h */
int	waitpid(int, int *, int);	/* not defined by any Sun .h file */
#endif /* __cplusplus */
#endif /* sun */

#if DEBUG_MALLOC
			/* For dynamic memory debugging. */
			/* Note that the parens around free and malloc */
			/* are essential to prevent macro expansion in */
			/* ANSI/ISO Standard C and C++.  Erroneous */
			/* preprocessors will go into an infinite loop */
			/* (e.g. SunOS /usr/CC/sun4/cpp) */
#if sun
int malloc_debug(int level);
int malloc_verify(void);
#else /* NOT sun */
#define malloc_debug(level)	level
#define malloc_verify()		1
#endif /* sun */
#undef free
#undef malloc
#define free(ptr) (malloc_verify(), \
		fprintf(stderr,"line %d: free(%p)\n",(int)__LINE__,(ptr)), \
		(free)(ptr))
static void *p__;			/* for malloc() debugging */
#define malloc(s) (malloc_verify(),\
		   p__ = (malloc)(s),\
		   fprintf(stderr,"line %d: malloc(%ld) -> %p\n",\
			   (int)__LINE__,(s),(p__)),\
		   p__)
#endif /* DEBUG_MALLOC */

#if __NeXT__
static char* p_;
static char* q_;
/* NB: This is not a general definition of tempnam(), but works for
this program! */
#define tempnam(dir,pfx)	(p_ = tmpnam((char*)NULL), \
				 q_ = (char*)malloc(strlen(p_)+1), \
				 strcpy(q_,p_))
#include <libc.h>			/* for struct rusage definition */
#define waitpid(pid, statusp, options)	wait4((pid), (union wait*)(statusp),\
					      (options), (struct rusage*)0)
#endif /* __NeXT__ */

#if ardent
/* Stardent has only simple wait-for-all-children function, sigh... */
#define waitpid(pid, statusp, options)	wait((int*)0)
char *getenv(const char *name);		/* missing from system header files */
#endif

#if __hppa
#include <sys/wait.h>
#endif /* __hppa */

#if __host_mips && !defined(ultrix) && !defined(__sgi)
#define waitpid(pid, statusp, options)	wait((int*)0)
#include <sys/wait.h>
#endif /* __host_mips && !defined(ultrix) && !defined(__sgi) */

#if __sgi
#include <sys/wait.h>
#endif /* __sgi */

#if ultrix
#include <sys/wait.h>
#endif /* ultrix */

/*
 *  provide ntohs(), ntohl(), htonl() and htons():
 */
#if defined(unix) && !defined(HAVE_NETINET_IN_H)
#define HAVE_NETINET_IN_H	unix
#endif

#if __cplusplus && __GNUC__ && ultrix
#undef HAVE_NETINET_IN_H /* prototypes wrong in <netinet/in.h>, sigh... */
#define HAVE_HTON_NTOH
extern "C" {
u_long	htonl(u_long hostlong);
u_short	htons(u_short hostsort);
u_long	ntohl(u_long netlong);
u_short	ntohs(u_short netshort);
};
#endif

#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#else
#ifndef HAVE_HTON_NTOH
/* define as no-op for standalone: */
#define	htonl(x)	(x)
#define	htons(x)	(x)
#define	ntohl(x)	(x)
#define	ntohs(x)	(x)
#endif /* HAVE_HTON_NTOH */
#endif /* HAVE_NETINET_IN_H */

/* ====================== Program-specific stuff ====================== */

#define FILE_VERSION  	3	/* file format version */
#define MAJOR_VERSION 	2	/* program version     */
#define MINOR_VERSION 	6

#define MAXWORD	  	255	/* maximum length of word indexed */
				/* must be large to support compound word
				indexing in version 2.6 */

#ifndef MAXRESULTS
#define MAXRESULTS	500
#endif

typedef char *	Word;

#ifndef USHRT_MAX
#define USHRT_MAX ((unsigned int) 0xffffL)
#endif

typedef unsigned int Index_t;
#define INDEX_NAN	UINT_MAX	/* "No such index" */
#define INDEX_BUILTIN	(INDEX_NAN-1)	/* Used for builtin abbrevs */

#if 0
#define BADWORDS	{ 	/* words not to index */ \
    "an", "and", "for", "in", "of", "on", "the", "to", "with", 0 \
}
#else

/* Be sure that this list is sorted alphabetically ! */
/*
 * bibindex ignores single letter words automagically. so we omit
 * "a", "e", "i", "l", "n", "o", "s", "t", "y" from this list
 */
/*
 * This is a list of articles, prepositions and pronouns of various,
 * languages, as found in the definition files of our library database.
 */
#define BADWORDS \
{	/* words not to index */ \
	"ab", "aber", "als", "an", "and", "are", "as", "auf", "aus", "az", \
	"bei", "bir", "but", \
	"da", "das", "dat", "de", "dei", "dem", "den", "der", /* "des", */ \
		"det", "di", "die", /* "dos", */ \
	"een", "eene", "egy", "ei", "ein", "eine", "einen", "einer", \
		"eines", "eit", "el", "en", "er", "es", "et", "ett", "eyn", \
		"eyne", \
	"for", "from", "fuer", "fur", \
	"gl", "gli", \
	"ha", "haben", "had", "hai", "has", "hat", "have", "he", "heis", \
		"hen", "hena", "henas", "het", "hin", "hinar", \
		"hinir", "hinn", "hith", "ho", "hoi", \
	"il", "in", "ist", \
	"ka", "ke", \
	"la", "las", "le", "les", "lo", "los", \
	"mia", "mit", \
	"na", "nji", "not", \
	"oder", "on", "of", "or", "os", "others" /* magic for bibtex */, \
	"sie", "sind", "so", \
	"ta", "the", "to", \
	"um", "uma", "un", "una", "und", "une", "uno", "unter", \
	"von", \
	"with", \
	"yr", \
	0 }
#endif

/* characters which cannot appear in keywords or abbreviations */
#define NONKEYCHARS	",\n\t \"#%'()={}"    /* See LaTeX book, p.143 */

#if defined(USE_MEMIO)

/* Memory-mapped input is very much faster for biblook, because it
avoids a large number of system calls to fread() for small amounts of
data.  On a 3MB test bibliography on a Sun SPARCstation LX, the
biblook startup time with USE_MEMIO is 75 times faster! */

#include "memio.h"
#define	fclose(fp)			mfclose(fp)
#undef feof
#define feof(fp)			mfeof(fp)
#define fgetc(fp)			mfgetc(fp)
#define	fopen(fn,mode)			mfopen(fn,mode)
#define	fread(buf,size,count,fp)	mfread(buf,size,count,fp)
#define	fseek(fp,offset,direction)	mfseek(fp,offset,direction)
#define	ftell(fp)			mftell(fp)

#undef getc
#define	getc(fp)			mgetc(fp)
					/* want macro mgetc, not function */

#undef getchar
#define getchar()			(getchar)()
					/* == getc(stdin), but need original */

#define ungetc(c,fp)			mungetc(c,fp)
#endif /* defined(USE_MEMIO) */
