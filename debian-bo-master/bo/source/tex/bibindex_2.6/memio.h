/***********************************************************************
This header, memio.h, provides an interface to the memory-mapped input
functions defined in memio.c.  See that file for a detailed description
of the facilities.
***********************************************************************/

#include <stdio.h>

#if defined(FOPEN_MAX)
#define MOPEN_MAX	FOPEN_MAX	/* ANSI/ISO Standard C symbol */
#else
#define MOPEN_MAX	20
#endif

typedef struct
{
    FILE		*fp;		/* handle for open file */
    unsigned char	*start;		/* start of file */
    unsigned char	*pos;		/* next available byte of file */
    unsigned char	*end;		/* one byte BEYOND end of file */
} MEMFILE;

extern MEMFILE memfiles_[MOPEN_MAX];

#define MPOS_SUCCESS	(0)
#define MPOS_FAILURE	(-1)

#define MSEEK_SUCCESS	(0)
#define MSEEK_FAILURE	(-1)

#ifndef SEEK_SET			/* missing on SunOS 4.1.3 */
#define SEEK_SET        0
#define SEEK_CUR        1
#define SEEK_END        2
#endif

int		(mfclose)(FILE *fp_);
int		(mfeof)(FILE *fp_);
int		(mfgetc)(FILE *fp_);
char		*(mfgets)(char *s_, int n_, FILE *fp_);
int		(mgetpos)(FILE *fp_, fpos_t *position_);
FILE		*(mfopen)(const char *filename_, const char *mode_);
/* This should really say (mfread), but a g++ bug prevents compilation */
size_t		mfread(void *buf_, size_t size_, size_t count_, FILE *fp_);
void		(mrewind)(FILE *fp_);
int		(mfseek)(FILE *fp_, long offset_, int direction_);
int		(msetpos)(FILE *fp_, const fpos_t *position_);
long		(mftell)(FILE *fp_);
int		(mungetc)(int c_, FILE *fp_);

/* Define fast inline macros for several of the functions.  These do
NOT test fp for validity!  Parenthesize the function name if you
really want the slower and safer external function version.  On most
systems, fileno() is a fast inline macro, but on HP 9000/7xx and 8xx
systems with HP-UX 8.x or 9.x, fileno() is a slow function call, so it
is important to minimize calls to it by saving its returned value. */

static int mf_fileno_fp_;		/* used internally in these macros */

#define mfeof(fp_)	(mf_fileno_fp_ = (int)fileno(fp_), \
			 (memfiles_[mf_fileno_fp_].pos > \
			  memfiles_[mf_fileno_fp_].end) ? 1 : 0)

#define mgetc(fp_) (mf_fileno_fp_ = (int)fileno(fp_), \
			(int)((memfiles_[mf_fileno_fp_].pos >= \
			  memfiles_[mf_fileno_fp_].end) \
			  ? (memfiles_[mf_fileno_fp_].pos++, EOF) : \
			 *memfiles_[mf_fileno_fp_].pos++))

/* NB: unlike the function form, this macro ALWAYS modifies *position_ */
#define mgetpos(fp_,position_)	((int)(*position_ = mftell(fp_), \
				       (*position_ == EOF) \
				       ? MPOS_FAILURE : MPOS_SUCCESS))

static size_t mread_have_, mread_nbytes_, mread_want_; /* temporary storage */
#define mfread(buf_,size_,count_,fp_) \
	(mf_fileno_fp_ = (int)fileno(fp_), \
	mread_want_ = size_ * count_, \
	mread_have_ = (size_t)(memfiles_[mf_fileno_fp_].end - \
			       memfiles_[mf_fileno_fp_].pos), \
	((mread_have_ == 0) \
	 ? memfiles_[mf_fileno_fp_].pos = memfiles_[mf_fileno_fp_].end + 1 \
	 : 0), \
	mread_nbytes_ = (mread_want_ < mread_have_) \
			? mread_want_ : mread_have_, \
	mread_nbytes_ = (mread_nbytes_ > 0) ? mread_nbytes_ : 0, \
	(void)memcpy(buf_,(void*)memfiles_[mf_fileno_fp_].pos, \
		     mread_nbytes_), \
	memfiles_[mf_fileno_fp_].pos += mread_nbytes_, \
	mread_nbytes_/size_)

#define mrewind(fp_)		(void)mfseek(fp_,0L,SEEK_SET)

#define msetpos(fp_,position_)	\
	((int)((mfseek(fp_,*position_,SEEK_SET) == MSEEK_FAILURE) \
	       ? MPOS_FAILURE : MPOS_SUCCESS))

#define mftell(fp_)	(mf_fileno_fp_ = (int)fileno(fp_), \
			(long)(memfiles_[mf_fileno_fp_].pos - \
				memfiles_[mf_fileno_fp_].start))

/* A real ungetc() would push c back into the input stream, but input
file has been mapped readonly, so we can only backup.  Usually, the
same character is pushed that was just read, so this is likely
okay. */

#define mungetc(c_,fp_)	(mf_fileno_fp_ = (int)fileno(fp_), \
			(int)((c_ == EOF) ? EOF : \
	       (((memfiles_[mf_fileno_fp_].pos > \
		  memfiles_[mf_fileno_fp_].end) ? \
		 memfiles_[mf_fileno_fp_].pos = \
		 memfiles_[mf_fileno_fp_].end : 0), \
		memfiles_[mf_fileno_fp_].pos--, \
		c_)))
