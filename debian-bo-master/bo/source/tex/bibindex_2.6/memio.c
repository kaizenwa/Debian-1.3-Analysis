/***********************************************************************

		 *************************************
		 * This code is in the PUBLIC DOMAIN *
		 *************************************

============
INTRODUCTION
============

This file provides a convenient interface to memory-mapped input files
on UNIX systems that have mmap()/munmap() and <sys/mman.h> (see below
for a list of systems).  Input from such files can be twice as fast as
that provided by UNIX fread().

This code is written in ANSI/ISO Standard C, and requires a Standard C
or C++ compiler to compile.


==========================
TESTING THE IMPLEMENTATION
==========================

Compile this file with the preprocessor symbol TEST defined to get a
standalone test program.  When executed, the test program will copy to a
temporary file the file(s) named on the command line, exercise the other
primitives, and do a timing comparison of memory-mapped input with
standard input using fread(), getc(), and fgetc().  A sample build and
run of the test program on a SPARCstation LX running Solaris 2.2
produced this output:

% CC -DTEST -o memio -O4 memio.c
% ./memio /usr/openwin/bin/xnews
Copying file [/usr/openwin/bin/xnews] to temporary file
	343 blocks 2804828 bytes
	Comparing files
	Comparing files with fseek()/mfseek() SEEK_SET positioning
	Comparing files with fseek()/mfseek() SEEK_CUR positioning
	Comparing files with fseek()/mfseek() SEEK_END positioning
	Comparing files with fgetpos()/fsetpos() positioning
	CPU timing (microsec)
		mfopen        1 fopen         1 ratio 1.000
		mfread   219999 fread    519999 ratio 0.423
		mfclose       1 fclose        1 ratio 1.000
		Total    220001 Total    520001 ratio 0.423
	Input rates
		MB/sec (mfread) 12.749  MB/sec (fread)  5.394   ratio  2.364
		MB/sec (mfread) 12.749  MB/sec (getc)   2.357   ratio  5.409
		MB/sec (mfread) 12.749  MB/sec (fgetc)  1.241   ratio 10.273
		MB/sec (mfread) 12.749  MB/sec (mgetc)  2.318   ratio  5.500
		MB/sec (mfread) 12.749  MB/sec (mfgetc) 1.031   ratio 12.364

Sample compilations:

Hewlett-Packard 9000/7xx HP-UX 9.0	c89 -c -D_HPUX_SOURCE memio.c
					CC -c memio.c (but mmap and
					munmap missing from C++ run-time
					library)

Hewlett-Packard 9000/375 BSD 4.3	cc -c memio.c (but test program
					fails because mmap() fails)

MIPS RC6280		EP/IX 2.1.1	cc -c -Dfpos_t=long memio.c (but
					test program fails to link because
					fgetpos() and fsetpos() are missing
					from library)

IBM RS/6000		AIX 3.2		cc -c memio.c
					xlC -c memio.c

Sun SPARC		SunOS 4.1.3	acc -c memio.c
					gcc -c memio.c
					g++ -c memio.c
Sun SPARC		Solaris 2.2	cc -c memio.c
					CC -c memio.c
					gcc -c memio.c
					g++ -c memio.c
Silicon Graphics	IRIX 4.0.5	cc -c memio.c
					gcc -c memio.c
					g++ -c memio.c
					CC -c memio.c

The open and close times are artificially small because they take less
time than the CPU timer resolution, which on many UNIX systems is 60 or
100 per ticks/sec.


================
PUBLIC INTERFACE
================

The functionality is identical to the Standard C fopen/fclose et al,
with functions having the prefix letter `m'.

After a successful mfopen(), and before a matching mfclose(), only these
functions should be used to refer to the file.  References to the
Standard C functions will work, but will not access the memory-mapped
copy of the file.  Functions that do not require access to the file
data, such as fileno() and fstat(), are perfectly safe to use.

Not all functions present in Standard I/O have equivalents here.
Notably absent are all output functions, and equivalents of ferror(),
fileno(), freopen(), fscanf(), getchar(), gets(), scanf(), setbuf(), and
setvbuf().  [Actually, mfileno() is provided, but only as an internal
private fucntion.]

	#include "memio.h"

		Include <stdio.h>, define function prototypes, and definitions
		of preprocessor symbols MPOS_FAILURE, MPOS_SUCCESS,
		MSEEK_FAILURE, and MSEEK_SUCCESS.

		In the interests of speed, most of the shorter functions have
		definitions in memio.h as inline macros that do NOT check the
		file handle for validity.  If you want the function versions
		which DO validate the file handle, you can get them by
		protecting the name with parentheses (cf. mgetc(fp)
		vs. (mgetc)(fp)).  However, following the practice of ISO/ANSI
		Standard C (15-Dec-1989), mfgetc() is guaranteed to be a
		function, not a macro.

	int mfclose(FILE *fp)

		Close the file, returning 0 on success, and EOF on failure.

	int mfeof(FILE *fp)

		Return non-zero if end-of-file has previously been detected
		reading the file, else zero.

	int mfgetc(FILE *fp)

		Return next character, or EOF at end-of-file.  This is a
		function call with validity checking.

	char *mfgets(char *s, int n, FILE *fp)
		Read into s at most one less than the number of bytes specified
		by n from the file pointed to by fp.  No additional characters
		are read after a newline character (which is retained) or after
		end-of-file.  A NUL character is written immediately after the
		last character read into s.

	int mgetc(FILE *fp)

		Return next character, or EOF at end-of-file.  This is an
		inline macro, with NO validity checking.

	int mgetpos(FILE *fp, fpos_t *position)

		Return MPOS_SUCCESS or MPOS_FAILURE.  On success, set *position
		to a magic cookie representing the byte offset of the current
		file position.

	FILE *mfopen(const char *filename, const char *mode)

		Return open file descriptor, or NULL on failure.  Only mode
		values of "r" or "rb" are accepted, since most UNIX systems do
		not support extending output files through the mmap() system
		call.

	size_t mfread(void *buf, size_t size, size_t count, FILE *fp)

		Read up to count items of size size into buf, and return count
		of items read, or 0 on failure.

	void mrewind(FILE *fp)

		Rewind the file, ignoring errors.

	int mfseek(FILE *fp, long offset, int direction)

		Position according to offset and direction (see documentation
		of fseek()), and return MSEEK_SUCCESS or MSEEK_FAILURE.

	int msetpos(FILE *fp, fpos_t *position)

		Position to *position, a magic cookie previously returned by
		mgetpos(), and return MPOS_SUCCESS or MPOS_FAILURE.

	long mftell(FILE *fp)

		Return current byte offset in file, or EOF on failure.

	int mungetc(int c, FILE *fp)

		Push the character c back into the input stream associated with
		file fp, unless c is EOF, in which case, suppress the pushback.
		Return c.


=================
USING THE PACKAGE
=================

Rather than modify existing code to call these routines directly, it is
better to use the preprocessor to define them directly, with code like
this:

#ifdef USE_MEMIO
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

#undef getchar
#define getchar()			(getchar)()

#define ungetc(c,fp)			mungetc(c,fp)
#endif

You must examine your code to ensure that all references to Standard C
output functions, and to the missing ones (fscanf() et al, see above),
are parenthesized to avoid calling the memory-mapped ones.  If you need
fscanf(), consider using mfgets() and sscanf() instead.  If you use both
getc() and getchar() in the same code, then only one of them can be
expanded inline, because stdin cannot be memory mapped if it is not
directed to a file.  The recommended solution above maps getc() to mgetc()
(an inline macro), and getchar() to the function.  Too bad that C's
preprocessor macro language is not powerful enough to save the old
definition of a macro, like one can in Lisp and TeX, because one could
then preserve getchar() as an inline macro.

Because the input file is mapped read-only, mungetc() will not actually
put back a character.  Normally, this doesn't matter, but it is legal,
if rare, for ungetc() to put back a different character than was most
recently read, and in such a case, mungetc() and ungetc() are NOT
equivalent.


======
AUTHOR
======

Nelson H. F. Beebe
Center for Scientific Computing
Department of Mathematics
University of Utah
Salt Lake City, UT 84112
USA
Tel: 1 801 581 5254
FAX: 1 801 581 4148
Email: beebe@math.utah.edu (Internet)

[20-Sep-1993]
***********************************************************************/

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>

#if __hppa && __cplusplus
extern "C" {
#endif
#include <sys/mman.h>
#if __hppa && __cplusplus
};
#endif

#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0		/* Sun has, SGI lacks */
#endif

#if defined(__NeXT__) || defined(ultrix)
/* declarations missing from <sys/mman.h> */
#if __cplusplus
extern "C" {
#endif
caddr_t mmap(caddr_t, size_t, int, int, int, off_t);
int munmap(caddr_t, size_t);
#if __cplusplus
};
#endif
#endif /* defined(__NeXT__) || defined(ultrix) */

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif /* EXIT_SUCCESS */

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif /* EXIT_FAILURE */

#include "memio.h"

MEMFILE memfiles_[MOPEN_MAX];

#ifdef MAX
#undef MAX
#endif

#define MAX(a,b)	(((a) > (b)) ? (a) : (b))

#ifdef MIN
#undef MIN
#endif

#define MIN(a,b)	(((a) < (b)) ? (a) : (b))

static int 		(mfileno)(FILE *fp_);

/* Make mfileno an inline macro for speed, since it is heavily used */

#if __GNUC__
/* simplify to suppress warning "comparison is always 1 due to limited 
range of data type" */
#define mfileno(fp)	( (fp != (FILE*)NULL) && \
			 ((int)fileno(fp) < MOPEN_MAX) && \
			 (memfiles_[fileno(fp)].fp == fp) ) \
			? ((int)fileno(fp)) : (EOF)
#else
#define mfileno(fp)	( (fp != (FILE*)NULL) && \
			 (0 <= (int)fileno(fp)) && \
			 ((int)fileno(fp) < MOPEN_MAX) && \
			 (memfiles_[fileno(fp)].fp == fp) ) \
			? ((int)fileno(fp)) : (EOF)
#endif


int (mfclose)(FILE *fp)		/* return 0 on success, EOF on failure */
{
    register int mf = mfileno(fp);
    int retval;

    if (mf == EOF)
	return(EOF);
    {
	retval = munmap((caddr_t)memfiles_[mf].start,
			(size_t)(memfiles_[mf].end - memfiles_[mf].start));
	memfiles_[mf].fp = (FILE*)NULL;
	memfiles_[mf].pos = (unsigned char*)NULL;
	memfiles_[mf].start = (unsigned char*)NULL;
	memfiles_[mf].end = (unsigned char*)NULL;
	fclose(fp);
	return ((retval == 0) ? 0 : EOF);
    }
}

int (mfeof)(FILE *fp)	/* return non-0 at end-of-file has been read, else 0 */
{
    register int mf = mfileno(fp);

    return (((mf == EOF) || (memfiles_[mf].pos > memfiles_[mf].end)) ? 1 : 0);
}

static int (mfileno)(FILE *fp)	/* return EOF on failure, else file handle */
{
    if ( (fp != (FILE*)NULL) &&
#if !__GNUC__
/* simplify to suppress warning "comparison is always 1 due to limited 
range of data type" */
	(0 <= (int)fileno(fp)) &&
#endif
	((int)fileno(fp) < MOPEN_MAX) &&
	(memfiles_[fileno(fp)].fp == fp) )
	return ((int)fileno(fp));
    else
	return (EOF);
}

int (mfgetc)(FILE *fp)	/* return next character, or EOF at end-of-file */
{
    register int mf = mfileno(fp);
    register int c;

    /* WARNING: the definition here is tricky, because mfeof() must not */
    /* return non-zero until EOF has been returned at least once.  We */
    /* therefore allow pos to advance beyond end; mfeof() is true when */
    /* pos > end, NOT when pos == end! */
    if ((mf == EOF) || (memfiles_[mf].pos >= memfiles_[mf].end))
	c = EOF;
    else
	c = (int)*memfiles_[mf].pos;
    memfiles_[mf].pos++;	/* can advance beyond memfiles_[mf].end */
    return (c);
}

int (mgetpos)(FILE *fp, fpos_t *position)
{				/* return MPOS_SUCCESS or MPOS_FAILURE */
    long pos;

    pos = mftell(fp);

    if (pos == EOF)
	return (MPOS_FAILURE);
    else
    {
	*position = (fpos_t)pos;
	return (MPOS_SUCCESS);
    }
}

char *(mfgets)(char *s, int n, FILE *fp)
{
    register size_t k;
    register size_t limit;
    register int mf = mfileno(fp);
    register unsigned char *t = (unsigned char *)s;

    if (mf == EOF)
	return ((char*)NULL);

    limit = MIN(n,(size_t)(memfiles_[mf].end - memfiles_[mf].pos));
    limit--;				/* must reserve space for final NUL */

    if (limit <= 0)
    {
	memfiles_[mf].pos = memfiles_[mf].end + 1; /* so mfeof() is non-zero */
	return ((char*)NULL);
    }

    for (k = 0; k < limit; ++k)
    {
	*t = *memfiles_[mf].pos++;
	if (*t++ == '\n')
	    break;
    }
    *t = '\0';
    return (s);
}

FILE *(mfopen)(const char *filename, const char *mode)
{			/* return open file descriptor, or NULL on failure */
    register const char *p;
    FILE *fp;
    register int mf;
    struct stat statbuf;
    caddr_t fmap;

    for (p = mode; *p; ++p)	/* check for mode = "r" or "rb" */
    {
	switch (*p)
	{
	case 'r':
	case 'b':
	    break;
	default:
	    return ((FILE*)NULL);
	}
    }
    fp = fopen(filename,mode);
    if (fp == (FILE*)NULL)
	return ((FILE*)NULL);

    mf = (int)fileno(fp);
    if ((mf < 0) || (mf > MOPEN_MAX))
    {
	(void)fclose(fp);
	return ((FILE*)NULL);
    }
    memfiles_[mf].fp = fp;

    (void)fstat((int)fileno(fp),&statbuf);
    fmap = (caddr_t)mmap((caddr_t)NULL, (size_t)statbuf.st_size,
			 PROT_READ, MAP_SHARED | MAP_NORESERVE,
			 (int)fileno(fp), (off_t)0);
    if (fmap == (caddr_t)(-1))
    {
	(void)fclose(fp);
	memfiles_[mf].fp = (FILE*)NULL;
	return ((FILE*)NULL);
    }

    memfiles_[mf].fp = fp;
    memfiles_[mf].start = (unsigned char*)fmap;
    memfiles_[mf].pos = memfiles_[mf].start;
    memfiles_[mf].end = memfiles_[mf].start + (size_t)statbuf.st_size;
    return (fp);
}

void (mrewind)(FILE *fp)
{
    (void)mfseek(fp,0L,SEEK_SET);
}

int (mfseek)(FILE *fp, long offset, int direction)
{				/* return MSEEK_SUCCESS or MSEEK_FAILURE */
    register int mf = mfileno(fp);

    if (mf == EOF)
	return (MSEEK_FAILURE);
    switch (direction)
    {
    case SEEK_CUR:
	memfiles_[mf].pos = MIN(memfiles_[mf].pos + (size_t)offset,
				memfiles_[mf].end);
	break;

    case SEEK_SET:
	if (offset < 0L)
	    return (MSEEK_FAILURE);
	else
	    memfiles_[mf].pos = MIN(memfiles_[mf].start + (size_t)offset,
				    memfiles_[mf].end);
	break;

    case SEEK_END:
	if (offset > 0L)
	    return (MSEEK_FAILURE);
	else
	    memfiles_[mf].pos = MAX(memfiles_[mf].end - (size_t)offset,
				    memfiles_[mf].start);
	break;

    default:
	return (MSEEK_FAILURE);
    }
    return (MSEEK_SUCCESS);
}

int (msetpos)(FILE *fp, const fpos_t *position)
{				/* return MPOS_SUCCESS or MPOS_FAILURE */
    return (mfseek(fp,*position,SEEK_SET) == MSEEK_FAILURE)
	? MPOS_FAILURE : MPOS_SUCCESS;
}

long (mftell)(FILE *fp)
{				/* return byte offset, or EOF on failure */
    register int mf = mfileno(fp);

    if (mf == EOF)
	return (EOF);
    else
	return (long)(memfiles_[mf].pos - memfiles_[mf].start);
}

int (mungetc)(int c,FILE *fp)
{
    register int mf = mfileno(fp);

    if ((c == EOF) ||
	(mf == EOF) ||
	(memfiles_[mf].pos <= memfiles_[mf].start))
	return (EOF);
    else
    {
	if (memfiles_[mf].pos > memfiles_[mf].end)
	    memfiles_[mf].pos = memfiles_[mf].end;
	memfiles_[mf].pos--;
#if 0
	/* input buffer is in read-only memory, so suppress store */
	*memfiles_[mf].pos = (unsigned char)c;
#endif
	return (c);
    }
}

#ifdef TEST

int		main(int argc_, char *argv_[]);
void		test(const char *filename_);
int		compare_files(FILE *fpmem_,FILE *fpfile_);

int main(int argc, char *argv[])
{
    int k;

    for (k = 1; k < argc; ++k)
    {
	test(argv[k]);
    }
    exit (EXIT_SUCCESS);
}

#define MAXBUF (size_t)8192

void test(const char *filename)
{
    unsigned char bufmem[MAXBUF];
    unsigned char buffile[MAXBUF];
    FILE *fpmem;
    FILE *fpfile;
    int k;
    long nblocks;
    long nbytes;
    size_t nread;
    size_t nwrite;
    fpos_t pos;
    clock_t tfgetc[4];
    clock_t tfile[4];
    clock_t tgetc[4];
    clock_t tmgetc[4];
    clock_t tmfgetc[4];
    clock_t tmem[4];

    fpmem = mfopen(filename,"r");
    if (fpmem == (FILE *)NULL)
    {
	(void)fprintf(stderr,"Cannot open file [%s]\n", filename);
	return;
    }

    fpfile = tmpfile();
    if (fpfile == (FILE*)NULL)
    {
	(void)fprintf(stderr,"Cannot open temporary file\n");
	(void)mfclose(fpmem);
	return;
    }

    (void)fprintf(stderr,"Copying file [%s] to temporary file\n",filename);

    for (nbytes = 0, nblocks = 0;;nbytes += nread, nblocks++)
    {
	nread = mfread(bufmem,1,MAXBUF,fpmem);
	if (nread > 0)
	    nwrite = fwrite(bufmem,1,nread,fpfile);
	else
	    break;
	if (nread != nwrite)
	{
	    (void)fprintf(stderr,
		"\tError: fwrite() wrote %ld bytes from %ld-byte buffer\n",
			  nwrite,nread);
	    (void)mfclose(fpmem);
	    (void)fclose(fpfile);
	    return;
	}
    }

    (void)fprintf(stderr,"\t%ld blocks %ld bytes\n", nblocks, nbytes);

    (void)fprintf(stderr,"\tComparing files\n");
    rewind(fpfile);
    mrewind(fpmem);
    if (compare_files(fpmem,fpfile))
	return;

    (void)fprintf(stderr,
	"\tComparing files with fseek()/mfseek() SEEK_SET positioning\n");
    (void)fseek(fpfile,nbytes/2,SEEK_SET);
    (void)mfseek(fpmem,nbytes/2,SEEK_SET);
    if (compare_files(fpmem,fpfile))
	return;

    (void)fprintf(stderr,
	"\tComparing files with fseek()/mfseek() SEEK_CUR positioning\n");
    (void)fseek(fpfile,nbytes/2,SEEK_SET);
    (void)fseek(fpfile,nbytes/4,SEEK_CUR);
    (void)mfseek(fpmem,nbytes/2,SEEK_SET);
    (void)mfseek(fpmem,nbytes/4,SEEK_CUR);
    if (compare_files(fpmem,fpfile))
	return;

    (void)fprintf(stderr,
	"\tComparing files with fseek()/mfseek() SEEK_END positioning\n");
    (void)fseek(fpfile,-nbytes/2,SEEK_END);
    (void)mfseek(fpmem,-nbytes/2,SEEK_END);
    if (compare_files(fpmem,fpfile))
	return;

    (void)fprintf(stderr,
	"\tComparing files with fgetpos()/fsetpos() positioning\n");
    (void)fseek(fpfile,nbytes/3,SEEK_SET);
    (void)fgetpos(fpfile,&pos);
    rewind(fpfile);
    (void)fsetpos(fpfile,&pos);
    (void)mfseek(fpmem,nbytes/3,SEEK_SET);
    (void)fgetpos(fpmem,&pos);
    mrewind(fpmem);
    (void)fsetpos(fpfile,&pos);
    if (compare_files(fpmem,fpfile))
	return;

    (void)mfclose(fpmem);
    (void)fclose(fpfile);

    /* Finally, read the file with mmap and with stdio, and print
       a comparison of the relative CPU times. */

    tmem[0] = clock();
    fpmem = mfopen(filename,"r");
    tmem[1] = clock();
    for (nbytes = 0, nblocks = 0; ; nbytes += nread, nblocks++)
    {
	nread = mfread(bufmem,1,MAXBUF,fpmem);
	if (nread <= 0)
	    break;
    }
    tmem[2] = clock();
    (void)fclose(fpmem);
    tmem[3] = clock();

    tfile[0] = clock();
    fpfile = fopen(filename,"r");
    tfile[1] = clock();
    for (nbytes = 0, nblocks = 0; ; nbytes += nread, nblocks++)
    {
	nread = fread(buffile,1,MAXBUF,fpfile);
	if (nread <= 0)
	    break;
    }
    tfile[2] = clock();
    (void)fclose(fpfile);
    tfile[3] = clock();

    tgetc[0] = clock();
    fpfile = fopen(filename,"r");
    tgetc[1] = clock();
    while (getc(fpfile) != EOF)
	/* NO-OP */;
    tgetc[2] = clock();
    (void)fclose(fpfile);
    tgetc[3] = clock();

    tfgetc[0] = clock();
    fpfile = fopen(filename,"r");
    tfgetc[1] = clock();
    while (fgetc(fpfile) != EOF)
	/* NO-OP */;
    tfgetc[2] = clock();
    (void)fclose(fpfile);
    tfgetc[3] = clock();

    tmgetc[0] = clock();
    fpfile = mfopen(filename,"r");
    tmgetc[1] = clock();
    while (mgetc(fpfile) != EOF)
	/* NO-OP */;
    tmgetc[2] = clock();
    (void)mfclose(fpfile);
    tmgetc[3] = clock();

    tmfgetc[0] = clock();
    fpfile = mfopen(filename,"r");
    tmfgetc[1] = clock();
    while (mfgetc(fpfile) != EOF)
	/* NO-OP */;
    tmfgetc[2] = clock();
    (void)mfclose(fpfile);
    tmfgetc[3] = clock();

    for (k = 0; k < 3; ++k)	/* prevent zero divides below */
    {
	if (tmem[k] == tmem[k+1])
	    tmem[k+1]++;
	if (tfile[k] == tfile[k+1])
	    tfile[k+1]++;
	if (tgetc[k] == tgetc[k+1])
	    tgetc[k+1]++;
	if (tfgetc[k] == tfgetc[k+1])
	    tfgetc[k+1]++;
	if (tmgetc[k] == tmgetc[k+1])
	    tmgetc[k+1]++;
	if (tmfgetc[k] == tmfgetc[k+1])
	    tmfgetc[k+1]++;
    }

    (void)fprintf(stderr,"\tCPU timing (microsec)\n");
    (void)fprintf(stderr,"\t\tmfopen %8ld\tfopen  %8ld\tratio %.3f\n",
		  (long)(tmem[1] - tmem[0]),(long)(tfile[1] - tfile[0]),
		  (double)(tmem[1] - tmem[0])/(double)(tfile[1] - tfile[0]));
    (void)fprintf(stderr,"\t\tmfread %8ld\tfread  %8ld\tratio %.3f\n",
		  (long)(tmem[2] - tmem[1]),(long)(tfile[2] - tfile[1]),
		  (double)(tmem[2] - tmem[1])/(double)(tfile[2] - tfile[1]));
    (void)fprintf(stderr,"\t\tmfclose%8ld\tfclose %8ld\tratio %.3f\n",
		  (long)(tmem[3] - tmem[2]),(long)(tfile[3] - tfile[2]),
		  (double)(tmem[3] - tmem[2])/(double)(tfile[3] - tfile[2]));
    (void)fprintf(stderr,"\t\tTotal  %8ld\tTotal  %8ld\tratio %.3f\n",
		  (long)(tmem[3] - tmem[0]),(long)(tfile[3] - tfile[0]),
		  (double)(tmem[3] - tmem[0])/(double)(tfile[3] - tfile[0]));
    (void)fprintf(stderr,"\tInput rates\n");
    (void)fprintf(stderr,
		  "\t\tMB/sec (mfread) %.3f\tMB/sec (fread)  %.3f\tratio %.3f\n",
		  (double)nbytes/(double)(tmem[3] - tmem[0]),
		  (double)nbytes/(double)(tfile[3] - tfile[0]),
		  (double)(tfile[3] - tfile[0])/(double)(tmem[3] - tmem[0]));
    (void)fprintf(stderr,
		  "\t\tMB/sec (mfread) %.3f\tMB/sec (getc)   %.3f\tratio %.3f\n",
		  (double)nbytes/(double)(tmem[3] - tmem[0]),
		  (double)nbytes/(double)(tgetc[3] - tgetc[0]),
		  (double)(tgetc[3] - tgetc[0])/(double)(tmem[3] - tmem[0]));
    (void)fprintf(stderr,
		  "\t\tMB/sec (mfread) %.3f\tMB/sec (fgetc)  %.3f\tratio %.3f\n",
		  (double)nbytes/(double)(tmem[3] - tmem[0]),
		  (double)nbytes/(double)(tfgetc[3] - tfgetc[0]),
		  (double)(tfgetc[3] - tfgetc[0])/(double)(tmem[3] - tmem[0]));
    (void)fprintf(stderr,
		  "\t\tMB/sec (mfread) %.3f\tMB/sec (mgetc)  %.3f\tratio %.3f\n",
		  (double)nbytes/(double)(tmem[3] - tmem[0]),
		  (double)nbytes/(double)(tmgetc[3] - tmgetc[0]),
		  (double)(tmgetc[3] - tmgetc[0])/(double)(tmem[3] - tmem[0]));
    (void)fprintf(stderr,
		  "\t\tMB/sec (mfread) %.3f\tMB/sec (mfgetc) %.3f\tratio %.3f\n",
		  (double)nbytes/(double)(tmem[3] - tmem[0]),
		  (double)nbytes/(double)(tmfgetc[3] - tmfgetc[0]),
		  (double)(tmfgetc[3] - tmfgetc[0])/(double)(tmem[3] - tmem[0]));
}

int compare_files(FILE *fpmem,FILE *fpfile)
{				/* return 0 on success, 1 on failure */
    size_t nmem;
    size_t nfile;
    long nblocks;
    long nbytes;
    unsigned char bufmem[MAXBUF];
    unsigned char buffile[MAXBUF];

    for (nbytes = 0L, nblocks = 0L; ;nbytes += nmem, nblocks++)
    {
	nmem = mfread(bufmem,1,MAXBUF,fpmem);
	if (nmem <= 0)
	    break;
	nfile = fread(buffile,1,MAXBUF,fpfile);
	if ((nfile <= 0) || (nfile != nmem))
	    break;
	if (memcmp(bufmem,buffile,nmem) != 0)
	{
	    (void)fprintf(stderr,"\tCompare error: block %ld bytes %ld\n",
			  nblocks, nbytes);
	    (void)mfclose(fpmem);
	    (void)fclose(fpfile);
	    return (1);
	}
	if (mftell(fpmem) != ftell(fpfile))
	{
	    (void)fprintf(stderr,"\tmtell()/ftell() mismatch\n");
	    (void)mfclose(fpmem);
	    (void)fclose(fpfile);
	    return (1);
	}
    }
    if (mfeof(fpmem) != EOF)
	(void)fprintf(stderr,"\tmeof() failure\n");
    return (0);
}
#endif /* TEST */

#if __GNUG__
#undef mfread			/* g++ compiler bug workaround, sigh... */
size_t mfread(void *buf, size_t size, size_t count, FILE *fp)
#else
size_t (mfread)(void *buf, size_t size, size_t count, FILE *fp)
#endif
{			/* return count of items read, or 0 on failure */
    size_t have;
    register int mf = mfileno(fp);
    size_t nbytes;
    size_t want;

    if (mf == EOF)
	return (0);
    else
    {
	want = size * count;
	have = (size_t)(memfiles_[mf].end - memfiles_[mf].pos);
	if (have == 0)
	    memfiles_[mf].pos = memfiles_[mf].end + 1; /* so mfeof() is true */
	nbytes = MIN(want,have);
	nbytes = MAX(nbytes,0);
	(void)memcpy(buf,(void*)memfiles_[mf].pos,nbytes);
	memfiles_[mf].pos += nbytes;
	return (nbytes/size);
    }
}
