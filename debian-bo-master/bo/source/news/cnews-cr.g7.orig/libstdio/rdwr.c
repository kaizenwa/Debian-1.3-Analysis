/*
 * fread and fwrite (optimised version)
 * Could do i/o to/from the user's buffer directly if the transfer is long
 * enough.  If there's a block-aligned block (or more) in the middle, could
 * transfer it directly.
 */

#include <stdio.h>
#include <string.h>			/* the ANSI one, for memcpy */

#ifndef PTR_TYPE
#define	PTR_TYPE char *			/* type of _ptr in stdio.h */
#endif

#ifndef voidstar
#ifdef __STDC__
#define voidstar void *
#else					/* __STDC__ */
#define voidstar char *
#endif					/* __STDC__ */
#endif					/* voidstar */

#ifndef size_type
#ifdef __STDC__
#define size_type size_t
#else					/* __STDC__ */
#define size_type int
#endif					/* __STDC__ */
#endif					/* size_type */

#ifndef THRESHOLD
#define THRESHOLD 12			/* memcpy vs in-line threshold */
#endif

typedef unsigned char putc_t;		/* cast putc args to this type */

size_type
fread(aptr, size, count, fp)
voidstar aptr;
size_type size, count;
register FILE *fp;
{
	register unsigned bytes = count * size;
	unsigned origbytes = bytes;
	char *ptr = aptr;

	while (bytes > 0) {	/* bytes still to be read */
		{
			/* all of bytes in buffer */
			register int copy = fp->_cnt;

			if (copy < 0)
				copy = 0;
			if (copy > bytes)	/* buffer longer than ptr */
				copy = bytes;	/* only fill ptr */
			bytes -= copy;	/* adjust to reflect next loop */
			fp->_cnt -= copy;
			if (copy < THRESHOLD) {
				register char *rptr = ptr,
						*bp = (char *)fp->_ptr;

				for (++copy; --copy > 0; )
					*rptr++ = *bp++;
				ptr = rptr;
				fp->_ptr = (PTR_TYPE)bp;
			} else {
				(void) memcpy(ptr, (char *)fp->_ptr, copy);
				ptr += copy;
				fp->_ptr += copy;
			}
		}
		if (bytes > 0) {	/* more to read, but buffer is empty */
			register int c = getc(fp);	/* refill buffer */

			if (c == EOF)
				break;
			else {
				*ptr++ = c;
				--bytes;
			}
		}
	}
	return size == 0? count: (origbytes - bytes) / size;
}

size_type
fwrite(aptr, size, count, fp)
voidstar aptr;
size_type size, count;
register FILE *fp;
{
	register unsigned bytes = count * size;
	unsigned origbytes = bytes;
	char *ptr = aptr;

	while (bytes > 0) {	/* bytes still to be written */
		{
			register int copy = fp->_cnt;

			if (copy < 0)
				copy = 0;
			if (copy > bytes)	/* buffer longer than ptr */
				copy = bytes;	/* only empty ptr */
			bytes -= copy;	/* adjust to reflect next loop */
			fp->_cnt -= copy;
			if (copy < THRESHOLD) {
				register char *rptr = ptr,
						*bp = (char *)fp->_ptr;

				for (++copy; --copy > 0; )
					*bp++ = *rptr++;
				ptr = rptr;
				fp->_ptr = (PTR_TYPE)bp;
			} else {
				(void) memcpy((char *)fp->_ptr, ptr, copy);
				ptr += copy;
				fp->_ptr += copy;
			}
		}
		if (bytes > 0)	/* more to write, but buffer is full */
			if (putc((putc_t)*ptr, fp) == EOF) /* flush buffer */
				break;
			else {
				ptr++;
				--bytes;
			}
	}
	return size == 0? count: (origbytes - bytes) / size;
}
