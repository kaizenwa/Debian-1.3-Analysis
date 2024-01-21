/* nasmlib.c	header file for nasmlib.h
 *
 * The Netwide Assembler is copyright (C) 1996 Simon Tatham and
 * Julian Hall. All rights reserved. The software is
 * redistributable under the licence given in the file "Licence"
 * distributed in the NASM archive.
 */

#ifndef NASM_NASMLIB_H
#define NASM_NASMLIB_H

/*
 * Wrappers around malloc, realloc and free. nasm_malloc will
 * fatal-error and die rather than return NULL; nasm_realloc will
 * do likewise, and will also guarantee to work right on being
 * passed a NULL pointer; nasm_free will do nothing if it is passed
 * a NULL pointer.
 */
void nasm_set_malloc_error (efunc);
void *nasm_malloc (size_t);
void *nasm_realloc (void *, size_t);
void nasm_free (void *);
char *nasm_strdup (char *);

/*
 * ANSI doesn't guarantee the presence of `stricmp' or
 * `strcasecmp'.
 */
int nasm_stricmp (char *, char *);
int nasm_strnicmp (char *, char *, int);

/*
 * Convert a string into a number, using NASM number rules. Sets
 * `*error' to TRUE if an error occurs, and FALSE otherwise.
 */
long readnum(char *str, int *error);

/*
 * seg_init: Initialise the segment-number allocator.
 * seg_alloc: allocate a hitherto unused segment number.
 */
void seg_init(void);
long seg_alloc(void);

/*
 * many output formats will be able to make use of this: a standard
 * function to add an extension to the name of the input file
 */
void standard_extension (char *inname, char *outname, char *extension,
			 efunc error);

/*
 * some handy macros that will probably be of use in more than one
 * output format: convert integers into little-endian byte packed
 * format in memory
 */

#define WRITELONG(p,v) \
  do { \
    *(p)++ = (v) & 0xFF; \
    *(p)++ = ((v) >> 8) & 0xFF; \
    *(p)++ = ((v) >> 16) & 0xFF; \
    *(p)++ = ((v) >> 24) & 0xFF; \
  } while (0)

#define WRITESHORT(p,v) \
  do { \
    *(p)++ = (v) & 0xFF; \
    *(p)++ = ((v) >> 8) & 0xFF; \
  } while (0)

/*
 * and routines to do the same thing to a file
 */
void fwriteshort (int data, FILE *fp);
void fwritelong (long data, FILE *fp);

/*
 * Routines to manage a dynamic random access array of longs which
 * may grow in size to be more than the largest single malloc'able
 * chunk.
 */

struct RAA;

struct RAA *raa_init (void);
void raa_free (struct RAA *);
long raa_read (struct RAA *, long);
struct RAA *raa_write (struct RAA *r, long posn, long value);

/*
 * Routines to manage a dynamic sequential-access array, under the
 * same restriction on maximum mallocable block. This array may be
 * written to in two ways: a contiguous chunk can be reserved of a
 * given size, and a pointer returned, or single-byte data may be
 * written. The array can also be read back in the same two ways:
 * as a series of big byte-data blocks or as a list of structures
 * of a given size.
 */

struct SAA;

struct SAA *saa_init (long elem_len);  /* 1 == byte */
void saa_free (struct SAA *);
void *saa_wstruct (struct SAA *);      /* return a structure of elem_len */
void saa_wbytes (struct SAA *, void *, long);  /* write arbitrary bytes */
void saa_rewind (struct SAA *);	       /* for reading from beginning */
void *saa_rstruct (struct SAA *);      /* return NULL on EOA */
void *saa_rbytes (struct SAA *, long *);   /* return 0 on EOA */
void saa_rnbytes (struct SAA *, void *, long); /* read a given no. of bytes */
void saa_fread (struct SAA *s, long posn, void *p, long len);   /* fixup */
void saa_fwrite (struct SAA *s, long posn, void *p, long len);   /* fixup */
void saa_fpwrite (struct SAA *, FILE *);

#endif
