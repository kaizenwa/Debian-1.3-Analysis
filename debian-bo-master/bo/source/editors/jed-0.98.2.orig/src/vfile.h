#ifndef _DAVIS_VFILE_H_
#define _DAVIS_VFILE_H_
/*
 *  Copyright (c) 1993, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */

#define VFILE_TEXT  1
#define VFILE_BINARY  2
extern unsigned int VFile_Mode;

typedef struct
{
   char *buf;			       /* buffer for stream */
   char *bmax;			       /* pointer to end buffer */
   char *bp;			       /* current pointer in stream */
   char *eof;			       /* pointer to EOF if non NULL */
   int fd;			       /* file descrip for stream */
   unsigned int size;		       /* default buffer size */
   unsigned int mode;
   unsigned int cr_flag;	       /* true if lines end in cr */
} VFILE;

extern char *vgets(VFILE *, unsigned int *);
extern VFILE *vopen(char *, unsigned int, unsigned int);
extern void vclose(VFILE *);
extern VFILE *vstream(int, unsigned int, unsigned int);

#endif
