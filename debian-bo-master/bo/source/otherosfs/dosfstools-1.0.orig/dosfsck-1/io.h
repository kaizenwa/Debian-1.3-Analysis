/* io.h  -  Virtual disk input/output */

/* Written 1993 by Werner Almesberger */


#ifndef _IO_H
#define _IO_H

void fs_open(char *path,int rw);

/* Opens the file system PATH. If RW is zero, the file system is opened
   read-only, otherwise, it is opened read-write. */

void fs_read(unsigned int pos,int size,void *data);

/* Reads SIZE bytes starting at POS into DATA. Performs all applicable
   changes. */

int fs_test(unsigned int pos,int size);

/* Returns a non-zero integer if SIZE bytes starting at POS can be read without
   errors. Otherwise, it returns zero. */

void fs_write(unsigned int pos,int size,void *data);

/* If write_immed is non-zero, SIZE bytes are written from DATA to the disk,
   starting at POS. If write_immed is zero, the change is added to a list in
   memory. */

int fs_close(int write);

/* Closes the file system, performs all pending changes if WRITE is non-zero
   and removes the list of changes. Returns a non-zero integer if the file
   system has been changed since the last fs_open, zero otherwise. */

int fs_changed(void);

/* Determines whether the file system has changed. See fs_close. */

#endif
