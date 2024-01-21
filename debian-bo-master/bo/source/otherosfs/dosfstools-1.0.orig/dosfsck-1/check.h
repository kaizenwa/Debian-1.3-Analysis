/* check.h  -  Check and repair a PC/MS-DOS file system */

/* Written 1993 by Werner Almesberger */


#ifndef _CHECK_H
#define _CHECK_H

int scan_root(DOS_FS *fs);

/* Scans the root directory and recurses into all subdirectories. See check.c
   for all the details. Returns a non-zero integer if the file system has to
   be checked again. */

#endif
