/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * rcfile.h - protos for rcfile.c
 */

extern struct zgv_config cfg;

/* rcfile.c */
extern int getconfig(void);
extern int saveconfig(void);
extern int modematch(int,int,int);
extern int parsecommandline(int argc,char **argv);
extern int usage_help();
