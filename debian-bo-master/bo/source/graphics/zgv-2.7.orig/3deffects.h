/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * 3deffects.h - prototypes for 3deffects.c
 */

/* type of msgbox:
 * at the moment, it's always MSGBOXTYPE_OK whatever replytype you pass,
 * but you should pass MSGBOXTYPE_OK for compatibility with future versions.
 */
#define MSGBOXTYPE_OK       1        /* will always cause return of 1 */
#define MSGBOXTYPE_YESNO    2        /* causes return of 1=yes, 0=no */
#define MSGBOXTYPE_OKCANCEL 3        /* similar to YESNO */

/* ok, from now on these prototypes are generated automatically,
 * 'cos I can't be bothered doing them by hand. so it's not my fault
 * they're ugly or don't fit 80 cols, alright?
 */

/* 3deffects.c */
extern int drawtext3d(int x, int y, int s, char *str, int isout, int lite, int dark, int txt);
extern int undrawtext3d(int x, int y, int s, char *str);
extern int draw3dbox(int x1, int y1, int x2, int y2, int depth, int isout, int lite, int dark);
extern int undraw3dbox(int x1, int y1, int x2, int y2, int depth);
extern int msgbox(int ttyfd,char *message,int replytype,int lite,int dark,int txt);

