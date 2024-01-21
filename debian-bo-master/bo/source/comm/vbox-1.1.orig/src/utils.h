/* $Id: utils.h,v 1.2 1996/07/15 10:02:49 root Exp $ */

#ifndef _VBOX_UTIL_H
#define _VBOX_UTIL_H 1

extern int	FileExist(char *, char *);
extern int	TouchFile(char *, char *);
extern void	DeleteFile(char *, char *);
extern void CopyString(char *, char *, int);

#endif /* _VBOX_UTIL_H */
