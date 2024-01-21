#ifndef __CHARIO_H__
#define __CHARIO_H__

extern int __lastchar, __pushed;
#define pushchar(c) {__lastchar = c;__pushed=1; }

extern int fetchar(void);
extern int getlinenum(void);
extern char *getfilename(void);
extern int addmembuf(char *, char *);
extern int includemembuf(char *, char *);
extern int addfile(char *);
extern int includefile(char *);

#define NOERR 0
#define RECURSE 1
#define FILEERR 2

#endif
