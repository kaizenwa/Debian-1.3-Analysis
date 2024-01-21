/*
 */

#ifndef UTIL_H_DEF
#define UTIL_H_DEF

#ifndef TRUE
#define TRUE  1
#endif
#ifndef FALSE
#define FALSE 0
#endif

extern int verbose, debug;

/* cmpopen.c */
int CmpSearchFile(char *name);
char *CmpExtension(int type);
FILE *CmpOpenFile(char *name, int *flag);
void CmpCloseFile(FILE *fp, int flag);

/* malloc.c */
void *safe_malloc(int size);
void safe_free(void *ptr);

void add_signal(int sig, void (*handler)(), int exit_after);

#endif
