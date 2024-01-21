#ifndef PWDB_COMMON_PUBLIC_H
#define PWDB_COMMON_PUBLIC_H

#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>

/* grcommon.c */

char * __pwdb_fgetsx (char *buf, int cnt, FILE *f);
int __pwdb_fputsx (const char *s, FILE *stream);

/*
 * prevent simultaneous updates of password files. Lock and un-lock
 * the password files.
 */
int __pwdb_lckpwdf(void);
int __pwdb_ulckpwdf(void);

/* commonio.c */
int do_lock_file(const char *file, const char *lock);
FILE * fopen_with_umask(const char *name, const char *mode, int mask);
int create_backup_file(FILE *fp, const char *backup, const struct stat *st);

/* misc.c */
char *__pwdb_strdup(const char *x);

     
#endif /* PWDB_COMMON_PUBLIC_H */

