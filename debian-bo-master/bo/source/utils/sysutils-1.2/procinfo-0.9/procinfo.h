/*

   procinfo.h

   Date:        1995-04-15 23:54:06
   Last Change: 1996-04-28 20:14:00

   Copyright (C) 1995, 1996 Sander van Malssen <svm@kozmix.ow.nl>

   This software is released under the GNU Public Licence. See the
   file `COPYING' for details.

 */

#ifndef _PROCINFO_H
#define _PROCINFO_H

#define VERSION		"0.9 (1996-04-28)"
#ifndef PROC_DIR
#define PROC_DIR	"/proc/"	/* Needs the trailing slash. */
#endif
#define ISSTR(s)	(!strcmp(s, type))
#define VAL		(atol(strtok(NULL, " ")))
#define MAX(a,b)	((a)>(b)?(a):(b))
#define DIFF(x)		(show_diff ? (long) (((new.x)-(old.x))/rate) : (new.x))

#define CDRV		0
#define BDRV		1
#ifndef MAX_CHRDEV
#define MAX_CHRDEV	32
#endif
#ifndef MAX_BLKDEV
#define MAX_BLKDEV	32
#endif
#define MAX_DEV		MAX(MAX_CHRDEV, MAX_BLKDEV)

struct info
{
    long uptime;
    long m_to, m_us, m_fr, m_sh, m_bu, m_ca;
    long s_to, s_us, s_fr;
    long cpu_user, cpu_nice, cpu_sys, cpu_idle;
    long disk[4];
    long disk_r[4];
    long disk_w[4];
    long pgin, pgout, swin, swout;
    long intr[16];
    long old_intr;
    long ctxt;
};

/* Prototypes. */

FILE *myfopen (char *name);
char *hms (long t);
char *perc (long i, long t);
void bye (int i);
void tstp (int i);
void cont (int i);
void window_init (int i);
void fatal (const char *s,...);
void init_terminal_data (void);
char *my_tgets (char *te);
char *make_version (FILE *verionfp);

#endif /* _PROCINFO_H */

/*
   Local variables:
   rm-trailing-spaces: t
   End:
 */
