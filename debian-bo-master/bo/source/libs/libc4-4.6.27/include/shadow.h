/*
 * Copyright 1988, 1989, 1990, John F. Haugh II
 * All rights reserved.
 *
 * Use, duplication, and disclosure prohibited without
 * the express written permission of the author.
 */

#ifndef _SHADOW_H

#define _SHADOW_H	1
#include <features.h>

__BEGIN_DECLS

#include <gnu/types.h>
#include <stdio.h>
#include <pwd.h>
#include <grp.h>

/*
 * This information is not derived from AT&T licensed sources.  Posted
 * to the USENET 11/88, and updated 11/90 with information from SVR4.
 *
 *	@(#)shadow.h	3.3	09:06:50	12/7/90
 */

typedef __time_t sptime;

/*
 * Shadow password security file structure.
 */

struct spwd
{
  char *sp_namp;		/* login name */
  char *sp_pwdp;		/* encrypted password */
  sptime sp_lstchg;		/* date of last change */
  sptime sp_min;		/* minimum number of days between changes */
  sptime sp_max;		/* maximum number of days between changes */
  sptime sp_warn;		/* number of days of warning before password
				   expires */
  sptime sp_inact;		/* number of days after password expires
				   until the account becomes unusable. */
  sptime sp_expire;		/* days since 1/1/70 until account expires */
  unsigned long sp_flag;	/* reserved for future use */
};

/*
 * Shadow password security file functions.
 */

extern void setspent __P ((void));
extern void endspent __P ((void));
extern struct spwd *sgetspent __P ((__const char *__string));
extern struct spwd *fgetspent __P ((FILE *__fp));
extern struct spwd *getspent __P ((void));
extern struct spwd *getspnam __P ((__const char *__name));
extern int putspent __P ((__const struct spwd *__sp, FILE *__fp));

#define SHADOW "/etc/shadow"

/*
 * Shadow group security file structure
 */

struct	sgrp
{
  char *sg_name;		/* group name */
  char *sg_passwd;		/* group password */
  char **sg_adm;		/* group administator list */
  char **sg_mem;		/* group membership list */
};

/*
 * Shadow group security file functions.
 */

extern void setsgent __P ((void));
extern void endsgent __P ((void));
extern struct sgrp *sgetsgent __P ((__const char *__string));
extern struct sgrp *fgetsgent __P ((FILE *__fp));
extern struct sgrp *getsgent __P ((void));
extern struct sgrp *getsgnam __P ((__const char *__str));
extern int putsgent __P ((__const struct sgrp *_grp, FILE *__fp));

#define GSHADOW "/etc/gshadow"

__END_DECLS

#endif /* shadow.h */
