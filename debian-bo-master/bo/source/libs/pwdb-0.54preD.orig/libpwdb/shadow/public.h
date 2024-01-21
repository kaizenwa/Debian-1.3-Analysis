/*
 * external defines for the pwdb libraries
 *
 * this library includes defines for passwd+shadow and group files
 */

#ifndef PWDB_SHADOW_PUBLIC_H
#define PWDB_SHADOW_PUBLIC_H

/*
 * The definitions for SHADOW
 */

/* STRUCTURE(S) */

typedef	long __pwdb_sptime;

/*
 * Shadow password security file structure.
 */

struct __pwdb_spwd {            /* The shadow password structure */
     char *sp_namp;	        /* login name */
     char *sp_pwdp;	        /* encrypted password */
     __pwdb_sptime sp_lstchg;   /* date of last change */
     __pwdb_sptime sp_min;	/* minimum number of days between changes */
     __pwdb_sptime sp_max;	/* maximum number of days between changes */
     __pwdb_sptime sp_warn;     /* number of days of warning before password
				   expires */
     __pwdb_sptime sp_inact;    /* number of days after password expires
				   until the account becomes unusable. */
     __pwdb_sptime sp_expire;   /* days since 1/1/70 until account expires */
     unsigned long sp_flag;     /* reserved for future use */
};

/* FUNCTIONS */

/*
 * Shadow password security file functions.
 */

/*
 * Read next entry from the shadow-file stream, opening it if necessary.
 */

struct __pwdb_spwd *__pwdb_getspent(void);

/*
 * Open and close the shadow-file stream
 */

void __pwdb_setspent(void);
void __pwdb_endspent(void);

/*
 * Read/write shadow entry from/to __stream
 */

struct __pwdb_spwd * __pwdb_fgetspent(FILE *);
int __pwdb_putspent(const struct __pwdb_spwd *, FILE *);

/*
 * Obtain shadow entry for the user identified by their "username"
 */

struct __pwdb_spwd *__pwdb_getspnam(const char *__username);

/*
 * convert a shadow entry from text (as it appears in the shadow file
 * to a __pwd_spwd structure)
 */

struct __pwdb_spwd * __pwdb_sgetspent(const char *);

/*
 * Shadow group security file structure
 */

struct	__pwdb_sgrp {
     char	*sg_name;	/* group name */
     char	*sg_passwd;	/* group password */
     char	**sg_adm;	/* group administator list */
     char	**sg_mem;	/* group membership list */
};

/*
 * Shadow group security file functions.
 */

/*
 * Read next entry from shadow-group file.
 */

struct	__pwdb_sgrp *  __pwdb_getsgent (void);

/*
 *  Open(rewind)/close the shadouw-group stream
 */

void	__pwdb_setsgent (void);
void	__pwdb_endsgent (void);

/*
 * get the shadow-group entry for the named group
 */

struct	__pwdb_sgrp *  __pwdb_getsgnam (const char *);

/*
 * create a sgrp structure from a buffer of text which is in the
 * format of a line in the shadow-group file
 */

struct	__pwdb_sgrp *  __pwdb_sgetsgent (const char *);

/* read/write a shadow entry from the argument stream */
struct	__pwdb_sgrp *  __pwdb_fgetsgent (FILE *);
int __pwdb_putsgent (const struct __pwdb_sgrp *sgrp, FILE *fp);

/* LOW level functions */
 
/* SHADOW FILE */
struct __pwdb_spwd * __pwdb___spw_dup (const struct __pwdb_spwd *);
int __pwdb_spw_close (void);
const struct __pwdb_spwd * __pwdb_spw_locate (const char *);
int __pwdb_spw_lock (void);
int __pwdb_spw_name (const char *);
const struct __pwdb_spwd * __pwdb_spw_next (void);
int __pwdb_spw_open (int);
int __pwdb_spw_remove (const char *);
int __pwdb_spw_rewind (void);
int __pwdb_spw_unlock (void);
int __pwdb_spw_update (const struct __pwdb_spwd *);

/* SHADOW GROUP */
int __pwdb_sgr_close (void);
const struct __pwdb_sgrp * __pwdb_sgr_locate (const char *);
int __pwdb_sgr_lock (void);
int __pwdb_sgr_name (const char *);
const struct __pwdb_sgrp * __pwdb_sgr_next (void);
int __pwdb_sgr_open (int);
int __pwdb_sgr_remove (const char *);
int __pwdb_sgr_rewind (void);
int __pwdb_sgr_unlock (void);
int __pwdb_sgr_update (const struct __pwdb_sgrp *);

struct __pwdb_sg_file_entry {
	char    *sgr_line;
	int     sgr_changed;
	struct  __pwdb_sgrp    *sgr_entry;
	struct  __pwdb_sg_file_entry *sgr_next;
};

/******************************************************************/

#endif /* PWDB_SHADOW_PUBLIC_H */
