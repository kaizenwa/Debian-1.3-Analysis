/*
 * external defines for the pwdb libraries
 *
 * this library includes defines for passwd+shadow and group files
 */

#ifndef PWDB_STANDARD_PUBLIC_H
#define PWDB_STANDARD_PUBLIC_H

/*
 * The definitions for VANILLA UNIX
 */

/* STRUCTURE(S) */

struct __pwdb_passwd {          /* The passwd structure. */
     char *pw_name;		/* Username.  */
     char *pw_passwd;		/* Password.  */
     __uid_t pw_uid;		/* User ID.  */
     __gid_t pw_gid;		/* Group ID.  */
     char *pw_gecos;		/* Real name.  */
     char *pw_dir;		/* Home directory.  */
     char *pw_shell;		/* Shell program.  */
};

/* FUNCTIONS */

/*
 * Read next entry from the password-file stream, opening it if necessary.
 */

struct __pwdb_passwd *__pwdb_getpwent(void);

/*
 * Open and close the password-file stream
 */

void __pwdb_setpwent(void);
void __pwdb_endpwent(void);

/*
 * Read/write password entry from/to __stream
 */

struct __pwdb_passwd *__pwdb_fgetpwent(FILE *__stream);
int __pwdb_putpwent(__const struct __pwdb_passwd *__p, FILE *__stream);

/*
 * Obtain pw entries for the user identified by
 *  (i) their UID, or
 * (ii) their "username"
 */
 
struct __pwdb_passwd *__pwdb_getpwuid(__uid_t __uid);
struct __pwdb_passwd *__pwdb_getpwnam(__const char *__name);

/*
 * The group manipulation files
 */

struct __pwdb_group {           /* The group structure.	 */
     char *gr_name;		/* Group name.	*/
     char *gr_passwd;		/* Password.	*/
     __gid_t gr_gid;		/* Group ID.	*/
     char **gr_mem;		/* Member list.	*/
};

/*
 * Rewind the group-file stream.
 */

void __pwdb_setgrent (void);

/*
 * Close the group-file stream.
 */

void __pwdb_endgrent (void);

/*
 * Read an entry from the group-file stream, opening it if necessary.
 */

struct __pwdb_group * __pwdb_getgrent (void);

/*
 * Read/Write a group entry from STREAM.
 */

struct __pwdb_group * __pwdb_fgetgrent (FILE * __stream);
int __pwdb_putgrent(const struct __pwdb_group *__g, FILE *__stream);

/*
 * Search for an entry with a matching group ID.
 */

struct __pwdb_group * __pwdb_getgrgid (__gid_t __gid);

/*
 * Search for an entry with a matching group name.
 */

struct __pwdb_group * __pwdb_getgrnam (__const char *__name);

/*
 * sgetpwent - convert a string to a (struct __pwdb_passwd)
 */
struct __pwdb_passwd *__pwdb_sgetpwent(const char *buf);
struct __pwdb_group * __pwdb_sgetgrent(const char *buf);
     
/*
 * LOW LEVEL FUNCTIONS
 */
 
/*
 * Locking functions we export
 */
int __pwdb_pw_lock (void);
int __pwdb_gr_lock (void);
int __pwdb_pw_unlock (void);
int __pwdb_gr_unlock (void);

/* USER database low-level manipulation */
int __pwdb_pw_close (void);
const struct __pwdb_passwd * __pwdb_pw_locate (const char *);
const struct __pwdb_passwd * __pwdb_pw_locate_id (__uid_t);
int __pwdb_pw_name (const char *);
const struct __pwdb_passwd * __pwdb_pw_next (void);
int __pwdb_pw_open (int);
int __pwdb_pw_remove (const char *);
int __pwdb_pw_rewind (void);
int __pwdb_pw_update (const struct __pwdb_passwd *);

/* GROUP database low-level manipulation */

struct __pwdb_gr_file_entry {
	char *grf_line;
	int grf_changed;
	struct __pwdb_group *grf_entry;
	struct __pwdb_gr_file_entry *grf_next;
};

int __pwdb_gr_close (void);
const struct __pwdb_group * __pwdb_gr_locate (const char *);
const struct __pwdb_group * __pwdb_gr_locate_id (__gid_t);
int __pwdb_gr_name (const char *);
const struct __pwdb_group * __pwdb_gr_next (void);
int __pwdb_gr_open (int);
int __pwdb_gr_remove (const char *);
int __pwdb_gr_rewind (void);
int __pwdb_gr_update (const struct __pwdb_group *);

#endif /*  PWDB_STANDARD_PUBLIC_H */
