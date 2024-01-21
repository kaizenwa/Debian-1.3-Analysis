#ifndef PWDB_NIS_PUBLIC_H
#define PWDB_NIS_PUBLIC_H

/* NIS function prototypes */
 
/* NIS PASSWD 
 * - only pure NIS functions are working at this moment
 * - the commented ones need access to the /etc/passwd entries,
 *   we have to build the interface first
 */
struct __pwdb_passwd * __pwdbNIS_getpwuid (uid_t);
struct __pwdb_passwd * __pwdbNIS_getpwnam (const char *);
int __pwdbNIS_update (const char *, const struct __pwdb_passwd *);
/* struct __pwdb_passwd * __pwdbNIS_sgetpwent (char *); */
/* struct __pwdb_passwd * __pwdbNIS_fgetpwent (FILE *); */
/* struct __pwdb_passwd * __pwdbNIS_getpwent (void);    */
/* void __pwdbNIS_setpwent (void);                      */
/* void __pwdbNIS_endpwent (void);                      */

/* NIS GROUP 
 * - only pure NIS functions are working at this moment
 * - the commented ones need access to the /etc/group entries,
 *   we have to build the interface first
 */
struct __pwdb_group * __pwdbNIS_getgrgid (gid_t);
struct __pwdb_group * __pwdbNIS_getgrnam (const char *);
/* struct __pwdb_group * __pwdbNIS_fgetgrent (FILE *); */
/* struct __pwdb_group * __pwdbNIS_getgrent (void); */
/* void __pwdbNIS_setgrent (void); */
/* void __pwdbNIS_endgrent (void); */

#endif /* PWDB_NIS_PUBLIC_H */
