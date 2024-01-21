#ifndef _SLRN_UTIL_H
#define _SLRN_UTIL_H

#include <limits.h>
#ifdef PATH_MAX
# define SLRN_MAX_PATH_LEN PATH_MAX
#else
# define SLRN_MAX_PATH_LEN 1024
#endif

extern int slrn_dircat (char *, char *, char *);
extern char *slrn_spool_dircat (char *, char *, int);
extern int slrn_fclose (FILE *);
extern int slrn_delete_file (char *);
extern int slrn_file_exists (char *);
extern char *slrn_basename (char *);

extern char *slrn_simple_strtok (char *, char *);
extern char *slrn_strchr (char *, char);
extern char *slrn_skip_whitespace (char *s);
extern char *slrn_trim_string (char *s);
extern int slrn_case_strncmp (unsigned char *, unsigned char *, unsigned int);
extern int slrn_case_strcmp (unsigned char *, unsigned char *);
extern char *slrn_strbrk (char *, char *);

extern char *slrn_safe_strmalloc (char *);
extern char *slrn_safe_malloc (unsigned int);
extern char *slrn_strmalloc (char *, int);
extern char *slrn_malloc (unsigned int, int, int);
extern char *slrn_realloc (char *, unsigned int, int);
extern void slrn_free (char *);

extern char *slrn_fix_regexp (char *);

/* This declaration is here although the function is not really defined 
 * in the C file.
 */
extern void slrn_exit_error (char *, ...);

#endif				       /* _SLRN_UTIL_H */
