/* filename: rlpr-dbfile.h
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlpr-dbfile.h,v 1.4 1996/10/12 07:09:02 meem Exp $
 * contents: prototypes and #defines for rlpr database file manipulation
 *
 * Time-stamp: <1996/10/12 02:08 -- meem@sherilyn.wustl.edu>
 */

#ifndef RLPR_DBFILE_H
#define RLPR_DBFILE_H

/* database properties */
#define DBNAME       ".rlprrc"            /* name of the database file */
#define DEFAULT_SYS_DBNAME "/etc/rlprrc"  /* the system-wide one */
#define DB_LINE_LEN  255                  /* max length of a line in the database file */

#define FINDQ        1
#define FINDHOST     2

/* function prototypes */
char * getqfromhost(char *printhost);     /* finds a printqueue name from a host */
char * gethostfromq(char *queue);         /* finds a host from a printqueue name */
void   opendbfile(void);                  /* opens the DBNAME file */
void   closedbfile(void);                 /* closes the DBNAME file */
char * db_search(char *sstr, int TYPE);   /* main search engine */

#endif /* RLPR_DBFILE_H */
