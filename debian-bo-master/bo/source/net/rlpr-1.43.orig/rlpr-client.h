/* filename: rlpr-client.h
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlpr-client.h,v 1.2 1996/11/19 01:16:22 meem Exp $
 * contents: common routines and structures for the rlpr client
 *
 * Time-stamp: <1996/11/17 02:14 -- meem@sherilyn.wustl.edu>
 */

#ifndef RLPR_CLIENT_H
#define RLPR_CLIENT_H

#define DEFAULT_WIDTH           "132" /* according to RFC 1179 */

#define DEFAULT_INDENT_NO_PARAM "8"
#define DEFAULT_WIDTH_NO_PARAM  "80"  /* according to RFC 1179 */

/* environment variable names */
#define PRINTHOST         "RLPR_PRINTHOST"
#define PROXYHOST         "RLPR_PROXYHOST"
#define PROXYHOST2        "RLPR_PROXY"

/* most of these constants are just used to make sure we don't overflow
 * any artificial limits in the server's lpd.  rlpr allocates the space
 * dynamically 
 */

#define MAX_STR_LEN       255
#define MAX_FILE_LEN      64	      /* NOT DEFINED IN RFC 1179! */
#define MAX_QUEUE_LEN     64	      /* NOT DEFINED IN RFC 1179! */

/* NOTHING BELOW THIS POINT SHOULD NEED TO BE CHANGED */

/* FROM RFC 1179 */
#define MAX_USER_LEN      31
#define MAX_SOURCE_LEN    131
#define MAX_HOST_LEN      31
#define MAX_JOB_LEN       99
#define MAX_CLASS_LEN     31
#define MAX_TITLE_LEN     79

/* printing attributes struct -- these attributes are general to
 * standard BSD lpr
 */

typedef struct print_attr {
  char         filetype;             /* format of file to print */
  int          fflag:1;              /* form feed after printing? */
  int          mflag:1;              /* mail after printing? */
  int          rflag:1;              /* remove file after printing? */
  int          bflag:1;              /* burst page flag (1 = print it) */
  int          wflag:1;              /* windows flag (for braindead lpd's) */
  char *       indent_cols;          /* number of columns to indent by (string) */
  char *       width;                /* width, also stored as a string */
  char *       tmpdir;               /* this follows the GNU custom */
  unsigned int copies;               /* number of copies to print */

  char *       class;                /* job classification on burst page */
  char *       printer;              /* name of printer queue */
  char *       user;                 /* user name to print on burst page */
  char *       job;                  /* job name to print on burst page */
  char *       title;                /* title (optional for pr) */
} print_attr;

typedef struct net_attr {
  char *       localhost;            /* hostname of host rlpr is running on */
  char *       proxyhost;            /* hostname of proxy to go through */
  char *       printhost;            /* hostname of printer to print to */
  u_short      port;                 /* port number to connect to */
} net_attr;

extern print_attr opts_;	     /* global printing options struct */
extern net_attr   net_;		     /* global network options struct  */
extern char *     name;		     /* program name */

#endif /* RLPR_CLIENT_H */

