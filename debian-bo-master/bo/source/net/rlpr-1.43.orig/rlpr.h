/* filename: rlpr.h
 * project: rlpr
 * author: meem  --  meem@sherilyn.wustl.edu
 * version: $Id: rlpr.h,v 1.3 1996/11/20 09:04:36 meem Exp $
 * content: general definitions/declarations for rlpr.c
 *
 * Time-stamp: <1996/11/17 02:26 -- meem@sherilyn.wustl.edu>
 */

#ifndef RLPR_H
#define RLPR_H

/* function prototypes */

/* adds line to control file */
void cf_add(char op, const char *s, int cfd);

/* puts usual stuff in cf */
void cf_header(int cfd, char *filename);

/* initialize global client opts */
void init_options(void);

/* tidy up and check over command-line args once all info known */
void tidy_and_check_options(void);

/* parse command line options, return amount parsed */
int parse_args(int argc, char *argv[]);

#endif /* RLPR_H */
