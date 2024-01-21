/* Copyright (C) 1993, 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* iutil2.h */
/* Interface to procedures in iutil2.c */

/* ------ Password utilities ------ */

/* Define the password structure. */
#define max_password 64			/* must be at least 11 */
typedef struct password_s {
	uint size;
	byte data[max_password];
} password;
# define NULL_PASSWORD {0, {0}}

/* Define the system password(s). */
extern password SystemParamsPassword;

/* Transmit a password to or from a parameter list. */
int param_read_password(P3(gs_param_list *, const char _ds *, password *));
int param_write_password(P3(gs_param_list *, const char _ds *, const password *));

/* Check a password from a parameter list. */
/* Return 0 if OK, 1 if not OK, or an error code. */
int param_check_password(P2(gs_param_list *, const password *));
