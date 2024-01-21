/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Cimarron D. Taylor of the University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)find.h	5.8 (Berkeley) 5/24/91
 */

#ifndef _FSP_FIND_H_
#define _FSP_FIND_H_

/* node type */
enum ntype {
	N_AND = 1, 				/* must start > 0 */
	N_TIME, N_CLOSEPAREN, N_EXEC, N_EXPR,
	N_LS, N_NAME, N_NEWER,
	N_NOT, N_OK, N_OPENPAREN, N_OR, N_PRINT,
	N_PRUNE, N_SIZE, N_TYPE,
};

/* node definition */
typedef struct _plandata {
	struct _plandata *next;			/* next node */
	int (*eval)();				/* node evaluation function */
	int flags;				/* private flags */
	enum ntype type;			/* plan node type */
	union {
		gid_t _g_data;			/* gid */
		ino_t _i_data;			/* inode */
		mode_t _m_data;			/* mode mask */
		nlink_t _l_data;		/* link count */
		off_t _o_data;			/* file size */
		time_t _t_data;			/* time value */
		uid_t _u_data;			/* uid */
		struct _plandata *_p_data[2];	/* PLAN trees */
		struct _ex {
			char **_e_argv;		/* argv array */
			char **_e_orig;		/* original strings */
			int *_e_len;		/* allocated length */
		} ex;
		char *_a_data[2];		/* array of char pointers */
		char *_c_data;			/* char pointer */
	} p_un;
#define	a_data	p_un._a_data
#define	c_data	p_un._c_data
#define	i_data	p_un._i_data
#define	g_data	p_un._g_data
#define	l_data	p_un._l_data
#define	m_data	p_un._m_data
#define	o_data	p_un._o_data
#define	p_data	p_un._p_data
#define	t_data	p_un._t_data
#define	u_data	p_un._u_data
#define	e_argv	p_un.ex._e_argv
#define	e_orig	p_un.ex._e_orig
#define	e_len	p_un.ex._e_len
} PLAN;

typedef struct _option {
  char *name;           /* option name */
  enum ntype token;     /* token type */
  PLAN *(*create)();    /* create function */
#define O_NONE          0x01    /* no call required */
#define O_ZERO          0x02    /* pass: nothing */
#define O_ARGV          0x04    /* pass: argv, increment argv */
#define O_ARGVP         0x08    /* pass: *argv, N_OK || N_EXEC */
  int flags;
} OPTION;

/* find.c */
extern void find_formplan PROTO0((char **));

/* fnmatch.c */
extern int fnmatch PROTO0((register char *, register char *));

/* function.c */
extern PLAN * c_time PROTO0((char *));
extern PLAN *c_exec PROTO0((char ***, int));
extern PLAN *c_ls PROTO0((void));
extern PLAN *c_name PROTO0((char *));
extern PLAN *c_newer PROTO0((char *));
extern PLAN *c_print PROTO0((void));
extern PLAN *c_prune PROTO0((void));
extern PLAN *c_size PROTO0((char *));
extern PLAN *c_type PROTO0((char *));
extern int find_expr PROTO0((PLAN *, struct stat *, char *));
extern PLAN *c_openparen PROTO0((void));
extern PLAN *c_closeparen PROTO0((void));
extern PLAN *c_not PROTO0((void));
extern PLAN *c_or PROTO0((void));

/* operator.c */
extern PLAN *paren_squish PROTO0((PLAN *));
extern PLAN *not_squish PROTO0((PLAN *));
extern PLAN *or_squish PROTO0((PLAN *));

/* options.c */
extern PLAN *find_create PROTO0((char ***));
extern OPTION *option PROTO0((char *));

#endif /* _FSP_FIND_H_ */
