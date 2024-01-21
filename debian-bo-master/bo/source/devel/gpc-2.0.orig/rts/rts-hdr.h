/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Function type external definitions.

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#ifndef __RTS_HDR_H
#define __RTS_HDR_H

/* Standard textfile WRITE routine, page & put in rts-write.c */
extern void	_p_write ();			/* Write anything */
extern void	_p_page PROTO((FDR));		/* Text file PAGE routine */
extern void	_p_put PROTO((FDR));		/* put routine */

/* Standard textfile READ & get in rts-read.c */
extern void	_p_read ();			/* Read anything */
extern void	_p_get PROTO((FDR));		/* get routine */

/* File management in rts-file.c */
extern void	_p_rewrite PROTO((FDR,char *,int));	/* rewrite routine */
extern void	_p_reset PROTO((FDR,char *,int));	/* reset routine */

/* Extended file management in rts-file.c */
extern void	_p_extend PROTO((FDR,char *,int));	/* Append to file */
extern void	_p_close PROTO((FDR));	/* Gpc extension: Close file */

/* Random access routines for Extended Pascal in rts-random.c */
extern void	_p_seekwrite PROTO((FDR,int));	/* Random access write */
extern void	_p_seekread PROTO((FDR,int));	/* Random access read */
extern void	_p_seekupdate PROTO((FDR,int));	/* Random access update */

extern int      _p_empty PROTO((FDR));		/* is file empty? */
extern void	_p_update PROTO((FDR));		/* Update file */
extern int	_p_position PROTO((FDR));	/* Where are we in the file? */
extern int	_p_lastposition PROTO((FDR));	/* How far could we go? */
extern void	_p_definesize PROTO((FDR,int));	/* Gpc extension: define files size */
						/* needs ftruncate() */

/* Extended Pascal time routines in rts-times.c */
/* Put date/time info in TimeStamp */
extern void	_p_gettimestamp PROTO((struct GPC_TIMESTAMP *));

/* Make date and time string from TimeStamp */
extern void	_p_date 	PROTO((struct GPC_TIMESTAMP *, char *));
extern void	_p_time 	PROTO((struct GPC_TIMESTAMP *, char *));

/* Misc. Extended Pascal routines in rts-misc.c */
extern void	_p_halt PROTO((int));		/* Exits the program */

/* Standard HEAP management in rts-heap.c */
extern char*	_p_new PROTO((int));	        /* new routine */
extern void	_p_dispose PROTO((char *,int));	/* dispose routine */

/* heap management, GPC extension.
   You can use either dispose() or (mark() and release()) but not
   both in the same program. Decided dynamically on the first use basis. */
extern void	_p_mark PROTO((char **));	/* mark routine */
extern void	_p_release PROTO((char *));	/* release routine */

/* Internal heap management (safe malloc()) */
char *_p_malloc PROTO((int));

/* Internal heap alignment initialize routine */
void _p_init_heap_alignment PROTO((void));

/* Standard MATHEMATICAL routines in rts-math.c */
extern double	_p_arctan  PROTO((double));		/* math lib call */
extern double	_p_sqrt    PROTO((double));		/* math lib call */
extern double	_p_ln      PROTO((double));		/* math lib call */
extern double	_p_exp     PROTO((double));		/* math lib call */
extern double	_p_sin     PROTO((double));		/* math lib call */
extern double	_p_cos     PROTO((double));		/* math lib call */
extern double	_p_expon   PROTO((double,double));	/* math lib call */
extern double	_p_pow     PROTO((double,int));
extern COMPLEX  _p_polar   PROTO((double,double));
extern double   _p_arg     PROTO((COMPLEX));

extern COMPLEX  _p_z_arctan PROTO((COMPLEX));
extern COMPLEX  _p_z_expon  PROTO((COMPLEX, double));
extern COMPLEX  _p_z_sqrt   PROTO((COMPLEX));
extern COMPLEX  _p_z_ln     PROTO((COMPLEX));
extern COMPLEX  _p_z_exp    PROTO((COMPLEX));
extern COMPLEX  _p_z_sin    PROTO((COMPLEX));
extern COMPLEX  _p_z_cos    PROTO((COMPLEX));
extern COMPLEX  _p_z_pow    PROTO((COMPLEX, int));

void   _p_bind    PROTO((void *, GPC_BINDING *, int));
void   _p_binding PROTO((void *, GPC_BINDING *));
void   _p_unbind  PROTO((void *));
BINDING *_p_get_binding PROTO((FDR));

/* Internal routines
 * _p_initialize is called from your pascal program (from main())
 * just before entering your main program. Just before termination,
 * _p_finalize() is called.
 * 
 * If you wish to do something before program is entered or
 * exited, write routines _p_setup() and _p_final() and link
 * them with your pascal program. (see rts-setup.c)
 */
	/* initialize run time system */
extern void	_p_initialize PROTO((int, char **, char **));
	/* called before program ends by compiler */
extern void	_p_finalize   PROTO((void));
extern void	_p_initfdr PROTO((FDR,char *,int,int));	/* initialize File Descriptor */
extern void	_p_setup PROTO((void));		/* setup routine */
extern void	_p_final PROTO((void));		/* final routine */

extern void	_p_makename PROTO((char *));		/* Create a unique filename */
extern char     *_p_errmsg PROTO((int));		/* output error message */

extern void	_p_malloc_warning PROTO((char *));	/* GNU malloc warning */

extern void	_p_fflush PROTO((int));		/* Flush the files in FDR Chain */

extern char     *_p_errmsg  PROTO((int));
extern int 	_p_generic PROTO((int));
extern void	_p_warning PROTO((char *));
extern void	_p_prmessage PROTO((char *,int, int));
extern void	_p_error ();

extern int	_p_paramcount PROTO((void));
extern int	_p_paramstr PROTO((int,STRING *));

extern int	_p_restore_stdin PROTO((FDR));

void _p_open PROTO ((FDR, char *, int, int));	/* Internal open routine */
int  _p_seek PROTO ((FDR, int, int, int));

#ifndef HAVE_STRDUP
char *_p_strdup PROTO((char *));
#endif

/* debugging routines */
extern void	_p_printfdr PROTO((FDR, char *));

/* In libgcc2 */
extern int	__gcc_bcmp PROTO((char *, char *, int));

extern void _p_initialize_std_files PROTO ((void));

#endif /* __RTS_HDR_H */
