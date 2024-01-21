/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     errors.h
 * Abstract:        definitions for system error codes
 */

#define E_OK            0       /* no error */
#define E_NOINPUT       -1      /* no input available */
#define E_NOWORD        -2      /* unknown word */
#define E_NOCOMP        -3      /* word must be compiled */
#define E_NOVOC         -4      /* corrupted dictionary */
#define E_NOMEM         -5      /* not enough memory */
#define E_DSTK_UNDER    -6      /* data-stack underflow */
#define E_DSTK_OVER     -7      /* data-stack overflow */
#define E_RSTK_UNDER    -8      /* return-stack underflow */
#define E_RSTK_OVER     -9      /* return-stack overflow */
#define E_FSTK_UNDER    -10     /* floating-stack undeflow */
#define E_FSTK_OVER     -11     /* floading-stack overflow */
#define E_DSPACE_UNDER  -12     /* dictionary-space underflow */
#define E_DSPACE_OVER   -13     /* dictionary-space overflow */
#define E_NOFILE        -14     /* unable to access image file */
#define E_NOPRIM        -15     /* primitive not implemented */
#define E_FPE			-16		/* floating point exception */
#define E_SEGV			-17		/* segmentation violation */
#define E_FILENOTFOUND	-18		/* file not found (during "included") */
