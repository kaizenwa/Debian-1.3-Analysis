/*
 *	(c) Copyright 1990, Kim Fabricius Storm.  All rights reserved.
 *
 * 	Semi-faked varargs feature
 */

#ifndef _NN_VARARG_H
#define _NN_VARARG_H 1

#ifdef HAVE_VARARGS
#undef NO_VARARGS
#endif

#ifdef NO_VARARGS

#define va_alist	zZa, zZb, zZc, zZd, zZe, zZf, zZg, zZh
#define va_dcl		char *zZa, *zZb, *zZc, *zZd, *zZe, *zZf, *zZg, *zZh;

#define va_arg1(type)	(type)zZa
#define va_arg2(type)	(type)zZb
#define va_arg3(type)	(type)zZc
#define va_args1toN	zZa, zZb, zZc, zZd, zZe, zZf, zZg, zZh
#define va_args2toN	zZb, zZc, zZd, zZe, zZf, zZg, zZh
#define va_args3toN	zZc, zZd, zZe, zZf, zZg, zZh
#define va_args4toN	zZd, zZe, zZf, zZg, zZh

#define vsprintf	sprintf
#define vprintf		printf

#define use_vararg
#define start_vararg
#define end_vararg

#define va_tail		va_alist
#define va_tdcl		va_dcl

#else

#include <varargs.h>

#define va_tail		zZap
#define	va_tdcl		va_list va_tail;

#define use_vararg	va_list zZap
#define start_vararg	va_start(zZap)
#define end_vararg	va_end(zZap)

#define va_arg1(type)	va_arg(zZap, type)
#define va_arg2(type)	va_arg(zZap, type)
#define va_arg3(type)	va_arg(zZap, type)
#define va_args1toN	zZap
#define va_args2toN	zZap
#define va_args3toN	zZap
#define va_args4toN	zZap

#endif


#endif /* _NN_VARARG_H */
