/* asm.h -- macros for sparc asm
 *
 * Copyright (c) 1996 Cygnus Support
 *
 * The authors hereby grant permission to use, copy, modify, distribute,
 * and license this software and its documentation for any purpose, provided
 * that existing copyright notices are retained in all copies and that this
 * notice is included verbatim in any distributions. No written agreement,
 * license, or royalty fee is required for any of the authorized uses.
 * Modifications to this software may be copyrighted by their authors
 * and need not follow the licensing terms described here, provided that
 * the new terms are clearly indicated on the first page of each file where
 * they apply.
 */

#ifndef __SPARC_ASM_h
#define __SPARC_ASM_h

/*
 *  Indicate we are in an assembly file and get the basic CPU definitions.
 */

#define ASM

/*
 *  Recent versions of GNU cpp define variables which indicate the
 *  need for underscores and percents.  If not using GNU cpp or
 *  the version does not support this, then you will obviously
 *  have to define these as appropriate.
 */

/*
 * __USER_LABEL_PREFIX__ doesn't work on gcc 2.7.0-3. The following
 * ifdef magic fixes the problem but results in a warning XXX when
 * compiling assembly code.
 */
#ifndef __USER_LABEL_PREFIX__
/* #define __USER_LABEL_PREFIX__ ""	/* no underscore for coff */
#define __USER_LABEL_PREFIX__ _		/* leading underscore for aout */
#endif

/*
 * Sigh, GCC is way broken, so we *always* redefine this to be '_'. It's
 * supposed to be empty for coff, but it's broken in all the currenbt
 * releases. We change the name slighly so we don't have to put up with
 * all the warning messages. Maybe someday gcc will be fixed...
 */
#define USER_LABEL_PREFIX _


/* ANSI concatenation macros.  */
#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

/* use the right prefix for global labels.  */
#define SYM(x) CONCAT1 (USER_LABEL_PREFIX,x)

/* end of file */
#endif
