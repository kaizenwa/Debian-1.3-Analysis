/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Common defines for GNU Pascal compiler and the run time system

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

#ifndef _RTS_TYPES_H_
#define _RTS_TYPES_H_
/*
 * The Pascal FILE_TYPE object is defined in rts/gpc_fdr.h
 *
 * The things the compiler needs to know about the FILE_TYPE are:
 *  1) The size of the object. (Could be avoided if file buffers
 *				were allocated from HEAP, but...)
 *  2) The file buffer address is in the first word of the object.
 *  3) The file status bits are in the second word of the file object
 *     The compiler can make inline code of EOF and EOLN by checking
 *     these bits. (_p_eof and _p_eoln also exist, but are slower)
 */

/* This file is also included in the compiler to keep the definition
 * unique (gpc-util.c file in the GCC compiler Pascal front end)
 */

typedef struct Fdr* FDR;

struct Fdr {
    char *FilBuf;	  /* address of the file buffer in heap 	*/
    int   FilSta;	  /* status bits; see below 			*/
    			  /* THE ABOVE FIELDS ACCESSED BY COMPILER      */
    			  /* The fields below are used only by RTS	*/
    int   FilSiz;	  /* bufsize: if packed then in bits else bytes */
    char *FilNam;	  /* Internal name of the file			*/
    char *ExtNam;	  /* External name of the file			*/
    FILE *FilJfn;	  /* FILE pointer				*/
    int   FilElem;	  /* size of file in buf elements (random acc.) */
    int   RtsSta;	  /* run time system status bits		*/
    FDR	  NxtFdr; 	  /* FDR chain pointer				*/
    void *Binding;	  /* Binding of the file */
};

#define FDR_Size  sizeof(struct Fdr)

/* FilSta bit definitions */

#define FiUnd 	(1 << 0) /* File buffer is totally undefined */
#define FiCom   (1 << 1) /* File buffer assigned to in pascal side */
#define FiEof	(1 << 2) /* End of file is true */
#define FiEln	(1 << 3) /* End of line is true. Textfiles only */
#define	FiTxt	(1 << 4) /* It's a TEXT file */
#define FiExt	(1 << 5) /* External file */
#define FiPck	(1 << 6) /* Packed file */
#define FiClr	(1 << 7) /* Empty file */
#define FiLzy	(1 << 8) /* This file is lazy */
#define FiEofOK	(1 << 9) /* Internal flag if FiUnd is set: Accept EOF without EOLN */
#define FiDacc	(1 << 10)/* This is a direct access file */
#define FiLget	(1 << 11)/* Must do a get before buffer reference (Lazy I/O) */
#define FiByte	(1 << 12)/* File buffer is actually one byte size */

/* File kind: shift counts for the bits in the fourth parameter
 * of _p_initfdr
 */
#define fkind_TEXT	0
#define fkind_PACKED	1
#define fkind_EXTERN	2
#define fkind_LAZY	3
#define fkind_DIRECT	4
#define fkind_BYTE	5

/* The following defines specify the argument types of the run time system
 * routines _p_read and _p_write that get variable number and type
 * of arguments.
 * The first parameter to these routines is always a GPC file of type FDR
 * and the second is an integer that tells how many of the following
 * types follow. Note that the types require different number of arguments.
 */

#define P_INT		1	/* READ: (int *)    WRITE: int */
#define P_CHAR		2	/* READ: (char *)   WRITE: int */
#define P_REAL		3	/* READ: (double *) WRITE: double */
#define P_LINE		4	/* none */
#define P_FIX_INT	5	/* WRITE: int    & int */
#define P_FIX1_REAL	6	/* WRITE: double & int */
#define P_FIX2_REAL	7	/* WRITE: double & int & int */
#define P_STRING	8	/* WRITE: char * & int */
#define P_FIX_STRING	9	/* WRITE: char * & int & int */
#define P_FIX_CHAR	10	/* WRITE: int   & int */
#define P_BOOL		11	/* WRITE: int */
#define P_FIX_BOOL	12	/* WRITE: int & int */
#define P_INT2		13	/* WRITE: int & int & int */

	/* The P_S_* and P_U_* are used as qualifiers
	 * for P_INT2 above.
	 */
#define P_S_BYTE	14	/* WRITE: signed byte    */
#define P_S_SHORT	15	/* WRITE: signed short   */
#define P_S_INT		16	/* WRITE: signed int     */
#define P_S_LONG	17	/* WRITE: signed long    */

#define P_U_BYTE	18	/* WRITE: unsigned byte  */
#define P_U_SHORT	19	/* WRITE: unsigned short */
#define P_U_INT		20	/* WRITE: unsigned int   */
#define P_U_LONG	21	/* WRITE: unsigned long  */

/* Run time _p_string() opcodes */
/* Lexicographic string relations: EQ NE LT LE GE GT */
#define R_EQ		1
#define R_NE		2
#define R_LT		3
#define R_LE		4
#define R_GE		5
#define R_GT		6
/* TRIM (s)      : trim spaces off the end of s    */
#define R_TRIM		7
/* INDEX (s1,s2) : index of s2 in s1               */
#define R_INDEX		8
/* LENGTH (s)    : This is inlined by the compiler */
#define R_LENGTH	9
/* SUBSTR (s, i, j) : substring of s from i to j   */
#define R_SUBSTR	10
/* String relations padding with spaces: =,!=,<,<=,>=,> */
#define R_eq		11
#define R_ne		12
#define R_lt		13
#define R_le		14
#define R_ge		15
#define R_gt		16

/* rts string arg type codes: flags for CHAR_TYPE parameters */
#define P_STR_FIRST_IS_CHAR	1
#define P_STR_SECOND_IS_CHAR	2

/*
 * The standard requires that the fields of the TimeStamp look like this:
 *
 * PACKED RECORD
 *   DateValid,
 *   TimeValid : Boolean;
 *   year      : integer;
 *   month     : 1 .. 12;
 *   day       : 1 .. 31;
 *   hour      : 0 .. 23;
 *   minute    : 0 .. 59;
 *   second    : 0 .. 59;
 * END;
 *
 * The record may contain extensions, like timezone, day of week,
 * microsecond-timer, etc.
 * 
 * @@@ Implement them later.
 */

struct GPC_TIMESTAMP {
    char Datevalid;
    char Timevalid;
    int  Year;
#ifdef GPC_PACKED_STRUCTURES
    char Month;
    char Day;
    char Hour;
    char Minute;
    char Second;
#else
    int Month;
    int Day;
    int Hour;
    int Minute;
    int Second;
#endif
};

/* Length of the NAME field of the BindingType required record
 *
 * Any length should work, as long as the name fits in. Passed as
 * VAR parameter to the run time system. NAME is copied to HEAP
 * in the RTS (length(name) bytes)
 * 
 * This should be a reasonable length.
 */
#define BINDING_NAME_LENGTH 255

/* Name of the standard input and output files
 */
#define INPUT_FILE_NAME   "Input"
#define OUTPUT_FILE_NAME  "Output"

/* declared in rt-rt0.c */
#define RTS_INPUT_FILE_NAME   "_p_stdin"
#define RTS_OUTPUT_FILE_NAME  "_p_stdout"

/* If nonzero coolecting constructors. If 0, running them */
#define RTS_COLLECT_FLAG "_p_collect_flag"

/* Implementation dependent length of the canonical-string-type
 * that is returned by the required function date(t)
 *
 * '14 Nov 1993'
 */
#define GPC_DATE_LENGTH	11

/* Implementation dependent length of the canonical-string-type
 * that is returned by the required function time(t)
 *
 * '22:55:26'
 */
#define GPC_TIME_LENGTH	 8

/* The following types are bindable in the rts-bind.c */
#define RTS_BIND_FILE 1

#endif /* _RTS_TYPES_H_ */
