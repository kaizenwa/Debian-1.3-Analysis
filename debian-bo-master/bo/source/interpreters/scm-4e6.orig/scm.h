/* Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */

/* "scm.h" SCM data types and external functions. */

#ifdef __cplusplus
extern "C" {
#endif

typedef long SCM;
typedef struct {SCM car, cdr;} cell;
typedef struct {long sname;SCM (*cproc)();} subr;
typedef struct {char *string;SCM (*cproc)();} iproc;
typedef struct {long sname;double (*dproc)();} dsubr;

#include <stdio.h>
#include "scmfig.h"

#ifdef USE_ANSI_PROTOTYPES
# define P(s) s
#else
# define P(s) ()
#endif

#ifndef STDC_HEADERS
	int isatty P((int));
#endif

typedef struct {
  SCM	(*mark)P((SCM));
  sizet	(*free)P((CELLPTR));
  int	(*print)P((SCM exp, SCM port, int writing));
  SCM	(*equalp)P((SCM, SCM));
} smobfuns;

typedef struct {
  SCM	(*mark)P((SCM ptr));
  int	(*free)P((FILE *p));
  int	(*print)P((SCM exp, SCM port, int writing));
  SCM	(*equalp)P((SCM, SCM));
  int	(*fputc)P((int c, FILE *p));
  int	(*fputs)P((char *s, FILE *p));
  sizet	(*fwrite)P((char *s, sizet siz, sizet num, FILE *p));
  int	(*fflush)P((FILE *stream));
  int	(*fgetc)P((FILE *p));
  int	(*fclose)P((FILE *p));
} ptobfuns;

typedef struct {
  SCM v;
  sizet base;
} array;
typedef struct {
  long lbnd;
  long ubnd;
  long inc;
} array_dim;

#ifdef FLOATS
typedef struct {char *string;double (*cproc)P((double));} dblproc;
# ifdef SINGLES
#  ifdef CDR_DOUBLES
typedef struct {SCM type;double num;} flo;
#  else
typedef struct {SCM type;float num;} flo;
#  endif
# endif
typedef struct {SCM type;double *real;} dbl;
#endif

#define IMP(x) (6 & (int)(x))
#define NIMP(x) (!IMP(x))

#define INUMP(x) (2 & (int)(x))
#define NINUMP(x) (!INUMP(x))
#define INUM0 ((SCM) 2)
#define ICHRP(x) ((0xff & (int)(x))==0xf4)
#define ICHR(x) ((unsigned char)((x)>>8))
#define MAKICHR(x) (((x)<<8)+0xf4L)

#define ILOCP(n) ((0xff & (int)(n))==0xfc)
#define ILOC00	(0x000000fcL)
#define IDINC	(0x00100000L)
#define ICDR	(0x00080000L)
#define IFRINC	(0x00000100L)
#define IDSTMSK	(-IDINC)
#define IFRAME(n) ((int)((ICDR-IFRINC)>>8) & ((int)(n)>>8))
#define IDIST(n) (((unsigned long)(n))>>20)
#define ICDRP(n) (ICDR & (n))

/* ISYMP tests for ISPCSYM and ISYM */
#define ISYMP(n) ((0x187 & (int)(n))==4)
/* IFLAGP tests for ISPCSYM, ISYM and IFLAG */
#define IFLAGP(n) ((0x87 & (int)(n))==4)
#define ISYMNUM(n) ((int)((n)>>9))
#define ISYMCHARS(n) (isymnames[ISYMNUM(n)])
#define MAKSPCSYM(n) (((n)<<9)+((n)<<3)+4L)
#define MAKISYM(n) (((n)<<9)+0x74L)
#define MAKIFLAG(n) (((n)<<9)+0x174L)

extern char *isymnames[];
#define NUM_ISPCSYM 14
#define IM_AND MAKSPCSYM(0)
#define IM_BEGIN MAKSPCSYM(1)
#define IM_CASE MAKSPCSYM(2)
#define IM_COND MAKSPCSYM(3)
#define IM_DO MAKSPCSYM(4)
#define IM_IF MAKSPCSYM(5)
#define IM_LAMBDA MAKSPCSYM(6)
#define IM_LET MAKSPCSYM(7)
#define IM_LETSTAR MAKSPCSYM(8)
#define IM_LETREC MAKSPCSYM(9)
#define IM_OR MAKSPCSYM(10)
#define IM_QUOTE MAKSPCSYM(11)
#define IM_SET MAKSPCSYM(12)
#define IM_DEFINE MAKSPCSYM(13)

#define s_and (ISYMCHARS(IM_AND)+2)
#define s_begin (ISYMCHARS(IM_BEGIN)+2)
#define s_case (ISYMCHARS(IM_CASE)+2)
#define s_cond (ISYMCHARS(IM_COND)+2)
#define s_do (ISYMCHARS(IM_DO)+2)
#define s_if (ISYMCHARS(IM_IF)+2)
#define s_lambda (ISYMCHARS(IM_LAMBDA)+2)
#define s_let (ISYMCHARS(IM_LET)+2)
#define s_letstar (ISYMCHARS(IM_LETSTAR)+2)
#define s_letrec (ISYMCHARS(IM_LETREC)+2)
#define s_or (ISYMCHARS(IM_OR)+2)
#define s_quote (ISYMCHARS(IM_QUOTE)+2)
#define s_set (ISYMCHARS(IM_SET)+2)
#define s_define (ISYMCHARS(IM_DEFINE)+2)

extern SCM i_dot, i_quote, i_quasiquote, i_unquote, i_uq_splicing;
#define s_apply (ISYMCHARS(IM_APPLY)+2)

/* each symbol defined here must have a unique number which */
 /* corresponds to it's position in isymnames[] in sys.c */
#define IM_APPLY MAKISYM(14)
#define IM_CONT MAKISYM(15)

#define NUM_ISYMS 16

#define BOOL_F MAKIFLAG(NUM_ISYMS+0)
#define BOOL_T MAKIFLAG(NUM_ISYMS+1)
#define UNDEFINED MAKIFLAG(NUM_ISYMS+2)
#define EOF_VAL MAKIFLAG(NUM_ISYMS+3)
#ifdef SICP
# define EOL BOOL_F
#else
# define EOL MAKIFLAG(NUM_ISYMS+4)
#endif
#define UNSPECIFIED MAKIFLAG(NUM_ISYMS+5)

/* Now some unnamed flags used as magic cookies by repl_driver. */
/* Argument n can range from -4 to 16 */
#ifdef SHORT_INT
#define COOKIE(n) (n)
#define UNCOOK(f) (f)
#else
#define COOKIE(n) MAKIFLAG(NUM_ISYMS+6+4+n)
#define UNCOOK(f) (ISYMNUM(f)-(NUM_ISYMS+6+4))
#endif

#define FALSEP(x) (BOOL_F==(x))
#define NFALSEP(x) (BOOL_F != (x))
/* BOOL_NOT returns the other boolean.  The order of ^s here is
   important for Borland C++. */
#define BOOL_NOT(x)  ((x) ^ (BOOL_T ^ BOOL_F))
#define NULLP(x) (EOL==(x))
#define NNULLP(x) (EOL != (x))
#define UNBNDP(x) (UNDEFINED==(x))
#define CELLP(x) (!NCELLP(x))
#define NCELLP(x) ((sizeof(cell)-1) & (int)(x))

#define GCMARKP(x) (1 & (int)CDR(x))
#define GC8MARKP(x) (0x80 & (int)CAR(x))
#define SETGCMARK(x) CDR(x) |= 1;
#define CLRGCMARK(x) CDR(x) &= ~1L;
#define SETGC8MARK(x) CAR(x) |= 0x80;
#define CLRGC8MARK(x) CAR(x) &= ~0x80L;
#define TYP3(x) (7 & (int)CAR(x))
#define TYP7(x) (0x7f & (int)CAR(x))
#define TYP7S(x) (0x7d & (int)CAR(x))
#define TYP16(x) (0xffff & (int)CAR(x))
#define TYP16S(x) (0xfeff & (int)CAR(x))
#define GCTYP16(x) (0xff7f & (int)CAR(x))

#define NCONSP(x) (1 & (int)CAR(x))
#define CONSP(x) (!NCONSP(x))
#define ECONSP(x) (CONSP(x) || (1==TYP3(x)))
#define NECONSP(x) (NCONSP(x) && (1 != TYP3(x)))

#define CAR(x) (((cell *)(SCM2PTR(x)))->car)
#define CDR(x) (((cell *)(SCM2PTR(x)))->cdr)
#define GCCDR(x) (~1L & CDR(x))
#define SETCDR(x, v) CDR(x) = (SCM)(v)

#define CLOSUREP(x) (TYP3(x)==tc3_closure)
#define CODE(x) (CAR(x)-tc3_closure)
#define SETCODE(x, e) CAR(x) = (e)+tc3_closure
#define ENV(x) CDR(x)

#define PORTP(x) (TYP7(x)==tc7_port)
#define OPPORTP(x) (((0x7f | OPN) & CAR(x))==(tc7_port | OPN))
#define OPINPORTP(x) (((0x7f | OPN | RDNG) & CAR(x))==(tc7_port | OPN | RDNG))
#define OPOUTPORTP(x) (((0x7f | OPN | WRTNG) & CAR(x))==(tc7_port | OPN | WRTNG))
#define OPIOPORTP(x) (((0x7f | OPN | RDNG | WRTNG) & CAR(x))==(tc7_port | OPN | RDNG | WRTNG))
#define FPORTP(x) (TYP16S(x)==tc7_port)
#define OPFPORTP(x) (((0xfeff | OPN) & CAR(x))==(tc7_port | OPN))
#define OPINFPORTP(x) (((0xfeff | OPN | RDNG) & CAR(x))==(tc7_port | OPN | RDNG))
#define OPOUTFPORTP(x) (((0xfeff | OPN | WRTNG) & CAR(x))==(tc7_port | OPN | WRTNG))

#define INPORTP(x) (((0x7f | RDNG) & CAR(x))==(tc7_port | RDNG))
#define OUTPORTP(x) (((0x7f | WRTNG) & CAR(x))==(tc7_port | WRTNG))
#define OPENP(x) (OPN & CAR(x))
#define CLOSEDP(x) (!OPENP(x))
#define STREAM(x) ((FILE *)(CDR(x)))
#define SETSTREAM SETCDR
#define CRDYP(port) (CAR(port) & CRDY)
#define CLRDY(port) {CAR(port) &= CUC;}
#define CGETUN(port) ((int)SRS(CAR(port), 22))
#define CUNGET(c, port) {CAR(port) += ((long)c<<22) + CRDY;}

#define tc_socket (tc7_port | OPN)
#define SOCKP(x) (((0x7f | OPN | RDNG | WRTNG) & CAR(x))==(tc_socket))
#define SOCKTYP(x) (CAR(x)>>24)

#define DIRP(x) (NIMP(x) && (TYP16(x)==(tc16_dir)))
#define OPDIRP(x) (NIMP(x) && (CAR(x)==(tc16_dir | OPN)))

#ifdef FLOATS
# define INEXP(x) (TYP16(x)==tc16_flo)
# define CPLXP(x) (CAR(x)==tc_dblc)
# define REAL(x) (*(((dbl *) (SCM2PTR(x)))->real))
# define IMAG(x) (*((double *)(CHARS(x)+sizeof(double))))
/* ((&REAL(x))[1]) */
# ifdef SINGLES
#  define REALP(x) ((~REAL_PART & CAR(x))==tc_flo)
#  define SINGP(x) (CAR(x)==tc_flo)
#  define FLO(x) (((flo *)(SCM2PTR(x)))->num)
#  define REALPART(x) (SINGP(x)?0.0+FLO(x):REAL(x))
# else /* SINGLES */
#  define REALP(x) (CAR(x)==tc_dblr)
#  define REALPART REAL
# endif /* SINGLES */
#endif

#ifdef FLOATS
# define NUMBERP(x) (INUMP(x) || (NIMP(x) && NUMP(x)))
#else
# ifdef BIGDIG
#  define NUMBERP(x) (INUMP(x) || (NIMP(x) && NUMP(x)))
# else
#  define NUMBERP INUMP
# endif
#endif
#define NUMP(x) ((0xfcff & (int)CAR(x))==tc7_smob)
#define BIGP(x) (TYP16S(x)==tc16_bigpos)
#define BIGSIGN(x) (0x0100 & (int)CAR(x))
#define BDIGITS(x) ((BIGDIG *)(CDR(x)))
#define NUMDIGS(x) ((sizet)(CAR(x)>>16))
#define SETNUMDIGS(x, v, t) CAR(x) = (((v)+0L)<<16)+(t)

#define SNAME(x) ((CAR(x)>>8)?(SCM)(heap_org+(CAR(x)>>8)):nullstr)
#define SUBRF(x) (((subr *)(SCM2PTR(x)))->cproc)
#define DSUBRF(x) (((dsubr *)(SCM2PTR(x)))->dproc)
#define CCLO_SUBR(x) (VELTS(x)[0])

#define SYMBOLP(x) (TYP7S(x)==tc7_ssymbol)
#define STRINGP(x) (TYP7(x)==tc7_string)
#define NSTRINGP(x) (!STRINGP(x))
#define VECTORP(x) (TYP7(x)==tc7_vector)
#define NVECTORP(x) (!VECTORP(x))
#define LENGTH(x) (((unsigned long)CAR(x))>>8)
#define LENGTH_MAX (0xffffffL)
#define SETLENGTH(x, v, t) CAR(x) = ((v)<<8)+(t)
#define CHARS(x) ((char *)(CDR(x)))
#define UCHARS(x) ((unsigned char *)(CDR(x)))
#define VELTS(x) ((SCM *)CDR(x))
#define SETCHARS SETCDR
#define SETVELTS SETCDR

extern long tc16_array;
#define ARRAYP(a) (tc16_array==TYP16(a))
#define ARRAY_V(a) (((array *)CDR(a))->v)
/*#define ARRAY_NDIM(x) NUMDIGS(x)*/
#define ARRAY_NDIM(x) ((sizet)(CAR(x)>>17))
#define ARRAY_CONTIGUOUS 0x10000
#define ARRAY_CONTP(x) (ARRAY_CONTIGUOUS & (int)CAR(x))
#define ARRAY_BASE(a) (((array *)CDR(a))->base)
#define ARRAY_DIMS(a) ((array_dim *)(CHARS(a)+sizeof(array)))

#define FREEP(x) (CAR(x)==tc_free_cell)
#define NFREEP(x) (!FREEP(x))

#define SMOBNUM(x) (0x0ff & (CAR(x)>>8));
#define PTOBNUM(x) (0x0ff & (CAR(x)>>8));

#define DIGITS '0':case '1':case '2':case '3':case '4':\
		case '5':case '6':case '7':case '8':case '9'

/* Aggregated types for dispatch in switch statements. */

#define tcs_cons_imcar 2:case 4:case 6:case 10:\
		 case 12:case 14:case 18:case 20:\
		 case 22:case 26:case 28:case 30:\
		 case 34:case 36:case 38:case 42:\
		 case 44:case 46:case 50:case 52:\
		 case 54:case 58:case 60:case 62:\
		 case 66:case 68:case 70:case 74:\
		 case 76:case 78:case 82:case 84:\
		 case 86:case 90:case 92:case 94:\
		 case 98:case 100:case 102:case 106:\
		 case 108:case 110:case 114:case 116:\
		 case 118:case 122:case 124:case 126
#define tcs_cons_nimcar 0:case 8:case 16:case 24:\
		 case 32:case 40:case 48:case 56:\
		 case 64:case 72:case 80:case 88:\
		 case 96:case 104:case 112:case 120
#define tcs_cons_gloc 1:case 9:case 17:case 25:\
		 case 33:case 41:case 49:case 57:\
		 case 65:case 73:case 81:case 89:\
		 case 97:case 105:case 113:case 121

#define tcs_closures   3:case 11:case 19:case 27:\
		 case 35:case 43:case 51:case 59:\
		 case 67:case 75:case 83:case 91:\
		 case 99:case 107:case 115:case 123
#define tcs_subrs tc7_asubr:case tc7_subr_0:case tc7_subr_1:case tc7_cxr:\
	case tc7_subr_3:case tc7_subr_2:case tc7_rpsubr:case tc7_subr_1o:\
	case tc7_subr_2o:case tc7_lsubr_2:case tc7_lsubr
#define tcs_symbols tc7_ssymbol:case tc7_msymbol
#define tcs_bignums tc16_bigpos:case tc16_bigneg

#define tc3_cons	0
#define tc3_cons_gloc	1
#define tc3_closure	3

#define tc7_ssymbol	5
#define tc7_msymbol	7
#define tc7_string	13
#define tc7_vector	15
#define tc7_bvect	21
/* spare 23 */
#define tc7_ivect	29
#define tc7_uvect	31
/* spare 37 39 */
#define tc7_fvect	45
#define tc7_dvect	47
#define tc7_cvect	53
#define tc7_port	55
#define tc7_contin	61
#define tc7_cclo	63

/* spare 69 71 77 79 */
#define tc7_subr_0	85
#define tc7_subr_1	87
#define tc7_cxr		93
#define tc7_subr_3	95
#define tc7_subr_2	101
#define tc7_asubr	103
#define tc7_subr_1o	109
#define tc7_subr_2o	111
#define tc7_lsubr_2	117
#define tc7_lsubr	119
#define tc7_rpsubr	125

#define tc7_smob	127
#define tc_free_cell	127

#define tc16_flo	0x017f
#define tc_flo		0x017fL

#define REAL_PART	(1L<<16)
#define IMAG_PART	(2L<<16)
#define tc_dblr		(tc16_flo|REAL_PART)
#define tc_dblc		(tc16_flo|REAL_PART|IMAG_PART)

#define tc16_bigpos	0x027f
#define tc16_bigneg	0x037f

#define OPN		(1L<<16)
#define RDNG		(2L<<16)
#define WRTNG		(4L<<16)
#define BUF0		(8L<<16)
#define CRDY		(32L<<16)
#define CUC		0x001fffffL

extern sizet numsmob, numptob;
extern smobfuns *smobs;
extern ptobfuns *ptobs;
extern ptobfuns pipob;
#define tc16_fport (tc7_port + 0*256L)
#define tc16_pipe (tc7_port + 1*256L)
#define tc16_strport (tc7_port + 2*256L)
#define tc16_sfport (tc7_port + 3*256L)
extern long tc16_dir;

extern SCM sys_protects[];
#define cur_inp sys_protects[0]
#define cur_outp sys_protects[1]
#define cur_errp sys_protects[2]
#define def_inp sys_protects[3]
#define def_outp sys_protects[4]
#define def_errp sys_protects[5]
#define listofnull sys_protects[6]
#define undefineds sys_protects[7]
#define nullvect sys_protects[8]
#define nullstr sys_protects[9]
#define progargs sys_protects[10]
#define transcript sys_protects[11]
#define rootcont sys_protects[12]
#define dynwinds sys_protects[13]
#define stacktrace sys_protects[14]
#ifdef FLOATS
# define flo0 sys_protects[15]
# define NUM_PROTECTS 16
#else
# define NUM_PROTECTS 15
#endif

/* now for connects between source files */

extern sizet num_finals;
extern void (**finals)P((void));
extern unsigned char upcase[], downcase[];
extern SCM symhash;
extern int symhash_dim;
extern long heap_size;
extern CELLPTR heap_org;
extern SCM freelist;
extern long gc_cells_collected, gc_malloc_collected, gc_ports_collected;
extern long gc_syms_collected;
extern long cells_allocated, lcells_allocated, mallocated, lmallocated;
extern long mtrigger;
extern SCM *loc_loadpath;
extern SCM *loc_errobj;
extern SCM loadport;
extern long linum;
extern int errjmp_bad, ints_disabled, sig_deferred, alrm_deferred;
extern SCM exitval;
extern int cursinit;
extern unsigned int poll_count, tick_count;
extern int dumped;
extern char *execpath;

/* strings used in several source files */

extern char s_read[], s_write[], s_newline[], s_system[];
extern char s_make_string[], s_make_vector[], s_list[], s_op_pipe[];
#define s_string (s_make_string+5)
#define s_vector (s_make_vector+5)
#define s_pipe (s_op_pipe+5)
extern char s_make_sh_array[];
#define s_array (s_make_sh_array+12)
extern char s_ccl[];
#define s_limit (s_ccl+10)
extern char s_close_port[];
#define s_port_type (s_close_port+6)

/* function prototypes */

void	gc_mark P((SCM p));
void	han_sig P((void));
void	han_alrm P((void));
char	*must_malloc P((long len, char *what));
char	*must_realloc P((char *where, long olen, long len, char *what));
void	must_free P((char *obj));
long	ilength P((SCM sx));
SCM	hash P((SCM obj, SCM n));
SCM	hashv P((SCM obj, SCM n));
SCM	hashq P((SCM obj, SCM n));
SCM	obhash P((SCM obj));
SCM	obunhash P((SCM obj));
unsigned long strhash P((unsigned char *str, sizet len, unsigned long n));
unsigned long hasher P((SCM obj, unsigned long n, sizet d));
SCM	repl_driver P((char *initpath));
SCM	lroom P((SCM args));
long	newsmob P((smobfuns *smob));
long	newptob P((ptobfuns *ptob));
void	prinport P((SCM exp, SCM port, char *type));
void	repl P((void));
void	growth_mon P((char *obj, long size, char *units));
void	gc_start P((char *what));
void	gc_end P((void));
void	heap_report P((void));
void	exit_report P((void));
void	stack_report P((void));
void	iprin1 P((SCM exp, SCM port, int writing));
void	intprint P((long n, int radix, SCM port));
void	iprlist P((char *hdr, SCM exp, int tlr, SCM port, int writing));
void	lputc P((int c, SCM port));
void	lputs P((char *s, SCM port));
int	lfwrite P((char *ptr, sizet size, sizet nitems, SCM port));
int	lgetc P((SCM port));
void	lungetc P((int c, SCM port));
char	*grow_tok_buf P((SCM tok_buf));
long	mode_bits P((char *modes));
long	time_in_msec P((long x));
SCM	my_time P((void));
SCM	your_time P((void));
void	init_iprocs P((iproc *subra, int type));
void	init_scm P((int iverbose, int buf0stdin, long init_heap_size));
SCM	scm_init_extensions P((void));
void	ignore_signals P((void));
void	unignore_signals P((void));
void	free_storage P((void));
void	add_feature P((char *str));
int	raprin1 P((SCM exp, SCM port, int writing));
SCM	markcdr P((SCM ptr));
SCM	mark0 P((SCM ptr));
SCM	equal0 P((SCM ptr1, SCM ptr2));
sizet	free0 P((CELLPTR ptr));
void	warn P((char *str1, char *str2));
void	everr P((SCM exp, SCM env, SCM arg, char *pos, char *s_subr));
void	wta P((SCM arg, char *pos, char *s_subr));
SCM	intern P((char *name, sizet len));
SCM	sysintern P((char *name, SCM val));
SCM	sym2vcell P((SCM sym));
SCM	makstr P((long len));
SCM	make_subr P((char *name, int type, SCM (*fcn)()));
SCM	closure P((SCM code, SCM env));
SCM	makprom P((SCM code));
SCM	force P((SCM x));
SCM	makarb P((SCM name));
SCM	tryarb P((SCM arb));
SCM	relarb P((SCM arb));
SCM	ceval P((SCM x, SCM env));
SCM	prolixity P((SCM arg));
SCM	gc_for_newcell P((void));
SCM	gc P((void));
SCM	tryload P((SCM filename));
SCM	acons P((SCM w, SCM x, SCM y));
SCM	cons2 P((SCM w, SCM x, SCM y));
SCM	resizuve P((SCM vect, SCM len));
SCM	lnot P((SCM x));
SCM	booleanp P((SCM obj));
SCM	eq P((SCM x, SCM y));
SCM	equal P((SCM x, SCM y));
SCM	consp P((SCM x));
SCM	cons P((SCM x, SCM y));
SCM	nullp P((SCM x));
SCM	setcar P((SCM pair, SCM value));
SCM	setcdr P((SCM pair, SCM value));
SCM	listp P((SCM x));
SCM	list P((SCM objs));
SCM	length P((SCM x));
SCM	append P((SCM args));
SCM	reverse P((SCM lst));
SCM	list_ref P((SCM lst, SCM k));
SCM	memq P((SCM x, SCM lst));
SCM	member P((SCM x, SCM lst));
SCM	memv P((SCM x, SCM lst));
SCM	assq P((SCM x, SCM alist));
SCM	assoc P((SCM x, SCM alist));
SCM	symbolp P((SCM x));
SCM	symbol2string P((SCM s));
SCM	string2symbol P((SCM s));
SCM	numberp P((SCM x));
SCM	exactp P((SCM x));
SCM	inexactp P((SCM x));
SCM	eqp P((SCM x, SCM y));
SCM	lessp P((SCM x, SCM y));
SCM	zerop P((SCM z));
SCM	positivep P((SCM x));
SCM	negativep P((SCM x));
SCM	oddp P((SCM n));
SCM	evenp P((SCM n));
SCM	lmax P((SCM x, SCM y));
SCM	lmin P((SCM x, SCM y));
SCM	sum P((SCM x, SCM y));
SCM	difference P((SCM x, SCM y));
SCM	product P((SCM x, SCM y));
SCM	divide P((SCM x, SCM y));
SCM	lquotient P((SCM x, SCM y));
SCM	absval P((SCM x));
SCM	lremainder P((SCM x, SCM y));
SCM	modulo P((SCM x, SCM y));
SCM	lgcd P((SCM x, SCM y));
SCM	llcm P((SCM n1, SCM n2));
SCM	number2string P((SCM x, SCM radix));
SCM	istring2number P((char *str, long len, long radix));
SCM	string2number P((SCM str, SCM radix));
SCM	istr2flo P((char *str, long len, long radix));
SCM	mkbig P((sizet nlen, int sign));
SCM	mkstrport P((SCM pos, SCM str, long modes, char *caller));
SCM	long2big P((long n));
SCM	ulong2big P((unsigned long n));
SCM	big2inum P((SCM b, sizet l));
sizet	iint2str P((long num, int rad, char *p));
SCM	floequal P((SCM x, SCM y));
SCM	uve_equal P((SCM u, SCM v));
SCM	raequal P((SCM ra0, SCM ra1));
SCM	array_equal P((SCM u, SCM v));
int     rafill P((SCM ra, SCM fill, SCM ignore));
SCM	uve_fill P((SCM uve, SCM fill));
SCM	array_fill P((SCM ra, SCM fill));
SCM	array_prot P((SCM ra));
int	bigprint P((SCM exp, SCM port, int writing));
int	floprint P((SCM sexp, SCM port, int writing));
SCM	istr2int P((char *str, long len, long radix));
SCM	istr2bve P((char *str, long len));
void	ipruk P((char *hdr, SCM ptr, SCM port));
SCM	charp P((SCM x));
SCM	char_lessp P((SCM x, SCM y));
SCM	chci_eq P((SCM x, SCM y));
SCM	chci_lessp P((SCM x, SCM y));
SCM	char_alphap P((SCM chr));
SCM	char_nump P((SCM chr));
SCM	char_whitep P((SCM chr));
SCM	char_upperp P((SCM chr));
SCM	char_lowerp P((SCM chr));
SCM	char2int P((SCM chr));
SCM	int2char P((SCM n));
SCM	char_upcase P((SCM chr));
SCM	char_downcase P((SCM chr));
SCM	stringp P((SCM x));
SCM	string P((SCM chrs));
SCM	make_string P((SCM k, SCM chr));
SCM	string2list P((SCM str));
SCM	st_length P((SCM str));
SCM	st_ref P((SCM str, SCM k));
SCM	st_set P((SCM str, SCM k, SCM chr));
SCM	st_equal P((SCM s1, SCM s2));
SCM	stci_equal P((SCM s1, SCM s2));
SCM	st_lessp P((SCM s1, SCM s2));
SCM	stci_lessp P((SCM s1, SCM s2));
SCM	substring P((SCM str, SCM start, SCM end));
SCM	st_append P((SCM args));
SCM	vectorp P((SCM x));
SCM	vector_length P((SCM v));
SCM	vector P((SCM l));
SCM	vector_ref P((SCM v, SCM k));
SCM	vector_set P((SCM v, SCM k, SCM obj));
SCM	make_vector P((SCM k, SCM fill));
SCM	vector2list P((SCM v));
SCM	for_each P((SCM proc, SCM arg1, SCM args));
SCM	procedurep P((SCM obj));
SCM	apply P((SCM proc, SCM arg1, SCM args));
SCM	map P((SCM proc, SCM arg1, SCM args));
SCM	scm_make_cont P((void));
SCM	copytree P((SCM obj));
SCM	eval P((SCM obj));
SCM	input_portp P((SCM x));
SCM	output_portp P((SCM x));
SCM	cur_input_port P((void));
SCM	cur_output_port P((void));
SCM	i_setbuf0 P((SCM port));
SCM	open_file P((SCM filename, SCM modes));
SCM	open_pipe P((SCM pipestr, SCM modes));
SCM	close_port P((SCM port));
SCM	lread P((SCM port));
SCM	scm_read_char P((SCM port));
SCM	peek_char P((SCM port));
SCM	eof_objectp P((SCM x));
SCM	lwrite P((SCM obj, SCM port));
SCM	display P((SCM obj, SCM port));
SCM	newline P((SCM port));
SCM	write_char P((SCM chr, SCM port));
SCM	file_position P((SCM port));
SCM	file_set_position P((SCM port, SCM pos));
SCM	lgetenv P((SCM nam));
SCM	prog_args P((void));
SCM	makacro P((SCM code));
SCM	makmacro P((SCM code));
SCM	makmmacro P((SCM code));
void	poll_routine P((void));
void	tick_signal P((void));
void	stack_check P((void));
SCM	list2ura P((SCM ndim, SCM prot, SCM lst));
SCM	make_ra P((int ndim));
SCM	makflo P((float x));
SCM	arrayp P((SCM v, SCM prot));
SCM	array_contents P((SCM ra, SCM strict));
SCM     uve_read P((SCM v, SCM port));
SCM     uve_write P((SCM v, SCM port));
SCM     ura_read P((SCM v, SCM port));
SCM     ura_write P((SCM v, SCM port));
SCM	aset P((SCM v, SCM obj, SCM args));
SCM	aref P((SCM v, SCM args));
SCM	cvref P((SCM v, sizet pos, SCM last));
SCM	quit P((SCM n));
void	add_final P((void (*final)(void)));
SCM	makcclo P((SCM proc, long len));
SCM	make_uve P((long k, SCM prot));
SCM	ra2contig P((SCM ra, int copy));
SCM	sc2array P((SCM s, SCM ra, SCM prot));
SCM	array_copy P((SCM src, SCM dst));
long	aind P((SCM ra, SCM args, char *what));
SCM	scm_eval_string P((SCM str));
SCM	scm_load_string P((SCM str));
void	scm_print_stack P((SCM stk));
char *	dld_find_executable P((const char* command));
SCM	scm_unexec P((const SCM pathname));
char *	scm_cat_path P((char *str1, const char *str2, long n));

				/* Defined in "rope.c" */
SCM	 long2num P((long n));
SCM	ulong2num P((unsigned long n));
unsigned char  num2uchar  P((SCM num, char *pos, char *s_caller));
unsigned short num2ushort P((SCM num, char *pos, char *s_caller));
unsigned long  num2ulong  P((SCM num, char *pos, char *s_caller));
	 long  num2long   P((SCM num, char *pos, char *s_caller));
        double num2dbl    P((SCM num, char *pos, char *s_caller));
SCM	makfromstr  P((char *src, sizet len));
SCM	makfromstrs P((int argc, char **argv));
SCM	makfrom0str P((char *scr));
char  **makargvfrmstrs P((SCM args, char *s_v));
void	must_free_argv P((char **argv));
SCM	scm_evstr  P((char *str));
void	scm_ldstr  P((char *str));
int	scm_ldfile P((char *path));
int	scm_ldprog P((char *path));
unsigned long scm_addr P((SCM args, char *name));

#ifdef FLOATS
SCM	makdbl P((double x, double y));
SCM	dbl2big P((double d));
double	big2dbl P((SCM b));
double	lasinh P((double x));
double	lacosh P((double x));
double	latanh P((double x));
double	ltrunc P((double x));
double	round P((double x));
double	floident P((double x));
#endif

#ifdef BIGDIG
void	longdigs P((long x, BIGDIG digs [DIGSPERLONG ]));
SCM	adjbig P((SCM b, sizet nlen));
SCM	normbig P((SCM b));
SCM	copybig P((SCM b, int sign));
SCM	addbig P((BIGDIG *x, sizet nx, int xsgn, SCM bigy, int sgny));
SCM	mulbig P((BIGDIG *x, sizet nx, BIGDIG *y, sizet ny, int sgn));
unsigned int divbigdig P((BIGDIG *ds, sizet h, BIGDIG div));
SCM	divbigint P((SCM x, long z, int sgn, int mode));
SCM	divbigbig P((BIGDIG *x, sizet nx, BIGDIG *y, sizet ny, int sgn,
		   int modes));
long	 pseudolong P((long x));
#endif
int	bigcomp P((SCM x, SCM y));
SCM	bigequal P((SCM x, SCM y));

#ifdef RECKLESS
# define ASSERT(_cond, _arg, _pos, _subr) ;
# define ASRTGO(_cond, _label) ;
#else
# define ASSERT(_cond, _arg, _pos, _subr) if(!(_cond))wta(_arg, (char *)(_pos), _subr);
# define ASRTGO(_cond, _label) if(!(_cond)) goto _label;
#endif

#define ARGn 0
#define ARG1 1
#define ARG2 2
#define ARG3 3
#define ARG4 4
#define ARG5 5
  /* following must match entry indexes in errmsgs[] */
#define WNA 6
#define OVFLOW 7
#define OUTOFRANGE 8
#define NALLOC 9
#define EXIT 10
#define HUP_SIGNAL 11
#define INT_SIGNAL 12
#define FPE_SIGNAL 13
#define BUS_SIGNAL 14
#define SEGV_SIGNAL 15
#define ALRM_SIGNAL 16

#define EVAL(x, env) (IMP(x)?(x):ceval((x), (env)))
#define SIDEVAL(x, env) if NIMP(x) ceval((x), (env))

#define NEWCELL(_into) {if IMP(freelist) _into = gc_for_newcell();\
	else {_into = freelist;freelist = CDR(freelist);++cells_allocated;}}

#ifdef __cplusplus
}
#endif
