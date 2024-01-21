/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     yforth.h
 * Abstract:        definition of constants, data types, prototypes, and so on.
 */

#ifndef __YFORTH__
#define __YFORTH__

#include <setjmp.h>
#include <limits.h>
#include "errors.h"

#include "config.h"

/* Following definitions may be tuned for a particular system. Note however
 * that their minimal value is defined by the standard.
 */

#define TMP_BUFFER_SIZE		80
#define FILE_BUFFER_SIZE	128
#define FILE_NAME_SIZE		128

#define MAX_LOCALS			8

#define VOC_HASH			8
#define WORD_LISTS			8

/* data structures definitions */

typedef void (*pfp)(void);

struct word_def {
	Char *name;
	struct word_def *link;
	Cell class;
	pfp func[1];
};

struct vocabulary {
	struct word_def *voc[VOC_HASH];
};

struct voc_marker {                         /* MARKER structure */
    struct vocabulary *list[WORD_LISTS];    /* vocabulary stack */
    Cell top;                               /* top of stack */
    struct vocabulary *voc;                 /* definition vocabulary */
    struct vocabulary v_list[WORD_LISTS];   /* content of vocabularies in stack */
    struct vocabulary v_voc;
	Char *_dp;								/* dictionary pointer */
    struct word_def *last;                  /* ptr to last defined word */
};

struct raw_voc {
	char *name;
	void (*func) (void);
	int class;
};

struct image_header {                       /* header for image file */
	Char header[24];
	Cell ver_hi, ver_lo;
	UCell pattern;
	Char *base;
	Cell dspace_size;
};

#ifdef DCELL_MEM
union double_cell {
	DCell d1;
	struct {
#ifdef LITTLE_ENDIAN
		Cell low;
		Cell high;
#else
		Cell high;
		Cell low;
#endif
	} d2;
};
DCell get_dcell(Cell *ptr);
void put_dcell(Cell *ptr, DCell d);
#endif

/* Some constant definitions. This should not be changed. */

#define INTERPRET       0
#define COMPILE         -1

#define BLOCK_SIZE		1024
#define NUM_BLOCKS		4

#define COMP_ONLY       0x0100
#define IMMEDIATE       0x0200
#define CLASS_MASK      (~(COMP_ONLY | IMMEDIATE))

#define A_PRIMITIVE     0
#define A_USER			1
#define A_VARIABLE      2
#define A_COLON			3
#define A_CONSTANT		4
#define A_FCONSTANT		5
#define A_FVARIABLE		6
#define A_CREATE		7
#define A_MARKER		8
#define A_2CONSTANT		9
#define A_2VARIABLE		10
#define A_LOCAL			11
#define A_VALUE			12
#define A_WORD			15

/* Some macros */

#define ALIGN_PTR(n)    	(((((Cell) (n)) - 1) | CellLog) + 1)
#define FALIGN_PTR(n)		(((((Cell) (n)) - 1) | RealLog) + 1)
#define WORD_PTR(ptr)   	(ALIGN_PTR((ptr) + *(ptr) + sizeof(Char)))
#define compile_cell(x)     *((Cell *) _dp) = x, _dp += sizeof(Cell)
#define compile_real(x)		*((Real *) _dp) = x, _dp += sizeof(Real)
#define hash_func(name,len)	((len) & (VOC_HASH - 1))
#ifdef DCELL_MEM
#	ifdef LITTLE_ENDIAN
#		define GET_DCELL(ptr)      	get_dcell((Cell *) ptr)
#		define PUT_DCELL(ptr, d)	put_dcell((Cell *) ptr, (DCell) d)
#	else
#		define GET_DCELL(ptr)		*((DCell *) ptr)
#		define PUT_DCELL(ptr, d)	*((DCell *) ptr) = d
#	endif
#else
#	ifdef LITTLE_ENDIAN
#		define GET_DCELL(ptr)		((DCell) (*(((Cell *) ptr) + 1)) + \
									(((DCell) (*((Cell *) ptr))) << CellBits))
#		define PUT_DCELL(ptr, d)	*(((Cell *) ptr) + 1) = (Cell) d, \
									*((Cell *) ptr) = (Cell) (d >> CellBits)
#	else
#		define GET_DCELL(ptr)		((DCell) (*((Cell *) ptr)) + \
									(((DCell) (*(((Cell *) ptr) + 1))) << CellBits))
#		define PUT_DCELL(ptr, d)	*((Cell *) ptr) = (Cell) d, \
									*(((Cell *) ptr) + 1) = (Cell) (d >> CellBits)
#	endif
#endif

#define GET_UDCELL(ptr)		((UDCell) GET_DCELL(ptr))
#define PUT_UDCELL(ptr, ud)	PUT_DCELL(ptr, ud)

/* Global variables */

extern jmp_buf warm_start_jump;
extern Char * dp0;
extern Cell dspace_size;
extern Cell dstack_size, rstack_size, fstack_size;
extern Cell tib_size;
extern Cell in_pnos, pnos_size;
extern Char * pnos, * p_pnos;
extern Cell pad_size;

extern struct vocabulary *list[WORD_LISTS];
extern Cell top;		/* indice primo vocabolario sulla pila */
extern struct vocabulary *voc;	/* ptr al vocabolario usato per le definzioni */
extern struct vocabulary *forth_wid;

/* Global functions prototypes */

void init_vocabulary(Char **dp);
void init_stacks(int dstack_size, int rstack_size, int fstack_size);
void init_data_space(int dspace_size);
void init_tib(int size);
void init_pad(int size);
void init_pnos(void);
void init_forth_environment(int reload);
void init_signals(void);
void print_version(void);

/* Virtual Machine registers definition */

extern pfp *ip;

extern Cell *sp, *sp_top, *sp_base;
extern Cell *rp, *rp_top, *rp_base;
extern Real *fp, *fp_top, *fp_base;
extern Cell *bp;

/* Some definitions that may be missing under certain systems or compilers */

#ifndef SEEK_SET
#   define SEEK_SET     0
#endif
#ifndef SEEK_CUR
#   define SEEK_CUR     1
#endif
#ifndef SEEK_END
#   define SEEK_END     2
#endif

#include "div.h"

#ifndef max
#   define max(a, b)    ((a) > (b) ? (a) : (b))
#endif

#endif
