/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     config.h
 * Abstract:        configuration file. Before any compilation please check
 *                  that actual configuration is consistent with your
 *                  hardware AND your compiler.
 */

/* module definition: 1 indicates that a module should be included in the
 * base vocabulary, 0 excludes a module. Note however that some words in
 * excluded word lists may be linked to final code if used by other words.
 */

#define COREE_DEF           1L
#define DOUBLE_DEF          1L
#define DOUBLEE_DEF         1L
#define FLOAT_DEF           1L
#define FLOATE_DEF          1L
#define MEMALL_DEF          1L
#define MEMALLE_DEF         0L
#define SEARCH_DEF          1L
#define SEARCHE_DEF         1L
#define TOOLS_DEF           1L
#define TOOLSE_DEF          1L
#define LOCALS_DEF          1L
#define LOCALSE_DEF         1L
#define FACILITY_DEF        1L
#define FACILITYE_DEF       0L
#define BLOCK_DEF           1L
#define BLOCKE_DEF          1L
#define EXCEPTION_DEF       1L
#define EXCEPTIONE_DEF      0L
#define FILE_DEF            1L
#define FILEE_DEF           1L
#define STRING_DEF          1L
#define STRINGE_DEF         0L

#define VERSION_PATTERN     (COREE_DEF | (DOUBLE_DEF << 1) |\
							 (DOUBLEE_DEF << 2) | (FLOAT_DEF << 3) |\
                             (FLOATE_DEF << 4) | (MEMALL_DEF << 5) |\
                             (MEMALLE_DEF << 6) | (SEARCH_DEF << 7) |\
                             (SEARCHE_DEF << 8) | (TOOLS_DEF << 9) |\
                             (TOOLSE_DEF << 10) | (LOCALS_DEF << 11) |\
                             (LOCALSE_DEF << 12) | (FACILITY_DEF << 13) |\
                             (FACILITYE_DEF << 14) | (BLOCK_DEF << 15) |\
                             (BLOCKE_DEF << 16) | (EXCEPTION_DEF << 17) |\
                             (EXCEPTIONE_DEF << 18) | (FILE_DEF << 19) |\
                             (FILEE_DEF << 20) | (STRING_DEF << 21) |\
                             (STRINGE_DEF << 22)\
                            )

/************************************************************************/
/* compilation and machine dependent definitions                        */
/************************************************************************/

/* Define LITTLE_ENDIAN if you machine is little-endian (e.g. Intel), undefine
 * it if your machine is big-endian (e.g. Motorola, Sparc...)
 * Note that some compilers have LITTLE_ENDIAN yet defined.
 */
#ifndef LITTLE_ENDIAN
#	define LITTLE_ENDIAN
#endif

/* When DCELL_MEM is defined, double cell transfer is realized by memory
 * copy, if not defined shift and logical operators are used to combine
 * or isolate cell values
 */
#define DCELL_MEM
 
/* DATA TYPES: please modify this list accordingly to your system. Note that
 * sizeof(DCell) == 2 * sizeof(Cell) MUST BE satisfied.
 * For example, using Borland C for DOS Cell may be "int" and DCell "long int".
 * Under Linux, Cell may be "int" and DCell "long long".
 */

#define Cell				int
#define Char				char
#define Real				long double	

#define UCell				unsigned Cell
#define DCell               long long
#define UDCell				unsigned DCell
#define UChar				unsigned Char

#define CellBits            (sizeof(Cell) * 8)
#define CellLog             (sizeof(Cell) - 1)
#define RealLog				(sizeof(Real) - 1)

#define FFLAG(n)            (-(n))

/* Please modify this definitions accordingly with your data types */

#define MAX_CHAR			UCHAR_MAX
#define MAX_D				LONG_MAX
#define MAX_N				INT_MAX
#define MAX_U				UINT_MAX
#define MAX_UD				ULONG_MAX
#define MAX_F				0.0

/* Some compilers doesn't provide some functions in the standard library.
 * If you don't have, undefine them
 */ 
#define HAVE_ACOSH
#define HAVE_ASINH
#define HAVE_ATANH
