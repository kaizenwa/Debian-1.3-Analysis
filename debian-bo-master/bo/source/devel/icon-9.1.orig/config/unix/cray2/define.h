/*
 * Definitions for Cray2 (probably will work for xmp, ymp
 */
#define StandardPP
#define StandardC
#define IntBits		64
#define WordBits	64
#define index		strchr
#define rindex		strrchr
#define GetHost
#define NoCoexpr
#define NoHeader

#define MinLong  ((long int)0xc000000000000000) /* smallest long integer */

#define MaxLong  ((long int)0x3fffffffffffffff) /* largest long integer */

#define MaxNegInt "-4611686018427387904"

#define Big	140737488355328.		/* 47 bits of precision */
#include <sys/param.h>
#define Hz HZ

#define QualLstSize	 10000
#define StackSize	  4000
#define MStackSize	 20000
#define MaxStrSpace	128000
#define MaxAbrSize	250000

#define F_Nqual  0x80000000000
#define F_Var	 0x40000000000
#define F_Tvar	 0x20000000000
#define F_Ptr	 0x10000000000

#define UNIX 1
