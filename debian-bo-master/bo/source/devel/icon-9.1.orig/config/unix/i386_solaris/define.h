/*
 * Icon configuration file for i386 running Solaris 2.x with Sun cc
 */

/* configure for Unix System V release 4 */
#define UNIX 1
#define Standard
#define index strchr
#define rindex strrchr
#define UtsName
#define KeyboardFncs
#define HaveTermio
#define NoRanlib
#define SysOpt

/* Sun parameters */
#define SUN 1
#define ZERODIVIDE
#define MaxHdr 8000
#define CLK_TCK (_sysconf(3))	/* from <limits.h> */

#define COpts "-I/usr/openwin/include"

