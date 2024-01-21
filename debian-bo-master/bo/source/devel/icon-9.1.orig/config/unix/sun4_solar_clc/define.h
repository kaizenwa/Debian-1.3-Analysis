/*
 * Icon configuration file for Sun 4 running Solaris 2.x with CenterLine C
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
#define SUN
#define ZERODIVIDE
#define MaxHdr 8000
#define CLK_TCK (_sysconf(3))	/* from <limits.h> */

#define LoadFunc

/* CPU architecture */
#define Double
#define StackAlign 8

/* use clcc to compile generated code */
#define CComp "clcc"
#define COpts "-w -I/usr/openwin/include -ldl -R/usr/openwin/lib"
