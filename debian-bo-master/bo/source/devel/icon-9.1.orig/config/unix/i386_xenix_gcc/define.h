/*
 * Icon configuration file for the i386 running Xenix with Gcc.
 */
#define HostStr "SCO XENIX System V/386"
#define MaxHdr  15000
#define CComp "/usr/local/bin/gcc"
#define COpts "-O -finline-functions -fstrength-reduce -lx"
#define index strchr
#define rindex strrchr
#define UtsName
#define Hz	50
#define StandardC		/* for gcc */
#define StandardPP		/* for gcc */
#define XENIX_386	1
#define MaxStatSize	20480
#define UNIX	1
#define KeyboardFncs
#define HaveTermio
