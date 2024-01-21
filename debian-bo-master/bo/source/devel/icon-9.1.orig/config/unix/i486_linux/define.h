/*  i486_linux  */

#define Standard
#define IconGcvt
#define index strchr
#define rindex strrchr
#define UtsName
#define Hz 100
#define MaxHdr 13400
#define MaxStatSize 20480

/*
 * Do not remove the following definition. It controls many
 * aspects of conditional assembly that are specific to UNIX
 * systems.
 */
#define UNIX 1
#define Linux 1

#define KeyboardFncs
#define HaveTermio
#define AllowConst
#define CComp "gcc"
#define COpts "-O2 -fomit-frame-pointer"
