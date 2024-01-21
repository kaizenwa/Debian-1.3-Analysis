/*
 * Icon configuration file for Dec Alpha running OSF
 */

/* standard Unix and C */
#define UNIX 1
#define Standard
#define index strchr
#define rindex strrchr
#define UtsName

/* OSF */
#define KeyboardFncs
#define HaveTioc
#define NoRanlib
#define SysOpt
#define LoadFunc

/* CPU architecture */
#define IntBits 32
#define WordBits 64
#define Double
#define StackAlign 8
#define Hz 60

/* suppress "Unaligned access" messages for benefit of old executables */
#define SuppressAlignmentMsg
