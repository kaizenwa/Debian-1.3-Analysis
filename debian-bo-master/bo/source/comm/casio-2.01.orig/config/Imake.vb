/*
 * To add support for another platform:
 * 
 *     1.  Identify a machine-specific cpp symbol.  If your preprocessor 
 *         does not have any built in, you will need to add the symbol to the
 *         cpp_argv table in config/imakemdep.h and rebuild imake with the
 *         BOOTSTRAPCFLAGS variable set (see the macII for an example).
 *
 *     2.  Add all machine-specific cpp symbols (either defined by you or by
 *         the preprocessor or compiler) to the predefs table in 
 *         config/imakemdep.h.
 *
 *     3.  But a new #ifdef block below that defines MacroIncludeFile and
 *         MacroFile for your new platform and then #undefs the machine-
 *         specific preprocessor symbols (to avoid problems with file names).
 *
 *     4.  Create a .cf file with the name given by MacroFile.
 */

#ifdef ultrix
#define MacroIncludeFile <ultrix.cf>
#define MacroFile ultrix.cf
#ifdef vax
#undef vax
#define VaxArchitecture
#endif
#ifdef mips
#undef mips
#define MipsArchitecture
#endif
#undef ultrix
#define UltrixArchitecture
#endif

#if defined(vax) && !defined(UltrixArchitecture)
#define MacroIncludeFile <bsd.cf>
#define MacroFile bsd.cf
#undef vax
#define VaxArchitecture
#endif

#ifdef sun
#define MacroIncludeFile <sun.cf>
#define MacroFile sun.cf
#undef sun
#define SunArchitecture
#endif /* sun */

#ifdef hpux
#define MacroIncludeFile <hp.cf>
#define MacroFile hp.cf
#undef hpux
#define HPArchitecture
#endif /* hpux */

#ifdef att
#define MacroIncludeFile <att.cf>
#define MacroFile att.cf
#undef att
#define ATTArchitecture
#endif /* att */

#ifdef apollo
#define MacroIncludeFile <apollo.cf>
#define MacroFile apollo.cf
#undef apollo
#define ApolloArchitecture
#endif /* apollo */

#ifdef sony
#define MacroIncludeFile <sony.cf>
#define MacroFile sony.cf
#undef sony
#undef sony_news
#define SonyArchitecture
#ifdef mc68020
#undef mc68020
#undef mc68030
#define Mc68020Architecture
#endif
#ifdef mips
#undef mips
#define MipsArchitecture
#endif
#if !defined(bsd43) || defined(SYSTYPE_SYSV)
#define SonySysvArchitecture
#else
#define SonyBsdArchitecture
#endif
#endif /* sony */

#ifdef M4310
#define MacroIncludeFile <pegasus.cf>
#define MacroFile pegasus.cf
#undef M4310
#define PegasusArchitecture
#endif /* M4310 */

#ifdef M4330
#define MacroIncludeFile <m4330.cf>
#define MacroFile m4330.cf
#undef  M4330
#define M4330Architecture
#endif /* M4330 */

#ifdef macII
/* A/UX cpp has no unique symbol:  build imake with BOOTSTRAPCFLAGS=-DmacII */
#define MacroIncludeFile <macII.cf>
#define MacroFile macII.cf
#undef  macII
#define MacIIArchitecture
#endif /* macII */

#ifdef CRAY
#define MacroIncludeFile <cray.cf>
#define MacroFile cray.cf
#undef cray
#define CrayArchitecture
#endif /* CRAY */

#ifdef sgi
#define MacroIncludeFile <sgi.cf>
#define MacroFile sgi.cf
#undef sgi
#define SGIArchitecture
#undef mips
#define MipsArchitecture
#endif

#ifdef stellar
#define MacroIncludeFile <stellar.cf>
#define MacroFile stellar.cf
#undef stellar
#define StellarArchitecture
#endif

#if defined(ibm) || defined(_IBMR2) || defined(ibm032) || defined(aix)
#define MacroIncludeFile <ibm.cf>
#define MacroFile ibm.cf
#ifdef ibm
#undef ibm
#endif
#define IBMArchitecture
#ifdef i386
#undef i386
#define PS2Architecture
#endif
#ifdef ibm032
#undef ibm032
#define RtArchitecture
#endif
#ifdef aix
#undef aix
#define AIXArchitecture
#endif
#ifdef _IBMR2
#undef _IBMR2
#define RsArchitecture
#endif
#endif /* ibm */

#ifdef luna
#undef luna
#define MacroIncludeFile <luna.cf>
#define MacroFile luna.cf
#ifdef mc68000
#undef mc68000
#define Mc68000Architecture
#else
#undef mc88000
#define Mc88000Architecture
#endif
#endif

#ifdef Mips
#  define MacroIncludeFile "Mips.cf"
#  define MacroFile Mips.cf
#  undef Mips
#  if defined(SYSTYPE_BSD43) || defined(BSD) || defined(BSD43)
#    define MipsBsdArchitecture
#  else /* BSD */
#    define MipsSysvArchitecture
#  endif /* BSD */
#endif /* Mips */

#ifdef MOTOROLA
# define MacroIncludeFile <moto.cf>
# define MacroFile moto.cf
# undef MOTOROLA	
# ifdef SYSV
#  define MotoR3Architecture
# endif
# ifdef SVR4
#  define MotoR4Architecture
# endif
#endif /* MOTOROLA */

#ifdef SYSV386
# define MacroIncludeFile <x386.cf>
# define MacroFile x386.cf
# ifdef SVR4
#  define i386SVR4Architecture
# else
#  define i386SVR3Architecture
# endif
#endif /* SYSV386 */

#ifdef DGUX
#define MacroIncludeFile <DGUX.cf>
#define MacroFile DGUX.cf
#undef DGUX
#define DguxArchitecture
#endif

#ifdef linux
#define MacroIncludeFile <linux.cf>
#define MacroFile linux.cf
#undef linux
#define LinuxArchitecture
#define i386Architecture
#endif /* linux */



#ifdef __convex__
# define MacroIncludeFile <convex.cf>
# define MacroFile convex.cf
# ifdef convex
#  undef convex
# endif
# define ConvexArchitecture
#endif

#ifndef MacroIncludeFile
XCOMM WARNING:  Imake.tmpl not configured; guessing at definitions!!!
XCOMM This might mean that BOOTSTRAPCFLAGS was not set when building imake.
#define MacroIncludeFile <generic.cf>
#define MacroFile generic.cf
#endif
