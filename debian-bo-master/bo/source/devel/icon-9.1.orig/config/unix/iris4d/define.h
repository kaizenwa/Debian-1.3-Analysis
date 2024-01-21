/*
 * Icon configuration file for the Iris 4d.
 */
#define IRIS4D
#define Hz 100
#define UtsName
#define MaxHdr 16384
#define CStateSize 32  /* anything >= 26 should actually do */
#define Double

#define NoRanlib
#ifdef NoCoexpr
#define MaxStatSize 9000
#endif				/* NoCoexpr */

#define COpts "-Wf,-XNd10000"

#define NoIconGcvt
#define StandardPP
#define VoidType
 
#define UNIX 1

#define GammaCorrection 1.0	/* WE don't need to; system does it */

#define LoadFunc
