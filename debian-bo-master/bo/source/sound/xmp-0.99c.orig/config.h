/* $Id: config.h,v 1.2 1997/03/13 13:31:23 claudio Exp $ */

/*  Endianism:
 *
 *  Define XMP_LITTLE_ENDIAN for little endian (i386) machines
 *  Note: This version runs only in i386 Linux!
 */

#if defined(PPC) || defined(sparc)
#undef 	XMP_LITTLE_ENDIAN
#else
#define	XMP_LITTLE_ENDIAN
#endif


/*  Output devices:
 *  
 *  GUS_DEVICE includes support for GUS or SoftOSS
 *  AWE_DEVICE includes support for AWE-32 (requires awedrv)
 */

#define USE_OSS
#define	GUS_DEVICE
#undef	AWE_DEVICE


/*  Supported module formats:
 *  
 *  Define LOAD_XM  for Fast Tracker II modules
 *  Define LOAD_S3M for Scream Tracker 3 modules
 *  Define LOAD_MOD for Protracker modules
 *  Define LOAD_STM for Scream Tracker 2 modules
 *  Define LOAD_669 for Composer 669 modules
 *  Define LOAD_MTM for Multitracker modules
 *  Define LOAD_PTM for Polytracker modules
 *  Define LOAD_OKT for Oktalyzer modules
 *  Define LOAD_FAR for Farandole composer modules (experimental)
 *  Define LOAD_IT  for Impulse tracker modules (incomplete)
 */

#define	LOAD_XM
#define	LOAD_S3M
#define	LOAD_MOD
#define	LOAD_STM
#define	LOAD_669
#define	LOAD_MTM
#define	LOAD_PTM
#define	LOAD_OKT
#define	LOAD_FAR
#undef	LOAD_IT


/*  Other options:
 *
 *  MATH_LOG causes xmp to use log() to convert C[2-5]SPD to note
 *	transpose/finetune values. Add -lm to the linker definition
 *	line in the Makefile if you define this.
 */

#undef	MATH_LOG


/* End of config.h */
