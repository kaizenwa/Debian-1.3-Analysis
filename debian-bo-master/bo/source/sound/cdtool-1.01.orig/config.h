/* "config.h" copyright 1994 thomas insel */

/* IF NEEDED, UNCOMMENT THE APPROPRIATE LINE FOR YOUR CD-ROM */

/* #define CD_DEVICE "/dev/sonycd" /* Sony */
/* #define CD_DEVICE "/dev/sr0"    /* SCSI */
/* #define CD_DEVICE "/dev/mcd"    /* Mitsumi */
/* #define CD_DEVICE "/dev/sbpcd"  /* SoundBlaster Pro */

/* otherwise, assume a sensible default */
#ifndef CD_DEVICE
#  ifdef linux
#    define CD_DEVICE "/dev/cdrom"
#  elif defined sun
#    define CD_DEVICE "/dev/sr0"
#  else
#    error Please define CD_DEVICE in config.h
#  endif
#endif

/* version string printed with usage message */
#define VERSION_STRING "CDTOOL 1.0 copyright 1994 Thomas Insel"
