/*
   Time-stamp: <96/07/19 20:17:01 yusuf>

   $Id: ttypes.h,v 1.4 1996/07/27 20:42:20 yusuf Exp $
*/


/* Taper types */

typedef signed int _s32;			 
typedef unsigned int _u32;
typedef unsigned short _u16;
typedef signed short _s16;
typedef signed char _s8;
typedef unsigned char _u8;
typedef void * _vptr;

/* The types under Linux are:
 * These are now hard coded. There may be a problem if using a
 *   different machine where the formats are different. There will
 *   be no problem restoring to/from the tape, but the values may
 *   be meaningless. In a future version, I will store time as a
 *   day/month/year, modes as x/x/x etc.. to overcome this.
 *
 *    long _time_t
 *    unsigned short _gid_t
 *    unsigned short _uid_t 
 *    unsigned short _dev_t
 *    unsigned short _umode_t
 *    int pid_t
*/

typedef long _time_t;
typedef _u16 _gid_t;
typedef _u16 _uid_t;
typedef _u16 _dev_t;
typedef _u16 _umode_t;

typedef _s8 _errstat;
