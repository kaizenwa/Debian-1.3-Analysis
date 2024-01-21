
/*
 * BIG FAT WARNING:
 *
 * This file (this is acconfig.h, _NOT_ config.h.in) is the only place 
 * where you can add #defines. If you edit config.h.in, your efforts
 * will become useless the next time you run autoheader.
 *
 * All lines before the TOP macro or after the BOTTOM macro are copied
 * unchanged into config.h.in. Read the GNU autoconf documentation
 * to learn more about configure & friends.
 *
 */

#ifndef CONFIG_H
#define CONFIG_H 1

#define    CONFIG_FILE           "/etc/dosemu.conf"
#define    DOSEMU_USERS_FILE     "/etc/dosemu.users"
#define    DOSEMU_LOGLEVEL_FILE  "/etc/dosemu.loglevel"

/* Do not worry about these */
#define    LIBSTART  0x20000000
#define    lint      1

@TOP@

/* Define the host for which Dosemu is configured */
#undef CONFIG_HOST

/* Define the configure time */
#undef CONFIG_TIME

/* Define the Dosemu version */
#undef VERSION

/* Define the Dosemu version release date */
#undef VERDATE

/* Define the Dosemu sublevel */
#undef SUBLEVEL

/* Define the Dosemu patchlevel */
#undef PATCHLEVEL

/* Define the version string */
#undef VERSTR

/* Define the EMU version */
#undef EMUVER

/* 
 * Define if the compiler doesn't have __FUNCTION__ macro
 * This is very gcc specific (and very useful!). It is defined
 * as "" if the compiler doesn't have it.
 */
#undef __FUNCTION__

/* 
 * Define if the compiler doesn't have __FILE__ macro
 * Most compilers have this one, but just to be sure...
 */
#undef __FILE__

/* 
 * Define if the compiler doesn't have __LINE__ macro
 * Most compilers have this one, but just to be sure...
 */
#undef __LINE__

/* DEFINE this, if you want the extended vm86 support (vm86plus)
 * You then either have to patch your kernel or load emumodule
 */
#undef REQUIRES_VM86PLUS

/* DEFINE this, if you have vm86plus built into the kernel */
#undef BUILTIN_VM86PLUS

/* DPMI test windows */
#undef WANT_WINDOWS

/* DEFINE this, if you want the dosdebugger (mhpdbg) */
#undef USE_MHPDBG

/* Define these for X support */
#undef X_SUPPORT
#undef X2_SUPPORT
#undef X_GRAPHICS
/* Add if you have X-Keycode problems */
#undef NEW_KEYCODES

/* Define this to use the new keyboard code */
#undef NEW_KBD_CODE

/*
 *  DEFINE this, if you have joystick support in the kernel
 *  or have the joystick module loaded (needing <linux/joystick.h>)
 */
#undef USE_MRP_JOYSTICK

/*
 * This is DEFINED when SB emulation is required. Just changing this is not
 * enough - you must reconfigure.
 */
#undef USE_SBEMU

/* Define this if you want to use MITSHM */
#undef HAVE_MITSHM

/* Define this if you want to use a monoton micro timing (Bernd Paysan) */
#undef MONOTON_MICRO_TIMING

/* Define this if you want dosemu permanently run as (suid) root and only gain
 * user privileges, when accessing secure relevant resources.
 * Otherwise dosemu will permanently run as user and only temporary take over
 * root privilege when needed.
 */
#undef RUN_AS_ROOT

@BOTTOM@

#endif /* CONFIG_H */
