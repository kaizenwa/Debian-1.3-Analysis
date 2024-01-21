/*
 * version.h: MAKEDEV-C version and version history
 *
 * Written 10-Dec-94 by David A. Holland, dholland@husc.harvard.edu
 *
 * Copyright 1994, 1995. All rights reserved. 
 * See the file LEGAL.NOTICE for conditions of redistribution.
 *
 * Known bugs, in no particular order:
 *    Problem with batches with hdc-d (see devinfo)
 *    Should issue warning if current dir is not /dev
 *    Should print more intelligent message if something isn't listed in
 *    /proc/devices.
 *    Should print more intelligent message if you try to make something 
 *    listed in omits.
 *    Should be able to omit ranges.
 *
 * History of MAKEDEV:
 *
 * Version 1.5:   25-Mar-95  Fixed makefile. 
 *                           Look for config files in ".." if TESTING.
 *                           Big source split: 
 *                             makedev.syn -> parser.syn and devices.c.
 *                           This file was created to hold the overall
 *                           version history (and bug list).
 *                           More fixes to devinfo.
 *
 * Version 1.4b:  25-Mar-95  Merged Rik's changes. 
 *                           Additional bug fixes:
 *                             Don't leave off the last entry in a range.
 *                             Parse hex digits correctly [sigh...].
 *                             Now we actually *use* the ishex flag.
 *                           This version not released.
 *
 * Version 1.4a: 26-Feb-95   Forced devinfo and makedev.cfg to be in /etc.
 *                           (Actually a variant of 1.4.1)
 *                           [from faith@cs.unc.edu]
 *                           This version shipped with util-linux 2.2.
 *
 * Version 1.4.1: 14-Feb-95  Bug fixes to DEVINFO.
 * Version 1.4:   15-Jan-95  Wrote man pages. Now reads DEVINFO.local.
 * Version 1.3:   31-Dec-94  Bug fixes. Added batches. Added omits.
 * Version 1.2:   11-Dec-94  Add configuration file parsing.
 * Version 1.1:   11-Dec-94  Distinguish block and character devices in the
 *                           table of major device numbers. Changed the name 
 *                           and format of the update cache file to include 
 *                           the type. It appears that the old script was 
 *                           broken in this regard.
 *
 * Version 1.0:   10-Dec-94  Initial version.
 *
 */

static const char *version = "MAKEDEV-C version 1.5";
#ifdef UTIL_LINUX
#define MYNAME "MAKEDEV-C"
#else
#define MYNAME "MAKEDEV"
#endif

