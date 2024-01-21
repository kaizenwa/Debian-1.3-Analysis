/* Version.h - Definition of af version and log record.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/****************************************************************************/
/* RCS info */

#define VERSIONID	"$Id: version.h,v 1.128 1997/05/06 16:16:26 malc Exp $"

/****************************************************************************/
/*
 *
 * $Log: version.h,v $
 * Revision 1.128  1997/05/06 16:16:26  malc
 * Fixed bad interaction causing smail to hang
 *
 * Revision 1.127  1997/05/05 02:50:01  malc
 * Several more minor bugs fixed
 *
 * Revision 1.126  1997/04/20 10:32:45  malc
 * Fixed problem with empty address groups
 * More corrections to time zone table
 * Fixed bug with redirected recursive typeout
 * Improved handling of "Re:" prefix
 *
 * Revision 1.125  1997/03/31 18:32:19  malc
 * Added UIDL support to POP3 code
 * Made lock files NFS-friendly
 * Fixed problem with folded subjects
 *
 * Revision 1.124  1997/03/06 01:30:52  malc
 * Fixed core dump when -EH options used
 *
 * Revision 1.123  1997/03/05 21:23:45  malc
 * Fixed serious bug in POP3 handling
 * Fixed major memory leaks
 * More minor bugfixes in search of stability
 *
 * Revision 1.122  1997/02/15 12:46:23  malc
 * Improved configuration script
 * More bugs fixed on the way to v2.0
 *
 * Revision 1.121  1997/02/02 17:54:28  malc
 * Fixed serious problem with POP3 buffers
 *
 * Revision 1.120  1997/01/29 18:33:12  malc
 * More bug-fixes as we approach v2.0
 *
 * Revision 1.119  1996/12/27 16:50:54  malc
 * Fixed bug in disaster-recovery code
 * Improved performance of regex code
 * Fixed minor glitch in keyboard macros
 * Several minor documentation improvements
 *
 * Revision 1.118  1996/10/17 18:53:02  malc
 * Another bug-fix release on the way to v2.0
 *
 * Revision 1.117  1996/10/06 17:35:48  malc
 * More bug-fixes as we move towards version 2.0
 *
 * Revision 1.116  1996/09/22 16:40:17  malc
 * Bug-fix release as we move towards 2.0
 *
 * Revision 1.115  1996/08/28 17:44:08  malc
 * New POSIX regular expression library added
 * Many code cleanups and minor bug fixes
 *
 * Revision 1.114  1996/05/06 10:11:30  malc
 * New -F command line option added
 * Several bugs of varying seriousness fixed
 *
 * Revision 1.113  1996/03/31 17:00:58  malc
 * Fixed quoting of backslashes in afl read syntax
 * Fixed problems with addresses-to-ignore variable
 *
 * Revision 1.112  1996/03/23 16:06:00  malc
 * Option to spell-check outgoing mail added
 *
 * Revision 1.111  1996/03/22 18:57:15  malc
 * Copying terms of manual modified
 * Minor portability problems fixed
 *
 * Revision 1.110  1996/03/17 01:10:47  malc
 * Major code rewrites and bug fixes
 * Many features cleaned up or enhanced
 * Texinfo manual added to distribution
 * Beta release for final testing
 *
 * Revision 1.109  1996/01/05 19:41:28  malc
 * Fixed configuration error affecting OSF/1
 * Fixed problems with POP3 network code
 *
 * Revision 1.108  1995/12/23 10:55:06  malc
 * Fixed several minor display glitches
 *
 * Revision 1.107  1995/12/20 18:36:45  malc
 * Changed POP3 handling to keep connections open
 *
 * Revision 1.106  1995/12/14 19:59:45  malc
 * Fixed several minor bugs
 *
 * Revision 1.105  1995/12/09 19:29:58  malc
 * POP3 and SMTP support added
 * Many small improvements made
 *
 * Revision 1.104  1995/10/02 21:23:37  malc
 * Source files replaced with afl reader
 * Assorted other fixes and enhancements
 *
 * Revision 1.103  1995/04/16  00:29:18  malc
 * Fixed problems in tag-thread command
 * Reordered entries in sort type completion
 *
 * Revision 1.102  1995/04/08  22:51:53  malc
 * Fixed minor bug in symbolic link handling
 *
 * Revision 1.101  1995/04/07  09:01:56  malc
 * Added sleep function to libaf
 * Removed function af_sleep
 *
 * Revision 1.100  1995/04/05  17:33:06  malc
 * Fixed trivial problem in header autofolding
 *
 * Revision 1.99  1995/04/05  17:16:10  malc
 * Fixed handling of edit-initial-headers
 *
 * Revision 1.98  1995/04/04  19:39:48  malc
 * Improved parsing of References in headers
 *
 * Revision 1.97  1995/03/28  07:25:53  malc
 * Fixed problem on systems where sleep is implemented using signals
 *
 * Revision 1.96  1995/03/25  23:47:11  malc
 * Fixed problem displaying metachars in the minibuffer
 *
 * Revision 1.95  1995/03/25  22:09:56  malc
 * Fixed minor bugs in list-bindings and typeout searches
 *
 * Revision 1.94  1995/03/12  19:23:47  malc
 * Tag-thread command added
 *
 * Revision 1.93  1995/03/06  21:57:33  malc
 * Fixed handling of quoted Content-Type parameters
 *
 * Revision 1.92  1995/02/10  19:40:31  malc
 * Keyboard macros implemented
 * More problems solved
 *
 * Revision 1.91  1995/02/08  10:20:50  malc
 * Updated configure to handle ospeed properly
 *
 * Revision 1.90  1995/01/29  01:17:46  hawk
 * Fixed bug in strict and cautious completion
 *
 * Revision 1.89  1995/01/27  20:47:50  hawk
 * Fixed minor problem in checking for MIME messages
 *
 * Revision 1.88  1995/01/20  22:40:42  hawk
 * Fixed minor bug in writing pending folders
 *
 * Revision 1.87  1995/01/18  15:16:55  hawk
 * Fixed several problems with filename completion
 *
 * Revision 1.86  1995/01/10  22:54:35  hawk
 * Typeout integrated and improved
 * Several problems fixed
 *
 * Revision 1.85  1994/12/07  22:58:08  hawk
 * Fixed bug when setting format variables
 *
 * Revision 1.84  1994/12/01  18:59:04  hawk
 * Fixed trivial bug causing core dump
 *
 * Revision 1.83  1994/11/29  22:12:40  hawk
 * Several serious bugs fixed
 *
 * Revision 1.82  1994/11/27  16:12:45  hawk
 * Memory problem when sending mail fixed
 * Several other bugs also fixed
 *
 * Revision 1.81  1994/11/23  23:08:07  hawk
 * Mail mode and keymaps added
 * Several minor commands added
 * Several features enhanced
 * Several bugs fixed
 *
 * Revision 1.80  1994/11/15  18:45:04  hawk
 * New -w command-line option implemented
 * Several configuration variables added
 * Interface improved and made more emacs-like
 * Several minor bugs fixed
 *
 * Revision 1.79  1994/11/01  20:02:21  hawk
 * Added preserve-cc-on-group-reply variable
 * MMDF support fixed
 * Added MIME support via metamail
 * Several minor bugs fixed
 *
 * Revision 1.78  1993/09/04  13:15:12  hawk
 * Argument handling implemented
 * Second beta-test distribution
 *
 * Revision 1.77  1993/01/27  11:30:46  hawk
 * Explode-digest command implemented
 * Several configuration variables added
 *
 * Revision 1.76  1992/12/22  10:00:59  hawk
 * Several enhancements added
 * A few minor bugs fixed
 *
 * Revision 1.75  1992/12/19  00:52:04  hawk
 * Sorting commands implemented
 *
 * Revision 1.74  1992/12/13  21:30:14  hawk
 * Searching commands implemented
 *
 * Revision 1.73  1992/12/09  19:36:49  hawk
 * Tag handling implemented
 *
 * Revision 1.72  1992/11/13  20:09:27  hawk
 * Mark and region handling implemented
 * Narrowing and kill ring added
 *
 * Revision 1.71  1992/11/12  10:12:10  hawk
 * Name changed to af
 * New directory tree used
 * Many minor improvements and bug fixes
 * Configuration by configure script added
 * Updated to GPL version 2.0
 *
 * Baseline for this release
 *
 */

/****************************************************************************/
/* Af definition values */

#define PROGNAME	"Af"
#define VERSION		"1.99.18 Beta"
#define RELEASE_DATE	"6 May 1997"
#define AUTHOR		"Malc Arnold"

/****************************************************************************/
