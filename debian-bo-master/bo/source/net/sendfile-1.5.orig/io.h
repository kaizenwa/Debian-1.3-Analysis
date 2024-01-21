/*
 * File:	netio.h
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 * 		23 Apr 96   Framstag	added file copying function
 *
 * Header-file for the read and write routines of the sendfile package.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


/* read n bytes from network socket */
int readn(int, char *, int);

/* write n bytes to network socket */
int writen(int, char *, int);

/* copy a file */
int fcopy(const char *, const char *, mode_t);
