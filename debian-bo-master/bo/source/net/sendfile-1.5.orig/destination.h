/*
 * File:	destination.h
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 *
 * Determine own username, recipient username and host and look
 * for an alias in /USERSPOOL/config/aliases and in ~/.elm/aliases.text
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */

void destination(int, char **, char *, char *, char *);
