/*
 * File:	net.h
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	12 Aug 95   Framstag	initial version
 * 		14 May 96   Framstag	included and modified send_data()
 *              24 Sep 96   Heiko	added get_domainname()
 *
 * Network routines header-file for the the sendfile client of the
 * sendfile package.
 *
 * Copyright © 1995,1996 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */

/* open socket and connect to client */
int open_connection(char *, int);

/* get a line from the network socket */
int sock_getline(int, char *);

/* send a line to the network socket */
int sock_putline(int, const char *);

/* get the reply on a command from the server */
char *getreply(int);

/* send a headerline and check the reply code */
int sendheader(int, char *);

/* send file data */
int send_data(int, unsigned long, const char*, const char*, int, int);

/* add domainname to host-string */
void get_domainname(char *);
