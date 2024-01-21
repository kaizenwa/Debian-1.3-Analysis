/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	%W% (Berkeley) %G%
 *
 * $Id: wire-test.c,v 5.2.2.2 1992/06/07 18:06:46 jsp Exp jsp $
 *
 */

#include "am.h"
#ifdef DEBUG
# ifdef HAS_SYSLOG
#  ifdef MACH3
#   include <sys/syslog.h>
#  else
#   include <syslog.h>
#  endif /* MACH3 */
# endif /* HAS_SYSLOG */
#endif /* DEBUG */

#define STRMAX 100

int mypid;
char *progname, hostname[MAXHOSTNAMELEN];
int orig_umask;

int main(argc, argv)
     int argc;
     char **argv;
{
  char *networkName1, *networkNumber1;
  char *networkName2, *networkNumber2;

  progname = argv[0];
  mypid = getpid();
  orig_umask = umask(0);

  if (gethostname(hostname, MAXHOSTNAMELEN) < 0) {
    perror(argv[0]);
    exit(1);
  }
  if ((networkName1 = (char *) calloc(STRMAX, sizeof(char)))
      == (char *) NULL) {
    perror(argv[0]);
    exit(1);
  }
  if ((networkNumber1 = (char *) calloc(STRMAX, sizeof(char)))
      == (char *) NULL) {
    perror(argv[0]);
    exit(1);
  }

  if ((networkName2 = (char *) calloc(STRMAX, sizeof(char)))
      == (char *) NULL) {
    perror(argv[0]);
    exit(1);
  }
  if ((networkNumber2 = (char *) calloc(STRMAX, sizeof(char)))
      == (char *) NULL) {
    perror(argv[0]);
    exit(1);
  }

  getwire(&networkName1, &networkNumber1, &networkName2, &networkNumber2);
  printf("Network name is \"%s\"\n", networkName1);
  printf("Network number is \"%s\"\n", networkNumber1);
  printf("Network name is \"%s\"\n", networkName2);
  printf("Network number is \"%s\"\n", networkNumber2);
  exit(0);
}

/****************************************************************************/
/* FUNCTIONS FROM OTHER AMD SOURCES (so this program would link.)	    */
/****************************************************************************/

void going_down(i)
     int i;
{
  exit(i);
}

/*
 * Make a dotted quad from a 32bit IP address
 * addr is in network byte order.
 * sizeof(buf) needs to be at least 16.
 */
char *inet_dquad P((char *buf, unsigned long addr));
char *inet_dquad(buf, addr)
char *buf;
unsigned long addr;
{
	addr = ntohl(addr);
	sprintf(buf, "%d.%d.%d.%d",
		((addr >> 24) & 0xff),
		((addr >> 16) & 0xff),
		((addr >> 8) & 0xff),
		((addr >> 0) & 0xff));
	return buf;
}

#ifndef strdup
/*
 * Dup a string
 */
char *strdup(s)
Const char *s;
{
	int len = strlen(s);
	char *sp = (char *) xmalloc(len+1);

	bcopy(s, sp, len);
	sp[len] = 0;

	return sp;
}
#endif /* !strdup */
/****************************************************************************/
/****************************************************************************/
