/*
 * msgid -- message ID test program
 * vix 13feb91 [negative caching]
 * vix 24may90 [written]
 * with mods ken@sdd.hp.com 01jul90
 *
 * $Header: msgid.c,v 1.6 91/08/17 12:04:11 vixie Locked $
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef hpux
#include <sys/param.h>
#include <libBSD.h>
#endif
#include <syslog.h>

#include "msgid.h"

#define SERVERTIMEOUT 30

char hostname[BUFSIZ];

main(argc, argv)
int	argc;
char	*argv[];
{
	register int n;
	char buf[BUFSIZ], cmd[20], id[BUFSIZ];

	if (gethostname(hostname, BUFSIZ)) {
		perror("hostname");
		exit(1);
	}
	(void) printf("host: %s\n", hostname);

	if (argc != 1) {
		(void) fprintf(stderr, "usage: %s\n", argv[0]);
		exit(1);
	}

#ifdef LOG_DAEMON
	openlog("msgid-test", LOG_PID, LOG_DAEMON);
#else
	openlog("msgid-test", LOG_PID);
#endif

	while (fputs("cmd msgid: ", stdout), fflush(stdout),
	     fgets(buf, BUFSIZ, stdin))
		if ((n = sscanf(buf, "%[^ \t]%*[ \t]%[^\n]", cmd, id)) == 2) {
			if (strcmp(cmd, "cancel") == 0)
				(void) printf("%s\n",
					(msgid(id, MCANCEL)? "failed": "ok"));
			else if (strcmp(cmd, "add") == 0)
				(void) printf("%d\n", msgid(id, MADD));
#ifdef notdef
				(void) printf("%sduplicate\n", 
					      (msgid(id, MADD)? "": "not a "));
#endif
			else if (strcmp(cmd, "old") == 0)
				(void) printf("%s\n",
					(msgid(id, MOLD)? "failed": "ok"));
			else
				(void) printf("possible cmds are cancel, add, and old\n");
		} else
			(void) printf("[%d] possible cmds are cancel, add, and old\n",
			     n);
}
