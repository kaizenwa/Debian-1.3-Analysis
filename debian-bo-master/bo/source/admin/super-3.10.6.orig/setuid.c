#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <pwd.h>

/* setuid --
 *	changes uid, then executes command.

 * Use:
 *	setuid uid|username command [args...]

 * Unlike su, doesn't ever ask for password when executed with effective
 * uid=root.  In contrast, some OS's (e.g. SunOS 4.1.x) seems to prompt
 * for the root password if your effective uid, but not real uid, is root.

 * Will Deich
 * will@astro.caltech.edu
 * Mar 1992
 */

char *prog;

int
main(argc, argv)
int argc;
char **argv;
{
    int uid;
    char *s;
    struct passwd *pw = NULL;
    int atoi(), setuid(), execvp();

    prog = (s = strrchr(argv[0], '/')) ? s+1 : argv[0];

    if (argc <= 2) {
	fprintf(stderr,"Use: %s uid|username command [args...]\n", prog);
	fprintf(stderr,"Purpose: changes uid, then executes command.\n");
	fprintf(stderr,"Unlike su(1):\n");
	fprintf(stderr, "	* won't ever ask for password\n");
	fprintf(stderr,"	* args are given to execvp(), not to a shell\n");
	(void) exit(1);
    }

    if (isdigit(*argv[1])) {
	uid = atoi(argv[1]);
    } else {
	pw = getpwnam(argv[1]);
	if (pw == (struct passwd *) NULL) {
	    (void) fprintf(stderr, "%s: can't find uid for user `%s'\n",
		prog, argv[1]);
	    (void) exit(1);
	}
	uid = pw->pw_uid;
    }

    if (setuid(uid)!= 0) {
	if (pw)
	    fprintf(stderr, "%s: setuid(user=%s) failed: ", prog, argv[1]);
	else
	    fprintf(stderr, "%s: setuid(uid=%d) failed: ", prog, uid);
	perror("");
	(void) exit(1);
    }
    (void) exit(execvp(argv[2], &argv[2]));
}
