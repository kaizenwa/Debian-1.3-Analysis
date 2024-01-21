/* 
 * forks into two - one to copy from fd1 to fd2, one to copy from fd2 to
 * fd1.  When EOF reached on fd1, wait for children to exit.  When EOF
 * reached on fd2, child exits, parent will catch SIGCHLD, zap child and
 * exit.
 */

#include <stdio.h>
#include <signal.h>

#define MAXTTYBUF 256

int
bicat(fd1, fd2)
register int fd1, fd2;
{
	int parent = getpid();
	int child = fork();

	if (child == -1)
		error("can't fork", "");
	if (child == 0) {		/* child process */
		(void) fcopy(fd1, fd2);
		sleep(3);		/* give other side time to settle */
		if (parent > 0)
			(void) kill(parent, SIGKILL);
		shutdown(fd1, 2);
		shutdown(fd2, 2);
		exit(0);
	}

	/* parent copies data from fd2 to fd1 */
	(void) fcopy(fd2, fd1);
	sleep(3);			/* give other side time to settle */
	if (child > 0)
		(void) kill(child, SIGKILL);
	return 0;
}

static int					/* success? */
fcopy(fd1, fd2)					/* copy fd1 to fd2 */
register int fd1, fd2;
{
	register int cc;
	char buf[MAXTTYBUF];

	while ((cc = read(fd1, buf, sizeof buf)) > 0)
		if (write(fd2, buf, cc) != cc)
			return 0;		/* error - bail out */
	return cc == 0;
}
