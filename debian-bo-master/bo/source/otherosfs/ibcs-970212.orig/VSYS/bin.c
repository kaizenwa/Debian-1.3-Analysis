#include <sys/types.h>
#include <pwd.h>
#include <string.h>
#include <unistd.h>


main(int argc, char *argv[])
{
	char buf[4096], *p;
	struct passwd *pw;

	/* Build the pathname of the virtual system root. */
	strcpy(buf, "/OS/");
	if (!(p = strrchr(argv[0], '/')))
		p = argv[0];
	else
		p++;
	strcat(buf, p);

	/* Change the root directory (but leave us where we were!) */
	if (chroot(buf) < 0) {
		perror(buf);
		exit(1);
	}

	/* Prep the environment for the virtual system. */
	if ((pw = getpwuid(getuid()))) {
		strcpy(buf, "HOME=");
		strcat(buf, pw->pw_dir);
		putenv(strdup(buf));
	}

	/* Drop root privileges. This program must be setuid root to
	 * do the chroot() but we don't want to execute the command
	 * specified by the user with any extra privileges!
	 */
	seteuid(getuid());
	setegid(getgid());

	/* Execute the given command. If the command name starts with
	 * a '-' treat it as a login shell - change directory to the
	 * user's home directory and strip the leading '-' from the
	 * command to get the filename to execute.
	 */
	p = argv[1];
	if (*p == '-') {
		p++;
		if (pw && pw->pw_dir)
			chdir(pw->pw_dir);
	}
	execvp(p, argv+1);
	perror(p);
	exit(1);
}
