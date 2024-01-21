/*
 * mkdirs - make the directories implied by `name'
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/*
 * Given a/b/c/d, try to make any of a, a/b, a/b/c and a/b/c/d which are missing;
 * stop on first failure.
 * Returns success.
 */
int
mkdirs(name, uid, gid)
register char *name;
int uid, gid;
{
	register char *cp;
	register int isthere = 1;
	struct stat stbuf;

	for (cp = name; isthere && *cp != '\0'; cp++)
		if (*cp == '/') {
			*cp = '\0';
			isthere = stat(name, &stbuf) >= 0;
			if (!isthere) {
				isthere = mkdir(name, 0777) >= 0;
				(void) chown(name, uid, gid);
			}
			*cp = '/';
		}
	return isthere;
}
