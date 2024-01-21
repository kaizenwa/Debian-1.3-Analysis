#include <unistd.h>
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <time.h>
#include <stddef.h>
#include <errno.h>

#ifndef S_ISLNK
#define S_ISLNK(mode) (((mode) & (_S_IFMT)) == (_S_IFLNK))
#endif

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

#define progver "%s: scan/change symbolic links - V1.0 - by Mark Lord\n\n"
static char *progname;
static int verbose = 0, fix_links = 0, recurse = 0, delete = 0;

/*
 * tidypath removes excess slashes and "." references from a path string
 */
int tidy_path (char *path)
{
	int tidied = 0;
	char *s, *p;

	strcat(path,"/");	/* tmp trailing slash simplifies things */
	while (NULL != (p = strstr(path,"/./"))) {
		s = p+2;
		if (p == path)
			++p;
		while ((*p++ = *s++));
		tidied = 1;
	}
	while (NULL != (p = strstr(path,"/../"))) {
		for (s = p+3; p != path && *--p != '/';);
		if (*p != '/')
			break;
		if (p == path) 
			++p;
		while ((*p++ = *s++));
		tidied = 1;
	}
	while (NULL != (p = strstr(path,"//")) && *(p+2)) {
		while ((*p = *(p+1))) ++p;
		tidied = 1;
	}
	p = path + strlen(path);
	if (*(p-1) == '/')
		*--p = '\0';	/* remove tmp trailing slash */
	while (--p != path && *p == '/') {	/* remove any others */
		*p = '\0';
		tidied = 1;
	}
	if (!strncmp(path,"./",2)) {
		for (p = path, s = path+2; (*p++ = *s++););
		tidied = 1;
	}
	return tidied;
}

void fix_symlink (char *path, dev_t my_dev)
{
	static char lpath[PATH_MAX], new[PATH_MAX];
	char *p, *np, *lp, *tail, *msg;
	struct stat stbuf;
	int c, fix_abs = 0, fix_messy = 0;

	if ((c = readlink(path, lpath, sizeof(lpath))) == -1) {
		perror(path);
		return;
	}
	lpath[c] = '\0';	/* readlink does not null terminate it */

	/* construct the absolute address of the link in new */
	new[0] = '\0';
	if (lpath[0] != '/') {
		strcat(new,path);
		if (((c = strlen(new)) > 0) && (new[c-1] == '/'))
			new[c-1] = '\0';
		if ((p = strrchr(new,'/')) != NULL)
			*p = '\0';
		strcat(new,"/");
	}
	strcat(new,lpath);

	/* check for various things */
	if (stat(new, &stbuf) == -1) {
		printf("dangling: %s -> %s\n", path, lpath);
		if (delete) {
			if (unlink (path))
				perror(path); 
			else
				printf("deleted:  %s -> %s\n", path, lpath);
		}
		return;
	}
	if (stbuf.st_dev != my_dev) {
		msg = "other_fs:";
	} else if (lpath[0] == '/') {
		msg = "absolute:";
		fix_abs = 1;
	} else if (verbose) {
		msg = "relative:";
	} else
		msg = NULL;
	fix_messy = tidy_path(strcpy(new,lpath));
	if (fix_messy && !fix_abs)
		msg = "messy:   ";
	if (msg != NULL)
		printf("%s %s -> %s\n", msg, path, lpath);
	if (!fix_links || !(fix_messy || fix_abs))
		return;

	if (fix_abs) {
		/* convert an absolute link to relative: */
		/* point tail at first part of lpath that differs from path */
		/* point p    at first part of path  that differs from lpath */
		(void) tidy_path(lpath);
		tail = lp = lpath;
		p = path;
		while (*p && (*p == *lp)) {
			if (*lp++ == '/') {
				tail = lp;
				while (*++p == '/');
			}
		}

		/* now create new, with "../"s followed by tail */
		np = new;
		while (*p) {
			if (*p++ == '/') {
				*np++ = '.';
				*np++ = '.';
				*np++ = '/';
				while (*p == '/') ++p;
			}
		}
		strcpy (np, tail);
		(void) tidy_path(new);
	}
	if (unlink (path)) {
		perror(path);
		return;
	}
	if (symlink(new, path)) {
		perror(path);
		return;
	}
	printf("changed:  %s -> %s\n", path, new);
}

void dirwalk (char *path, int pathlen, dev_t dev)
{
 	char *name;
	DIR *dfd;
	static struct stat st;
	static struct direct *dp;

	if ((dfd = opendir(path)) == NULL) {
		perror(path);
		return;
	}

	name = path + pathlen;
	if (*(name-1) != '/')
		*name++ = '/'; 

	while ((dp = readdir(dfd)) != NULL ) {
		strcpy(name, dp->d_name);
                if (strcmp(name, ".") && strcmp(name,"..")) {
			if (lstat(path, &st) == -1) {
				perror(path);
			} else if (st.st_dev == dev) {
				if (S_ISLNK(st.st_mode)) {
					fix_symlink (path, dev);
				} else if (recurse && S_ISDIR(st.st_mode)) {
					dirwalk(path, strlen(path), dev);
				}
			}
		}
	} 
	closedir(dfd);
	path[pathlen] = '\0';
}

void usage_error (void)
{
	fprintf(stderr, progver, progname);
	fprintf(stderr, "Usage:\t%s [-crv] dirlist\n\n", progname);
	fprintf(stderr, "Flags:\t-c == change absolute/messy links to relative\n\t-d == delete dangling links\n\t-r == recurse into subdirs\n\t-v == verbose (show all symlinks)\n\n");
	exit(1);
}

void main(int argc, char **argv)
{
	static char path[PATH_MAX+2], cwd[PATH_MAX+2];
	int dircount = 0;
	char c, *p;

	if  ((progname = (char *) strrchr(*argv, '/')) == NULL)
                progname = *argv;
        else
                progname++;

	if (NULL == getcwd(cwd,PATH_MAX) || *cwd == '\0') {
		fprintf(stderr,"getcwd() failed\n");
		exit (1);
	}
	if (cwd[strlen(cwd)-1] != '/')
		strcat(cwd,"/");

	while (--argc) {
		p = *++argv;
		if (*p == '-') {
			if (*++p == '\0')
				usage_error();
			while ((c = *p++)) {
				     if (c == 'v')	verbose   = 1;
				else if (c == 'c')	fix_links = 1;
				else if (c == 'r')	recurse   = 1;
				else if (c == 'd')	delete    = 1;
				else			usage_error();
			}
		} else {
			struct stat st;
			if (*p == '/')
				*path = '\0';
			else
				strcpy(path,cwd);
			tidy_path(strcat(path, p));
			if (lstat(path, &st) == -1)
				perror(path);
			else
				dirwalk(path, strlen(path), st.st_dev);
			++dircount;
		}
	}
	if (dircount == 0)
		usage_error();
	exit (0);
}
