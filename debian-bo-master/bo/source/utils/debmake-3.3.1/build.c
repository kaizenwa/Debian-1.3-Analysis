/*
 * suidexec   Execute setuid shell scripts. This program has to be
 *            the first line of a shell script like this:
 *
 *            #!/sbin/suidexec /bin/sh
 *            id
 *            exit 0
 *
 *            On startup, it will open() the shell script, check
 *            that it's not a symbolic link and a shell script
 *            indeed and then sets it's uid and gid the same as
 *            the modes on the shell script. It then starts the
 *            command line interpreter with the file descriptor file
 *            in the /proc file system as it's first argument.
 *            There are no races - if someone replaces the script
 *            "in between", the new script will have no setuid
 *            bits and will not be executed setuid.
 *
 *            The environment is reset to just PATH, TERM, HOME
 *            and LOGNAME, so fiddling with IFS and ENV etc. is
 *            impossible.
 *
 * Version:   @(#) suidexec  1.00  12-Jan-1995  MvS
 *
 * Author:    Miquel van Smoorenburg, <miquels@cistron.nl>
 * 
 * Modified to be the build wrapper for debmake by
 *            Christoph Lameter <clameter@debian.org>
 *
 */

#define _GNU_SOURCE

#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Get environment variable. */
char *lookup(char *var)
{
  int i;
  int len = strlen(var);
  char *p;

  for(i = 0; (p = environ[i]); i++) {
	if (strncmp(var, p, len) == 0 && p[len] == '=')
		return(p);
  }
  return(NULL);
}

/* Reset the environment to "safe" default values. */
void set_environ()
{
  static char *env[16];
  int i = 0;

  /* Set default values. */
  env[i++] = "PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/bin/X11";
  env[i++] = lookup("TERM") ? lookup("TERM") : "TERM=dumb";

  /* Import some values from the old environment. */
  if (getenv("HOME")) env[i++] = lookup("HOME");
  if (getenv("LOGNAME")) env[i++] = lookup("LOGNAME");
  if (getenv("PGPPASS")) env[i++] = lookup("PGPPASS");

  env[i++] = NULL;
  environ = env;
}


int main(int argc, char **argv)
{
  setuid(0);
  setgid(0);

  /* Reset environment. */
  set_environ();

  if (argc==1 || argv[1][0]=='-') {
    argv[0]="dpkg-buildpackage";
    return execvp("dpkg-buildpackage",argv);
  } else
  if (strcmp(argv[1],"binary")==0) return execl("debian/rules","rules","binary",NULL);
  if (strcmp(argv[1],"clean")==0) return execl("debian/rules","rules","clean",NULL);
  if (strcmp(argv[1],"binary-indep")==0) return execl("debian/rules","rules","binary-indep",NULL);
  if (strcmp(argv[1],"binary-arch")==0) return execl("debian/rules","rules","binary-arch",NULL);
  
  /* Failed - so complain. */
  fprintf(stderr, "Usage: build [binary|clean]\n");
  exit(1);
}
