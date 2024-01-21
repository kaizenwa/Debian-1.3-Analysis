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
 */

#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
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
  env[i++] = "PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/etc:/usr/etc";
  env[i++] = lookup("TERM") ? lookup("TERM") : "TERM=dumb";

  /* Import some values from the old environment. */
  if (getenv("HOME")) env[i++] = lookup("HOME");
  if (getenv("LOGNAME")) env[i++] = lookup("LOGNAME");

  env[i++] = NULL;
  environ = env;
}


int main(int argc, char **argv)
{
  char *cmd;
  char procbuf[64];
  char buf[2];
  struct stat st;
  int fd;

  /* Check syntax. */
  if (argc < 3) {
	fprintf(stderr, "Syntax error while executing %s\n", argv[0]);
	fprintf(stderr, "Usage: #! %s <shell>\n", argv[0]);
	exit(1);
  }
  cmd = argv[1];

  /* Now, open the script file. */
  if ((fd = open(argv[2], O_RDONLY)) < 0) {
	perror(argv[2]);
	exit(1);
  }

  /* Check if /proc/self/fd/<fd> is there. */
  sprintf(procbuf, "/proc/self/fd/%d", fd);
  if (stat(procbuf, &st) < 0) {
	fprintf(stderr, "%s: cannot stat %s. /proc not mounted?\n",
		argv[2], procbuf);
	exit(1);
  }

  /* Check permissions on script. */
  (void) fstat(fd, &st);
  if (!S_ISREG(st.st_mode)) {
	fprintf(stderr, "%s: not a regular file\n", argv[2]);
	exit(1);
  }
  if ((st.st_mode & (S_IWGRP | S_IWOTH)) != 0) {
	fprintf(stderr, "%s: script is writable - security breach.\n", argv[2]);
	exit(1);
  }

  /* See if this really is a shell script. */
  read(fd, buf, 2);
  if (strncmp(buf, "#!", 2) != 0) {
	fprintf(stderr, "%s: not a shell script.\n", argv[2]);
	exit(1);
  }
  lseek(fd, 0L, SEEK_SET);

  /* Check setuid / setgid bit. */
  if ((st.st_mode & S_ISGID) == S_ISGID) setegid(st.st_gid);
  if ((st.st_mode & S_ISUID) == S_ISUID)
	seteuid(st.st_uid);
  else
	setuid(getuid());

  /* Reset environment. */
  set_environ();

  /* And execute the command interpreter. */
  argv++;
  argv[0] = argv[1];  /* Name of the script. */
  argv[1] = procbuf;  /* File descriptor of script. */
  execv(cmd, argv);

  /* Failed - so complain. */
  fprintf(stderr, "%s: ", argv[0]);
  perror(cmd);
  exit(1);
}

