#include "internal.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <termios.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/wait.h>
#include <string.h>
#include <sys/mount.h>
#include <linux/fs.h>

const char		init_usage[] = "Used internally by the system.";
const char		console[] = "/dev/console";
const char *	first_terminal = "/dev/tty1";
const char *	second_terminal = "/dev/tty2";
const char		log[] = "/dev/tty3";

static void
message(const char * terminal, const char * pattern, ...)
{
	int	fd;
	FILE *	con = 0;
	va_list	arguments;

	/*
	 * Open the console device each time a message is printed. If the user
	 * has switched consoles, the open will get the new console. If we kept
	 * the console open, we'd always print to the same one.
	 */
	if ( ((fd = open(terminal, O_WRONLY|O_NOCTTY)) < 0)
	 ||  ((con = fdopen(fd, "w")) == NULL) )
		return;

	va_start(arguments, pattern);
	vfprintf(con, pattern, arguments);
	va_end(arguments);
	fclose(con);
}

static int
waitfor(int pid)
{
	int	status;
	int	wpid;
	
	message(log, "Waiting for process %d.\n", pid);
	while ( (wpid = wait(&status)) != pid ) {
		if ( wpid > 0 ) {
			message(
			 log
			,"pid %d exited, status=%x.\n"
			,wpid
			,status);
		}
	}
	return wpid;
}

static int
run(
 const char *			program
,const char * const *	arguments
,const char *			terminal
,int					get_enter
,const char * add_environment)
{
	static const char	control_characters[] = {
		'\003',
		'\034',
		'\177',
		'\025',
		'\004',
		'\0',
		'\1',
		'\0',
		'\021',
		'\023',
		'\032',
		'\0',
		'\022',
		'\017',
		'\027',
		'\026',
		'\0'
	};

	static char * environment[] = {
		"HOME=/",
		"PATH=/bin:/sbin:/usr/bin:/usr/sbin",
		"SHELL=/bin/sh",
		"TERM=linux",
		"USER=root",
		0,
		0
	};

	static const char	press_enter[] =
	 "Please press Enter to activate this console. ";

	int	pid;

	environment[5]=add_environment;

	pid = fork();
	if ( pid == 0 ) {
		struct termios	t;
		const char * const * arg;

		close(0);
		close(1);
		close(2);
		setsid();

		open(terminal, O_RDWR);
		dup(0);
		dup(0);
		tcsetpgrp(0, getpgrp());

		tcgetattr(0, &t);
		memcpy(t.c_cc, control_characters, sizeof(control_characters));
		t.c_line = 0;
		t.c_iflag = ICRNL|IXON|IXOFF;
		t.c_oflag = OPOST|ONLCR;
		t.c_lflag = ISIG|ICANON|ECHO|ECHOE|ECHOK|ECHOCTL|ECHOKE|IEXTEN;
		tcsetattr(0, TCSANOW, &t);

		if ( get_enter ) {
			/*
			 * Save memory by not exec-ing anything large (like a shell)
			 * before the user wants it. This is critical if swap is not
			 * enabled and the system has low memory. Generally this will
			 * be run on the second virtual console, and the first will
			 * be allowed to start a shell or the installation system.
			 */
			char	c;
			write(1, press_enter, sizeof(press_enter) - 1);
			read(0, &c, 1);
		}
		
		message(log, "Executing ");
		arg = arguments;
		while ( *arg != 0 )
			message(log, "%s ", *arg++);
		message(log, "\n");

		execve(program, (char * *)arguments, (char * *)environment);
		message(log, "%s: could not execute: %s.\r\n", program, strerror(errno));
		exit(-1);
	}
	return pid;
}

static int
mem_total()
{
  char s[80];
  char *p;
  FILE *f;
  const char pattern[]="MemTotal:";

  f=fopen("/proc/meminfo","r");
  while (NULL != fgets(s,79,f)) {
    p=strstr(s, pattern);
    if (NULL != p) {
      fclose(f);
      return(atoi(p+strlen(pattern)));
    }
  }
  return -1;
}

static int
root_dev()
{
  int n;
  struct stat statbuf;

  stat("/", &statbuf);
  n = statbuf.st_dev;

  /*  printf("Root Device is: major: %d, minor: %d\n", n >> 8, n & 0xf);*/

  return n;
}

static void
set_free_pages()
{
  char s[80];
  FILE *f;

  f=fopen("/proc/sys/vm/freepages","r");
  fgets(s,79,f);
  if (atoi(s) < 32) {
    fclose(f);
    f=fopen("/proc/sys/vm/freepages","w");
    fprintf(f,"30\t40\t50\n");
    printf("\nIncreased /proc/sys/vm/freepages values to 30/40/50\n");
  }
  fclose(f);
}

static void
shutdown_system(int do_reboot)
{
	static const char * const umount_args[] = {"/bin/umount", "-a", "-n", 0};

	sync();
	/* Allow Ctrl-Alt-Del to reboot system. */
	reboot(0xfee1dead, 672274793, 0x89ABCDEF);

	/* Send signals to every process _except_ pid 1 */
	message(console, "Sending SIGHUP to all processes.\r\n");
	kill(-1, SIGHUP);
	sleep(2);
	sync();
	message(console, "Sending SIGKILL to all processes.\r\n");
	kill(-1, SIGKILL);
	sleep(1);
	waitfor(run("/bin/umount", umount_args, console, 0, NULL));
	sync();
	bdflush(1, 0);
	sync();
	reboot(0xfee1dead, 672274793, do_reboot ? 0x01234567 : 0xCDEF0123);
	exit(0);
}

static void
halt_signal(int sig)
{
	shutdown_system(0);
}

static void
reboot_signal(int sig)
{
	shutdown_system(1);
}

static void
exit_signal(int sig)
{
  
  /* initrd doesn't work anyway */

  shutdown_system(1);

	/* This is used on the initial ramdisk */

  /*	message(log, "Init exiting.");
	exit(0);
	*/
}

extern int
init_main(struct FileInfo * i, int argc, char * * argv)
{
	static const char * const	rc = "etc/rc";
	const char *				arguments[100];
	int							run_rc = 1;
	int							j;
	int							pid1 = 0;
	int							pid2 = 0;
	int							create_swap= -1;
	struct stat						statbuf;
	const char *				tty_commands[2] = { "sbin/dinstall", "bin/sh"};
	char				swap[20];
	char				swap_env[20];
	char * 				add_environment = NULL;

	/*
	 * If I am started as /linuxrc instead of /sbin/init, I don't have the
	 * environment that init expects. I can't fix the signal behavior. Try
	 * to divorce from the controlling terminal with setsid(). This won't work
	 * if I am the process group leader.
	 */
	setsid();

	signal(SIGUSR1, halt_signal);
	signal(SIGUSR2, reboot_signal);
	signal(SIGINT, reboot_signal);
	signal(SIGTERM, exit_signal);

	reboot(0xfee1dead, 672274793, 0);

	message(log, "%s: started. ", argv[0]);

	for ( j = 1; j < argc; j++ ) {
		if ( strcmp(argv[j], "single") == 0 ) {
			run_rc = 0;
			tty_commands[0] = "bin/sh";
			tty_commands[1] = 0;
		}
	}
	for ( j = 0; environ[j] != 0; j++ ) {
		if ( strncmp(environ[j], "tty", 3) == 0
		 && environ[j][3] >= '1'
		 && environ[j][3] <= '2'
		 && environ[j][4] == '=' ) {
			const char * s = &environ[j][5];

			if ( *s == 0 || strcmp(s, "off") == 0 )
				s = 0;

			tty_commands[environ[j][3] - '1'] = s;
		}
		else if ( strncmp(environ[j], "console=", 8) == 0 ) {
			first_terminal=&(environ[j][8]);
		}
	}

	printf("mounting /proc ...\n");
	if (mount("/proc","/proc","proc",0,0)) {
	  perror("mounting /proc failed\n");
	}
	printf("\tdone.\n");

	set_free_pages();

	if (mem_total() < 3500) { /* not enough memory for standard install */
	  int retval;
	  retval= stat("/etc/revision_ext.lowmem",&statbuf);
	  if (retval) {
	    printf("
You do not have enough RAM, hence you must use the special
low-memory installation procedure. 

Read the instructions in the install.html file.
");
	    while (1) {;}
	  } else if ( 1 == (root_dev() >> 8)) { /* rootdev is ramdisk - error */
		  printf("
You booted from the Low-Memory Root Disk. 

In order to do a low-memory installation you must boot using the
floppy boot method.

So reboot and at the boot: prompt type `floppy'.

For details read the instructions in the install.html file.
");
		  while (1) {;}
	  } else { /* everything OK */

	    retval = stat("/sbin/swapsetup",&statbuf);
	    if (retval) { /* swapsetup doesn't exist, second stage */
	      FILE *f;
	      
	      f=fopen("/etc/swappartition","r");
	      fgets(swap,19,f);
	      *(strstr(swap,"\n"))='\0';
	      
	      if (swapon(swap,0)) {
		perror("swapon failed\n");
	      } else {
		sprintf(swap_env,"Swap=%s",swap);
		add_environment=swap_env;
		create_swap = 0;
	      }
	    } else { /* first stage */
	      tty_commands[0] = "sbin/swapsetup";
	      create_swap = 1;
	    }
	  }
	}

	/*
	 * Don't modify **argv directly, it would show up in the "ps" display.
	 * I don't want "init" to look like "rc".
	 */
	arguments[0] = rc;
	for ( j = 1; j < argc; j++ ) {
		arguments[j] = argv[j];
	}
	arguments[j] = 0;

	if ( run_rc )
		waitfor(run(rc, arguments, console, 0, NULL));

	if ( 0 == create_swap) {
	  if (unlink("/etc/swappartition")) {
	    perror("unlinking /etc/swappartition");
	  }
	}

	arguments[0] = "-sh";
	arguments[1] = 0;
	for ( ; ; ) {
		int	wpid;
		int	status;

		if ( pid1 == 0 && tty_commands[0] )
			pid1 = run(tty_commands[0], arguments, first_terminal, 0, add_environment);
		if ( pid2 == 0 && tty_commands[1] )
			pid2 = run(tty_commands[1], arguments, second_terminal, 1, NULL);
		wpid = wait(&status);
		if ( wpid > 0 ) {
			/* DEBUGGING */
			message(log, "pid %d exited, status=%x.\n", wpid, status);
		}
		if ( wpid == pid1 ) {
		  pid1 = 0;
		}
		if ( wpid == pid2 )
			pid2 = 0;
	}
}
