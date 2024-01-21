#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <signal.h>
#include <fcntl.h>
#include <termios.h>
#include <linux/kd.h>
#include <linux/keyboard.h>
#include <sys/ioctl.h>

#define VERSION "0.91"

int tmp;	/* for debugging */

#define OPTION "medium raw"

extern int getfd();
int fd;
int oldkbmode;
struct termios old;

/*
 * version 0.81 of showkey would restore kbmode unconditially to XLATE,
 * thus making the console unusable when it was called under X.
 */
void get_mode(void) {
        char *m;

	if (ioctl(fd, KDGKBMODE, &oldkbmode)) {
		perror("KDGKBMODE");
		exit(1);
	}
	switch(oldkbmode) {
	  case K_RAW:
	    m = "RAW"; break;
	  case K_XLATE:
	    m = "XLATE"; break;
	  case K_MEDIUMRAW:
	    m = "MEDIUMRAW"; break;
	  default:
	    m = "?UNKNOWN?"; break;
	}
	printf("kb mode was %s\n", m);
	if (oldkbmode != K_XLATE) {
	    printf("[ if you are trying this under X, it might not work\n");
	    printf("since the X server is also reading /dev/console ]\n");
	}
	printf("\n");
}

void clean_up(void) {
	if (ioctl(fd, KDSKBMODE, oldkbmode)) {
		perror("KDSKBMODE");
		exit(1);
	}
	tmp = tcsetattr(fd, 0, &old);
	if (tmp)
		printf("tcsetattr = %d\n", tmp);
	close(fd);
}

void die(int x) {
	printf("caught signal %d, cleaning up...\n", x);
	clean_up();
	exit(1);
}

void watch_dog(int x) {
	clean_up();
	exit(0);
}

void usage(void) {
	fprintf(stderr, "\
showkey version " VERSION " (" OPTION ")

usage: showkey [options...]

valid options are:

	-h --help	display this help text
	-s --scancodes	display only the raw scan-codes
	-k --keycodes	display only the interpreted keycodes (default)
");
	exit(1);
}

int
main (int argc, char *argv[]) {
	const char *short_opts = "hsk";
	const struct option long_opts[] = {
		{ "help",	no_argument, NULL, 'h' },
		{ "scancodes",	no_argument, NULL, 's' },
		{ "keycodes",	no_argument, NULL, 'k' },
		{ NULL, 0, NULL, 0 }
	};
	int c;
	int show_keycodes = 1;

	struct termios new;
	unsigned char buf[16];
	int i, n;

	while ((c = getopt_long(argc, argv,
		short_opts, long_opts, NULL)) != -1) {
		switch (c) {
			case 's':
				show_keycodes = 0;
				break;
			case 'k':
				show_keycodes = 1;
				break;
			case 'h':
			case '?':
				usage();
		}
	}

	if (optind < argc)
		usage();

	fd = getfd();

	/* the program terminates when there is no input for 10 secs */
	signal(SIGALRM, watch_dog);

	/*
		if we receive a signal, we want to exit nicely, in
		order not to leave the keyboard in an unusable mode
	*/
	signal(SIGHUP, die);
	signal(SIGINT, die);
	signal(SIGQUIT, die);
	signal(SIGILL, die);
	signal(SIGTRAP, die);
	signal(SIGABRT, die);
	signal(SIGIOT, die);
	signal(SIGFPE, die);
	signal(SIGKILL, die);
	signal(SIGUSR1, die);
	signal(SIGSEGV, die);
	signal(SIGUSR2, die);
	signal(SIGPIPE, die);
	signal(SIGTERM, die);
#ifdef SIGSTKFLT
	signal(SIGSTKFLT, die);
#endif
	signal(SIGCHLD, die);
	signal(SIGCONT, die);
	signal(SIGSTOP, die);
	signal(SIGTSTP, die);
	signal(SIGTTIN, die);
	signal(SIGTTOU, die);

	get_mode();
	tmp = tcgetattr(fd, &old);
	if (tmp)
		printf("tcgetattr = %d\n", tmp);
	tmp = tcgetattr(fd, &new);
	if (tmp)
		printf("tcgetattr = %d\n", tmp);

	new.c_lflag &= ~ (ICANON | ECHO | ISIG);
	new.c_iflag = 0;
	new.c_cc[VMIN] = sizeof(buf);
	new.c_cc[VTIME] = 1;	/* 0.1 sec intercharacter timeout */

	tmp = tcsetattr(fd, TCSAFLUSH, &new);
	if (tmp)
		printf("tcsetattr = %d\n", tmp);
	if (ioctl(fd, KDSKBMODE,
		show_keycodes ? K_MEDIUMRAW : K_RAW)) {
		perror("KDSKBMODE");
		exit(1);
	}

	printf("press any key (program terminates after 10s of last keypress)...\n");
	while (1) {
		alarm(10);
		n = read(fd, buf, sizeof(buf));
		for (i = 0; i < n; i++) {
			if (!show_keycodes)
				printf("0x%02x ", buf[i]);
			else
				printf("keycode %3d %s\n",
					buf[i] & 0x7f,
					buf[i] & 0x80 ? "release" : "press");
		}
		if (!show_keycodes)
			printf("\n");
	}

	clean_up();
	exit(0);
}
