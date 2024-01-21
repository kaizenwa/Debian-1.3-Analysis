#include <stdio.h>
#include <termios.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>

#define	TTY	"/dev/tty"

/* Issue prompt and read reply with echo turned off */
char *getpass(const char * prompt)
{
	struct termios ttyb,ttysav;
	char *cp;
	int c;
	FILE *tty;
	static char pbuf[128];

	if ((tty = fdopen(open(TTY, O_RDWR), "r")) == NULL)
		tty = stdin;
	else
		setbuf(tty, (char *)NULL);

	ioctl(fileno(tty), TCGETS, &ttyb);
	ioctl(fileno(tty), TCGETS, &ttysav);

	ttyb.c_lflag &= ~(ECHO|ISIG);
	ioctl(fileno(tty), TCSETS, &ttyb);

	fputs(prompt, stderr); fflush(stderr);

	cp = pbuf;
	for (;;) {
		c = getc(tty);
		if(c == '\r' || c == '\n' || c == EOF)
			break;
		if (cp < &pbuf[120])
			*cp++ = c;
		else
		        break;
	}
	*cp = '\0';

	fputs("\r\n", stderr); fflush(stderr);

	ioctl(fileno(tty), TCSETS, &ttysav);
	if (tty != stdin)
		fclose(tty);

	return(pbuf);
}
