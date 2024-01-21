/* uartcount: (linux) display the modem input interrupt counters */

#include <stdio.h>
#include <termios.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <linux/serial.h>

int main(int argc, char **argv)
{
	int fd;
	struct serial_icounter_struct c;

	if ((fd = open(argc == 2 ? argv[1] : "/dev/modem", 
			O_RDONLY | O_NONBLOCK | O_NOCTTY)) < 0) {
		perror("cannot open modem device");
		exit(1);
	}
	while (1) {
		if (ioctl(fd, TIOCGICOUNT, &c) != 0)
			exit(1);
		printf("Count: RI=%6d # CD=%6d # CTS=%6d # DSR=%6d\n",
		       c.rng, c.dcd, c.cts, c.dsr);
		ioctl(fd, TIOCMIWAIT, TIOCM_RNG | TIOCM_CTS | TIOCM_DSR | TIOCM_CD);
	}
}
