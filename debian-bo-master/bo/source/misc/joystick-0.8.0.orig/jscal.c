
/*
   Joystick calibration program
*/

#include <linux/joystick.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <errno.h>

int main (int argc, char **argv)
{
	int fd, status, tmp;
	long tmpl;
	char *fname;
	struct JS_DATA_TYPE js_data;

	/* should be one argument, and it should be "0" or "1" */
	if (argc != 2 || (strcmp (argv[1], "0") && strcmp (argv[1], "1"))) {
		fprintf (stderr, "usage: jscal 0|1\n");
		exit (1);
	}

	/* pick appropriate device file */
	if (!strcmp (argv[1], "0"))
		fname = "/dev/js0";
	else if (!strcmp (argv[1], "1"))
		fname = "/dev/js1";
	else
		fname = NULL;

	/* open device file */
	fd = open (fname, O_RDONLY);
	if (fd < 0) {
		perror ("jscal");
		exit (1);
	}

	status = ioctl (fd, JS_GET_TIMEOUT, &tmp);
	if (status == -1) {
		perror ("jscal");
		exit (1);
	}

	printf ("Timeout value = %d\n", tmp);

	status = ioctl (fd, JS_GET_TIMELIMIT, &tmpl);
	if (status == -1) {
		perror ("jscal");
		exit (1);
	}

	printf ("Timelimit value = %ld ms\nSetting Timelimit = 100 ms\n", tmpl);

	tmpl = 100;

	status = ioctl (fd, JS_SET_TIMELIMIT, &tmpl);
	if (status == -1) {
		perror ("jscal");
		exit (1);
	}

	status = ioctl (fd, JS_GET_CAL, &js_data);
	if (status == -1) {
		perror ("jscal");
		exit (1);
	}

	printf ("Current correction: %d , %d\n", js_data.x, js_data.y);

	printf ("Move joystick to lower right corner and press either button\n");

	while ((read (fd, &js_data, JS_RETURN) > 0) && js_data.buttons == 0x00)
		printf ("Got x = %04x, y = %04x\r", js_data.x, js_data.y);

	for (tmp = 0; js_data.x > 0xff; tmp++, js_data.x = js_data.x >> 1);
	js_data.x = tmp;
	for (tmp = 0; js_data.y > 0xff; tmp++, js_data.y = js_data.y >> 1);
	js_data.y = tmp;

	printf ("Setting correction: %d , %d\n", js_data.x, js_data.y);

	status = ioctl (fd, JS_SET_CAL, &js_data);
	if (status == -1) {
		perror ("jscal");
		exit (1);
	}

	status = ioctl (fd, JS_GET_CAL, &js_data);
	if (status == -1) {
		perror ("jscal");
		exit (1);
	}

	printf ("Verify Correction (interrupt to exit): %d, %d\n",
		js_data.x,
		js_data.y);

	while (1)
		if (read (fd, &js_data, JS_RETURN) != JS_RETURN)
			perror ("jscal");
		else {
			fprintf (stdout, "%x %x %x        \r",
				js_data.buttons, js_data.x, js_data.y);
			fflush (stdout);
			usleep(100);
		}
	close (fd);
}
