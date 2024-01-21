/*
	scanport.c

	Scan unregistered i/o ports for a possible ISA device.

	Anything that is not registered in /proc/ioports is considered
	fair game (i.e. devices with modules unloaded, etc.) so beware.

	This code is ugly, and is hence GPL.	 :-P

	This is a hacked version for DOS/TurboC, and doesn't have
	the /proc/ioports safety feature like the linux one.

						Paul Gortmaker 09/96

*/

#define IO_START	0x100	/* Don't touch anything below 0x100	*/
#define IO_END		0x400	/* ISA i/o space wraps around at 0x400	*/

#include <stdlib.h>
#include <stdio.h>
#include "io.h"

void guess_hdwr(int port);

typedef struct io_entry {
	int start;
	int stop;
	char *name;
} io_entry;


/* Some common entries that we can help out with */
io_entry common_list[] = {
	{0x170, 0x177, "IDE hard disk ctrlr #2"},
	{0x1f0, 0x1f7, "IDE hard disk ctrlr #1"},
	{0x200, 0x20f, "joystick/game port"},
	{0x278, 0x27f, "2nd printer port"},
	{0x2e8, 0x2ef, "4th serial port"},
	{0x2f8, 0x2ff, "2nd serial port"},
	{0x370, 0x375, "floppy ctrlr #2"},
	{0x376, 0x376, "IDE hard disk ctrlr #2"},
	{0x377, 0x377, "floppy ctrlr #2"},
	{0x378, 0x37f, "1st printer port"},
	{0x388, 0x38b, "Yamaha sound chip"},
	{0x3b0, 0x3bb, "Hercules/mono display"},
/*	{0x3b0, 0x3bf, "EGA display"},		Nobody has these anymore. */
	{0x3bc, 0x3be, "mono printer port"},
	{0x3bf, 0x3bf, "Hercules/mono display"},
	{0x3c0, 0x3df, "EGA/VGA/SVGA card"},
/*	{0x3d4, 0x3d5, "CGA display card"},	Or these. */
	{0x3e8, 0x3ef, "3rd serial port"},
	{0x3f0, 0x3f5, "floppy ctrlr #1"},
	{0x3f6, 0x3f6, "IDE hard disk ctrlr #1"},
	{0x3f7, 0x3f7, "floppy ctrlr #1"},
	{0x3f8, 0x3ff, "1st serial port"},
	{0x000, 0x000, NULL}
};

void main(void) {

io_entry io_list[64];
int i=0, j, val;
char start[8], stop[8], name[32];

printf("\n\n\b\tWARNING: Reading i/o ports of some hardware may hang your machine\n");
printf("\b\tYou have 5 seconds to hit ^C (control+c) to abort...\n");
sleep(5);

printf("\nScanning for non 0xff values from %#x to %#x....\n\n", IO_START, IO_END);

for (i=IO_START;i<IO_END;i++) {
	val=inb_p(i);
	if (val != 0xff) {
		printf("Port 0x%3x has value 0x%x ",i, val);
		guess_hdwr(i);
	}
}

printf("\nDone.\n");

} /* end main */

void guess_hdwr(int port) {

int found = 0;
io_entry *this_dev;

printf("\t[ maybe: ");
for (this_dev = common_list; this_dev->name != NULL; this_dev++) {
	if ((this_dev->start <= port) && (this_dev->stop >= port)) {
		printf("%s? ",this_dev->name);
		found++;
	}
}

if (found == 0) printf("???????? ");
printf("]\n");

} /* end guess_hdwr */


