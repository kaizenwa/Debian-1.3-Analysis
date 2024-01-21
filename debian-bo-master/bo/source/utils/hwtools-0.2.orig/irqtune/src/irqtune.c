/* irqtune -- load IRQ tuning module */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <irqhigh.h>

#define RNGE(c,lo,hi) \
	(((c) >= (lo)) && ((c) <= (hi)))

unsigned char irq_high_list[2] = { IRQ_HIGH_MST_DFT, IRQ_HIGH_SLV_DFT };

int badflg;
char *pgmname;
char bigbf[1024];

/* module names */
char modname[1024];
char modroot[1024];
char modtail[1024];

/* function prototypes */
char *filehead(char *name);
char *filetail(char *name);
void usage(void);

/* main -- main program */
void
main(int argc,char **argv)
{
	char *cp;
	int idx;
	int code;
	int val;
	char *logf;

	pgmname = *argv;

	--argc;
	++argv;

	/* decode options */
	for (;  argc > 0;  --argc, ++argv) {
		cp = *argv;
		if (*cp != '-')
			break;

		switch (cp[1]) {
		case 'o':
			badflg = 1;
			break;

		default:
			usage();
			break;
		}
	}

	/* get IRQ priority arguments */
	idx = 0;
	for (;  argc > 0;  --argc, ++argv) {
		cp = *argv;
		if (! RNGE(*cp,'0','9'))
			break;

		if (idx >= sizeof(irq_high_list))
			usage();

		val = atoi(cp);
		if (! RNGE(val,0,15))
			usage();
		irq_high_list[idx++] = val & 0x0F;
	}

	/* set bad defaults */
	if (badflg) {
		irq_high_list[0] = IRQ_HIGH_MST_BAD;
		irq_high_list[1] = IRQ_HIGH_SLV_BAD;
	}

	/* locate the module */
	strcpy(modname,pgmname);
	strcpy(modroot,filetail(modname));
	strcat(modroot,MODTAIL);
	strcat(modname,MODTAIL);
	strcat(modname,".o");

	/* NOTE: we are version independent */
	/* insmod complains even with the force option */
	logf = "/dev/null";
	sprintf(bigbf,"sh -c 'insmod -f %s priority=%d,%d >> %s 2>&1'",
		modname,irq_high_list[0],irq_high_list[1],logf);

	/* we don't need to check system log (we really should to be sure) */
	fprintf(stderr,"irqtune: setting system IRQ priority to %u/%u\n",
		irq_high_list[0],irq_high_list[1]);
	fflush(stderr);

	/* sync the disks, just in case */
	sync();

	code = system(bigbf);
	if (code) {
		fprintf(stderr,"irqtune: insmod failed on '%s'\n",modname);
		exit(1);
	}

	/* the mere act of installing does the job so uninstall immediately */
	sprintf(bigbf,"rmmod %s",modroot);
	code = system(bigbf);
	if (code) {
		fprintf(stderr,"irqtune: rmmod failed on '%s'\n",modroot);
		exit(1);
	}

	exit(0);
}

/* filehead -- get head of name */
/* RETURNS: pointer to tail */
char *
filehead(char *name)
/* name -- name to get head of */
{
	char *head;

	head = strrchr(name,'/');
	if (head != NULL)
		*head = 0;

	return name;
}

/* filetail -- get tail of name */
/* RETURNS: pointer to tail */
char *
filetail(char *name)
/* name -- name to get tail of */
{
	char *tail;

	tail = strrchr(name,'/');
	if (tail != NULL)
		++tail;
	else
		tail = name;

	return tail;
}

/* usage -- show program usage */
void
usage(void)
{

	fprintf(stderr,"usage: irqtune [options] [arguments]\n");

	fprintf(stderr,"options:\n");
	fprintf(stderr,"  -o -- reset to original values (%u/%u)\n",
		IRQ_HIGH_MST_BAD,IRQ_HIGH_SLV_BAD);

	fprintf(stderr,"arguments:\n");
	fprintf(stderr,"  1 - IRQ number of desired high priority IRQ on 8259 master (DEFAULT: %d)\n",
		IRQ_HIGH_MST_DFT);
	fprintf(stderr,"  2 - IRQ number of desired high priority IRQ on 8259 slave (DEFAULT: %d)\n",
		IRQ_HIGH_SLV_DFT);

	exit(1);
}
