/*
 * setleds.c - aeb, 940130, 940909
 *
 * Call: setleds [-L] [-D] [-F] [{+|-}{num|caps|scroll}]*
 * will set or clear the indicated flags on the stdin tty,
 * and report the settings before and after.
 * In particular, setleds without arguments will only report.
 */

#include <stdio.h>
#include <fcntl.h>
#include <linux/kd.h>
#include <sys/ioctl.h>

void
usage(void)
{
    fprintf(stderr, "\
Usage:
	setleds [-v] [-L] [-D] [-F] [[+|-][ num | caps | scroll ]]
Thus,
	setleds +caps -num
will set CapsLock, clear NumLock and leave ScrollLock unchanged.
The settings before and after the change (if any) are reported
when the -v option is given or when no change is requested.
Normally, setleds influences the vt flag settings
(and these are usually reflected in the leds).
With -L, setleds only sets the leds, and leaves the flags alone.
With -D, setleds sets both the flags and the default flags, so
that a subsequent reset will not change the flags.
");
    exit(1);
}

    

#define onoff(a) ((a) ? "on " : "off")

void
report(leds) int leds;
{
    printf("NumLock %s   CapsLock %s   ScrollLock %s\n",
	   onoff(leds & LED_NUM),
	   onoff(leds & LED_CAP),
	   onoff(leds & LED_SCR));
}

struct led {
    char *name;
    int bit;
} leds[3] = {
    { "scroll", LED_SCR },
    { "num", LED_NUM },
    { "caps", LED_CAP }
};

int
main(argc,argv) int argc; char **argv;
{
    int optL = 0, optD = 0, optF = 0, verbose = 0;
    char oleds, nleds, oflags, nflags, odefflags, ndefflags;
    char nval, ndef, sign;
    char *ap;
    struct led *lp;

    if (ioctl(0, KDGETLED, &oleds)) {
	perror("KDGETLED");
	fprintf(stderr,
	  "Error reading current led setting. Maybe stdin is not a VT?\n");
	exit(1);
    }

    if (ioctl(0, KDGKBLED, &oflags)) {
	perror("KDGKBLED");
	fprintf(stderr,
          "Error reading current flags setting. Maybe an old kernel?\n");
	exit(1);
    }

    while (argc > 1) {
	if (!strcmp("-L", argv[1]))
	  optL = 1;
	else if (!strcmp("-D", argv[1]))
	  optD = 1;
	else if (!strcmp("-F", argv[1]))
	  optF = 1;
	else if (!strcmp("-v", argv[1]))
	  verbose = 1;
	else
	  break;
	argc--;
	argv++;
    }

    odefflags = ndefflags = ((oflags >> 4) & 7);
    oflags = nflags = (oflags & 7);

    if (argc <= 1) {
	if (optL) {
	    nleds = 0xff;
	    if (ioctl(0, KDSETLED, &nleds)) {
		perror("KDSETLED");
		fprintf(stderr, "Error resetting ledmode\n");
		exit(1);
	    }
	}

	/* If nothing to do, report, even if not verbose */
	if (!optD && !optL && !optF)
	  optD = optL = optF = 1;
	if (optD) {
	    printf("Current default flags:  ");
	    report(odefflags);
	}
	if (optF) {
	    printf("Current flags:          ");
	    report(oflags & 07);
	}
	if (optL) {
	    printf("Current leds:           ");
	    report(oleds);
	}
	exit(0);
    }

    if (!optL)
      optF = 1;
    nval = 0;
    ndef = 0;

    while(--argc) {
	ap = *++argv;
	sign = 1;		/* by default: set */
	if(*ap == '+')
	  ap++;
	else if(*ap == '-') {
	    sign = 0;
	    ap++;
	}
	for (lp = leds; lp-leds < 3; lp++) {
	    if(!strcmp(ap, lp->name)) {
		if(sign)
		  nval |= lp->bit;
		ndef |= lp->bit;
		goto nxtarg;
	    }
	}
	fprintf(stderr, "unrecognized argument: _%s_\n\n", ap);
	usage();

      nxtarg: ;
    }

    if (optD) {
	ndefflags = (odefflags & ~ndef) | nval;
	if (verbose) {
	    printf("Old default flags:    ");
	    report(odefflags);
	    printf("New default flags:    ");
	    report(ndefflags);
	}
    }
    if (optF) {
	nflags = ((oflags & ~ndef) | nval);
	if (verbose) {
	    printf("Old flags:            ");
	    report(oflags & 07);
	    printf("New flags:            ");
	    report(nflags & 07);
	}
    }
    if (optD || optF) {
	if (ioctl(0, KDSKBLED, (ndefflags << 4) | nflags)) {
	    perror("KDSKBLED");
	    exit(1);
	}
    }
    if (optL) {
	nleds = (oleds & ~ndef) | nval;
	if (verbose) {
	    printf("Old leds:             ");
	    report(oleds);
	    printf("New leds:             ");
	    report(nleds);
	}
	if (ioctl(0, KDSETLED, nleds)) {
	    perror("KDSETLED");
	    exit(1);
	}
    }
    exit(0);
}
