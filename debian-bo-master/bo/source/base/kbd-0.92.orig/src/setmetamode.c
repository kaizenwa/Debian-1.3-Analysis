/*
 * setmetamode.c - aeb, 940130
 *
 * Call: setmetamode { metabit | escprefix }
 * and report the setting before and after.
 * Without arguments setmetamode will only report.
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
	setmetamode [ metabit | meta | bit | escprefix | esc | prefix ]
Each vt has his own copy of this bit. Use
	setmetamode [arg] < /dev/ttyn
to change the settings of another vt.
The setting before and after the change are reported.
");
    exit(1);
}

void
report(meta) int meta;
{
    char *s;

    switch(meta) {
      case K_METABIT:
	s = "Meta key sets high order bit\n";
	break;
      case K_ESCPREFIX:
	s = "Meta key gives Esc prefix\n";
	break;
      default:
	s = "Strange mode for Meta key?\n";
    }
    printf(s);
}

struct meta {
    char *name;
    int val;
} metas[] = {
    { "metabit",   K_METABIT },
    { "meta",      K_METABIT },
    { "bit",       K_METABIT },
    { "escprefix", K_ESCPREFIX },
    { "esc",       K_ESCPREFIX },
    { "prefix",    K_ESCPREFIX }
};

#define SIZE(a) (sizeof(a)/sizeof(a[0]))

int
main(argc,argv) int argc; char **argv;
{
    char ometa, nmeta;
    struct meta *mp;

    if (ioctl(0, KDGKBMETA, &ometa)) {
	perror("KDGKBMETA");
	fprintf(stderr, "Error reading current setting. Maybe stdin is not a VT?\n");
	exit(1);
    }

    if (argc <= 1) {
	report(ometa);
	exit(0);
    }

    nmeta = 0;			/* make gcc happy */
    for (mp = metas; mp-metas < SIZE(metas); mp++) {
	if(!strcmp(argv[1], mp->name)) {
	    nmeta = mp->val;
	    goto fnd;
	}
    }
    fprintf(stderr, "unrecognized argument: _%s_\n\n", argv[1]);
    usage();

  fnd:
    printf("old state:    ");
    report(ometa);
    if (ioctl(0, KDSKBMETA, nmeta)) {
	perror("KDSKBMETA");
	exit(1);
    }
    printf("new state:    ");
    report(nmeta);
    exit(0);
}
