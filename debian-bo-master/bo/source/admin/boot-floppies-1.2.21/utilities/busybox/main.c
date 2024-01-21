#include "internal.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>

static const struct Applet	applets[] = {
{ "block_device",block_device_main,	0,	block_device_usage,1,1	},
{ "cat",	cat_more_main,	cat_fn,		cat_usage,		0,	-1	},
{ "chgrp",	chgrp_main,		0,			chgrp_usage,	2,	-1	},
{ "chmod",	chmod_main,		0,			chmod_usage,	2,	-1	},
{ "chown",	chown_main,		0,			chown_usage,	2,	-1	},
{ "clear",	clear_main,		0,			clear_usage,	0,	 0	},
{ "cp",		dyadic_main,	cp_fn,		cp_usage,		2,	-1	},
{ "date",	date_main,		0,			date_usage,		0,	 0	},
{ "dd",		dd_main,		0,			dd_usage,	0,	2	},
{ "df",		df_main,		0,			df_usage,		0,	-1	},
{ "dmesg",	dmesg_main,		0,			dmesg_usage,		0,	 0	},
{ "false",	false_main,		0,			false_usage,	0,	 0	},
{ "fdflush",monadic_main,	fdflush_fn,	fdflush_usage,	1,	-1	},
{ "find",		find_main,	find_fn,    find_usage,	1,	-1 },
{ "floppy_merge",floppy_merge_main,0,	floppy_merge_usage,	1,	1 },
{ "halt",	halt_main,		0,			halt_usage,		0,	 0	},
{ "init",	init_main,		0,			init_usage,		0,	-1	},
{ "kill",	kill_main,		0,			kill_usage,		1,	-1	},
{ "length",	length_main,	0,			length_usage,	1,	 1	},
{ "ln",		dyadic_main,	ln_fn,		ln_usage,		2,	-1	},
{ "losetup",	losetup_main,	0,			losetup_usage,	1,	2	},
{ "ls",		ls_main,		0,			ls_usage,		0,	-1	},
{ "math",	math_main,		0,			math_usage,		1,	-1	},
{ "mkdir",	monadic_main,	mkdir_fn,	mkdir_usage,	1,	-1	},
{ "mknod",	mknod_main,		0,			mknod_usage,	2,	 5	},
{ "mkswap",	mkswap_main,	0,			mkswap_usage,	1,	-1	},
{ "more",	cat_more_main,	more_fn,	more_usage,		0,	-1	},
{ "mount",	mount_main,		0,			mount_usage,	0,	-1	},
{ "mt",		mt_main,		0,			mt_usage,		1,	-1	},
{ "mv",		dyadic_main,	mv_fn,		mv_usage,		2,	-1	},
{ "pwd",	pwd_main,		0,			pwd_usage,		0,	 0	},
{ "reboot",	reboot_main,	0,			reboot_usage,	0,	 0	},
{ "rm",		rm_main,		rm_fn,		rm_usage,		1,	-1	},
{ "rmdir",	monadic_main,	rmdir_fn,	rmdir_usage,	1,	-1	},
{ "sleep",	sleep_main,		0,			sleep_usage,	1,	-1	},
{ "star",	star_main,		0,			star_usage,		0,	 0	},
{ "swapoff",monadic_main,	swapoff_fn,	swapoff_usage,	1,	-1	},
{ "swapon",	monadic_main,	swapon_fn,	swapon_usage,	1,	-1	},
{ "sync",	sync_main,		0,			sync_usage,		0,	 0	},
{ "touch",	monadic_main,	touch_fn,	touch_usage,	1,	-1	},
{ "true",	true_main,		0,			true_usage,		0,	 0	},
{ "tryopen",tryopen_main,	0,			tryopen_usage,	0,	-1	},
{ "umount",	umount_main,	0,			umount_usage,	1,	-1	},
{ "update",	update_main,	0,			update_usage,	0,	-1	},
{ "zcat",	zcat_main,	0,			zcat_usage,	0,	-1	},
{ 0 }
};

extern int
main(int argc, char * * argv)
{
	char *			s = argv[0];
	char *			name = argv[0];
	const struct Applet *	a = applets;
	struct FileInfo	i;
	

	while ( *s != '\0' ) {
		if ( *s++ == '/' )
			name = s;
	}

	while ( a->name != 0 ) {
		if ( strcmp(name, a->name) == 0 ) {
			int	status;

			if ( argc - 1 < a->minimumArgumentCount
			 || (a->maximumArgumentCount > 0
			  && argc - 1 > a->maximumArgumentCount )
			 || (a->usage && argc >= 2 && strcmp(argv[1], "--help") == 0 ) ) {
				usage(a->usage);
				return 1;
			}
			errno = 0;
			memset((void *)&i, 0, sizeof(struct FileInfo));
			i.orWithMode = 0777;
			i.andWithMode = ~0;
			i.applet = a;
			status = ((*(a->main))(&i, argc, argv));
			if ( status < 0 ) {
				fprintf(
				 stderr
				,"%s: %s\n"
				,a->name
				,strerror(errno));
			}
			exit(status);
		}
		a++;
	}
	fprintf(stderr, "error: called as %s\n", argv[0]);
	return -1;
}
