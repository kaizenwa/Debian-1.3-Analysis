/*
 * grpunconv - update /etc/group with information from /etc/gshadow.
 *
 * Copyright (C) 1996, Michael Meskes <meskes@debian.org>
 * using sources from Marek Michalkiewicz
 * <marekm@i17linuxb.ists.pwr.wroc.pl>
 * This program may be freely used and distributed.  If you improve
 * it, please send me your changes.  Thanks!
 */

#include <config.h>

#include <stdio.h>

#ifdef SHADOWGRP

#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>

#include <grp.h>
#include "prototypes.h"
#include "groupio.h"
#include "sgroupio.h"

int
main()
{
	const struct group *gr;
	struct group grent;
	const struct sgrp *sg;
	int fd;

	if (!gr_lock()) {
		fprintf(stderr, "can't lock group file\n");
		exit(5);
	}
	if (!gr_open(O_RDWR)) {
		fprintf(stderr, "can't open group file\n");
		exit(1);
	}

	if (!sgr_lock()) {
		fprintf(stderr, "can't lock shadow group file\n");
		exit(5);
	}
	if (!sgr_open(O_RDWR)) {
		fprintf(stderr, "can't open shadow group file\n");
		exit(1);
	}

	/*
	 * Update group passwords if non-shadow password is "x".
	 */
	gr_rewind();
	while ((gr = gr_next())) {
		sg = sgr_locate(gr->gr_name);
		if (sg && strcmp(gr->gr_passwd, "x") == 0) {
	  		/* add password to /etc/group */
	  		grent = *gr;
			grent.gr_passwd = sg->sg_passwd;
			if (!gr_update(&grent)) {
				fprintf(stderr, "can't update entry for group %s\n",
					grent.gr_name);
				exit(3);
			}
		}
	}

	if (!sgr_close()) {
		fprintf(stderr, "can't update shadow group file\n");
		exit(3);
	}

	if (!gr_close()) {
		fprintf(stderr, "can't update group file\n");
		exit(3);
	}

	if (unlink(GSHADOW) != 0) {
		fprintf(stderr, "can't delete shadow group file\n");
	        exit(3);
	}		                        

	sgr_unlock();
	gr_unlock();
	return 0;
}
#else
int
main(argc, argv)
int argc;
char **argv;
{
        fprintf(stderr, "%s: not configured for shadow group support.\n",argv[0]);
        return 1;
}
#endif
                                                
