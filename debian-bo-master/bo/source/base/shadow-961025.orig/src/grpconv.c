/*
 * grpconv - create or update /etc/gshadow with information from
 * /etc/group.
 *
 * Copyright (C) 1996, Marek Michalkiewicz
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

#include "rcsid.h"
RCSID("$Id: grpconv.c,v 1.1.1.1 1996/08/10 07:59:52 marekm Exp $")

int
main()
{
	const struct group *gr;
	struct group grent;
	const struct sgrp *sg;
	struct sgrp sgent;
	int fd;

	if (!gr_lock()) {
		fprintf(stderr, "can't lock group file\n");
		exit(5);
	}
	if (!gr_open(O_RDWR)) {
		fprintf(stderr, "can't open group file\n");
		exit(1);
	}

	/*
	 * Create empty /etc/gshadow if it doesn't exist.
	 */
	fd = open("/etc/gshadow", O_CREAT | O_WRONLY, 0400);
	if (fd < 0) {
		perror("open /etc/gshadow");
		exit(1);
	}
	close(fd);

	if (!sgr_lock()) {
		fprintf(stderr, "can't lock shadow group file\n");
		exit(5);
	}
	if (!sgr_open(O_RDWR)) {
		fprintf(stderr, "can't open shadow group file\n");
		exit(1);
	}

	/*
	 * Remove /etc/gshadow entries for groups not in /etc/group.
	 */
	sgr_rewind();
	while ((sg = sgr_next())) {
		if (gr_locate(sg->sg_name))
			continue;

		if (!sgr_remove(sg->sg_name)) {
			/*
			 * This shouldn't happen (the entry exists) but...
			 */
			fprintf(stderr, "can't remove shadow group %s\n",
				sg->sg_name);
			exit(3);
		}
	}

	/*
	 * Update shadow group passwords if non-shadow password is not "x".
	 * Add any missing shadow group entries.
	 */
	gr_rewind();
	while ((gr = gr_next())) {
		sg = sgr_locate(gr->gr_name);
		if (sg) {
#if 0  /* because of sg_mem, but see below */
			if (strcmp(gr->gr_passwd, "x") == 0)
				continue;
#endif
			/* update existing shadow group entry */
			sgent = *sg;
			if (strcmp(gr->gr_passwd, "x") != 0)
				sgent.sg_passwd = gr->gr_passwd;
		} else {
			static char *empty = 0;

			/* add new shadow group entry */
			memset(&sgent, 0, sizeof sgent);
			sgent.sg_name = gr->gr_name;
			sgent.sg_passwd = gr->gr_passwd;
			sgent.sg_adm = &empty;
		}
		/*
		 * XXX - sg_mem is redundant, it is currently always a copy
		 * of gr_mem.  Very few programs actually use sg_mem, and
		 * all of them are in the shadow suite...  Maybe this field
		 * could be used for something else?  Any suggestions?
		 */
		sgent.sg_mem = gr->gr_mem;

		if (!sgr_update(&sgent)) {
			fprintf(stderr, "can't update shadow entry for %s\n",
				sgent.sg_name);
			exit(3);
		}
		/* remove password from /etc/group */
		grent = *gr;
		grent.gr_passwd = "x";
		if (!gr_update(&grent)) {
			fprintf(stderr, "can't update entry for group %s\n",
				grent.gr_name);
			exit(3);
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
	fprintf(stderr, "%s: not configured for shadow group support.\n",
		argv[0]);
	return 1;
}
#endif
