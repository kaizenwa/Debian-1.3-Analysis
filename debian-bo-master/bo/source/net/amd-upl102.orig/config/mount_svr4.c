/*
 * $Id: mount_svr4.c,v 5.2.1.1 90/10/21 22:30:59 jsp Exp $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	%W% (Berkeley) %G%
 */

/*
 * SVR4:
 * Solaris 2.1 (SunOS 5.1) Mount helper.
 *	-Erez Zadok <ezk@cs.columbia.edu>
 */

#include "misc-svr4.h"

#include <netdir.h>
#include <sys/stat.h>


/*
 * Map from conventional mount arguments
 * to Solaris 2.1 (SunOS 5.1) style arguments.
 */
mount_svr4(fsname, dir, flags, type, data)
char *fsname;
char *dir;
int flags;
int type;
caddr_t data;
{
	if (type == MOUNT_TYPE_NFS)
		return mount(fsname, dir, (MS_DATA|flags),
			     "nfs", (char *) data,
			     sizeof(struct nfs_args));

	if (type == MOUNT_TYPE_UFS)
		return mount(fsname, dir, (MS_DATA|flags),
			     "ufs", (char *) data,
			     sizeof(struct ufs_args));

#if defined(MOUNT_TYPE_CDFS) && defined(MTAB_TYPE_CDFS)
	if (type == MOUNT_TYPE_CDFS)
		return mount(fsname, dir, (MS_DATA|flags),
			     MTAB_TYPE_CDFS, (char *) data,
			     sizeof(struct cdfs_args));
#endif

#if defined(MOUNT_TYPE_LOFS) && defined(MTAB_TYPE_LOFS)
	if (type == MOUNT_TYPE_LOFS)
		return mount(fsname, dir, (MS_FSS|flags),
			     MOUNT_TYPE_LOFS, (char *) NULL,
			     0);
#endif

#if defined(MOUNT_TYPE_PCFS) && defined(MTAB_TYPE_PCFS)
	if (type == MOUNT_TYPE_PCFS)
		return mount(fsname, dir, (MS_DATA|flags),
			     "pcfs", (char *) data,
			     sizeof(struct pc_args));
#endif


	return EINVAL;
}

/*
 * Find netconfig info for udp device, and fill in the knetconfig 
 * structure.
 */
int get_knetconfig(kncpp)
     struct knetconfig **kncpp;
{
	struct netconfig *ncp = (struct netconfig *) NULL;
	struct stat statbuf;

	ncp = getnetconfigent(NC_UDP);
	if (!ncp)
		return -2;

	if ((*kncpp = (struct knetconfig *)
	     calloc(1, sizeof(struct knetconfig)))
	    == (struct knetconfig *) NULL)
	  return -3;
	(*kncpp)->knc_semantics = ncp->nc_semantics;
	(*kncpp)->knc_protofmly = strdup(ncp->nc_protofmly);
	(*kncpp)->knc_proto = strdup(ncp->nc_proto);
	if (stat(ncp->nc_device, &statbuf) < 0)
		return -3;	/* amd will end (free not needed) */
	(*kncpp)->knc_rdev = (dev_t) statbuf.st_rdev;
	freenetconfigent(ncp);
	ncp = NULL;
	return 0;
}

void free_knetconfig(kncp)
     struct knetconfig *kncp;
{
  if (kncp) {
    if (kncp->knc_protofmly)
      free(kncp->knc_protofmly);
    if (kncp->knc_proto)
      free(kncp->knc_proto);
    free(kncp);
    kncp = (struct knetconfig *) NULL;
  }
}
