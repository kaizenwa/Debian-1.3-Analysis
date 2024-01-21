/*
 * $Id: mount_isc3.c,v 5.2.1.1 90/10/21 22:30:59 jsp Exp $
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
 * IX/386 Mount helper
 */

#include "misc-isc3.h"

/*
 * Map from conventional mount arguments
 * to IX/386 style arguments.
 */
mount_isc3(fsname, dir, flags, type, data)
char *fsname;
char *dir;
int flags;
int type;
void *data;
{
#ifdef DEBUG
	dlog("mount_isc3: fsname %s, dir %s, type %d", fsname, dir, type);
#endif

	if (type == MOUNT_TYPE_NFS)
		return mount("/dev/nfsd", dir, (MS_FSS|MS_DATA|flags),
			     type, (struct nfs_args *) data,
			     sizeof(struct nfs_args));

	if (type == MOUNT_TYPE_UFS)
		return mount(fsname, dir, (MS_FSS|flags), type);

	return EINVAL;
}
