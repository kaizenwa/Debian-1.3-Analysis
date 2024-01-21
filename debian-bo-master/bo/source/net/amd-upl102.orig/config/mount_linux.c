/*
 * $Id: mount_linux.c,v 5.2.1.1 90/10/21 22:30:59 jsp Exp $
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
 *
 * Adapted from nfsmount.c and mount.c copyright Rick Sladkey, H.J.Lu, and
 * Doug Quale - thanx guyz.
 * -- Mitchum D'Souza -- 1993-07-04
 */

#define MS_RDONLY    1 /* mount read-only */
#define MS_NOSUID    2 /* ignore suid and sgid bits */
#define MS_NODEV     4 /* disallow access to device special files */
#define MS_NOEXEC    8 /* disallow program execution */
#define MS_SYNC     16 /* writes are synced at once */
#define MS_REMOUNT  32 /* alter flags of a mounted FS */

struct opt_map
{
	const char *opt;		/* option name */
	int  inv;			/* true if flag value should be inverted */
	int  mask;			/* flag mask value */
};

const struct opt_map opt_map[] =
{
	{ "defaults",	0, 0		},
	{ "ro",		0, MS_RDONLY	},
	{ "rw",		1, MS_RDONLY	},
	{ "exec",	1, MS_NOEXEC	},
	{ "noexec",	0, MS_NOEXEC	},
	{ "suid",	1, MS_NOSUID	},
	{ "nosuid",	0, MS_NOSUID	},
	{ "dev",	1, MS_NODEV	},
	{ "nodev",	0, MS_NODEV	},
	{ "sync",	0, MS_SYNC	},
	{ "async",	1, MS_SYNC	},
#ifdef MS_NOSUB

	{ "sub",	1, MS_NOSUB	},
	{ "nosub",	0, MS_NOSUB	},
#endif
	{ NULL,		0, 0		}
};

static inline void
parse_opt (const char *opt, int *mask, char *extra_opts)
{
	const struct opt_map *om;

	for (om = opt_map; om->opt != NULL; ++om)
		if (STREQ (opt, om->opt))
		{
			if (om->inv)
				*mask &= ~om->mask;
			else
				*mask |= om->mask;
			return;
		}
	strcat (extra_opts, ",");
	strcat (extra_opts, opt);
}

/* Take -o options list and compute 4th and 5th args to mount(2).  flags
   gets the standard options and extra_opts anything we don't recognize.  */
static void
parse_opts (char *opts, int *flags, char **extra_opts, int *noauto)
{
	char *opt;
	int readonly = 0, readwrite = 0;

	*noauto = 0;
	if (opts != NULL)
	{
		*extra_opts = xmalloc (strlen (opts) + 2);
		**extra_opts = '\0';

		for (opt = strtok (opts, ",");
		     opt != NULL;
		     opt = strtok (NULL, ","))
			if (!((readwrite = (STREQ (opt, "rw"))) ||
			      (readonly  = (STREQ (opt, "ro"))) ||
			      (*noauto   = (STREQ (opt, "noauto"))) ||
			      (strncmp(opt,"type",4)==0)))
				parse_opt (opt, flags, *extra_opts);
	}
	if (readonly)
		*flags |= MS_RDONLY;
	if (readwrite)
		*flags &= ~MS_RDONLY;
}

int linux_mount(type, mnt, flags, mnt_data)
char *type;
struct mntent *mnt;
int flags;
struct nfs_mount_data *mnt_data;
{
	char *extra_opts = NULL;
	char *tmp_opts = NULL;
	char *sub_type = NULL;
	int noauto = 0;
	int errorcode;

	if (mnt->mnt_opts && STREQ (mnt->mnt_opts, "defaults"))
		mnt->mnt_opts = NULL;

	if (type == NULL)
		type = (index (mnt->mnt_fsname, ':') != NULL) ? "nfs" : MOUNT_TYPE_UFS ;

	if (STREQ(type,"nfs")) {
		/* Fake some values for linux */
		mnt_data->version=NFS_MOUNT_VERSION;
		if (!mnt_data->timeo)	mnt_data->timeo=7;
		if (!mnt_data->retrans) mnt_data->retrans=3;

		/* These are the only two reliable values currently */
		if (!mnt_data->rsize)	mnt_data->rsize=1024;
		if (!mnt_data->wsize)	mnt_data->wsize=1024;
		if (((mnt_data->fd=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP))< 0 )||
		    (bindresvport(mnt_data->fd, 0) < 0) ||
		    (connect(mnt_data->fd, (struct sockaddr *) &mnt_data->addr,
			    sizeof (mnt_data->addr)) < 0)) {
			perror("Can't create socket for kernel");
			errorcode=1;
			goto fail;
		}
#ifdef Linux_DEBUG
	plog(XLOG_INFO, "linux mount: type %s\n",type);
	plog(XLOG_INFO, "linux mount: version %d\n",mnt_data->version);
	plog(XLOG_INFO, "linux mount: fd %d\n",mnt_data->fd);
	plog(XLOG_INFO, "linux mount: hostname %s\n", \
		inet_ntoa(mnt_data->addr.sin_addr));
	plog(XLOG_INFO, "linux mount: port %d\n", \
		htons(mnt_data->addr.sin_port));
#endif
	} else {
	/* Non nfs mounts */
	if ((sub_type=hasmntopt(mnt,"type")) &&
	    (sub_type=index(sub_type,'=')) &&
	    (sub_type=strdup(sub_type+1)))
		(type=strpbrk(sub_type,",:;\n\t"))==NULL ?
			type=MOUNT_TYPE_UFS :
			(*type=(char)NULL), type=sub_type;

	if (!hasmntopt(mnt,"type")) mnt->mnt_type=MOUNT_TYPE_UFS;
	/* We only parse opts if non nfs drive */
	parse_opts (tmp_opts=strdup (mnt->mnt_opts), &flags, &extra_opts, &noauto);

#ifdef Linux_DEBUG
	plog(XLOG_INFO, "linux mount: type %s\n",type);
	plog(XLOG_INFO, "linux mount: xopts %s\n",extra_opts);
#endif
	}
#ifdef Linux_DEBUG
	plog(XLOG_INFO, "linux mount: fsname %s\n",mnt->mnt_fsname);
	plog(XLOG_INFO, "linux mount: type (mntent) %s\n",mnt->mnt_type);
	plog(XLOG_INFO, "linux mount: opts %s\n",mnt->mnt_opts);
	plog(XLOG_INFO, "linux mount: dir %s\n",mnt->mnt_dir);
#endif

/* If we have an nfs mount, the 5th argument to system mount() must be the
 * nfs_mount_data structure, otherwise it is the return from parse_opts()
 */
	errorcode=mount(mnt->mnt_fsname, mnt->mnt_dir, type, 0xC0ED0000 |
		 (flags), STREQ(type,"nfs") ? (char *) mnt_data : extra_opts);

/* If we failed, (i.e. errorcode != 0), then close the socket if its is open
 */
	if (errorcode)
		if (mnt_data->fd != -1) close (mnt_data->fd);

/* Free all allocated space and return errorcode.
 */
fail:		if (extra_opts != NULL)	free (extra_opts);
		if (tmp_opts != NULL)	free (tmp_opts);
		if (sub_type != NULL)	free (sub_type);
		return (errorcode);
}
