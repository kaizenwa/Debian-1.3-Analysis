/*
 * fh		This module handles the file-handle cache.
 *		FILE HANDLE PACKAGE FOR USER-LEVEL NFS SERVER
 *
 *		Interfaces:
 *		    pseudo_inode
 *			mostly used internally, but also called from unfsd.c
 *			when reporting directory contents.
 *		    fh_init
 *			Initializes the queues and 'flush' timer
 *		    fh_pr
 *			debugging primitive; converts file handle into a
 *			printable text string
 *		    fh_create
 *			establishes initial file handle; called from mount
 *			daemon
 *		    fh_path
 *			returns unix path corresponding to fh
 *		    fh_fd
 *			returns open file descriptor for given file handle;
 *			provides caching of open files
 *		    fd_idle
 *			provides mututal exclusion of normal file descriptor
 *			cache use, and alarm-driven cache flushing
 *		    fh_compose
 *			construct new file handle from existing file handle
 *			and directory entry
 *		    fh_psi
 *			returns pseudo_inode corresponding to file handle
 *		    fh_remove (new, by Don Becker)
 *			delete the file handle associated with PATH from the
 *			cache
 *
 * Authors:	Mark A. Shand, May 1988
 *		Donald J. Becker <becker@super.org>
 *		Rick Sladkey <jrs@world.std.com>
 *		Patrick	Sweeney <pjs@raster.Kodak.COM>
 *		Orest Zborowski <obz@raster.Kodak.COM>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Olaf Kirch, <okir@monad.swb.de>
 *
 *		Copyright 1988 Mark A. Shand
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */

#include <assert.h>
#include "nfsd.h"
#include "rpcmisc.h"

#define FHTRACE

#define hash_psi(psi) (((psi)^((psi)>>8)^((psi)>>16)^((psi)>>24)) & 0xff)

static mutex ex_state = inactive;
static mutex io_state = inactive;
#define HASH_TAB_SIZE	256
static fhcache fh_head, fh_tail;
static fhcache *fh_hashed[HASH_TAB_SIZE];
static fhcache *fd_lru_head = NULL,
               *fd_lru_tail = NULL;
static int fh_list_size;
static time_t curtime;

#ifndef FOPEN_MAX
#define FOPEN_MAX 256
#endif

#ifndef FHTRACE
#undef	D_FHTRACE
#define D_FHTRACE	D_FHCACHE
#endif

static fhcache *fd_cache[FOPEN_MAX] = { NULL };
static int fd_cache_size = 0;

#ifndef NFSERR_INVAL			/* that Sun forgot */
#define NFSERR_INVAL	22
#endif

struct {
	enum nfsstat error;
	int errno;
} nfs_errtbl[]= {
	{ NFS_OK,		0		},
	{ NFSERR_PERM,		EPERM		},
	{ NFSERR_NOENT,		ENOENT		},
	{ NFSERR_IO,		EIO		},
	{ NFSERR_NXIO,		ENXIO		},
	{ NFSERR_ACCES,		EACCES		},
	{ NFSERR_EXIST,		EEXIST		},
	{ NFSERR_NODEV,		ENODEV		},
	{ NFSERR_NOTDIR,	ENOTDIR		},
	{ NFSERR_ISDIR,		EISDIR		},
	{ NFSERR_INVAL,		EINVAL		},
	{ NFSERR_FBIG,		EFBIG		},
	{ NFSERR_NOSPC,		ENOSPC		},
	{ NFSERR_ROFS,		EROFS		},
	{ NFSERR_NAMETOOLONG,	ENAMETOOLONG	},
	{ NFSERR_NOTEMPTY,	ENOTEMPTY	},
#ifdef EDQUOT
	{ NFSERR_DQUOT,		EDQUOT		},
#endif
	{ NFSERR_STALE,		ESTALE		},
	{ NFSERR_WFLUSH,	EIO		},
	{ -1,			EIO		}
};

/* Forward declared local functions */
static _PRO(psi_t path_psi, (char *, nfsstat *, struct stat *, struct stat *));
static _PRO(int fh_flush_fds, (void));
static _PRO(char *fh_dump, (svc_fh *));
static _PRO(void fh_insert_fdcache, (fhcache *fhc));
static _PRO(void fh_unlink_fdcache, (fhcache *fhc));

static void fh_move_to_front(fhc)
fhcache *fhc;
{
	/* Remove from current posn */
	fhc->prev->next = fhc->next;
	fhc->next->prev = fhc->prev;

	/* Insert at head */
	fhc->prev = &fh_head;
	fhc->next = fh_head.next;
	fhc->prev->next = fhc;
	fhc->next->prev = fhc;
}

static void fh_inserthead(fhc)
fhcache *fhc;
{
	register fhcache **hash_slot;

	/* Insert at head. */
	fhc->prev = &fh_head;
	fhc->next = fh_head.next;
	fhc->prev->next = fhc;
	fhc->next->prev = fhc;
	fh_list_size++;

	/* Insert into hash tab. */
	hash_slot = &(fh_hashed[fhc->h.psi % HASH_TAB_SIZE]);
	fhc->hash_next = *hash_slot;
	*hash_slot = fhc;
}

static fhcache *fh_lookup(psi)
psi_t psi;
{
	register fhcache *fhc;

	fhc = fh_hashed[psi % HASH_TAB_SIZE];
	while (fhc != NULL && fhc->h.psi != psi)
		fhc = fhc->hash_next;
	return (fhc);
}

static void fh_insert_fdcache(fhc)
fhcache	*fhc;
{
	if (fhc == fd_lru_head)
		return;
	if (fhc->fd_next || fhc->fd_prev)
		fh_unlink_fdcache(fhc);
	if (fd_lru_head)
		fd_lru_head->fd_prev = fhc;
	else
		fd_lru_tail = fhc;
	fhc->fd_next = fd_lru_head;
	fd_lru_head = fhc;

#ifdef FHTRACE
	if (fd_cache[fhc->fd] != NULL) {
		Dprintf(L_ERROR, "fd cache inconsistency!\n");
		return;
	}
#endif
	fd_cache[fhc->fd] = fhc;
	fd_cache_size++;
}

static void fh_unlink_fdcache(fhc)
fhcache	*fhc;
{
	fhcache	*prev = fhc->fd_prev,
		*next = fhc->fd_next;

	fhc->fd_next = fhc->fd_prev = NULL;
	if (next) {
		next->fd_prev = prev;
	} else if (fd_lru_tail == fhc) {
		fd_lru_tail = prev;
	} else {
		Dprintf(L_ERROR, "fd cache inconsistency\n");
		return;
	}
	if (prev) {
		prev->fd_next = next;
	} else if (fd_lru_head == fhc) {
		fd_lru_head = next;
	} else {
		Dprintf(L_ERROR, "fd cache inconsistency\n");
		return;
	}

#ifdef FHTRACE
	if (fd_cache[fhc->fd] != fhc) {
		Dprintf(L_ERROR, "fd cache inconsistency!\n");
		return;
	}
#endif
	fd_cache[fhc->fd] = NULL;
	fd_cache_size--;
}

static void fh_close(fhc)
fhcache *fhc;
{
	if (fhc->fd >= 0) {
		Dprintf(D_FHCACHE,
			"fh_close: closing handle %x ('%s', fd=%d)\n",
			fhc, fhc->path ? fhc->path : "<unnamed>", fhc->fd);
		fh_unlink_fdcache(fhc);
		close(fhc->fd);
		fhc->fd = -1;
	}
}

static void fh_delete(fhc)
fhcache *fhc;
{
	register fhcache **hash_slot;

#ifdef FHTRACE
	if (fhc->h.hash_path[0] == (unsigned char)-1)
		return;
#endif

	Dprintf(D_FHTRACE|D_FHCACHE,
		"fh_delete: deleting handle %x ('%s', fd=%d)\n",
		fhc, fhc->path ? fhc->path : "<unnamed>", fhc->fd);

	/* Remove from current posn */
	fhc->prev->next = fhc->next;
	fhc->next->prev = fhc->prev;
	fh_list_size--;

	/* Remove from hash tab */
	hash_slot = &(fh_hashed[fhc->h.psi % HASH_TAB_SIZE]);
	while (*hash_slot != NULL && *hash_slot != fhc)
		hash_slot = &((*hash_slot)->hash_next);
	if (*hash_slot == NULL)
		Dprintf(L_ERROR,
			"internal inconsistency -- fhc(%x) not in hash table\n",
			fhc);
	else
		*hash_slot = fhc->hash_next;

	fh_close(fhc);

	/* Free storage. */
	if (fhc->path != NULL)
		free(fhc->path);

#ifdef FHTRACE
	/* Safeguard against cache corruption */
	fhc->path = NULL;
	fhc->h.hash_path[0] = -1;
#endif

	free(fhc);
}

/* Lookup an NFS error code and return UNIX equivalent. */
enum nfsstat nfs_errno()
{
	int i;

	for (i = 0; nfs_errtbl[i].error != -1; i++) {
		if (nfs_errtbl[i].errno == errno)
			return (nfs_errtbl[i].error);
	}
	Dprintf(L_ERROR, "non-standard errno: %d (%s)\n",
		errno, strerror(errno));
	return (NFSERR_IO);
}

/*
 * INODES and DEVICES.  NFS assumes that each file within an NFS mounted
 * file-system has a unique inode number.  Thus to mount an entire file
 * hierarchy, as this server sets out to do, requires mapping from inode/devs
 * to pseudo-inode.  Furthermore mount points must be detected and so that
 *	pseudo-inode("name") == pseudo-inode(direntry("name/../name"))
 * One option is to force the directory entry inode to correspond to the
 * result of the stat call, but this involves stat'ing every directory entry
 * during a readdir.  Instead we force the stat call to correspond to the
 * directory entry inode (see inner_getattr).  Of course this technique
 * requires that the parent directory is readable.  If it is not the normal
 * stat call result is used.  There is no chance of conflict because the
 * directory can never be read.
 *
 * In theory unique pseudo-inodes cannot be guaranteed, since inode/dev
 * contains 48 bits of information which must be crammed into an inode
 * number constrained to 32 bits.  Fortunately inodes numbers tend to be
 * small (often < 64k, almost always < 512k)
 *
 * On the Alpha, dev_t is 32bit. Fold the device number before hashing it.
 */
psi_t pseudo_inode(inode, dev)
ino_t inode;
dev_t dev;
{
	psi_t		dmajor, dminor;

	/*
         * Assuming major and minor numbers are small integers,
         * gravitate bits of dmajor & dminor device number to
         * high-order bits of word, to avoid clash with real inode num.
         */
	/* reverse (byte-wise) */
#if SIZEOF_DEV_T == 4
	dev = (((dev >> 16) & 0xff00) ^ ((dev >> 8) & 0xff00)) | 
	      (((dev >> 8) & 0xff) ^ (dev & 0xff));
#endif
	dmajor = ((dev & 0xf0f) << 4) | ((dev & 0xf0f0) >> 4);
	dmajor = ((dmajor & 0x3333) << 2) | ((dmajor & 0xcccc) >> 2);
	dmajor = ((dmajor & 0x5555) << 1) | ((dmajor & 0xaaaa) >> 1);

	/* spread low-16 -> 32 with 0's in even posn */
	dmajor = ((dmajor & 0xff00) << 8) | (dmajor & 0xff);
	dmajor = ((dmajor & 0xf000f0) << 4) | (dmajor & 0xf000f);
	dmajor = ((dmajor & 0xc0c0c0c) << 2) | (dmajor & 0x3030303);
	dmajor = ((dmajor & 0x22222222) << 1) | (dmajor & 0x11111111);
	dminor = (dmajor & 0x5555) << 15;
	dmajor = dmajor & 0x55550000;

	/*
	Dprintf(D_FHCACHE,
		"pseudo_inode: dev=%d, inode=%d, psi=%d\n", dev, inode,
		(dmajor | dminor) ^ inode);
	*/
	return ((dmajor | dminor) ^ inode);
}

#if 1
static char *fh_buildpath(h)
svc_fh *h;
{
	char		pathbuf[PATH_MAX + NAME_MAX + 1], *path;
	long		cookie_stack[HP_LEN + 1];
	char		*slash_stack[HP_LEN];
	struct stat	sbuf;
	psi_t		psi;
	int		i;

	if (h->hash_path[0] >= HP_LEN) {
		Dprintf(L_ERROR, "impossible hash_path[0] value: %s\n", 
					fh_dump(h));
		return NULL;
	}

	if (stat("/", &sbuf) < 0)
		return (NULL);
	psi = pseudo_inode(sbuf.st_ino, sbuf.st_dev);
	if (h->hash_path[0] == 0) {
		if (psi != h->psi)
			return (NULL);
		return xstrdup("/");
	}
	/* else */
	if (hash_psi(psi) != h->hash_path[1])
		return (NULL);

	auth_override_uid(ROOT_UID);	/* for x-only dirs */
	strcpy(pathbuf, "/");
	cookie_stack[2] = 0;
	for (i = 2; i <= h->hash_path[0] + 1; i++) {
		DIR *dir;
		struct dirent *dp;

	backtrack:
		if (stat(pathbuf, &sbuf) >= 0
		    && (dir = opendir(pathbuf)) != NULL) {
			if (cookie_stack[i] != 0)
				seekdir(dir, cookie_stack[i]);
			while ((dp = readdir(dir))) {
				if (strcmp(dp->d_name, ".") != 0
				    && strcmp(dp->d_name, "..") != 0) {
					psi = pseudo_inode(dp->d_ino, sbuf.st_dev);
					if (i == h->hash_path[0] + 1) {
						if (psi == h->psi) {
							/*GOT IT*/
							strcat(pathbuf, dp->d_name);
							path = xstrdup(pathbuf);
							closedir(dir);
							auth_override_uid(auth_uid);
							return (path);
						}
					} else {
						if (hash_psi(psi) == h->hash_path[i]) {
							/*PERHAPS WE'VE GOT IT */
							cookie_stack[i] = telldir(dir);
							cookie_stack[i + 1] = 0;
							slash_stack[i] = pathbuf + strlen(pathbuf);
							strcpy(slash_stack[i], dp->d_name);
							strcat(pathbuf, "/");

							closedir(dir);
							goto deeper;
						}
					}
				}
			}
			/* dp == NULL */
			closedir(dir);
		}
		/* shallower */
		i--;
		if (i < 2) {
			auth_override_uid(auth_uid);
			return (NULL);	/* SEARCH EXHAUSTED */
		}

		/* Prune path */
		*(slash_stack[i]) = '\0';
		goto backtrack;
	deeper:
		;
	}
	auth_override_uid(auth_uid);
	return (NULL);		/* actually not reached */
}

#else
/* This code is somewhat more readable (and safer) but doesn't work yet */
static int fh_buildcomp(h, dev, dir, i, path)
svc_fh *h;
dev_t dev;
DIR *dir;
int i;
char *path;
{
	struct dirent *	dp;
	psi_t		psi;
	int		len;

	while ((dp = readdir(dir))) {
		if (!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		psi = pseudo_inode(dp->d_ino, dev);
		if (i == h->hash_path[0] + 1) {
			if (psi != h->psi)
				continue;

			/*GOT IT*/
			len = strlen(path);
			if (len + strlen(dp->d_name) >= PATH_MAX + NAME_MAX)
				continue; /* shucks */

			strcat(path, dp->d_name);
			return 1;
		} else if (hash_psi(psi) == h->hash_path[i]) {
			/* PERHAPS WE'VE GOT IT */

			len = strlen(path);
			if (len + strlen(dp->d_name) + 1 >= PATH_MAX + NAME_MAX)
				continue;

			strcpy(path + len, dp->d_name);
			strcpy(path + len, "/");
			return 1;
		}
	}
	return 0;
}

static char *
fh_buildpath(h)
svc_fh *h;
{
	char		pathbuf[PATH_MAX + NAME_MAX + 1], *path;
	long		cookie_stack[HP_LEN + 1];
	char		*slash_stack[HP_LEN];
	struct stat	sbuf;
	psi_t		psi;
	int		i;

	if (h->hash_path[0] >= HP_LEN) {
		Dprintf(L_ERROR, "impossible hash_path[0] value: %s\n", 
					fh_dump(h));
		return NULL;
	}

	if (stat("/", &sbuf) < 0)
		return (NULL);
	psi = pseudo_inode(sbuf.st_ino, sbuf.st_dev);

	if (h->hash_path[0] == 0) {
		if (psi != h->psi)
			return (NULL);
		return xstrdup("/");
	}
	if (hash_psi(psi) != h->hash_path[1])
		return (NULL);

	auth_override_uid(ROOT_UID);
	strcpy(pathbuf, "/");
	i = 2;
	cookie_stack[i] = 0;
	while (i <= h->hash_path[0] + 1) {
		DIR *dir;

		if (stat(pathbuf, &sbuf) >= 0
		    && (dir = opendir(pathbuf)) != NULL) {
			if (cookie_stack[i] != 0)
				seekdir(dir, cookie_stack[i]);
			if (!fh_buildcomp(h, sbuf.st_dev, dir, i, pathbuf)) {
				closedir(dir);
				goto shallower;
			}
			if (i != h->hash_path[0] + 1) {
				/* more components to go */
				slash_stack[i] = pathbuf + strlen(pathbuf);
				cookie_stack[i] = telldir(dir);
				cookie_stack[i + 1] = 0;
				closedir(dir);
				i++;
				continue;
			}
			path = xstrdup(pathbuf);
			closedir(dir);
			auth_override_uid(auth_uid);
			return (path);
		}
	shallower:
		if (--i < 2)
			break;
		/* Prune path */
		*(slash_stack[i]) = '\0';
	}
	auth_override_uid(auth_uid);
	return (NULL);
}
#endif

static psi_t path_psi(path, status, sbp, tsbp)
char *path;
nfsstat *status;
struct stat *sbp;
struct stat *tsbp;
{
	struct stat sbuf;

	if (sbp == NULL)
		sbp = &sbuf;
	if (lstat(path, sbp) < 0) {
		*status = nfs_errno();
		return (0);
	}
	if (tsbp)
		*tsbp = *sbp;
	if (S_ISDIR(sbp->st_mode) && strcmp(path, "/") != 0) {
		/* Special case for directories--test for mount point. */
		struct stat ddbuf;
		char *sindx;
		char *fname;
		char squirrel;

		/* Find start of last component of path. */
		if ((sindx = strrchr(path, '/')) == path) {
			sindx++;
			fname = sindx;
		} else
			fname = sindx + 1;

		/* Remove last element of path. */
		squirrel = *sindx;
		*sindx = '\0';
		if (lstat(path, &ddbuf) < 0) {
			*sindx = squirrel;
			*status = nfs_errno();
			return (0);
		}
		/* Sindx now points to directory entry name. */
		if (ddbuf.st_dev != sbp->st_dev) {
			/* Directory is a mount point. */
			DIR *dirp;
			struct dirent *dp;

			errno = 0;
			if ((dirp = opendir(path)) == NULL) {
				*sindx = squirrel;	/* restore path */
				if (errno == EACCES)
					goto unreadable;
				if (errno != 0)
					*status = nfs_errno();
				else
					*status = NFSERR_NOENT;
			} else {
				*sindx = squirrel;	/* restore path */
				*status = NFS_OK;
				do {
					if ((dp = readdir(dirp)) == NULL) {
						*status = NFSERR_NOENT;
						closedir(dirp);
						return (0);
					}
				} while (strcmp(fname, dp->d_name) != 0);
				sbp->st_dev = ddbuf.st_dev;
				sbp->st_ino = dp->d_ino;
				closedir(dirp);
			}
		} else
			*sindx = squirrel;	/* restore path */
	unreadable:
		;
	}
	return (pseudo_inode(sbp->st_ino, sbp->st_dev));
}

fhcache *fh_find(h, mode)
svc_fh *h;
int mode;
{
	char buff[1024], *sp;
	register fhcache *fhc, *flush;

#ifdef FHTRACE
	if (h->hash_path[0] >= HP_LEN) {
		Dprintf(L_ERROR, "stale fh detected: %s\n", fh_dump(h));
		return NULL;
	}
#endif

	sprintf(buff, "fh_find: psi=%lx... ", (unsigned long) h->psi);
	sp = buff + strlen(buff);
	ex_state = active;
	time(&curtime);
	while ((fhc = fh_lookup(h->psi)) != NULL) {
		Dprintf(D_FHCACHE, "%s found '%s', fd=%d\n", buff,
			fhc->path ? fhc->path : "<unnamed>",
			fhc->fd);

		/* But what if hash_paths are not the same? Something is stale. */
		if (memcmp(h->hash_path, fhc->h.hash_path, HP_LEN) != 0) {
			Dprintf(D_FHTRACE, "fh_find: stale fh (path mismatch)\n");
#ifdef FHTRACE
			Dprintf(D_FHTRACE, "\tdata: %s\n", fh_dump(h));
#endif
			if (mode != FHFIND_FCREATE && mode != FHFIND_FEXISTS) {
				ex_state = inactive;
				return (NULL);
			}
			fh_delete(fhc);
			Dprintf(D_FHCACHE,
				"fh_find: deleted old handle... sortof\n");
			break;
		}
		if (fhc != fh_head.next)
			fh_move_to_front(fhc);
		fhc->last_used = curtime;
		ex_state = inactive;
		return (fhc);
	}

	Dprintf(D_FHCACHE, "%s not found.\n", buff);
	if (mode == FHFIND_FCACHED) {
		ex_state = inactive;
		return NULL;
	}

	for (flush = fh_tail.prev; fh_list_size > FH_CACHE_LIMIT; flush = fhc) {
		/* Don't flush current head. */
		if (flush == &fh_head)
			break;
		fhc = flush->prev;
		fh_delete(flush);
	}
	fhc = (fhcache *) xmalloc(sizeof *fhc);
	if (mode == FHFIND_FCREATE) {
		/* File will be created */
		fhc->path = NULL;
	} else {
		/* File must exist. Attempt to construct from hash_path */
		char *path;

		if ((path = fh_buildpath(h)) == NULL) {
#ifdef FHTRACE
			Dprintf(D_FHTRACE, "fh_find: stale fh (hash path)\n");
			Dprintf(D_FHTRACE, "\tdata: %s\n", fh_dump(h));
#endif
			free(fhc);
			ex_state = inactive;
			return NULL;
		}
		fhc->path = path;
	}
	fhc->flags = 0;
	if (fhc->path) {
		struct stat	stb;

		if (stat(fhc->path, &stb) >= 0) {
			if (re_export && nfsmounted(fhc->path, &stb))
				fhc->flags |= FHC_NFSMOUNTED;
		}
	}
	fhc->fd = -1;
	fhc->last_used = curtime;
	fhc->h = *h;
	fhc->last_clnt = NULL;
	fhc->last_mount = NULL;
	fhc->last_uid = (uid_t)-1;
	fhc->fd_next = fhc->fd_prev = NULL;
	fh_inserthead(fhc);
	Dprintf(D_FHCACHE,
		"fh_find: created new handle %x ('%s')\n",
		fhc, fhc->path ? fhc->path : "<unnamed>");
	ex_state = inactive;
	if (fh_list_size > FH_CACHE_LIMIT)
		flush_cache(0);
#ifdef FHTRACE
	if (fhc->h.hash_path[0] == 0xFF) {
		Dprintf(L_ERROR, "newly created fh instantly flushed?!");
		return NULL;
	}
#endif
	return (fhc);
}

/*
 * This function is usually called from the debugging code, where
 * the user has not been authenticated yet. Hence, no path lookups.
 */
char *fh_pr(fh)
nfs_fh *fh;
{
	fhcache *h;

	if ((h = fh_find((svc_fh *) fh, FHFIND_FCACHED)) == NULL)
		return fh_dump((svc_fh *) fh);
	return (h->path);
}

static char *fh_dump(fh)
svc_fh *fh;
{
	static char	buf[65];
	char		*sp;
	int		i, n = fh->hash_path[0];

	sprintf(buf, "%08x %02x ", fh->psi, fh->hash_path[0]);
	for (i = 1, sp = buf + 12; i <= n && i < HP_LEN; i++, sp += 2)
		sprintf(sp, "%02x", fh->hash_path[i]);
	return buf;
}

/*
 * This routine is only used by the mount daemon.
 * It creates the initial file handle.
 */
int fh_create(fh, path)
nfs_fh *fh;
char *path;
{
	svc_fh	key;
	fhcache	*h;
	psi_t	psi;
	nfsstat	status;
	char	*s;

	memset(&key, 0, sizeof(key));
	status = NFS_OK;
	if ((psi = path_psi("/", &status, NULL, NULL)) == 0)
		return ((int) status);
	s = path;
	while ((s = strchr(s + 1, '/')) != NULL) {
		if (++(key.hash_path[0]) >= HP_LEN)
			return ((int) NFSERR_NAMETOOLONG);
		key.hash_path[key.hash_path[0]] = hash_psi(psi);
		*s = '\0';
		if ((psi = path_psi(path, &status, NULL, NULL)) == 0)
			return ((int) status);
		*s = '/';
	}
	if (*(strrchr(path, '/') + 1) != '\0') {
		if (++(key.hash_path[0]) >= HP_LEN)
			return ((int) NFSERR_NAMETOOLONG);
		key.hash_path[key.hash_path[0]] = hash_psi(psi);
		if ((psi = path_psi(path, &status, NULL, NULL)) == 0)
			return ((int) status);
	}
	key.psi = psi;
	h = fh_find(&key, FHFIND_FCREATE);

#ifdef FHTRACE
	if (!h)
		return NFSERR_STALE;
#endif

	/* assert(h != NULL); */
	if (h->path == NULL) {
		h->fd = -1;
		h->path = xstrdup(path);
		h->flags = 0;
	}
	memcpy(fh, &key, sizeof(key));
	return ((int) status);
}

char *fh_path(fh, status)
nfs_fh *fh;
nfsstat *status;
{
	fhcache *h;

	if ((h = fh_find((svc_fh *) fh, FHFIND_FEXISTS)) == NULL) {
		*status = NFSERR_STALE;
		return (NULL);
	}
	*status = NFS_OK;
	return (h->path);
}

nfs_fh *fh_handle(h)
fhcache *h;
{
	return ((nfs_fh*)&(h->h));
}

int path_open(path, omode, perm)
char *path;
int omode;
int perm;
{
	int fd;
	int oerrno, ok;
	struct stat buf;

	fh_flush_fds();

	/* If the file exists, make sure it is a regular file. Opening
	 * device files might hang the server. There's still a tiny window
	 * here, but it's not very likely someone's able to exploit
	 * this.
	 */
	if ((ok = (lstat(path, &buf) >= 0)) && !S_ISREG(buf.st_mode)) {
		errno = EISDIR;	/* emulate SunOS server */
		return -1;
	}

#if 1
	fd = open(path, omode, perm);
#else
	/* First, try to open the file read/write. The O_*ONLY flags ored
	 * together do not yield O_RDWR, unfortunately. 
	 * Backed out for now; we have to record the new omode in
	 * h->omode to be effective, anyway.
	 */
	fd = open(path, (omode & ~O_ACCMODE)|O_RDWR, perm);
	if (fd < 0)
		fd = open(path, omode, perm);
#endif

	oerrno = errno;

	/* The file must exist at this point. */
	if (!ok && lstat(path, &buf) < 0) {
		/*
		Dprintf(L_ERROR,
			"path_open(%s, %o, %o): failure mode 1, err=%d\n",
			path, omode, perm, errno);
		 */
		errno = oerrno;
		return -1;
	}

	/* Do some serious cheating for statelessness. The following accomp-
	 * lishes two things: first, it gives the file owner r/w access to
	 * the file whatever the permissions are, so that files are still
	 * accessible after an fchown(fd, 0). The second part of the
	 * condition allows read access to mode 0111 executables.
	 *
	 * The old conditon read like this:
	 * if (fd < 0 && oerrno == EACCES) {
	 *	if (oerrno == EACCES && (buf.st_uid == auth_uid
	 *	    || (omode == O_RDONLY && (buf.st_mode & S_IXOTH)))) {
	 *		override uid; etc...
	 *	}
	 * }
	 * This would truncate read-only files on creat() calls. Now
	 * ftruncate(fd, 0) should still be legal for the user when the
	 * file was chmoded *after* opening it, but we have no way to tell,
	 * and a semi-succeding `cp foo readonly-file' is much more
	 * unintuitive and destructive than a failing ftruncate().
	 */
	if (fd < 0 && oerrno == EACCES && !(omode & (O_CREAT|O_TRUNC))) {
		if ((buf.st_uid == auth_uid && (omode & O_ACCMODE) == omode)
		 || ((buf.st_mode & S_IXOTH) && omode == O_RDONLY)) {
			auth_override_uid(ROOT_UID);
			fd = open(path, omode, perm);
			oerrno = errno;
			auth_override_uid(auth_uid);
		}
	}

	if (fd < 0) {
		Dprintf(D_FHCACHE,
			"path_open(%s, %o, %o): failure mode 2, err=%d, oerr=%d\n",
			path, omode, perm, errno, oerrno);
		errno = oerrno;
		return -1;
	}

	errno = oerrno;
	return (fd);
}

int fh_fd(h, status, omode)
fhcache *h;
nfsstat *status;
int omode;
{
	int	retry;

	if (h->fd >= 0) {
		/* If the requester's uid doesn't match that of the user who
		 * opened the file, we close the file. I guess we could work
		 * some magic with the eaccess stuff, but I don't know if
		 * this would be any faster than simply re-doing the open.
		 */
		if (h->last_uid == auth_uid && (h->omode == omode ||
		    ((omode == O_RDONLY || omode == O_WRONLY) && h->omode == O_RDWR))) {
			Dprintf(D_FHCACHE, "fh_fd: reusing fd=%d\n", h->fd);
			fh_insert_fdcache(h);	/* move to front of fd LRU */
			return (h->fd);
		}
		Dprintf(D_FHCACHE,
		    "fh_fd: uid/omode mismatch (%d/%d wanted, %d/%d cached)\n",
		     auth_uid, omode, h->last_uid, h->omode);
		fh_close(h);
	}
	errno = 0;
	if (!h->path) {
		*status = NFSERR_STALE;
		return (-1);	/* something is really hosed */
	}
	for (retry = 0; retry < 2; retry++) {
		if ((h->fd = path_open(h->path, omode, 0)) >= 0) {
			io_state = active;
			h->omode = omode & O_ACCMODE;
			fh_insert_fdcache(h);
			Dprintf(D_FHCACHE, "fh_fd: new open as fd=%d\n", h->fd);
			h->last_uid = auth_uid;
			return (h->fd);
		} 
		Dprintf(D_FHCACHE, "fh_fd: open failed.\n");
		*status = nfs_errno();
		if (errno != ENOENT)
			return -1;

		/* maybe the cached path is stale */
		free(h->path);
		h->path = NULL;
		if (!retry && !fh_buildpath(&h->h))
			break;
	}
	return -1;
}

void fd_inactive(fd)
int fd;
{
	io_state = inactive;
}

nfsstat fh_compose(dopa, new_fh, sbpp, fd, omode)
diropargs *dopa;
nfs_fh *new_fh;
struct stat **sbpp;
int fd;
int omode;
{
	svc_fh *key;
	fhcache *dirh, *h;
	char *sindx;
	int is_dd;
	nfsstat ret;
	struct stat tsbuf;
	char pathbuf[PATH_MAX + NAME_MAX + 1];

	if ((dirh = fh_find((svc_fh *) & (dopa->dir), FHFIND_FEXISTS)) == NULL)
		return (NFSERR_STALE);

	/* This allows only single directories to be looked up, could be
	   a bit more sophisticated, but i don't know if that is neccesary */
	if (strchr(dopa->name, '/') != NULL)
		return(NFSERR_ACCES);

	/* Construct path.
	 * Lookups of "" generated by broken OS/2 clients
	 */
	if (strcmp(dopa->name, ".") == 0 || dopa->name[0] == '\0') {
		*new_fh = dopa->dir;
		*sbpp = NULL;
		return (NFS_OK);
	}
	if (strcmp(dopa->name, "..") == 0) {
		is_dd = 1;
		sindx = strrchr(dirh->path, '/');
		if (sindx == dirh->path)
			strcpy(pathbuf, "/");
		else {
			int len = sindx - dirh->path;
			strncpy(pathbuf, dirh->path, len);
			pathbuf[len] = '\0';
		}
	}
	else if (!re_export && (dirh->flags & FHC_NFSMOUNTED))
		return (NFSERR_NOENT);
	else {
		int len = strlen(dirh->path);

		is_dd = 0;
		if (dirh->path[len - 1] == '/')
			len--;
		strncpy(pathbuf, dirh->path, len);
		pathbuf[len] = '/';
		strcpy(pathbuf + (len + 1), dopa->name);
	}

	*new_fh = dopa->dir;
	key = (svc_fh *) new_fh;
	if ((key->psi = path_psi(pathbuf, &ret, *sbpp, &tsbuf)) == 0)
		return (ret);

	if (is_dd) {
		/* Don't cd .. from root, or mysterious ailments will
		 * befall your fh cache... Fixed. */
		if (key->hash_path[0] > 0)
			key->hash_path[key->hash_path[0]--] = 0;
	} else {
		if (++(key->hash_path[0]) >= HP_LEN)
			return (NFSERR_NAMETOOLONG);
		key->hash_path[key->hash_path[0]] = hash_psi(dirh->h.psi);
	}
	h = fh_find(key, FHFIND_FCREATE);

#ifdef FHTRACE
	if (h == NULL)
		return NFSERR_STALE;
	if (h->h.hash_path[0] >= HP_LEN) {
		Dprintf(L_ERROR, "fh cache corrupted! file %s hplen %02x",
					h->path? h->path : "<unnamed>",
					h->h.hash_path[0]);
		return NFSERR_STALE;
	}
#endif

	/* New code added by Don Becker */
	if ((h->path != NULL) && (strcmp(h->path, pathbuf) != 0)) {
		/* We must have cached an old file under the same inode # */
		Dprintf(D_FHTRACE, "Disposing of fh with bad path.\n");
		fh_delete(h);
		h = fh_find(key, FHFIND_FCREATE);
#ifdef FHTRACE
		if (!h) return NFSERR_STALE;
#endif
		if (h->path)
			Dprintf(L_ERROR, "Internal inconsistency: double entry (path '%s', now '%s').\n",
				h->path, pathbuf);
	}
	Dprintf(D_FHCACHE, "fh_compose: using  handle %x ('%s', fd=%d)\n",
		h, h->path ? h->path : "<unnamed>", h->fd);
	/* End of new code */

	/* assert(h != NULL); */
	if (h->path == 0) {
		h->path = xstrdup(pathbuf);
		h->flags = 0;
		if (!re_export && nfsmounted(pathbuf, &tsbuf))
			h->flags |= FHC_NFSMOUNTED;
#ifdef FHTRACE
		Dprintf(D_FHTRACE, "fh_compose: created handle %s\n", h->path);
		Dprintf(D_FHTRACE, "\tdata: %s\n", fh_dump(&h->h));
#else
		Dprintf(D_FHCACHE,
			"fh_compose: +using  handle %x ('%s', fd=%d)\n",
			h, h->path, h->fd);
#endif
	}

	if (fd >= 0) {
		Dprintf(D_FHCACHE,
			"fh_compose: handle %x using passed fd %d\n", h, fd);
		if (h->fd >= 0)
			fh_close(h);
		h->fd = fd;
		fh_insert_fdcache(h);
		Dprintf(D_FHCACHE,
			"fh_compose: +using  handle %x ('%s', fd=%d)\n",
			h, h->path ? h->path : "<unnamed>", h->fd);
	}
	if (omode >= 0)
		h->omode = omode & O_ACCMODE;
	return (NFS_OK);
}

psi_t fh_psi(fh)
nfs_fh *fh;
{
	svc_fh *h = (svc_fh *) fh;
	return (h->psi);
}

void fh_remove(path)
char *path;
{
	psi_t	psi;
	nfsstat status;
	fhcache *fhc;

	psi = path_psi(path, &status, NULL, NULL);
	if (psi == 0)
		return;
	ex_state = active;
	fhc = fh_lookup(psi);
	if (fhc != NULL)
		fh_delete(fhc);

	ex_state = inactive;
	return;
}

/*
 * Close a file to make an fd available for a new file.
 */
static int fh_flush_fds()
{
	if (io_state == active) {
		Dprintf(D_FHCACHE, "fh_flush_fds: not flushing... io active\n");
		return (-1);
	}
	while (fd_cache_size >= FD_CACHE_LIMIT)
		fh_close(fd_lru_tail);
	return (0);
}

/*
 * fh_flush() is invoked periodically from SIGALRM, and on
 * demand from fh_find.  A simple form of mutual exclusion
 * protects this routine from multiple concurrent executions.
 * Since the preemption that occurs when a signal is received
 * is one-sided, we do need an atomic test and set.  If the
 * signal arrives between the test and the set, the first
 * invocation safely stalls until the signal-caused invocation
 * completes.
 *
 * NOTE: fh_flush is now always called from the top RPC dispatch
 * routine, and the ex_state stuff is likely to go when this proves
 * to work.
 */
void fh_flush(force)
int force;
{
	register fhcache *h;

#ifdef DEBUG
	time_t now;
	time(&now);
	Dprintf(D_FHTRACE, "flushing cache at %s: state = %s\n",
		ctime(&now), (ex_state == inactive) ? "inactive" : "active");
#endif

	if (ex_state == inactive) {
		int cache_size = 0;

		ex_state = active;
		time(&curtime);
		/* Single execution thread */

		/* works in empty case because: fh_tail.next = &fh_tail */
		h = fh_head.next;
		while (h != &fh_tail) {
			if (cache_size > FH_CACHE_LIMIT
			    || curtime > h->last_used + DISCARD_INTERVAL
			    || force) {
				h = h->next;
				fh_delete(h->prev);
			} else {
				if (h->fd >= 0 &&
				    curtime > h->last_used + CLOSE_INTERVAL)
					fh_close(h);
				cache_size++;
				h = h->next;
			}
		}
		if (fh_list_size != cache_size)
			Dprintf(L_ERROR,
				"internal inconsistency (fh_list_size=%d) != (cache_size=%d)\n",
				fh_list_size, cache_size);
		fh_list_size = cache_size;
		ex_state = inactive;
	}
}

RETSIGTYPE flush_cache(sig)
int sig;
{
	static volatile int	inprogress = 0;

	signal (SIGALRM, flush_cache);
	if (_rpcsvcdirty) {
		alarm(BUSY_RETRY_INTERVAL);
		need_flush = 1;
		return;
	}
	if (inprogress++)
		return;
	fh_flush(0);
	if (_rpcpmstart)
		rpc_closedown();
	inprogress = 0;
	need_flush = 0;
	alarm(FLUSH_INTERVAL);
}

void fh_init()
{
	static int	initialized = 0;

	if (initialized)
		return;
	initialized = 1;

	fh_head.next = fh_tail.next = &fh_tail;
	fh_head.prev = fh_tail.prev = &fh_head;
	/* last_flushable = &fh_tail; */

	signal(SIGALRM, flush_cache);
	alarm(FLUSH_INTERVAL);

	umask(0);
}

