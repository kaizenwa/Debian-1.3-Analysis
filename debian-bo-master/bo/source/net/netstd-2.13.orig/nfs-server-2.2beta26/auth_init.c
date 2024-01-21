/*
 * auth_init.c	This module takes care of request authorization.
 *
 * Authors:	Donald J. Becker, <becker@super.org>
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Olaf Kirch, <okir@monad.swb.de>
 *		Alexander O. Yuriev, <alex@bach.cis.temple.edu>
 *
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */

#include "nfsd.h"
#include "fakefsuid.h"
#include <pwd.h>

#define LINE_SIZE	1024
#define CHUNK_SIZE	1024	/* the 'typical' maximum line length	*/

#ifndef EXPORTSFILE
#define EXPORTSFILE	"/etc/exports"
#endif

/* Support for file access control on /etc/exports by Alex Yuriev. */
#include "faccess.h"
#ifndef EXPORTSOWNERUID
#define EXPORTSOWNERUID		((uid_t) 0)
#endif
#ifndef EXPORTSOWNERGID
#define EXPORTSOWNERGID		((gid_t) 0)
#endif

exportnode *export_list = NULL;
int		allow_non_root = 0;
int		promiscuous = 0;
int		re_export = 0;
int		trace_spoof = 1;
int		auth_initialized = 0;
int		have_setfsuid = 0;

static _PRO(int  filt_getc,    (FILE *)					);
static _PRO(int  getline,      (char **, FILE *)			);
static _PRO(char *parse_opts,  (char *, char, nfs_mount *, char *)	);
static _PRO(void parse_squash, (nfs_mount *mp, int uidflag, char **cpp)	);
static _PRO(int  parse_num,    (char **cpp)				);
static _PRO(void free_exports, (void)					);
#if 0
static _PRO(char *h_strerror, (int));
#endif

static int filt_getc(f)
FILE *f;
{
	int c;

	c = getc(f);
	if (c == '\\') {
		c = getc(f);
		if (c == '\n')
			return (' ');
		if (c != EOF)
			ungetc(c, f);
		return ('\\');
	} else if (c == '#') {
		int lastc = c;
		while ((c = getc(f)) != '\n' && c != EOF)
			lastc = c;
		if (c == '\n' && lastc == '\\')
			c = getc(f);
	}
	return (c);
}

static int getline(lbuf, f)
char **lbuf;
FILE *f;
{
	register c;
	register char *p;
	char *buf;
	int sz = CHUNK_SIZE;

	buf = (char *) xmalloc(sz);
	p = buf;
	while ((c = filt_getc(f)) != '\n' && c != EOF) {
		if (p - buf == sz - 2) {
			buf = (char *) xrealloc(buf, sz * 2);
			p = buf + sz - 2;
			sz *= 2;
		}
		*p++ = c;
	}
	if (c == EOF && p == buf) {
		free(buf);
		*lbuf = NULL;
		return (0);
	}
	*p++ = '\0';
	*lbuf = buf;
	return (1);
}

/*
 * Parse number.
 */
static int parse_num(cpp)
char	**cpp;
{
	char	*cp = *cpp, c;
	int	num = 0;

	if (**cpp == '-')
		(*cpp)++;
	while (isdigit(**cpp))
		(*cpp)++;
	c = **cpp; **cpp = '\0'; num = atoi(cp); **cpp = c;
	return num;
}

/*
 * Install uid mapping records for a specified list of uids/gids.
 * We first map these ids to the compile-time value AUTH_[UG]ID_NOBODY,
 * which is later (in luid/lgid) mapped to the current nobody_uid/nobody_gid.
 * We take these contortions because users may define option lists like this:
 *
 *	/foo		foo.bar.edu(squash=0-20,anonuid=32767)
 *
 * In this example, the squash list is parsed before we know what the anonuid
 * really is.
 */
static void parse_squash(nfs_mount *mp, int uidflag, char **cpp)
{
	char	*cp = *cpp;
	int	id0, id1;

	do {
		id0 = parse_num(&cp);
		if (*cp == '-') {
			cp++;
			id1 = parse_num(&cp);
		} else {
			id1 = id0;
		}
		if (uidflag) {
			while (id0 <= id1)
				ugid_map_uid(mp, id0++, AUTH_UID_NOBODY);
		} else {
			while (id0 <= id1)
				ugid_map_gid(mp, id0++, AUTH_GID_NOBODY);
		}
		if (*cp != ',' || !isdigit(cp[1]))
			break;
		cp++;
	} while(1);
	*cpp = cp;
}

/*
 * Parse option string pointed to by s and set mount options accordingly.
 */
static char *parse_opts(cp, terminator, mp, client_name)
char *cp;
char terminator;
nfs_mount *mp;
char *client_name;
{
	char *kwd;
	int  klen;

	/* skip white */
	while (isspace(*cp))
		cp++;
	while (*cp != terminator) {
		kwd = cp;
		while (isalpha(*cp) || *cp == '_' || *cp == '=')
			cp++;
		klen = cp - kwd;

		/* process keyword */
		if (strncmp(kwd, "secure", 6) == 0)
			mp->o.secure_port = 1;
		else if (strncmp(kwd, "insecure", 8) == 0)
			mp->o.secure_port = 0;
		else if (strncmp(kwd, "root_squash", 11) == 0)
			mp->o.root_squash = 1;
		else if (strncmp(kwd, "no_root_squash", 14) == 0)
			mp->o.root_squash = 0;
		else if (strncmp(kwd, "ro", 2) == 0)
			mp->o.read_only = 1;
		else if (strncmp(kwd, "rw", 2) == 0)
			mp->o.read_only = 0;
		else if (strncmp(kwd, "link_relative", 13) == 0)
			mp->o.link_relative = 1;
		else if (strncmp(kwd, "link_absolute", 13) == 0)
			mp->o.link_relative = 0;
		else if (strncmp(kwd, "map_daemon", 10) == 0)
			mp->o.uidmap = map_daemon;
		else if (strncmp(kwd, "map_identity", 12) == 0)
			mp->o.uidmap = identity;
		else if (strncmp(kwd, "all_squash", 10) == 0)
			mp->o.all_squash = 1;
		else if (strncmp(kwd, "no_all_squash", 13) == 0)
			mp->o.all_squash = 0;
		else if (strncmp(kwd, "noaccess", 8) == 0)
			mp->o.noaccess = 1;
		else if (strncmp(kwd, "squash_uids=", 12) == 0)
			parse_squash(mp, 1, &cp);
		else if (strncmp(kwd, "squash_gids=", 12) == 0)
			parse_squash(mp, 0, &cp);
		else if (strncmp(kwd, "anonuid=", 8) == 0)
			mp->o.nobody_uid = parse_num(&cp);
		else if (strncmp(kwd, "anongid=", 8) == 0)
			mp->o.nobody_gid = parse_num(&cp);
		else if (strncmp(kwd, "async", 5) == 0)
			/* knfsd compatibility, ignore */;
		else if (strncmp(kwd, "sync", 4) == 0)
			/* knfsd compatibility, ignore */;
		else {
			Dprintf(L_ERROR,
				"Unknown keyword \"%.*s\" in export file\n",
				klen, kwd);
			mp->o.all_squash = 1;
			mp->o.read_only = 1;
		}
		while (isspace(*cp))
			cp++;
		if (*cp == terminator)
			break;
		if (*cp == ',')
			cp++;
		else if (!isalpha(*cp) && *cp != '_' && *cp != '\0') {
			if (client_name == NULL)
				Dprintf(L_ERROR,
					"Comma expected in opt list for dflt clnt (found '%c')\n", *cp);
			else
				Dprintf(L_ERROR,
					"Comma expected in opt list for clnt %s (found '%c')\n",
					client_name, *cp);
		}
		while (isspace(*cp))
			cp++;

		if (*cp == '\0' && *cp != terminator) {
			Dprintf(L_ERROR,
				"missing terminator \"%c\" on option list\n",
				terminator);
			return (cp);
		}
	}
	if (*cp != terminator)
		Dprintf(L_ERROR, "Trouble in parser, character '%c'.\n", *cp);

	cp++;			/* Skip past terminator */
	while (isspace(*cp))
		cp++;
	return (cp);
}

static nfs_client *get_client(hname)
char *hname;
{
	nfs_client *cp;

	if (hname && *hname == '\0')
		hname = NULL;
	if ((cp = auth_get_client(hname)) == NULL)
		cp = auth_create_client(hname, NULL);

	return cp;
}

void auth_init(fname)
char *fname;
{
	exportnode	*resex;		/* export data for showmount -x */
	groupnode	*resgr;
	static char	*auth_file = NULL;
	FILE		*ef;
	char		*cp;		/* Current line position */
	char		*sp;		/* Secondary pointer */
	char		*fs_name;
	char		path[PATH_MAX];
	char		resolved_path[PATH_MAX];

	if (auth_initialized) {
		free_exports();
		fname = auth_file;
	}

	auth_init_lists();

	if (fname == NULL)
		fname = EXPORTSFILE;
	auth_file = fname;	/* Save for re-initialization */

	/* Check protection of exports file. */
	switch(iCheckAccess(auth_file, EXPORTSOWNERUID, EXPORTSOWNERGID)) {
	case FACCESSWRITABLE:
		Dprintf(L_ERROR,
			"SECURITY: A user with uid != %d can write to %s!\n",
				EXPORTSOWNERUID, fname);
		Dprintf(L_ERROR, "exiting because of security violation.\n");
		exit(1);
	case FACCESSBADOWNER:
		Dprintf(L_ERROR,
			"SECURITY: File %s not owned by uid %d/gid %d!\n",
				fname, EXPORTSOWNERUID, EXPORTSOWNERGID);
		Dprintf(L_ERROR, "exiting because of security violation.\n");
		exit(1);
	}

	if ((ef = fopen(fname, "r")) == NULL) {
		Dprintf(L_ERROR, "Could not open exports file %s: %s\n",
			fname, strerror(errno));
		exit(1);
	}
	while (getline(&cp, ef)) {
		char *saved_line = cp;	/* For free()ing it later. */
		char *mount_point, *host_name, cc;
		nfs_client *clnt;
		nfs_mount *mnt;

		while (isspace(*cp))
			cp++;

		/* Check for "empty" lines. */
		if (*cp == '\0')
			continue;

		/* Get the file-system name. */
		fs_name = cp;
		while (*cp != '\0' && !isspace(*cp))
			cp++;
		for (sp = path; fs_name < cp;)
			*sp++ = *fs_name++;
		*sp = '\0';

		/* Make sure it's symlink-free, if possible. */
		if (realpath(path, resolved_path) == NULL)
			strcpy(resolved_path, path);

		/* Copy it into a new string. */
		mount_point = xstrdup(resolved_path);

		/* Build the RPC mount export list data structure. */
		resex = (exportnode *) xmalloc(sizeof *resex);
		resex->ex_dir = mount_point;
		resex->ex_groups = NULL;

		while (isspace(*cp))
			cp++;

#ifndef NEW_STYLE_EXPORTS_FILE
		/* Special case for anononymous NFS. */
		if (*cp == '\0') {
			clnt = get_client(NULL);
			mnt = auth_add_mount(clnt, mount_point);
		}
		while (*cp != '\0') {
			host_name = cp;

			/* Host name. */
			while (*cp != '\0' && !isspace(*cp) && *cp != '(')
				cp++;
			cc = *cp; *cp = '\0';
			clnt = get_client(host_name);
			*cp = cc;

			mnt = auth_add_mount(clnt, mount_point);

			/* Finish parsing options. */
			while (isspace(*cp))
				cp++;
			if (*cp == '(')
				cp = parse_opts(cp + 1, ')', mnt,
						clnt->clnt_name);

			/* For anon exports, resex->ex_groups should be
			 * NULL */
			if (!mnt->o.noaccess && clnt->clnt_name) {
				resgr = (groupnode *) xmalloc(sizeof(*resgr));
				resgr->gr_name = clnt->clnt_name;
				resgr->gr_next = resex->ex_groups;
				resex->ex_groups = resgr;
			}
		}
#endif
		resex->ex_next = export_list;
		export_list = resex;

		free(saved_line);
	}
	fclose(ef);

	if (promiscuous) {
		auth_create_default_client();
	}

	/* Finally, resolve any mount points for netgroup and wildcard
	 * hosts that apply to known hosts as well.
	 */
	auth_check_all_netmasks();
	auth_check_all_netgroups();
	auth_check_all_wildcards();

#if defined(MAYBE_HAVE_SETFSUID) && !defined(HAVE_SETFSUID)
	/* check if the a.out setfsuid syscall works on this machine */
	have_setfsuid = (setfsuid(0) >= 0);
#endif

	auth_initialized = 1;
}

/* 
 * Clear the export list.
 */
static void
free_exports()
{
	exportnode	*ex, *nex;
	groupnode	*gr, *ngr;

	for (ex = export_list; ex != NULL; ex = nex) {
		nex = ex->ex_next;
		free (ex->ex_dir);
		for (gr = ex->ex_groups; gr != NULL; gr = ngr) {
			ngr = gr->gr_next;
			/* gr->gr_name has already been freed in auth.c */
			free (gr);
		}
		free (ex);
	}
	export_list = NULL;
}

#if 0
static char *h_strerror(errnum)
int errnum;
{
	char *reason;

	switch (h_errno) {
#ifdef HOST_NOT_FOUND		/* Only on BSD 4.3 and compatible systems. */
	case HOST_NOT_FOUND:
		reason = "Authoritative -- the host exists only in your imagination.";
		break;
	case TRY_AGAIN:
		reason = "Non-Authoritative -- the host might exist.";
		break;
	case NO_RECOVERY:
		reason = "Non-recoverable error.";
		break;
	case NO_ADDRESS:
		reason = "Valid host name, but no address.";
		break;
#endif
	default:
		reason = "Unknown reason.";
	}
	return reason;
}
#endif
