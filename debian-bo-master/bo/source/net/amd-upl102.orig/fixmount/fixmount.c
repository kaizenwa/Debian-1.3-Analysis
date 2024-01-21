/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#if !defined(lint) && !defined(SABER)
static char *rcsid = "$Id: fixmount.c,v 1.29 1995/01/03 21:21:12 stolcke Exp $ ICSI (Berkeley)";
#endif

#include <stdio.h>
#include <string.h>
#include <sys/param.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <errno.h>
#ifdef sgi
#include <sys/fs/nfs.h>
#else
#include <nfs/nfs.h>
#endif
/* #include <rpcsvc/mount.h> */
#include "mount.h"

#define CREATE_TIMEOUT	2	/* seconds */
#define CALL_TIMEOUT	5	/* seconds */

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

#ifndef INADDR_NONE
#define INADDR_NONE	0xffffffff
#endif

/* Constant defs */
#define	ALL	1
#define	DIRS	2

#define	DODUMP		0x1
#define	DOEXPORTS	0x2
#define	DOREMOVE	0x4
#define	DOVERIFY	0x8
#define	DOREMALL	0x10

static char thishost[MAXHOSTNAMELEN] = "";
static struct in_addr thisaddr;
static mountlist mntdump;
static exports mntexports;
static int type = 0;

static CLIENT *clnt_create_timeout();

static int quiet = 0;

/*
 * This command queries the NFS mount daemon for it's mount list and/or
 * it's exports list and prints them out.
 * See "NFS: Network File System Protocol Specification, RFC1094, Appendix A"
 * for detailed information on the protocol.
 */
main(argc, argv)
	int argc;
	char **argv;
{
	exports exp;
	groups grp;
	extern char *optarg;
	extern int optind;
	register int rpcs = 0;
	int ch;
	char *host;
	CLIENT *client;
	AUTH *auth;
	enum clnt_stat estat;
	struct timeval tv;
	int morethanone;
	int force = 0;

	while ((ch = getopt(argc, argv, "adervAqfh:")) != EOF)
		switch((char)ch) {
		case 'a':
			if (type == 0) {
				type = ALL;
				rpcs |= DODUMP;
			} else
				usage();
			break;
		case 'd':
			if (type == 0) {
				type = DIRS;
				rpcs |= DODUMP;
			} else
				usage();
			break;
		case 'e':
			rpcs |= DOEXPORTS;
			break;
		case 'r':
			rpcs |= DOREMOVE;
			break;
		case 'A':
			rpcs |= DOREMALL;
			break;
		case 'v':
			rpcs |= DOVERIFY;
			break;
		case 'q':
			quiet = 1;
			break;
		case 'f':
			force = 1;
			break;
		case 'h':
			strncpy(thishost, optarg, sizeof(thishost));
			thishost[sizeof(thishost) - 1] = '\0';
			break;
		case '?':
		default:
			usage();
		}

	if (optind == argc)
		usage();

	if (rpcs == 0)
		rpcs = DODUMP;

	if (!*thishost) {
	 	struct hostent *he;

		if (gethostname(thishost, sizeof(thishost)) < 0) {
			perror("gethostname");
			exit(1);
		}

		/*
		 * We need the hostname as it appears to the other side's
		 * mountd, so get our own hostname by reverse address
		 * resolution.
		 */
		if (!(he = gethostbyname(thishost))) {
			fprintf(stderr, "gethostbyname failed on %s\n",
					thishost);
			exit (1);
		}

		memcpy(&thisaddr, he->h_addr, sizeof(thisaddr));
		if (!(he = gethostbyaddr((char *)&thisaddr, sizeof(thisaddr),
					 he->h_addrtype)))
		{
			fprintf(stderr, "gethostbyaddr failed on %s\n",
					inet_ntoa(thisaddr));
			exit(1);
		}

		strncpy(thishost, he->h_name, sizeof(thishost));
		thishost[sizeof(thishost) - 1] = '\0';
	} else {
		thisaddr.s_addr = INADDR_NONE;
	}

	if (!(auth = authunix_create_default())) {
		fprintf(stderr, "couldn't create authentication handle\n");
		exit(1);
	}

	morethanone = (optind + 1 < argc);

	for (; optind < argc; optind++) {

		host = argv[optind];
		tv.tv_sec = CREATE_TIMEOUT; tv.tv_usec = 0;

		if (!(client = clnt_create_timeout(host, &tv)))
			continue;
		
		client->cl_auth = auth;
		tv.tv_sec = CALL_TIMEOUT; tv.tv_usec = 0;

		if (rpcs & (DODUMP|DOREMOVE|DOVERIFY))
			if ((estat = clnt_call(client,
				MOUNTPROC_DUMP, xdr_void, (char *)0,
				xdr_mountlist, (char *)&mntdump, tv)) != RPC_SUCCESS) {
				fprintf(stderr, "%s: MOUNTPROC_DUMP: ", host);
				clnt_perrno(estat);
				fflush(stderr);
				mntdump = NULL;
				goto next;
			}
		if (rpcs & DOEXPORTS)
			if ((estat = clnt_call(client,
				MOUNTPROC_EXPORT, xdr_void, (char *)0,
				xdr_exports, (char *)&mntexports, tv)) != RPC_SUCCESS) {
				fprintf(stderr, "%s: MOUNTPROC_EXPORT: ", host);
				clnt_perrno(estat);
				fflush(stderr);
				mntexports = NULL;
				goto next;
			}

		/* Now just print out the results */
		if ((rpcs & (DODUMP|DOEXPORTS)) &&
		    morethanone) {
			printf(">>> %s <<<\n", host);
			fflush(stdout);
		}

		if (rpcs & DODUMP) {
			print_dump(mntdump);
		}
		if (rpcs & DOEXPORTS) {
			exp = mntexports;
			while (exp) {
				printf("%-35s", exp->ex_dir);
				grp = exp->ex_groups;
				if (grp == NULL) {
					printf("Everyone\n");
				} else {
					while (grp) {
						printf("%s ", grp->gr_name);
						grp = grp->gr_next;
					}
					printf("\n");
				}
				exp = exp->ex_next;
			}
		}
		if (rpcs & DOVERIFY)
			fix_rmtab(client, host, mntdump, 0, force);

		if (rpcs & DOREMOVE)
			fix_rmtab(client, host, mntdump, 1, force);

		if (rpcs & DOREMALL)
			remove_all(client, host);

	next:
		if (mntdump)
			(void)clnt_freeres(client, xdr_mountlist,
			                   (char *)&mntdump);
		if (mntexports)
			(void)clnt_freeres(client, xdr_exports,
					   (char *)&mntexports);

 		clnt_destroy(client);
	}
}

usage()
{
	fprintf(stderr, "usage: fixmount [-adervAqf] [-h hostname] host ...\n");
	exit(1);
}

static jmp_buf before_rpc;

void
static create_timeout()
{
	signal(SIGALRM, SIG_DFL);
	longjmp(before_rpc, 1);
}

static CLIENT *
clnt_create_timeout(host, to)
	char *host;
	struct timeval *to;
{
	CLIENT *client;

	if (setjmp(before_rpc)) {
		if (!quiet) {
			fprintf(stderr, "%s: ", host);
			clnt_perrno(RPC_TIMEDOUT);
			fprintf(stderr, "\n");
			fflush(stderr);
		}
		return NULL;
	}

	signal(SIGALRM, create_timeout);
	ualarm(to->tv_sec * 1000000 + to->tv_usec, 0);

	/*
	 * Try TCP first (in case return data is large), then UDP
	 */
	if (!(client = clnt_create(host, MOUNTPROG, MOUNTVERS, "tcp")) &&
	    !(client = clnt_create(host, MOUNTPROG, MOUNTVERS, "udp"))) {
		ualarm(0,0);
		if (!quiet) {
			clnt_pcreateerror(host);
			fflush(stderr);
		}
		return NULL;
	}

	ualarm(0,0);
	return client;
}
		
/*
 * Check hostname against other name and its IP address 
 */
static int
is_same_host(name1, name2, addr2)
	char *name1, *name2;
	struct in_addr addr2;
{
	if (strcasecmp(name1, name2) == 0) {
		return 1;
	} else if (addr2.s_addr == INADDR_NONE) {
		return 0;
	} else {
		static char lasthost[MAXHOSTNAMELEN] = "";
		static struct in_addr addr1;
		struct hostent *he;

		/*
		 * To save nameserver lookups, and because this function
		 * is typically called repeatedly on the same names,
		 * cache the last lookup result and reuse it if possible.
		 */
		if (strcasecmp(name1, lasthost) == 0) {
			return (addr1.s_addr == addr2.s_addr);
		} else if (!(he = gethostbyname(name1))) {
			return 0;
		} else {
			strncpy(lasthost, name1, sizeof(lasthost)-1);
			memcpy(&addr1, he->h_addr, sizeof(addr1));
			return (addr1.s_addr == addr2.s_addr);
		}
	}
}

/*
 * Print the binary tree in inorder so that output is sorted.
 */
print_dump(mp)
	mountlist mp;
{

	if (mp == NULL)
		return;
	if (is_same_host(mp->ml_hostname, thishost, thisaddr)) {
		switch (type) {
		case ALL:
			printf("%s:%s\n", mp->ml_hostname, mp->ml_directory);
			break;
		case DIRS:
			printf("%s\n", mp->ml_directory);
			break;
		default:
				printf("%s\n", mp->ml_hostname);
			break;
		};
	}
	if (mp->ml_next)
		print_dump(mp->ml_next);
}

#if (defined(sun) || defined(NeXT) || defined(sgi)) && !defined(SVR4)
#include <mntent.h>

#ifndef _PATH_MTAB
#define _PATH_MTAB "/etc/mtab"
#endif

int
check_mount(host, hostaddr ,path)
	char *host;		/* remote host name */
	struct in_addr hostaddr;/* remote host address */
	char *path;		/* path to check */
{
	FILE *mtab;
	struct mntent *ment;
	int found = 0;
	
	/* scan mtab for path */
	if (!(mtab = setmntent(_PATH_MTAB, "r"))) {
		perror(_PATH_MTAB);
		exit(1);
	}

	/* setmntent() doesn't do locking in read-only mode.
	   Too bad -- it seems to rely on mount() and friends to do
	   atomic updates by renaming the file.  Well, our patched amd
	   rewrites mtab in place to avoid NFS lossage, so better do the
	   locking ourselves.
	*/
	if (flock(fileno(mtab), LOCK_SH) < 0) {
		perror(_PATH_MTAB);
		exit(1);
	}

	while (!found && (ment = getmntent(mtab))) {
		char *colon;

		if (colon = strchr(ment->mnt_fsname, ':')) {
			*colon = '\0';
			if ((strcmp(colon+1, path) == 0 ||
			     strcmp(ment->mnt_dir, path) == 0) &&
			    is_same_host(ment->mnt_fsname, host, hostaddr))
				found = 1;
		}
	}

	(void) endmntent(mtab);

	if (!found) {
		char *swap;

		/*** HACK ***/
		/* swap files never show up in mtab, only root fs */
		if (swap = strstr(path, "swap")) {
			strncpy(swap, "root", 4);
			found = check_mount(host, hostaddr, path);
			strncpy(swap, "swap", 4);
		}
	}
	
	return found;
}
#endif /* BSD */

#ifdef ultrix
#include <sys/mount.h>

int
check_mount(host, hostaddr, path)
	char *host;		/* remote host name */
	struct in_addr hostaddr;/* remote host address */
	char *path;		/* path to check */
{
	int start = 0;
	int err = 0;
	struct fs_data ment;
	int found = 0;
	
	/* scan mtab for path */
	while (!found && (err = getmnt(&start, &ment, sizeof(ment),
		                       NOSTAT_MANY, NULL)) > 0) {
		char *colon;

		if (colon = strchr(ment.fd_devname, ':')) {
			*colon = '\0';
			if ((strcmp(colon+1, path) == 0 ||
			     strcmp(ment.fd_path, path) == 0) &&
			    is_same_host(ment.fd_devname, host, hostaddr))
				found = 1;
		}
	}

	if (!found && err < 0) {
		perror("getmnt");
		exit(1);
	}
	
	return found;
}
#endif /* ultrix */

#ifdef __osf__
#include <sys/mount.h>

int
check_mount(host, hostaddr, path)
	char *host;		/* remote host name */
	struct in_addr hostaddr;/* remote host address */
	char *path;		/* path to check */
{
	int nentries, i;
	struct statfs *fslist;
	int found = 0;

	nentries = getmntinfo(&fslist, MNT_NOWAIT);
	if (nentries <= 0) {
		perror("getmntinfo");
		exit(1);
	}
	for (i = 0; !found && (i < nentries); i++) {
		char *delim;

		/*
		 * Apparently two forms of nfs mount syntax are
		 * accepted: host:/path or /path@host
		 */
		if (delim = strchr(fslist[i].f_mntfromname, ':')) {
			*delim = '\0';
			if ((strcmp(delim+1, path) == 0 ||
			     strcmp(fslist[i].f_mntonname, path) == 0) &&
			    is_same_host(fslist[i].f_mntfromname,
						host, hostaddr))
				found = 1;
		} else if (delim = strchr(fslist[i].f_mntfromname, '@')) {
			*delim = '\0';
			if ((strcmp(fslist[i].f_mntfromname, path) == 0 ||
			     strcmp(fslist[i].f_mntonname, path) == 0) &&
			    is_same_host(delim+1, host, hostaddr))
				found = 1;
		}
	}

	return found;
}
#endif /* __osf__ */

#ifdef SVR4
#include <sys/mnttab.h>

int
check_mount(host, hostaddr, path)
	char *host;		/* remote host name */
	struct in_addr hostaddr;/* remote host address */
	char *path;		/* path to check */
{
	FILE *mtab;
	struct mnttab ment;
	int err = 0;
	int found = 0;
	
	/* scan mtab for path */
	if (!(mtab = fopen(MNTTAB, "r"))) {
		perror(MNTTAB);
		exit(1);
	}

	while (!found && (err = getmntent(mtab, &ment)) == 0) {
		char *colon;

		if (colon = strchr(ment.mnt_special, ':')) {
			*colon = '\0';
			if ((strcmp(colon+1, path) == 0 ||
			     strcmp(ment.mnt_mountp, path) == 0) &&
			    is_same_host(ment.mnt_special, host, hostaddr))
				found = 1;
		}
	}
	if (err > 0) {
		fprintf(stderr, "getmntent: %s: %s\n", MNTTAB,
			err == MNT_TOOLONG ? "entry exceeds MNT_LINE_MAX" :
			 err == MNT_TOOMANY ? "too many fields in line" :
			  err == MNT_TOOFEW ? "too few fields in line" :
			    "unkown error code");
		exit(1);
	}

	(void) fclose(mtab);

	/* XXX: Is this still valid in SunOS 5.x ? */
	if (!found) {
		char *swap;

		/*** HACK ***/
		/* swap files never show up in mtab, only root fs */
		if (swap = strstr(path, "swap")) {
			strncpy(swap, "root", 4);
			found = check_mount(host, path);
			strncpy(swap, "swap", 4);
		}
	}
	
	return found;
}
#endif /* SVR4 */

static char dir_path[NFS_MAXPATHLEN];

/*
 * remove entry from remote rmtab
 */
remove_mount(client, host, ml, fixit)
	CLIENT *client;		/* RCP handle to remote mountd */
	char *host;		/* remote host name */
	mountlist ml;		/* remote mount entry */
	int fixit;		/* go ahead with fix */
{
	enum clnt_stat estat;
	struct timeval tv;
	char *pathp = dir_path;

	strncpy(dir_path, ml->ml_directory, sizeof(dir_path));

	if (!fixit) {
		printf("%s: bogus mount %s:%s\n", host, ml->ml_hostname, ml->ml_directory);
		fflush(stdout);
	}
	else {
		printf("%s: removing %s:%s\n", host, ml->ml_hostname, ml->ml_directory);
		fflush(stdout);

		tv.tv_sec = CALL_TIMEOUT; tv.tv_usec = 0;

		if ((estat = clnt_call(client,
			MOUNTPROC_UMNT, xdr_dirpath, (char *)&pathp,
			xdr_void, (char *)0, tv)) != RPC_SUCCESS) {
			fprintf(stderr, "%s:%s MOUNTPROC_UMNT: ",
				host, ml->ml_directory);
			clnt_perrno(estat);
			fflush(stderr);
			return -1;
		}
	}
	return 0;
}

/*
 * fix mount list on remote host
 */
fix_rmtab(client, host, mp, fixit, force)
	CLIENT *client;		/* RCP handle to remote mountd */
	char *host;		/* remote host name */
	mountlist mp;		/* mountlist from mountd */
	int fixit;		/* go ahead with fix */
	int force;		/* force removal */
{
	mountlist p;
	struct hostent *he;
	struct in_addr hostaddr;

	/*
	 * Obtain remote address for comparisons
	 */
	if (he = gethostbyname(host)) {
		memcpy(&hostaddr, he->h_addr, sizeof(hostaddr));
	} else {
		hostaddr.s_addr = INADDR_NONE;
	}
	
	for (p = mp; p; p = p->ml_next) {
		if (is_same_host(p->ml_hostname, thishost, thisaddr)) {
			if (force ||
			    !check_mount(host, hostaddr, p->ml_directory))
				remove_mount(client, host, p, fixit);
		}
	}
}

/*
 * remove all entries from remote rmtab
 */
remove_all(client, host)
	CLIENT *client;		/* RCP handle to remote mountd */
	char *host;		/* remote host name */
{
	enum clnt_stat estat;
	struct timeval tv;

	printf("%s: removing ALL\n", host);
	fflush(stdout);

	tv.tv_sec = CALL_TIMEOUT; tv.tv_usec = 0;

	if ((estat = clnt_call(client,
		MOUNTPROC_UMNTALL, xdr_void, (char *)0,
		xdr_void, (char *)0, tv)) != RPC_SUCCESS) {
		/*
		 * RPC_SYSTEMERROR is returned even if all went well
		 */
		if (estat != RPC_SYSTEMERROR) {
			fprintf(stderr, "%s MOUNTPROC_UMNTALL: ", host);
			clnt_perrno(estat);
			fflush(stderr);
			return -1;
		}
	}
}

