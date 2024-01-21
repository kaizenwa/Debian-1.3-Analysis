/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
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
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
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
 *
 *	%W% (Berkeley) %G%
 *
 * $Id: hlfsd.c,v 1.11 1994/11/06 00:19:52 ezk Exp ezk $
 *
 * HLFSD was written at Columbia University Computer Science Department, by
 * Erez Zadok <ezk@cs.columbia.edu> and Alexander Dupuy <dupuy@cs.columbia.edu>
 * It is being distributed under the same terms and conditions as amd does.
 */

#include "hlfs.h"

#include <sys/stat.h>
#include <sys/signal.h>
#include WAIT
#include <sys/ioctl.h>
#include <fcntl.h>
#include <grp.h>

#define KDEVICE "ticlts"	/* can be "udp" */

#define NFS
#define NFSCLIENT

#ifdef NFS_3
typedef nfs_fh fhandle_t;
#endif /* NFS_3 */
#ifdef NFS_HDR
#include NFS_HDR
#endif /* NFS_HDR */
#include <sys/mount.h>
#include "mount.h"

#if !defined(HOSTNAMESZ) && !defined(__BSDI__) && !defined(__NetBSD__) && !defined(__FreeBSD__)
#include <nfs/nfs_clnt.h>
#endif /* !HOSTNAMESZ */

#if defined(__BSDI__) || defined(__NetBSD__) || defined(__FreeBSD__)
#include <sys/syslimits.h>
#define MAXNAMELEN NAME_MAX
#endif /* BSDI */

#ifdef POSIX
#include <unistd.h>
#endif /* POSIX */

#if defined(__osf__)
/*
 * Needs to know pathconf structure for hlfsd under OSF/1 1.3.
 */
#include <nfs/pathconf.h>
#endif

#if defined(MACH2)
typedef nfs_fh fhandle_t;
#include <nfs/nfs_mount.h>
#endif

#ifdef HOSTNAMESZ
# define PROGNAMESZ HOSTNAMESZ - 5
#else
# define PROGNAMESZ MAXNAMELEN
#endif /* HOSTNAMESZ */

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#if defined(DIRENT) || defined(_POSIX_VERSION) || defined(_AIX) || defined(__osf__)
# include <dirent.h>
# define NLENGTH(dirent) (strlen((dirent)->d_name))
#else
# define dirent direct
# define NLENGTH(dirent) ((dirent)->d_namlen)
# ifdef USG
#  ifdef SYSNDIR
#   include <sys/ndir.h>
#  else
#   include <ndir.h>
#  endif /* SYSNDIR */
# else
# include <sys/dir.h>		/* Assume SYSDIR, or not if you want.  */
# endif /* USG */
#endif

#ifdef notdef
#ifdef strrchr
#include <strings.h>
#else
#include <string.h>
#endif /* strchr */
#endif

#define DEFAULT_DIRNAME "/hlfs/home"
#define DEFAULT_INTERVAL 900	/* secs b/t re-reads of the password maps */
#define DEFAULT_CACHE_INTERVAL 300 /* secs during which assume a link is up */
#define DEFAULT_HLFS_GROUP	"hlfs"	/* Group name for special hlfs_gid */

#define ALT_SPOOLDIR "/var/hlfs" /* symlink to use if others fail */
#define HOME_SUBDIR ".hlfsdir"	/* dirname in user's home dir */

#define OPEN_SPOOLMODE 01777

#ifdef HAS_SYSLOG
#define DEFAULT_LOGFILE "syslog"
#else
#define DEFAULT_LOGFILE 0
#endif

#ifdef DEBUG
/* List of debug options. */
static struct opt_tab dbg_opt[] = {
	{"all", D_ALL},		/* All */
	{"daemon", D_DAEMON},	/* Enter daemon mode (nodameon == don't daemonize) */
	{"fork", D_FORK},	/* Fork server (nofork = dont' fork) */
	{"full", D_FULL},	/* Program trace */
#ifdef DEBUG_MEM
	{"mem", D_MEM},		/* Trace memory allocations */
#endif /* DEBUG_MEM */
	{"mtab", D_MTAB},	/* Use local mtab file */
	{"test", D_TEST},	/* Full debug - but no daemon */
	{0, 0}
};
int debug_flags;
#endif /* DEBUG */

extern void nfs_program_2 P((struct svc_req *, SVCXPRT *));
#if defined(__GNUC__) && !defined(__STRICT_ANSI__) && !defined(NeXT)
extern volatile void exit P((int));
#else
extern void exit P((int));
#endif /* defined(__GNUC__) && !defined(__STRICT_ANSI__) */

SVCXPRT *transp;
char *progname;
char *slinkname = 0;
int noverify = 0;
int masterpid = 0;
int serverpid = 0;
nfstime startup;
static struct itimerval reloadinterval =
{{DEFAULT_INTERVAL, 0}, {DEFAULT_INTERVAL, 0}};

int mypid;			/* for AMD routines */
int orig_umask;			/* for AMD routines */
char hostname[MAXHOSTNAMELEN] = "localhost";

int sys_nerr;
#if defined(__NetBSD__) || defined(__FreeBSD__)
extern const char *const sys_errlist[];
#else
extern char *sys_errlist[];
#endif

#ifdef UPDATE_MTAB
char *mtab = MOUNTED;
#endif /* UPDATE_MTAB */

#define ERRM ": %m"
#define fatalerror(str) \
  (fatal (strcat (strnsave ((str), strlen ((str)) + sizeof (ERRM) - 1), ERRM)))

extern void fatal P((char *));
char *alt_spooldir = ALT_SPOOLDIR;
char *home_subdir = HOME_SUBDIR;
static char *hlfs_group = DEFAULT_HLFS_GROUP;
int hlfs_gid = -3;

int cache_interval = DEFAULT_CACHE_INTERVAL;

static void usage (P_void);
static SIG_HNDL_TYP proceed P((int));
static SIG_HNDL_TYP reload P((int));
static void hlfsd_init (P_void);
static SIG_HNDL_TYP reaper P((int));
static int printpid = 0;
char *logfile = DEFAULT_LOGFILE;
static char default_dirname[] = DEFAULT_DIRNAME;
static char *dirname = default_dirname;
static int stoplight = 0;

/*
 * Turn off all kinds of attribute and symlink caches as much as possible.
 */
#ifdef M_CACHE
#ifdef NFSMNT_SYMTTL
static char default_mntopts[] = "ro,noac,nocache,symttl=0";
#else
static char default_mntopts[] = "ro,noac,nocache";
#endif /* NFSMNT_SYMTTL */
#else
#ifdef NFSMNT_SYMTTL
static char default_mntopts[] = "ro,noac,symttl=0";
#else
static char default_mntopts[] = "ro,noac";
#endif /* NFSMNT_SYMTTL */
#endif /* M_CACHE */


static void usage()
{
	fprintf(stderr,
		"Usage: %s [-Cfhnpv] [-a altdir] [-c cache-interval] [-g group]\n",
		progname);
	fprintf(stderr, "\t[-i interval] [-l logfile] [-o mntopts]\n");
	show_opts('x', xlog_opt);
#ifdef DEBUG
	show_opts('D', dbg_opt);
#endif /* DEBUG */
	fprintf(stderr, "\t[dirname [subdir]]\n");
	exit(2);
}


#ifdef DEBUG
int debug_option(opt)
char *opt;
{
	return (cmdoption(opt, dbg_opt, &debug_flags));
}
#endif /* DEBUG */

int main(argc, argv)
int argc;
char *argv[];
{
	int opt;
	int opterrs = 0;
	int forcefast = 0;
	int forcecache = 0;
	char *dot;
	struct stat stmodes;
	DIR *mountdir;
	struct dirent *direntry;
	char *mntopts = (char *) NULL;
	MTYPE_TYPE type = MOUNT_TYPE_NFS;
	struct mntent mnt;
	struct nfs_args mountargs;
	int mntflags = 0;
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__) || defined(HPUX_9)
	struct sigaction sa;
#endif
#ifdef __svr4__
	SVCXPRT *sexp;
	struct netconfig *ncp;
	struct knetconfig knc;
	struct stat statbuf;
#else
	struct sockaddr_in localsocket;
#endif /* __svr4__ */

#ifdef NFSMNT_POSIX
	static struct pathcnf posixcrap	= {2, 0, 0, NFS_MAXNAMLEN, NFS_MAXPATHLEN};
#endif /* NFSMNT_POSIX */
	char hostpid_fs[MAXHOSTNAMELEN + 1 + 16]; /* room for ":(pid###)" */
	char progpid_fs[PROGNAMESZ + 1 + 11]; /* room for ":pid" */
	int retry;
	struct group *grp;

	extern int optind;
	extern char *optarg;

	if (geteuid()) {
	  fprintf(stderr, "hlfsd can only be run as root\n");
	  exit(1);
	}
	setbuf(stdout, (char *)NULL);
	umask(0);

	/* get program name and truncate so we don't overflow progpid_fs */

	if ((progname = strrchr(argv[0], '/')) != NULL)
		progname++;
	else
		progname = argv[0];
	if ((int) strlen(progname) >  PROGNAMESZ) /* truncate to reasonable size */
		progname[PROGNAMESZ] = '\0';

	while ((opt = getopt(argc, argv, "a:c:CD:fg:hi:l:no:px:v")) != EOF)
		switch (opt) {
		case 'a':
			if (!optarg || optarg[0] != '/') {
				printf("%s: invalid directory for -a: %s\n",
				       progname, optarg);
				exit(3);
			}
			alt_spooldir = optarg;
			break;

		case 'c':
			if (!atoi(optarg)) {
				printf("%s: invalid interval for -c: %s\n",
				       progname, optarg);
				exit(3);
			}
			cache_interval = atoi(optarg);
			break;

		case 'C':
			forcecache++;
			break;

		case 'f':
			forcefast++;
			break;

		case 'g':
			hlfs_group = optarg;
			break;

		case 'i':
			if (!atoi(optarg)) {
				printf("%s: invalid interval for -i: %s\n",
				       progname, optarg);
				exit(3);
			}
			reloadinterval.it_interval.tv_sec = atoi(optarg);
			reloadinterval.it_value.tv_sec = atoi(optarg);
			break;

		case 'l':
			logfile = optarg;
			break;

		case 'n':
			noverify++;
			break;

		case 'o':
			mntopts = optarg;
			break;

		case 'p':
			printpid++;
			break;

		case 'v':
			fprintf(stderr, "%s\n", HLFSD_VERSION);
			exit(0);

		case 'x':
			opterrs += switch_option(optarg);
			break;

		case 'D':
#ifdef DEBUG
			opterrs += debug_option(optarg);
#else
			fprintf(stderr,
				"%s: not compiled with DEBUG -- sorry.\n",
				progname);
#endif /* DEBUG */
			break;

		case 'h':
		case '?':
			opterrs++;
		}

/*
 * Terminate if did not ask to forcecache (-C) and hlfsd would not be able
 * to set the minimum cache intervals.
 */
#if !defined(NFSMNT_ACREGMIN) || !defined(NFSMNT_ACDIRMIN)
	if (!forcecache) {
		fprintf(stderr, "%s: will not be able to turn off attribute caches.\n", progname);
		exit(1);
	} 
#endif /* !defined(NFSMNT_ACREGMIN) || !defined(NFSMNT_ACDIRMIN) */

	switch (argc - optind) {
	case 2:
		home_subdir = argv[optind + 1];
	case 1:
		dirname = argv[optind];
	case 0:
		break;
	default:
		opterrs++;
	}

	if (opterrs)
		usage();

	/* find gid for hlfs_group */
	if ((grp = getgrnam(hlfs_group)) == (struct group *) NULL) {
		fprintf(stderr, "%s: cannot get gid for group \"%s\".\n", progname, hlfs_group);
	} else {
		hlfs_gid = grp->gr_gid;
	}

	/* get hostname for logging and open log before we reset umask */
	(void) gethostname(hostname, MAXHOSTNAMELEN);
	if ((dot = strchr(hostname, '.')) != NULL)
		*dot = '\0';
	if (logfile)
		(void) switch_to_logfile(logfile);
	orig_umask = umask(0);

#ifdef DEBUG
#ifdef UPDATE_MTAB
#if 0
	/* XXX: EZK: REMOVE THIS HACK!!! */
	if (debug_flags & D_MTAB)
		mtab = DEBUG_MTAB;
#endif 0
#else
	if (debug_flags & D_MTAB)
		dlog("-D mtab option ignored");
#endif /* UPDATE_MTAB */
#endif /* DEBUG */
	/* avoid hanging on other NFS servers if started elsewhere */

	if (chdir("/") < 0)
		fatal("cannot chdir to /: %m");

	if (geteuid() != 0)
		fatal("must be root to mount filesystems");

	/*
	 * dirname must match "^(/.*)/([^/]+)$", and is split at last '/' with
	 * slinkname = `basename $dirname` - requires dirname be writable
	 */

	if (dirname[0] != '/'
	    || ((slinkname = strrchr(dirname, '/')), *slinkname++ = '\0',
		(dirname[0] == '\0' || slinkname[0] == '\0'))) {
		if (slinkname)
			*--slinkname = '/';
		printf("%s: invalid mount directory/link %s\n",
		       progname, dirname);
		exit(3);
	}
	if (!forcefast) {
		/* make sure mount point exists and is at least mode 555 */

		if (stat(dirname, &stmodes) < 0)
			if (errno != ENOENT || mkdirs(dirname, 0555) < 0
			    || stat(dirname, &stmodes) < 0)
				fatalerror(dirname);

		if ((stmodes.st_mode & 0555) != 0555) {
			fprintf(stderr, "%s: directory %s not read/executable\n",
				progname, dirname);
			plog(XLOG_WARNING, "directory %s not read/executable",
			     dirname);
		}

		/* warn if extraneous stuff will be hidden by mount */

		if ((mountdir = opendir(dirname)) == NULL)
			fatalerror(dirname);

		while ((direntry = readdir(mountdir)) != NULL) {
			if (strncmp(".", direntry->d_name,
				    NLENGTH(direntry)) &&
			    strncmp("..", direntry->d_name,
				    NLENGTH(direntry)) &&
			    strncmp(slinkname, direntry->d_name,
				    NLENGTH(direntry)))
				break;
		}

		if (direntry != NULL) {
			fprintf(stderr, "%s: %s/%s will be hidden by mount\n",
				progname, dirname, direntry->d_name);
			plog(XLOG_WARNING, "%s/%s will be hidden by mount\n",
			     dirname, direntry->d_name);
		}
		(void) closedir(mountdir);

		/* make sure alternate spool dir exists */

		if (errno = mkdirs(alt_spooldir, OPEN_SPOOLMODE)) {
			fprintf(stderr, "%s: cannot create alternate dir ",
				progname);
			perror(alt_spooldir);
			plog(XLOG_ERROR, "cannot create alternate dir %s: %m",
			     alt_spooldir);
		}
		(void) chmod(alt_spooldir, OPEN_SPOOLMODE);

		/* create failsafe link to alternate spool directory */

		slinkname[-1] = '/';	/* unsplit dirname to include link */

		if (lstat(dirname, &stmodes) == 0 &&
		    (stmodes.st_mode & S_IFMT) != S_IFLNK) {
			fprintf(stderr, "%s: failsafe %s not a symlink\n",
				progname, dirname);
			plog(XLOG_WARNING, "failsafe %s not a symlink\n",
			     dirname);
		} else {
			(void) unlink(dirname);

			if (symlink(alt_spooldir, dirname) < 0) {
				fprintf(stderr,
				 "%s: cannot create failsafe symlink %s -> ",
					progname, dirname);
				perror(alt_spooldir);
				plog(XLOG_WARNING,
				"cannot create failsafe symlink %s -> %s: %m",
				     dirname, alt_spooldir);
			}
		}

		slinkname[-1] = '\0';	/* resplit dirname */
	}			/* end of "if (!forcefast) {" */

	/*
	 * Register hlfsd as an nfs service with the portmapper.
	 */
#ifdef __svr4__
	if ((ncp = getnetconfigent(KDEVICE)) == (struct netconfig *) NULL)
	  fatal("cannot getnetconfigent for kernel device (udp or ticlts)");
	
	if ((sexp = svc_tli_create(RPC_ANYFD, ncp,
				   (struct t_bind *) NULL, 0, 0))
	    == (SVCXPRT *) NULL)
	  fatal("cannot create tli service");
	if (svc_reg(sexp, NFS_PROGRAM, NFS_VERSION, nfs_program_2,
		    (struct netconfig *) NULL) != 1)
	  fatal("could not register hlfsd nfs service");
#else
	transp = svcudp_create(RPC_ANYSOCK);
	if (transp == NULL)
		fatal("cannot create rpc/udp service");

	if (!svc_register(transp, NFS_PROGRAM, NFS_VERSION, nfs_program_2, 0))
		fatal("unable to internally register service");

#endif /* __svr4__ */

#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__) || defined(HPUX_9)
	sa.sa_handler = proceed;
	sa.sa_flags = 0;
	sigemptyset(&(sa.sa_mask));
	sigaddset(&(sa.sa_mask), SIGUSR2);
	sigaction(SIGUSR2, &sa, NULL);
#else
	(void) signal(SIGUSR2, proceed);
#endif

	hlfsd_init();		/* start up child (forking) to run svc_run */

#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__) || defined(HPUX_9)
	sa.sa_handler = reaper;
	sa.sa_flags = 0;
	sigemptyset(&(sa.sa_mask));
	sigaddset(&(sa.sa_mask), SIGCHLD);
	sigaction(SIGCHLD, &sa, NULL);
#else
	(void) signal(SIGCHLD, reaper);
#endif

#ifdef DEBUG
	/*
	 * In the parent, if -D nodaemon (or -D daemon) , we don't need to
	 * set this signal handler.
	 */
	Debug(D_DAEMON) {
#endif /* DEBUG */
	/* XXX: port to use pure svr4 signals */
	while (stoplight != SIGUSR2) {
		int s = -99;
		s = sigpause(0);	/* wait for child to set up */
	}
#ifdef DEBUG
	}
#endif /* DEBUG */

	/*
	 * setup options to mount table (/etc/{mtab,mnttab}) entry
	 */
	(void) sprintf(hostpid_fs, "%s:(pid%d)", hostname, masterpid);
	bzero((char *) &mnt, sizeof(mnt));
	mnt.mnt_dir = dirname;	/* i.e., "/mail" */
	mnt.mnt_fsname = hostpid_fs;
	mnt.mnt_type = MNTTYPE_AUTO;
	mnt.mnt_opts = mntopts ? mntopts : default_mntopts;
	mnt.mnt_freq = 0;
	mnt.mnt_passno = 0;

	/*
	 * Set the address field of the nfs_args structure.
	 */
	bzero((char *) &mountargs, sizeof(mountargs));

#ifdef __svr4__
	mountargs.addr = &sexp->xp_ltaddr;

	/*
	 * set up knconf field
	 */
	bzero((char *) &knc, sizeof(struct knetconfig));
	knc.knc_semantics = ncp->nc_semantics;
	knc.knc_protofmly = ncp->nc_protofmly;
	knc.knc_proto = ncp->nc_proto;
	if (stat(ncp->nc_device, &statbuf) < 0)
	  fatal("cannot stat ncp->nc_device");
	else {
	  knc.knc_rdev = statbuf.st_rdev;
	  mountargs.knconf = &knc;
	}

	/*
	 * set up syncaddr field
	 */
	mountargs.syncaddr = (struct netbuf *) NULL;
	
#else
/*
 * Define this in config/os-*.h if your system does
 * not support the loopback interface.
 */
# ifdef NOLOOPBACK
	get_myaddress (&localsocket);
# else
	bzero((char *) &localsocket, sizeof(localsocket));
	localsocket.sin_family = AF_INET;
	localsocket.sin_addr.s_addr = 0x7f000001; /* 127.0.0.1 = localhost */
# endif /* NOLOOPBACK */
	localsocket.sin_port = transp->xp_port;

	NFS_SA_DREF(mountargs, &localsocket);
#endif /* __svr4__ */

	/*
	 * Update filehandle field
	 */
	NFS_FH_DREF(mountargs.fh, (NFS_FH_TYPE)&root);

	/*
	 * Update hostname field.
	 * Make some name prog:pid (i.e., hlfsd:174) for hostname
	 */	  
	sprintf(progpid_fs, "%s:%d", progname, masterpid);
#ifdef HOSTNAMESZ
	/* Most kernels have a name length restriction. */
	if ((int) strlen(progpid_fs) >= (int) HOSTNAMESZ)
		strcpy(progpid_fs + HOSTNAMESZ - 3, "..");
#endif /* HOSTNAMESZ */
#ifdef  NFS_HN_DREF
	NFS_HN_DREF(mountargs.hostname, progpid_fs);
#else
	mountargs.hostname = progpid_fs;
#endif /* NFS_HN_DREF */

	/*
	 * General mount options
	 */
#ifdef __svr4__
	mountargs.flags |= NFSMNT_KNCONF;
#endif /* __svr4__ */
#ifdef NFSMNT_FSNAME
	mountargs.fsname = hostpid_fs;
	mountargs.flags |= NFSMNT_FSNAME;
#endif /* NFSMNT_FSNAME */
	mountargs.flags |= NFSMNT_HOSTNAME;
	if (mountargs.timeo = hasmntval(&mnt, "timeo"))
		mountargs.flags |= NFSMNT_TIMEO;

	if (mountargs.retrans = hasmntval(&mnt, "retrans"))
		mountargs.flags |= NFSMNT_RETRANS;

	if (hasmntopt(&mnt, MNTOPT_SOFT) != NULL)
		mountargs.flags |= NFSMNT_SOFT;
#ifdef MNTOPT_INTR
	if (hasmntopt(&mnt, MNTOPT_INTR) != NULL)
		mountargs.flags |= NFSMNT_INT;
#endif /* MNTOPT_INTR */
#ifdef NFSMNT_BIODS
	if (mountargs.biods = hasmntval(&mnt, "biods"))
		mountargs.flags |= NFSMNT_BIODS;
#endif /* NFSMNT_BIODS */
#ifdef NFSMNT_DUMBTIMR
	mountargs.flags |= NFSMNT_DUMBTIMR;
#endif /* NFSMNT_DUMBTIMR */

#ifdef NFSMNT_NOAC
	mountargs.flags |= NFSMNT_NOAC;
#endif /* NFSMNT_NOAC */

#ifdef HAVE_SYMLINK_CACHE
#  define NFSMNT_AC_VALUE  0
#else
#  define NFSMNT_AC_VALUE  1
#endif /* HAVE_SYMLINK_CACHE */

#ifdef NFSMNT_ACREGMIN
	mountargs.flags |= NFSMNT_ACREGMIN;
	mountargs.acregmin = NFSMNT_AC_VALUE;
#endif /* NFSMNT_ACREGMIN */
#ifdef NFSMNT_ACREGMAX
	mountargs.flags |= NFSMNT_ACREGMAX;
	mountargs.acregmax = NFSMNT_AC_VALUE;
#endif /* NFSMNT_ACREGMAX */
#ifdef NFSMNT_ACDIRMIN
	mountargs.flags |= NFSMNT_ACDIRMIN;
	mountargs.acdirmin = NFSMNT_AC_VALUE;
#endif /* NFSMNT_ACDIRMIN */
#ifdef NFSMNT_ACDIRMAX
	mountargs.flags |= NFSMNT_ACDIRMAX;
	mountargs.acdirmax = NFSMNT_AC_VALUE;
#endif /* NFSMNT_ACDIRMAX */

#ifdef NFSMNT_SYMTTL
	mountargs.flags |= NFSMNT_SYMTTL;
	mountargs.symttl = 0;
#endif /* NFSMNT_SYMTTL */

#if defined(NFSMNT_POSIX) && defined(MNTOPT_POSIX)
	if (hasmntopt(&mnt, MNTOPT_POSIX) != NULL) {
	  mountargs.flags |= NFSMNT_POSIX;
	  mountargs.pathconf = &posixcrap;
	}
#endif /* NFSMNT_POSIX && MNTOPT_POSIX */

	mntflags = compute_mount_flags(&mnt);

#ifdef ULTRIX_HACK
	mountargs.gfs_flags = mntflags;
	mntflags &= M_RDONLY;
	if (mntflags & M_RDONLY)
		mountargs.flags |= NFSMNT_RONLY;
#endif /* ULTRIX_HACK */

	retry = hasmntval(&mnt, "retry");


/*
 * The following code could be cleverly ifdef-ed, but I duplicated the
 * mount_fs call three times for simplicity and readability.
 */
#ifdef DEBUG
/*
 * For some reason, this mount may have to be done in the background, if I am
 * using -D nodebug.  I suspect that the actual act of mounting requires
 * calling to hlfsd itself to invoke one or more of its nfs calls, to stat
 * /mail.  That means that even if you say -D nodaemon, at least the mount
 * of hlfsd itself on top of /mail will be done in the background.
 * The other alternative I have is to run svc_run, but set a special
 * signal handler to perform the mount in N seconds via some alarm.
 * 	-Erez Zadok.
 */
	if (debug_flags & D_DAEMON) { /* asked for -D daemon */
	    if (mount_fs(&mnt, mntflags, (caddr_t) & mountargs, retry, type) < 0)
		fatal("nfsmount: %m");
	} else {		/* asked for -D nodaemon */
	    if (fork() == 0) { /* child runs mount */
		if (mount_fs(&mnt, mntflags, (caddr_t) & mountargs, retry, type) < 0)
		    fatal("nfsmount: %m");
		dlog("exit in file %s:%d", __FILE__, __LINE__);
		exit(0); /* all went well */
	    }
	}
#else
	if (mount_fs(&mnt, mntflags, (caddr_t) & mountargs, retry, type) < 0)
	    fatal("nfsmount: %m");
#endif /* DEBUG */

	if (printpid)
		printf("%d\n", masterpid);

	plog(XLOG_INFO, "hlfsd ready to serve");
#ifdef DEBUG
	/*
	 * If asked not to fork a daemon (-D nodaemon), then hlfsd_init()
	 * will not run svc_run.  We must start svc_run here.
	 */
	dlog("starting no-daemon debugging svc_run");
	DebugNo(D_DAEMON)
		svc_run();
#endif /* DEBUG */
	cleanup(0);		/* should never happen here */
	return (0);		/* everything went fine? */
}

static void hlfsd_init()
{
	int child = 0;
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__) || defined(HPUX_9)
	struct sigaction sa;
#endif
#ifdef TIOCNOTTY
	int tty;
#endif /* TIOCNOTTY */

#ifdef DEBUG
	/*
	 * If -D daemon then we must fork. 
	 */
	Debug(D_DAEMON)
#endif /* DEBUG */
		child = fork();

	if (child < 0)
		fatal("fork: %m");
	
	if (child == 0)	{	/* child will be initialize server */
		init_homedir();

		masterpid = serverpid = mypid = getpid();

		/*
		 * SIGALRM/SIGHUP: reload password database if timer expired
		 * or user sent HUP signal.
		 */
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__) || defined(HPUX_9)
		sa.sa_handler = reload;
		sa.sa_flags = 0;
		sigemptyset(&(sa.sa_mask));
		sigaddset(&(sa.sa_mask), SIGALRM);
		sigaddset(&(sa.sa_mask), SIGHUP);
		sigaction(SIGALRM, &sa, NULL);
		sigaction(SIGHUP, &sa, NULL);
#else
		(void) signal(SIGALRM, reload);
		(void) signal(SIGHUP, reload);
#endif
		/*
		 * SIGTERM: cleanup and exit.
		 */
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__) || defined(HPUX_9)
		sa.sa_handler = cleanup;
		sa.sa_flags = 0;
		sigemptyset(&(sa.sa_mask));
		sigaddset(&(sa.sa_mask), SIGTERM);
		sigaction(SIGTERM, &sa, NULL);
#else
		(void) signal(SIGTERM, cleanup);
#endif
		/*
		 * SIGCHLD: interlock sycronization and testing
		 */
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__) || defined(HPUX_9)
		sa.sa_handler = interlock;
		sa.sa_flags = 0;
		sigemptyset(&(sa.sa_mask));
		sigaddset(&(sa.sa_mask), SIGCHLD);
		sigaction(SIGCHLD, &sa, NULL);
#else
		(void) signal(SIGCHLD, interlock);
#endif
		/*
		 * SIGUSR1: dump internal hlfsd maps/cache to file
		 */
#if defined(_POSIX_SOURCE) && !defined(sigmask) && !defined(_AIX) && !defined(__osf__) || defined(__svr4__) || defined(HPUX_9)
# if defined(DEBUG) || defined(DEBUG_PRINT)
		sa.sa_handler = plt_print;
# else
		sa.sa_handler = SIG_IGN;
# endif /* DEBUG || DEBUG_PRINT */
		sa.sa_flags = 0;
		sigemptyset(&(sa.sa_mask));
		sigaddset(&(sa.sa_mask), SIGUSR1);
		sigaction(SIGUSR1, &sa, NULL);
#else
# if defined(DEBUG) || defined(DEBUG_PRINT)
		(void) signal(SIGUSR1, plt_print);
# else
		(void) signal(SIGUSR1, SIG_IGN);
# endif /* DEBUG || DEBUG_PRINT */
#endif

		if (setitimer(ITIMER_REAL, &reloadinterval,
			      (struct itimerval *) 0) < 0)
			fatal("setitimer: %m");

		(void) gettimeofday((struct timeval *) &startup, (struct timezone *) 0);

#ifdef DEBUG
		/*
		 * If -D daemon, then start serving here in the child,
		 * and the parent will exit.  But if -D nodaemon, then
		 * skip this code and make sure svc_run is entered elsewhere.
		 */
		Debug(D_DAEMON) {
#endif /* DEBUG */
#ifdef TIOCNOTTY
			if ((tty = open("/dev/tty", O_RDWR)) < 0) {
				/* not an error if already no ctty */
				if (errno != ENXIO)
					plog(XLOG_WARNING,
					"Could not open controlling tty: %m");
			} else {
				if (ioctl(tty, TIOCNOTTY, 0) < 0
				    && errno != ENOTTY)
					plog(XLOG_WARNING,
				 "Could not disassociate tty (TIOCNOTTY): %m");
				(void) close(tty);
			}
#else
			(void) setpgrp();
#endif /* TIOCNOTTY */
			/*
			 * signal parent we are ready. parent should
			 * mount(2) and die.
			 */
			if (kill(getppid(), SIGUSR2) < 0)
				fatal("kill: %m");
			plog(XLOG_INFO, "starting svc_run");
			svc_run();
			cleanup(0);	/* should never happen, just in case */
#ifdef DEBUG
		}	/* end of code that runs iff hlfsd daemonizes */
#endif /* DEBUG */
	} else {		/* parent process - save child pid */
		masterpid = child;
		mypid = getpid();	/* for AMD routines */
	}
}


static SIG_HNDL_TYP proceed(signum)
int signum;
{
	stoplight = signum;
}


static SIG_HNDL_TYP reload(signum)
int signum;
{
	int child;
	int status;

	if (getpid() != masterpid)
		return;

	/*
	 * If received a SIGHUP, close and reopen the log file (so that it
	 * can be rotated)
	 */
	if (signum == SIGHUP && logfile)
		(void) switch_to_logfile(logfile);

	if ((child = fork()) > 0) {
		serverpid = child;

		init_homedir();

		if (kill(child, SIGKILL) < 0) {
			plog(XLOG_ERROR, "kill child: %m");
		} else {	/* wait for child to die before continue */
			if (wait((WAIT_STATUS_TYPE *) &status) != child) {
				/*
				 * I took out this line because it generates
				 * annoying output.  It indicates a very
				 * small bug in hlfsd which is totally
				 * harmless.  It causes hlfsd to work a bit
				 * harder than it should.  Nevertheless, I
				 * intend on fixing it in a future release.
				 * Erez Zadok <ezk@cs.columbia.edu>
				 */
				/* plog(XLOG_ERROR, "unknown child"); */
			}
		}

		serverpid = masterpid;
	} else if (child < 0) {
		plog(XLOG_ERROR, "unable to fork: %m");
	} else			/* let child handle requests while we reload */
		serverpid = getpid();
}

SIG_HNDL_TYP cleanup(signum)
int signum;
{
	struct stat stbuf;
	int umount_result;

#ifdef DEBUG
	Debug(D_DAEMON)
#endif /* DEBUG */
	if (getpid() != masterpid)
		return;

#ifdef DEBUG
	Debug(D_DAEMON)
#endif /* DEBUG */
	if (fork() != 0) {
		masterpid = 0;
		return;
	}
	mypid = getpid();

	for (;;) {
		while ((umount_result = UMOUNT_FS(dirname)) == EBUSY) {
#ifdef DEBUG
			dlog("cleanup(): umount delaying for 10 seconds");
#endif /* DEBUG */
			sleep(10);
		}
		if (stat(dirname, &stbuf) == 0 && stbuf.st_ino == ROOTID) {
			plog(XLOG_ERROR, "unable to unmount %s", dirname);
			plog(XLOG_ERROR, "suspending, unmount before terminating");
			kill(mypid, SIGSTOP);
			continue; /* retry unmount */
		}
		break;
	}
#ifdef DEBUG
	dlog("cleanup(): killing processes and terminating");
	Debug(D_DAEMON)
#endif /* DEBUG */
	(void) kill(masterpid, SIGKILL);
#ifdef DEBUG
	Debug(D_DAEMON)
#endif /* DEBUG */
	(void) kill(serverpid, SIGKILL);

	plog(XLOG_INFO, "hlfsd terminating with status 0\n");
#ifdef DEBUG
	dlog("exit in file %s:%d", __FILE__, __LINE__);
#endif /* DEBUG */
	exit(0);
}

static SIG_HNDL_TYP reaper(signum)
int signum;
{
	int result;

	if (wait((WAIT_STATUS_TYPE *) &result) == masterpid) {
#ifdef DEBUG
		dlog("exit in file %s:%d", __FILE__, __LINE__);
#endif /* DEBUG */
		exit(4);
	}
}

void fatal(mess)
char *mess;
{
	if (logfile && strcmp(logfile, "stderr")) {
		char lessmess[128];
		int messlen;

		messlen = strlen(mess);

		if (strcmp(&mess[messlen + 1 - sizeof(ERRM)], ERRM))
			fprintf(stderr, "%s: %s\n", progname, mess);
		else {
			strcpy(lessmess, mess);
			lessmess[messlen - 4] = '\0';

			if (errno < sys_nerr)
				fprintf(stderr, "%s: %s: %s\n", progname,
					lessmess, sys_errlist[errno]);
			else
				fprintf(stderr, "%s: %s: Error %d\n",
					progname, lessmess, errno);
		}
	}
	plog(XLOG_FATAL, mess);

	going_down(1);
}

void going_down(rc)
int rc;
{
	int mypid = getpid();

	if (mypid == masterpid)
		cleanup(0);
	else if (mypid == serverpid)
		(void) kill(masterpid, SIGTERM);

#ifdef DEBUG
	dlog("exit in file %s:%d", __FILE__, __LINE__);
#endif /* DEBUG */
	exit(rc);
}
