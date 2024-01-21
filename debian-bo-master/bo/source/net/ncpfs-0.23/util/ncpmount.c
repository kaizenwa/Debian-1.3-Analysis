/*
 *  nwmount.c
 *
 *  Copyright (C) 1995 by Volker Lendecke
 *
 *  1/20/96 - Steven N. Hirsch (hirsch@emba.uvm.edu)
 *
 *  If the ncpfs support is not loaded and we are using kerneld to
 *  autoload modules, then we don't want to do it here.  I added
 *  a conditional which leaves out the test and load code.
 *
 *  Even if we _do_ want ncpmount to load the module, passing a
 *  fully-qualified pathname to modprobe causes it to bypass a 
 *  path search.  This may lead to ncpfs.o not being found on
 *  some systems.
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <pwd.h>
#include <grp.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/stat.h>
#include <sys/types.h>
/* #include <sys/wait.h> */  /* generates a warning here */
extern pid_t waitpid(pid_t, int *, int);
#include <sys/errno.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <sys/mount.h>
#include <mntent.h>
#include <linux/ipx.h>
#include <sys/ioctl.h>

#include <linux/fs.h>
#include <linux/ncp.h>
#include <linux/ncp_fs.h>
#include <linux/ncp_mount.h>
#include "ncplib.h"
#include "com_err.h"

static char *progname;
static void usage(void);
static void help(void);

#ifndef HAVE_KERNELD

/* Returns 0 if the filesystem is in the kernel after this routine
   completes */
static int
load_ncpfs(void)
{
	FILE *ffs;
	char s[1024];
	char *p, *p1;
        pid_t pid;
        int status;

	/* Check if ncpfs is in the kernel */
	ffs = fopen("/proc/filesystems", "r");

	if (ffs == NULL)
	{
		perror("Error: \"/proc/filesystems\" could not be read:");
		return -1;
	}

	p = NULL;
	while (! feof(ffs))
	{
		p1 = fgets(s, sizeof(s), ffs);
		if (p1)
		{
			p = strstr(s, "ncpfs");
			if (p)
			{
				break;
			}
		}
	}
	fclose(ffs);

	if (p)
	{
		return 0;
	}

        /* system() function without signal handling, from Stevens */

        if ((pid = fork()) < 0)
	{
                return 1;
        }
	else if (pid == 0)
	{
                /* child */
                execl("/sbin/modprobe", "modprobe", "ncpfs", NULL);
                _exit(127);     /* execl error */
        }
	else
	{
                /* parent */
                while (waitpid(pid, &status, 0) < 0)
		{
                        if (errno != EINTR)
			{
                                status = -1;
                                break;
                        }
                }
        }
        return status;
}

#endif /* HAVE_KERNELD */

/* Check whether user is allowed to mount on the specified mount point */
static int
mount_ok(struct stat *st)
{
        if (!S_ISDIR(st->st_mode))
        {
                errno = ENOTDIR;
                return -1;
        }
	
        if (   (getuid() != 0)
            && (   (getuid() != st->st_uid)
                || ((st->st_mode & S_IRWXU) != S_IRWXU)))
        {
                errno = EPERM;
                return -1;
        }

        return 0;
}



int 
main(int argc, char *argv[])
{
        struct ncp_mount_data data;
        struct stat st;
	char mount_name[256];

        int fd, result;
	struct sockaddr_ipx addr;
	struct sockaddr_ipx *server_addr;
	int addrlen;

        int upcase_password;
	long err;

        int um;
	unsigned int flags;

        char mount_point[MAXPATHLEN];
        struct mntent ment;
        FILE *mtab;
	char *tmp_mount;

        char *server   = NULL;
	char *user     = NULL;
	char *password = NULL;
	struct ncp_conn_spec *spec;

	uid_t conn_uid = getuid();

	struct ncp_conn *conn;

	int opt;

        progname = argv[0];

	memzero(data); memzero(spec);

	if (geteuid() != 0)
	{
                fprintf(stderr, "%s must be installed suid root\n", progname);
                exit(1);
        }

        data.uid = getuid();
        data.gid = getgid();
        um = umask(0);
        umask(um);
        data.file_mode = (S_IRWXU|S_IRWXG|S_IRWXO) & ~um;
        data.dir_mode  = 0;
	data.flags    |= NCP_MOUNT_SOFT;
	data.time_out = 60;
	data.retry_count = 5;

	upcase_password = 1;

        while ((opt = getopt (argc, argv, "CS:U:c:u:g:f:d:P:nhvV:t:r:"))
	       != EOF)
	{
                switch (opt)
		{
                case 'C':
                        upcase_password = 0;
                        break;
		case 'S':
			if (strlen(optarg) >= sizeof(spec->server))
			{
				fprintf(stderr, "Servername too long:%s\n",
					optarg);
				return 1;
			}
			server = optarg;
			break;
                case 'U':
                        if (strlen(optarg) >= sizeof(spec->user))
			{
                                fprintf(stderr, "Username too long: %s\n",
                                        optarg);
                                return 1;
                        }
			user = optarg;
			break;
                case 'c':
                        if (isdigit(optarg[0]))
			{
				conn_uid = atoi(optarg);
                        }
			else
			{
                                struct passwd *pwd = getpwnam(optarg);
                                if (pwd == NULL)
				{
                                        fprintf(stderr, "Unknown user: %s\n",
                                                optarg);
                                        return 1;
                                }
                                conn_uid = pwd->pw_uid;
                        }
                        break;
                case 'u':
                        if (isdigit(optarg[0]))
			{
				data.uid = atoi(optarg);
                        }
			else
			{
                                struct passwd *pwd = getpwnam(optarg);
                                if (pwd == NULL)
				{
                                        fprintf(stderr, "Unknown user: %s\n",
                                                optarg);
                                        return 1;
                                }
                                data.uid = pwd->pw_uid;
                        }
                        break;
                case 'g':
                        if (isdigit(optarg[0]))
			{
                                data.gid = atoi(optarg);
                        }
			else
			{
                                struct group *grp = getgrnam(optarg);
                                if (grp == NULL)
				{
                                        fprintf(stderr, "Unknown group: %s\n",
                                                optarg);
                                        return 1;
                                }
                                data.gid = grp->gr_gid;
                        }
                        break;
                case 'f':
                        data.file_mode = strtol(optarg, NULL, 8);
                        break;
                case 'd':
                        data.dir_mode = strtol(optarg, NULL, 8);
                        break;
                case 'P':
			if (strlen(optarg) >= sizeof(spec->password))
			{
				printf("password too long\n");
				exit(1);
			}
			password = optarg;
                        break;
		case 'V':
			if (strlen(optarg) >= sizeof(data.mounted_vol))
			{
				printf("Volume too long: %s\n", optarg);
				exit(1);
			}
			strcpy(data.mounted_vol, optarg);
			break;
                case 'n':
			password = "";
                        break;
		case 't':
			data.time_out = atoi(optarg);
			break;
		case 'r':
			data.retry_count = atoi(optarg);
			break;
		case 'h':
			help();
			exit(1);
		case 'v':
			fprintf(stderr, "ncpfs version %s\n", NCPFS_VERSION);
			exit(1);
                default:
			usage();
                        return -1;
                }
        }

	if ((spec = ncp_find_conn_spec(server,user,password,1, data.uid, &err))
	    == NULL)
	{
		com_err(progname, err, "in find_conn_spec");
		exit(1);
	}

	if (upcase_password != 0)
	{
		str_upper(spec->password);
	}

	if (optind != argc-1)
	{
		usage();
		return -1;
	}

	realpath(argv[optind], mount_point);

        if (stat(mount_point, &st) == -1)
	{
                fprintf(stderr, "could not find mount point %s: %s\n",
                        mount_point, strerror(errno));
                exit(1);
        }

        if (mount_ok(&st) != 0)
	{
                fprintf(stderr, "cannot to mount on %s: %s\n",
                        mount_point, strerror(errno));
                exit(1);
        }

#ifndef HAVE_KERNELD
	/* Check if the ncpfs filesystem is in the kernel.  If not, attempt
	 * to load the ncpfs module */
	if (load_ncpfs() != 0)
	{
		fprintf(stderr, "Error: Unable to load ncpfs, exiting...\n");
		exit(1);
	}
#endif

	data.version     = NCP_MOUNT_VERSION;
        data.mounted_uid = conn_uid;
	memcpy(data.server_name, spec->server, sizeof(data.server_name));

        if (data.dir_mode == 0)
	{
                data.dir_mode = data.file_mode;
                if ((data.dir_mode & S_IRUSR) != 0)
                        data.dir_mode |= S_IXUSR;
                if ((data.dir_mode & S_IRGRP) != 0)
                        data.dir_mode |= S_IXGRP;
                if ((data.dir_mode & S_IROTH) != 0)
                        data.dir_mode |= S_IXOTH;
        }

	if ((tmp_mount = ncp_find_permanent(spec)) != NULL)
	{

		fprintf(stderr,
			"You already have mounted server %s\nas user "
			"%s\non mount point %s\n", spec->server, spec->user,
			tmp_mount);
		exit(1);
	}

	if ((server_addr = ncp_find_fileserver(spec->server, &err)) == NULL)
	{
		com_err("ncpmount", err, "when trying to find %s",
			spec->server);
		exit(1);
	}
	data.serv_addr = *server_addr;

	data.ncp_fd  = socket(AF_IPX, SOCK_DGRAM, PF_IPX);
	if (data.ncp_fd == -1)
	{
		com_err("ncpmount", err, "opening ncp_socket");
		exit(1);
	}

	data.wdog_fd = socket(AF_IPX, SOCK_DGRAM, PF_IPX);
	if (data.wdog_fd == -1)
	{
		fprintf(stderr, "could not open wdog socket: %s\n",
			strerror(errno));
		exit(1);
	}

	memzero(addr);
	addr.sipx_type = NCP_PTYPE;

	if (bind(data.ncp_fd, (struct sockaddr *)&addr, sizeof(addr)) == -1)
	{
		fprintf(stderr, "\nbind: %s\n",
			strerror(errno));
		fprintf(stderr,
			"\nMaybe you want to use \n"
			"ipx_configure --auto_interface=on --auto_primary=on\n"
			"and try again after waiting a minute.\n\n");
		exit(1);
	}

	addrlen = sizeof(addr);

	if (getsockname(data.ncp_fd, (struct sockaddr *)&addr, &addrlen)==-1)
	{
		perror("getsockname ncp socket");
		close(data.ncp_fd); close(data.wdog_fd);
		exit(1);
	}

	addr.sipx_port = htons(ntohs(addr.sipx_port) + 1);

	if (bind(data.wdog_fd, (struct sockaddr *)&addr, sizeof(addr)) == -1)
	{
		fprintf(stderr, "bind(wdog_sock, ): %s\n",
			strerror(errno));
		exit(1);
	}

#if NCP_MOUNT_VERSION>1

	data.message_fd = socket(AF_IPX, SOCK_DGRAM, PF_IPX);
	if (data.message_fd == -1)
	{
		fprintf(stderr, "could not open message socket: %s\n",
			strerror(errno));
		exit(1);
	}

	addr.sipx_port = htons(ntohs(addr.sipx_port) + 1);

	if (bind(data.message_fd, (struct sockaddr *)&addr,sizeof(addr)) == -1)
	{
		fprintf(stderr, "bind(message_sock, ): %s\n",
			strerror(errno));
		exit(1);
	}

	if (strlen(mount_point) < sizeof(data.mount_point))
	{
		strcpy(data.mount_point, mount_point);
	}
#endif

	flags = MS_MGC_VAL;

	strcpy(mount_name, spec->server);
	strcat(mount_name, "/");
	strcat(mount_name, spec->user);

	result = mount(mount_name, mount_point, "ncpfs", flags, (char *)&data);

	if (result < 0)
	{
		printf("mount failed\n");
		exit(1);
	}

	if ((conn = ncp_open_mount(mount_point, &err)) == NULL)
	{
		com_err("ncpmount", err, "attempt to open mount point");
		umount(mount_point);
		exit(1);
	}

	if ((err = ncp_login_user(conn, spec->user, spec->password)) != 0)
	{
		struct nw_property p;
		struct ncp_prop_login_control *l
			= (struct ncp_prop_login_control *)&p;

		if (conn->completion != NCP_GRACE_PERIOD)
		{
			com_err("ncpmount", err, "in login");
			fprintf(stderr, "Login denied\n");
			ncp_close(conn);
			umount(mount_point);
			exit(1);
		}

		fprintf(stderr, "Your password has expired\n");

		if ((err = ncp_read_property_value(conn, NCP_BINDERY_USER,
						   spec->user, 1,
						   "LOGIN_CONTROL", &p)) == 0)
		{
			fprintf(stderr, "You have %d login attempts left\n",
				l->GraceLogins);
		}
	}

	if ((err = ioctl(conn->mount_fid, NCP_IOC_CONN_LOGGED_IN, NULL)) != 0)
	{
		com_err("ncpmount", err, "in logged_indication");
		ncp_close(conn);
		umount(mount_point);
		exit(1);
	}
	ncp_close(conn);

        ment.mnt_fsname = mount_name;
        ment.mnt_dir = mount_point;
        ment.mnt_type = "ncpfs";
        ment.mnt_opts = "rw";
        ment.mnt_freq = 0;
        ment.mnt_passno= 0;

        if ((fd = open(MOUNTED"~", O_RDWR|O_CREAT|O_EXCL, 0600)) == -1)
        {
                fprintf(stderr, "Can't get "MOUNTED"~ lock file");
                exit(1);
        }
        close(fd);
	
        if ((mtab = setmntent(MOUNTED, "a+")) == NULL)
        {
                fprintf(stderr, "Can't open " MOUNTED);
                exit(1);
        }

        if (addmntent(mtab, &ment) == 1)
        {
                fprintf(stderr, "Can't write mount entry");
                exit(1);
        }
        if (fchmod(fileno(mtab), 0644) == -1)
        {
                fprintf(stderr, "Can't set perms on "MOUNTED);
                exit(1);
        }
        endmntent(mtab);

        if (unlink(MOUNTED"~") == -1)
        {
                fprintf(stderr, "Can't remove "MOUNTED"~");
                exit(1);
        }

	return 0;
}	

static void
usage(void)
{
	printf("usage: %s [options] mount-point\n", progname);
        printf("Try `%s -h' for more information\n", progname);
}

static void
help(void)
{
        printf("\n");
        printf("usage: %s [options] mount-point\n", progname);
        printf("\n"
	       "-S server      Server name to be used\n"
               "-U username    Username sent to server\n"
	       "-V volume      Volume to mount, for NFS re-export\n"
               "-u uid         uid the mounted files get\n"
               "-g gid         gid the mounted files get\n"
               "-f mode        permission the files get (octal notation)\n"
               "-d mode        permission the dirs get (octal notation)\n"
	       "-c uid         uid to identify the connection to mount on\n"
	       "               Only makes sense for root\n"
	       "-t time_out    Waiting time (in 1/100s) to wait for\n"
	       "               an answer from the server. Default: 60\n"
	       "-r retry_count Number of retry attempts. Default: 5\n"
               "-C             Don't convert password to uppercase\n"
               "-P password    Use this password\n"
               "-n             Do not use any password\n"
               "               If neither -P nor -n are given, you are\n"
               "               asked for a password.\n"
               "-h             print this help text\n"
	       "-v             print ncpfs version number\n"
               "\n");
}
