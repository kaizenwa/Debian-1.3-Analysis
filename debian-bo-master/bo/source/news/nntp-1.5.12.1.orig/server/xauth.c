#ifndef lint
static char	sccsid[] = "@(#)$Id: xauth.c,v 1.2 1994/12/09 02:52:18 sob Exp sob $";
#endif

/*
 * Simple user/password authentication
 *
 * client must supply "xauthinfo user <userid>"
 * followed by "xauthinfo pass <password>"
 * which will be looked up in the server machine's password file.
 * Password must match userid, userid must have gid matching the
 * /etc/group entry for "nntp"
 *
 * note that passwords travel over the network in plaintext.  This
 * can be a problem but isn't easy to remedy.  At least it's as safe
 * as logging in over the network would be
 *
 */

#include "common.h"
#include <grp.h>

extern timeout();
extern char *crypt();

#ifdef	AUTH

extern int	Needauth;
extern char	User[];
extern char	Host[];

/* forward declarations */
void getpass();
void getuser();

 
#ifdef GENAUTH

static char *genauth_ret;

/* returns:
	-1 for problem (such as no such authenticator etc.)
	0 for authentication succeeded
	1 for authentication failed

   It sets and reads the various control parameters like canpost/canread
   etc., based on a INN-like nnrp_access file being given to it via
   the authenticator.
 */
static int
genauth(av)
char	*av[];
{
	char path[COPYSIZE], *fields[6], *p;
	int pid, i, pan[2], exit_status;
#if defined(USG) || defined(BSD_44)
	int status;
#else
	union wait status;
#endif
	struct stat stb;
	char *malloc();

	av += 2;

	if (!*av) {
		printf("%d no authenticator\r\n", ERR_CMDSYN);
		return(-1);
	}

	/* check for ../.  I'd use strstr, but there doesn't appear to
	   be any other references for it, and I don't want to break
	   portability */
	for (p = av[0]; *p; p++)
		if (*p == '.' && p[1] == '.' && p[2] == '/') {
			printf("%d ../ in authenticator %s\r\n",
					 ERR_CMDSYN, av[0]);
			return(-1);
		}

	(void)sprintf(path, "%s/%s", GENAUTH, av[0]);

	if (stat(path, &stb) || !(stb.st_mode&S_IXUSR)) {
		printf("%d No such authenticator %s\r\n", ERR_FAULT, av[0]);
		return -1;
	}

	/* Create a pipe. */
	if (pipe(pan) < 0) {
		syslog(LOG_ERR, "can't pipe for %s %m", av[0]);
		return -1;
	}

	for (i = 0; (pid = fork()) < 0; i++) {
		if (i == 10) {
			printf("%d Can't fork\r\n", ERR_FAULT);
			syslog(LOG_ERR, "can't fork %s %m", av[0]);
			return -1;
		}
		syslog(LOG_INFO, "can't fork %s -- waiting", av[0]);
		(void)sleep(5);
	}

	/* Run the child, with redirection. */
	if (pid == 0) {
		(void)close(fileno(stderr));	/* close existing stderr */
		(void)close(pan[0]);

		/* stderr goes down the pipe. */
		if (pan[1] != 2) {
			if ((i = dup2(pan[1], 2)) != 2) {
				syslog(LOG_ERR, "can't dup2 %d to %d got %d %m",
					pan[1], 2, i);
				_exit(1);
			}
			(void)close(pan[1]);
		}

		/*syslog(LOG_ERR, "path: %s, av[0]: %s, av[1]: %s\n", path, av[0], av[1]);*/

		(void)execv(path, av);
		printf("%s\r\n", ERR_COMMAND);

		syslog(LOG_ERR, "can't execv %s %m", path);
		_exit(1);
	}

	(void)close(pan[1]);
	i = read(pan[0], path, sizeof(path));

	if (p = index(path, '\n'))
		*p = '\0';

	if (genauth_ret)
		free(genauth_ret);

	genauth_ret = malloc(strlen(path)+1);

	if (genauth_ret)
		strcpy(genauth_ret, path);

	while (pid != wait(&status)) ;
#if defined(USG) || defined(BSD_44)
	exit_status = (status >> 8) & 0xff;
#else
	exit_status = status.w_T.w_Retcode;
#endif

	/*syslog(LOG_ERR, "%s (%d) returned: %d %s %d\n", av[0], pid, i, path, status);*/
	/* Split "host:permissions:user:pass:groups" into fields. */
	for (fields[0] = path, i = 0, p = path; *p; p++)
		if (*p == ':') {
			*p = '\0';
			fields[++i] = p + 1;
		}

	canread = canpost = canxfer = 0;

	for (p = fields[1]; *p; p++)
	if (index("Rr", *p))
		canread = 1;
	else if (index("Pp", *p))
		canpost = 1;
	else if (index("Xx", *p))
		canxfer = 1;
	else if (index("Bb", *p))
		canread = canxfer = 1;

	(void)strcpy(Host, fields[0]);
	(void)strcpy(User, fields[2]);

	/*(void)strcpy(pass, fields[3]);*/

	if (strcmp(fields[4], "*") == 0)
		fields[4] = "any";
	ngpermcount = get_nglist(&ngpermlist, fields[4]);

	/*(void)strcpy(writeaccess, fields[5]); future work? */

	/*for (i = 0; ngpermlist[i]; i++)
		printf("permlist[%d] = %s\n", i, ngpermlist[i]);*/

	return !exit_status;
}

#endif

static int
argcount(cnt, shouldbe)
int cnt;
int shouldbe;
{
	if (cnt != shouldbe) {
		printf("%d Syntax error\r\n", ERR_CMDSYN);
		fflush(stdout);
		return(1);
	}
	return(0);
}

void
doxauthcap(argc,argv)
int argc;
char *argv[];
{
	printf("%d authcap not implemented\r\n", ERR_COMMAND);
	fflush(stdout);
	return;
}

void
doxauthsys(argc,argv)
int argc;
char *argv[];
{
	printf("%d authsys not implemented\r\n", ERR_COMMAND);
	fflush(stdout);
	return;
}

void
doxauthinfo(argc,argv)
int argc;
char *argv[];
{
	if (!Needauth) {
		printf("%d Authorization already completed\r\n", ERR_AUTHREJ);
		fflush(stdout);
		return;
	}

	if (!strcasecmp(argv[1],"user")) {
		if (argcount(argc, 3))
			return;
		if (strlen(User)) {
			printf("%d USER already specified\r\n", ERR_AUTHREJ);
			fflush(stdout);
			return;
		}
		getuser(argv[2]);
		return;
	}

	if (!strcasecmp(argv[1],"pass")) {
		if (argcount(argc, 3))
			return;
		if (strlen(User) < 1) {
			printf("%d USER required first\r\n", ERR_AUTHREJ);
			fflush(stdout);
			return;
		}
		getpass(argv[2]);
		return;
	}
#ifdef GENAUTH
	if (!strcasecmp(argv[1], "generic")) {
		strcpy(User, "<none>");
		switch (genauth(argv)) {
			case 1:
				syslog(LOG_NOTICE, "%s auth %s@%s (%s)",
					 hostname, User, Host, genauth_ret ?
						genauth_ret: "");
				printf("%d Authentication succeeded\r\n",
					 OK_AUTH);
				fflush(stdout);
				Needauth = 0;
				break;
			case 0:
				syslog(LOG_NOTICE, "%s bad_auth %s", hostname,
					 User);
				printf("%d Authentication failed\r\n", ERR_ACCESS);
				fflush(stdout);
				exit(1);
			default:
				/* lower level has issued Reply */
				break;
		}
		return;
	}
#endif
	
#ifdef	GENAUTH
	printf("%d user Name|pass Password|generic <prog> <args>\r\n", ERR_CMDSYN);
#else
	printf("%d user Name|pass Password\r\n", ERR_CMDSYN);
#endif
	fflush(stdout);
}

/* get userid and prompt for password */
void
getuser(p)
char *p;
{
	strncpy(User,p,8);
	User[8] = 0;
	/* get the password */
	printf("%d PASS required\r\n", NEED_AUTHDATA);
	fflush(stdout);
}

/* password */
void
getpass(p)
char *p;
{
	static char pass[10];
	char *namep;
	struct passwd *pwd;
	struct group *grp;
	extern struct group *getgrnam();

	strncpy(pass,p,8);
	pass[8] = 0;
	/* check for valid login */
	pwd = getpwnam(User);
	namep = NULL;

	if (pwd != NULL)
		namep = crypt(pass, pwd->pw_passwd);
	
	grp = getgrnam("nntp");

	if (grp == NULL || pwd == NULL || namep == NULL
			|| strcmp(namep, pwd->pw_passwd)
			|| pwd->pw_gid != grp->gr_gid) {
#ifdef SYSLOG
		syslog(LOG_ERR, "AUTHENTICATION ERROR");
#endif
		printf("%d Authentication error\r\n",ERR_ACCESS);
		(void) fflush(stdout);
		(void) fflush(stdout);
		exit(1);
	}

#ifdef SYSLOG
#ifdef LOG
	syslog(LOG_INFO, "user %s", User);
#endif
#endif
	printf("%d Authentication accepted\r\n",OK_AUTH);
	fflush(stdout);
	Needauth = 0;
}

#endif /* AUTH */
