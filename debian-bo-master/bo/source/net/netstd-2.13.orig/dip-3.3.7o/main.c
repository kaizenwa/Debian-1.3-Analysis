/*
 * dip		A program for handling dialup IP connecions.
 *		This program handles the connections needed for dialup
 *		IP links, like SLIP or PPP.  It can handle both incoming
 *		and outgoing connections, using password security for
 *		incoming connections.  The outgoing connections use the
 *		system's dial(3) library if possible.
 *
 * Usage: 	dip [-v] -k [-l tty]
 *		dip -t [-v]
 *		dip [-v] [-m mtu] [-p proto] [telno | script]
 *		diplogin [-v]
 *		diplogini [-v]
 *		dip -a [-v]
 *
 * Version:	@(#)main.c	3.3.7o-uri	02/08/96
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *
 * Modified:    Uri Blumenthal <uri@watson.ibm.com>
 *              Copyright 1994 
 *
 *		Paul Cadach, <paul@paul.east.alma-ata.su>
 *		(C) 1994
 *
 *              Pauline Middelink <middelin@calvin.iaf.nl>
 *              - Changed various paths for FSSTND
 *              - Allow exit with optional exitcode.
 *              - Added setproctitle, so dip shows the connection under proc-ps
 *              - Removed the strange parsing of argv[0] which overwrote
 *                part of the string.
 *              - Added code to ask interactivly for username. (-a)
 *              - added "timeout <n>" for disconnects after <n> idle seconds
 *              - various source changes
 *              - Well, and since everybody is doing it: (C) 1994.
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#define GLOBAL
#include "dip.h"


#define VERSION	"3.3.7o-uri (8 Feb 96)"


char *Version = "@(#) dip " VERSION;


struct dip	mydip;			/* global DIP entry		*/
int		opt_v  = 0;		/* debug flag			*/
int		opt_i  = 0;		/* server-mode flag     	*/
struct response *chat = NULL;		/* List of modem's answers      */
struct ifcfg    *cfg = NULL;		/* List of config's calls	*/
 
char	       **the_argv = (char **)0;
char           **the_envp = (char **)0;
int              the_argc = 0;

extern int    onexit_argc;              /* declarations for "onexit" */
extern char **onexit_argv;
extern int    do_shell (int, char**);
extern int    arg_clean (int, char**);

/* Cleanup before exit. */
void
cleanup(void)
{
  int  disc;

  /* execute the onexit command(s) */
  if (onexit_argc != 0 && onexit_argv != NULL)
    do_shell (onexit_argc, onexit_argv);
  arg_clean (onexit_argc, onexit_argv);
  

  if (tty_askfd() >= 0) {
    if(tty_get_disc(&disc) == 0) {
      if ((disc > 0) && (disc != N_TTY))
        detach(&mydip);
    }
    if (tty_getmode())
      tty_login_close();
    else
      tty_close();
  }
}

#if !defined(HAS_STPCPY)
char *stpcpy(char *dest, const char *src)
{
  char *s = (char *)src, *d = dest;
  
  while((*d = *s++) != '\0')
    ++d;
  return(d);
}
#endif

static void
catch(int sig)
{
  (void) sig;
  signal(SIGHUP, SIG_IGN);
  cleanup();
  exit(1);
}

void
log_cleanup(void)
{
  if (tty_askfd() >= 0) {
    if (tty_getmode())
      tty_login_close();
    else
      tty_close();
  }
}

static void
log_catch(int sig)
{
  (void) sig;
  signal(SIGHUP, SIG_IGN);
  log_cleanup();
  exit(1);
}


void
dip_addchat(struct response **seq, char *string, int code)
{
  struct response *resp;
  
  resp = malloc(sizeof(*resp));
  resp->string = strdup(string);
  resp->code = code;
  resp->next = *seq;
  *seq = resp;
}


/* Kill a running DIP process. */
static void
kill_dip(void)
{
  char buff[128];
  int pid;
  FILE *fp;

  fp = fopen(_PATH_DIP_PID, "r");
  if (fp == NULL) {
	fprintf(stderr, "DIP: cannot open %s: %s\n",
			_PATH_DIP_PID, strerror(errno));
	return;
  }
  (void) fgets(buff, 128, fp);
  pid = atoi(buff);
  seteuid(getuid());
  if (kill(pid, SIGTERM)) {
	fprintf(stderr, "DIP: cannot kill process %d: %s\n",
			pid, strerror(errno));
	return;
  } else printf("DIP: process %d killed.\n", pid);
  seteuid(0);
  (void) fclose(fp);
  (void) unlink(_PATH_DIP_PID);
}


static void new_kill_dip(char *nam)
{
  char buf[128];
  int pid = 0;
  FILE *fp;
#if HAVE_V2_LOCKFILES
  int fu_read = 0;
#endif /* HAVE_V2_LOCKFILES */

  if ((nam == (char *)0) || (nam[0] == '\0')) { 
    /* tty name was not given */
    if (opt_v == 1) 
       fprintf(stderr, "No tty name given, will use /etc/dip.pid...\n");
    kill_dip();
    return;
  }
   
  sprintf(buf, "%s/LCK..%s", _PATH_LOCKD, nam);

  fp = fopen(buf, "r");
  if (fp == (FILE *)0) {
	fprintf(stderr, "DIP: cannot open %s: %s\n",
			buf, strerror(errno));
	return;
  }
#if HAVE_V2_LOCKFILES
  fu_read = read(fileno(fd), &pid, sizeof(pid));
#else
  (void) fscanf(fp, "%d", &pid);
#endif /* HAVE_V2_LOCKFILES */
  (void) fclose(fp);
  seteuid(getuid());
  if (kill(pid, SIGTERM)) {
	fprintf(stderr, "DIP: cannot kill process %d: %s\n",
			pid, strerror(errno));
	return;
  } else printf("DIP: process %d killed.\n", pid);
  seteuid(0);
  (void) unlink(buf);
}


/* Fill in the global DIP entry. */
static void
dip_init(void)
{
  struct passwd *pw;
  struct hostent *hp;

  if ((pw = getpwuid(getuid())) == (struct passwd *)NULL) {
	fprintf(stderr, "You do not exist.  Go away!\n");
	exit(-1);
  }
  memset((char *) &mydip, 0, sizeof(struct dip));
  strncpy(mydip.name, pw->pw_name, sizeof(mydip.name));
  strcpy(mydip.home, "/tmp");

  if (gethostname(mydip.local, 128) < 0) {
	perror("gethostname");
	exit(-1);
  }
  if ((hp = gethostbyname(mydip.local)) == (struct hostent *)NULL) {
	herror(mydip.local);
	strcpy(mydip.local, "");
	exit(-1);
  }
  strncpy(mydip.local,    hp->h_name, sizeof(mydip.local));
  strncpy(mydip.hom_host, hp->h_name, sizeof(mydip.hom_host));
  memcpy((char *) &mydip.loc_ip, (char *) hp->h_addr_list[0], hp->h_length);
  memcpy((char *) &mydip.hom_ip, (char *) hp->h_addr_list[0], hp->h_length);

  strcpy(mydip.protocol, DEF_PROT);
  mydip.protonr = get_prot(mydip.protocol);
  mydip.mtu = DEF_MTU;
}


static void
usage(void)
{
  fprintf(stderr, "Usage: dip -i [-v]\n");
  fprintf(stderr, "       dip [-i] -a [-v]\n");
  fprintf(stderr, "       diplogin [-v]\n");
  fprintf(stderr, "       diplogini [-v]\n");
  fprintf(stderr, "       dip [-v] -k [-l ttyname]\n");
  fprintf(stderr, "       dip -t [-v]\n");
  fprintf(stderr, "       dip [-v] [-m mtu] [-p proto] [telno | script]\n");
  exit(-1);
}

void
setproctitle(char *fmt, ...)
{
  int i;
  va_list	ap;
  
  static char title[128];
  
  va_start(ap, fmt);
  memset(title, 0, sizeof(title));
  vsprintf(&title[0], fmt, ap);
  va_end(ap);
  
#if 0
  the_argv[0] = (char *) &title[0];
#else
  for (i = 0; i < the_argc; i++) {
    memset(the_argv[i], 0, strlen(the_argv[i]));
    /* the_argv[i] = (char *) 0; */
  }
  strcpy(the_argv[0], title);
#endif

}

int
main(int argc, char **argv, char **envp)
{
  char path[128];
  FILE *fp;
  register int s;
  register char *sp;
  int opt_a, opt_k, opt_t;

  the_argv = argv;
  the_envp = envp;
  the_argc = argc;

  /* Setup. */
  dip_init();

  bzero(path, sizeof(path));
  opt_i = 0;
  opt_a = 0;
  opt_k = 0;
  opt_t = 0;

  /*
   * This is a kludge for bad "login" programs which cannot pass
   * arguments. Needless to say, this includes the SLS login(8)
   * program, and most BSD derivatives... -FvK
   */
  /*
   * Added "login" below, so diplogin can be called directly
   * from getty_ps... SJS
   */
  if ((sp = strrchr(argv[0], '/')) == NULL) 
    sp = argv[0];
  else
    sp++;

  if (*sp == '-') sp++;

  if ((strcmp(sp, "diplogin") == 0) || (strcmp(sp, "login")    == 0)) 
    opt_i = 1;
  if (strcmp(sp, "diplogini") == 0)
    opt_i = opt_a = 1;
  
  /* Scan command line for any arguments. */
  opterr = 0;
  while ((s = getopt(argc, argv, "aikl:m:p:vt")) != EOF) switch(s) {
	case 'a':
		opt_a = 1;
		opt_i = 1; /* probably needed! uri */
		break;

	case 'i':
		opt_i = 1;
		break;

	case 'k':
		opt_k = 1;
		break;

        case 'l':
		strncpy(path, optarg, sizeof(path));
		break;

	case 'm':
		mydip.mtu = atoi(optarg);
		if (mydip.mtu <= 0 || mydip.mtu > 32767) usage();
		break;

	case 'p':
		strncpy(mydip.protocol, optarg, sizeof(mydip.protocol));
		mydip.protonr = get_prot(mydip.protocol);
		if (mydip.protonr == 0) usage();
		break;

	case 't':
		opt_t = 1;
		break;

	case 'v':
		opt_v = 1;
		break;

	default:
		usage();
  }

  /* make sure we log the attachments */
  openlog("dip", LOG_PID, LOG_DAEMON);
  
  printf("DIP: Dialup IP Protocol Driver version %s\n", VERSION);
  printf("Written by Fred N. van Kempen, MicroWalt Corporation.\n\n");

  /* Are we called to kill off a DIP process? */
  if (opt_k == 1) {
    new_kill_dip(path);
    exit(0);
  }
  
  dip_addchat(&chat, "OK",          0);
  dip_addchat(&chat, "CONNECT",     1);
  dip_addchat(&chat, "ERROR",       2);
  dip_addchat(&chat, "BUSY",        3);
  dip_addchat(&chat, "NO CARRIER",  4);
  dip_addchat(&chat, "NO DIALTONE", 5);

  (void) signal(SIGINT,  catch);
  (void) signal(SIGTERM, catch);
  (void) signal(SIGHUP,  log_catch);
  (void) signal(SIGQUIT, catch);  

  /* Verbose mode? -> print mydip values */
  if (opt_v == 1) {
    printf("DIP: name=%s home=%s\n", mydip.name, mydip.home);
    printf("     host=%s IP=%s\n", mydip.local, inet_ntoa(mydip.loc_ip));
    printf("     prot=%s MTU=%d\n\n", mydip.protocol, mydip.mtu);
  }

  /* Are we going to be a dialIN server? */
  if (opt_i == 1) {
    /* Enforce Proxy ARP for now */
    mydip.proxyarp = 1;

    if (opt_a == 1) sp = NULL;
    else if (optind == argc) sp = mydip.name;
    else sp = argv[optind];
    do_login(sp);
    /*NOTREACHED*/
  }

  /* Are we running in TERMINAL/DIALOG mode? */
  if (opt_t == 1) {
    if (optind != argc) usage();
    do_command(stdin);
    /*NOTREACHED*/
  }

  /* No, so we are running in dialOUT mode. */
  if (optind != (argc - 1)) usage();
  strncpy(path, argv[optind], sizeof(path) - sizeof(DIP_SFX) + 1);
  if ((sp = strrchr(path, '/')) != (char *)NULL) sp++;
    else sp = path;
  if (strchr(sp, '.') == (char *)NULL) strcat(path, DIP_SFX);

  /* set euid to ruid */
  if (setreuid(geteuid(), getuid()) != 0){
   	fprintf(stderr, "dip: setreuid(%d, %d): %s\n", geteuid(), getuid(),
		strerror(errno));
	exit(-1);
  }
  /* open file with real uid */
  if ((fp = fopen(path, "r")) == (FILE *)NULL) {
	fprintf(stderr, "dip: %s: %s\n", path, strerror(errno));
	exit(-1);
  }
  /* set uids back */
  if (setreuid(geteuid(), getuid()) != 0){
        fprintf(stderr, "dip: setreuid(%d, %d): %s\n", geteuid(), getuid(),
                strerror(errno));
        exit(-1);
  }
  (void) setbuf(fp, (char *)NULL);
  do_command(fp);

  /*NOTREACHED*/
  return(-1);
}
