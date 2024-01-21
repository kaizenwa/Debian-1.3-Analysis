#define I_IOCTL
#define I_ERRNO
#define I_STRING
#define I_SYS
#define I_STAT
#define I_SIGNAL
#define I_STRING
#define I_PTY
#define I_LIMITS
#define I_STROPT
#define I_PROCESS
#include "includes.h"

#include "debug.h"

#ifdef SYSV
#if defined(SVR4) || defined(SCO)
int grantpt(int);
char *ptsname(int);
int unlockpt(int);
#endif
#endif

int pty_pid;

/*
 * This area heavily modified to use Dynix/PTX ptys
 * basically just dropped in a replacement open_pty command
 * Barry Boone (boone@a.cs.okstate.edu), June 4, 1994
 * Note that *prog is ignored... the shell is determined by SHELL if present,
 * or uses /bin/sh
 */


/* I've tried to make things more readable.  Hopefully I didn't invent
 * too many new errors in the process.  First you have about 5 different
 * versions of getpseudotty.  You need to figure out which will work or
 * write your own to add to the list...  Maybe if you are really lucky
 * your system already has this.  Then the terminal settings in open_pty
 * will probably need some OS specific command.  But without this you should
 * get some type of result still to tell you that the getpseudotty is working.
 */


#ifdef SYSV
#define HAS_PSEUDO
#define MASTERLINE "/dev/ptmx"
int getpseudotty(char **Slaveline, char **Masterline)
{
  int masterfd = -1;
  void (*savesig)(int);

  *Masterline = MASTERLINE;

  if ((masterfd = open(*Masterline, O_RDWR, 0)) < 0)
    return -1;
  savesig = signal(SIGCHLD, SIG_DFL);
  if (grantpt(masterfd) < 0) {
    x__close(masterfd);
    sigset(SIGCHLD, savesig);
    return -1;
  }
  sigset(SIGCHLD, savesig);
  if (unlockpt(masterfd) < 0) {
    x__close(masterfd);
    return -1;
  }
  *Slaveline = ptsname(masterfd);
  if (*Slaveline == NULL) {
    x__close(masterfd);
    return -1;
  }
  return masterfd;
}
#endif

#ifdef _AIX
#define HAS_PSEUDO
#define MASTERLINE "/dev/ptc"

int getpseudotty(char **Slaveline, char **Masterline)
{
  int masterfd = -1;

  *Masterline = MASTERLINE;

  if ( (masterfd=open(*Masterline,O_RDWR,0600)) < 0 )
      return -1; /* No ptys available */
  *Slaveline = ttyname(masterfd);
  if(masterfd < 0) return -1;
  return masterfd;
}
#endif

#ifdef sgi
# define HAS_PSEUDO
int getpseudotty(char **Slaveline, char **Masterline)
{
  int masterfd = -1;

  *Masterline = NULL;
  *Slaveline = _getpty(&masterfd, O_RDWR, 0600, 0);
  
  if (*Slaveline == NULL)
    return -1; /* no ptys available */
  
  /* Don't check slave - have to assume it's going to work */
  if(masterfd < 0) return -1;
  return masterfd;
}
#endif /* sgi */



#ifdef USE_TTYNAME
#define HAS_PSEUDO
#define MASTERLINE "/dev/ptyX-"
#define FIRST "pqrstuvwxyz"

int getpseudotty(char **Slaveline, char **Masterline)
{
  int masterfd = -1, slavefd = -1, j;
  char *first=FIRST;
  static char masterline[sizeof(MASTERLINE)];

  *Masterline = strcpy(masterline,MASTERLINE);

#define MFIRST  (sizeof (MASTERLINE) - 3)

  for (j = 0; first[j]; ++j) {
     masterline[MFIRST] = first[j];
     if ((masterfd = open(masterline, O_RDWR)) >= 0) {
         basename(*Slaveline = (char *)ttyname(masterfd))[0] = 't';
         if ((slavefd = open(*Slaveline, O_RDWR)) >= 0) {
             x__close(slavefd);
             break;
         }
         x__close(masterfd);
     }
  }

  if(masterfd < 0) return -1;
  return masterfd;
}
#endif /* USE_TTYNAME */


	/* Here is the routine, as complicated as it is... */

#ifndef HAS_PSEUDO

#ifdef __hpux
#define MASTERLINE "/dev/ptym/ptyXY"
#define SLAVELINE "/dev/pty/ttyXY"
#define FIRST "pqrstuv"
#define SECOND "0123456789abcde"
#else
#define MASTERLINE "/dev/ptyXY"
#define SLAVELINE "/dev/ttyXY"
#define SECOND "0123456789abcdef"
#ifdef convex
#define FIRST "pqrstonmlkjihgfe"
#else
#define FIRST "pqrstuvwxyz"
#endif
#endif

int getpseudotty(char **Slaveline, char **Masterline)
{
  int masterfd = -1, slavefd = -1;
  int i, j;
  char *first = FIRST;
  char *second = SECOND;
  static char masterline[sizeof(MASTERLINE)];
  static char slaveline[sizeof(SLAVELINE)];

#define SFIRST  (sizeof (SLAVELINE) - 3)
#define SSECOND (sizeof (SLAVELINE) - 2)

                                /* Ok. Stuff for not sgi. */
#define MFIRST  (sizeof (MASTERLINE) - 3)
#define MSECOND (sizeof (MASTERLINE) - 2)

  *Masterline = strcpy(masterline,MASTERLINE);
  *Slaveline = strcpy(slaveline,SLAVELINE);

  /* get a pseudo tty */
  for (j = 0; first[j]; ++j) {
    struct stat statbuff;
    
    masterline[MFIRST] = first[j];
    masterline[MSECOND] = '0';
    
    if (stat (masterline, &statbuff) < 0)
      continue; /* no ptys on this bank available */

    for (i=0; second[i]; ++i) {
      for (; second[i]; ++i) {
        masterline[MSECOND] = second[i];
        if ((masterfd = open (masterline, O_RDWR, 0)) >= 0)
          break;
      }
      if (! second[i]) break;

                                /* Ok. now check to make sure we can */
                                /* open the slave as well. */
      slaveline[SFIRST] = masterline[MFIRST];
      slaveline[SSECOND] = masterline[MSECOND];

      if ((slavefd = open (slaveline, O_RDWR, 0)) >= 0) break;
      x__perror("Term: Slave tty");
      x__close(masterfd);
    };
    if (second[i]) break;
  }

  if (! first[j])
    return -1;
  
  DEBUG_FP(stderr, "%s: opened pty master=%s(%u) slave=%s(%u)\n",
              term_server, masterline, masterfd, slaveline, slavefd);

#ifdef SUIDROOT
  if (fchmod (slavefd, 0622))
    return -2;
#endif
  
  x__close(slavefd);

  if(masterfd < 0) return -1;
  return masterfd;
}
#endif /* HAS_PSEUDO */


	/* OK, this routine does opens a pty and runs a command in it.  */
	/* I've tried to make it more readable, but it is still a bitch. */

int open_pty(char *prog)
{
  int masterfd = -1, slavefd = -1, i, pid;
  char *slaveline=NULL, *masterline=NULL;
#if !defined(DYNIXPTX) && !defined(USE_SPAWN)
  int pip[2];
#endif

	/* Here is where we get the tty */

  if ( (masterfd = getpseudotty(&slaveline, &masterline) ) < 0)
    return -1;

	/* Next we open some sort of pipe so we know if we are successful */

  fflush (stdout);

#if defined(DYNIXPTX) || !defined(USE_SPAWN)
  if (pipe(pip) < 0) 	/* This is only for debugging purposes */
      pip[0] = pip[1] = -1;
#endif

  /* Now we fork, and save the parent some work. */

#ifndef USE_SPAWN
  if ((pid = x__fork()) < 0) {
    x__perror("Term: fork");
    return -3;
  }
  if (! pid) {
#else
  { char iov[10];
#endif
    char **a, *b[2];
    char argv0[PATH_MAX];
    char *progname=NULL;
    int loginshell=0;

    b[0]=b[1]=NULL;

#ifndef USE_SPAWN
    x__close(0);
    x__close(1);
    x__close(2);
#ifndef DYNIXPTX
    lose_ctty();
#else
    setpgrp();
#endif    
#endif

	/* make it control tty */
    if ((slavefd = open (slaveline, O_RDWR, 0)) < 0) exit(1);

	/* I don't understand this but I'll leave it */
#if defined(USE_VHANGUP) && !defined(USE_SPAWN)
    signal (SIGHUP, SIG_IGN);
    vhangup ();
    signal (SIGHUP, SIG_DFL);
    
    if ((slavefd = open (slaveline, O_RDWR, 0)) < 0) exit(1);
#endif

#ifdef USE_TIOCSCTTY
    ioctl(slavefd, TIOCSCTTY, 0);
#endif

#ifdef SYSV
    if (ioctl(slavefd, I_PUSH, "ptem") < 0) {
      exit(1);
    }
    if (ioctl(slavefd, I_PUSH, "ldterm") < 0) {
      exit(1);
    }
#ifdef SVR4
    if (ioctl(slavefd, I_PUSH, "ttcompat") < 0) {
      exit(1);
    }
#endif /* SVR4 */
#endif /* SYSV */


	/* Set up input and output */
	/* Let's close things down before doing the exec */
	/* Here we reset/set the terminal */
#ifndef USE_SPAWN
    x__dup2 (slavefd, 0);
    x__dup2 (slavefd, 1);
    x__dup2 (slavefd, 2);  
    if (slavefd > 2) x__close(slavefd);

#ifdef DYNIXPTX
    for (i = 3; i < 256; i++)
      x__fcntl(i,F_SETFD,1);

    terminal_restore(0,0);
#else /* DYNIXPTX */
    for (i = 3; i < 256; ++i) 
      if (i != pip[1])  x__close(i);
    if (pip[1] >= 0) if (x__fcntl (pip[1], F_SETFD, 1) == -1) {
      if (pip[1] >= 0) x__close (pip[1]);
      if (pip[0] >= 0) x__close (pip[0]);
      pip[0] = pip[1] = -1;
    }
#if !(defined(SVR4)) && !(defined(__hpux)) && !defined(DYNIXPTX)
    terminal_new(0); 
#endif
#endif /* DYNIXPTX */
#else /* USE_SPAWN */
    iov[0] = iov[1] = iov[2] = slavefd;
    iov[3] = iov[4] = iov[5] = iov[6] = iov[7] = iov[8] = iov[9] = -1;

    for (i = 0; i < 10; i++)	 /* changed */
      x__fcntl(i,F_SETFD,FD_CLOEXEC);

    terminal_new(slavefd);
#endif /* USE_SPAWN */

	/* Lets build the exec() arguments */

  /* find out what shell to run. */
    a = rebuild_arg(prog);
    if ( !a )
      a = b;
    else if ( !a[0] )
      a = b;
    else {
      while(strchr(a[0],'=') != NULL && ! strncmp(a[0],"-D",2)) {
        term_putenv(&a[0][2]);
        a = &a[1];
        if ( !a[0] ) break;
      };
      if ( !a[0] ) 
        a = b;
      else if ( !*a[0] )
        a = b;
    }

    if(!rshtype) {
      if ( !a[0] )  { a[0] = getenv("SHELL"); loginshell=1; }
      if ( !a[0] )  a[0] = "/bin/sh";
      progname = a[0];
      if (loginshell) {
        sprintf(argv0, "-%s", strrchr(a[0], '/') ?
               (char*)(strrchr(a[0], '/') + 1 ) : (char*) a[0]);
        a[0] = argv0;
      }
    }
#ifndef DYNIXPT
    else {
      if ( !a[0] ) { a[0] = getenv("SHELL"); loginshell=1; }
      if ( !a[0] ) a[0] = "/bin/sh";
    }
#endif

	/* OK, now just a few misc. things before executing */

#if !defined(hcx)
     /*  If the strings are too long, the printfs blocks on hcx ! */
    printf ("Remote: term %s\r\n", str_version(VERSION));
    printf ("tty %s. exec %s\r\n", slaveline, a[0]);
#endif
	/* Finally we can execute */

    setgid(getgid());
    setuid(getuid());
#ifndef USE_SPAWN
#ifdef DYNIXPTX
    execl(progname, progname, NULL);
#endif
#if !defined(USE_SPAWN) && !defined(DYNIXPTX)
    execvp(rshtype ? a[0] : progname, a);

   /* write something so parent notes exec failure */
    write(pip[1],"",1);
    x__close(pip[1]);
#endif
    /* for what it's worth */
    printf ("Exec failed %s\r\n", a[0]);
    exit(1);
#else /* USE_SPAWN */
    pid = qnx_spawn(0, 0, 0, -1, -1, _SPAWN_SETSID|_SPAWN_TCSETPGRP,
                rshtype ? a[0] : progname, a, environ, iov, 0);
    setgid(0);
    setuid(0);

    if (pid == -1) {	/* for what it's worth */
      DEBUG_FP(stderr,"%s:exec failed %s\r\n", term_server, a[0]);
      x__close(masterfd);
      return -1;
    }
    DEBUG_FP (stderr, "%s:exec succeeded\n", term_server);
#endif /* USE_SPAWN */
  }

	/* Now get the status of the process and get out of here! */

  if (slavefd >= 0) x__close(slavefd);
  set_nonblock(masterfd);
  pty_pid = pid;

#if !defined(__QNX__) && !defined(DYNIXPTX)
  if (pip[1] > 0) x__close(pip[1]);

  if (pip[0] > 0) {	/* Wait for exec or pipe is closed */
    if (read(pip[0], (char *) &i, 1) == 0)
      DEBUG_FP (stderr, "%s:exec apparently succeeded\n", term_server);
    else
      DEBUG_FP (stderr, "%s:exec apparently failed\n", term_server);
    x__close(pip[0]);
  }
#endif

  return masterfd;
}



static char *argv[4] = {"/bin/sh", "-c", "rlogin localhost", NULL};

/* non-pty exec */
/* usage is more like rsh, shell can handle compound command */
int open_socket(char *prog) 
{
  int i=0,j=0;
  int soc[2];
  int pid;
  int pip[2];

  if (S_Pipe(soc) < 0)
	return -4;	/* no stream pipe */

  /* soc[0] is term's end */
  set_nonblock(soc[0]);

  pipe(pip);
  if ((pid = x__fork ()) < 0)
    return -3; /* unable to fork */
  
  if (pid == 0) /* child. */
  { 
    x__close (0);
    x__close (1);
    x__close (2);
    x__close (pip[0]);

    lose_ctty();

    /* soc[1] is child's end */
    x__dup2 (soc[1], 0);
    x__dup2 (soc[1], 1);
    x__dup2 (soc[1], 2);
    for (i = 3; i < 64;++i)
      if (i != pip[1])
	x__close(i);
    
    setbuf(stdout, NULL);
    
    if(prog[0] != '\0'){

      /* unpack args */

      for (j = 0; prog[j] == '-' && prog[j+1] == 'D'; j = i + 1) {
        char e[256];

        for (i=j+2; prog[i] != '\0' && prog[i] != '\377'; ++i);
        strncpy(e,&prog[j+2],256);
        e[i-j-2] = '\0'; 
        if(! strchr(e,'=')) break;
        term_putenv(e);
      }

      for (i=j; prog[i] != '\0'; ++i)
        if (prog[i] == '\377')
  	  prog[i] = ' ';

      argv[2] = &prog[j];
    };

    /* pipe will close upon successful exec */
    if (x__fcntl (pip[1], F_SETFD, 1) == -1)
      x__close (pip[1]);

    setuid(getuid());
    setgid(getgid());

#ifndef DYNIXPTX
    execvp(argv[0], argv);
#else
    execl(argv[0], argv[0], argv[1], argv[2], argv[3]);
#endif

    write(pip[1],"",1);
    x__close(pip[1]);
    
    printf ("Exec failed %s\r\n", argv[0]);
    exit(1);
  }

  /* parent */
  x__close(pip[1]);
  x__close(soc[1]);

  /* Wait for exec, read blocks until pipe is closed */
  if (read(pip[0], (char *) &i, 1) == 0)
    DEBUG_FP (stderr, "%s:exec apparently succeeded\n", term_server);
  else
    DEBUG_FP (stderr, "%s:exec apparently failed\n", term_server);
  x__close(pip[0]);

  pty_pid = pid;
  return soc[0];
}

