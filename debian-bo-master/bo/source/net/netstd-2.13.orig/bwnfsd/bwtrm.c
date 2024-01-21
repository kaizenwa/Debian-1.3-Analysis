#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef SYSV32
#include <sys/wait.h>
#endif
#include <sys/file.h>
#include <signal.h>
#ifndef SYSV32
#include <strings.h>
#include <termios.h>
#else /* if SYSV32 */
#include <string.h>
#include <termio.h>
#endif
#ifndef sgi
#include <sys/ttold.h>
#else
#include "my_sgtty.h"
#endif
#ifndef SYSV32
#include <sys/ttycom.h>
#endif

#ifdef SYSV32
#define O_ANYP 0
#define ECHOCTL 0
#define ECHOKE 0
#define IMAXBEL 0
#endif

char 	*line;
int	p,t,realmask,mask,cc,pid;
char	buf[132];
char    **env,**environ;
#ifndef SYSV32
struct  termios tr;
struct winsize ws;
#else /* if SYSV32 */
struct termio tr;
#endif
char	*term="TERM=bwtrm";
#ifndef SYSV32
char    *termcap="TERMCAP=bwtrm:do=^J:co#80:li#24:cl=50\\E[;H\\E[2J:le=^H:bs:am:cm=5\\E[%i%d;%dH:nd=2\\E[C:up=2\\E[A:ce=3\\E[K:cd=50\\E[J:so=2\\E[7m:se=2\\E[m:us=2\\E[4m:ue=2\\E[m:md=2\\E[1m:mr=2\\E[7m:mb=2\\E[5m:me=2\\E[m:rs=\\E[l\\E[?7h:ku=^@H:kd=^@P:kr=^@M:kl=^@K:kb=^H:ho=\\E[H:k1=^@;:k2=^@<:k3=^@=:k4=^@>:pt:sr=5\\E[M:vt#3:xn:sc=\\E[s:rc=\\E[u:";
#else
void
catcher()
{
    exit(0);
}
#endif

main(argc,argv)
	int	argc;
	char	*argv[];
{
	char	*prog,c;
	int	got_term=0,i;


	if (argc < 2)
	{
		(void) fprintf(stderr,"%s program ...\n",argv[0]);
		exit(1);
	}
        for ( i = 0 ; environ[i] != NULL; i++);
	env=(char **) malloc(sizeof(env)*(i+3));
	for ( i = 0 ; environ[i] != NULL; i++)
	{
	   env[i]=environ[i];
	   c=*(env[i]+5);
	   *(env[i]+5)='\00';
           if ( strcmp("TERM=",env[i]) == NULL)
	   {
	      got_term = 1;
	      env[i]=term;
	   }
	   else
	      *(env[i]+5)=c;
	}
	if ( got_term == 0)
	   env[i++]=term;
#ifndef SYSV32
	env[i++]=termcap;
#endif
	env[i]=NULL;
	environ=env;

	prog=argv[1];
	if ( (argv[1] = rindex(prog,'/')) == NULL)
		argv[1] = prog;
	else
		argv[1]++;
	for (c = 'p'; c <= 's'; c++) {
		struct stat stb;
		line = "/dev/ptyXX";
		line[strlen("/dev/pty")] = c;
		line[strlen("/dev/ptyp")] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
			line[strlen("/dev/ptyp")] = "0123456789abcdef"[i];
			   p = open(line, 2);
			   if (p > 0)
				goto gotpty;
		}
	}
	(void) fprintf(stderr,"Out of ptys\n");
	exit(1);
gotpty:
	line[strlen("/dev/")] = 't';
	t = open(line, 2);
	if (t < 0)
	{
		(void) fprintf(stderr,"Error opening pty\n");
		exit(1);
	}
	{ struct sgttyb b;
	  (void) gtty(t, &b); 
          b.sg_flags = O_RAW|O_ANYP; 
          (void) stty(t, &b);
	}
#ifndef SYSV32
 	  (void) ioctl(t,TIOCGWINSZ,&ws);
	  ws.ws_row=25;
	  ws.ws_col=80;
	  (void) ioctl(t,TIOCSWINSZ,&ws);
	  (void) ioctl(t,TCGETS,&tr);
#else
	  (void) ioctl(t,TCGETA,&tr);
#endif
	  tr.c_lflag |= ISIG|ICANON|ECHOCTL|ECHOKE|ECHO|ECHOE;
	  tr.c_iflag |= IXON|IMAXBEL|ISTRIP|ICRNL|BRKINT|IGNPAR;
	  tr.c_iflag &= ~INPCK;
	  tr.c_cflag &= ~CS8;
#ifndef	SYSV32
	  tr.c_cflag |= CS7;
#else /* if SYSV32 */
	  tr.c_cflag |= CS7|B38400|HUPCL;
#endif
	  tr.c_oflag |= OPOST|ONLCR;
#ifdef SYSV32
	  tr.c_cc[VINTR] = 0x03; /* ^c */
	  tr.c_cc[VERASE]= 0x7f; /* RUBOUT/DEL */
	  tr.c_cc[VKILL] = 21;   /* ^u */
	  tr.c_cc[VEOF]  = 0x04; /* ^d */
	  (void) ioctl(t,TCSETA,&tr);
#else
	  (void) ioctl(t,TCSETS,&tr);
#endif
	pid = fork();
	if (pid < 0)
	{
		(void) fprintf(stderr,"Unable to fork\n");
		exit(1);
	}
	if (pid == 0) {   /* Child */
	    (void) close(p);
	    for ( p=1 ; p <= NSIG; p++)
		(void) signal(p,SIG_IGN);
	    p = getpgrp(0);
#ifndef SYSV32
	    (void) ioctl ( t,TIOCSPGRP,&p);
#endif
	    (void) dup2(t, 0), (void) dup2(t, 1), (void) dup2(t, 2);
	    (void) close(t);
	    execvp(prog, &argv[1]);
	    (void) fprintf(stderr,"Error activating image\n");
	    exit(1);
	}
	/* Parent */
	(void) close(t);
#ifndef SYSV32
	(void) signal(SIGTTOU,SIG_IGN);
	(void) signal(SIGTTIN,SIG_IGN); 
#else /* if SYSV32 */
	(void) signal(SIGCLD,catcher);
#endif
	realmask = (1 << p) | 1;
	do
	{
		mask = realmask;
		(void) select( p+1,&mask,NULL,NULL,NULL);
		if ( (mask &  1 ) != 0 )
		{
			cc = read(1,buf,128);
			if ( cc > 0 )
				(void) write(p,buf,cc);
			else
				realmask = 0;
		}
		if ( (mask & ( 1 << p)) != 0 )
		{
			cc = read(p,buf,128);
			if ( cc > 0 )
				(void) write(1,buf,cc);
			else
				realmask = 0;
		}
	} while ( realmask == (( 1 << p) | 1) );
}
