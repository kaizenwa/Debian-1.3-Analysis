#include	<stdio.h>
#include	<sys/types.h>
#include	<errno.h>
extern	int	errno;
#include	<fcntl.h>
#include	<sys/file.h>
#include	<sys/time.h>
#include	<sys/socket.h>
#include	<netinet/in.h>

#define	DEFAULT_FILE	"/var/log/messages"
/*
#define	DEFAULT_FILE	"/usr/spool/mqueue/syslog"
*/

extern	char	*getenv();

main(ac,av)
int	ac;
char	**av;
{
	char			rbuf[BUFSIZ];
	int			red;
	int			*tb;
	int			max = 0;
	int			xx;

	/* file descriptor table */
	tb = (int *)malloc(sizeof(int) * getdtablesize());
	if(tb == (int *)0) {
		perror("cannot malloc file descriptor table");
		exit(1);
	} else {
		red = getdtablesize();
		for(xx = 0; xx < red; xx++)
			tb[xx] = -1;
	}


	/* open all the files */
	if(ac > 1) {
		for(xx = 1; xx < ac; xx++)
			if(openfile(av[xx],tb,&max))
				exit(1);
	} else {
		char	*envnam;

		if((envnam = getenv("FLOG_FILE")) == (char *)0)
			envnam = DEFAULT_FILE;
		if(openfile(envnam,tb,&max))
			exit(1);
	}

looptop:
	if(getppid() == 1)
		exit(0);

	for(xx = 0; xx < max; xx++) {
		if(tb[xx] != -1) {
		getmore:
			red = read(tb[xx],rbuf,sizeof(rbuf));
			if(red < 0) {
				(void)close(tb[xx]);
				tb[xx] = -1;
				continue;
			}
			if(red > 0) {
				if(write(1,rbuf,red) != red)
					exit(1);
				goto getmore;
			}
		}
	}

	sleep(1);
	goto looptop;
}


openfile(path,tb,max)
char	*path;
int	*tb;
int	*max;
{
	if((tb[*max] = open(path,O_RDONLY,0)) < 0) {
		perror(path);
		return(1);
	}
	(void)lseek(tb[*max],0L,L_XTND);
	*max = *max + 1;
	tb[*max] = -1;
	return(0);
}
