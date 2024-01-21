#include	<stdio.h>
#include	<syslog.h>

extern	char	*rindex();

main(ac,av)
int	ac;
char	*av[];
{
	char	buf[BUFSIZ];
	int	xx;
	char	*name;
	char	*p;
	char	*file = (char *)0;
	int	prio = LOG_NOTICE;

	/* pass one. strip out stuff */
	for(xx = 1; xx < ac; xx++) {
		if(av[xx][0] != '-')
			break;
		p = av[xx];
		av[xx] = (char *)0;
		switch(p[1]) {
		case 'n':
			xx++;
			if(av[xx] != (char *)0)
				name = av[xx];
			av[xx] = (char *)0;
			break;
		case 'p':
			xx++;
			if(av[xx] != (char *)0)
				prio = atoi(av[xx]);
			av[xx] = (char *)0;
			break;
		case 'f':
			file = av[++xx];
			av[xx] = (char *)0;
			break;
		default:
			exit(usage());
		}
	}

	/* setup name for logging */
	if(name == (char *)0) {
		name = av[0];
		if((p = rindex(name,'/')) != (char *)0) {
			*p++ = '\0';
			name = p;
		}
	}

	openlog(name,LOG_PID);

	/* take input from a file */
	if(file != (char *)0) {
		FILE	*fin;
		char	*elp;

		if((fin = fopen(file,"r")) == (FILE *)0) {
			syslog(prio,"cannot open logfile input %s: %m",file);
			exit(1);
		}
		while(fgets(fin,buf,sizeof(buf)) != (char *)0) {
			if((elp = rindex(buf,'\n')) != (char *)0)
				*elp = '\0';
			syslog(prio,"%s",buf);
		}
		exit(0);
	}

	/* pass two. log it */
	p = buf;
	for(xx = 1; xx < ac; xx++) {
		char	*op;

		if(av[xx] == (char *)0)
			continue;
		op = av[xx];
		while(*op != '\0') {
			if(p >= buf + sizeof(buf) - 2)
				break;
			*p++ = *op++;
		}
		if(xx != ac - 1)
			*p++ = ' ';
	}
	*p = '\0';
	syslog(prio,"%s",buf);
	exit(0);
}

usage()
{
	fprintf(stderr,"usage: %s [-n logname] [-p priority] [-f inputfile]\n");
	return(1);
}
