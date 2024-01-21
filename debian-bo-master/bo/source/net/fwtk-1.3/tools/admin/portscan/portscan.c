#include	<ctype.h>
#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>

extern	char		*optarg;
extern	int		optind;
extern	struct servent	*getservbyport();

main(ac,av)
int	ac;
char	**av;
{
	int			loport = 0;
	int			hiport = 30000;
	char			*s;
	struct	sockaddr_in	addr;
	struct	hostent		*hp;
	int			fd;
	int			x;
	int			xit;
	char			*p;
	int			vflg = 0;

	while((x = getopt(ac,av,"l:h:v")) != -1) {
		switch(x) {
		case 'v':
			vflg++;
			break;
		case 'h':
			hiport = atoi(optarg);
			break;
		case 'l':
			loport = atoi(optarg);
			break;
		}
	}

	if(optind == ac) {
		fprintf(stderr,"usage: %s [-l low port] [-h high port] [-v] host\n",av[0]);
		exit(1);
	}

	for(xit = optind; xit < ac; xit++) {
		s = av[xit];

		p = s;
		while(*p != '\0' && (*p == '.' || isdigit(*p)))
			p++;

		/* not all digits or dots */
		if(*p != '\0') {
			if((hp = gethostbyname(s)) == (struct hostent *)0) {
				fprintf(stderr,"%s: address unknown\n",s);
				exit(1);
			}

			(void)bcopy(hp->h_addr,(char *)&addr.sin_addr,hp->h_length);
		} else {
			unsigned long	f;

			if((f = inet_addr(s)) == -1L) {
				fprintf(stderr,"%s: address unparsable\n",s);
				exit(1);
			}
			(void)bcopy((char *)&f,(char *)&addr.sin_addr,sizeof(f));
		}

		if(vflg)
			fprintf(stderr,"%s: trying stream ports between %d and %d\n",s,loport,hiport);
		for(x = loport; x < hiport + 1; x++) {
			if(vflg)
				fprintf(stderr,".");
			addr.sin_port = htons(x);
			addr.sin_family = AF_INET;

			fd = socket(AF_INET,SOCK_STREAM,0);
	
			if(fd < 0) {
				perror("socket");
				exit(1);
			}

			if(connect(fd,(struct sockaddr *)&addr,sizeof(addr)) == 0) {
				struct servent	*sp;

				if((sp = getservbyport(x,"tcp")) != (struct servent *)0) {
					if(vflg)
						fprintf(stderr,"\n%c%s\n",07,sp->s_name);
					else
						printf("%s\n",sp->s_name);
				} else {
					if(vflg)
						fprintf(stderr,"\n%c%d\n",07,x);
					else
						printf("%d\n",x);
				}
			}
			close(fd);
		}
		if(vflg)
			fprintf(stderr,"\n");
	}
	exit(0);
}
