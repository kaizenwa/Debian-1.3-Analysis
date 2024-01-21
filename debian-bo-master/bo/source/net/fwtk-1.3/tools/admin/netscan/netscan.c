#include	<ctype.h>
#include	<stdio.h>
#include	<sys/time.h>
#include	<sys/file.h>
#include	<sys/errno.h>
extern	int	errno;
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>
#include	<fcntl.h>

extern	char		*inet_ntoa();
extern	char		*optarg;
extern	int		optind;

int			vflg = 0;
int			delay = 1;
int			port = 23;

main(ac,av)
int	ac;
char	**av;
{
	char			s[512];
	int			x;
	char			*p;
	char			*p2;
	int			i1;
	int			i2;
	int			i3 = -1;

	while((x = getopt(ac,av,"d:vp:")) != -1) {
		switch(x) {
		case 'd':
			delay = atoi(optarg);
			break;
		case 'v':
			vflg++;
			break;
		case 'p':
			port = atoi(optarg);
			break;
		}
	}

	if(optind == ac) {
		fprintf(stderr,"usage: %s [-v] network\n",av[0]);
		exit(1);
	}

	for(x = optind; x < ac; x++) {
		strcpy(s,av[x]);

		for(p = s; isdigit(*p); p++);
		if(*p != '.') {
			fprintf(stderr,"malformed network address: %s\n",av[x]);
			exit(1);
		}
		*p++ = '\0';
		i1 = atoi(s);
		if(i1 < 1 || i1 > 254) {
			fprintf(stderr,"first octet %d out of range in address: %s\n",i1,av[x]);
			exit(1);
		}
		p2 = p;
		for(; isdigit(*p); p++);
		if(*p != '.' && *p != '\0') {
			fprintf(stderr,"network address: %s malformed\n",av[x]);
			exit(1);
		}
		if(*p == '.')
			*p++ = '\0';
		i2 = atoi(p2);
		if(i2 < 1 || i2 > 254) {
			fprintf(stderr,"second octet %d out of range in address: %s\n",i2,av[x]);
			exit(1);
		}

		if(*p != '\0') {
			p2 = p;
			for(; isdigit(*p); p++);
			if(*p != '\0') {
				fprintf(stderr,"network address: %s malformed\n",av[x]);
				exit(1);
			}
			i3 = atoi(p2);
			if(i3 < 1 || i3 > 254) {
				fprintf(stderr,"third octet %d out of range in address: %s\n",i3,av[x]);
				exit(1);
			}
		}
		if(i3 != -1) {
			if(crunchnet(i1,i2,i3))
				exit(1);
		} else {
			for(x = 1; x < 255; x++)
				if(crunchnet(i1,i2,x))
					exit(1);
		}
	}
	exit(0);
}



char	*
addrof(in)
struct	in_addr	in;
{
	struct	hostent	*hp;
	static	char	xuf[512];

	hp = gethostbyaddr((char *)&in,sizeof(in),AF_INET);
	if(hp == (struct hostent *)0)
		return(inet_ntoa(in));
	else {
		sprintf(xuf,"%s (%s)",hp->h_name,inet_ntoa(in));
		return(xuf);
	}
}


	
crunchnet(i1,i2,i3)
int	i1;
int	i2;
int	i3;
{
	struct	sockaddr_in	addr;
	struct	timeval		timo;
	int			fd;
	int			x;
	char			*s;

	if(vflg)
		fprintf(stderr,"trying subnet %d.%d.%d\n",i1,i2,i3);

	s = (char *)&addr.sin_addr;
	s[0] = i1;
	s[1] = i2;
	s[2] = i3;
	timo.tv_usec = 0;
	timo.tv_sec = delay;

	for(x = 1; x < 255; x++) {
		s[3] = x;
		addr.sin_port = htons(port);
		addr.sin_family = AF_INET;

		fd = socket(AF_INET,SOCK_STREAM,0);
		if(fd < 0) {
			perror("socket");
			return(1);
		}
		fcntl(fd,F_SETFL,FNDELAY);

		if(connect(fd,(struct sockaddr *)&addr,sizeof(addr)) < 0) {
			fd_set	msk;

#ifdef	EINPROGRESS
			if(errno != EWOULDBLOCK && errno != EINPROGRESS)
				goto giveup;
#else
			if(errno != EWOULDBLOCK)
				goto giveup;
#endif
			FD_ZERO(&msk);
			FD_SET(fd,&msk);

			if(select(fd + 1,0,&msk,0,&timo) != 1)
				goto	giveup;
			if(vflg)
				fprintf(stderr,"%c%s\n",07,addrof(addr.sin_addr));
			else
				printf("%s\n",addrof(addr.sin_addr));

		} else {
			if(vflg)
				fprintf(stderr,"%c%s\n",07,addrof(addr.sin_addr));
			else
				printf("%s\n",addrof(addr.sin_addr));
		}
		close(fd);
		continue;
giveup:
		if(vflg)
			fprintf(stderr,"\t%s\n",addrof(addr.sin_addr));
		close(fd);
	}
	if(vflg)
		fprintf(stderr,"\n");
	return(0);
}
