/*
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
 */
static	char	RcsId[] = "Header: smap.c,v 1.6 94/11/01 11:57:52 mjr rel ";


#include	<sys/types.h>
#include	<sys/file.h>
#include	<sys/stat.h>
#include	<stdio.h>
#include	<time.h>
#include	<ctype.h>
#include	<syslog.h>
#include	<netdb.h>
#include	<sys/socket.h>
#include	<sys/signal.h>
#include	<netinet/in.h>

#include	"firewall.h"

/*
	smap - sendmail wrapper.

	the object of this program is to allow us to present an SMTP service
	for people to talk to, which is unprivileged, and runs in a chrooted
	directory. a secondary requirement is that the code be as simple as
	possible, to permit manual review. this code, therefore, contains
	no comments other than this one - comments being an indication that
	code is too complex to be trusted.

	files created in spool directory are locked and given mode 644. when
	smap is done gathering them, they are unlocked and given mode 755.
	smapd checks the mode and locking status to ensure that it's not
	dealing with a partial file. unlocked files with mode 644 are by
	definition partial.

	mjr. 1993
*/

extern	char	*strtok();

extern	char	*optarg;

struct	towho	{
	char		*who;
	struct	towho	*nxt;
};


static	struct	towho	*recip = (struct towho *)0;
static	char		*ruser = (char *)0;
static	char		*rhost = (char *)0;
static	char		rladdr[512];
static	char		riaddr[512];
static	char		*rootdir = (char *)0;
static	int		runuid = -1;
static	int		rungid = -1;
static	char		*tempfile = (char *)0;
static	int		maxbytes = -1;
static	int		maxrecip = -1;
static	int		curbytes = 0;
static	int		currecip = 0;
static	int		timeout = 0;
static	int		livetim = 0;

static	FILE	*smapopen();
static	char	*crlfgets();


static	void
ringring()
{
	if(livetim == 0) {
		syslog(LLEV,"timeout - exiting");
		if(tempfile != (char *)0)
			unlink(tempfile);
		exit(1);
	}
	livetim = 0;
	alarm(timeout);
}



main(ac,av)
int	ac;
char	*av[];
{
	Cfg		*cf;
	Cfg		*cfp;
	char		buf[BUFSIZ * 4];
	char		myhostname[512];
	char		*p;
	int		x;
	FILE		*vout = (FILE *)0;
	int		gotdata = 0;
	struct hostent	*hp;

#ifndef	LOG_DAEMON
	openlog("smap",LOG_PID);
#else
	openlog("smap",LOG_PID|LOG_NDELAY,LFAC);
#endif

	if(peername(0,rladdr,riaddr,sizeof(riaddr))) {
		syslog(LLEV,"cannot get remote host");
		exit(1);
	} else
		syslog(LLEV,"connect host=%s/%s",rladdr,riaddr);


	if((cfp = cfg_read("smap")) == (Cfg *)-1)
		exit(1);

	if((cf = cfg_get("groupid",cfp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: groupid must have one parameter, line %d",cf->ln);
			exit(1);
		}
		if((rungid = mapgid(cf->argv[0])) == -1) {
			syslog(LLEV,"fwtkcfgerr: cannot map %s to gid",cf->argv[0]);
			exit(1);
		}
	}

	if((cf = cfg_get("userid",cfp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: userid must have one parameter, line %d",cf->ln);
			exit(1);
		}
		if((runuid = mapuid(cf->argv[0])) == -1) {
			syslog(LLEV,"fwtkcfgerr: cannot map %s to uid",cf->argv[0]);
			exit(1);
		}
	}


	if((cf = cfg_get("directory",cfp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: chroot must have one parameter, line %d",cf->ln);
			exit(1);
		}
		rootdir = cf->argv[0];
	}


	if((cf = cfg_get("maxbytes",cfp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: maxbytes must have one parameter, line %d",cf->ln);
			exit(1);
		}
		maxbytes = atoi(cf->argv[0]);
	}


	if((cf = cfg_get("maxrecip",cfp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: maxrecip must have one parameter, line %d",cf->ln);
			exit(1);
		}
		maxrecip = atoi(cf->argv[0]);
	}


	if((cf = cfg_get("timeout",cfp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: timeout must have one parameter, line %d",cf->ln);
			exit(1);
		}
		timeout = atoi(cf->argv[0]);
		signal(SIGALRM,ringring);
		alarm(timeout);
	}

	if(rootdir != (char *)0) {
		chdir("/");
		if((chdir(rootdir) || chroot(rootdir))) {
			syslog(LLEV,"fwtksyserr: cannot chroot to %s: %m",rootdir);
			exit(1);
		}
		chdir("/");
	}


	if(rungid != -1 && setgid(rungid)) {
		syslog(LLEV,"fwtksyserr: cannot set gid to %d: %m",rungid);
		exit(1);
	}

	if(runuid != -1 && setuid(runuid)) {
		syslog(LLEV,"fwtksyserr: cannot set uid to %d: %m",runuid);
		exit(1);
	}

	if(gethostname(myhostname,sizeof(myhostname)))
		strcpy(myhostname,"amnesiac");
	else if ((hp = gethostbyname(myhostname)) != 0)
		strcpy(myhostname,hp->h_name);
	printf("220 %s SMTP/smap Ready.\r\n",myhostname);
	fflush(stdout);


#ifdef	SO_KEEPALIVE
	x = 1;
	(void)setsockopt(fileno(stdin),SOL_SOCKET,SO_KEEPALIVE,&x,sizeof(x));
#endif

	while(1) {
		if(crlfgets(buf,sizeof(buf),stdin) == NULL) {
			syslog(LLEV,"peer dropped connection: %m");
			break;
		}

		if((p = strtok(buf," \r\t\n")) == (char *)0) {
			printf("500 Command unrecognized\r\n");
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"MAIL")) {
			char	*q;


			if((q = strtok((char *)0,"\r\n")) == (char *)0) {
				printf("501 Syntax error\r\n");
				fflush(stdout);
				continue;
			}

			if(strncasecmp(q,"From:",5)) {
				printf("501 Syntax error\r\n");
				fflush(stdout);
				continue;
			}
			q += 5;
			while(isspace(*q))
				q++;
			if(ruser != (char *)0)
				free(ruser);
			ruser = malloc(strlen(q) + 1);
			if(ruser == (char *)0) {
				printf("410 out of memory\r\n");
				fflush(stdout);
				syslog(LLEV,"fwtksyserr: out of memory: %m");
				unlink(tempfile);
				exit(1);
			}
			strcpy(ruser,q);
			printf("250 %s... Sender Ok\r\n",ruser);
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"RCPT")) {
			struct	towho	*nr;
			char	*q;


			if(ruser == (char *)0) {
				printf("503 need MAIL From: first.\r\n");
				fflush(stdout);
				continue;
			}

			if((q = strtok((char *)0,"\r\n")) == (char *)0) {
				printf("501 Syntax error\r\n");
				fflush(stdout);
				continue;
			}

			if(strncasecmp(q,"To:",3)) {
				printf("501 Syntax error\r\n");
				fflush(stdout);
				continue;
			}

			q += 3;
			while(isspace(*q))
				q++;
#ifdef	SPECIALDOMAIN
			if(!checkvalid(q)) {
				syslog(LLEV,"securityalert: rejecting recip %s",q);
				fflush(stdout);
				continue;
			}
#endif

			if(maxrecip > 0 && currecip++ > maxrecip) {
				printf("550 too many recipients\r\n");
				fflush(stdout);
				syslog(LLEV,"exiting - too many recipients");
				unlink(tempfile);
				exit(1);
			}

			nr = (struct towho *)malloc(sizeof(struct towho));
			if(nr == (struct towho *)0) {
				printf("410 out of memory\r\n");
				fflush(stdout);
				syslog(LLEV,"fwtksyserr: out of memory: %m");
				unlink(tempfile);
				exit(1);
			}
			nr->who = malloc(strlen(q) + 1);
			if(nr->who == (char *)0) {
				printf("410 out of memory\r\n");
				fflush(stdout);
				syslog(LLEV,"fwtksyserr: out of memory: %m");
				unlink(tempfile);
				exit(1);
			}
			strcpy(nr->who,q);
			nr->nxt = recip;
			recip = nr;
			printf("250 %s OK\r\n",q);
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"DATA")) {
			struct	towho	*tp;
			long		now;
			int		msgsize;

			curbytes = 0;
			currecip = 0;
			if(recip == (struct towho *)0) {
				printf("503 need RCPT first.\r\n");
				fflush(stdout);
				continue;
			}
			if((vout = smapopen()) == (FILE *)0) {
				syslog(LLEV,"fwtksyserr: cannot open temp file %m");
				exit(1);
			}

			printf("354 Enter mail, end with \".\" on a line by itself\r\n");
			fflush(stdout);

			fprintf(vout,"FROM %s\n",ruser);
			for(tp = recip; tp != (struct towho *)0; tp = tp->nxt)
				fprintf(vout,"RCPT %s\n",tp->who);
			fprintf(vout,"BODY\n");
			time(&now);
			fprintf(vout,"Received: from %s(%s) by %s via smap (%s)\n\tid %s; %s",rladdr,riaddr,myhostname,FWTK_VERSION_MINOR,tempfile,ctime(&now));
			switch(crunchbody(stdin,vout))  {
				case 1:
					printf("550 you could say goodbye\r\n");
					fflush(stdout);
					break;
				case 2:
					printf("550 Too much data\r\n");
					fflush(stdout);
					syslog(LLEV,"exiting too much data");
					unlink(tempfile);
					exit(1);
				case 3:
					printf("451 System problem\r\n");
					fflush(stdout);
					continue;
				default:
					printf("250 Mail accepted\r\n");
					fflush(stdout);
					gotdata++;
					/* .... fall through... */
			}

			fflush(vout);
			msgsize = filesize(fileno(vout));
			for(tp = recip; tp != (struct towho *)0; tp = tp->nxt)
				syslog(LLEV,"host=%s/%s bytes=%d from=%s to=%s",rladdr,riaddr,msgsize,ruser,tp->who);
			if(recip != (struct towho *)0) {
				currecip = 0;
				/* should really free them */
				recip = (struct towho *)0;
			}
			if(!gotdata) {
				unlink(tempfile);
				syslog(LLEV,"SMTP QUIT with no message %s/%s",rladdr,riaddr);
			}
			chmod(tempfile,0700);
			lockun_fd(fileno(vout));
			syslog(LLEV,"exiting host=%s/%s bytes=%d",rladdr,riaddr,msgsize);
			fclose(vout);
			vout = (FILE *)0;
			continue;
		}


		if(!strcasecmp(p,"VRFY") || !strcasecmp(p,"EXPN")) {
			char	*q;

			if((q = strtok((char *)0," \t\r\n")) == (char *)0)
				printf("550 User Unknown\r\n");
			else
				printf("250 <%s>\r\n",q);
			syslog(LLEV,"%s %s (%s/%s)",p,q,rladdr,riaddr);
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"HELP")) {
			printf("214-Commands\r\n");
			printf("214-HELO    MAIL    RCPT    DATA    RSET\r\n");
			printf("214 NOOP    QUIT    HELP    VRFY    EXPN\r\n");
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"NOOP")) {
			printf("220 OK\r\n");
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"QUIT")) {
			printf("221 Closing connection\r\n");
			fflush(stdout);
			break;
		}


		if(!strcasecmp(p,"RSET")) {
			recip = (struct towho *)0;
			ruser = (char *)0;
			if(vout != (FILE *)0) {
				unlink(tempfile);
				fclose(vout);
				if((vout = smapopen()) == (FILE *)0) {
					syslog(LLEV,"fwtksyserr: cannot open temp file %m");
					exit(1);
				}
			}
			printf("250 Reset state\r\n");
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"HELO")) {
			char	*q;

			if((q = strtok((char *)0,"\r\n")) == (char *)0) {
				printf("250 Charmed, Im sure.\r\n");
				rhost = "??";
			} else {
				printf("250 (%s) pleased to meet you.\r\n",q);
				rhost = malloc(strlen(q) + 1);
				strcpy(rhost,q);
			}
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"VERB")) {
			printf("200 Verbose mode\r\n");
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"ONEX")) {
			printf("200 Only one transaction\r\n");
			fflush(stdout);
			continue;
		}


		if(!strcasecmp(p,"DEBUG")) {
			printf("200 Debug set -NOT!\r\n");
			fflush(stdout);
			continue;
		}

		printf("500 Command unrecognized\r\n");
		fflush(stdout);
	}
	exit(0);
}



static	FILE	*
smapopen()
{
	FILE	*ret;
	int	fd;
	char	nuf[512];

	strcpy(nuf,"smaXXXXXX");
	/* system V locking will have problems if mkstemp does
	not open for random access as well as write */
	if((fd = mkstemp(nuf)) < 0)
		return((FILE *)0);
	chmod(nuf,0600);
	if((ret = fdopen(fd,"w")) == (FILE *)0)
		return((FILE *)0);
	lock_fd(fd);
	if((tempfile = malloc(strlen(nuf) + 1)) == (char *)0) {
		syslog(LLEV,"fwtksyserr: out of memory: %m");
		unlink(nuf);
		exit(1);
	}
	strcpy(tempfile,nuf);
	return(ret);
}



crunchbody(in,out)
FILE	*in;
FILE	*out;
{
	char	buf[BUFSIZ];
	int	x;

	while(1) {
		if(crlfgets(buf,sizeof(buf),in) == NULL)
			return(1);
		if(buf[0] == '.' && buf[1] == '\0')
			break;
		if(buf[0] == '.' && buf[1] == '.')
			x = fprintf(out,"%s\n",&buf[1]);
		else
			x = fprintf(out,"%s\n",buf);
		if(x == -1 || ferror(out)) {
			syslog(LLEV,"fwtksyserr: cannot fprintf: %m");
			return(3);
		}
		if(maxbytes > 0) {
			if((curbytes += strlen(buf)) > maxbytes)
				return(2);
		}

	}
	if(fflush(out) || ferror(out)) {
		syslog(LLEV,"fwtksyserr: cannot fprintf: %m");
		return(3);
	}
	return(0);
}



static	char	*
crlfgets(buf,siz,fp)
char	*buf;
int	siz;
FILE	*fp;
{
	int	x;
	char	*s = buf;

	if(ferror(fp) || feof(fp))
		return((char *)0);

	while(--siz) {
		if((x = fgetc(fp)) == EOF) {
			if(s == buf)
				return((char *)0);
			break;
		}
		if(x == '\n') {
			/* back up over carriage returns */
			if(s > buf && *(s - 1) == '\r')
				s--;
			break;
		}
		*s++ = x;
	}
	*s = '\0';
	livetim = 1;
	return(buf);
}


filesize(fd)
int	fd;
{
	struct	stat	sbuf;

	if(fstat(fd,&sbuf))
		return(-1);
	return(sbuf.st_size);
}


usage()
{
	printf("usage:\n");
	return(1);
}


/* kludge for whitehouse.gov anti-spoofing bogosity */
#ifdef	SPECIALDOMAIN
extern	char	*index();
extern	char	*rindex();
extern	char	*strpbrk();

char	*bad = "550 Recipient must be in form of: user, user@eop.gov, or user@whitehouse.gov\r\n";

checkvalid(r)
char	*r;
{
	char	*atp;
	char	*jxp;
	char	*chop;
	char	*chsavp;
	int	x;

	if((chop = malloc((x = strlen(r)) + 1)) == (char *)0) {
		unlink(tempfile);
		syslog(LLEV,"fwtksyserr: of memory: %m");
		exit(1);
	}
	chsavp = chop;
	strcpy(chop,r);

	if(r[0] == '<') {
		if(chop[x - 1] == '>')
			chop[x - 1] = '\0';
		chop++;
	}

	if((atp = rindex(chop,'@')) != (char *)0) {
		atp++;

		/* check if it ends in @whitehouse.gov || @eop.gov */
		if(strcasecmp(atp,"whitehouse.gov") &&
			strcasecmp(atp,"eop.gov"))
			goto bomb;

		/* now make sure there are no other routing chars */
		atp--;
		*atp = '\0';
		if((jxp = strpbrk(chop,"%@:[]!")) != (char *)0) {
			goto bomb;
		}
	}
	if((jxp = strpbrk(chop,"%@:[]!")) != (char *)0)
		goto bomb;

	free(chsavp);
	return(1);
bomb:
	printf(bad);
	free(chsavp);
	return(0);
}
#endif
