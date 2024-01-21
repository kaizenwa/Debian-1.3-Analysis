/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
 */
static	char	RcsId[] = "Header: smapd.c,v 1.8 94/11/01 11:57:58 mjr rel ";


#include	<stdio.h>
#include	<syslog.h>
#include	<sysexits.h>
#include	<ctype.h>
#include	<sys/param.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<sys/file.h>
#include	<sys/dir.h>
#include	<sys/signal.h>

#include	"firewall.h"

#define	MAXSM	512

#ifndef	DIRECT_STRUCT
#define	DIRECT_STRUCT	struct direct
#endif

/*
	smapd - sendmail wrapper de-queuer daemon.

	this programs sits in a loop and scans a queue directory.
	if it finds a file it locks it, then forks off a copy of itself
	with the name of the file to process, clearing the lock. the child
	is invoked with a filename, which causes it to process the
	file through sendmail for final delivery. when the delivery
	side exits, it deletes the completed work file.
	the mechanism is somewhat convoluted, but this is done to
	minimize the amount of time spent waiting for processes
	to complete and to eliminate situations where a crash might
	cause a message not to be delivered.

	mjr. 1993
*/


extern	char	*index();


struct	towho	{
	char		*who;
	struct	towho	*nxt;
};

static	char	*baddir = (char *)0;
static	char	*badargcount = "fwtkcfgerr: config line %d - incorrect number of parameters";
static	char	*smailprog = "/usr/lib/sendmail";
int		maxpids = 6;
int		curpids = 0;
int		dbgflg = 0;

static	void
reapchild()
{
	int	s;
	int	c;

	while((c = wait(&s)) != -1) {
		curpids--;
		if(dbgflg)
			fprintf(stderr,"reap %d exit=%d curpid=%d maxpid=%d\n",c,s,curpids,maxpids);
	}
	if(curpids < 0)
		curpids = 0;
	signal(SIGCHLD,reapchild);
}



main(ac,av)
int	ac;
char	*av[];
{
	DIR	*dp;
	Cfg	*cp;
	Cfg	*cfp;
	int	x;
	int	doze = 30;
	char	*rusername = (char *)0;

#ifndef	LOG_DAEMON
	openlog("smapd",LOG_PID);
#else
	openlog("smapd",LOG_PID|LOG_NDELAY,LFAC);
#endif

	if(ac > 1 && !strcmp(av[1],"-d"))
		dbgflg = 1;

	if((cfp = cfg_read("smapd")) == (Cfg *)-1)
		exit(1);


	if((cp = cfg_get("sendmail",cfp)) != (Cfg *)0) {
		if(cp->argc != 1) {
			syslog(LLEV,badargcount,cp->ln);
			exit(1);
		}
		smailprog = cp->argv[0];
	}


	if((cp = cfg_get("baddir",cfp)) != (Cfg *)0) {
		if(cp->argc != 1) {
			syslog(LLEV,badargcount,cp->ln);
			exit(1);
		}
		baddir = cp->argv[0];
	}


	if((cp = cfg_get("groupid",cfp)) != (Cfg *)0) {
		int	ugid;

		if(cp->argc != 1) {
			syslog(LLEV,badargcount,cp->ln);
			exit(1);
		}
		if((ugid = mapgid(cp->argv[0])) == -1) {
			syslog(LLEV,"fwtkcfgerr: cannot decipher group-id %s",cp->argv[0]);
			exit(1);
		}
		if(setgid(ugid)) {
			syslog(LLEV,"fwtksyserr: cannot setgid %s/%d: %m",cp->argv[0],ugid);
			exit(1);
		}
	}

	if((cp = cfg_get("userid",cfp)) != (Cfg *)0) {
		int	uuid;

		if(cp->argc != 1) {
			syslog(LLEV,badargcount,cp->ln);
			exit(1);
		}
		if((uuid = mapuid(cp->argv[0])) == -1) {
			syslog(LLEV,"fwtkcfgerr: cannot decipher user-id %s",cp->argv[0]);
			exit(1);
		}
		if(setuid(uuid)) {
			syslog(LLEV,"fwtksyserr: cannot setuid %s/%d: %m",cp->argv[0],uuid);
			exit(1);
		}
	}


	if((cp = cfg_get("directory",cfp)) != (Cfg *)0) {
		if(cp->argc != 1) {
			syslog(LLEV,badargcount,cp->ln);
			exit(1);
		}
		if(chdir(cp->argv[0])) {
			syslog(LLEV,"fwtksyserr: cannot chroot to spool directory %s: %m",cp->argv[0]);
			exit(1);
		}
	}


	if((cp = cfg_get("wakeup",cfp)) != (Cfg *)0) {
		if(cp->argc != 1) {
			syslog(LLEV,badargcount,cp->ln);
			exit(1);
		}
		if((doze = atoi(cp->argv[0])) <= 0) {
			syslog(LLEV,"fwtkcfgerr: bad delay %s: %m",cp->argv[0]);
			exit(1);
		}
	}


	if((cp = cfg_get("maxchildren",cfp)) != (Cfg *)0) {
		if(cp->argc != 1) {
			syslog(LLEV,badargcount,cp->ln);
			exit(1);
		}
		if((maxpids = atoi(cp->argv[0])) <= 0) {
			syslog(LLEV,"fwtkcfgerr: bad maxchildren %s: %m",cp->argv[0]);
			exit(1);
		}
	}


	if(!dbgflg)
		daemonize();
	signal(SIGCHLD,reapchild);

	if((dp = opendir(".")) == (DIR *)0) {
		syslog(LLEV,"fwtksyserr: cannot scan spool directory: %m");
		exit(1);
	}

	while(1) {
		DIRECT_STRUCT	*fn;

		if(dbgflg)
			fprintf(stderr,"re-scanning directory\n");
		while((fn = readdir(dp)) != (DIRECT_STRUCT *)0) {
			int	sl;

			while(curpids >= maxpids) {
				if(dbgflg)
					fprintf(stderr,"sleep curpid=%d maxpid=%d\n",curpids,maxpids);
				sleep(2);
			}

			if(fn->d_name[0] == '.')
				continue;

			if(strncmp(fn->d_name,"sma",3))
				continue;

			curpids++;
			if((sl = fork()) == 0)
				exit(deliver(fn->d_name));

			if(dbgflg)
				fprintf(stderr,"child %d deliver %s curpid=%d maxpid=%d\n",sl,fn->d_name,curpids,maxpids);
			if(sl < 0) {
				syslog(LLEV,"fwtksyserr: cannot fork: %m");
				curpids--;
				goto zone;
			}
		}
		rewinddir(dp);

#ifdef	CHECK_FD_LEAK
		{
			int	xxx;
			if((xxx = open("/dev/null",O_RDONLY)) > -1) {
				syslog(LLEV,"high fd %d",xxx);
				close(xxx);
			}
		}
#endif
zone:
		if(dbgflg)
			fprintf(stderr,"zoning out for %d\n",doze);
		sleep(doze);
	}
}


deliver(path)
char	*path;
{
	struct	towho	*recips = (struct towho *)0;
	struct	towho	*nrec;
	FILE		*fp;
	off_t		bodyoff;
	char		*fromaddr;
	int		w;
	struct	stat	sbuf;
	char		buf[BUFSIZ];
	char		*x;
	int		pid;
	int		y;
	int		bad = 0;
#ifdef PARSEADDR
	enum	{ H_NONE, H_ANY, H_ADDR, H_BODY } state;
	char		**h;
	static	char	*addr_hdrs[] = {
		"bcc:",
		"cc:",
		"errors-to:",
		"from:",
		"reply-to:",
		"return-path:",
		"return-receipt-to:",
		"to:",
		0,
	};
#endif


	signal(SIGCHLD,SIG_IGN);
	if((fp = fopen(path,"r+")) == (FILE *)0) {
		syslog(LLEV,"fwtksyserr: cannot open %s: %m",path);
		return(1);
	}
	if(locktest_fd(fileno(fp))) {
		fclose(fp);
		return(1);
	}

	/* sanity checks */
	if(fstat(fileno(fp),&sbuf)) {
		syslog(LLEV,"fwtksyserr: fstat %s: %m",path);
		goto bombout;
	}
	if((sbuf.st_mode & S_IFMT) != S_IFREG) {
		syslog(LLEV,"securityalert: %s: not a regular file",path);
		goto leave_gracefully;
	}
	if((sbuf.st_mode & 0755 == 0) && (sbuf.st_mode & 0700 == 0)) {
		syslog(LLEV,"%s: not a complete file - discarded",path);
		goto bombout;
	}




	/* file begins with FROM */
	if(fgets(buf,sizeof(buf),fp) == (char *)0) {
		syslog(LLEV,"%s is empty?! discarded (%m)",path);
		goto bombout;
	}
	if(strncmp(buf,"FROM ",5)) {
		syslog(LLEV,"%s does not begin with 'FROM' - discarded",path);
		goto bombout;
	}
	if((x = index(buf,'\n')) == (char *)0) {
		syslog(LLEV,"%s FROM line too long - discarded",path);
		goto bombout;
	}
	*x = '\0';
	x = &buf[5];
#ifdef PARSEADDR
	if(chkaddr(x) < 0) {
		syslog(LLEV,"%s bad FROM %s - discarded",path,x);
		goto bombout;
	}
#endif
	if((fromaddr = malloc(strlen(x) + 1)) == (char *)0) {
		syslog(LLEV,"fwtksyserr: malloc: %m");
		goto leave_gracefully;
	}
	strcpy(fromaddr,x);

	if(dbgflg)
		fprintf(stderr,"from %s\n",fromaddr);


	/* FROM is followed by a list of: RCPT */
	while(1) {
		if(fgets(buf,sizeof(buf),fp) == (char *)0) {
			syslog(LLEV,"%s is bodiless?! discarded (%m)",path);
			goto bombout;
		}

		if(!strncmp(buf,"RCPT ",5)) {
			if((x = index(buf,'\n')) == (char *)0) {
				syslog(LLEV,"%s RCPT line too long - discarded",path);
				goto bombout;
			}
			*x = '\0';
			x = &buf[5];
			while(isspace(*x))
				x++;

			/* try to catch folks sending params to sendmail */
			if(*x == '-') {
				syslog(LLEV,"securityalert: WARNING '-' param: %s",x);
				continue;
			}
#ifdef PARSEADDR
			if(chkaddr(x) < 0) {
				syslog(LLEV,"%s bad RCPT %s - ignored",path,x);
				continue;
			}
#endif

			/* store the recip */
			nrec = (struct towho *)malloc(sizeof(struct towho));
			if(nrec == (struct towho *)0) {
				syslog(LLEV,"fwtksyserr: malloc: %m");
				goto leave_gracefully;
			}
			nrec->who = malloc(strlen(x) + 1);
			if(nrec->who == (char *)0) {
				syslog(LLEV,"fwtksyserr: malloc: %m");
				goto leave_gracefully;
			}

			strcpy(nrec->who,x);
			nrec->nxt = recips;
			recips = nrec;
			continue;
		}

		if(!strncmp(buf,"BODY",4))
			break;
		syslog(LLEV,"%s unexpected envelope: %s - discarded",buf);
		goto bombout;
	}

	/* remember where the body is! */
	bodyoff = ftell(fp);

#ifdef PARSEADDR
	/* look for dangerous mail header stuff */
	state = H_NONE;
	while(state != H_BODY && fgets(buf,sizeof(buf),fp) != (char *)0) {
		if ((x = index(buf,'\n')) == 0) {
			syslog(LLEV,"%.30s... header too long - discarded",buf);
			goto bombout;
		}
		*x = 0;
		if(isspace(buf[0])) {
			if(state == H_NONE || allblanks(buf))
				state = H_BODY;
			else
				x = buf;
		} else {
			state = H_BODY;
			for(x = buf; *x && isgraph(*x); x++) {
				if(*x == ':') {
					state = H_ANY;
					for(h = addr_hdrs; *h; h++) {
						if(!strncasecmp(buf,*h,x-buf)) {
							state = H_ADDR;
							break;
						}
					}
					break;
				}
			}
		}
		if(state == H_ADDR && chkaddr(x + 1) < 0) {
			syslog(LLEV,"%s bad address in header - discarded",buf);
			goto bombout;
		}
	}
#endif
	nrec = recips;
	while(nrec != (struct towho *)0) {
		char	*faka[MAXSM];
		int	i;
		int	wx;

		i = 0;
		faka[i++] = smailprog;
		faka[i++] = "-f";
		faka[i++] = fromaddr;

		while(nrec != (struct towho *)0) {
			if(i > MAXSM - 2)
				break;
			faka[i++] = nrec->who;
			nrec = nrec->nxt;
		}
		faka[i] = (char *)0;

		if((pid = fork()) == 0) {
			int	fd;

			/*
			opened twice!!!
			kludge to get around busted mach2.0
			*/
			if((fd = open(path,O_RDONLY|O_RDWR)) < 0) {
				syslog(LLEV,"fwtksyserr: cannot open %s: %m",path);
				exit(1);
			}
			if(lseek(fd,bodyoff,0) != bodyoff) {
				syslog(LLEV,"fwtksyserr: cannot seek %s: %m",path);
				exit(1);
			}
			close(0);
			if(dup(fd) < 0) {
				syslog(LLEV,"fwtksyserr: cannot dup: %m",path);
				exit(1);
			}
			execv(faka[0],faka);
			lockun_fd(fileno(fp));
			syslog(LLEV,"fwtksyserr: cannot exec %s: %m",faka[0]);
			exit(1);
		}

		while((wx = wait(&w)) != pid && wx != -1)
			;
#ifdef	WEXITSTATUS
		w = WEXITSTATUS(w);
#endif

		if(w != 0)
			switch(w) {
			/* these indicate we should retry */
			case EX_OSERR:
			case EX_OSFILE:
			case EX_UNAVAILABLE:
			case EX_TEMPFAIL:
			case EX_IOERR:
			case EX_NOPERM:
				syslog(LLEV,"error return=%d (retrying)",w);
				goto leave_gracefully;

			case EX_NOUSER:
				syslog(LLEV,"discarding (no user)");
				break;
			default:
				syslog(LLEV,"error (other error %d)",w);
				goto bombout;
			}
	}

	lockun_fd(fileno(fp));
	close(fileno(fp));
	unlink(path);
	syslog(LLEV,"delivered file=%s",path);
	return(0);

bombout:
	lockun_fd(fileno(fp));
	if(fp != (FILE *)0)
		fclose(fp);
	if(baddir) {
		sprintf(buf,"%s/%s",baddir,path);
		(void)rename(path,buf);
	}
	(void)unlink(path);
	return(1);

leave_gracefully:
	lockun_fd(fileno(fp));
	fclose(fp);
	return(1);
}

#ifdef PARSEADDR

chkaddr(s)
char	*s;
{
	char		*open_parens = "(<";
	char		*close_parens = ")>";
	char		paren_stack[10];
	char		*sp = paren_stack;
	unsigned	ch;
	char		*userpart;
	char		*start;
	char		*cp;

#define separator(c) (isspace(c) || (c) == ',')

	/*
	 * For each address in this list, find the user part that would be
	 * seen on the final destination host. The code below understands
	 * user@host, @host:user, host!user, host::user, host:user, user%host
	 * and combinations thereof (for instance, @host:user%host). After
	 * stripping quotes from the user part, require that it does not
	 * start with |. If the user part begins with /, require that the /
	 * is followed by attribute=value.
	 */

	while((ch = *s) != 0) {
		if(separator(ch)) {
			++s;
			continue;
		}
		start = userpart = s;

		while((ch = *s) != 0 && (sp > paren_stack || !separator(ch))) {
			if(ch == '\\') {
				if(*++s)
					++s;
			} else if((cp = index(open_parens, ch)) != 0) {
				if(sp >= paren_stack + sizeof(paren_stack))
					return(-1);
				*sp++ = close_parens[cp - open_parens];
				++s;
			} else if(sp > paren_stack && ch == sp[-1]) {
				sp--;
				++s;
			} else if(ch == '"') {
				while((ch = *++s) != 0) {
					if(ch == '\\' && s[1]) {
						++s;
					} else if(ch == '"') {
						++s;
						break;
					}
				}
			} else {
				++s;
			}
			if(index("!:<",ch) != 0)
				userpart = s;
		}
		while(*userpart == '"')
			userpart++;
		if(*userpart == '|')
			return(-1);
		if(*userpart == '/') {
			cp = userpart + 1;
			while((ch = *cp++) != '=') {
				if(ch == 0 || index("/%@>",ch) != 0)
					return(-1);
			}
		}
	}
	return(1);
}

allblanks(s)
char	*s;
{
	int	ch;

	while((ch = *s++) != 0)
		if(!isascii(ch) || !isspace(ch))
			return(0);
	return(1);
}
#endif
