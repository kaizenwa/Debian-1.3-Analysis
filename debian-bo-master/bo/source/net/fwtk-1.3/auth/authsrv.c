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
static	char	RcsId[] = "Header: authsrv.c,v 1.7 94/11/01 11:52:12 mjr rel ";
#include	<stdio.h>
#include	<syslog.h>
#include	<time.h>

extern	char	*rindex();
extern	char	*ctime();

#include	"firewall.h"
#include	"auth.h"


static	Cfg		*confp;

static	char		riaddr[256];
static	char		rladdr[256];
static	char		*permdenied = "Permission Denied.";

static	char		authuser[512];
static	char		authcomment[512];
static	Auth		authbuf;
static	AProto		*authproto;
extern	AProto		*codetoproto();
extern	AProto		*nametoproto();
static	Auth		authusrbuf;
static	int		authenticated = 0;
static	int		badsleep = -1;	/* how long to sleep if bad */
static	int		interactive = 0;
static	int		noboguschallenge = 0;

static	int	do_quit();
static	int	do_authorize();
static	int	do_response();
static	int	do_display();
static	int	do_adduser();
static	int	do_deluser();
static	int	do_enauser();
static	int	do_passwd();
static	int	do_proto();
static	int	do_wiz();
static	int	do_group();
static	int	do_help();
static	int	do_login();
static	int	do_superwiz();
static	int	do_rename();
static	int	do_list();
static  int	do_authoper();

typedef	struct {
	char	*cnam;
	char	*help;
	int	(*cfun)();
} Cmd;
static Cmd ctab[] = {
"authorize",	"authorize username [comment]",		do_authorize,
"authenticate",	"authenticate username",		do_authorize,
"response",	"response <text>",			do_response,
"quit",		"quit",					do_quit,
"exit",		"exit",					do_quit,
"display",	"display username",			do_display,
"adduser",	"adduser username [fullname]",		do_adduser,
"deluser",	"deluser username",			do_deluser,
"enable",	"enable username [onetime]",		do_enauser,
"disable",	"disable username",			do_enauser,
"login",	0,					do_login,
"password",	"password [username] passwordtext",	do_passwd,
"passwd",	"passwd [username] passwordtext",	do_passwd,
"proto",	"proto username protoname",		do_proto,
"group",	"group username groupname",		do_group,
"rename",	"rename username newname [fullname]",	do_rename,
"wiz",		"wiz username",				do_wiz,
"unwiz",	"unwiz username",			do_wiz,
"superwiz",	"superwiz username",			do_superwiz,
"operation",	"operation group/user username command dest [tokens]",	do_authoper,
"list",		"list [group]",				do_list,
"ls",		"ls [group]",				do_list,
"?",		"?",					do_help,
"help",		"help",					do_help,
0,		0,					0
};



main(ac,av)
int	ac;
char	*av[];
{
	Cmd	*cp;
	Cfg	*cfp;
	char	buf[BUFSIZ];
	char	*xa[128];
	char	xb[BUFSIZ];
	int	xc;
	int	runuid;
	char	*p;
	int	nl;
	int	promptlen;
	char	*prompt;

#ifndef	LOG_NDELAY
	openlog("authsrv",LOG_PID);
#else
	openlog("authsrv",LOG_PID|LOG_NDELAY,LFAC);
#endif

	authuser[0] = '\0';
	prompt = "authsrv-> ";
	promptlen = strlen(prompt);

	/* if invoked by root from a tty go into admin mode */
	if(isatty(0) || (ac > 1 && !strcmp(av[1],"-s"))) {
		if(getuid() == 0) {
			authenticated = 1;
			strcpy(authuser,"administrator");
			authusrbuf.gp[0] = '\0';
			authusrbuf.bcnt = 0;
			authusrbuf.flgs = 0 | AUTHFLG_WIZ;
			authusrbuf.last = (long)0;
			authusrbuf.atyp = 'p';
			authusrbuf.pw[0] = '*';
			authusrbuf.pw[1] = '\0';
			prompt = "authsrv# ";
			promptlen = strlen(prompt);
		}
		if(isatty(0))
			interactive = 1;
	}


	if((confp = cfg_read("authsrv")) == (Cfg *)-1)
		exit(1);
	if(auth_dbconfig(confp)) {
		if(interactive)
			fprintf(stderr,"Cannot open database\n");
		exit(1);
	}

	/*
	initialize I/O if needed (check caller perms, etc)
	this uses "confp" to check permissions / client keys
	*/
	if(!interactive && ac < 2) {
		if(peername(0,rladdr,riaddr,sizeof(riaddr))) {
			syslog(LLEV,"fwtksyserr: cannot get peer name: %m");
			exit(1);
		}
		if(srvsayinit(confp,rladdr,riaddr)) {
			srvsay("Authentication server does not recognize this host");
			exit(1);
		}
		sprintf(buf,"Authsrv ready. (%s)",FWTK_VERSION_MINOR);
		srvsay(buf);
	}


	if((cfp = cfg_get("groupid",confp)) != (Cfg *)0) {
		if(cfp->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: groupid must have one parameter, line %d",cfp->ln);
			exit(1);
		}
		if((runuid = mapgid(cfp->argv[0])) == -1) {
			syslog(LLEV,"fwtkcfgerr: cannot map %s to gid",cfp->argv[0]);
			exit(1);
		}
		if(setgid(runuid)) {
			syslog(LLEV,"fwtksyserr: setgid %d: %m",runuid);
			exit(1);
		}
	}

	if((cfp = cfg_get("userid",confp)) != (Cfg *)0) {
		if(cfp->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: userid must have one parameter, line %d",cfp->ln);
			exit(1);
		}
		if((runuid = mapuid(cfp->argv[0])) == -1) {
			syslog(LLEV,"fwtkcfgerr: cannot map %s to uid",cfp->argv[0]);
			exit(1);
		}
		if(setuid(runuid)) {
			syslog(LLEV,"fwtksyserr: setuid %d: %m",runuid);
			exit(1);
		}
	}


	if((cfp = cfg_get("badsleep",confp)) != (Cfg *)0) {
		if(cfp->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: badsleep must have one parameter, line %d",cfp->ln);
			exit(1);
		}
		badsleep = atoi(cfp->argv[0]);
	}


	if((cfp = cfg_get("nobogus",confp)) != (Cfg *)0)
		noboguschallenge++;


	while(1) {
		if(interactive)
			write(0,prompt,promptlen);
		if(srvhear(buf,sizeof(buf)) != 0)
			break;
		if((p = rindex(buf,'\n')) != (char *)0)
			*p = '\0';

		xc = enargv(buf,xa,sizeof(xa)/sizeof(char *),xb,sizeof(xb));
		if(xc < 0) {
			srvsay("Too many command parameters.");
			continue;
		}
		if(xc == 0)
			continue;

		nl = strlen(xa[0]);
		for(cp = ctab; cp->cnam != (char *)0; cp++) {
			if(!strncasecmp(cp->cnam,xa[0],nl))
				break;
		}

		if(cp->cnam == (char *)0) {
			char	xuf[512];
			sprintf(xuf,"Command \"%s\" unrecognized.",xa[0]);
			srvsay(xuf);
			continue;
		}
		(*cp->cfun)(xc,xa,buf);
	}
	if(interactive)
		write(0,"\nEOT\n",5);
}



static	int
do_quit()
{
	srvexit(0);
}



static	int
do_authorize(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	char		buf[512];

	authuser[0] = '\0';
	authenticated = 0;

	if(ac < 2) {
		srvsay("Usage: auth username");
		return(1);
	}
	authcomment[0] = '\0';
	if(ac > 2) {
		strncpy(authcomment,av[2],sizeof(authcomment));
		authcomment[sizeof(authcomment) - 1] = '\0';
	}

	strncpy(authuser,av[1],sizeof(authuser));
	authuser[sizeof(authuser) - 1] = '\0';

	/* if there is no record for the guy, send a dummy challenge */
	if(auth_dbgetu(authuser,&authbuf)) {
		if(authcomment[0] != '\0')
			syslog(LLEV,"BADAUTH %s (%s)",authuser,authcomment);
		else
			syslog(LLEV,"BADAUTH %s",authuser);
		authuser[0] = '\0';
		if(noboguschallenge)
			return(srvsay("Userid nonexistent"));
		return(bogusprompt());
	}

	/* if the login is sleeping from bad logins */
	if(authbuf.flgs & AUTHFLG_BSLP) {
		long	now;

		time(&now);
		if(badsleep == -1 || now - authbuf.last < badsleep) {
			if(authcomment[0] != '\0')
				syslog(LLEV,"BADAUTH %s too many tries (%s)",authuser,authcomment);
			else
				syslog(LLEV,"BADAUTH %s too many tries",authuser);
			authuser[0] = '\0';
			if(noboguschallenge && badsleep != -1)
				return(srvsay("Userid temporarily disabled"));
			if(noboguschallenge)
				return(srvsay("Userid disabled"));
			return(bogusprompt());
		}

		/*
		this is weird, but there's a reason for it.
		if we've been sleeping because we're bad, we'll have
		only gotten this far if the case above was not true,
		and the account is ready to give another 5 tries to.
		so, we clear the bad count and let the log entries
		stand
		*/
		if(authbuf.flgs & AUTHFLG_BSLP)
			authbuf.flgs &= ~AUTHFLG_BSLP;
	}

	/* if the login is disabled */
	if(authbuf.flgs & AUTHFLG_DIS) {
		if(authcomment[0] != '\0')
			syslog(LLEV,"BADAUTH %s is disabled (%s)",authuser,authcomment);
		else
			syslog(LLEV,"BADAUTH %s is disabled",authuser);
		authuser[0] = '\0';
		if(noboguschallenge)
			return(srvsay("Userid disabled"));
		return(bogusprompt());
	}

	/* figure out what authentication protocol this user uses */
	if((authproto = codetoproto(authbuf.atyp)) == (AProto *)0) {
		syslog(LLEV,"fwtkcfgerr: %s: bad auth protocol %c",authuser,authbuf.atyp);
		authenticated = 0;
		if(noboguschallenge)
			return(srvsay("Userid authentication protocol invalid"));
		return(bogusprompt());
	}

	/*
	if there is no challenge generator for this protocol
	just ask for a password.
	*/
	if(authproto->challf == 0) {
		srvsay("password");
		return(0);
	}

	/* ok, there IS a challenge generator - call it */
	strcpy(buf,"challenge ");
	if((*authproto->challf)(authuser,&buf[10],sizeof(buf) - 10) != 0) {
		authuser[0] = '\0';
		if(noboguschallenge)
			return(srvsay(&buf[10]));
		return(bogusprompt());
	}
	srvsay(buf);
	return(0);
}



static int
time_ok(upper,lower)
char	*upper;
char	*lower;
{
	int		up;
	int		low;
	int 		present;
	int		p_min;
	int		l_min;
	int		u_min;
	struct tm	tm_present;
	time_t		time_sec;
	char		buf[26];
	char 		temp[3];
	char		ubuf[24];
	char		lbuf[24];

	strncpy(ubuf,upper,sizeof(ubuf) - 1);
	ubuf[sizeof(ubuf) - 1] = '\0';
	upper = ubuf;
	strncpy(lbuf,lower,sizeof(lbuf) - 1);
	lbuf[sizeof(lbuf) - 1] = '\0';
	lower = lbuf;

	time(&tm_present);
	time(&time_sec);
	strcpy(buf,ctime(&time_sec));

	/* Convert the hours */
	temp[0] = buf[11];
	temp[1] = buf[12];
	temp[2] = '\0';
	present = atoi(temp) * 100;
	temp[0] = buf[14];
	temp[1] = buf[15];
	temp[2] = '\0';
	p_min = atoi(temp);
	present = present + p_min;

	/*
	Convert time limits and present time into integer values
	then determine if in the correct range.
	*/
	temp[0] = upper[0];
	temp[1] = upper[1];
	temp[2] = '\0';
	up = atoi(temp) * 100;
	temp[0] = upper[3];
	temp[1] = upper[4];
	temp[2] = '\0';
	u_min = atoi(temp);

	if(p_min > u_min) {
		up = up - 100;
		u_min = u_min + 60;
		up = up + u_min;
	} else
		up = up + u_min;

	temp[0] = lower[0];
	temp[1] = lower[1];
	temp[2] = '\0';
	low = atoi(temp) * 100;
	temp[0] = lower[3];
	temp[1] = lower[4];
	temp[2] = '\0';
	l_min = atoi(temp);
	if(p_min > l_min) {
		low = low - 100;
		l_min = l_min + 60;
		low = low + l_min;
	} else
		low - low + l_min;

	/* Wrap around midnight */
	if(up < low) {
		if((present - low >= 0) || (up - present >= 0))
			return(0);
		else
			return(1);
	}

	if((up - present >= 0) && ((present - low) >= 0))
		return(0);
	if((up - present) < 0) 
		return(1);
	if((present - low) < 0)
		return(1);
}



static int
do_authoper(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	Cmd	*cp;
	Cfg	*cfp;
	char	group[256];
	char	*token;
	int	counter;
	int	top;

	if(ac < 5){
		srvsay ("Usage:operation user/group user command destination");
		return(1);
	}

	/* Next get the record that matches the user or group */
	cfp = cfg_get("operation", confp);
	while((cfp->argc < 3) && (cfp != (Cfg*)0))
		cfp = cfg_get("operation", (Cfg*)0);

	/* Match the record to the group */
	if(auth_dbgetu(av[2],&authbuf)) {
		syslog(LLEV,"fwtkcfgerr: authsrv asked to verify user %s who does not exist?",av[2]);
		return(srvsay("User not found in auth DB"));
	} else {
		strncpy(group,authbuf.gp,sizeof(group));
		group[sizeof(group) - 1] = '\0';
		token = group;
	}

 	/*
	Now try matching the identifier with the record in netperm-table
	The first while loop reads through the linked list, second 
	while loop reads through the records in the list.
	*/
	while((cfp != (Cfg*)0)) {
		/* setup group/username mapping */
		if(!strcmp(cfp->argv[0],"group")) {
			if(!strcmp(av[2],"unauth"))
				token = av[2];
		} else {
			if((strcmp(cfp->argv[0],"user"))) {
				syslog(LLEV,"fwtkcfgerr: operation should specify user or group, line %d",cfp->ln);
				return(srvsay("operation: syntax error in netperm-table"));
			}
			token = av[2];
		}

		if(strcmp(cfp->argv[2],av[3]))
			goto get_next;

		if(!hostmatch(cfp->argv[3], av[4]))
			goto get_next;

		/* match intervening tokens (if any) up to possible "time" field */
		top = 0;
		if(!strcmp(cfp->argv[cfp->argc - 3],"time"))
			top = cfp->argc - 3;
		for(counter = 4; counter < top; counter++) {
			if(av[counter + 1] == (char *)0)
				goto get_next;
			if(!namatch(cfp->argv[counter],av[counter + 1]))
				goto get_next;
		}

		/* we have matched everything */
	        if((ac == cfp->argc + 1) || (top != 0))
			break;

get_next:
		cfp = cfg_get("operation", (Cfg*) 0);
	}

	if(cfp == (Cfg *)0) {
		srvsay("No match in netperm-table");
		return(0);
	}

	/*
	New processing added in to allow time validation 
	more than 5 argments implies time range is specified.
	*/
	if(cfp->argc > 5 && top != 0) {
		if(!time_ok(cfp->argv[cfp->argc - 1], cfp->argv[cfp->argc - 2])) {
			if(cfp->flags & PERM_DENY) 
				return(srvsay(permdenied));
			else
				return(srvsay("ok "));
		} else
			return(srvsay(permdenied));
	} else {
		if(cfp->flags & PERM_DENY)
			goto deny_cmd;
		else
			return(srvsay("ok "));
	}

deny_cmd:
	srvsay(permdenied);
	return(0);
}



static	int
do_help(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	Cmd	*cp;

	srvsay("Command List:");
	srvsay("(Commands may be invoked with unique leading abbreviation)");
	for(cp = ctab; cp->cnam != (char *)0; cp++)

		if(cp->help != (char *)0)
			srvsay(cp->help);
	return(0);
}


static	int
do_login(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	srvsay("Login not supported directly in authsrv.");
	srvsay("Use 'authorize' and 'response'");
	return(0);
}


static	int
do_response(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	char	rbuf[512];

	/* this guy is skanking us */
	if(authenticated || authuser[0] == '\0') {
		authuser[0] = '\0';
		authenticated = 0;
		srvsay(permdenied);
		return(0);
	}
	if(ac != 2)
		av[1] = "";

	if(authproto != (AProto *)0 && authproto->verf == 0) {
		strcpy(rbuf,"ok");
		goto	okay_login;
	}

	if(authproto == (AProto *)0 || (*authproto->verf)(authuser,av[1],&authbuf,rbuf)) {
		if(authcomment[0] != '\0')
			syslog(LLEV,"BADAUTH %s (%s)",authuser,authcomment);
		else
			syslog(LLEV,"BADAUTH %s",authuser);
		/* the guy bombed out. :( */
		authbuf.bcnt++;
		time(&(authbuf.last));

		/* set the guy to have a login delay */
		if((authbuf.bcnt >= AUTH_MAXBAD) &&
			((authbuf.flgs & (AUTHFLG_WIZ | AUTHFLG_GWIZ)) == 0)) {
				authbuf.flgs |= AUTHFLG_BSLP;
				authbuf.bcnt = 0;
			if(authcomment[0] != '\0')
				syslog(LLEV,"securityalert: repeated bad auth attempts %s (%s)",authuser,authcomment);
			else
				syslog(LLEV,"securityalert: repeated bad auth attempts %s",authuser);
		}
		(void)auth_dbputu(authuser,&authbuf);
		authuser[0] = '\0';
		authenticated = 0;
		if(authproto == (AProto *)0)
			srvsay(permdenied);
		else
			srvsay(rbuf);
		return(0);
	}

okay_login:
	authenticated = 1;
	bcopy(&authbuf,&authusrbuf,sizeof(Auth));

	/* update last login */
	time(&(authusrbuf.last));
	authusrbuf.bcnt = 0;
	authusrbuf.flgs &= ~AUTHFLG_BSLP;
	if(authusrbuf.flgs & AUTHFLG_ONETIME) {
		syslog(LLEV,"ONE-TIME %s will be disabled",authuser);
		authusrbuf.flgs |= AUTHFLG_DIS;
	}
	(void)auth_dbputu(authuser,&authusrbuf);
	if(authcomment[0] != '\0')
		syslog(LLEV,"AUTHENTICATE %s (%s)",authuser,authcomment);
	else
		syslog(LLEV,"AUTHENTICATE %s",authuser);
	srvsay(rbuf);
	return(0);
}


static	int
do_adduser(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	if(!authenticated) {
		srvsay(permdenied);
		return(0);
	}
	if((authusrbuf.flgs & AUTHFLG_WIZ) == 0 && (authusrbuf.flgs & AUTHFLG_GWIZ) == 0) {
		srvsay(permdenied);
		return(0);
	}
	if(ac < 2) {
		srvsay("Usage: adduser username [longname]");
		return(0);
	}
	if(auth_dbgetu(av[1],&authbuf) == 0) {
		srvsay("User already exists.");
		return(0);
	}

	authbuf.ln[0] = '\0';
	if(ac == 3) {
		strncpy(authbuf.ln,av[2],AUTH_LNSIZ - 1);
		authbuf.ln[AUTH_LNSIZ - 1] = '\0';
	}
	authbuf.pw[0] = '*';
	authbuf.pw[1] = '\0';

	/* inherit authentication method from creator */
	authbuf.atyp = authusrbuf.atyp;

	authbuf.bcnt = 0;
	authbuf.flgs = 0 | AUTHFLG_DIS;
	authbuf.last = (long)0;

	/* if created by a group wizard, become a member of the group */
	if((authusrbuf.flgs & AUTHFLG_GWIZ))
		strcpy(authbuf.gp,authusrbuf.gp);
	else
		authbuf.gp[0] = '\0';

	if(auth_dbputu(av[1],&authbuf)) {
		srvsay("database error");
		return(0);
	}
	syslog(LLEV,"%s ADDED USER %s",authuser,av[1]);
	srvsay("ok - user added initially disabled");
	return(0);
}


static	int
do_rename(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	if(!authenticated) {
		srvsay(permdenied);
		return(0);
	}

	if((authusrbuf.flgs & AUTHFLG_WIZ) == 0 && (authusrbuf.flgs & AUTHFLG_GWIZ) == 0) {
		srvsay(permdenied);
		return(0);
	}


	if(ac < 3) {
		srvsay("Usage: rename oldname newname [longname]");
		return(0);
	}
	if(auth_dbgetu(av[2],&authbuf) == 0) {
		char	xuf[512];

		sprintf(xuf,"User %s already exists.",av[2]);
		srvsay(xuf);
		return(0);
	}
	if(auth_dbgetu(av[1],&authbuf) != 0) {
		char	xuf[512];

		sprintf(xuf,"User %s does not exist.",av[1]);
		return(0);
	}

	/* you can only do it if you're in that group or a wizard */
	if(authusrbuf.flgs & AUTHFLG_GWIZ) {
		if(strcmp(authusrbuf.gp,authbuf.gp)) {
			if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
				srvsay(permdenied);
				return(0);
			}
		}
	} else {
		if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
			srvsay(permdenied);
			return(0);
		}
	}


	if(ac == 4) {
		strncpy(authbuf.ln,av[3],AUTH_LNSIZ - 1);
		authbuf.ln[AUTH_LNSIZ - 1] = '\0';
	}

	if(auth_dbputu(av[2],&authbuf) || auth_dbdelu(av[1])) {
		srvsay("database error");
		return(0);
	}
	syslog(LLEV,"%s RENAMED USER %s to %s",authuser,av[1],av[2]);
	srvsay("ok - user renamed");
	return(0);
}


static	int
do_deluser(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	if(!authenticated) {
		srvsay(permdenied);
		return(0);
	}
	if((authusrbuf.flgs & AUTHFLG_WIZ) == 0 && (authusrbuf.flgs & AUTHFLG_GWIZ) == 0) {
		srvsay(permdenied);
		return(0);
	}
	if(ac < 2) {
		srvsay("Usage: deluser username");
		return(0);
	}
	if(auth_dbgetu(av[1],&authbuf)) {
		srvsay("Unknown user");
		return(0);
	}

	/* if a group wizard, can delete a member of the group */
	if(authusrbuf.flgs & AUTHFLG_GWIZ) {
		if(strcmp(authusrbuf.gp,authbuf.gp)) {
			if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
				srvsay(permdenied);
				return(0);
			}
		}
	} else {
		if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
			srvsay(permdenied);
			return(0);
		}
	}

	if(auth_dbdelu(av[1])) {
		srvsay("database error");
		return(0);
	}
	syslog(LLEV,"%s DELETED USER %s",authuser,av[1]);
	srvsay("deleted");
	return(0);
}


static	int
do_enauser(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	int	onetime = 0;

	if(!authenticated) {
		srvsay(permdenied);
		return(0);
	}
	if((authusrbuf.flgs & AUTHFLG_WIZ) == 0 && (authusrbuf.flgs & AUTHFLG_GWIZ) == 0) {
		srvsay(permdenied);
		return(0);
	}

	if(ac < 2) {
		char	cbuf[512];

		sprintf(cbuf,"Usage: %s username",av[0]);
		srvsay(cbuf);
		return(0);
	}

	if(ac > 2 && !strncmp(av[2],"onetime",strlen(av[2])))
		onetime = 1;

	if(auth_dbgetu(av[1],&authbuf)) {
		srvsay("Unknown user");
		return(0);
	}

	/* if a group wizard, can enable/disable a member of the group */
	if(authusrbuf.flgs & AUTHFLG_GWIZ) {
		if(strcmp(authusrbuf.gp,authbuf.gp)) {
			if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
				srvsay(permdenied);
				return(0);
			}
		}
	} else {
		if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
			srvsay(permdenied);
			return(0);
		}
	}

	if(av[0][0] == 'e' || av[0][0] == 'E') {
		authbuf.flgs &= ~(AUTHFLG_DIS | AUTHFLG_BSLP);
		authbuf.bcnt = 0;
		if(auth_dbputu(av[1],&authbuf)) {
			srvsay("database error");
			return(0);
		}
		syslog(LLEV,"%s ENABLED USER %s",authuser,av[1]);
		srvsay("enabled");
		return(0);
	}
	authbuf.flgs |= AUTHFLG_DIS;
	authbuf.bcnt = 0;
	if(onetime)
		authbuf.flgs |= AUTHFLG_ONETIME;
	if(auth_dbputu(av[1],&authbuf)) {
		srvsay("database error");
		return(0);
	}
	syslog(LLEV,"%s DISABLED USER %s",authuser,av[1]);
	srvsay("disabled");
	return(0);
}


static	int
do_wiz(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	if(!authenticated) {
		srvsay(permdenied);
		return(0);
	}
	if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
		srvsay(permdenied);
		return(0);
	}

	if(ac < 2) {
		char	cbuf[512];

		sprintf(cbuf,"Usage: %s username",av[0]);
		srvsay(cbuf);
		return(0);
	}

	if(auth_dbgetu(av[1],&authbuf)) {
		srvsay("Unknown user");
		return(0);
	}

	if(av[0][0] == 'u' || av[0][0] == 'U') {
		authbuf.flgs &= ~AUTHFLG_GWIZ;
		if(auth_dbputu(av[1],&authbuf)) {
			srvsay("database error");
			return(0);
		}
		syslog(LLEV,"%s UN-GWIZ USER %s",authuser,av[1]);
		srvsay("unset group-wizard");
		return(0);
	}
	authbuf.flgs |= AUTHFLG_GWIZ;
	if(auth_dbputu(av[1],&authbuf)) {
		srvsay("database error");
		return(0);
	}
	syslog(LLEV,"%s GWIZ USER %s",authuser,av[1]);
	srvsay("set group-wizard");
	return(0);
}


static	int
do_superwiz(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	if(!authenticated) {
		srvsay(permdenied);
		return(0);
	}
	if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
		srvsay(permdenied);
		return(0);
	}
	if(!interactive) {
		srvsay("Must be logged in as root on terminal.");
		return(0);
	}
	if(ac != 2) {
		srvsay("Usage: superwiz username");
		return(0);
	}

	if(auth_dbgetu(av[1],&authbuf)) {
		srvsay("Unknown user");
		return(0);
	}

	authbuf.flgs |= AUTHFLG_WIZ;
	if(auth_dbputu(av[1],&authbuf)) {
		srvsay("database error");
		return(0);
	}
	syslog(LLEV,"%s WIZ USER %s",authuser,av[1]);
	srvsay("set wizard");
	return(0);
}


static	int
do_group(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	if(!authenticated) {
		srvsay(permdenied);
		return(0);
	}
	if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
		srvsay(permdenied);
		return(0);
	}

	if(ac != 3) {
		srvsay("Usage: group username groupname");
		return(0);
	}

	if(auth_dbgetu(av[1],&authbuf)) {
		srvsay("Unknown user");
		return(0);
	}
	strcpy(authbuf.gp,av[2]);
	if(auth_dbputu(av[1],&authbuf)) {
		srvsay("database error");
		return(0);
	}
	syslog(LLEV,"%s GROUP USER %s TO %s",authuser,av[1],av[2]);
	srvsay("set group");
	return(0);
}


static	int
do_passwd(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	char	*usernam;
	char	*password;
	AProto	*aprp;
	char	rbuf[512];

	if(!authenticated) {
		srvsay(permdenied);
		return(0);
	}

	if(ac < 2) {
		srvsay("Usage: password [username] passwordtext");
		return(0);
	}

	usernam = authuser;
	if(ac > 2) {
		if((authusrbuf.flgs & AUTHFLG_WIZ) == 0 && (authusrbuf.flgs & AUTHFLG_GWIZ) == 0) {
			srvsay(permdenied);
			return(0);
		}
		usernam = av[1];
		password = av[2];
	} else {
		password = av[1];
	}

	if(auth_dbgetu(usernam,&authbuf)) {
		srvsay("Unknown user");
		return(0);
	}

	if(ac > 2) {
		if(authusrbuf.flgs & AUTHFLG_GWIZ) {
			if(strcmp(authusrbuf.gp,authbuf.gp)) {
				if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
					srvsay(permdenied);
					return(0);
				}
			}
		} else {
			if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
				srvsay(permdenied);
				return(0);
			}
		}
	}

	if((aprp = codetoproto(authbuf.atyp)) == (AProto *)0) {
		syslog(LLEV,"fwtkcfgerr: %s: bad auth protocol %c",usernam,authbuf.atyp);
		srvsay("Bad authentication protocol for user - abort");
		return(0);
	}

	if(aprp->setf == 0) {
		srvsay("Password is not settable for this protocol");
		return(0);
	}

	if(strlen(password) >= AUTH_PWSIZ - 1) {
		srvsay("Password is too long");
		return(0);
	}

	/* ok, there IS a password setter - call it */
	if((*aprp->setf)(usernam,password,&authbuf,rbuf)) {
		srvsay(rbuf);
		return(0);
	}
	syslog(LLEV,"%s CHANGE PASSWORD %s",authuser,usernam);
	srvsay(rbuf);
	return(0);
}




static	int
do_proto(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	AProto	*aprp;

	if(!authenticated) {
		srvsay(permdenied);
		return(0);
	}

	if(ac != 3) {
		srvsay("Usage: proto username protocol");
		return(0);
	}

	if(auth_dbgetu(av[1],&authbuf)) {
		srvsay("Unknown user");
		return(0);
	}

	if(authusrbuf.flgs & AUTHFLG_GWIZ) {
		if(strcmp(authusrbuf.gp,authbuf.gp)) {
			if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
				srvsay(permdenied);
				return(0);
			}
		}
	} else {
		if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
			srvsay(permdenied);
			return(0);
		}
	}

	if((aprp = nametoproto(av[2])) == (AProto *)0) {
		char	puf[1024];
		char	ruf[1024];

		getprotolist(puf,sizeof(puf));
		syslog(LLEV,"fwtkcfgerr: %s: bad auth protocol %s",av[1],av[2]);
		sprintf(ruf,"Unknown protocol \"%s\", use one of: %s",av[2],puf);
		srvsay(ruf);
		return(0);
	}
	authbuf.atyp = aprp->typ;
	if(auth_dbputu(av[1],&authbuf)) {
		srvsay("database error");
		return(0);
	}
	syslog(LLEV,"%s CHANGE PROTOCOL %s",authuser,av[1]);
	srvsay("changed");
	return(0);
}


/* client expects this command to be multiline. kludgy. */
static	int
do_display(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	AProto	*ap;
	char	buf[1024];

	if(!authenticated) {
		srvsay(permdenied);
		if(!interactive)
			srvsay(".");
		return(0);
	}

	if(ac != 2) {
		srvsay("Usage: display username");
		if(!interactive)
			srvsay(".");
		return(0);
	}

	if(auth_dbgetu(av[1],&authbuf)) {
		srvsay("Unknown user");
		if(!interactive)
			srvsay(".");
		return(0);
	}

	if(authusrbuf.flgs & AUTHFLG_GWIZ) {
		if(strcmp(authusrbuf.gp,authbuf.gp)) {
			if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
				srvsay(permdenied);
				if(!interactive)
					srvsay(".");
				return(0);
			}
		}
	} else {
		if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
			srvsay(permdenied);
			if(!interactive)
				srvsay(".");
			return(0);
		}
	}

	/* print user and group */
	if(authbuf.gp[0] != '\0')
		sprintf(buf,"Report for user %s, group %s",av[1],authbuf.gp);
	else
		sprintf(buf,"Report for user %s",av[1]);
	if(authbuf.ln[0] != '\0') {
		strcat(buf," (");
		strcat(buf,authbuf.ln);
		strcat(buf,")");
	}
	srvsay(buf);


	/* print last login */
	if(authbuf.last != 0) {
		char		*ap;
		extern	char	*ctime();

		ap = ctime(&(authbuf.last));
		ap[24] = '\0';
		sprintf(buf,"Last authenticated: %s",ap);
		srvsay(buf);
	}

	/* print bad logins */
	if(authbuf.bcnt != 0) {
		sprintf(buf,"%d bad logins",authbuf.bcnt);
		srvsay(buf);
	}

	/* misc flags */
	if((ap = codetoproto(authbuf.atyp)) == (AProto *)0)
		strcpy(buf,"Unknown authentication protocol");
	else
		sprintf(buf,"Authentication protocol: %s",ap->name);
	srvsay(buf);

	strcpy(buf,"Flags:");
	if(authbuf.flgs & AUTHFLG_WIZ)
		strcat(buf," WIZARD");
	if(authbuf.flgs & AUTHFLG_GWIZ)
		strcat(buf," GROUP-WIZARD");
	if(authbuf.flgs & AUTHFLG_DIS)
		strcat(buf," DISABLED");
	if(buf[6] == '\0')
		strcat(buf,"none");
	srvsay(buf);

	/* end multiline */
	if(!interactive)
		srvsay(".");
	syslog(LLEV,"%s DISPLAY %s",authuser,av[1]);
	return(0);
}




static	void
send_user_listing(user,a)
char	*user;
Auth	*a;
{
	AProto	*ap;
	char	buf[512];
	char	dp;
	char	wp = ' ';
	char	gp = ' ';
	char	*rp;
	char	*an;

	/* setup status */
	if(a->flgs & AUTHFLG_DIS)
		dp = 'n';
	else
		if(a->flgs & AUTHFLG_BSLP)
			dp = 'b';
		else
			dp = 'y';
	if(a->flgs & AUTHFLG_WIZ)
		wp = 'W';
	if(a->flgs & AUTHFLG_GWIZ)
		wp = 'G';

	/* last login */
	if(a->last == 0)
		rp = "never";
	else {
		rp = ctime(&(a->last));
		rp[24] = '\0';
	}


	/* auth protocol */
	if((ap = codetoproto(a->atyp)) == (AProto *)0)
		an = "unknown";
	else
		an = ap->name;

	sprintf(buf,"%-10.10s %-10.10s %-15.15s %c%c%c  %-5.5s      %-24.24s",user,a->gp,a->ln,dp,gp,wp,an,rp);
	srvsay(buf);
}



/* client expects this command to be multiline. kludgy. */
static	int
do_list(ac,av,orig)
int	ac;
char	*av[];
char	*orig;
{
	char	buf[1024];
	Auth	abuf;

	if(!authenticated) {
		srvsay(permdenied);
		if(!interactive)
			srvsay(".");
		return(0);
	}

	if(ac > 2) {
		srvsay("Usage: list [group]");
		if(!interactive)
			srvsay(".");
		return(0);
	}
	if(ac > 1) {
		if(authusrbuf.flgs & AUTHFLG_GWIZ) {
			if(strcmp(authusrbuf.gp,av[1])) {
				if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
					srvsay(permdenied);
					if(!interactive)
						srvsay(".");
					return(0);
				}
			}
		}
	} else {
		if((authusrbuf.flgs & AUTHFLG_WIZ) == 0) {
			srvsay(permdenied);
			if(!interactive)
				srvsay(".");
			return(0);
		}
	}

	if(ac > 1)
		sprintf(buf,"Report for users in group %s",av[1]);
	else
		sprintf(buf,"Report for users in database");
	srvsay(buf);
srvsay("user       group      longname      status proto      last");
srvsay("----       -----      --------      ------ -----      ----");

	if(auth_dbopen()) {
		srvsay("Cannot open database.");
		if(!interactive)
			srvsay(".");
		return(0);
	}
	if(auth_dbtraversestart(buf,&abuf) == 0) {
		if(ac < 2 || ((ac > 1) && !strcmp(abuf.gp,av[1])))
			send_user_listing(buf,&abuf);
		while(auth_dbtraversenext(buf,&abuf) == 0) {
			if(ac < 2 || ((ac > 1) && !strcmp(abuf.gp,av[1])))
				send_user_listing(buf,&abuf);
		}
	}
	auth_dbclose();
	/* end multiline */
	if(!interactive)
		srvsay(".");
	if(ac > 1)
		syslog(LLEV,"%s LIST %s",authuser,av[1]);
	else
		syslog(LLEV,"%s LIST",authuser);
	return(0);
}



srvexit(code)
int	code;
{
	exit(code);
}


/* generate a bogus challenge for users w/o accounts */
static	int
randomchallenge(c,siz)
char	*c;
int	siz;
{
	extern	long	randomnumber();
	int		len;

	for(len = 0; len < siz; len++)
		c[len] = ((int)randomnumber() % 10) + '0';
	c[siz] = '\0';
	return(0);
}



/* if we somehow have a bogus user give them a bogus challenge */
bogusprompt()
{
	char	cbuf[128];

	strcpy(cbuf,"challenge Challenge \"");
	randomchallenge(&cbuf[21],6);
	strcat(cbuf,"\": ");
	srvsay(cbuf);
	return(0);
}
