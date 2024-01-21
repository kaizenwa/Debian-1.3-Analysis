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
static	char	RcsId[] = "Header: db.c,v 1.3 94/05/27 14:09:11 mjr rel ";
#include	<sys/types.h>
#include	<sys/file.h>
#include	<fcntl.h>
#include	<ndbm.h>
#include	<syslog.h>

#include	"firewall.h"
#include	"auth.h"


/* log reads/writes to syslog to verify store/retrieve */
/* #define DBDEBUG */

static	DBM	*dbfile = (DBM *)0;
static	int	lockfd = -1;
static	int	isopen = 0;
static	char	*dbpath = AUTH_DEFAULTDBFILE;


int
auth_dbconfig(confp)
Cfg	*confp;
{
	Cfg	*cf;

	if(confp != (Cfg *)0 && (cf = cfg_get("database",confp)) != (Cfg *)0) {
		if(cf->argc != 1) {
			syslog(LLEV,"fwtkcfgerr: database missing, line %d",cf->ln);
			return(1);
		}
		dbpath = cf->argv[0];
	} else {
		if(dbpath == (char *)0) {
			syslog(LLEV,"fwtkcfgerr: database path undefined");
			return(1);
		}
	}
	return(0);
}


int
auth_dbopen()
{

	if(isopen)
		return(0);
	if((lockfd = open(dbpath,O_RDWR|O_CREAT,0600)) < 0) {
		syslog(LLEV,"fwtksyserr: cannot lock %s: %m",dbpath);
		return(-1);
	}
	lock_fd(lockfd);
	if((dbfile = dbm_open(dbpath,O_RDWR|O_CREAT,0600)) == (DBM *)0) {
		syslog(LLEV,"fwtksyserr: cannot open %s: %m",dbpath);
		return(-1);
	}
	isopen = 1;
	return(0);
}



int
auth_dbclose()
{
	if(!isopen)
		return(0);
	dbm_close(dbfile);
	lockun_fd(lockfd);
	close(lockfd);
	isopen = 0;
	return(0);
}



#ifdef	DBDEBUG
static	int
dbdump(dr,nam,a)
char	*dr;
char	*nam;
Auth	*a;
{
	syslog(LLEV,"%s: %s",dr,nam);
	syslog(LLEV,"%s: flags %d",nam,a->flgs);
	syslog(LLEV,"%s: type %d",nam,a->atyp);
	syslog(LLEV,"%s: longname %s",nam,a->ln);
	syslog(LLEV,"%s: group %s",nam,a->gp);
	syslog(LLEV,"%s: pass %s",nam,a->pw);
}
#endif


/* get a user name */
int
auth_dbgetu(nam,abuf)
char	*nam;
Auth	*abuf;
{
	datum	dat;
	datum	rdat;

	if(auth_dbopen())
		return(-1);
	
	dat.dptr = nam;
	dat.dsize = strlen(nam) + 1;
	rdat = dbm_fetch(dbfile,dat);
	if(rdat.dptr == (char *)0 || rdat.dsize != sizeof(Auth)) {
		auth_dbclose();
		return(1);
	}
	bcopy(rdat.dptr,(char *)abuf,rdat.dsize);
#ifdef	DBDEBUG
	dbdump("read",nam,abuf);
#endif
	auth_dbclose();
	return(0);
}


auth_dbputu(nam,abuf)
char	*nam;
Auth	*abuf;
{
	datum	dat;
	datum	pdat;

	if(auth_dbopen())
		return(-1);

	dat.dptr = nam;
	dat.dsize = strlen(nam) + 1;
	pdat.dptr = (char *)abuf;
	pdat.dsize = sizeof(Auth);

	if(dbm_store(dbfile,dat,pdat,DBM_REPLACE)) {
		syslog(LLEV,"fwtksyserr: cannot store %s to %s: %m",nam,dbpath);
		auth_dbclose();
		return(-1);
	}
	auth_dbclose();
#ifdef	DBDEBUG
	dbdump("write",nam,abuf);
#endif
	return(0);
}


auth_dbdelu(nam)
char	*nam;
{
	datum	dat;

	if(auth_dbopen())
		return(-1);

	dat.dptr = nam;
	dat.dsize = strlen(nam) + 1;

	if(dbm_delete(dbfile,dat)) {
		syslog(LLEV,"fwtksyserr: cannot delete %s from %s: %m",nam,dbpath);
		auth_dbclose();
		return(-1);
	}
	auth_dbclose();
	return(0);
}



auth_dbtraversestart(u,abuf)
char	*u;
Auth	*abuf;
{
	datum	key;
	datum	dat;

	if(!isopen)
		return(1);
	key = dbm_firstkey(dbfile);
	if(key.dptr == (char *)0)
		return(1);

	dat = dbm_fetch(dbfile,key);
	if(dat.dptr == (char *)0) {
		syslog(LLEV,"fwtksyserr: Key (%s) without record!",key.dptr);
		return(1);
	}
	if(dat.dsize != sizeof(Auth)) {
		syslog(LLEV,"fwtksyserr: Key (%s) record wrong size!",key.dptr);
		return(1);
	}
	bcopy(key.dptr,u,key.dsize);
	bcopy(dat.dptr,abuf,sizeof(Auth));
	return(0);
}



auth_dbtraversenext(u,abuf)
char	*u;
Auth	*abuf;
{
	datum	key;
	datum	dat;

	if(!isopen)
		return(1);
	key = dbm_nextkey(dbfile);
	if(key.dptr == (char *)0)
		return(1);

	dat = dbm_fetch(dbfile,key);
	if(dat.dptr == (char *)0) {
		syslog(LLEV,"fwtksyserr: Key (%s) without record!",key.dptr);
		return(1);
	}
	if(dat.dsize != sizeof(Auth)) {
		syslog(LLEV,"fwtksyserr: Key (%s) record wrong size!",key.dptr);
		return(1);
	}
	bcopy(key.dptr,u,key.dsize);
	bcopy(dat.dptr,abuf,sizeof(Auth));
	return(0);
}
