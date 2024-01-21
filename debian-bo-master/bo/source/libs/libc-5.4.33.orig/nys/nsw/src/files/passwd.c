/*
** passwd.c                           /etc/passwd access functions
**
** This file is part of the NYS Library.
**
**      Copyright (c) 1993 Signum Support AB
**
** The NYS Library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** The NYS Library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with the NYS Library; see the file COPYING.LIB.  If
** not, write to the Free Software Foundation, Inc., 675 Mass Ave,
** Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include "misc.h"
#include "xalloc.h"


static FILE *pfp = NULL;
static const char *path_passwd = _PATH_PASSWD;
struct passwd *_sgetpwent(char *);

void _setpwent(void)
{
    if (pfp)
	rewind(pfp);
    else
	pfp = fopen(path_passwd, "r");
}


void _endpwent(void)
{
    if (pfp)
	fclose(pfp);
    pfp = NULL;
}


int setpwfile(const char *file)
{
    if (pfp)
	_endpwent();
    
    if (file)
	path_passwd = file;
    else
	path_passwd = _PATH_PASSWD;

    return 0;
}



struct passwd *_sgetpwent(char *buf)
{
    static struct passwd pwd;
    char *cp, *sp;

    
    /* Get user name */
    sp = buf;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;
    
    *cp++ = '\0';
    pwd.pw_name = sp;


    /* Get password entry */
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;

    *cp++ = '\0';
    pwd.pw_passwd = sp;


    /* Get uid entry */
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;
	    
    *cp++ = '\0';
    if (*sp)
	pwd.pw_uid = atoi(sp);
    else
	pwd.pw_uid = -1;

    
    /* Get gid entry */
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;

    *cp++ = '\0';
    if (*sp)
	pwd.pw_gid = atoi(sp);
    else
	pwd.pw_gid = -1;


    /* Get GECOS entry */
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;
    
    *cp++ = '\0';
    pwd.pw_gecos = sp;


    /* Get home directory */
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;
    
    *cp++ = '\0';
    pwd.pw_dir = sp;

        
    pwd.pw_shell = cp;

    return &pwd;
}



struct passwd *sgetpwent(const char *str)
{
    static char *buf = NULL;
	

    if (Xalloc(&buf, strlen(str)+1) == NULL)
	return NULL;

    strcpy(buf, str);

    return _sgetpwent(buf);
}

struct passwd *_fgetpwent(FILE *fp, char **pw_buf, int *pw_size)
{
    int len;


    len = _nsw_getline(pw_buf, pw_size, fp);
    if (len < 0)
	return NULL;
    
    return _sgetpwent(*pw_buf);
}


struct passwd *fgetpwent(FILE *fp)
{
    static char *pw_buf = NULL;
    static int pw_size = 0;


    return _fgetpwent(fp, &pw_buf, &pw_size);
}


struct passwd *_getpwent(void)
{
    if (pfp == NULL)
	_setpwent();
    
    if (pfp == NULL)
	return NULL;

    return fgetpwent(pfp);
}


struct passwd *_getpwnam(const char *name)
{
    struct passwd *pwp;

    _setpwent();
    
    while ((pwp = _getpwent()) != NULL &&
	   strcmp(pwp->pw_name, name) != 0)
	;

    _endpwent();
	      
    return pwp;
}

struct passwd *_getpwuid(uid_t uid)
{
    struct passwd *pwp;


    _setpwent();
    
    while ((pwp = _getpwent()) != NULL &&
	   pwp->pw_uid != uid)
	;

    _endpwent();
    
    return pwp;
}
