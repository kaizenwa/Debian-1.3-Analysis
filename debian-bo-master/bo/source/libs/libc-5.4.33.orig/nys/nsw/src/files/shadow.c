/*
** shadow.c                           /etc/shadow access functions
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
#include <errno.h>
#include "pwd.h"
#include "shadow.h"
#include "misc.h"
#include "xalloc.h"


static FILE *sfp = NULL;
static FILE *pfp = NULL;


static struct spwd *pw_fgetspent(FILE *fp)
{
    static struct spwd spb;
    static char *pwbuf = NULL;
    static int pwlen = 0;
    struct passwd *pwp;

    
    pwp = _fgetpwent(fp, &pwbuf, &pwlen);
    if (pwp == NULL)
	return NULL;

    spb.sp_namp = pwp->pw_name;
    spb.sp_pwdp = pwp->pw_passwd;
    spb.sp_lstchg = -1;
    spb.sp_min = -1;
    spb.sp_max = -1;
    spb.sp_warn = -1;
    spb.sp_inact = -1;
    spb.sp_expire = -1;
    spb.sp_flag = (unsigned long) -1;

    return &spb;
}


struct spwd *sgetspent(const char *buf)
{
    static struct spwd spb;
    char *cp, *sp;
    static char *sbuf = NULL;


    if (Xalloc(&sbuf, strlen(buf)+1) == NULL)
	return NULL;

    strcpy(sbuf, buf);

    spb.sp_lstchg = spb.sp_min = spb.sp_max = spb.sp_warn = -1;
    spb.sp_inact = spb.sp_expire = -1;
    spb.sp_flag = -1;

    cp = strchr(sp = sbuf, ':');
    if (cp == NULL)
	return NULL;

    if (*cp)
	*cp++ = '\0';
    spb.sp_namp = sp;
    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return NULL;

    if (*cp)
	*cp++ = '\0';
    spb.sp_pwdp = sp;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';
    
    if (*sp)
	spb.sp_lstchg = atoi(sp);
    else
	spb.sp_lstchg = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';
    if (*sp)
	spb.sp_min = atoi(sp);
    else
	spb.sp_min = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';

    if (*sp)
	spb.sp_max = atoi(sp);
    else
	spb.sp_max = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';

    if (*sp)
	spb.sp_warn = atoi(sp);
    else
	spb.sp_warn = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';

    if (*sp)
	spb.sp_inact = atoi(sp);
    else
	spb.sp_inact = -1;

    
    sp = cp;
    cp = strchr(sp, ':');
    if (cp == NULL)
	return &spb;

    if (*cp)
	*cp++ = '\0';

    if (*sp)
	spb.sp_expire = atoi(sp);
    else
	spb.sp_expire = -1;
	
    if (*cp)
	spb.sp_flag = atoi(cp);
    else
	spb.sp_flag = -1;

    return &spb;
}


struct spwd *fgetspent(FILE *fp)
{
    static char *buf = NULL;
    static int size = 0;
    int len;


    len = _nsw_getline(&buf, &size, fp);
    if (len < 0)
	return NULL;
    
    return sgetspent(buf);
}      


void __setspent(void)
{
    if (sfp)
	rewind(sfp);
    else
	sfp = fopen(_PATH_SHADOW, "r");

    if (sfp && pfp)
    {
	fclose(pfp);
	pfp = NULL;
    }
}


void _setspent(void)
{
    if (sfp)
	rewind(sfp);
    else
    {
	sfp = fopen(_PATH_SHADOW, "r");
	
	if (sfp == NULL && errno != EACCES)
	{
#ifdef DEBUG
	    fprintf(stderr, "Getting shadow info from passwd file...\n");
#endif
	    if (pfp)
		rewind(pfp);
	    else
		pfp = fopen(_PATH_PASSWD, "r");
	}
	else
	    if (pfp)
	    {
#ifdef DEBUG
		fprintf(stderr,
			"Closing passwd file since not needed anymore.\n");
#endif
		fclose(pfp);
		pfp = NULL;
	    }
    }
}


void __endspent(void)
{
    if (sfp)
	fclose(sfp);

    sfp = NULL;
}


void _endspent(void)
{
    __endspent();
    
    if (pfp)
	fclose(pfp);
    
    pfp = NULL;
}


struct spwd *__getspent(void)
{
    if (sfp == NULL)
	_setspent();

    if (sfp == NULL)
	return NULL;
    else
	return fgetspent(sfp);
}


struct spwd *_getspent(void)
{
    if (sfp == NULL && pfp == NULL)
	_setspent();

    if (sfp == NULL && pfp == NULL)
	return NULL;
    
    if (sfp == NULL)
    {
#ifdef DEBUG
	fprintf(stderr, "Getting shadow entry from passwd file\n");
#endif
	return pw_fgetspent(pfp);
    }

    return fgetspent(sfp);
}


struct spwd *__getspnam(const char *name)
{
    struct spwd *sp;

    __setspent();
    
    while ((sp = __getspent()) != NULL &&
	   strcmp(sp->sp_namp, name) != 0)
	;

    __endspent();
    
    if (sp == NULL)
	errno = 0;
    
    return sp;
}


struct spwd *_getspnam(const char *name)
{
    struct spwd *sp;

    _setspent();

    if (sfp == NULL && pfp == NULL)
	return NULL;
    
    while ((sp = _getspent()) != NULL &&
	   strcmp(sp->sp_namp, name) != 0)
	;

    _endspent();
    
    if (sp == NULL)
	errno = 0;
    
    return sp;
}


static int a_num(char **cp, long val)
{
    if (sprintf(*cp, ":%ld", val) < 0)
	return -1;
    
    while (**cp)
	++*cp;

    return 0;
}


int sputspent(const struct spwd *sp, char *buf)
{
    char *cp;

    
    if (sp == NULL || buf == NULL)
	return -1;
    
    strcpy(buf, sp->sp_namp);
    strcat(buf, ":");
    strcat(buf, sp->sp_pwdp);
/*  strcat(buf, ":"); */

    for (cp = buf; *cp; cp++)
	;

    if ((a_num(&cp, sp->sp_lstchg) |
	 a_num(&cp, sp->sp_min) |
	 a_num(&cp, sp->sp_max) |
	 a_num(&cp, sp->sp_warn) |
	 a_num(&cp, sp->sp_inact) |
	 a_num(&cp, sp->sp_expire) |
	 a_num(&cp, (long) sp->sp_flag)))
	return -1;

    return 0;
}


int fputspent(const struct spwd *sp, FILE *fp)
{
    char buf[1024];

    if (sputspent(sp, buf) < 0)
	return -1;

    if (fprintf(fp, "%s\n", buf) < 0)
	return -1;

    return 0;
}


/* John F. Haugh II shadow-suite compatibility function */
int putspent(const struct spwd *sp, FILE *fp)
{
    return fputspent(sp, fp);
}
