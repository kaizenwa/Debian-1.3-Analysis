/**
 *
 * $Id: Xmos.c,v 1.6 1996/11/28 09:22:31 u27113 Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static char rcsid[] = "$Id: Xmos.c,v 1.6 1996/11/28 09:22:31 u27113 Exp $";

#include <LTconfig.h>
#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/XmosP.h>
#include <X11/Xfuncs.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <LTglob.h>

#include <XmI/DebugUtil.h>

/*
 * this is OS dependent, but this should catch most OS's.
 */
#define MAX_PATH_LEN   2048

/*
 * Pauses program execution for a given number of seconds. Although
 * the sillable `Micro' suggest otherwise, the granularity is really
 * only seconds.
 *
 * BTW - Does anyone knows whether we continue sleeping after a
 * signal was raised? I think so, but I'm not sure... --aldi
 */
int 
_XmMicroSleep(long secs)
{
    unsigned int zzz;

    if ( secs > 0 ) {
	zzz = (unsigned int) secs;
	while ((zzz = sleep(zzz)) > 0)
	   ; /* continue sleeping */
    }
    return 0;
}

/* Motif 2.* version of the above */
int
XmeMicroSleep(long secs)
{
	return _XmMicroSleep(secs);
}

XmString 
_XmOSGetLocalizedString(char *reserved,
			Widget w,
			String resourceName,
			String value)
{
    /* THIS IS A BIG HACK --- FIX ME  --- DON'T KNOW HOW... SORRY */
    return XmStringCreateSimple(value);
}

/*
 * find the pattern part in a fileSpec
 * Rules I've been able to get out of M*tif
 * 1) If a spec ends in '/', return the remaining null string.
 * 2) Ordinarily, return either a) the first path component without
 * an escaped wildcard, or b) the last component that doesn't end in '/'.
 * 3) Escaped wildcards ('\') will be ignored.
 */
String
_XmOSFindPatternPart(String fileSpec) 
{
    static char *wildcards = "*?[]+/";
    String ret, tmp;
    
    for (;;) {
	tmp = fileSpec;
retry:
	if (strlen(tmp) == 0)
	    return fileSpec;
	if ((ret = strpbrk(tmp, wildcards)) == NULL)
	    return fileSpec;
	else {
	    if (ret > tmp && ret[-1] == '\\') {
		tmp = ret + 1;
		goto retry;
	    }
	    else if (*ret == '/') {
		fileSpec = ret + 1; 
		continue;
	    }
	    while ((ret > fileSpec) && (*ret != '/'))
		ret--;
	    if (*ret == '/')
		ret++;
	    return ret;
       }
    }
    
}

 /*
  * this is used if the cwd is unreadable
  */
String
_XmOSGetHomeDirName(void) 
{
     char *ret;
     
     if ((ret = getenv("HOME")) != NULL)
	 return ret;
     /* FIXME -- check password file */
     return NULL;
}

/* Motif 2.* version of the above */
String
XmeGetHomeDirName(void) 
{
	return _XmOSGetHomeDirName();
}

/*
 * qualify a file/dir spec
 * Rules I've learned from Motif:
 * 1) If dirSpec has wildcards, this routine won't try to remove them.  So
 * wildcards had better not get here.
 * 2) If the dirSpec is all dots, or combinations of dots, the routine will
 * resolve the dots relative to the current directory and return that.
 */
void
_XmOSQualifyFileSpec(String dirSpec,
		     String filterSpec,
		     String *pQualifiedDir,
		     String *pQualifiedPattern) 
{
    String dir, filt;
    char *tmp, *tmp2;
    
    /* no dir string? get cwd */
    if (strlen(dirSpec) == 0 ||
	strncmp(dirSpec, ".", 1) == 0 || strncmp(dirSpec, "..", 2) == 0) {
	if ((tmp = (char *)getcwd(NULL, MAX_PATH_LEN)) == NULL) {
	    if ((tmp = _XmOSGetHomeDirName()) == NULL)
		abort();  /* we've gotta give up sometime */
	    if ((tmp = XtNewString(tmp)) == NULL)
		abort();  /* we've gotta give up sometime */
	}
	dir = (String)XtMalloc(strlen(tmp) + 1);
	strcpy(dir, tmp);
	XtFree(tmp);
	while (strncmp(dirSpec, "..", 2) == 0) {
	    tmp = dir + strlen(dir);
	    while (tmp > dir && *tmp != '/')
		tmp--;
	    if (*tmp == '/')
		*tmp = 0;
	    dirSpec += 2;
	    if (*dirSpec == '/')
		dirSpec++;
	}
	while (strncmp(dirSpec, ".", 2) == 0) {
	    dirSpec++;
	    if (*dirSpec == '/')
		dirSpec++;
	}
    }
    /* otherwise, we have a dir */
    else {
	dir = (String)XtMalloc(strlen(dirSpec) + 1);
	strcpy(dir, dirSpec);
    }

    /* if the dir doesn't terminate in a '/', add one */
    if (dir[strlen(dir)-1] != '/') {
	dir = (String)XtRealloc(dir, strlen(dir) + 2);
	strcat(dir, "/");
    }

    /* if the filter starts with an absolute pathname, use it */
    if (*filterSpec == '/') {
	XtFree(dir);
	if ((dir = XtNewString(filterSpec)) == NULL)
	    abort(); /* oh, well */
    }
    /* if the filter is empty, make it all files */
    else if (strlen(filterSpec) == 0) {
	dir = (String)XtRealloc(dir, strlen(dir) + 2);
	strcat(dir, "*");
    }
    else {
	/* now add the filter spec */
	dir = XtRealloc(dir, strlen(dir) + strlen(filterSpec) + 1);
	strcat(dir, filterSpec);
    }

    /* eat any "..", ".", or "//" left in the path */
    for (;;) {
	if ((tmp = strstr(dir, "/./")) != NULL) {
	    if (tmp > dir && tmp[-1] != '\\') {
		tmp2 = XtNewString(tmp+2);
		XtFree(tmp);
		tmp = tmp2;
		continue;
	    }
	}
	else if ((tmp = strstr(dir, "/../")) != NULL) {
	    if (tmp > dir && tmp[-1] != '\\') {
		*tmp = 0;
		if ((tmp2 = strrchr(dir, '/')) != NULL) {
		    tmp2 = XtNewString(tmp+3);
		    XtFree(tmp);
		    tmp = tmp2;
		    continue;
		}
	    }
	}
	else
	    break;
    }

    /* extract the last component for the filter spec */
    if ((tmp = strrchr(dir, '/')) != NULL) {
	if (strlen(tmp) != 0) {
	    tmp++;
	    if ((filt = XtNewString(tmp)) == NULL)
		abort();
	    *tmp = 0;
	}
	else
	    if ((filt = XtNewString("*")) == NULL)
		abort();
    }
    else {
	filt = dir;
	if ((dir = XtNewString("/")) == NULL)
	    abort();
    }
    
    *pQualifiedDir = dir;
    *pQualifiedPattern = filt;
}

/*
 * Takes a string and converts /this/dir/. into /this/dir/
 * also converts /this/dir/.. into /this/
 */
void
_XmOSGetDotDot(String s)
{
	int	i, j;

	if (s == 0)
		return;

	for (i=0; s[i]; i++);

	if (s[i-1] == '.' && s[i-2] == '.' && s[i-3] == '/') {	/* parent dir */
		for (j=i-3; j>0 && s[j] != '/'; j--) ;
		for (j--; j>0 && s[j] != '/'; j--) ;
		if (j >= 0 && s[j] == '/') {
			s[j+1] = '\0';
		}
	} else if (s[i-1] == '.' && s[i-2] == '/') {	/* this dir */
		for (j=i-2; j>0 && s[j] != '/'; j--) ;
		if (j >= 0 && s[j] == '/') {
			s[j+1] = '\0';
		}
	}
	/* else no action */
}

/*
 * does this operate on one dir, or many ? (later) One.
 * Motif apparently does it this way.  If someone can prove otherwise,
 * PLEASE send the offending output to me (miers@packet.net), and a
 * description of the problem.
 * FIXME -- This is a hack.  It should understand locale (for * mapping).
 */
void
_XmOSGetDirEntries(String qualifiedDir,
		   String matchPattern,
		   unsigned char fileType,
		   Boolean matchDotsLiterally,
		   Boolean listWithFullPath,
		   String **pEntries,
		   unsigned int *pNumEntries,
		   unsigned int *pNumAlloc) 
{
    glob_t result;
    char buf[2048];
    int i, cnt, max;
    String slash, tmp, *ret = NULL;
    int flags = GLOB_MARK|(matchDotsLiterally ? 0 : GLOB_PERIOD);

    XdbDebug(__FILE__, NULL, "_XmOSGetDirEntries(%s,%s)\n", qualifiedDir, matchPattern);

    _XmOSGetDotDot(qualifiedDir);
    _XmOSGetDotDot(matchPattern);

    if (strlen(matchPattern) == 0)
	qualifiedDir = "*";
    else if ((slash = strstr(matchPattern, "/")) != NULL) {
	if (slash > matchPattern && slash[-1] != '\\') {
	    tmp = XtMalloc(slash - qualifiedDir + 1);
	    bcopy(qualifiedDir, tmp, slash - qualifiedDir);
	    tmp[slash - qualifiedDir] = 0;
	    matchPattern = tmp;
	}
    }

    if (matchPattern[0] == '/')
	strcpy(buf, matchPattern);
    else {
	strcpy(buf, qualifiedDir);
	for (i=0; buf[i]; i++) ;
	i--;
	if (buf[i] != '/')
		strcat(buf, "/");
	strcat(buf, matchPattern);
    }

    XdbDebug(__FILE__, NULL, "_XmOSGetDirEntries -> work on '%s'\n", buf);

    bzero((void *)&result, sizeof(result));
    i = _Lesstif_glob(buf, flags, NULL, &result);
    if (i)
	return;

    max = *pNumAlloc;
    if (!max) {
	max = 64;
	ret = (String *)XtCalloc(sizeof(String *), max);
    }
    for (i = 0, cnt = *pNumEntries; i < result.gl_pathc; i++) {
	if (cnt == max) {
	    max *= 2;
	    ret = (String *)XtRealloc((char *)ret, max * sizeof(String *));
	}
	if (fileType == XmFILE_ANY_TYPE) {
	    if (result.gl_pathv[i][strlen(result.gl_pathv[i])-1] == '/')
		result.gl_pathv[i][strlen(result.gl_pathv[i])-1] = 0;
	    if (listWithFullPath) {
		if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    }
	    else {
		if ((tmp = strrchr(result.gl_pathv[i], '/')) == NULL)
		    _XmError(NULL, "No '/' in path!\n");
		if ((ret[cnt] = XtNewString(tmp+1)) == NULL)
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    }
	    cnt++;
	}
	else if (fileType == XmFILE_REGULAR &&
		 result.gl_pathv[i][strlen(result.gl_pathv[i])-1] != '/') {
	    if (listWithFullPath) {
		if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    }
	    else {
		if ((tmp = strrchr(result.gl_pathv[i], '/')) == NULL)
		    _XmError(NULL, "No '/' in path!\n");
		if ((ret[cnt] = XtNewString(tmp+1)) == NULL)
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    }
	    cnt++;
	}
	else if (fileType == XmFILE_DIRECTORY &&
		 result.gl_pathv[i][strlen(result.gl_pathv[i])-1] == '/') {
	    if (result.gl_pathv[i][strlen(result.gl_pathv[i])-1] == '/')
		result.gl_pathv[i][strlen(result.gl_pathv[i])-1] = 0;
	    if (listWithFullPath) {
		if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    }
	    else {
		if ((tmp = strrchr(result.gl_pathv[i], '/')) == NULL)
		    _XmError(NULL, "No '/' in path!\n");
		if ((ret[cnt] = XtNewString(tmp+1)) == NULL)
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    }
	    cnt++;
	}
    }
    _Lesstif_globfree(&result);
    *pNumAlloc = max;
    if (cnt == 0) {
	XtFree((char *)ret);
	*pEntries = NULL;
	*pNumEntries = 0;
    }
    else {
	*pNumEntries = cnt;
	*pEntries = ret;
    }

    if (XdbInDebug(__FILE__, NULL)) {
	int	i;

	XdbDebug(__FILE__, NULL, "_XmOSGetDirEntries: %d results\n", cnt);
	for (i=0; i<cnt; i++)
		XdbDebug(__FILE__, NULL, "\t[%d] - %s\n", i, ret[i]);
    }
}

void
_XmOSBuildFileList(String dirPath,
		   String pattern,
		   unsigned char typeMask,
		   String **pEntries,
		   unsigned int *pNumEntries,
		   unsigned int *pNumAlloc) 
{
    glob_t result;
    char buf[2048];
    int i, cnt, max;
    String *ret = NULL;
    int flags = GLOB_MARK | GLOB_PERIOD | GLOB_NOSORT;

    XdbDebug(__FILE__, NULL, "_XmOSBuildFileList(%s,%s)\n", dirPath, pattern);

    _XmOSGetDotDot(dirPath);
    _XmOSGetDotDot(pattern);

    if (strlen(dirPath) == 0)
	dirPath = "*";

    if (pattern[0] == '/')
	strcpy(buf, pattern);
    else {
	strcpy(buf, dirPath);
	for (i=0; buf[i]; i++) ;
	i--;
	if (buf[i] != '/')
		strcat(buf, "/");
	strcat(buf, pattern);
    }

    i = _Lesstif_glob(buf, flags, NULL, &result);
    if (i)
	return;

    max = *pNumAlloc;
    if (!max) {
	max = 64;
	ret = (String *)XtCalloc(sizeof(String *), max);
    }
    for (i = 0, cnt = *pNumEntries; i < result.gl_pathc; i++) {
	if (cnt == max) {
	    max *= 2;
	    ret = (String *)XtRealloc((char *)ret, max * sizeof(String *));
	}
	if (typeMask & XmFILE_REGULAR &&
	    result.gl_pathv[i][strlen(result.gl_pathv[i])-1] != '/') {
	    if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
		_XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    cnt++;
	}
	if (typeMask & XmFILE_DIRECTORY &&
	    result.gl_pathv[i][strlen(result.gl_pathv[i])-1] == '/') {
	    if (result.gl_pathv[i][strlen(result.gl_pathv[i])-1] == '/')
		result.gl_pathv[i][strlen(result.gl_pathv[i])-1] = 0;
	    if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
		_XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    cnt++;
	}
    }
    _Lesstif_globfree(&result);
    *pNumAlloc = max;
    if (cnt == 0) {
	XtFree((char *)ret);
	*pEntries = NULL;
	*pNumEntries = 0;
    }
    else {
	*pNumEntries = cnt;
	*pEntries = ret;
    }

    if (XdbInDebug(__FILE__, NULL)) {
	int	i;

	XdbDebug(__FILE__, NULL, "_XmOSBuildFileList: %d results\n", cnt);
	for (i=0; i<cnt; i++)
		XdbDebug(__FILE__, NULL, "\t[%d] - %s\n", i, ret[i]);
    }
}

/*
 * a sort function, perhaps?
 */
int
_XmOSFileCompare(const void *sp1,
		 const void *sp2) 
{
    return strcmp(sp1, sp2);
}

char    _XmSDEFAULT_FONT[] = "fixed";
char    _XmSDEFAULT_BACKGROUND[] = "Blue";

