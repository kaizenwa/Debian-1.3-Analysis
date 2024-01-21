/*
** nis_names.c
**
** Copyright (c) 1993 Signum Support AB, Sweden
**
** This file is part of the NYS Library.
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
**
** Author: Peter Eriksson <pen@signum.se>
*/

#include <stdio.h>
#include <rpc/rpc.h>
#include <rpcsvc/nis.h>
#include "nis_conf.h"
#include "xalloc.h"


nis_name nis_local_directory(void)
{
    static char *sbuf = NULL;
    char buf[1024];
    int len;

    if (nis_setup())
	return NULL;
    
    if (_nis_config->domainname)
	return _nis_config->domainname;
	
    if (getdomainname(buf, sizeof(buf)) < 0)
	return NULL;

    len = strlen(buf);

    /* Missing trailing dot? */
    if (buf[len-1] != '.')
    {
	buf[len++] = '.';
	buf[len] = '\0';
    }

    if (Xalloc(&sbuf, len+1) == NULL)
	return NULL;

    strcpy(sbuf, buf);
    
    return sbuf;
}


nis_name nis_local_host(void)
{
    static char *sbuf = NULL;
    char buf[1024], *dir;
    int len;

    
    if (gethostname(buf, sizeof(buf)) < 0)
	return NULL;

    len = strlen(buf);
    if (len < 1)
	return NULL;

    /* Hostname already fully qualified? */
    if (buf[len-1] == '.')
	goto exit;

    dir = nis_local_directory();
    if (dir == NULL)
	return NULL;
    
    buf[len] = '.';
    strcpy(buf+len+1, dir);

  exit:
    if (Xalloc(&sbuf, strlen(buf)+1) == NULL)
	return NULL;

    strcpy(sbuf, buf);
    return sbuf;
}

nis_name nis_local_principal(void)
{
  static char principal_name[NIS_MAXNAMELEN + 1];
  char buf[NIS_MAXNAMELEN + 1];
  uid_t uid;
  nis_result *res;

  uid = geteuid ();
  
  if (uid != 0)
    {
      sprintf(buf, "[auth_name=%d,auth_type=LOCAL],cred.org_dir.%s", 
              uid, nis_local_directory ());
      
      if (buf[strlen(buf)-1] != '.')
        strcat(buf, ".");
      
      res = nis_list(buf, FOLLOW_LINKS+FOLLOW_PATH, NULL, NULL);
      
      if (res == NULL) 
        return "nobody";
      
      if (res->status == NIS_SUCCESS) 
        {
          if (res->objects.objects_len > 1)
            {
              /*
               * More than one principal with same uid?
               * something wrong with cred table. Should be unique
               * Warn user and continue.
               */
	      printf ("LOCAL entry for %d in directory %s not unique\n",
		      uid, nis_local_directory ());
            }
          strcpy (principal_name, ENTRY_VAL(res->objects.objects_val, 0));
          nis_freeresult (res);
          return principal_name;	  
        }
      else
        {
          nis_freeresult(res);
          return "nobody";
        }
    }
  else
    return nis_local_host ();
  
  return "nobody";
}


nis_name nis_local_group(void)
{
  static char default_group[NIS_MAXNAMELEN];
  char *cptr;
  
  if ((cptr=getenv("NIS_GROUP"))==NULL)
    return "";

  if (strlen(cptr) >= sizeof(default_group))
    return "";
  
  strcpy(default_group, cptr);
  if (default_group[strlen(default_group)-1] != '.') 
    {
      cptr = nis_local_directory();
      if(strlen(default_group)+strlen(cptr) + 1 < sizeof(default_group))
        {
          strcat(default_group,".");
          strcat(default_group,cptr);
        }
      else   
        return "";
    }
  return default_group;
}





static int insert_name(int *pos,
		       int *size,
		       nis_name **rnames,
		       nis_name name,
		       nis_name dir)
{
    int len;

    
    while (*pos >= *size)
    {
	(*size) += 10;
	if (Xalloc(rnames, (*size)+1) == NULL)
	    return -1;
    }

    len = strlen(name);
    
    if (Xalloc(&(*rnames)[*pos], len+strlen(dir)+2) == NULL)
	return -1;

    strcpy((*rnames)[*pos], name);
    (*rnames)[*pos][len] = '.';
    strcpy((*rnames)[*pos]+len+1, dir);

    ++(*pos);
    (*rnames)[*pos] = NULL;

    return 0;
}


static int insert_dirpath(int *pos,
			  int *size,
			  nis_name **rnames,
			  nis_name name)
{
    char *sdir, *dir, *cp;
    int tobottom = 0;


    dir = nis_local_directory();
    if (dir == NULL)
	return -1;
    sdir = strdup(dir);
    if (sdir == NULL)
	return -1;

    cp = sdir;
    dir = sdir;
    
    tobottom = (strchr(name, '.') != NULL);

    while ((cp = strchr(dir, '.')) != NULL && (cp[1] != '\0' || tobottom))
    {
	if (insert_name(pos, size, rnames, name, dir) < 0)
	{
	    free(sdir);
	    return -1;
	}

	dir = cp+1;
    }

    if (tobottom)
	if (insert_name(pos, size, rnames, name, "") < 0)
	{
	    free(sdir);
	    return -1;
	}

    free(sdir);
    return 0;
}


static nis_name expand_wild(nis_name wild)
{
    int i, len, dlen;
    char *dir, *cp;
    char *rname = NULL;
    

    dir = nis_local_directory();
    if (dir == NULL)
	return NULL;

    dlen = strlen(dir);

    len = 0;
    for (i = 0; wild[i] != '\0'; i++)
	if (wild[i] == '$')
	    len += dlen;
	else
	    len++;

    if (Xalloc(&rname, len+1) == NULL)
	return NULL;

    cp = rname;
    
    for (i = 0; wild[i] != '\0'; i++)
	if (wild[i] == '$')
	{
	    strcpy(cp, dir);
	    while (*cp)
		cp++;
	}
	else
	    *cp++ = wild[i];

    *cp = '\0';
    return rname;
}


nis_name *nis_getnames(const nis_name name)
{
    nis_name *rnames = NULL;
    char *path, *cp;
    int len, size;
    
    
    len = strlen(name);
    if (name[len-1] == '.')
    {
	if (Xalloc(&rnames, 2) == NULL)
	    return NULL;
	
	if ((rnames[0] = strdup(name)) == NULL)
	{
	    free(rnames);
	    return NULL;
	}

	rnames[1] = NULL;
	
	return rnames;
    }

    path = getenv("NIS_PATH");
    if (path == NULL)
	path = "$";
    
    path = strdup(path);
    if (path == NULL)
	return NULL;
    
    size = 0;
    len = 0;

    cp = strtok(path, ":");
    while (cp)
    {
	if (strcmp(cp, "$") == 0)
	{
	    if (insert_dirpath(&len, &size, &rnames, name) < 0)
		goto error;
	}
	else
	{
	    char *tmp;

	    tmp = expand_wild(cp);
	    if (tmp == NULL)
		goto error;

	    if (insert_name(&len, &size, &rnames, name, tmp) < 0)
	    {
		free(tmp);
		goto error;
	    }

	    free(tmp);
	}
	
	cp = strtok(NULL, ":");
    }

    free(path);
    
    return rnames;

  error:
    free(path);
    if (rnames)
	free(rnames);
    
    return NULL;
}



void nis_freenames(nis_name *names)
{
    int i;

    for (i = 0; names[i]; i++)
	free(names[i]);

    free(names);
}
