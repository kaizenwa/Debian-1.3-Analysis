/*
** nis_subr.c - nis_leaf_of, nis_name_of, nis_domain_of, nis_dir_cmp
**
** Copyright (c) 1996 Thorsten Kukuk, Germany
**
** This file is part of the NIS+ Server Library.
**        
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
** 
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with this library; if not, write to the Free
** Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
**
** Author: Thorsten Kukuk <kukuk@uni-paderborn.de>
**
*/

static char rcsid[] = "$Id: nis_subr.c,v 1.1 1996/09/21 14:03:15 kukuk Exp $";

#include <string.h>
#include <rpcsvc/nis.h>

nis_name nis_leaf_of(const nis_name name)
{
  static char result[NIS_MAXNAMELEN+1];
  char *cptr;

  memset(result, '\0', sizeof(result));
  strncpy(result,name,NIS_MAXNAMELEN);
  cptr = strchr(result,'.');
  if(cptr != NULL)
    *cptr = '\0';

  return result;
}

nis_name nis_name_of(const nis_name name)
{
  static char *local_domain = NULL;
  static char result[NIS_MAXNAMELEN];
  int diff;

  if (local_domain == NULL)
    local_domain = strdup(nis_local_directory());

  diff = strlen(name) - strlen(local_domain);
  if (diff <= 0)
    return NULL;

  if (strcmp(&name[diff],local_domain) != 0)
    return NULL;

  strncpy(result,name,diff-1);

  if (strlen(result) == 0)
    return NULL;

  return result;
}

nis_name nis_domain_of(const nis_name name)
{
  static char result[NIS_MAXNAMELEN];
  char *cptr;

  cptr = strchr(name,'.');
  cptr++;

  if (strlen(cptr) == 0)
    return ".";
  else
    strcpy(result,cptr);
  
  return result;
}

name_pos nis_dir_cmp(const nis_name n1, const nis_name n2)
{
  int len1, len2;

  len1 = strlen(n1);
  len2 = strlen(n2);

  if (len1 == len2)
  {
    if (strcmp(n1,n2) == 0)
      return SAME_NAME;
    else
      return NOT_SEQUENTIAL;
  }
  
  if (len1 < len2)
    {
      if(n2[len2-len1-1] != '.')
	return NOT_SEQUENTIAL;
      else
	if(strcmp(&n2[len2-len1],n1) == 0)
	  return HIGHER_NAME;
	else
	  return NOT_SEQUENTIAL;
    }
  else
    {
      if(n1[len1-len2-1] != '.')
	return NOT_SEQUENTIAL;
      else
	if(strcmp(&n1[len1-len2],n2) == 0)
	  return LOWER_NAME;
	else
	  return NOT_SEQUENTIAL;

    }
}
