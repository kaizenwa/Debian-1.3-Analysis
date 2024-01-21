/* Copyright (C) 1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Extended pascal routines.

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include "rts.h"

#include "rts-fdr.h"

#include <sys/stat.h>

/* List of bound objects */
static BINDING *Bound_objects   = (BINDING *)NULL;

/* Free list of bindings */
static BINDING *Bound_free_list = (BINDING *)NULL;

/* BIND(object, b);
 *
 * Attempt to bind OBJECT to B.NAME.
 *
 * This routine must not update any status fields in B
 *
 * BINDING (object) is used to get the binding status info
 * of OBJECT after BIND has returned.
 */
void
_p_bind (addr, b, type)
void *addr;			/* Address of the variable        */
GPC_BINDING *b;			/* Expression of type BindingType */
int type;			/* type of the variable to bind   */
{
  BINDING *item;
  int permissions = 0;
  int known_size = -1;

  /* Copy of the name, null terminated */
  char *name;
  int len = b->Name.length;

  if (type != RTS_BIND_FILE)
    {
      _p_error (REPORT, "GPC supports only binding of FILE_TYPE objects");
      return;
    }

  for (item = Bound_objects; item; item = item->next)
    if (item->addr == addr)
      {
	_p_error (ABORT, "Object (0x%lx) already bound to `%s'",
		  addr, item->name);
	return;
      }

  if (len <= 0)
    {
      _p_error (REPORT, "Can not bind to an empty string");
      return;
    }

  if (len >= BINDING_NAME_LENGTH)
    {
      _p_error (REPORT, "External names of bound objects must be shorter than %d characters", BINDING_NAME_LENGTH);
    }

  /* If we come here, the binding might work, so get a buffer for it */

  if (Bound_free_list)
    item = Bound_free_list, Bound_free_list = Bound_free_list->next;
  else
    item = (BINDING *) _p_malloc (sizeof (BINDING));

  /* Copy the name we are binding to (need it null terminated) */
  name = _p_malloc (len+1);
  strncpy (name, &b->Name.string[0], len);
  name[ len ] = '\000';

#if 0
  /* I don't know if it's correct to strip spaces around filenames */

  /* Strip leading spaces and tabs */
  while (*name == ' ' || *name == '\t')
    name++;

  {
    /* Strip trailing spaces and tabs */
    char *scan = name;

    while (*scan)
      scan++;
    do {
      scan--;
    } while (*scan == ' ' || *scan == '\t');
    *++scan = '\000';
  }

  len = b->Name.len = (scan - name);

#endif /* skip leading and trailing spaces */

  if (type == RTS_BIND_FILE)
    {
      FDR File = (FDR) addr;
      struct stat st;
      char *copy = name;

      if (m_STATUS(File) != FiNOP)
	{
	  /* @@@ Should we close it if it is opened instead of this? */
	  _p_error (REPORT, "GPC requires the file in unopened state when it is bound");
	  _p_error (REPORT, "    the binding takes effect after next open of the file");
	}

      /* Unfortunately there is no knowledge if the file will be
       * reset, rewritten or extended, so I added some fields
       * to the bindingtype to let user have a control.
       */
      
      while (1)
	{
	  /* Read permission? */
	  permissions  = (access (copy, R_OK) == 0) * 1;

	  /* write permission? */
	  permissions |= (access (copy, W_OK) == 0) * 2;

	  /* File exists and directories allow file access */
	  permissions |= (access (copy, F_OK) == 0) * 4;

	  if (stat (copy, &st) == 0)
	    {
	      /* Don't allow pascal programs to read/write directories
	       * with standard operations
	       *
	       * If we are checking access permissions for non-existing file,
	       * copy != name in the second round.
	       */

	      /* Calculate the number of elements in the file */
	      known_size = BYTENUM (File, st.st_size);

	      if (copy != name)
		{
		  if ((st.st_mode & S_IFDIR) == 0)
		    _p_error (ABORT, "Bind: Should be checking a directory after name strip");
		  /* Only write permissions are valid because the file did not exist
		   */
		  if (permissions & 2)
		    permissions = 2;
		  else
		    permissions = 0;	/* No write, No dice */

		  break;
		}
	      else if (st.st_mode & S_IFDIR)
		permissions = 0;

	      break;
	    }
	  else if (permissions)
	    break;
	  else if (errno == ENOENT)
	    {
	      char *slash;
	      /* Check for permissions to write/read the directory */
	      
	      if (copy == name)
		copy = _p_strdup (name);
	      else
		break; /* Only check the directory where the unexisting
			* file would be created (not /tmp/non1/non2/non3)
			*/

	      slash = rindex (copy, PATH_SEPARATOR);
	      if (! slash)
		{
		  /* Nonexisting file in current directory */
		  copy[0] = '.';
		  copy[1] = '\000';
		  continue;
		}
	      else if (*(slash+1) == '\000')
		break;	/* /directory/name/ending/with/slash/ */

	      /* get rid of the file component, leave the path */
	      *slash = '\000';
	    }
	}
		 
      if (name != copy)
	free (copy);

      if (! permissions)
	{
	  /* Oops, garbage collect, no permissions, no access */

	  item->next = Bound_free_list;
	  Bound_free_list = item;

	  free (name);

	  return;
	}
    }

  /* Now the binding is guaranteed to succeed */

  item->next = Bound_objects;
  Bound_objects = item;

  item->addr = addr;
  item->type = type;
  item->name = name;

  /* Copy the structure header */
  item->to = (GPC_BINDING *) _p_malloc (sizeof (GPC_BINDING));
  bcopy ((char *)b, (char *)item->to, sizeof (GPC_BINDING));

  /* Extensions */
  item->to->Readable = !!(permissions&1);
  item->to->Writable = !!(permissions&2);
  item->to->Existing = !!(permissions&4);
  item->to->Error    = 0;
  item->to->Size     = known_size;
  item->to->Extensions_valid = TRUE;

  /* Standard flag */
  item->to->Bound = TRUE;

  /* Not used yet */
  item->status = 0;
}

void
_p_binding (addr, b)
     void *addr;
     GPC_BINDING *b;
{
  BINDING *item;

  b->Bound    = FALSE;
  b->Name.length = 0;
  b->Name.string[0] = '\000';

  /* Clear extension fields */
  b->Extensions_valid = FALSE;
  b->Writable = FALSE;
  b->Readable = FALSE;
  b->Existing = FALSE;

  for (item = Bound_objects; item; item = item->next)
    if (item->addr == addr)
      {
	int len = item->to->Name.length;

	/* Copy all fields except the Name field */
	*b = *item->to;

	if (len >= BINDING_NAME_LENGTH)
	  {
	    len = BINDING_NAME_LENGTH-1;
	    b->Name.length = len;
	    _p_error (REPORT, "'binding': bound name truncated to %d characters", len);
	  }

	/* Now copy the name, does not matter if null terminated or not */
	strncpy (&b->Name.string[0], item->name, len+1);

	return;
      }
}

/* to find out where the file is bound to */
BINDING *
_p_get_binding (addr)
     FDR addr;
{
  BINDING *item;

  for (item = Bound_objects; item; item = item->next)
    if (item->addr == (void *)addr)
      return item;
	
  return (BINDING *)NULL;
}

void
_p_unbind (addr)
     void *addr;			/* Address of the variable */
{
  BINDING *item, *prev;
  for (prev = item = Bound_objects; item; item = item->next)
    {
      if (item->addr == addr)
	{
	  /* Unbinding a file closes it as well.
	   * This is an implementation defined action
	   */
	  if (item->type == RTS_BIND_FILE)
	    {
	      _p_close ((FDR)addr);
	  
	      /* Destroy all bindings between the file and the binding */
	      m_BINDING ((FDR) addr) = NULL;
	    }

	  /* First element */
	  if (item == Bound_objects)
	    Bound_objects = item->next;
	  else
	    prev->next = item->next;

	  /* Prepend this to free list */
	  item->next = Bound_free_list;
	  free (item->name);
	  item->name = NULL;

	  Bound_free_list = item;

	  return;
      }
      
      if (prev != item)
	prev = prev->next;
    }
}

