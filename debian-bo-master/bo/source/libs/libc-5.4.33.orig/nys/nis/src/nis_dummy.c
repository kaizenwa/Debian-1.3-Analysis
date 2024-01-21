/*
** nis_dummy.c               Dummy (to be implemented) functions in NIS+
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

#include "config.h"

#ifdef ENABLE_NISEMU

#include <stdio.h>
#include <rpc/rpc.h>
#include "rpcsvc/nis.h"
#include "nis_alloc.h"
#include "nis_conf.h"
#include "xalloc.h"

nis_result *nis_add_entry(const nis_name, const nis_object *, const u_long);
nis_result *nis_remove_entry(const nis_name, const u_long);
nis_result *nis_modify_entry(const nis_name, const nis_object *, const u_long);
nis_error nis_mkdir(const nis_name, const nis_server *);
nis_error nis_rmdir(const nis_name, const nis_server *);
nis_error nis_servstate(const nis_server *, const nis_tag *, const int, nis_tag **);
nis_error nis_stats(const nis_server *, const nis_tag *, const int, nis_tag **);
void nis_freetags(nis_tag *, const int);
nis_server **nis_getservlist(const nis_name);
void nis_freeservlist(nis_server **);
bool_t nis_ismember(const nis_name, const nis_name);
nis_error nis_addmember(const nis_name, const nis_name group);
nis_error nis_removemember(const nis_name, const nis_name);
nis_error nis_creategroup(const nis_name, const u_long);
nis_error nis_destroygroup(const nis_name);
void nis_print_group_entry(const nis_name);
nis_error nis_verifygroup(const nis_name);

nis_result *
nis_add_entry(const nis_name name,
			  const nis_object *object,
			  const u_long flags)
{
    return NULL;
}

nis_result *
nis_remove_entry(const nis_name name,
			     const u_long flags)
{
    return NULL;
}

nis_result *
nis_modify_entry(const nis_name name,
			     const nis_object *object,
			     const u_long flags)
{
    return NULL;
}

nis_error
nis_mkdir(const nis_name dirname,
          const nis_server *machine)
{
    return NIS_SYSTEMERROR;
}

nis_error
nis_rmdir(const nis_name dirname,
          const nis_server *machine)
{
    return NIS_SYSTEMERROR;
}

nis_error
nis_servstate(const nis_server *machine,
              const nis_tag *tags,
              const int numtags,
              nis_tag **result)
{
    return NIS_SYSTEMERROR;
}

nis_error
nis_stats(const nis_server *machine,
          const nis_tag *tags,
          const int numtags,
          nis_tag **result)
{
    return NIS_SYSTEMERROR;
}

void
nis_freetags(nis_tag *tags,
             const int numtags)
{
}

nis_server **
nis_getservlist(const nis_name dirname)
{
    return NULL;
}

void
nis_freeservlist(nis_server **machines)
{
}


bool_t
nis_ismember(const nis_name principal,
             const nis_name group)
{
    return FALSE;
}

nis_error
nis_addmember(const nis_name member,
              const nis_name group)
{
    return NIS_SYSTEMERROR;
}

nis_error
nis_removemember(const nis_name member,
                 const nis_name group)
{
    return NIS_SYSTEMERROR;
}

nis_error
nis_creategroup(const nis_name group,
                const u_long flags)
{
    return NIS_SYSTEMERROR;
}

nis_error
nis_destroygroup(const nis_name group)
{
    return NIS_SYSTEMERROR;
}

void
nis_print_group_entry(const nis_name group)
{
    puts("FOO! nis_print_group_entry() is not yet implemented.\n");
}

nis_error
nis_verifygroup(const nis_name group)
{
    return NIS_SYSTEMERROR;
}


#endif
