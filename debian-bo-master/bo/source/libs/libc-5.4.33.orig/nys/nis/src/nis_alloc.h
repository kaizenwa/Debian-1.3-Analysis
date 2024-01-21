/*
** nis_alloc.h           NIS+ objects allocation / freeing routines.
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

#ifndef __NIS_OBJECTS_H__
#define __NIS_OBJECTS_H__

#include <rpcsvc/nis.h>

extern netobj *nis_copynetobj(netobj *nno, netobj *no);
extern netobj *nis_dupnetobjs(netobj *no, int len);
extern netobj *nis_dupnetobj(netobj *no);
extern void nis_cleannetobj(netobj *no);
extern void nis_freenetobjs(netobj *no, int len);
extern void nis_freenetobj(netobj *no);

extern endpoint *nis_copyendpoint(endpoint *nep, endpoint *ep);
extern endpoint *nis_dupendpoints(endpoint *ep, int len);
extern endpoint *nis_dupendpoint(endpoint *ep);
extern void nis_cleanendpoint(endpoint *ep);
extern void nis_freeendpoints(endpoint *ep, int len);
extern void nis_freeendpoint(endpoint *ep);

extern nis_server *nis_copyserver(nis_server *nns, nis_server *ns);
extern nis_server *nis_dupservers(nis_server *ns, int len);
extern nis_server *nis_dupserver(nis_server *ns);
extern void nis_cleanserver(nis_server *ns);
extern void nis_freeservers(nis_server *ns, int len);
extern void nis_freeserver(nis_server *ns);

extern directory_obj *nis_copydirectory(directory_obj *nd, directory_obj *d);
extern directory_obj *nis_dupdirectories(directory_obj *dob, int len);
extern directory_obj *nis_dupdirectory(directory_obj *dob);
extern void nis_cleandirectory(directory_obj *dob);
extern void nis_freedirectories(directory_obj *dob, int len);
extern void nis_freedirectory(directory_obj *dob);

extern group_obj *nis_copygroup(group_obj *ngo, group_obj *go);
extern group_obj *nis_dupgroups(group_obj *go, int len);
extern group_obj *nis_dupgroup(group_obj *go);
extern void nis_cleangroup(group_obj *go);
extern void nis_freegroups(group_obj *go, int len);
extern void nis_freegroup(group_obj *go);

extern table_obj *nis_copytable(table_obj *nto, table_obj *to);
extern table_obj *nis_duptables(table_obj *to, int len);
extern table_obj *nis_duptable(table_obj *to);
extern void nis_cleantable(table_obj *to);
extern void nis_freetables(table_obj *to, int len);
extern void nis_freetable(table_obj *to);

extern entry_obj *nis_copyentry(entry_obj *neo, entry_obj *eo);
extern entry_obj *nis_dupentries(entry_obj *eo, int len);
extern entry_obj *nis_dupentry(entry_obj *eo);
extern void nis_cleanentry(entry_obj *eo);;
extern void nis_freeentries(entry_obj *eo, int len);
extern void nis_freeentry(entry_obj *eo);

extern nis_attr *nis_copyattr(nis_attr *nat, nis_attr *at);
extern nis_attr *nis_dupattrs(nis_attr *at, int len);
extern nis_attr *nis_dupattr(nis_attr *at);
extern void nis_cleanattr(nis_attr *at);
extern void nis_freeattrs(nis_attr *at, int len);
extern void nis_freeattr(nis_attr *at);

extern link_obj *nis_copylink(link_obj *nlo, link_obj *lo);
extern link_obj *nis_duplinks(link_obj *lo, int len);
extern link_obj *nis_duplink(link_obj *lo);
extern void nis_cleanlink(link_obj *lo);
extern void nis_freelinks(link_obj *lo, int len);
extern void nis_freelink(link_obj *lo);

extern objdata *nis_copyobjdata(objdata *nobd, objdata *obd);
extern objdata *nis_dupobjdatas(objdata *obd, int len);
extern objdata *nis_dupobjdata(objdata *obd);
extern void nis_cleanobjdata(objdata *obd);
extern void nis_freeobjdatas(objdata *obd, int len);
extern void nis_freeobjdata(objdata *obd);

extern nis_object *nis_copyobject(nis_object *nob, nis_object *ob);
extern nis_object *nis_dupobjects(nis_object *ob, int len);
extern nis_object *nis_dupobject(nis_object *ob);
extern void nis_cleanobject(nis_object *ob);
extern void nis_freeobjects(nis_object *ob, int len);
extern void nis_freeobject(nis_object *ob);

extern nis_result *nis_copyresult(nis_result *nres, nis_result *res);
extern nis_result *nis_dupresult(nis_result *res);
extern void nis_cleanresult(nis_result *nsres);
extern void nis_freeresults(nis_result *res, int len);
extern void nis_freeresult(nis_result *res);

#endif
