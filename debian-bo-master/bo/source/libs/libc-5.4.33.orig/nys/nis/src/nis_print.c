/*
** nis_print.c
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
#include <rpcsvc/nis.h>
#include "nis_utils.h"
#include "nis_print.h"


void
nis_fprint_directory(const directory_obj *dob, int ind, FILE *fp)
{
    fprintf(fp, "%*sName = %s\n", ind, "", dob->do_name);
    fprintf(fp, "%*sType = %d\n", ind, "", dob->do_type);
    fprintf(fp, "%*sTTL = %lu (seconds)\n", ind, "", dob->do_ttl);
}

void
nis_print_directory(const directory_obj *dob)
{
    nis_fprint_directory(dob, 0, stdout);
}


void
nis_fprint_group(const group_obj *go, int ind, FILE *fp)
{
    unsigned int i;
    
    fprintf(fp, "%*sFlags = 0x%08lX\n", ind, "", go->gr_flags);
    fprintf(fp, "%*sMembers = %d:\n", ind, "", go->gr_members.gr_members_len);

    for (i = 0; i < go->gr_members.gr_members_len; i++)
	fprintf(fp, "%*s  1: %*s\n", ind, "", i,
		go->gr_members.gr_members_val[i]);
}

void
nis_print_group(const group_obj *go)
{
    nis_fprint_group(go, 0, stdout);
}


void
nis_fprint_table_col(const table_col *tc, int ind, FILE *fp)
{
   fprintf(fp, "%*sName   = %s\n", ind, "", tc->tc_name);
   fprintf(fp, "%*sFlags  = 0x%08lX (%s)\n", ind, "", tc->tc_flags,
	  	   nis_taflags2str(tc->tc_flags, NULL));
   fprintf(fp, "%*sRights = 0x%08lX (%s)\n", ind, "", tc->tc_rights,
	  nis_access2str(tc->tc_rights, NULL));
}


void
nis_fprint_table(const table_obj *to, int ind, FILE *fp)
{
    unsigned int i;
    
    fprintf(fp, "%*sType = %s\n", ind, "", to->ta_type);
    fprintf(fp, "%*sMaxCol = %d\n", ind, "", to->ta_maxcol);
    fprintf(fp, "%*sSeparator = '%c' (%d)\n", ind, "", to->ta_sep, to->ta_sep);

    for (i = 0; i < to->ta_cols.ta_cols_len; i++)
    {
	fprintf(fp, "%*s  Column #%d:\n", ind, "", i);
	nis_fprint_table_col(&to->ta_cols.ta_cols_val[i], ind+4, fp);
    }

    fprintf(fp, "%*sPath = %s\n", ind, "", to->ta_path);
}

void
nis_print_table(const table_obj *to)
{
    nis_fprint_table(to, 0, stdout);
}


void
nis_fprint_entry_col(const entry_col *ec, int ind, FILE *fp)
{
    fprintf(fp, "%*sFlags = %08lX (%s)\n", ind, "", ec->ec_flags,
	   nis_enflags2str(ec->ec_flags, NULL));
    fprintf(fp, "%*sValue length = %d\n", ind, "", ec->ec_value.ec_value_len);
    fprintf(fp, "%*sValue data   = %s\n", ind, "", ec->ec_value.ec_value_val);
}


void
nis_fprint_entry(const entry_obj *eo, int ind, FILE *fp)
{
    unsigned int i;
    
    fprintf(fp, "%*sType = %s\n", ind, "", eo->en_type);
    fprintf(fp, "%*sValues = %d\n", ind, "", eo->en_cols.en_cols_len);
    for (i = 0; i < eo->en_cols.en_cols_len; i++)
    {
	fprintf(fp, "%*s  Value #%d:\n", ind, "", i);
	nis_fprint_entry_col(&eo->en_cols.en_cols_val[i], ind+4, fp);
    }
}

void
nis_print_entry(const entry_obj *eo)
{
    nis_fprint_entry(eo, 0, stdout);
}


void
nis_fprint_attr(const nis_attr *at, int ind, FILE *fp)
{
    fprintf(fp, "%*sNDX = %s\n", ind, "", at->zattr_ndx);
    fprintf(fp, "%*sLength = %d\n", ind, "", at->zattr_val.zattr_val_len);
}

void
nis_print_attr(const nis_attr *at)
{
    nis_fprint_attr(at, 0, stdout);
}


void
nis_fprint_link(const link_obj *lo, int ind, FILE *fp)
{
    unsigned int i;
    
    fprintf(fp, "%*sType = %d\n", ind, "", lo->li_rtype);
    fprintf(fp, "%*sName = %s\n", ind, "", lo->li_name);
    fprintf(fp, "%*sAttributes = %d\n", ind, "", lo->li_attrs.li_attrs_len);
    for (i = 0; i < lo->li_attrs.li_attrs_len; i++)
    {
	fprintf(fp, "%*s  Attribute #%d:\n", ind, "", i);
	nis_fprint_attr(&lo->li_attrs.li_attrs_val[i], ind+4, fp);
    }
}

void
nis_print_link(const link_obj *lo)
{
    nis_fprint_link(lo, 0, stdout);
}


void
nis_fprint_objdata(const objdata *obd, int ind, FILE *fp)
{
    fprintf(fp, "%*sObjType = %d ", ind, "", obd->zo_type);
    switch (obd->zo_type)
    {
      case BOGUS_OBJ:
	fprintf(fp, "(Bogus Object)\n");
	break;
	
      case NO_OBJ:
	fprintf(fp, "(No Object)\n");
	break;
	
      case DIRECTORY_OBJ:
	fprintf(fp, "(Directory)\n");
	nis_fprint_directory(&obd->objdata_u.di_data, ind+4, fp);
	break;

      case GROUP_OBJ:
	fprintf(fp, "(Group)\n");
	nis_fprint_group(&obd->objdata_u.gr_data, ind+4, fp);
	break;

      case TABLE_OBJ:
	fprintf(fp, "(Table)\n");
	nis_fprint_table(&obd->objdata_u.ta_data, ind+4, fp);
	break;

      case ENTRY_OBJ:
	fprintf(fp, "(Entry)\n");
	nis_fprint_entry(&obd->objdata_u.en_data, ind+4, fp);
	break;

      case LINK_OBJ:
	fprintf(fp, "(Link)\n");
	nis_fprint_link(&obd->objdata_u.li_data, ind+4, fp);
	break;

      case PRIVATE_OBJ:
	fprintf(fp, "(Private)\n");
	fprintf(fp, "%*s    Data Length = %d\n", ind, "", 
	       obd->objdata_u.po_data.po_data_len);
	break;

      default:
	fprintf(fp, "(Unknown object)\n");
	break;
    }
}


void
nis_fprint_object(const nis_object *ob, int ind, FILE *fp)
{
    fprintf(fp, "%*sObject created at %s", ind, "", ctime(&ob->zo_oid.ctime));
    fprintf(fp, "%*s last modified at %s", ind, "", ctime(&ob->zo_oid.mtime));
    fprintf(fp, "%*sName   = \"%s\"\n", ind, "", ob->zo_name);
    fprintf(fp, "%*sOwner  = \"%s\"\n", ind, "", ob->zo_owner);
    fprintf(fp, "%*sGroup  = \"%s\"\n", ind, "", ob->zo_group);
    fprintf(fp, "%*sDomain = \"%s\"\n", ind, "", ob->zo_domain);
    fprintf(fp, "%*sRights = 0x%08lX (%s)\n", ind, "", ob->zo_access,
	   nis_access2str(ob->zo_access, NULL));
    fprintf(fp, "%*sTTL    = %lu (seconds)\n", ind, "", ob->zo_ttl);
    
    nis_fprint_objdata(&ob->zo_data, ind+4, fp);
}

void
nis_print_object(const nis_object *ob)
{
    nis_fprint_object(ob, 0, stdout);
}


void
nis_fprint_result(const nis_result *nsres, int ind, FILE *fp)
{
    unsigned int i;

    
    fprintf(fp, "%*sStatus = %d\n", ind, "", nsres->status);
    fprintf(fp, "%*sNumber of objects = %d\n", ind, "",
	    nsres->objects.objects_len);
    
    for (i = 0; i < nsres->objects.objects_len; i++)
    {
	fprintf(fp, "%*s  Object #%d:\n", ind, "", i);
	nis_fprint_object(&nsres->objects.objects_val[i], ind+4, fp);
    }
}

void
nis_print_result(const nis_result *res)
{
    nis_fprint_result(res, 0, stdout);
}
