/*
** nislib.h              NIS+ client access function definitions
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

#ifndef __NISLIB_NISLIB_H__
#define __NISLIB_NISLIB_H__

#include <stdio.h>

/*
** Standard NIS+ client access functions
*/
extern nis_result *nis_lookup(nis_name name, u_long flags);
extern nis_result *nis_list(const nis_name name,
			    const u_long flags,
			    int (*callback)(const nis_name name,
					    const nis_object *object,
					    const void *userdata),
			    const void *userdata);
			    
extern nis_result *nis_first_entry(const nis_name name);
extern nis_result *nis_next_entry(const nis_name name, netobj *key);

extern void nis_freeresult(nis_result *res);

extern name_pos nis_dir_cmp(const nis_name, const nis_name);
extern nis_name nis_domain_of(const nis_name);
extern nis_name nis_leaf_of(const nis_name);
extern nis_name nis_name_of(const nis_name);
extern nis_name nis_local_directory(void);
extern nis_name nis_local_host(void);
extern nis_name nis_local_group(void);
extern nis_name nis_local_principal(void);
extern nis_name *nis_getnames(const nis_name name);
extern void nis_freenames(nis_name *namelist);

extern const char *nis_sperrno(const nis_error status);
extern void nis_perror(const nis_error status, const char *label);
extern void nis_lerror(const nis_error status, const char *label);
extern char *nis_sperror(const nis_error status, const char *label);

/*
** NYS-specific NIS+ client functions, you should
** probably not use these in your programs if you care
** at all for portability
*/
extern int nis_setup(void);

extern void nis_fprint_directory(const directory_obj *dob, int ind, FILE *fp);
extern void nis_print_directory(const directory_obj *dob);
extern void nis_fprint_group(const group_obj *go, int ind, FILE *fp);
extern void nis_print_group(const group_obj *go);
extern void nis_fprint_table_col(const table_col *tc, int ind, FILE *fp);
extern void nis_fprint_table(const table_obj *to, int ind, FILE *fp);
extern void nis_print_table(const table_obj *to);
extern void nis_fprint_entry_col(const entry_col *ec, int ind, FILE *fp);
extern void nis_fprint_entry(const entry_obj *eo, int ind, FILE *fp);
extern void nis_print_entry(const entry_obj *eo);
extern void nis_fprint_attr(const nis_attr *at, int ind, FILE *fp);
extern void nis_print_attr(const nis_attr *at);
extern void nis_fprint_link(const link_obj *lo, int ind, FILE *fp);
extern void nis_print_link(const link_obj *lo);
extern void nis_fprint_objdata(const objdata *obd, int ind, FILE *fp);
extern void nis_fprint_object(const nis_object *ob, int ind, FILE *fp);
extern void nis_print_object(const nis_object *ob);
extern void nis_fprint_result(const nis_result *nsres, int ind, FILE *fp);
extern void nis_print_result(const nis_result *res);

#endif

