/*

Copyright 1990 by Cray Research, Inc.

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of Cray Research, Inc. not be used in 
advertising or publicity pertaining to distribution of the software without
specific, written prior permission.  Cray Research, Inc. makes no 
representations about the suitability of this software for any purpose.  It 
is provided "as is" without express or implied warranty.

*/

static char vars_rcsid[]="$Id: get_UV_value.c,v 1.2 1994/09/19 21:23:29 bobo Exp $";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include "defs.h"
#include "vardefs.h"

char value_return[1024];
extern int VART_LEN;
extern struct var_ent var_table[];

char *
get_variable_value(vn)
char *vn;
{
int var_index;
char *tmp_val;

	for(var_index=0;var_index<VART_LEN;var_index++)
	{
		if(strcmp(vn,var_table[var_index].name)==0)
		{
			if(var_table[var_index].type == VARINT)
			{
				sprintf(value_return,"%d",*(var_table[var_index].place));
				return(value_return);
			}
			else
			{
				if(*(var_table[var_index].place)==0)
					return(getenv(vn));
				return((char *)*(var_table[var_index].place));
			}
		}
	}

	tmp_val=getenv(vn);
	if(tmp_val==(char )0)
	{
		sprintf(value_return,"$%s",vn);
		return(value_return);
	}
	else
		return(tmp_val);
}

char *
get_user_value(un)
char *un;
{
struct passwd *my_pwent;

	if(strlen(un)==0)
		return(getenv("HOME"));

	my_pwent=getpwnam(un);
	if(my_pwent==(struct passwd *)0)
	{
		value_return[0]=(char)0;
	}
	else
	{
		strcpy(value_return,my_pwent->pw_dir);
	}
	endpwent();
	return(value_return);
}				
	
	

