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

static char vars_rcsid[]="$Id: expand_value.c,v 1.2 1994/09/19 21:22:32 bobo Exp $";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include "defs.h"


extern char *folder;
	
char *
expand_value(ln,dof)
char *ln;
int	dof; /* do folder expansion */
{
char *tmp_ptr,*output_ptr,*name_ptr,tmp_data[1024];
int var_index;
int ln_index,name_len;

	output_ptr=(char *)malloc(1);
	output_ptr[0]=(char)0;

	for(ln_index=0;ln_index<strlen(ln);ln_index++)
	{
		switch((int)ln[ln_index])
		{
			case (int)'$':
				/*
				 * it's a variable
				 */
				ln_index++;
				name_len=strcspn(&ln[ln_index],
					" \t~!@#$%^&*()-_+={[}]|\\:;\"'<,>.?/");
				name_ptr=(char *)malloc(name_len+1);
#ifdef SYSV
				memcpy(name_ptr,&ln[ln_index],name_len);
#else
				bcopy(&ln[ln_index],name_ptr,name_len);
#endif
				name_ptr[name_len]=(char)0;
				tmp_ptr=(char *)get_variable_value(name_ptr);
				/*
				 * increment ln_index to the end of the
				 * variable name.
				 */
				ln_index+=name_len-1;
				break;
			case (int)'~':
				/*
				 * it's a user thing.
				 */
				ln_index++;
				name_len=strcspn(&ln[ln_index],
					" \t~!@#$%^&*()-_+={[}]|\\:;\"'<,>.?/");
				name_ptr=(char *)malloc(name_len+1);
#ifdef SYSV
				memcpy(name_ptr,&ln[ln_index],name_len);
#else
				bcopy(&ln[ln_index],name_ptr,name_len);
#endif
				name_ptr[name_len]=(char)0;
				tmp_ptr=(char *)get_user_value(name_ptr);
				/*
				 * increment ln_index to the end of the
				 * user name.
				 */
				ln_index+=name_len-1;
				break;
			case (int)'\\':
				/*
				 * it's an escaped character.
				 */
				ln_index++;
				tmp_ptr=tmp_data;
				tmp_ptr[1]=(char)0;

				switch((int)ln[ln_index])
				{
					case (int)'n':
						tmp_ptr[0]='\n';
						break;
					case (int)'r':
						tmp_ptr[0]='\r';
						break;
					case (int)'t':
						tmp_ptr[0]='\t';
						break;
					case (int)'b':
						tmp_ptr[0]='\b';
						break;
					default:
						tmp_ptr[0]=ln[ln_index];
						break;
				}
				break;
			case (int)'+':
				if(!dof)
				{
					/*
					 * since we aren't doing folder
					 * expansion, we have to copy the "+"
					 * to the output buffer.
					 */
					tmp_ptr=tmp_data;
					tmp_ptr[0]='+';
					tmp_ptr[1]=(char)0;
					break;
				}
				/*
				 * it's something relative to the folder
				 * variable value.
				 */
				if(folder!=(char *)0)
				{
					/*
					 * we don't want to recursivly
					 * expand the folder variable if
					 * it contains a "+" character.
					 */
					tmp_ptr=expand_value(folder,0);
					sprintf(tmp_data,"%s/",tmp_ptr);
					free(tmp_ptr);
				}
				else
				{
					tmp_data[0]='+';
					tmp_data[1]=(char)0;
				}
				tmp_ptr=tmp_data;
				break;
			default:
				/*
				 * it's just a character.
				 */
				tmp_ptr=tmp_data;
				tmp_ptr[0]=ln[ln_index];
				tmp_ptr[1]=(char)0;
				break;
			}

		output_ptr=(char *)realloc(output_ptr,
					strlen(output_ptr)+
					strlen(tmp_ptr) + 2);
		strcat(output_ptr,tmp_ptr);
	}
	return(output_ptr);
}

	

