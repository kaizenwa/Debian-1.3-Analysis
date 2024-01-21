/*
    db_dump.c - Routines for reading dBase III files
    Copyright (c) 1995  Martin Schulze <Martin.Schulze@Infodrom.North.DE>

    This file is part of the dbview package, a viewer for dBase II files.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


/*
    Most of the code in this file comes from Greg Twaites anno 87. I
    only took the file and wrote a program around it. I enclude the
    whole header. I have obtained this file from a free software
    archive, namely nic.funet.fi.

      ftp://nic.funet.fi/pub/msdos/languages/c/dbase.c
*/

/*
 * These functions are used to read dbase files.
 *
 * These functions are provided by Valour Software as a gift.
 *
 * The program is for test purposes only.  No warranty is expressed nor
 * implied. USE AT YOUR OWN RISK!
 *
 * Fri Jun 21 19:30:44 1996:  Martin Schulze <joey@infodrom.north.de>
 *	Fixed a bug which causes dbview to eat a character at the end
 *	of beginning of a line.  Thanks to Chris Childs
 *	<cchilds@arnold.pinc.com> for submitting a patch.
 *
 * Thu Sep 26 21:38:33 1996:  Martin Schulze <joey@infodrom.north.de>
 *	Modified on some places based on corrections from Vladimir
 *	Michl <Vladimir.Michl@upol.cz>
 *
 */
 
#include "db_dump.h"
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>
#include <malloc.h>
#include <ctype.h>
#include <string.h>
 
DBASE_HEAD  dbhead={0};
FLD_LIST    *db_fld_root=0;
char        *Buffer;
char        buf_work[255];
int         dbfile;
 
/*------------------------------------------------------------code-------*/
void
db3_process(dbfn, flags, delim)
char    *dbfn;
int	flags;
char	delim;
{
    dbfile=open(dbfn,O_RDONLY);
    if (dbfile == -1) {
        printf("Unable to open file `%s'\n", dbfn);
	return;
    }
    db3_read_dic(flags);
    if ( ! (flags & DB_FL_OMIT))
        db3_print_recs(dbhead.count, flags, delim);
    close(dbfile);
}
 

/******************************************************
                                        db3_rm_ld_spc()
This function removes leading space characters and
returns a new string;
******************************************************/

char *
db3_rm_ld_spc (var)
char	*var;
{
    static char rvar[257];	/* dBase fields can only
				 * be 256 byts long, max. 
				 */
    char *c;

    bzero (rvar, sizeof(rvar));
    for (c=var; isspace (*c); c++);
    strcpy(rvar, c);
    return rvar;
}

/******************************************************
                                        db3_rm_tr_spc()
This function removes trailing space characters and
returns a new string;
******************************************************/

char *
db3_rm_tr_spc (var)
char	*var;
{
    char *c=var;

    for (c+=strlen(var); isspace (*c); c--);

    *(++c) = '\0';
    return var;
}

/******************************************************
                                         db3_cvt_fld()
This function is called with a valid db3 field
specifier. It will convert the fieldname into a
more friendly look.

This is done by tr '_' ' ' and tr [:upper:] [:lower:].
******************************************************/

void
db3_cvt_fld (fld)
DBASE_FIELD	*fld;
{
    char *c;
    char first=1;

    for (c=fld->name; *c; c++)
	if (!first) {
	    if (isupper(*c))
		*c = tolower(*c);
	    else
		if (*c == '_')
		    { *c = ' '; first = 1; }
	    first = 0;
	}
	else
	    first = 0;
}

 
/******************************************************
                                         db3_read_dic()
This function is called with a file name to
read to create a record type to support the
dbase file
******************************************************/
 
void
db3_read_dic(flags)
int	flags;
{
    int             fields;
    DBASE_FIELD     *fld;

    if(dbfile==-1) {
        printf("open failed");
        return;
        }
    read(dbfile,&dbhead,sizeof(DBASE_HEAD));
    if( !(dbhead.version==3 || dbhead.version==0x83) ) {
        printf ("Version %d not supported\n",dbhead.version);
	if(dbhead.version==0x8b ) {
	    printf ("dBase IV - partially known...\n");
	}
	return;
    }

    if (flags & DB_FL_INFO) {
	printf("File version  : %d\n",dbhead.version);
	printf("Last update   : %02d/%02d/%2d\n", dbhead.l_update[1],dbhead.l_update[2],dbhead.l_update[0]);
	printf("Number of recs: %ld\n",dbhead.count);
	printf("Header length : %d\n",dbhead.header);
	printf("Record length : %d\n",dbhead.lrecl);
    }

    Buffer=malloc(dbhead.lrecl);
 
    fields=(dbhead.header-1)/32-1;
    
    if (flags & DB_FL_DESCR) {
	printf("Field Name\tType\tLength\tDecimal Pos\n");
    }
    while(fields--) {
	fld=(DBASE_FIELD *)malloc(sizeof(DBASE_FIELD));
	if (!fld) {
	    printf ("Not enough memory\n");
	    return;
	}
	read(dbfile,fld,sizeof(DBASE_FIELD));
	if (! (flags & DB_FL_RESERVE))
	    db3_cvt_fld (fld);
	if (flags & DB_FL_DESCR) {
	    printf("%-10s\t  %c\t  %3d\t  %3d\n", fld->name, fld->type,
		   fld->length,fld->dec_point);
	}
	stack_field(fld);
    }
    read(dbfile,Buffer,1);              /* read the silly little \r 0x0d character */
    read(dbfile,Buffer,1);              /* strange, it only works if we read another byte */

    return;
}
 
/******************************************************
                                        db3_print_recs()
Read records and print the data
******************************************************/
 
void
db3_print_recs(cnt, flags, delim)
int     cnt;
int     flags;
char	delim;
{
    int     bytes;
 
    while(cnt) {
        bytes=read(dbfile,Buffer,dbhead.lrecl);
        if(bytes!=dbhead.lrecl)
            break;
	/* Check if deleted == '*' */
        if(Buffer[0]==' ') {
            db3_print(flags, delim);
            cnt--;
            }
        }
    return;
}
 
 
/******************************************************
                                          db3_print()
Print a single record
******************************************************/
 
void
db3_print(flags, delim)
int	flags;
char	delim;
{
    FLD_LIST    *temp;
 
    temp=db_fld_root;
    while (temp) {
        memcpy(buf_work,temp->data,temp->fld->length);
        buf_work[temp->fld->length] = '\0';
	if (flags & DB_FL_BROWSE)
	  if (flags & DB_FL_TRIM)
            printf("%s%c",db3_rm_tr_spc(db3_rm_ld_spc(buf_work)), delim);
	  else
	    printf("%s%c",buf_work, delim);
	else
	    printf("%-11s: %s\n",temp->fld->name,db3_rm_tr_spc(db3_rm_ld_spc(buf_work)));
        temp=temp->next;
        }
    printf("\n");
    return;
}
 
/******************************************************
                                         stack_field()
Add a field to the linked list of fields
******************************************************/
 
void
stack_field(fld)
DBASE_FIELD *fld;
{
    FLD_LIST    *list, *temp;
 
    list=(FLD_LIST *)calloc(1,sizeof(FLD_LIST));
    if (!list) {
        printf ("Not enough memory\n");
	return;
    }
    list->fld=fld;
    if(!db_fld_root) {
        list->data=Buffer+1;                            /*skip delete byte*/
        db_fld_root=list;
        return;
        }
    temp=db_fld_root;
    while(temp->next)
        temp=temp->next;
    temp->next=list;
    list->data=temp->data + temp->fld->length;
    return;
}
