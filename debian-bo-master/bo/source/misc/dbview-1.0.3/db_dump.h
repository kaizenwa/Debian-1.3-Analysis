/*
    db_dump.h - Routines for reading dBase III files
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
 *
 */

#define DB_FL_BROWSE	0x01
#define DB_FL_INFO  	0x02
#define DB_FL_DESCR 	0x04
#define DB_FL_RESERVE	0x08
#define DB_FL_OMIT	0x10
#define DB_FL_TRIM	0x20

typedef struct dbase_head { 
    unsigned char	version;		/* 03 for dbIII and 83 for dbIII w/memo file */
    unsigned char	l_update[3];		/* yymmdd for last update*/
    unsigned long	count;			/* number of records in file*/
    unsigned short	header;			/* length of the header
						 * includes the \r at end
						 */
    unsigned short	lrecl;			/* length of a record
						 * includes the delete
						 * byte
						 */
    unsigned char   reserv[20];
    } DBASE_HEAD;

#define DB_FLD_CHAR  'C'
#define DB_FLD_NUM   'N'
#define DB_FLD_LOGIC 'L'
#define DB_FLD_MEMO  'M'
#define DB_FLD_DATE  'D'
 
typedef struct dbase_fld {
    char    name[11];                                           /*field name*/
    char    type;                                               /*field type*/
    /* A-T uses large data model but drop it for now */
    char   *data_ptr;                                           /*pointer into buffer*/
    unsigned char length;                                       /*field length*/
    char   dec_point;                         /*field decimal point*/
    char   fill[14];
    } DBASE_FIELD;
 
typedef struct fld_list {
    struct fld_list *next;
    DBASE_FIELD     *fld;
    char            *data;
    } FLD_LIST;

void
db3_process(char*, int, char);

/******************************************************
                                         db3_read_dic()
This function is called with a file name to
read to create a record type to support the
dbase file
******************************************************/
 
void
db3_read_dic(int);
 
/******************************************************
                                        db3_print_recs()
Read records and print the data
******************************************************/
 
void
db3_print_recs(int, int, char);
 
/******************************************************
                                          db3_print()
Print a single record
******************************************************/
 
void
db3_print(int, char);
 
/******************************************************
                                         stack_field()
Add a field to the linked list of fields
******************************************************/
 
void
stack_field(DBASE_FIELD *);
