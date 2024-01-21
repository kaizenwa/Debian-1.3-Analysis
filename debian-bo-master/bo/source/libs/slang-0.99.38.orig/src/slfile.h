/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */

typedef struct 
{
   int fd;			       /* handle */
   FILE *fp;			       /* kind of obvious */
   unsigned int flags;		       /* modes, etc... */
#ifdef HAS_SUBPROCESSES
   int pid;			       /* pid of child (if any) */
#endif
} SL_File_Table_Type;

#define SL_MAX_FILES 30
extern SL_File_Table_Type SL_File_Table[SL_MAX_FILES];

#define SL_READ		0x01
#define SL_WRITE	0x02
#define SL_BINARY	0x04
#define SL_SOCKET	0x08
#define SL_PROCESS	0x10

#ifdef HAS_SUBPROCESSES
extern int SLcreate_child_process (char *);
#endif

/* extern SL_File_Table_Type *get_file_table_entry(void); */




