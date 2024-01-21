/*
 * Author:	George Carrette, <GJC@MITECH.COM>
 *
 * @(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/vms_comp.h,v 3.0 1996/05/06 16:12:52 william Exp $
 */

/* Header file for vms_comp routines, generally useful functions
   to aid in porting/running Unix and BSD code under VAX/VMS.

   **  Written by George Carrette, <GJC@MITECH.COM>.
   **  Includes modified VMS readdir() routines.
   **  Written by Rich $alz, <rsalz@bbn.com> in August, 1990.

To use:

#include "vms_comp.h"

*/

#ifndef DESCRIP_H_DEFINED
#include <descrip.h>
#endif

/* 12-NOV-1990 added d_namlen field -GJC@MITECH.COM */

    /* Data structure returned by READDIR(). */
struct dirent {
    char	d_name[100];		/* File name		*/
    int         d_namlen;
    int		vms_verscount;		/* Number of versions	*/
    int		vms_versions[20];	/* Version numbers	*/
};

    /* Handle returned by opendir(), used by the other routines.  You
     * are not supposed to care what's inside this structure. */
typedef struct _dirdesc {
    long			context;
    int				vms_wantversions;
    char			*pattern;
    struct dirent		entry;
    struct dsc$descriptor_s	pat;
} DIR;

/* Another name for this? Used by TGIF */

struct direct {
    char	d_name[100];		/* File name		*/
    int         d_namlen;
    int		vms_verscount;		/* Number of versions	*/
    int		vms_versions[20];	/* Version numbers	*/
};


#define rewinddir(dirp)		seekdir((dirp), 0L)

extern DIR		*opendir();
extern struct dirent	*readdir();
extern long		telldir();
extern void		seekdir();
extern void		closedir();
extern void		vmsreaddirversions();
