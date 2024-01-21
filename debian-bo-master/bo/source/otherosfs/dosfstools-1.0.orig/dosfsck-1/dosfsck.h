/* dosfsck.h  -  Common data structures and global variables */

/* Written 1993 by Werner Almesberger */


#ifndef _DOSFSCK_H
#define _DOSFSCK_H

#include <linux/msdos_fs.h>


typedef struct msdos_dir_entry DIR_ENT;

typedef struct _dos_file {
    DIR_ENT dir_ent;
    int offset;
    struct _dos_file *parent; /* parent directory */
    struct _dos_file *next; /* next entry */
    struct _dos_file *first; /* first entry (directory only) */
} DOS_FILE;

typedef struct {
    unsigned short value;
    DOS_FILE *owner;
    int prev; /* number of previous clusters */
} FAT_ENTRY;

typedef struct {
    unsigned int fat_start,fat_size; /* unit is bytes */
    unsigned int root_start,root_entries;
    unsigned int data_start;
    int cluster_size,clusters;
    FAT_ENTRY *fat;
} DOS_FS;


extern int interactive,list,verbose,test,write_immed;
extern void *mem_queue;

#endif
