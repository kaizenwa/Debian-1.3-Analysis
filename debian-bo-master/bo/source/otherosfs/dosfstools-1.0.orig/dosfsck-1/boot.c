/* boot.c  -  Read and analyze ia PC/MS-DOS boot sector */

/* Written 1993 by Werner Almesberger */


#include <stdio.h>
#include <linux/msdos_fs.h>

#include "common.h"
#include "dosfsck.h"
#include "io.h"
#include "boot.h"


#define ROUND_TO_MULTIPLE(n,m) ((n) && (m) ? (n)+(m)-1-((n)-1)%(m) : 0)
    /* don't divide by zero */


static void dump_boot(DOS_FS *fs,struct msdos_boot_sector *b)
{
    printf("Boot sector contents:\n");
    printf("%10d bytes per logical sector\n",CF_LE_W(*(unsigned short *)
      &b->sector_size));
    printf("%10d bytes per cluster\n",fs->cluster_size);
    printf("%10d reserved sectors\n",CF_LE_W(b->reserved));
    printf("First FAT starts at byte %u\n",fs->fat_start);
    printf("%10d FATs\n",b->fats);
    printf("%10d bytes per FAT\n",fs->fat_size);
    printf("Root directory starts at byte %u\n",fs->root_start);
    printf("%10d root directory entries\n",fs->root_entries);
    printf("Data area starts at byte %u\n",fs->data_start);
    printf("%10u data clusters (%u bytes)\n",fs->clusters,fs->clusters*
      fs->cluster_size);
}


void read_boot(DOS_FS *fs)
{
    struct msdos_boot_sector b;
    int logical_sector_size,fat_entries,data_size;

    fs_read(0,sizeof(b),&b);
    logical_sector_size = CF_LE_W(*(unsigned short *) &b.sector_size);
    fs->cluster_size = b.cluster_size*logical_sector_size;
    if (b.fats != 2)
	die("Currently, only 2 FATs are supported, not %d.\n",b.fats);
    fs->fat_start = CF_LE_W(b.reserved)*logical_sector_size;
    fs->fat_size = CF_LE_W(b.fat_length)*logical_sector_size;
    fs->root_start = (CF_LE_W(b.reserved)+b.fats*CF_LE_W(b.fat_length))*
      logical_sector_size;
    fs->root_entries = CF_LE_W(*((unsigned short *) &b.dir_entries));
    fs->data_start = fs->root_start+ROUND_TO_MULTIPLE(fs->root_entries <<
      MSDOS_DIR_BITS,logical_sector_size);
    data_size = (CF_LE_W(*((unsigned short *) &b.sectors)) ? CF_LE_W(
      *((unsigned short *) &b.sectors)) : CF_LE_L(b.total_sect))*
      logical_sector_size-fs->data_start;
    if (!fs->cluster_size) die("Cluster size is zero.");
    if (!logical_sector_size) die("Logical sector size is zero.");
    fs->clusters = data_size/fs->cluster_size;
    fat_entries = (fs->clusters > MSDOS_FAT12 ? fs->fat_size/2 :
      fs->fat_size*2/3)-2;
    if (fs->clusters > fat_entries)
	die("File system has %d clusters but only space for %d FAT entries.",
	  fs->clusters,fat_entries);
    if (!fs->root_entries) die("Root directory has zero size.");
    if (fs->root_entries & (MSDOS_DPS-1))
	die("Root directory (%d entries) doesn't span an integral number of "
	  "sectors.",fs->root_entries);
    if (logical_sector_size & (SECTOR_SIZE-1))
	die("Logical sector size (%d bytes) is not a multiple of the physical "
	  "sector size.",logical_sector_size);
    if (!b.secs_track || !b.heads)
	die("Invalid disk format in boot sector.");
    if (verbose) dump_boot(fs,&b);
}
