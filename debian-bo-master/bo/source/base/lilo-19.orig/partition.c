/* partition.c  -  Partition table handling */

/* Copyright 1992-1996 Werner Almesberger. See file COPYING for details. */


#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <limits.h>
#include <errno.h>

#include <linux/fs.h>

#include "config.h"
#include "lilo.h"
#include "common.h"
#include "cfg.h"
#include "device.h"
#include "geometry.h"


/* For older kernels ... */
 
#ifndef DOS_EXTENDED_PARTITION
#define DOS_EXTENDED_PARTITION EXTENDED_PARTITION
#endif

#ifndef LINUX_EXTENDED_PARTITION
#define LINUX_EXTENDED_PARTITION EXTENDED_PARTITION
#endif


void part_verify(int dev_nr,int type)
{
    GEOMETRY geo;
    DEVICE dev;
    char backup_file[PATH_MAX+1];
    int fd,bck_file,part,size,lin_3d,cyl;
    struct partition part_table[PART_MAX];

    if (!(dev_nr & PART_MASK) || (dev_nr & PART_MASK) > PART_MAX ||
      (MAJOR(dev_nr) != MAJOR_HD && MAJOR(dev_nr) != MAJOR_XT && MAJOR(dev_nr)
      != MAJOR_SD) && MAJOR(dev_nr) != MAJOR_IDE2) return;
    geo_get(&geo,dev_nr & ~PART_MASK,-1,1);
    fd = dev_open(&dev,dev_nr & ~PART_MASK,cfg_get_flag(cf_options,"fix-table")
      && !test ? O_RDWR : O_RDONLY);
    part = (dev_nr & PART_MASK)-1;
    if (lseek(fd,PART_TABLE_OFFSET,0) < 0) pdie("lseek partition table");
    if (!(size = read(fd,(char *) part_table,sizeof(struct partition)*
      PART_MAX))) die("Short read on partition table");
    if (size < 0) pdie("read partition table");
    if (type && part_table[part].sys_ind != PART_LINUX_MINIX &&
      part_table[part].sys_ind != PART_LINUX_NATIVE &&
      part_table[part].sys_ind != DOS_EXTENDED_PARTITION &&
      part_table[part].sys_ind != LINUX_EXTENDED_PARTITION) {
	fflush(stdout);
	fprintf(stderr,"Device 0x%04X: Partition type 0x%02X does not seem "
	  "suitable for a LILO boot sector\n",dev_nr,part_table[part].sys_ind);
	if (!cfg_get_flag(cf_options,"ignore-table")) exit(1);
    }
    cyl = part_table[part].cyl+((part_table[part].sector >> 6) << 8);
    lin_3d = (part_table[part].sector & 63)-1+(part_table[part].head+
      cyl*geo.heads)*geo.sectors;
    if ((lin_3d > part_table[part].start_sect || (lin_3d <
      part_table[part].start_sect && cyl != BIOS_MAX_CYLS-1)) && !nowarn) {
	fflush(stdout);
	fprintf(stderr,"Device 0x%04X: Invalid partition table, %d%s entry\n",
	  dev_nr & ~PART_MASK,part+1,!part ? "st" : part == 1 ? "nd" : part ==
	  2 ? "rd" : "th");
	fprintf(stderr,"  3D address:     %d/%d/%d (%d)\n",part_table[part].
	  sector & 63,part_table[part].head,cyl,lin_3d);
	cyl = part_table[part].start_sect/geo.sectors/geo.heads;
	fprintf(stderr,"  Linear address: %d/%d/%d (%d)\n",part_table[part].
	  sector = (part_table[part].start_sect % geo.sectors)+1,part_table
	  [part].head = (part_table[part].start_sect/geo.sectors) %
	  geo.heads,cyl,part_table[part].start_sect);
	part_table[part].sector |= cyl >> 8;
	part_table[part].cyl = cyl & 0xff;
	if (!cfg_get_flag(cf_options,"fix-table") && !cfg_get_flag(cf_options,
	  "ignore-table")) exit(1);
	if (test || cfg_get_flag(cf_options,"ignore-table"))
	    fprintf(stderr,"The partition table is *NOT* being adjusted.\n");
	else {
	    sprintf(backup_file,BACKUP_DIR "/part.%04X",dev_nr & ~PART_MASK);
	    if ((bck_file = creat(backup_file,0644)) < 0)
		die("creat %s: %s",backup_file,strerror(errno));
	    if (!(size = write(bck_file,(char *) part_table,
	      sizeof(struct partition)*PART_MAX)))
		die("Short write on %s",backup_file);
	    if (size < 0) pdie(backup_file);
	    if (close(bck_file) < 0)
		die("close %s: %s",backup_file,strerror(errno));
	    if (verbose > 0)
		printf("Backup copy of partition table in %s\n",backup_file);
	    printf("Writing modified partition table to device 0x%04X\n",
	      dev_nr & ~PART_MASK);
	    if (lseek(fd,PART_TABLE_OFFSET,0) < 0)
		pdie("lseek partition table");
	    if (!(size = write(fd,(char *) part_table,sizeof(struct partition)*
	      PART_MAX))) die("Short write on partition table");
	    if (size < 0) pdie("write partition table");
	}
    }
    dev_close(&dev);
}
