/* boot.c  -  Boot image composition */

/* Copyright 1992-1996 Werner Almesberger. See file COPYING for details. */


#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <a.out.h>
#include <sys/stat.h>

#include <linux/config.h>

#include "config.h"
#include "common.h"
#include "lilo.h"
#include "geometry.h"
#include "cfg.h"
#include "map.h"
#include "partition.h"
#include "boot.h"


static GEOMETRY geo;
static struct stat st;


static void check_size(char *name,int setup_secs,int sectors)
{
    if (sectors > setup_secs+MAX_KERNEL_SECS)
	die("Kernel %s is too big",name);
}


void boot_image(char *spec,IMAGE_DESCR *descr)
{
    BOOT_SECTOR buff;
    SETUP_HDR hdr;
    char *initrd;
    int setup,fd,sectors;
    int modern_kernel;

    if (verbose > 0) printf("Boot image: %s\n",spec);
    fd = geo_open(&geo,spec,O_RDONLY);
    if (fstat(fd,&st) < 0) die("fstat %s: %s",spec,strerror(errno));
    if (read(fd,(char *) &buff,SECTOR_SIZE) != SECTOR_SIZE)
	die("read %s: %s",spec,strerror(errno));
#ifdef LCF_VARSETUP
    setup = buff.sector[VSS_NUM] ? buff.sector[VSS_NUM] : SETUPSECS;
#else
    setup = SETUPSECS;
#endif
    if (read(fd,(char *) &hdr,sizeof(hdr)) != sizeof(hdr))
	die("read %s: %s",spec,strerror(errno));
    modern_kernel = !strncmp(hdr.signature,NEW_HDR_SIG,4) && hdr.version >=
      NEW_HDR_VERSION;
    if (modern_kernel) descr->flags |= FLAG_MODKRN;
    if (verbose > 1)
	printf("Setup length is %d sector%s.\n",setup,setup == 1 ? "" : "s");
    map_add(&geo,0,(st.st_size+SECTOR_SIZE-1)/SECTOR_SIZE);
    sectors = map_end_section(&descr->start,setup+SPECIAL_SECTORS);
    if (!modern_kernel || !(hdr.flags & LFLAG_HIGH))
	check_size(spec,setup,sectors);
    else {
	if (hdr.start % PAGE_SIZE)
	    die("Can't load kernel at mis-aligned address 0x%08lx\n",hdr.start);
	descr->start_page = hdr.start/PAGE_SIZE; /* load kernel high */
    }
    geo_close(&geo);
    if (verbose > 1)
	printf("Mapped %d sector%s.\n",sectors,sectors == 1 ? "" : "s");
    if ((initrd = cfg_get_strg(cf_kernel,"initrd")) || (initrd = cfg_get_strg(
      cf_options,"initrd"))) {
	if (!modern_kernel) die("Kernel doesn't support initial RAM disks");
	if (verbose > 0) printf("Mapping RAM disk %s\n",initrd);
	fd = geo_open(&geo,initrd,O_RDONLY);
	if (fstat(fd,&st) < 0) die("fstat %s: %s",initrd,strerror(errno));
	*(unsigned long *) descr->rd_size = st.st_size;
	map_begin_section();
	map_add(&geo,0,(st.st_size+SECTOR_SIZE-1)/SECTOR_SIZE);
	sectors = map_end_section(&descr->initrd,0);
	if (verbose > 1)
	    printf("RAM disk: %d sector%s.\n",sectors,sectors == 1 ?  "" :
	      "s");
	geo_close(&geo);
    }
}


void boot_device(char *spec,char *range,IMAGE_DESCR *descr)
{
    char *here;
    int start,secs;
    int sectors;

    if (verbose > 0) printf("Boot device: %s, range %s\n",spec,range);
    (void) geo_open(&geo,spec,O_NOACCESS);
    if (here = strchr(range,'-')) {
	*here++ = 0;
	start = to_number(range);
	if ((secs = to_number(here)-start+1) < 0) die("Invalid range");
    }
    else {
	if (here = strchr(range,'+')) {
	    *here++ = 0;
	    start = to_number(range);
	    secs = to_number(here);
	}
	else {
	    start = to_number(range);
	    secs = 1;
	}
    }
    map_add(&geo,start,secs);
    check_size(spec,SETUPSECS,sectors = map_end_section(&descr->start,60));
				/* this is a crude hack ... ----------^^*/
    geo_close(&geo);
    if (verbose > 1)
	printf("Mapped %d sector%s.\n",sectors,sectors == 1 ? "" : "s");
}


void boot_other(char *loader,char *boot,char *part,IMAGE_DESCR *descr)
{
    int b_fd,l_fd,p_fd,walk,found,size,no_loader;
    unsigned short magic;
    BOOT_SECTOR buff;
    struct stat st;

    if (no_loader = !loader) loader = DFL_CHAIN;
    if (verbose > 0)
	printf("Boot other: %s%s%s, loader %s\n",boot,part ? ", on " : "",part
	  ? part : "",loader);
    if (cfg_get_flag(cf_other,"unsafe")) {
	(void) geo_open_boot(&geo,boot);
	if (part) die("TABLE and UNSAFE are mutually incompatible.");
    }
    else {
	b_fd = geo_open(&geo,boot,O_RDONLY);
	if (fstat(b_fd,&st) < 0)
	    die("fstat %s: %s",boot,strerror(errno));
	if (!geo.file) part_verify(st.st_rdev,0);
	if (lseek(b_fd,(long) BOOT_SIG_OFFSET,0) < 0)
	    die("lseek %s: %s",boot,strerror(errno));
	if ((size = read(b_fd,(char *) &magic,2)) != 2)
	    if (size < 0) die("read %s: %s",boot,strerror(errno));
	    else die("Can't get magic number of %s",boot);
	if (magic != BOOT_SIGNATURE)
	    die("First sector of %s doesn't have a valid boot signature",boot);
    }
    if ((geo.device & 0x7f) && no_loader)
	die("Must specify LOADER for BIOS device 0x%02X",geo.device);
    if (!part) {
	memset(&buff,0,SECTOR_SIZE);
	p_fd = -1; /* pacify GCC */
	if (cfg_get_flag(cf_other,"rewrite-table"))
	    die("REWRITE-TABLE requires TABLE to be set");
    }
    else {
	if ((p_fd = open(part,O_RDONLY)) < 0)
	    die("open %s: %s",part,strerror(errno));
	if (read(p_fd,(char *) &buff,SECTOR_SIZE) != SECTOR_SIZE)
	    die("read %s: %s",part,strerror(errno));
	if (cfg_get_flag(cf_other,"rewrite-table")) {
#ifndef LCF_REWRITE_TABLE
	    die("REWRITE-TABLE requires LILO to be compiled with the "
	      "REWRITE_TABLE option");
#else
	    int partition;

	    if (!boot_dev_nr || (boot_dev_nr & PART_MASK))
		die("REWRITE-TABLE requires LILO to be installed on the MBR");
	    partition = st.st_rdev & PART_MASK;
	    if (geo.file || !partition || partition > PART_MAX)
		die("REWRITE-TABLE requires the boot sector to be loaded from "
		  "a primary partition");
	    if (stat(part,&st) < 0) die("stat %s: %s",part,strerror(errno));
	    if (!S_ISBLK(st.st_mode) || (st.st_rdev & PART_MASK))
		die("REWRITE-TABLE requires TABLE to specify a partition "
		  "table");
	    die("Sorry, this version of LILO doesn't support REWRITE-TABLE");
#endif
	}
    }
    if ((l_fd = open(loader,O_RDONLY)) < 0)
	die("open %s: %s",loader,strerror(errno));
    if ((size = read(l_fd,(char *) &buff,PART_TABLE_OFFSET+1)) < 0)
	die("read %s: %s",loader,strerror(errno));
    check_version(&buff,STAGE_CHAIN);
    if (size > PART_TABLE_OFFSET)
	die("Chain loader %s is too big",loader);
    if (part) {
	found = 0;
	for (walk = 0; walk < PARTITION_ENTRIES; walk++)
	    if (!PART(buff,walk).sys_ind || PART(buff,walk).start_sect !=
	      geo.start) {
		if (PART(buff,walk).sys_ind != PART_DOS12 && PART(buff,walk).
		  sys_ind != PART_DOS16 && PART(buff,walk).sys_ind != PART_DOS32)
		  PART(buff,walk).sys_ind = PART_INVALID;
	    }
	    else {
		if (found) die("Duplicate entry in partition table");
		buff.par_c.offset = walk*PARTITION_ENTRY;
		PART(buff,walk).boot_ind = 0x80;
		found = 1;
	    }
	if (!found) die("Partition entry not found.");
	(void) close(p_fd);
    }
    (void) close(l_fd);
    buff.par_c.drive = geo.device;
    map_add_zero();
    map_add_sector(&buff);
    map_add(&geo,0,1);
    (void) map_end_section(&descr->start,2+SPECIAL_SECTORS); /* size is known */
    geo_close(&geo);
    if (verbose > 1) printf("Mapped 4 (2+1+1) sectors.\n");
}


void dump(char *spec,IMAGE_DESCR *descr)
{
    int fd,size,sectors;

    if (verbose > 0) printf("Dump file: %s\n",spec);
    fd = geo_open(&geo,spec,O_RDONLY);
    if (fstat(fd,&st) < 0) die("fstat %s: %s",spec,strerror(errno));
    size = st.st_size/SECTOR_SIZE;
    map_add_zero(); /* dummy boot sector */
    map_add(&geo,0,size);
    sectors = map_end_section(&descr->start,size+1);
    geo_close(&geo);
    if (verbose > 1)
	printf("Mapped %d sector%s.\n",sectors,sectors == 1 ? "" : "s");
}
