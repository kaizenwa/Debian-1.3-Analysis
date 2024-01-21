/* fat.c  -  Read/write access to the FAT */

/* Written 1993 by Werner Almesberger */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <linux/msdos_fs.h>

#include "common.h"
#include "dosfsck.h"
#include "io.h"
#include "fat.h"


static void get_fat(FAT_ENTRY *entry,void *fat,int cluster,int fat16)
{
    unsigned char *ptr;

    if (fat16) entry->value = CF_LE_W(((unsigned short *) fat)[cluster]);
    else {
	ptr = &((unsigned char *) fat)[cluster*3/2];
	entry->value = 0xfff & (cluster & 1 ? (ptr[0] >> 4) | (ptr[1] << 4) :
	  (ptr[0] | ptr[1] << 8));
    }
    entry->owner = NULL;
}


void read_fat(DOS_FS *fs)
{
    int fat16,eff_size,i;
    void *first,*second,*use;
    int first_ok,second_ok;

    fat16 = fs->clusters > MSDOS_FAT12;
    eff_size = fat16 ? (fs->clusters+2)*2 : ((fs->clusters+2)*3+1)/2;
    first = alloc(eff_size);
    second = alloc(eff_size);
    fs_read(fs->fat_start,eff_size,first);
    fs_read(fs->fat_start+fs->fat_size,eff_size,second);
    if (!memcmp(first,second,eff_size)) use = first;
    else {
	first_ok = CF_LE_W(*(unsigned short *) first) == (fat16 ? 0xfff8 :
	  0xff8);
	second_ok = CF_LE_W(*(unsigned short *) second) == (fat16 ? 0xfff8 :
	  0xff8);
	if (first_ok && !second_ok) {
	    printf("FATs differ - using first FAT.\n");
	    fs_write(fs->fat_start+fs->fat_size,eff_size,use = first);
	}
	if (!first_ok && second_ok) {
	    printf("FATs differ - using second FAT.\n");
	    fs_write(fs->fat_start,eff_size,use = second);
	}
	if (first_ok && second_ok)
	    if (interactive) {
		printf("FATs differ but appear to be intact. Use which FAT ?\n"
		  "1) Use first FAT\n2) Use second FAT\n");
		if (get_key("12","?") == '1')
		    fs_write(fs->fat_start+fs->fat_size,eff_size,use = first);
		else fs_write(fs->fat_start,eff_size,use = second);
	    }
	    else {
		printf("FATs differ but appear to be intact. Using first "
		  "FAT.\n");
		fs_write(fs->fat_start+fs->fat_size,eff_size,use = first);
	    }
	if (!first_ok && !second_ok) {
	    printf("Both FATs appear to be corrupt. Giving up.\n");
	    exit(1);
	}
    }
    fs->fat = qalloc(&mem_queue,sizeof(FAT_ENTRY)*(fs->clusters+2));
    for (i = 2; i < fs->clusters+2; i++) get_fat(&fs->fat[i],use,i,fat16);
    for (i = 2; i < fs->clusters+2; i++)
	if (fs->fat[i].value >= fs->clusters+2 && (fs->fat[i].value < (fat16 ?
	  0xfff7 : 0xff7))) {
	    printf("Cluster %d out of range (%d > %d). Setting to EOF.\n",i-2,
	      fs->fat[i].value,fs->clusters+2-1);
	    set_fat(fs,i,-1);
	}
    free(first);
    free(second);
}


void set_fat(DOS_FS *fs,int cluster,int new)
{
    unsigned char data[2];
    int fat16,offs;

    fat16 = fs->clusters > MSDOS_FAT12;
    if (new < 0) new += fat16 ? 0xfff9 : 0xff9;
    fs->fat[cluster].value = new;
    if (fat16) {
	offs = fs->fat_start+cluster*2;
	*(unsigned short *) data = CT_LE_W(new);
    }
    else {
	offs = fs->fat_start+cluster*3/2;
	if (cluster & 1) {
	    data[0] = ((new & 0xf) << 4) | (fs->fat[cluster-1].value >> 8);
	    data[1] = new >> 4;
	}
	else {
	    data[0] = new & 0xff;
	    data[1] = (new >> 8) | (cluster == fs->clusters-1 ? 0 :
	      (0xff & fs->fat[cluster+1].value) << 4);
	}
    }
    fs_write(offs,2,&data);
    fs_write(offs+fs->fat_size,2,&data);
}


int bad_cluster(DOS_FS *fs,int cluster)
{
    return fs->fat[cluster].value == (fs->clusters > MSDOS_FAT12 ? 0xfff7 :
      0xff7);
}


int next_cluster(DOS_FS *fs,int cluster)
{
    unsigned short value,limit;

    value = fs->fat[cluster].value;
    limit = fs->clusters > MSDOS_FAT12 ? 0xfff7 : 0xff7;
    if (value == limit)
	die("Internal error: next_cluster on bad cluster");
    return value < limit ? value : -1;
}


unsigned int cluster_start(DOS_FS *fs,int cluster)
{
    return fs->data_start+(cluster-2)*fs->cluster_size;
}


void set_owner(DOS_FS *fs,int cluster,DOS_FILE *owner)
{
    if (owner && fs->fat[cluster].owner)
	die("Internal error: attempt to change file owner");
    fs->fat[cluster].owner = owner;
}


DOS_FILE *get_owner(DOS_FS *fs,int cluster)
{
    return fs->fat[cluster].owner;
}


void fix_bad(DOS_FS *fs)
{
    int limit,i;

    limit = fs->clusters > MSDOS_FAT12 ? 0xfff7 : 0xff7;
    for (i = 2; i < fs->clusters+2; i++)
	if (!get_owner(fs,i) && fs->fat[i].value != limit)
	    if (!fs_test(cluster_start(fs,i),fs->cluster_size)) {
		printf("Cluster %d is unreadable.\n",i);
		set_fat(fs,i,-2);
	    }
}


void reclaim_free(DOS_FS *fs)
{
    int i,reclaimed,limit;

    reclaimed = 0;
    limit = fs->clusters > MSDOS_FAT12 ? 0xfff7 : 0xff7;
    for (i = 2; i < fs->clusters+2; i++)
	if (!get_owner(fs,i) && fs->fat[i].value && fs->fat[i].value != limit) {
	    set_fat(fs,i,0);
	    reclaimed++;
	}
    if (reclaimed)
	printf("Reclaimed %d unused cluster%s (%d bytes).\n",reclaimed,
	  reclaimed == 1 ?  "" : "s",reclaimed*fs->cluster_size);
}


static void tag_free(DOS_FS *fs,DOS_FILE *ptr)
{
    DOS_FILE *owner;
    int i,walk,prev;

    for (i = 2; i < fs->clusters+2; i++)
	if (fs->fat[i].value && fs->fat[i].value != (fs->clusters > MSDOS_FAT12
	  ? 0xfff7 : 0xff7) && !get_owner(fs,i) && !fs->fat[i].prev) {
	    prev = 0;
	    for (walk = i; walk > 0; walk = next_cluster(fs,walk)) {
		if (!(owner = get_owner(fs,walk))) set_owner(fs,walk,ptr);
		else if (owner != ptr)
		        die("Internal error: free chain collides with file");
		    else {
			set_fat(fs,prev,-1);
			break;
		    }
		prev = walk;
	    }
	}
}


void reclaim_file(DOS_FS *fs)
{
    DIR_ENT *root;
    DOS_FILE dummy;
    int i,reclaimed,files,next,changed,scan,walk;
    int curr_num,next_free,limit;

    root = alloc(fs->root_entries*sizeof(DIR_ENT));
    fs_read(fs->root_start,fs->root_entries*sizeof(DIR_ENT),root);
    limit = fs->clusters > MSDOS_FAT12 ? 0xfff7 : 0xff7;
    for (i = 2; i < fs->clusters+2; i++) fs->fat[i].prev = 0;
    for (i = 2; i < fs->clusters+2; i++) {
	next = fs->fat[i].value;
	if (!get_owner(fs,i) && next && next < fs->clusters+2)
	    if (get_owner(fs,next) || !fs->fat[next].value || fs->fat[next].
	      value == limit) set_fat(fs,i,-1);
	    else fs->fat[next].prev++;
    }
    do {
	tag_free(fs,&dummy);
	changed = 0;
	for (i = 2; i < fs->clusters+2; i++)
	    if (fs->fat[i].value && fs->fat[i].value != limit && !get_owner(fs,
	      i)) {
		if (!fs->fat[fs->fat[i].value].prev--)
		    die("Internal error: prev going below zero");
		set_fat(fs,i,-1);
		changed = 1;
		printf("Broke cycle at cluster %d in free chain.\n",i);
		break;
	    }
    }
    while (changed);
    files = reclaimed = curr_num = next_free = 0;
    for (i = 2; i < fs->clusters+2; i++)
	if (get_owner(fs,i) == &dummy && !fs->fat[i].prev) {
	    files++;
	    while (next_free < fs->root_entries)
		if (IS_FREE(root[next_free].name)) break;
		else next_free++;
	    if (next_free == fs->root_entries)
		die("Root directory is full.");
	    memset(&root[next_free],0,sizeof(DIR_ENT));
	    while (1) {
		sprintf(root[next_free].name,"FSCK%04dREC",curr_num);
		for (scan = 0; scan < fs->root_entries; scan++)
		    if (scan != next_free && !strncmp(root[scan].name,
		      root[next_free].name,MSDOS_NAME)) break;
		if (scan == fs->root_entries) break;
		if (++curr_num == 10000) die("Unable to create unique name");
	    }
	    root[next_free].start = CT_LE_W(i);
	    for (walk = i; walk > 0; walk = next_cluster(fs,walk)) {
		root[next_free].size = CT_LE_L(CF_LE_L(root[next_free].size)+
		  fs->cluster_size);
		reclaimed++;
	    }
	    fs_write(fs->root_start+next_free*sizeof(DIR_ENT),sizeof(DIR_ENT),
	      &root[next_free]);
	}
    free(root);
    if (reclaimed)
	printf("Reclaimed %d unused cluster%s (%d bytes) in %d chain%s.\n",
	  reclaimed,reclaimed == 1 ? "" : "s",reclaimed*fs->cluster_size,files,
	  files == 1 ? "" : "s");
}
