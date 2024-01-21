/*
 * ext2.c - extfs-specific functions and data for the Linux file system 
 * degragmenter. 
 *
 * This file is responsible for managing those data structures dependent
 * on the extfs.  Specifically, it
 * + allocates space for the various disk maps stored in memory; 
 * + reads and writes superblock information; 
 * + reads the used-data-zones and used-inodes maps into memory
 *   (inode_map and d2n/n2d_map respectively); and, 
 * + once the defragmentation is complete, rewrites the new free-space
 *   map to disk.
 *
 * Copyleft  (C) 1993 Alexey Vovenko (vovenko@ixwin.ihep.su)
 * Copyright (C) 1992, 1993 Stephen Tweedie (sct@dcs.ed.ac.uk)
 * Copyright (C) 1992 Remy Card (card@masi.ibp.fr)
 * Copyright (C) 1991 Linus Torvalds (torvalds@kruuna.helsinki.fi)
 * 
 * This file may be redistributed under the terms of the GNU General
 * Public License.
 *
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include "defrag.h"
#include "display.h"

char * program_name = "e2defrag";
char * fsck = "e2fsck";

int groups;
struct ext2_group_desc *bg;
struct groups_population *gp;

/* Joined free blocks bitmap and manipulation routines */
static char *zone_map = NULL;

#define bm_zone_in_use(x) (bit(zone_map,(x)-FIRSTZONE)) 
#define bm_mark_zone(x) (setbit(zone_map,(x)-FIRSTZONE),changed=1)
#define bm_unmark_zone(x) (clrbit(zone_map,(x)-FIRSTZONE),changed=1) 


static void read_groups(void) {
   char s[132];
   bg = malloc(sizeof(struct ext2_group_desc)*groups);
   gp = malloc(sizeof(struct groups_population)*groups);
                
   if ((bg == NULL) || (gp==NULL))
      die("Out of memory allocating group descriptors");
   
   memset(gp,0,sizeof(struct groups_population)*groups);
   
   if (nlseek(IN, BLOCK_SIZE*2, SEEK_SET) < 0)
      die("Can't seek to the group descriptors");
   if (nread(IN,bg,sizeof(struct ext2_group_desc) * groups)!=
       sizeof(struct ext2_group_desc) * groups) 
      die("Can't read group descriptors\n");
         
           /* die here, if any bitmap alignment problems exist */
           /* It's quite improbable, that bitmaps are not byte aligned */
   if ((Super.s_blocks_per_group % 8) != 0) {
      sprintf(s,"Strange blocks_per_group (%lu) value",
              Super.s_blocks_per_group);
      fatal_error(s);
   }         
   if ((Super.s_inodes_per_group % 8) != 0) {
      sprintf(s,"Strange inodes_per_group (%lu) value",
              Super.s_blocks_per_group);
      fatal_error(s);
   }
}

/* Read the superblock and group description tables and reserve space 
 * for the remaining disk map tables (inodes, inodes & block bitmaps) */
void read_tables (void)
{
    if (debug)
	printf ("DEBUG: read_tables()\n");
    if (BLOCK_SIZE != nlseek (IN, BLOCK_SIZE, SEEK_SET))
	die ("seek failed");
    if (BLOCK_SIZE != nread (IN, super_block_buffer, BLOCK_SIZE))
	die ("unable to read super-block");
    if (MAGIC != EXT2_SUPER_MAGIC)
	die ("bad magic number in super-block");
    if (ZONESIZE != 0 || BLOCK_SIZE != 1024)
	die("Only 1k blocks/zones supported");
    if (Super.s_state & EXT2_VALID_FS == 0)
	die("Please run fsck first, filesystem is not marked valid\n");
    groups = (Super.s_blocks_count - Super.s_first_data_block + 
	      Super.s_blocks_per_group - 1) /
	Super.s_blocks_per_group;

    read_groups();

    inode_average_map = malloc (INODES * sizeof(*inode_average_map));
    if (!inode_average_map)
	die ("Unable to allocate buffer for inode averages");
    memset (inode_average_map, 0, (INODES * sizeof(*inode_average_map)));

    inode_priority_map = malloc (INODES * sizeof(*inode_priority_map));
    if (!inode_priority_map)
	die ("Unable to allocate buffer for inode priorities");
    memset (inode_priority_map, 0, 
	    (INODES * sizeof(*inode_priority_map)));

    inode_order_map = malloc (INODES * sizeof(*inode_order_map));
    if (!inode_order_map)
	die ("Unable to allocate buffer for inode order");
    memset (inode_order_map, 0, (INODES * sizeof(*inode_order_map)));

    d2n_map = malloc ((ZONES - FIRSTZONE) * sizeof (*d2n_map));
    if (!d2n_map)
	die ("Unable to allocate zones map\n");
    n2d_map = malloc ((ZONES - FIRSTZONE) * sizeof (*n2d_map));
    if (!n2d_map)
	die ("Unable to allocate zones map\n");

    fixed_map = malloc (((ZONES - FIRSTZONE) / 8) + 1);
    if (!fixed_map)
	die ("Unable to allocate unmoveable zones bitmap\n");
    memset(fixed_map, 0, ((ZONES - FIRSTZONE) / 8) + 1);

    if (NORM_FIRSTZONE != FIRSTZONE)
	/* It is not clear for me what the first zone is designed for.
	 * Some implications about it being equal to 1 are splattered
	 * accross the source code, so let's be cautious.
	 * There is no possibility to set any non-standard value in
	 * current mke2fs, so this shouldn't be a problem
	 */
	die("Warning: Firstzone != Norm_firstzone\n");
    first_zone = FIRSTZONE;
    zones = ZONES;
    block_size = BLOCK_SIZE;
    bad_block_inode = BAD_INO;
}

void show_super_stats(void) {
    char s[256];
    if (voyer_mode)
    {
	sprintf (s, "%6ld block%s, %6ld free (%ld%%)", 
		 ZONES,  (ZONES  != 1) ? "s" : "",FREEBLOCKSCOUNT,
		 (100*FREEBLOCKSCOUNT) / ZONES);
	add_comment(s);           
	sprintf (s,"%6ld inode%s, %6ld free (%ld%%)", 
		 INODES, (INODES != 1) ? "s" : "",FREEINODESCOUNT,
		 (100*FREEINODESCOUNT) / INODES);
	add_comment(s);         
	sprintf (s,"%d group%s,", 
		 groups, (groups != 1) ? "s" : "");
	add_comment(s);
	display_comments("");           
    }
    else if (show)
    {
	printf ("%ld inode%s\n", INODES, (INODES != 1) ? "s" : "");
	printf ("%ld block%s\n", ZONES, (ZONES != 1) ? "s" : "");
	printf ("%d bad block%s\n", badblocks, (badblocks != 1) ? "s" : "");
	printf ("Firstdatazone=%ld (%d)\n", FIRSTZONE, NORM_FIRSTZONE);
	printf ("%ld free block%s\n", FREEBLOCKSCOUNT,
		(FREEBLOCKSCOUNT != 1) ? "s" : "");
	printf ("%ld free inode%s\n", FREEINODESCOUNT,
		(FREEINODESCOUNT != 1) ? "s" : "");
	printf ("Zonesize=%d\n", BLOCK_SIZE << ZONESIZE);
    }
}

/* Read in the map of used/unused inodes.  
 */
void init_inode_bitmap (void)
{
	int i,size;
        ulong pos;

	if (debug)
		printf ("DEBUG: init_inode_bitmap()\n");
        
        size = Super.s_inodes_per_group >> 3;

	inode_map = malloc (size * groups);
	if (!inode_map)
		die ("Unable to allocate inodes bitmap\n");
	memset (inode_map, 0, (INODES / 8) + 1);

        for (i = 0; i < groups; i++) {
                pos = bg[i].bg_inode_bitmap * block_size;
                if (debug)
                        printf("Group:%d inode_bitmap at:%lu\n",i,pos);
                if (pos!=nlseek(IN,pos,SEEK_SET)) 
                        die("seek failed reading inode bitmap");
           
                if (nread(IN,inode_map+size*i,size) != size) 
                        die("error reading inode bitmap");        
        }  
}

static void mark_fixed_blocks(Block block, int count) {
   int i;
   if (debug)
        printf("Mark unmoveable block:%ld count %d\n",block,count);
 
   for (i=0; i < count; i++) {
        mark_fixed(block+i);
   }
}      

static void mark_group_zones(void) {
int i,count,sb_count;
   sb_count = 1 + /* superblock + blocks occupied by group descriptors */
           UPPER(sizeof(struct ext2_group_desc)*groups, block_size);
                             
   for (i=0; i < groups; i++) { 
                /* Each group contains a copy of superblock and group desc */
      mark_fixed_blocks(i*Super.s_blocks_per_group + 1,sb_count);    
                /* Block bitmap */
      count = UPPER(Super.s_blocks_per_group / 8,block_size);
      mark_fixed_blocks(bg[i].bg_block_bitmap,count);
                /* Inode bitmap */
      count = UPPER(Super.s_inodes_per_group / 8,block_size);
      mark_fixed_blocks(bg[i].bg_inode_bitmap,count);
                /* Inode table */
      count = UPPER((Super.s_inodes_per_group * sizeof(struct d_inode)),block_size);
      mark_fixed_blocks(bg[i].bg_inode_table,count);
   }                                    
}

/* Count free blocks in each group using block allocation bitmaps.
 * We do not use bg_free_blocks_count, I'm not sure they are actually correct.
 * Blocks, reserved for superuser sometimes are not counted in 
 * bg_free_blocks_count. They are represented however in the 
 * s_free_blocks_count. (10-dec-93)
 */ 
/* (14-dec-93). bg_free_blocks sometimes are not correct, but I'm sure, that
 * there was a situation with wrong counts and the new e2fsck told nothing 
 * about it. Let's see if this is true.
 * Yes it does. I've managed to spoil file system: 0 blocks are actually 
 * free, bitmap is ok, but bg_free_blocks says "76". Older versions of 
 * e2fsck are buggy.  (This is fixed in all recent versions of e2fsck.)
 */ 

static void count_free_blocks(void) {
   int n,i;
   ulong total_free = 0;
   ulong blocks;
   
   for (n = 0; n < groups; n++) {
       ulong count = 0;
       if (n != groups - 1)
           blocks = Super.s_blocks_per_group;
       else
           blocks = ((ZONES - FIRSTZONE) % Super.s_blocks_per_group) ? : 
	             Super.s_blocks_per_group;
       for (i = 0; i < blocks; i++)
           if (!bm_zone_in_use(1 + i + n*Super.s_blocks_per_group))
               count++;
       gp[n].free_blocks = count;
       total_free += count;
       if (debug) printf("Group %d, free blocks %lu\n",n,count);        
       if (count!=bg[n].bg_free_blocks_count)
          fprintf(stderr,"Group %d: free_blocks:%u counted:%lu\n",n,
                  bg[n].bg_free_blocks_count,count);
   }
   if (debug) printf("Total free blocks counted: %lu\n",total_free);
   if (FREEBLOCKSCOUNT!=total_free) {
       char s[256];
       sprintf(s,"Free blocks count wrong, free:"
	       "%lu, reserved:%lu, counted:%lu\nRun fsck.\n",
              FREEBLOCKSCOUNT,Super.s_r_blocks_count,total_free);
       fatal_error(s);
   }       
}                                  

/* Read the map of used/unused data zones on disk.  
 * The map is held jointly in d2n_map and n2d_map, described in
 * defrag.h.  These are initialised to the identity map (d2n(i) = n2d(i)
 * = i), and then the free zone list is scanned, and all unused zones
 * are marked as zero in both d2n_map and n2d_map. 
 * Mark blocks, occupied by group bitmaps and inode tables as unmoveable.
 */
   
void init_zone_maps (void)
{
	Block i;
        int n;
        ulong size,pos;
        
        size = Super.s_blocks_per_group >> 3;
                           /* The last group on the disk can be shorter, 
                            * that's why groups * s_blocks_per_group >= s_blocks
                            */
	zone_map = malloc (size*groups);
	if (!zone_map)
		die ("Unable to allocate zone bitmap\n");

        for (n = 0; n < groups; n++) {
                pos = bg[n].bg_block_bitmap * block_size;
                if (pos!=nlseek(IN,pos,SEEK_SET)) 
                        die("seek failed reading zone bitmap");
           
                if (nread(IN,zone_map+size*n,size) != size) 
                        die("error reading zone bitmap");        
        }  

	if (debug)
		printf ("DEBUG: init_zone_maps()\n");
	for (i = FIRSTZONE; i < ZONES; i++)
	{
		if (bm_zone_in_use(i))
		{
			d2n(i) = i;
			n2d(i) = i;
		}
		else
		{
			d2n(i) = 0;
			n2d(i) = 0;
		}
	}
        count_free_blocks();
        mark_group_zones();
}

/* Write the superblock inode bitmaps to disk */
void write_tables (void)
{       
	if (debug)
		printf ("DEBUG: write_tables()\n");
	if (BLOCK_SIZE != nlseek (IN, BLOCK_SIZE, SEEK_SET))
		die ("seek failed in write_tables");
                
	if (BLOCK_SIZE != nwrite (IN, super_block_buffer, BLOCK_SIZE))
		die ("unable to write super-block");
/* Inodes are not actually moved in the current version - nothing to write */                
}

/* Rewrite the free zone map on disk.  The defragmentation procedure
   will migrate all free blocks to the end of the disk partition, and so
   after defragmentation the free space map must be updated to reflect
   this. Free zones are determined by n2d_map, the macro zone_in_use(n)
   is defined in defrag.h for this purpose. The ext2fs stores the free
   zone map as a number of bitmaps, one in each group
 */   
void salvage_free_zones (void) {
   Block blk;
   int n;
   ulong size,pos;
   ulong bmp_zones;     /* Number of zones defined by bitmap size,  */
                        /* can be larger than the actual zone count */
        
   size = Super.s_blocks_per_group >> 3;
   bmp_zones = groups * Super.s_blocks_per_group;   

   if (verbose)
	stat_line ("Salvaging free zones list ...\n");
        
   for (n=0; n < groups; n++)
        bg[n].bg_free_blocks_count = 0;     

   memset(zone_map,0,bmp_zones / 8);
   for (blk = FIRSTZONE; blk < ZONES; blk++) {
        if (zone_in_use(blk) || zone_is_fixed(blk)) 
             bm_mark_zone(blk);
        else 
             /* FIXME - it's may be to slow to have division operations 
              * in the innermost cycle.
              */
             bg[(blk-1) / Super.s_blocks_per_group].bg_free_blocks_count++;
   }
   for (blk=ZONES; blk < bmp_zones+FIRSTZONE; blk++) 
            /* Padding to the end of bitmap */
            bm_mark_zone(blk);

   for (n = 0; n < groups; n++) {
           pos = bg[n].bg_block_bitmap * block_size;
           if (pos!=nlseek(IN,pos,SEEK_SET)) 
                   die("seek failed writing zone bitmap");
      
           if (nwrite(IN,zone_map+size*n,size) != size) 
                   die("error writing zone bitmap");                          
   }

   if (nlseek(IN, BLOCK_SIZE*2, SEEK_SET) < 0)
      die("Can't seek to the group descriptors");
   if (nwrite(IN,bg,sizeof(struct ext2_group_desc) * groups)!=
       sizeof(struct ext2_group_desc) * groups) 
      die("Can't write group descriptors\n");
}

int seek_to_inode(int i) {
        struct ext2_group_desc *p;
        
        i--;
        p = &bg[i / Super.s_inodes_per_group];
   
	if (nlseek(IN,
            p->bg_inode_table*block_size + sizeof(struct d_inode)* 
            (i % Super.s_inodes_per_group), 
            SEEK_SET)<0) {
		io_error ("Can't seek to inode table");
		return 1;
	}
	return 0;
}
 
void update_group_population(ulong znr, enum walk_zone_mode mode, ulong inode) {
   int i_group = (inode-1) / Super.s_inodes_per_group;
   int z_group = (znr-1) / Super.s_blocks_per_group;
   
   assert(i_group < groups);
   assert(z_group < groups);    

   if (mode == WZ_FIXED_BLOCKS) {
       gp[z_group].native_blocks++;     /* Count fixed blocks as native */
       return;                           
   }       
   if (mode != WZ_SCAN)
       return;
   if (i_group == z_group) 
       gp[i_group].native_blocks++;
   else {
       gp[z_group].wants_away++;
       gp[i_group].wants_back++;
   }
}

/* The main problem with groups in is that their population is not
 * even.  The current allocation strategy tries to balance the number
 * of allocated inodes and the total number of blocks in each group.
 * This does not necessarily mean that the number of blocks owned by
 * native inodes are equal in each group. In fact some of the groups
 * are overpopulated and some are not.
 * 
 * When we deal with an overpopulated group, we have to put its blocks
 * into some other groups. So we have to know, how many blocks we can
 * put into not-yet filled group without forcing out the native
 * blocks. 
 */
void check_group_population(void) {
   int i;   
   
   if (verbose > 1) stat_line("Checking groups population...\n");
   for (i=0; i < groups; i++) {  
   
                  /* Print data about the present situation in the groups */
       if (verbose > 2 && !voyer_mode) {          
          printf("Group:%d   native:%lu(+%lu), foreign:%lu, free:%lu\n",i,
                 gp[i].native_blocks,gp[i].wants_back, 
                 gp[i].wants_away, gp[i].free_blocks);
          printf("\t total:%lu\n",gp[i].native_blocks + gp[i].wants_away 
                 + gp[i].free_blocks);
       }
                  /* Now let's estimate, how many free blocks we are going
                   * to have after the defragmentation 
                   */
                
       gp[i].free_blocks += gp[i].wants_away;
       if (gp[i].free_blocks > gp[i].wants_back) 
           gp[i].free_blocks -= gp[i].wants_back;
       else 
           gp[i].free_blocks = 0;
       gp[i].native_blocks = 0;
       gp[i].wants_away = 0;    
           
       gp[i].next_block_to_fill = i*Super.s_blocks_per_group + 1;
       if (i != groups-1) 
           gp[i].last_block = (i+1)*Super.s_blocks_per_group;       
       else
           gp[i].last_block = ZONES - 1; /* FIXME */
   }
}

ulong try_other_groups(ulong inode,int native_group) {
   signed int i;
                     /* look for free place in groups below the native one */
   for (i=native_group-1; i >= 0 ; i--) {
      if (debug)
         printf("try_other_groups, gr:%d,  free:%lu, next:%lu last:%lu\n",
                i,gp[i].free_blocks,gp[i].next_block_to_fill,gp[i].last_block);
      if (gp[i].free_blocks==0)
             continue;
      if (gp[i].next_block_to_fill > gp[i].last_block) 
             continue;             
      while (zone_is_fixed(gp[i].next_block_to_fill))
             if (++gp[i].next_block_to_fill > gp[i].last_block) 
                   continue;
      gp[i].free_blocks--;             
      gp[i].wants_away++;
      return gp[i].next_block_to_fill++;             
   }
                   /* and if all groups below the native are filled in, then */
                   /* look for free space in all other groups */    
   for (i=native_group+1; i < groups; i++) {
      if (debug)
         printf("try_other_groups, gr:%d,  free:%lu, next:%lu last:%lu\n",
                i,gp[i].free_blocks,gp[i].next_block_to_fill,gp[i].last_block);
      if (gp[i].free_blocks==0)
             continue;
      if (gp[i].next_block_to_fill > gp[i].last_block) 
             continue;             
      while (zone_is_fixed(gp[i].next_block_to_fill))
             if (++gp[i].next_block_to_fill > gp[i].last_block) 
                   continue;
      gp[i].free_blocks--;             
      gp[i].wants_away++;
      return gp[i].next_block_to_fill++;             
   }
   
   printf("Can't find a block for inode %lu\n",inode);
   for (i=0; i < groups; i++) 
      printf("Group:%d, native:%lu,foreign:%lu\n",i,
             gp[i].native_blocks,gp[i].wants_away);
   die("Internal problem\n");
}  

/* Try to allocate block in its group and if this fails, than in any other
 * group with sufficient free space 
 */

ulong choose_block(ulong inode) {

   int i_group = (inode-1) / Super.s_inodes_per_group;

   if (gp[i_group].next_block_to_fill > gp[i_group].last_block) 
        return try_other_groups(inode,i_group);
   
   while (zone_is_fixed(gp[i_group].next_block_to_fill)) {
        if (++gp[i_group].next_block_to_fill > gp[i_group].last_block) 
              return try_other_groups(inode,i_group);
   }
   gp[i_group].native_blocks++;
   return gp[i_group].next_block_to_fill++;
}

/* ---------------------------------------------------------------------*/
void show_reserved_blocks(void) {
   int i,j;
   for (i=0; i < groups; i++) {  
   
                          /* a copy of superblock in each group */
      set_attr(i*Super.s_blocks_per_group+1,AT_SUPER);
      
                          /* a copy of group descriptors in each group */
      for (j=0; j < UPPER(groups*sizeof(struct ext2_group_desc),
                          block_size) ; j++)
         set_attr(i*Super.s_blocks_per_group+2,AT_GROUP);
      
                          /* block and inode bitmaps */                    
      for (j=0; j < UPPER(Super.s_blocks_per_group, (8*block_size)) ; j++)
         set_attr(bg[i].bg_block_bitmap + j,AT_BITMAP);
      for (j=0; j < UPPER(Super.s_inodes_per_group, (8*block_size)) ; j++)
         set_attr(bg[i].bg_inode_bitmap + j,AT_BITMAP); 
         
                          /* table of inodes */
      for(j=0; 
          j < UPPER(Super.s_inodes_per_group, (block_size / sizeof(struct ext2_inode)));
          j++)
         set_attr(bg[i].bg_inode_table + j,AT_INODE);
   }
   display_map();
   display_legend(AT_DATA|AT_BITMAP|AT_INODE|AT_SUPER|AT_GROUP|AT_BAD);
}
