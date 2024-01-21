/*
 * ext2.h - extfs-specific include file for the Linux file system 
 * degragmenter. 
 *
 * changes are Copyleft  (C) 1997 Anthony Tong (antoine@eci.com)
 * Copyleft  (C) 1993 Alexey Vovenko (vovenko@ixwin.ihep.su)
 * Copyright (C) 1992, 1993 Stephen Tweedie (sct@dcs.ed.ac.uk)
 * Copyright (C) 1992 Remy Card (card@masi.ibp.fr)
 * Copyright (C) 1991 Linus Torvalds (torvalds@kruuna.helsinki.fi)
 * 
 * This file may be redistributed under the terms of the GNU General
 * Public License.
 *
 */

#include <linux/fs.h>
#include <linux/ext2_fs.h>

#define HAS_TIND

#define DIRECT_ZONES     EXT2_NDIR_BLOCKS

typedef unsigned long Block;

#define d_inode ext2_inode
#define i_zone i_block

#define UPPER(size,n)		((size + ((n) - 1)) / (n))
#define INODE_SIZE		(sizeof (struct d_inode))
#define INODE_BLOCKS		UPPER(INODES, EXT2_INODES_PER_BLOCK)
#define INODE_BUFFER_SIZE	(INODE_BLOCKS * BLOCK_SIZE)

#define BITS_PER_BLOCK		(BLOCK_SIZE<<3)

#define Super		(* (struct ext2_super_block *) super_block_buffer)
#define INODES		(Super.s_inodes_count)
#define ZONES		(Super.s_blocks_count)
#define FREEBLOCKSCOUNT	(Super.s_free_blocks_count)
#define FREEINODESCOUNT	(Super.s_free_inodes_count)
#define FIRSTZONE	(Super.s_first_data_block) 
#define ZONESIZE	(Super.s_log_block_size)
#define MAGIC		(Super.s_magic)
#define NORM_FIRSTZONE	1                          
#define INODES_PER_BLOCK (BLOCK_SIZE >> 2)

#define ROOT_INO         EXT2_ROOT_INO
#define BAD_INO          EXT2_BAD_INO

/* Replacement for EXT2_FIRST_INO(s) */
#define FIRST_USER_INODE ((Super.s_rev_level == EXT2_GOOD_OLD_REV) ? \
				EXT2_GOOD_OLD_FIRST_INO : \
				Super.s_first_ino)


    /* This structure is used to calculate, how many blocks are claimed 
     * by the inodes in the each group. 
     * native_blocks is set by update_group_population and counts blocks
     *               that should be in the group and already are there.
     *               This is the most useless variable. It's collected for
     *               report only.
     *
     * wants_away    is set by update_group_population and counts blocks   
     *               that shouldn't be in the group, but initially are.
     *
     * wants_back    is also set by check_group_population and holds 
     *               count of blocks, that aren't initially in their group
     *
     * free_blocks   is set by_check_group_population() from the block 
     *               bitmaps to the amount of the blocks that would be
     *               left free in the group, after all its blocks are
     *               brought there and all foreign blocks are removed.
     *               This is the key parameter which tells us, how
     *               many blocks from other groups are allowed into the
     *               current group without pushing out the blocks from 
     *               this group.
     * 
     */
      
struct groups_population {
   ulong free_blocks;
   ulong native_blocks; /* Not used actually anywhere, but verbose report */
   ulong wants_away;
   ulong wants_back;
   ulong next_block_to_fill;
   ulong last_block;
};

ulong choose_block(ulong inode);
void check_group_population(void);
void update_group_population(ulong znr,enum walk_zone_mode mode,ulong inode);
   
