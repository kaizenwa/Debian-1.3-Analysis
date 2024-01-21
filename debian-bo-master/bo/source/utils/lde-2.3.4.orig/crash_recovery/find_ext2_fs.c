#include <linux/ext2_fs.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

/* (C) 1994 Scott Heavner (sdh@po.cwru.edu) */

/* find_ext2_fs.c -- greps a full disk devices for remnants of an ext2 partition.
 *                   Will be most useful to recover from a bad fdisk experience.
 *
 * Usage: find_ext2_fs -best-guess /dev/name
 *        find_ext2_fs /dev/name
 *        find_ext2_fs                           -- defaults to /dev/hda
 *
 * If it is called with 3 parameters as shown above (-best-guess can be anything),
 * it will only display one entry for the most likely filesystem cantidates.
 * Otherwise, it will print out any blocks which contain EXT2_MAGIC, which may or
 * may not be superblocks.  Also, there are many copies of the super block on 
 * each filesystem, they will all yield the same size, but will have different
 * starts and ends.  It may be necessary to leave out the best guess option if
 * nothing shows up on the first run.
 */ 

int main(int argc, char **argv)
{
  unsigned long i=0UL, lastblock, last_lastblock=0UL, blocksize, last_size=0UL, ranch_size=0UL, ranch_last=0UL;
  char *device_name="/dev/hdb";
  int fd, best_guess=0, this_is_it;
  struct ext2_super_block *sb;

  sb = malloc(1024);

  if (argc==3) {
    best_guess = 1;
    device_name = argv[2];
  } else if (argc==2) {
    device_name = argv[1];
  } else if (argc>3) {
    printf("Usage: %s -best-only devicename\n",argv[0]);
    exit(-argc);
  }

  if ((fd=open(device_name,O_RDONLY))<0) {
    printf("Can't open device %s\n",device_name);
    exit(-1);
  }

  printf("Checking %s for ext2 filesystems %s\n",device_name,
    (best_guess?"(Best guesses only)":"(Display even the faintest shreds)"));

  while(++i) {
    if ( read(fd, sb, 1024) != 1024 ) {
      printf("Error reading block or EOF encountered (EOF is good)\n");
      exit(-2);
    }

    this_is_it = 0;
    
    if ( (sb->s_magic== EXT2_SUPER_MAGIC)||(sb->s_magic == EXT2_PRE_02B_MAGIC) ) {
      blocksize = (unsigned long)EXT2_MIN_BLOCK_SIZE << sb->s_log_block_size;
      lastblock = (i-2UL)*2UL+(sb->s_blocks_count*(blocksize/512UL));
      if ( ((i-2) > last_lastblock) || (last_size != sb->s_blocks_count) ) {
      if (!best_guess)
	  printf("THESE ENTRIES ARE PROBABLY ALL FOR THE SAME PARTITION:\n");
        last_lastblock = lastblock;
        last_size = sb->s_blocks_count;
	if ( (blocksize >= EXT2_MIN_BLOCK_SIZE) && (blocksize <= EXT2_MAX_BLOCK_SIZE) ) {
	  if ( !(ranch_size) || ((ranch_size==last_size)&&(ranch_last==last_lastblock)) ) {
            if (!best_guess)
	      printf("**** I'D BET THE RANCH ON THIS NEXT ENTRY *************\n");
            this_is_it = 1;
	    ranch_size = last_size;
	    ranch_last = last_lastblock;
	  }
	}
      }
      if ((!best_guess)||(this_is_it)) {
        printf("   * Found ext2_magic in block %lu (1024 byte blocks).\n",i-1UL);
        printf("     This file system is %lu blocks long (each block is %lu bytes)\n",
  	       sb->s_blocks_count,blocksize);
        printf("     Filesystem runs %lu : %lu (512 byte sectors)\n",(i-2UL)*2UL,lastblock);
      }
    }
  }

  exit(0);
}
