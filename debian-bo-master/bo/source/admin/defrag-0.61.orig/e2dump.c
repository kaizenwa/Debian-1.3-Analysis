
/* This is the result of my first experiments with the ext2 file system.
 * The code is rather messy and sometimes simply wrong,
 * but for me it's better than nothing.
 * 
 * Alexey Vovenko (vovenko@ixwin.ihep.su)
 */

/* added #define; FIRST_USER_INODE replaces EXT2_FIRST_INO
 * Anthony Tong (antoine@eci.com)
 */

 /* Known bugs:
  * 1. Picture option uses ncurses and requires a terminfo description
  *    from the ncurses package. Default terminfo files (from SLS at least)
  *    are broken.
  */
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <linux/ext2_fs.h>
#include "display.h"

#define FIRST_USER_INODE ((s.s_rev_level == EXT2_GOOD_OLD_REV) ? \
				EXT2_GOOD_OLD_FIRST_INO : \
				s.s_first_ino)

#ifndef TRUE
  #define FALSE 0 
  #define TRUE  1
#endif  
typedef unsigned char byte;

int voyer_mode = 1;

static int super_valid = FALSE;
static int block_size = 0;
static int groups = 0;  
static ulong *indblock;  /* indirection blocks used in file allocation scheme */
static ulong *dindblock;
static ulong *tindblock;


struct ext2_super_block s;
struct group_table {
   struct ext2_group_desc gr;
   byte *block_bmap;
   byte *inode_bmap;
} *gt;

static inline int bit_is_set(char * bitmap,unsigned int nr) 
{ 
	int __res; 
	__asm__ __volatile__("btl %1,%2; adcl $0,%0" 
		:"=g" (__res) 
		:"r" (nr),"m" (*(bitmap)),"0" (0)); 
	return __res; 
}
#define block_is_busy(bn) bit_is_set(gt[(bn-1) / s.s_blocks_per_group].block_bmap,\
                                        (bn-1) % s.s_blocks_per_group)
#define inode_is_busy(bn) bit_is_set(gt[(bn-1) / s.s_inodes_per_group].inode_bmap,\
                                        (bn-1) % s.s_inodes_per_group)

#define UPPER(size,n)		((size + ((n) - 1)) / (n))

#define my_printf(a...) if (!quiet) printf(##a)

int IN;         /* Input device handle */

void die(char *s) {
   fflush(NULL);       /* flush all streams */
   fprintf(stderr,s);
   exit(1);
}

void * malloc_chk(ulong size) {
  void *p = malloc(size);
/*  fprintf(stderr,"Alloc %u\n",size); */
  if (p == NULL) 
     die("Out of memory\n");
  return p;  
}
     
void load_super(void) {
   lseek(IN,1024,SEEK_SET);               /* read super block */
   if (read(IN,&s,sizeof(s))!=sizeof(s)) 
      exit(1);
   super_valid = (s.s_magic == EXT2_SUPER_MAGIC);
   if (!super_valid) 
             return;
   block_size = EXT2_MIN_BLOCK_SIZE << s.s_log_block_size;
   groups = (s.s_blocks_count - s.s_first_data_block + 
             s.s_blocks_per_group - 1) /
             s.s_blocks_per_group;
}
void load_groups(void) {
   int i,size;
   if (!super_valid) 
           return;
   gt = malloc_chk(sizeof(struct group_table)*groups);
   for (i = 0; i < groups; i++) {
      lseek(IN,block_size*2 + i * sizeof(struct ext2_group_desc),SEEK_SET);
      if (read(IN,&gt[i].gr,sizeof(struct ext2_group_desc))!=
          sizeof(struct ext2_group_desc)) 
         die("Unable to read group descriptors\n");
         
      if ((s.s_blocks_per_group % 8) != 0) 
         printf("Strange blocks_per_group (%lu) value",s.s_blocks_per_group);
      size = s.s_blocks_per_group / 8;
      gt[i].block_bmap = malloc_chk(size);
      lseek(IN,gt[i].gr.bg_block_bitmap*block_size,SEEK_SET);
      if (read(IN,gt[i].block_bmap,size)!=size) 
         die("Unable to read block bitmap\n");
               
      if ((s.s_inodes_per_group % 8) != 0) 
         printf("Strange inodes_per_group (%lu) value",s.s_blocks_per_group);
      size = s.s_inodes_per_group / 8;
      gt[i].inode_bmap = malloc_chk(size);   
      lseek(IN,gt[i].gr.bg_inode_bitmap*block_size,SEEK_SET);
      if (read(IN,gt[i].inode_bmap,size)!=size) 
          die("Unable to read inode bitmap\n");
          
/*      printf("Group %d, inode_bitmap offs: %d, size %d\n",
 *            i,gt[i].gr.bg_inode_bitmap,size);
 */
   }
   indblock  = malloc_chk(block_size); /* Required for allocation checks */
   dindblock = malloc_chk(block_size);
   tindblock = malloc_chk(block_size);
}

/* Inode number is in [1..super_block.s_inodes_count] range */
void load_inode(struct ext2_inode *n,ulong inode_no) {
   struct ext2_group_desc *bg;
   static struct ext2_inode *inode_group = NULL;
   static struct ext2_group_desc *last_accessed_bg = NULL;
   
   if (inode_no < 1 || inode_no > s.s_inodes_count) 
      die("Illegal inode requested!\n");
   inode_no--;
   
   if (inode_group == NULL) {
      ulong size = sizeof(struct ext2_inode)*s.s_inodes_per_group;
      inode_group = malloc_chk(size);
   }

   bg = &gt[inode_no / s.s_inodes_per_group].gr;
/* The obvious approach is here:
 *
 *   lseek(IN,bg->bg_inode_table*block_size + sizeof(struct ext2_inode)* 
 *         (inode_no % s.s_inodes_per_group), SEEK_SET);
 *   read(IN,n,sizeof(struct ext2_inode));
 *
 * but we are using 1 group size cache in hope that it is faster:
 */
   if (bg != last_accessed_bg) {     /* Load a new group */
      ulong len = sizeof(struct ext2_inode) * s.s_inodes_per_group;
      lseek(IN, block_size*bg->bg_inode_table,SEEK_SET);
      if (read(IN,inode_group,len)!=len) 
         exit(1);
      last_accessed_bg = bg;   
   }
   memcpy(n,inode_group + (inode_no % s.s_inodes_per_group),
          sizeof(struct ext2_inode));
}

void load_block(void * buf, ulong bn) {
   lseek(IN, bn*block_size,SEEK_SET);
   if (read(IN,buf,block_size)!=block_size) 
      die("Unable to read block");
}
  
#define N_BLOCKS_IN_PAGE 256
struct block_list{
    struct block_list *next;
    ulong blocks[N_BLOCKS_IN_PAGE];
} *bl = NULL;

struct block_list *add_p, *read_p;
int add_i,read_i;

void init_block_list(void) {
   if (bl == NULL) {
      bl = malloc_chk(sizeof(struct block_list));
      bl->next = 0;   
      bl->blocks[0] = 0;  /* Mark the end of the list */
  }  
  add_p = read_p = bl;
  add_i = read_i = 0;
}
                  
void add_to_block_list(ulong bn) {
   if (add_i < N_BLOCKS_IN_PAGE) 
      add_p->blocks[add_i++] = bn;
   else {
      if (add_p->next == NULL) {
         add_p->next = malloc_chk(sizeof(struct block_list));
         add_p = add_p->next;
         add_p->next = NULL;   
      }
      else
         add_p = add_p->next;
      
      add_i = 0;
      add_p->blocks[add_i++] = bn;
   }
}   
      
ulong get_from_block_list(void) {
   if ((add_p == read_p) && (read_i >= add_i)) 
      return 0;                             /* No more blocks */
   
   if (read_i < N_BLOCKS_IN_PAGE) 
      return read_p->blocks[read_i++];
   else {
      read_p = read_p->next;
      if (read_p == NULL) 
         return 0;
      read_i = 0;
      return read_p->blocks[read_i++];
   }
}
                 /* statistic on file fragmentation */
ulong old_bn,fragments,sparse_blocks,distance;
                 /* statistic on group usage */
ulong start_of_group=0, end_of_group=0, blocks_out_of_group,
      blocks_in_file; /* including indirection blocks */

ulong check_block_location(ulong bn) {
   if (bn!=0) {
      if (bn >= s.s_blocks_count) {
         fprintf(stderr,"check_block_location:%lu\n",bn);
         exit(1);
      }
      if (!block_is_busy(bn)) {
         fprintf(stderr,"block %lu is not marked busy in bitmap\n",bn);
         exit(1);
      }
      blocks_in_file++;
      if ((bn >= end_of_group) || (bn < start_of_group)) 
         blocks_out_of_group++;
         
      add_to_block_list(bn);
      if (old_bn+1!=bn) {
         fragments++;
         if (old_bn!=0) 
            if (old_bn > bn)
               distance += old_bn - bn - 1;
            else
               distance += bn - old_bn - 1;   
      }      
      old_bn = bn;
   }   
   else 
      sparse_blocks++;
   return bn;       
}

void check_allocation(struct ext2_inode *n, int quiet) {
   int checked_size = 0;
   int i_idx = 0;
   int ii_idx = 0;
   int id_idx = 0;
   int it_idx = 0;
   ulong bn=0;       /* block numbers */
   
   fragments = 0;    /* inconsistency counter */
   sparse_blocks = 0;
   distance  = 0;
   old_bn = 0;
   blocks_out_of_group = 0;
   blocks_in_file = 0;
      
   init_block_list();
   if (n->i_blocks==0)
      return;
      
   my_printf("Allocation:") ;
   while (checked_size < n->i_size) {
      if (i_idx < EXT2_NDIR_BLOCKS) {      /* first 11k allocation */
          bn = check_block_location(n->i_block[i_idx]);
          my_printf("%5lu ",bn);
          i_idx++;
          if (i_idx % 8 == 0) my_printf("\n");
      }
      else 
      if (i_idx == EXT2_IND_BLOCK) {       /* 256k allocation */
         if (ii_idx == 0) {
            bn = check_block_location(n->i_block[i_idx]);
            my_printf("(indblock %lu)\n",bn);
            load_block(indblock,bn);
         }   
         bn = check_block_location(indblock[ii_idx]);
         my_printf("%5lu ",bn);
         ii_idx++;
         if (ii_idx % 8 == 0) my_printf("\n");
         if (ii_idx*sizeof(ulong) >= block_size) {
            i_idx++;
            ii_idx = 0;
         }
      }
      else 
      if (i_idx == EXT2_DIND_BLOCK) {        /* 256*256k = 64M */                 
         if (id_idx == 0 && ii_idx == 0) {
            bn = check_block_location(n->i_block[i_idx]);
            my_printf("(dindblock %lu)\n",bn);
            load_block(dindblock,bn);
         }   
         if (ii_idx == 0) {
            bn = check_block_location(dindblock[id_idx]);
            my_printf("(indblock %lu)\n",bn);
            load_block(indblock,bn);
         }
         bn = check_block_location(indblock[ii_idx]);
         my_printf("%5lu ",bn);
         ii_idx++;
         if (ii_idx % 8 == 0) my_printf("\n");
         if (ii_idx*sizeof(ulong) >= block_size) {
            id_idx++;
            ii_idx = 0;
         }
         if (id_idx*sizeof(ulong) >= block_size) {
            i_idx++;
            ii_idx = 0;
            id_idx = 0;
         }
      }
      else 
      if (i_idx == EXT2_TIND_BLOCK) {        /* Really huge files :-) */
         if (it_idx== 0 && id_idx == 0 && ii_idx == 0) {
                                     /* load triple indirection block */
            bn = check_block_location(n->i_block[i_idx]);
            my_printf("(tindblock %lu)\n",bn);
            load_block(tindblock,bn);
         }                           /* load double indirection block */
         if (id_idx == 0 && ii_idx == 0) {
            bn = check_block_location(tindblock[it_idx]);
            my_printf("(dindblock %lu)\n",bn);
            load_block(dindblock,bn);
         }                           /* load indirection block */
         if (ii_idx == 0) {
            bn = check_block_location(dindblock[id_idx]);
            my_printf("(indblock %lu)\n",bn);
            load_block(indblock,bn);
         }
         bn = check_block_location(indblock[ii_idx]);
         my_printf("%5lu ",bn);
         ii_idx++;
         if (ii_idx % 8 == 0) my_printf("\n");
         if (ii_idx*sizeof(ulong) >= block_size) {
            id_idx++;
            ii_idx = 0;
         }
         if (id_idx*sizeof(ulong) >= block_size) {
            it_idx++;
            ii_idx = 0;
            id_idx = 0;
         }
         if (it_idx*sizeof(ulong) >= block_size)        
            die("File > big");
      }
      checked_size += block_size;
   }
   my_printf("\n");
   
   if (fragments > 1) 
      my_printf("Fragments:%lu, average distance %lu\n",
                 fragments,distance/(fragments-1));
   else               
      my_printf("Not fragmented\n");
   if (sparse_blocks) 
      my_printf("File has %lu unallocated blocks\n",sparse_blocks);
   return;
}

void dump_super(void) {
  printf("SUPERBLOCK:\n");
  printf("Inodes count:%lu\n",s.s_inodes_count);	/* Inodes count */
  printf("Blocks count:%lu\n",s.s_blocks_count);	/* Blocks count */
  printf("Reserv blocks count:%lu\n",s.s_r_blocks_count);    /* Reserved blocks count */
  printf("Free blocks   count:%lu\n",s.s_free_blocks_count); /* Free blocks count */
  printf("Free inodes   count:%lu\n",s.s_free_inodes_count); /* Free inodes count */
  printf("First data block   :%lu\n",s.s_first_data_block);  /* First Data Block */
  printf("Block size   :%u\n",EXT2_MIN_BLOCK_SIZE << s.s_log_block_size); 
  printf("Fragment size:%u\n",EXT2_MIN_FRAG_SIZE << s.s_log_frag_size); 
  printf("Blocks per group:%lu\n",s.s_blocks_per_group);/* # Blocks per group */
  printf("Frags  per group:%lu\n",s.s_frags_per_group); /* # Fragments per group */
  printf("Inodes per group:%lu\n",s.s_inodes_per_group);/* # Inodes per group */
  printf("mount time:%s", ctime(&s.s_mtime)); /* Mount time */
  printf("write time:%s", ctime(&s.s_wtime)); /* Write time */
  printf("magic:0x%X", s.s_magic); 		       /* Magic signature */
  if (s.s_magic == EXT2_SUPER_MAGIC) printf(" (OK)\n");
  else printf(" (???) ");
  printf("state:0x%X\n\n",(unsigned) s.s_state);          /* Flag */
}

void dump_inode(ulong inode_no) {
   struct ext2_inode n;

   printf("\nINODE %lu\n", inode_no);
   load_inode(&n,inode_no);

   printf("File mode %o ",(uint)n.i_mode);
   if (S_ISREG(n.i_mode)) printf("(regular file)\n");
   else if (S_ISDIR(n.i_mode)) printf("(directory)\n");
   else if (S_ISCHR(n.i_mode)) printf("(character dev)\n");
   else if (S_ISBLK(n.i_mode)) printf("(block dev)\n");
   else if (S_ISLNK(n.i_mode)) printf("(link)\n");
   else if (S_ISFIFO(n.i_mode)) printf("(fifo)\n");
   else if (S_ISSOCK(n.i_mode)) printf("(socket)\n");
   printf("Owner Uid %d ",(uint)n.i_uid);
   printf("Group Id %d\n",(uint) n.i_gid);
   printf("File size %lu\n",n.i_size);
   printf("Access time      : %s",ctime(&n.i_atime));
   printf("Creation time    : %s",ctime(&n.i_ctime));
   printf("Modification time: %s",ctime(&n.i_mtime));
   if (inode_is_busy(inode_no)) {
      if (n.i_dtime!=0) printf("ERROR: bitmap is 1  ");
   }
   else
      if (n.i_dtime==0 && n.i_ctime!=0) printf("ERROR: bitmap is 0\n ");
      
   if (n.i_dtime!=0)
      printf("Deletion time    : %s",ctime(&n.i_dtime));
   printf("Links count: %d\n", (uint) n.i_links_count);
                             /* in 512 byte blocks for some unknown reason */
   printf("512-Blocks count: %ld\n",n.i_blocks); 
   if (n.i_flags !=0) printf("Flags 0x%lX\n",n.i_flags);
   printf("Version: %lu\n",n.i_version);
   
   if (n.i_file_acl!=0 || n.i_dir_acl!=0) {    
      printf("File ACL: %lu  ",n.i_file_acl);
      printf("Directory ACL: %lu\n",n.i_dir_acl);	
   }
   if (n.i_faddr!=0 || n.i_frag!=0 || n.i_fsize!=0) {     
      printf("Fragment address: %lu\n",n.i_faddr);
      printf("Fragment number: %u\n",n.i_frag);
      printf("Fragment size: %u\n",n.i_fsize);
   }
   start_of_group = 1 + (inode_no/s.s_inodes_per_group)*s.s_blocks_per_group;
   end_of_group = start_of_group+s.s_blocks_per_group;
      
   check_allocation(&n,FALSE); 
   if (blocks_out_of_group) {
      printf("%lu out of %lu blocks are not in the right group",
             blocks_out_of_group, blocks_in_file);
      printf("(group is from %lu to %lu)\n",start_of_group,end_of_group);
   }          
}

void dump_groups(void) {
   struct ext2_group_desc *bg;
   int group_no;
                
   for (group_no = 0; group_no < groups; group_no++) {
      bg = &gt[group_no].gr;
      printf("GROUP %d\n",group_no);   
      printf("Blocks bitmap location:%lu\n",bg->bg_block_bitmap);
      printf("Inodes bitmap location:%lu\n",bg->bg_inode_bitmap);
      printf("Inodes table location:%lu\n",bg->bg_inode_table);
      printf("Free blocks in the group:%u\n",bg->bg_free_blocks_count);
      printf("Free inodes in the group:%u\n",bg->bg_free_inodes_count);
      printf("Directories count:%u\n",bg->bg_used_dirs_count);
   }
}

void dump_block_bitmap(void) {
   byte *bmp;
   int group_no,i,entries;
   
   entries = s.s_blocks_per_group / 8;             
   for (group_no = 0; group_no < groups; group_no++) {
      printf("Group %d block bitmap:\n",group_no);
      bmp = gt[group_no].block_bmap;
      for (i=0; i < entries; ) {
         printf("%2.2X ",bmp[i]);
         i++;       
         if (i%16 == 0)
                printf("\n");
      }
   }             
}

void dump_block(ulong bn) {
   int i;
   byte *buf;
   
   printf("Block %lu:\n",bn);
   buf = malloc_chk(block_size);
   if (lseek(IN,bn*block_size,SEEK_SET)<0)
       die("Can't seek there\n");
   if (block_size!=read(IN,buf,block_size))
       die("Can't read the block\n");
       
   for (i=0; i < block_size; ) {
         printf("%2.2X ",buf[i]);
         i++;       
         if (i%16 == 0)
                printf("\n");
   }             
   free(buf);
}
void report_fragm(void) {
   ulong inode_no;
   struct ext2_inode n;
   ulong total_files=0,
         total_frag_files=0,
         total_fragments=0,
         total_dirs=0,
         total_frag_dirs=0,
         avr_dist=0,
         avr_fr_size=0,
         avr_dist_file=0,
         avr_fr_size_file=0;
         
   for (inode_no=FIRST_USER_INODE; inode_no <= s.s_inodes_count; inode_no++) {
      load_inode(&n,inode_no);
      if (n.i_dtime != 0 || n.i_ctime==0) { /* FIXME: is it a free inode? */
         if (inode_is_busy(inode_no))
              printf("ERROR: deleted inode %lu marked busy in bitmap\n",inode_no);
         continue;                   
      }   
      if (!inode_is_busy(inode_no)) 
         printf("ERROR: inode %lu not marked busy in bitmap\n",inode_no);
      check_allocation(&n,TRUE);
      if (fragments > 1) {
         printf("Inode: %lu, frag: %lu, file size %lu, ",
                inode_no, fragments, n.i_size);
         total_frag_files++;       
         total_fragments += fragments;
      }
      if (n.i_size > 0) {
         if (fragments > 1) {
            avr_dist_file  = distance /(fragments-1);
            avr_fr_size_file = UPPER(n.i_size,block_size)/(fragments);
            avr_dist += avr_dist_file;
            avr_fr_size += avr_fr_size_file;
         }   
         if (S_ISREG(n.i_mode)) {
            total_files++;
            if (fragments > 1) {
               printf("(Average frag size: %lu, dist: %lu blocks)\n",
                      avr_fr_size_file,
                      avr_dist_file);
            }
         }   
         if (S_ISDIR(n.i_mode)) {
            total_dirs++;
            if (fragments > 1) {
               total_frag_dirs++;
               printf("Fragmented directory!\n");
            }   
         }       
      }
      if (sparse_blocks) printf("Inode %lu has %lu unallocated blocks\n",
                                inode_no,sparse_blocks);   
   }       
   printf("\n%lu out of %lu files are fragmented (%ld%%)\n",
          total_frag_files,total_files,
          (total_files==0) ? 0 : (total_frag_files*100)/total_files);
   printf("%lu out of %lu directories are fragmented (%ld%%)\n",
          total_frag_dirs,total_dirs,
          (total_dirs==0) ? 0 : (total_frag_dirs*100)/total_dirs);
   printf("Total number of fragments: %lu, ",total_fragments);
   if (total_frag_files==0) total_frag_files = 1;  
   printf("Average is %lu fragments per fragmented file\n",
          total_fragments / total_frag_files);
   printf("Average fragment size in fragmented file:%lu blocks\n",
          avr_fr_size / total_frag_files);
   printf("Average distance between fragments: %ld blocks\n",
          avr_dist / total_frag_files );              
}
void report_group_fill(void) {
   ulong inode_no,gr_no,blocks_in_the_group;
   struct ext2_inode n;
   ulong gr_blocks=0,
         tot_blocks=0,
         far_files=0,
         frag_files=0,
         frag_blocks=0,
         far_blocks=0,
         tot_files=0,       /* total non-emtpy files counted here */
         tot_far_files=0,
         tot_frag_files=0,
         tot_alien_blocks=0;
        
         
   struct ext2_group_desc * bg;
   
   for (gr_no = 0; gr_no < groups; gr_no++) {   
      start_of_group = 1 + gr_no*s.s_blocks_per_group;
      end_of_group = start_of_group+s.s_blocks_per_group;
      
      if(gr_no==groups-1) 
         blocks_in_the_group = (s.s_blocks_count % s.s_blocks_per_group) - 1;
      else
         blocks_in_the_group = s.s_blocks_per_group;
         
      /* we have a copy of sb and group descriptors in each group */
      blocks_in_the_group -= 1  /* superblock */
                             + UPPER(groups*sizeof(struct ext2_group_desc), 
                               block_size);    
                                      
      blocks_in_the_group -= UPPER(s.s_inodes_per_group, 8*block_size) +
                             UPPER(s.s_blocks_per_group, 8*block_size) +
            UPPER(s.s_inodes_per_group*sizeof(struct ext2_inode),block_size);
                                
      for (inode_no=gr_no*s.s_inodes_per_group+1; 
           inode_no <= s.s_inodes_per_group*(gr_no+1); 
           inode_no++) {
         if (inode_no < FIRST_USER_INODE && inode_no!=EXT2_ROOT_INO) 
             continue;  
            
         load_inode(&n,inode_no);
         if (n.i_dtime != 0 || n.i_ctime==0) { /* FIXME: is it a free inode? */
            if (inode_is_busy(inode_no))
                 printf("ERROR: deleted inode %lu marked busy in bitmap\n",inode_no);
            continue;                   
         }  
          
         if (!inode_is_busy(inode_no)) 
            printf("ERROR: inode %lu not marked busy in bitmap\n",inode_no);
         check_allocation(&n,TRUE);
         if (!blocks_in_file) 
             continue;
                 
         tot_files++;
         gr_blocks += blocks_in_file;
         if (blocks_out_of_group) 
/*            printf("Inode %d has %d blocks out of the group\n",
 *                  inode_no,blocks_out_of_group);
 */           
         if (blocks_out_of_group == blocks_in_file) {
            far_files++;
            far_blocks += blocks_out_of_group;
         }   
         else   
         if (blocks_out_of_group) {
            if (blocks_out_of_group > blocks_in_file) {
               printf("Inode:%ld, file_blocks:%ld, blocks out:%ld\n",
                      inode_no,blocks_in_file,blocks_out_of_group);
               die("Block counting problem\n");
            }
            frag_files++;
            frag_blocks += blocks_out_of_group;
         }
         tot_alien_blocks += blocks_out_of_group;
      }   
      bg = &gt[gr_no].gr;
      printf("GROUP %ld:   ",gr_no);
      printf("%ld used inodes\n",
             s.s_inodes_per_group - bg->bg_free_inodes_count);
      printf("%lu blocks required for all files, ",
             gr_blocks);
      printf("%lu blocks used, %u free\n",
             blocks_in_the_group - bg->bg_free_blocks_count,
             bg->bg_free_blocks_count);
      printf("%ld files have %lu blocks outside the group,\n",frag_files,
              frag_blocks);
      printf("%ld files (%lu blocks) are located "
	     "completely out of the group\n",
              far_files,far_blocks);               
              
                  /* alien blocks include ACL blocks, bad blocks etc */
      printf("%ld alien blocks\n\n",blocks_in_the_group - gr_blocks 
                                    + far_blocks + frag_blocks 
                                    - bg->bg_free_blocks_count);  
      tot_blocks += gr_blocks;
      tot_frag_files  += frag_files;
      tot_far_files += far_files;
      gr_blocks = 0;
      far_files = 0;
      frag_files = 0;
      far_blocks = 0;
      frag_blocks = 0;
   }
   if (tot_files==0) tot_files=1; /* avoid division by 0 */
   if (tot_blocks==0) tot_blocks=1;
   printf("%ld%% of files are completely out of their groups\n",
          (tot_far_files*100) / tot_files);
   printf("%ld%% of files have blocks out of their groups\n",
          (tot_frag_files*100) / tot_files);
   printf("%ld%% of blocks are allocated out of their parent inode's group\n",
          (tot_alien_blocks*100) / tot_blocks);      
}


void show_map(void) {                /* make block map */
   ushort attr;    
   int i,j,inode;
   struct ext2_inode n;
   ulong bn;
   ulong total_files=0,total_dirs=0,
         frag_files=0,frag_dirs=0,
         far_files=0;
   char str[256];      
         
   for (i=0; i < groups; i++) {  
   
                          /* a copy of superblock in each group */
      set_attr(i*s.s_blocks_per_group+1,AT_SUPER);
      
                          /* a copy of group descriptors in each group */
      for (j=0; j < UPPER(groups*sizeof(struct ext2_group_desc),
                          block_size) ; j++)
          set_attr(i*s.s_blocks_per_group+2,AT_GROUP);
      
                          /* block and inode bitmaps */                    
      for (j=0; j < UPPER(s.s_blocks_per_group, (8*block_size)) ; j++)
         set_attr(gt[i].gr.bg_block_bitmap + j,AT_BITMAP);
      for (j=0; j < UPPER(s.s_inodes_per_group, (8*block_size)) ; j++)
         set_attr(gt[i].gr.bg_inode_bitmap + j,AT_BITMAP); 
         
                          /* table of inodes */
      for(j=0; 
          j < UPPER(s.s_inodes_per_group, (block_size / sizeof(struct ext2_inode)));
          j++)
         set_attr(gt[i].gr.bg_inode_table + j,AT_INODE);
   }
   
   for (inode=1; inode <= s.s_inodes_count; inode++) {
      load_inode(&n,inode);
      if (n.i_dtime != 0)            
         continue;
      if (n.i_ctime == 0)  /* Should not happen */
         continue;        
         
      start_of_group = 1 + (inode/s.s_inodes_per_group)*s.s_blocks_per_group;
      end_of_group = start_of_group+s.s_blocks_per_group;      
      check_allocation(&n,TRUE);
      
      attr = 0;   
      if (S_ISDIR(n.i_mode)) {
         attr |= AT_DIR;
         total_dirs++;
         if (fragments > 1) 
             frag_dirs++;
      }   
      if (S_ISREG(n.i_mode)) {
         attr |= AT_REG;   
         total_files++;
         if (fragments > 1) 
             frag_files++;
      }
      if (fragments > 1) 
         attr |= AT_FRAG;

      if (blocks_out_of_group)
         far_files++;    
      if (inode==EXT2_BAD_INO) 
         attr |= AT_BAD;
      while (0 != (bn=get_from_block_list())) 
         set_attr(bn,attr);
   }

   display_map();
   if (total_files==0) total_files=1; /* avoid division by 0 */
   if (total_dirs==0) total_dirs=1;
   
   
   sprintf(str,"%ld%% files fragmented",(frag_files*100)/total_files);
   add_comment(str);
   sprintf(str,"%ld%% directiories fragmented",(frag_dirs*100)/total_dirs);
   add_comment(str);
   sprintf(str,"%ld%% files left their groups",
	   (far_files*100)/(total_files+total_dirs));
   add_comment(str);
   display_comments(" Fragmentation ");
}

enum commands {      
   CM_NONE,
   CM_SUPER,
   CM_GROUPS,
   CM_INODE,
   CM_FRAG,
   CM_FILE,
   CM_DISPLAY,
   CM_B_BITMAP,
   CM_B_DUMP,
   CM_GR_FILL
};

char *InputDevice=NULL;   /* Ext2-formatted block device we are analysing */
char *FileName;           /* Name of a file to print fragmentation data   */
ulong StartInode,InodeCount;                   /* Range of inodes to dump */
ulong StartBlock; /* Block to dump */

void usage(void) {
   printf("Usage:e2dump [<option>] <device>\n");
   printf("\t-s\t\tdumps superblock\n");
   printf("\t-g\t\tdumps groups tables\n");
   printf("\t-m\t\tdumps block allocation bitmaps in hex\n");
   printf("\t-b <number>\tdumps block's contents\n"); 
   printf("\t-i <number>\tdumps inode data and allocation chain\n");
   printf("\t-n <file>\tdumps file's inode data and allocation chain\n");
   printf("\t-f\t\tprints some statistic for all fragmented files on device\n");   
   printf("\t-G\t\tprints group usage statistic\n");
   printf("\t-d (default)\tdisplays a picture of device allocation\n");
   
   exit(1);
}

enum commands parse_args(int argc, char *argv[]) {
   int c;
   c = getopt(argc,argv,"sgi:fn:dmb:G");
   InputDevice = argv[optind];
   if (c==EOF) return CM_DISPLAY;
   
   switch (c) {
      case 's': return CM_SUPER; 
      case 'g': return CM_GROUPS;
      case 'i': if (0 == sscanf(optarg,"%lu", &StartInode))
                   usage();
                InodeCount = 1;
                return CM_INODE;
      case 'n': FileName = optarg;
                return CM_FILE;         
      case 'f': return CM_FRAG;
      case 'd': return CM_DISPLAY;
      case 'm': return CM_B_BITMAP;       
      case 'b': if (0 == sscanf(optarg,"%lu", &StartBlock))
                   usage();
                return CM_B_DUMP;       
      case 'G': return CM_GR_FILL;       
      default:  usage();
                return CM_NONE; /* never reached in fact */
   }                      
}

int main(int argc, char *argv[]) {
   ulong i;
   enum commands cmd;
   struct stat st;
                    
   cmd = parse_args(argc,argv);
   IN = open(InputDevice,O_RDONLY);
   if (IN < 0) {
      fprintf(stderr,"Unable to open device %s\n",InputDevice);
      usage();
      exit(1);
   }
   load_super();
   load_groups();
   if (!super_valid && cmd!=CM_SUPER) {
      fprintf(stderr,"Bad magic in superblock\n");
      exit(1);
   }
   switch (cmd) {
      case CM_SUPER:  dump_super();  break;
      case CM_GROUPS: dump_groups(); break;
      case CM_INODE:  for (i = StartInode; i < StartInode + InodeCount; i++)
                         dump_inode(i);
                      break;   
      case CM_GR_FILL:report_group_fill();
                      break;                
      case CM_FRAG:   report_fragm(); break;
      case CM_FILE:   if (0!=stat(FileName,&st)) {
                         char s[256];
                         sprintf(s,"Can't stat %s",FileName);
                         perror(s);
                         exit(1);
                      }
                      dump_inode(st.st_ino);
                      break;
      case CM_B_BITMAP:
                      dump_block_bitmap();
                      break;
      case CM_B_DUMP: dump_block(StartBlock);                
                      break;
      case CM_DISPLAY:init_screen(s.s_blocks_count);
                      show_map();
                      display_legend(AT_REG|AT_DIR|AT_BITMAP|
                                     AT_INODE|AT_GROUP|AT_SUPER|AT_BAD);
                      done_screen(TRUE);
                      /* Fall through */
      case CM_NONE:   break;
   }
   return 0;
}
