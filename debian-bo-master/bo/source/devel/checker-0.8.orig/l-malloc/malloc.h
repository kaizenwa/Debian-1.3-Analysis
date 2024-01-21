/* Declaration for malloc, free, etc
   Copyright 1990, 1991, 1992 Free Software Foundation

   Written May 1989 by Mike Haertel.
   Heavily modified Mar 1992 by Fred Fish. (fnf@cygnus.com)
   Heavily mofified Sep 1993 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

   The author may be reached (Email) at the address mike@ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation. 

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/

#ifndef _MALLOC_H
#define _MALLOC_H	1

/* Configuration files.  */
#include "define.h"
#include "../checker.h"

#ifdef	__cplusplus
extern "C"
{
#endif

/* ANSI-C is assumed */
#ifndef PTR
#define PTR void *
#endif

#ifndef	NULL
#define	NULL	0
#endif

#include <stddef.h>

/* Declared later in this file, identify a heap.  */
struct mdesc;
  
/* Allocate 'size' bytes of memory.  */
extern PTR malloc (size_t __size);

/* The internal form of malloc.  */
extern PTR malloc_1 (struct mdesc *mdp, size_t __size);
  
/* Re-allocate the previously allocated block
   in PTR, making the new block 'size' bytes long.  */
extern PTR realloc (PTR __ptr, size_t __size);

/* Allocate 'nmemb' elements of 'size' bytes each, all initialized to 0.  */
extern PTR calloc (size_t __nmemb, size_t __size);

/* Free a block allocated by `malloc', `realloc' or `calloc'.  */
extern void free (PTR __ptr);
extern void free_1 (struct mdesc *mdp, PTR __ptr);

/* Allocate 'size' bytes allocated to 'alignment' bytes.  */
extern PTR memalign (size_t __alignment, size_t __size);
#if 0
extern PTR memalign_1 (struct mdesc *mdp, size_t __alignment, size_t __size);
#endif

/* Allocate 'size' bytes on a page boundary.  */
extern PTR valloc (size_t __size);

#ifndef DONT_DEFINE_SBRK
/* Emulate sbrk.  */
PTR sbrk (int incr);
#endif

#ifdef _MALLOC_INTERNAL

#include <chkrlib.h>

/* Header of a block in a heap.  Blocks are doubly linked.  */
struct malloc_header;
struct malloc_header
{
  /* The next malloc_header, on NULL_HEADER.  */
  struct malloc_header *prev;
  
  /* The previous malloc_header, or NULL_HEADER.  */
  struct malloc_header *next;
  
  /* The total size of this block.  */
  size_t size;
  
  /* The current state of this block.  See MD*.  */
  char state;

  /* Type of garbage.  See POINT_*.  */
  char garbage_t;
  
  /* A flag used for garbage.  */
  char g_flag;
  
  /* Equal to size - info.busy.real_size.  */
  char s_diff;
  
  /* Informations according to state.  */
  union
    {
      /* state == MDFREE.  */
      struct
        {
          /* The next free block, in the free list.  */
          struct malloc_header *prev;
          
          /* The previous...  */
	  struct malloc_header *next;
        } free;

      /* state == MDAGED.  */
      struct
        {
          /* The next aged block, in the aged list.  */
          struct malloc_header *prev;
          
          /* The previous...  */
	  struct malloc_header *next;
	} aged;

      /* state == MDBUSY.  */
      struct
        {
          /* Describe the garbage familly.  */
	  union
	    {
	      /* Common group.  */
	      struct malloc_header *g_parent;
	      
	      /* Number of brothers.  */
	      unsigned int g_number;
	    }
	  g_link;
	  
	  /* Real value of SIZE, as passed to malloc.  */
	  int real_size;
	} busy;

      /* state == MDBRK.  */
      struct
        {
	  int padpad;
	  
	  /* Real value passed to sbrk.  */
	  int real_size;
	} brk;
    } info;
};

/* Size of the hash table for free lists.  If you change it, see bitops.h. */
#define HASH_SIZE 20

/* Heapindex was a bad optimization.  */
#define NO_HEAPINDEX

/* Standard value.  */
#define NULL_HEADER (struct malloc_header*)0
#define HEADER_SIZE (sizeof(struct malloc_header))

/* The smallest space that malloc can allocate.  Is it useful ?  */
#define LITTLE_SPACE BIGGEST_ALIGNMENT

/* State possibilities.  */
/* The block is free.  */
#define MDFREE 1

/* The block is busy.  */
#define MDBUSY 2

/* The block is in the aged list.  */
#define MDAGED 3

/* The block is reserved for internal use.  */
#define MDINTRN 4

/* The block is used by brk/sbrk.  */
#define MDBRK 5

/* The block is kept for ever.  */
#define MDKEEP 6

/* A flag used while checking.  */
#define MDCHECK 128

/* Numbers of bytes at the end before they will be returned to the system.  */
#define FINAL_FREE_SIZE 0x2000	/* 8 kb */

/* Default of the maximum age.  */
#define DEFAULT_BLOCK_AGE 10

/* Used to calculate the hash index.  */
#include "bitops.h"

void _internal_free (struct mdesc *mdp, struct malloc_header *block);
struct malloc_header *find_header (struct mdesc *mdp, const PTR ptr, int soft);
int get_age (struct mdesc *mdp, struct malloc_header *block);
void disp_block_history (PTR * ptr);
size_t test_malloc0 (size_t size, char *id);

#ifdef CHKR_GARBAGE
/* Values of garbage_t */
#define POINT_NOT   0		/* no pointer on it */
#define POINT_SURE  1		/* a pointer on the begin */
#define POINT_MAYBE 2		/* a pointer inside */
#define POINT_LEAK  3		/* This is a leak */
#define POINT_SEEN  4		/* already seen (for new zones) */
#define POINT_MASK  15
#define INUSE_SEEN  16		/* already seen (for in_use) */
#define INUSE_NOT   0
#define INUSE_MASK  240

/* Values of g_flag */
#define G_ALONE 0		/* no brothers */
#define G_CHILD 2		/* a child. g_group points to its parents */
#define G_PARENT 3		/* a parent. g_number is the number of childs */
#else /* !CHKR_GARBAGE */
#define be_red_zone 0
#define af_red_zone 0
#endif /* CHKR_GARBAGE */

  void *morecore (int size);
  extern int __malloc_initialized;

#define MMALLOC_MAGIC		"MMalloc"	/* Mapped file magic number */
#define MMALLOC_MAGIC_SIZE	8		/* Size of magic number buf */
#define MMALLOC_VERSION		1		/* Current mmalloc version */
#define MMALLOC_KEYS		16		/* Keys for application use */

/* The difference between two pointers is a signed int.  On machines where
   the data addresses have the high bit set, we need to ensure that the
   difference becomes an unsigned int when we are using the address as an
   integral value.  In addition, when using with the '%' operator, the
   sign of the result is machine dependent for negative values, so force
   it to be treated as an unsigned int. */

#define ADDR2UINT(addr)	((unsigned int) ((char *) (addr) - (char *) NULL))
#define RESIDUAL(addr,bsize) ((unsigned int) (ADDR2UINT (addr) % (bsize)))

/* Address to block number and vice versa.  */

#define BLOCK(A) (((char *) (A) - mdp -> heapbase) / BLOCKSIZE + 1)

#define ADDRESS(B) ((PTR) (((B) - 1) * BLOCKSIZE + mdp -> heapbase))

/* Statistics available to the user.
   FIXME:  By design, the internals of the malloc package are no longer
   exported to the user via an include file, so access to this data needs
   to be via some other mechanism, such as mmstat_<something> where the
   return value is the <something> the user is interested in. */

struct mstats
  {
    size_t bytes_total;		/* Total size of the heap. */
    size_t chunks_used;		/* Chunks allocated by the user. */
    size_t bytes_used;		/* Byte total of user-allocated chunks. */
    size_t chunks_free;		/* Chunks in the free list. */
    size_t bytes_free;		/* Byte total of chunks in the free list. */
  };

/* Internal structure that defines the format of the malloc-descriptor.
   This gets written to the base address of the region that mmalloc is
   managing, and thus also becomes the file header for the mapped file,
   if such a file exists. */

struct bitmapinfo 
  {
     unsigned char *base;
     unsigned int size;
     unsigned int pages;
  };
  
struct mdesc
{
  /* The "magic number" for an mmalloc file. */

  char magic[MMALLOC_MAGIC_SIZE];

  /* The size in bytes of this structure, used as a sanity check when reusing
     a previously created mapped file. */

  unsigned int headersize;

  /* The version number of the mmalloc package that created this file. */

  unsigned char version;

  /* Some flag bits to keep track of various internal things. */

  unsigned int flags;

  /* If a system call made by the mmalloc package fails, the errno is
     preserved for future examination. */

  int saved_errno;

  /* Pointer to the function that is used to get more core, or return core
     to the system, for requests using this malloc descriptor.  For memory
     mapped regions, this is the mmap() based routine.  There may also be
     a single malloc descriptor that points to an sbrk() based routine
     for systems without mmap() or for applications that call the mmalloc()
     package with a NULL malloc descriptor.

     FIXME:  For mapped regions shared by more than one process, this
     needs to be maintained on a per-process basis. */

  PTR (*morecore) (struct mdesc *, int);
     
  /* Pointer to the function that causes an abort when the memory checking
     features are activated.  By default this is set to abort(), but can
     be set to another function by the application using mmalloc().

     FIXME:  For mapped regions shared by more than one process, this
     needs to be maintained on a per-process basis. */

  void (*abortfunc) (void);

  /* Debugging hook for free.

     FIXME:  For mapped regions shared by more than one process, this
     needs to be maintained on a per-process basis. */

  void (*mfree_hook) (PTR, PTR);

  /* Debugging hook for `malloc'.

     FIXME:  For mapped regions shared by more than one process, this
     needs to be maintained on a per-process basis. */

  PTR (*mmalloc_hook) (PTR, size_t);

  /* Debugging hook for realloc.

     FIXME:  For mapped regions shared by more than one process, this
     needs to be maintained on a per-process basis. */

  PTR (*mrealloc_hook) (PTR, PTR, size_t);

  struct malloc_header *_heapinfo[HASH_SIZE];	/* hash table for free blocks */
#ifndef NO_HEAPINDEX
  unsigned int _heapindex;
#endif
  struct malloc_header *_firstblock;	/* first block in the memory */
  struct malloc_header *_lastblock;	/* last block in the mem */
  struct malloc_header *_youngerblock;
  struct malloc_header *_olderblock;
  unsigned int _agedblock;	/* number of block in the aged list */

  /* Instrumentation.  */

  struct mstats heapstats;

  /* The base address of the memory region for this malloc heap.  This
     is the location where the bookkeeping data for mmap and for malloc
     begins. */

  char *base;

  /* The current location in the memory region for this malloc heap which
     represents the end of memory in use. */

  char *breakval;

  /* The end of the current memory region for this malloc heap.  This is
     the first location past the end of mapped memory. */

  char *top;

  /* Open file descriptor for the file to which this malloc heap is mapped.
     This will always be a valid file descriptor, since /dev/zero is used
     by default if no open file is supplied by the client.  Also note that
     it may change each time the region is mapped and unmapped. */

  int fd;

  /* Flag to avoid recursion.  See MUTEX_FAIL below. */
  
  uint lock;
  
  /* An array of keys to data within the mapped region, for use by the
     application.  */

  PTR keys[MMALLOC_KEYS];
  
  union
    {
      struct
        {
          struct mdesc *next_mdesc;
          struct mdesc *prev_mdesc;
          unsigned int mapinfo;
          struct bitmapinfo *bitmap;
          PTR *history;
        } inmem;
      struct
        {
          unsigned int off_bitmap;	/* Offset in the file */
          unsigned int len_bitmap;	/* Size in bytes */
        } ondisk;
    } info;
};

#define NULL_MDESC ((struct mdesc*)0)
/* Bits to look at in the malloc descriptor flags word */

#define MMALLOC_DEVZERO		(1 << 0)	/* Have mapped to /dev/zero */
#define MMALLOC_INITIALIZED	(1 << 1)	/* Initialized mmalloc */
#define MMALLOC_MMCHECK_USED	(1 << 2)	/* mmcheck() called already */
#define MMALLOC_SBRK_HEAP	(1 << 3)	/* Use the real sbrk() */

/* Allocate SIZE bytes of memory.  */

extern PTR mmalloc (struct mdesc *, size_t);

/* Re-allocate the previously allocated block in PTR, making the new block
   SIZE bytes long.  */

extern PTR mrealloc (struct mdesc *, PTR, size_t);

/* Allocate NMEMB elements of SIZE bytes each, all initialized to 0.  */

extern PTR mcalloc (struct mdesc *, size_t, size_t);

/* Free a block allocated by `mmalloc', `mrealloc' or `mcalloc'.  */

extern void mfree (struct mdesc *, PTR);

/* Allocate SIZE bytes allocated to ALIGNMENT bytes.  */

extern PTR mmemalign (struct mdesc *, size_t, size_t);

/* Allocate SIZE bytes on a page boundary.  */

extern PTR mvalloc (struct mdesc *, size_t);

PTR mmalloc_detach (PTR md);
PTR mmalloc_attach (int fd, PTR baseaddr);

/* Activate a standard collection of debugging hooks.  */

extern int mmcheck (struct mdesc *, void (*) (void));

/* Pick up the current statistics. (see FIXME elsewhere) */

extern struct mstats mmstats (struct mdesc *);

/* Hooks for debugging versions.  */

extern void (*__mfree_hook) (PTR, PTR);
extern PTR (*__mmalloc_hook) (PTR, size_t);
extern PTR (*__mrealloc_hook) (PTR, PTR, size_t);

/* A default malloc descriptor for the single sbrk() managed region. */

extern struct mdesc *__mmalloc_default_mdp;

/* Initialize the first use of the default malloc descriptor, which uses
   an sbrk() region. */

extern struct mdesc *__mmalloc_sbrk_init (void);

/* Grow or shrink a contiguous mapped region using mmap().
   Works much like sbrk() */

extern PTR __mmalloc_mmap_morecore (struct mdesc *, int);

/* Macro to convert from a user supplied malloc descriptor to pointer to the
   internal malloc descriptor.  If the user supplied descriptor is NULL, then
   use the default internal version, initializing it if necessary.  Otherwise
   just cast the user supplied version (which is void *) to the proper type
   (struct mdesc *). */

#define MD_TO_MDP(md) \
  ((md) == NULL \
   ? (__mmalloc_default_mdp == NULL \
      ? __mmalloc_sbrk_init () \
      : __mmalloc_default_mdp) \
   : (struct mdesc *) (md))

extern struct mdesc *_firstmdesc;
extern struct mdesc *_lastmdesc;
extern uint mdesc_lock; /* lock flag for _firstmdesc, _lastmdesc ... */

/* Action to do if mutex fails.
 * Currently it simply aborts.  With multi-threaded implementation, the mutex
 * value can be the thread id.  The action can wait if an other thread is
 * using the structure or abort if the same thread is using. */
#define MUTEX_FAIL 						\
do 								\
  {								\
    chkr_perror(M_I_REC_MA_ET);					\
    chkr_abort();						\
  }								\
while(0)

#endif /* _MALLOC_INTERNAL */

int new_heap (int brk, struct mdesc *mdp);
void remove_heap (int slot);

#ifdef CHKR_PROFILE
extern uint nbr_free_calls;
extern uint nbr_malloc_calls;
extern uint nbr_realloc_calls;

uint get_total_aged_size (struct mdesc *mdp);
uint get_total_mem (void);
#endif

#ifdef __cplusplus
}

#endif

#endif /* _MALLOC_H */
