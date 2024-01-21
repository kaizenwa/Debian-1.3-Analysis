/* Memory access checker.
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written September 1993 by Tristan Gingold

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

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
/* This file defines two important functions: 
    chkr_check_addr and chkr_set_right.
  These functions handle the bitmaps.  */

#include <sys/stat.h>
#define NEED_MM
#include "checker.h"

#ifdef GNU_MALLOC
#include "chkr-stubs.h"
#endif

#define _MALLOC_INTERNAL
#define NEED_MM
#include "malloc.h"

#ifdef HAVE_SHM
#include <sys/shm.h>
#endif

#include "errlist.h"
#include "message.h"

#ifndef MDCHECKER
#include "maccess.mes"

/* Protect internals data against recursion.  */
#ifndef ENTER_CHECKER
#ifdef NO_SIGNALS
#define ENTER_CHECKER
#define LEAVE_CHECKER
#else
#define ENTER_CHECKER chkr_in_checker++
#define LEAVE_CHECKER 				\
	do					\
	  {					\
	    chkr_in_checker--;			\
	    if (chkr_sig_catched)		\
	      chkr_send_delayed_signals ();	\
	  }					\
	while (0)
#endif
#endif

typedef unsigned char uchar;
/* typedef unsigned int uint; */

#ifdef NEED_CHKR_LD_OPTIONS
/* Defined in ../parse-args.c  Can disable some errors */
extern const int chkr_ld_options;
#endif

/* True if the text segment is writable: linked with ld -N.  */
int is_text_writable;

/* Number of memory access error displayed.  */
static uint nbr_rep_access_error;

/* Number of memory access error detected.  */
static uint nbr_tot_access_error;

/* Different types of bitmap:
   SegFinish : end of table
   SegVoid :   Can't be accessed (e.g. NULL pointers)
   SegText:    Text segment.
   SegROnly :  Can't be written.
   SegNormal : Normal segment (e.g. heap segment)
   SegRW :     Can be read and write but no bitmap (e.g. data segment)
   SegWOnly :  Can be only written. (!)
   SegMmap :   memory allocated by mmap.
   SegShm :    memory allocated by shm.
   SegPage :   One byte of right per page.
 */
typedef enum
  {
    SegFinish, SegVoid, SegText, SegNormal,
    SegRW, SegROnly, SegWOnly, SegMmap, SegShm, SegPage
  } MapType;

/* Defined latter in this file.  */
struct BitMem;
struct BitMemInfo;
struct Mmapinfo;
struct Pageinfo;
struct Shminfo;
struct Heapinfo;

/* Anything you want to know about a bitmap.  */
typedef struct MAPInfo
{
  /* Base of the segment.  This segment recovers [base, base + length).  */
  PTR base;
  
  /* Real base of the segment.  This segment recovers this area:
     [real_base, real_base + length).  It is usually equal to base, except
     for a growing down stack segment.  In this case, real_base is the stack
     pointer and base is the pointer to the first byte reachable by the bitmap.
     The memory containing the bitmap is allocated by pages.  Base points to
     the first byte which correspond to the first byte of the bitmap.
     Is it clear ???  */
  PTR real_base;
  
  /* Length of the segment in bytes.  */
  uint length;
  
  /* Base of the bitmap.  */
  uchar *bmbase;
  
  /* Type of the segment.  */
  MapType type;
  
  /* Name of the segment.  */
  const char *name;
  
  /* Global right such as executable.  The meaning depends on the type.  */
  uint right;
  
  /* Function to call to describe the error.  */
  void (*efunc) (struct BitMem const *bm, struct BitMemInfo *bmi);
  
  /* Function to call to identify the error.  */
  int (*idfunc) (struct BitMem const *bm, struct BitMemInfo *bmi);
  
  /* If the previous infos are not enough, you can use this:  */
  union
  {
    struct Mmapinfo *mmapinfo;		/* for SegMmap */
    struct Shminfo *shminfo;		/* for SegShm */
    struct Heapinfo *heapinfo;		/* for heap */
    struct Pageinfo *pageinfo;		/* for SegPage */
  } info;
  
#ifdef CHKR_PROFILE
  /* Just a profile information:  how many times the bitmap was handled.  It
     is set by SetBitMem.  */
  uint addressed;
#endif
} MapInfo;

/* The Mmap and shm zone have no bitmaps but some other informations:
   REAL_LENGTH is the length specified by the user; the OS have to round
    the length owing to pages size.
   HISTORY points to a zone where the stack frames are written (usually just
    after these structures). 
   RIGHT is the right the user want.  */
struct Mmapinfo
{
  uint real_length;
  void *history;
};

struct Pageinfo
{
  uint real_length;
  PTR history;
};

struct Shminfo
{
  uint real_length;
  void *history;
  int shmid;
};

struct Heapinfo
{
 struct mdesc *mdp;
};

/* This structure is filled by SetBitMem, which `converts' an address
   into a bitmap and an offset in this bitmap.  */
struct BitMem
{
  /* Offset in the segment.  Rougly speaking, it is addr - base.  */
  uint seg_offset;
  
  /* Offset in the bitmap.  */
  uint bm_offset;
  
  /* Offset in the byte of the bitmap.  Always between 0 and 3.  */
  uint bit_offset;
  
  /* Number of bytes to the end.  */
  uint length;
  
  /* Description of the bitmap.  */
  MapInfo *info;
};

/* This structure is filled by chkr_check_addr when an error is found.
   Some field are rather fuzzy and not always needed.  */
struct BitMemInfo
{
  /* The memory address.  * */
  PTR ptr;
  
  /* The request right.  * */
  int arq;
  
  /* Number of bytes to the end (?).  */
  int remain;
  
  /* Number of bytes accessed.  */
  int size;
  
  /* The bad val.  * */
  int val;
};

/* Contains ranges of disabled addresses.  This forms a simply linked list.  */
struct range_disable
{
 /* If the PC of the instruction which caused the error is in [first; last],
    the error is silently ignored.  */
 uint first;
 uint last;
 struct range_disable *next;
};
static struct range_disable *disable;

/* Simply linked list which describes ignored errors due to the history.
   Each element contains an history.  */
struct func_suppress
{
 /* Type of the error that can be ignored by this history.  See error.c  */
 int type;
 
 /* Number of function in this history.  */
 int nbr_funcs;
 
 /* Next member.  */
 struct func_suppress *next;
 
 /* This is the history.  1 can be replaced by nbr_funcs.  History consists
    in (function) names.  */
 char *funcs[1];
};
static struct func_suppress *suppress;

/* A pointer to the maps descriptors table.  */
static MapInfo *Fmapinfo;

/* Current number of descriptors.  */
static uint nbr_mapinfo;

/* Maximum number of descriptors before having to call sys_realloc().  */
static uint max_mapinfo;

/* A pointer to the desciptors of always-defined maps.  */
static MapInfo **mapinfo;

/* Here are the offset of these always-defined maps.  */
#define NULLBM  0
#define TEXTBM  1
#define DATABM  2
#define STACKBM 3

/* A special descriptor returned by SetBitMem when an address is bad (ie: it
   cannot be found in any map).  */
static MapInfo null_mapinfo = 
		{(PTR) 0, (PTR) 0, 0, (uchar *) 0, SegFinish, M_NO_MEM};

/* Values that define bitmaps.  See init_morecore().
   To increase speed (?), they are a little redondant.
   bbm stands for byte of bitmap.  */
/* Examples:
   byte_per_state:		1	2	4
   bytes_per_state_round:	0	1	3
   log_bytes_per_state:		0	1	2
   bytes_per_bbm:		4	8	16
   bytes_per_bbm_round:		3	7	15
   log_bytes_per_bbm:		2	3	4
   states_per_bbm:		4	4	4
   states_per_bbm_round:	3	3	3
 */

/* To speed up Checker, you can do:
   #define bytes_per_state 1
   In this case, use precomputed values.  */
#ifdef bytes_per_state
/* Be sure that bytes_per_state = 1 */
#undef bytes_per_state
#define bytes_per_state 1
#define bytes_per_state_round 0
#define log_bytes_per_state 0
#define bytes_per_bbm_round 3
#define bytes_per_bbm 4
#define log_bytes_per_bbm 2
#else
/* Number of byte(s) regrouped by a state.  Only 1 is tested.
   bytes_per_state = 1 << log_bytes_per_state.  */
unsigned int bytes_per_state = 1;

/* bytes_per_state_round = bytes_per_state - 1.  */
unsigned int bytes_per_state_round;

/* Log of bytes_per_state.  */
static int log_bytes_per_state = 0;

/* Number of bytes per byte of bitmap -1.
   bytes_per_bbm_round = 1 << log_bytes_per_bbm - 1.  */
static int bytes_per_bbm_round;

/* Number of bytes per byte of bitmap. 
   bytes_per_bbm = 1 << log_bytes_per_bbm.  */
static int bytes_per_bbm;

/* Log number of bytes per byte of bitmap. 
   log_bytes_per_bbm = log_bytes_per_state + 2.  */
static int log_bytes_per_bbm;
#endif

/* Number of states per bbm.  Since bytes are 8 bits and a state use 2 bits,
   states_per_bbm = 4.  */
#if 0
static int states_per_bbm = 4;
static int states_per_bbm_round = 3;
#else
#define states_per_bbm 4
#define states_per_bbm_round 3
#endif

/* Bit tabs used by chkr_set_right and chkr_check_addr.  */
static const uchar tab_beg_offset[4] =
 {0x00, 0x54, 0x50, 0x40};
static const uchar tab_end_offset[4] =
 {0x00, 0x01, 0x05, 0x15};
static const uchar tab_mask_offset[5] =
 {0xff, 0xfc, 0xf0, 0xc0, 0x00};
static const uchar val_right[4] = 
 {0x55 * CHKR_UN, 0x55 * CHKR_RO, 0x55 * CHKR_WO, 0x55 * CHKR_RW};
static const uchar tab_offset[4] =
 {0x03, 0x0c, 0x30, 0xc0};

/* Optimized version for `mask * right' if right is a real right (ie a 2 bit
   value.  */
#if 1
#define RIGHT_MUL(mask, right) \
   ((right & 1 ? mask : 0) | (right & 2 ? mask << 1 : 0))
#else
#define RIGHT_MUL(mask, right) (mask * right)
#endif

/* The current stack pointer.  It is ajusted by the stubs.  */
PTR known_stack_limit;

/* The limit of the bitmap for the stack.  The stub can reduce this value if
   the stack use less space.  However, only adjust_bm() can expand it.  */
PTR stack_bitmapped;

/* The function called by chkr_check_addr() when an error has been found.  */
static int chkr_access_error (const PTR ptr, int arq, struct BitMem const *bm,
			      char val, int size, int len);

/* The function which can expand the stack bitmap.  */
static void adjust_bm (void);

#ifdef CHKR_STACKBITMAP
/* Alloc space for the stack bitmap.  */
void adjust_stackbitmap (PTR ptr);
void adjust_bm_when_stack_reduce (void);

/* Function called by chkr_access_error() to give more informations.  */
static void chkr_access_error_stack (struct BitMem const *bm, 
                                    struct BitMemInfo *bmi);
#endif /* CHKR_STACKBITMAP */

/* Functions called by chkr_access_error() to give more informations.  */
static void chkr_access_error_heap (struct BitMem const *bm, 
				   struct BitMemInfo *bmi);
static void chkr_access_error_mmap (struct BitMem const *bm,
                                   struct BitMemInfo *bmi);
#ifdef HAVE_SHM
static void chkr_access_error_shm (struct BitMem const *bm,
                                  struct BitMemInfo *bmi);
#endif
static void chkr_access_error_null (struct BitMem const *bm,
                                   struct BitMemInfo *bmi);
static void chkr_access_error_text (struct BitMem const *bm,
                                   struct BitMemInfo *bmi);
static void chkr_access_error_object (struct BitMem const *bm, 
				   struct BitMemInfo *bmi);
				   
#ifdef CHKR_PROFILE
/* Number of times adjust_bm() was called.  */
static uint adjust_bm_called = 0;
#endif

#if 0 
/* Use chkr_stack_pointer instead.  */
void
chkr_adjust_stack (void)
{
#ifdef CHKR_STACKBITMAP
#ifdef SET_KNOWN_STACK_LIMIT
  SET_KNOWN_STACK_LIMIT;
#endif
  if (known_stack_limit != stack_bitmapped)
    {
      if (known_stack_limit < stack_bitmapped)
        adjust_bm ();
      else
        adjust_bm_when_stack_reduce ();
    }
#endif
}
#endif

/* Tell Checker the value of the stack pointer.  Used to update the stack
   bitmap.  */
void
chkr_stack_pointer (PTR sp)
{
#ifdef CHKR_STACKBITMAP
  known_stack_limit = sp;
  if (known_stack_limit != stack_bitmapped)
    {
      if (known_stack_limit < stack_bitmapped)
        adjust_bm ();
      else
        adjust_bm_when_stack_reduce ();
    }
#endif
}

/* Returns informations in BITMEM about ADDR: which bitmap, what offset... */
static void
SetBitMem (const PTR addr, struct BitMem *bitmem)
{
  register MapInfo *tmpinfo = Fmapinfo;

#ifdef CHKR_STACKBITMAP
#ifdef SET_KNOWN_STACK_LIMIT
  SET_KNOWN_STACK_LIMIT;
#endif
  if (known_stack_limit != stack_bitmapped)
    {
      if (known_stack_limit < stack_bitmapped)
        adjust_bm ();
      else
        adjust_bm_when_stack_reduce();
    }
#endif

  /* Search the good descriptor.  */
  while (tmpinfo->type != SegFinish)
    {
      /* Test if ADDR is inside the zone specify by TMPINFO.  */
      if (!(addr >= tmpinfo->real_base 
          && addr < (tmpinfo->base + tmpinfo->length)))
        {
	  tmpinfo++;
	  continue;
        }
      /* Fill the fields.  */
      bitmem->seg_offset = addr - tmpinfo->base;
      bitmem->length = tmpinfo->length - bitmem->seg_offset;
      bitmem->bit_offset = (bitmem->seg_offset & bytes_per_bbm_round) >> log_bytes_per_state;
      bitmem->bm_offset = bitmem->seg_offset >> log_bytes_per_bbm;
      bitmem->info = tmpinfo;
#ifdef CHKR_PROFILE
      tmpinfo->addressed++;
#endif
      return;
    }
  /* Not found.  */
  bitmem->info = &null_mapinfo;
  return;
}

/* Adjust all bitmaps (in fact, the stack bitmap).
   Alloc more memory for the bitmap if needed.
   Set the rights.
   This is based on known_stack_limit and stack_bitmapped.
   Example:
               00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
   0xffffac00: ?? ?? ?? ?? ?? ?? ?? ?? -- -- -- -- RW RW RW RW
                           |           +-------------------- stack_bitmapped
                           +-------------------------------- known_stack_limit
   ->          ?? ?? ?? ?? WO WO WO WO -- -- -- -- RW RW RW RW
                           +-------------------------------- stack_bitmapped
                           +-------------------------------- known_stack_limit
   After this function, stack_bitmapped = known_stack_limit.  */
static void
adjust_bm (void)
{
#ifdef CHKR_STACKBITMAP
  /* Set correctly the stack bitmap.  */
  int bit_off1;
  int bit_off2;
  unsigned char *bm_offset2;
  unsigned char *bm_offset1;

#ifdef CHKR_PROFILE
  adjust_bm_called++;	/* For profile infos.  */
#endif

  /* Alloc (or dealloc) space for the bitmap.  */
  /* FIXME: avoid to call this function if not necessary.  */
#ifdef SET_KNOWN_STACK_LIMIT
  SET_KNOWN_STACK_LIMIT;
#endif  
  adjust_stackbitmap (known_stack_limit);
  
  /* Some check that could be removed later.  */
#ifdef STACK_GROWS_DOWNWARD
  if (known_stack_limit > stack_bitmapped)
    chkr_abort ();
#else
  if (known_stack_limit < stack_bitmapped)
    chkr_abort ();
#endif
      
  /* Compute the offsets.  */
  /* bit_off are really useful if:
     stack pointer can be not aligned.
     bytes_per_state > 1.  */
  bit_off1 = (((int) stack_bitmapped) >> log_bytes_per_state) & states_per_bbm_round;
  bit_off2 = (((int) known_stack_limit) >> log_bytes_per_state) & states_per_bbm_round;
  bm_offset2 = (uchar*)MM_STACK - 1 -
	((STACK_BASE - (int) stack_bitmapped) >> log_bytes_per_bbm);
  bm_offset1 = (uchar*)MM_STACK - 1 -
	((STACK_BASE - (int) known_stack_limit) >> log_bytes_per_bbm);
	
#ifdef STACK_GROWS_DOWNWARD
  if (bm_offset1 == bm_offset2 && bit_off1 && bit_off2)
    {
      /* Only one byte of the bitmap must be modified.  */
      /* We are sure that bit_off2 < bit_off1.  */
      *bm_offset2 &= ~(tab_mask_offset[bit_off2] - tab_mask_offset[bit_off1]);
      *bm_offset2 |= (tab_mask_offset[bit_off2] - tab_mask_offset[bit_off1])
      		     & val_right[CHKR_WO];
    }
  else
    {
      if (bit_off1 != 0)
        {
          *bm_offset2 &= tab_mask_offset[bit_off1];
          *bm_offset2 |= RIGHT_MUL (tab_end_offset[bit_off1], CHKR_WO);
          bm_offset2--;
        }
      /* Set the rights.  */
      if (bm_offset2 > bm_offset1)
        memset (bm_offset1, val_right[CHKR_WO], (int) bm_offset2 - (int) bm_offset1 + 1);
      if (bit_off2 != 0)
        {
          *bm_offset1 &= ~tab_mask_offset[bit_off2];
          *bm_offset1 |= RIGHT_MUL (tab_beg_offset[bit_off2], CHKR_WO);
        }
    }
#else
#error STACK_GROWS_UPWARD is not yet supported
#endif
    
  mapinfo[STACKBM]->real_base = known_stack_limit;
  stack_bitmapped = known_stack_limit;
#endif
}

/* Called by codecheck.S FIXME.  */
void
adjust_bm_when_stack_reduce (void)
{
#if CHKR_STACKBITMAP
  mapinfo[STACKBM]->real_base = known_stack_limit;
  stack_bitmapped = known_stack_limit;
#endif
}

/* Read the bitmap.  API function.
   Read at most LEN byte states for address PTR and put the result in BM.
   The user must provide enough place in BM.  This is not checked.
   Each char of BM contains a byte right (--, RO, WO, RW).
   Example: Suppose we have this bitmap:
               FE FF 00 01 02 03 04 05
   0x0001CAFE: WO WO RW RW RW RW -- -- ...
   __chkr_read_bitmap (0x1CAFF, array, 6) will return 6 and array will
   contains { 2, 3, 3, 3, 3, 0}.
   Of course, if bytes_per_state is 4, four consecutives (and aligned) bytes 
   of BM have the smae value, ie, __chkr_read_bitmap de-compacts the bitmap.
  
   Return values:
   Error: -1: bad address (no bitmap)
   Success: the number of states written in BM (can be 0).  */
int
__chkr_read_bitmap (const PTR ptr, uchar *bm, int len)
{
  struct BitMem bitmem;
  int nlen;			/* Number of rights read.  */
  int i,j,k;
  uint addr;
  uchar *base;			/* Bitmap base.  */
  uint val = 0;			/* Val to set in the bitmap.  */
  uchar *bm_orig = bm;
  int bm_offset;

  /* Be sure Checker can write in the buffer.  */
  chkr_check_addr (bm, len, CHKR_WO);
  
  /* Search info about ptr.  */
  SetBitMem (ptr, &bitmem);
  base = bitmem.info->bmbase;
  
  /* Number of rights.  */
  if (len > bitmem.length)
    len = bitmem.length;
    
  if (len == 0)
    return 0;

  switch (bitmem.info->type)
    {
    /* The rights of these segments can't be changed.  */
    case SegFinish:
    case SegVoid:
      return -1;	/* Bad address.  */
    case SegText:
      if (is_text_writable)
        memset (bm, CHKR_RW, len);
      else
        memset (bm, CHKR_RO, len);
      break;
    case SegMmap:
      memset (bm, bitmem.info->right, len);
      break;
    case SegShm:
      memset (bm, bitmem.info->right, len);
      break;
    case SegRW:
      memset (bm, CHKR_RW, len);
      break;
    case SegROnly:
      memset (bm, CHKR_RO, len);
      break;
    case SegWOnly:
      memset (bm, CHKR_WO, len);
      break;
    case SegNormal:
      nlen = len;
      addr = (uint) ptr & (~bytes_per_bbm_round);	/* round ptr.  */
      bm_offset = bitmem.bm_offset;
      for (i = 0; nlen > 0; i++)
        {
          val = base[bm_offset] + (base[bm_offset + 1] << 8) + (base[bm_offset + 2] << 16)
	    + (base[bm_offset + 3] << 24);
          bm_offset += 4;
          for (j = 0; (j < 16) && (nlen > 0); j++)
            {
              for (k = 0; (k < bytes_per_state) && (nlen > 0); k++)
                {
	          if (addr + k + ((i * 16 + j) << log_bytes_per_state) >= (uint) ptr)
	            {
	              *bm++ = val & CHKR_MASK;
	              nlen--;
	            }
	        }
	      val >>= 2;
	    }
	}
      break;
    case SegPage:
      for (i = 0; i < len; i++)
        *bm++ = base[(ptr - bitmem.info->base) >> LOG_PAGESIZE] & CHKR_MASK;
      break;
    default:
      return -2;
    }
  
  /* Mark the user buffer RW.  */
  chkr_set_right (bm_orig, len, CHKR_RW);
  
  return len;
}

/* Write to the bitmap.  API function.
   Write at most LEN byte states for address PTR from BM.
   See __chkr_read_bitmap.
   Return values:
   Error: -1: bad address
          -2: rights cannot be changed.
   Success: the number of states written in the bitmap.  */
int
__chkr_write_bitmap (const PTR ptr, uchar *bm, int len)
{
  struct BitMem bitmem;
  int n;
  int nlen;			/* Number of rights read.  */
  uchar *base;			/* Bitmap base.  */
  uchar val;			/* Val to set in the bitmap.  */

  /* Check the buffer can be read.  */
  chkr_check_addr (bm, len, CHKR_RO);
  
  /* Search info about ptr.  */
  SetBitMem (ptr, &bitmem);
  base = bitmem.info->bmbase;
  
  if (len > bitmem.length)
    nlen = bitmem.length;
  else
    nlen = len;
    
  if (nlen == 0)
    return 0;

  switch (bitmem.info->type)
    {
    /* The rights of these segments can't be changed.  */
    case SegFinish:
    case SegVoid:
      return -1;	/* Bad address.  */
    case SegText:
    case SegMmap:
    case SegShm:
    case SegRW:
    case SegROnly:
    case SegWOnly:
    case SegPage:
      return -2;	/* cannot be changed.  */
    case SegNormal:
      len = nlen;
      while (nlen > 0)
        {
          val = 0;
          n = 0;
          do
            {
              val |= *bm;
              bm++;
              nlen--;
              ptr++;
              n++;
            }
          while (nlen && ((uint)ptr & bytes_per_state_round));
          val &= CHKR_MASK;
          val <<= 2 * bitmem.bit_offset;
          if (n == bytes_per_state)
            base[bitmem.bm_offset] &= ~tab_offset[bitmem.bit_offset];
          base[bitmem.bm_offset] |= val;
          /* Update for the next right */
          if (bitmem.bit_offset == 3)
            {
              bitmem.bit_offset = 0;
              bitmem.bm_offset++;
            }
          else
            bitmem.bit_offset++;
        }
      return len;
    default:
      return -2;
    }
}

/* Used by chkr_qsort of __chkr_disp_map().
   Sort the buffer according to the offset.  */
static int
map_compare (const void *a, const void *b)
{
  if ( (*((MapInfo**)a))->base > (*((MapInfo**)b))->base )
    return 1;
  else if ( (*((MapInfo**)a))->base == (*((MapInfo**)b))->base )
    return 0;
  else
    return -1;
}

/* Disp the memory map.  */
void
__chkr_disp_map (void)
{
  static char *right_name[] = { "--", "r-", "-w", "rw"};
  static char *type_name[] = { "Finish", "Void", "Text", "Normal", "RW",
  			"ROnly", "WOnly", "Mmap", "Shm", "Page"};
  MapInfo **buf;
  int i;
  uint len;
  int nbr_map = nbr_mapinfo - 1;
  PTR base = (PTR)0;
  
  /* Fill a tab with the addresses of the segments and sort it.  */
  buf = (MapInfo**) sys_malloc (nbr_map * sizeof (MapInfo*));
  for (i = 0; i < nbr_map; i++)
    buf[i] = &Fmapinfo[i];
  chkr_qsort (buf, nbr_map, sizeof(MapInfo*), map_compare);
  
  chkr_report (M_C_MES_CK_ET);
  chkr_printf ("Memory map:\n");
  chkr_printf ("Name           | Base       | End        | Size        | Right | Type\n");
  chkr_printf ("---------------+------------+------------+-------------+-------+-----\n");
  for (i = 0; i < nbr_map; i++)
    {
      if (buf[i]->real_base > base)
        {
          len = (uint)buf[i]->real_base - (uint)base;
          chkr_printf ("   (nothing)   | 0x%-8x | 0x%-8x | ",
                  (int) base,
                  (int) buf[i]->real_base - 1);
          if (len < 2 * 1024)
            chkr_printf ("%8d  b | ---   | -\n", len);
          else if (len < 2 * 1024 * 1024)
            chkr_printf ("%8d kb | ---   | -\n", len / 1024);
          else chkr_printf ("%8d Mb | ---   | -\n", len / (1024 * 1024));
        }
      base = buf[i]->base + buf[i]->length;
      len = (uint)(buf[i]->base + buf[i]->length) - (uint)buf[i]->real_base;
      chkr_printf ("%14s | 0x%-8x | 0x%-8x | ", 
                  buf[i]->name,
                  (int)buf[i]->real_base, 
                  (int)buf[i]->base + buf[i]->length - 1);
      if (len < 2 * 1024)
        chkr_printf ("%8d  b ", len);
      else if (len < 2 * 1024 * 1024)
        chkr_printf ("%8d kb ", len / 1024);
      else chkr_printf ("%8d Mb ", len / (1024 * 1024));
      chkr_printf ("| %s%c   | %s\n", 
                  right_name[buf[i]->right & CHKR_MASK],
                  buf[i]->right & CHKR_EXEC ? 'x' : '-',
                  type_name[buf[i]->type]);
    }
  sys_free ((PTR) buf);
}

/* Return the total size of the memory used.  */
uint
get_total_mem (void)
{
  uint total = 0;
  int i;
  
  for (i = 0; i < nbr_mapinfo; i++)
    {
      total += Fmapinfo[i].length 
               - ((uint)Fmapinfo[i].real_base - (uint)Fmapinfo[i].base);
    }
  return total;
}

/* Disp the bitmap from addr ptr.  LEN is the number of bytes.  */
void
__chkr_disp_right (const PTR ptr, int len)
{
  struct BitMem bitmem;
  int bm_offset;
  uchar *base;
  int val;
  int i, j;
  uint addr = (int) ptr & (~bytes_per_bbm_round);	/* round ptr.  */

  chkr_report (M_C_MES_CK_ET);
  
  /* Search info about the bitmap.  */
  SetBitMem (ptr, &bitmem);	/* (PTR) addr.  */
  base = bitmem.info->bmbase;
  bm_offset = bitmem.bm_offset;

  /* Skip bad addresses.  */
  if (base == (uchar *) 0)
    {
      chkr_printf (M_NO_BM_4ADDRESS);
      return;
    }
  switch (bitmem.info->type)
    {
    case SegNormal:
#if 0    
      if (bitmem.bit_offset != 0)
	chkr_printf (M_BO_NOT_ALIGNED);
#endif
      
      /* Print the header.  */
      chkr_printf ("          ");
      for (i = 0; i < 16; i++)
	chkr_printf ("%02x ", ((addr & 0x0f) + (i << log_bytes_per_state)) &
		     (0x0f << log_bytes_per_state));
      chkr_printf ("\n");
      
      for (i = 0; len > 0; i++)
	{
	  /* Address of the line.  */
	  chkr_printf ("%08x: ", addr + i * (16 << log_bytes_per_state));
	  /* Bitmap.  */
	  /* val = ((unsigned int*)base)[bm_offset]; */
	  val = base[bm_offset] + (base[bm_offset + 1] << 8) + (base[bm_offset + 2] << 16)
	    + (base[bm_offset + 3] << 24);
	  bm_offset += 4;
	  for (j = 0; j < 16; j++)
	    {
	      if (addr + ((i * 16 + j) << log_bytes_per_state) >= (uint) ptr)
		{
		  chkr_printf ("%s ", short_right_name[val & CHKR_MASK]);
		  len--;
		}
	      else
		chkr_printf ("?? ");
	      val >>= 2;
	    }
	  chkr_printf ("\n");
	}
      break;
    case SegPage:
      /* Print the header.  */
      chkr_printf ("          ");
      for (i = 0; i < 16; i++)
	chkr_printf (" %1x  ", i);
      chkr_printf ("\n");
      
      for (i = 0; len > 0; i++)
	{
	  /* Address of the line.  */
	  chkr_printf ("%08lx: ", (daddr_t) bitmem.info->base + i * (16 << LOG_PAGESIZE));
	  for (j = 0; j < 16; j++)
	    if (i * 16 + j < (bitmem.info->length >> LOG_PAGESIZE))
	      {
		val = base[i * 16 + j];
		chkr_printf ("%s%c ", short_right_name[val & CHKR_MASK],
			     val & CHKR_EXEC ? 'X' : '-');
	      }
	    else
	      chkr_printf ("??? ");
	  chkr_printf ("\n");
	  len -= 16;
	}
      break;
    default:
      if (bitmem.info->type != SegNormal)
	{
	  chkr_printf (M_NO_BM_4ADDRESS);
	  return;
	}
    }
}

/* Test the execution right.  */
void
chkr_check_exec (const PTR ptr)
{
  struct BitMem bitmem;
  
  SetBitMem (ptr, &bitmem);

  switch (bitmem.info->type)
    {
    case SegNormal:
    case SegROnly:
    case SegWOnly:
    case SegRW:
    case SegMmap:
      if (!(bitmem.info->right & CHKR_EXEC))
        break;
      return;
        
    case SegFinish:
    case SegVoid:
    case SegShm:
      break;

    case SegText:
      return;			/* always return */
    
    case SegPage:
      {
        char *base;
        /* BASE points to the first byte right.  */
        base = bitmem.info->bmbase + ((ptr - bitmem.info->base) >> LOG_PAGESIZE);
      
        if ((*base & CHKR_EXEC) != CHKR_EXEC)
          break;
      }
      return;
      
    default:
      chkr_abort ();
    }
  if (chkr_report (M_E_ZNE_VD_ET))
    {
      chkr_printf (M_TRY_TO_EXECUTE, ptr, bitmem.info->name);
      chkr_printf (M_PROD_SEG_FAULT);
      chkr_disp_call_chain ();
    }
}

/* Optimized version for `mask * right' if right is a real right (ie a 2 bit
   value.  */
#if 1
#define RIGHT_MUL(mask, right) \
   ((right & 1 ? mask : 0) | (right & 2 ? mask << 1 : 0))
#else
#define RIGHT_MUL(mask, right) (mask * right)
#endif

/* Set the right of a zone of length LEN, starting at PTR.  */
void
chkr_set_right (const PTR ptr, int len, int right)
{
  struct BitMem bitmem;
  uchar *base;			/* Bitmap base.  */
  uchar val = 0;		/* Val to set in the bitmap.  */
  uchar mask = 0;		/* Mask corresponding to the val.  */
  int round_flag = 0;

  /* Search info about ptr */
  SetBitMem (ptr, &bitmem);
  base = bitmem.info->bmbase;

  /* Be sure about right.  Right is 2 bits length. */
  right &= CHKR_MASK;

#ifdef NEED_CHKR_LD_OPTIONS
  /* When the program contains standard file, Checker is B&W !  */
  if (chkr_ld_options & 1)
    right = (right != CHKR_UN) ? CHKR_RW : CHKR_UN;
#endif

  switch (bitmem.info->type)
    {
    /* The rights of these segments can't be changed.  */
    case SegFinish:
    case SegVoid:
    case SegText:
    case SegMmap:
    case SegShm:
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_ERR_SET_RIGHT);
      return;
    case SegRW:
      return;
    case SegROnly:
      if (right & CHKR_WO)
        {
          chkr_report (M_C_MES_CK_ET);
	  chkr_printf (M_ERR_SET_RIGHT);
	}
      return;
    case SegWOnly:
      if (right & CHKR_RO)
        {
          chkr_report (M_C_MES_CK_ET);
	  chkr_printf (M_ERR_SET_RIGHT);
	}
      return;
    case SegNormal:
      if (len > bitmem.length || len == 0)
	{
	  chkr_report (M_C_MES_CK_ET);
	  chkr_printf (M_BAD_LEN_IN_SR);
#if 1
	  chkr_printf ("len=%d, bitmem.length=%d\n", len, bitmem.length);
	  __chkr_disp_map ();
	  chkr_abort ();	/* FIXME */
#endif
	  return;
	}
      
      /* Set round_flag.  */
      if ((uint)ptr & bytes_per_state_round)
        round_flag |= 1;
      if (((uint)ptr + len) & bytes_per_state_round)
        round_flag |= 2;
        
      /* Convert a number of bytes to a number of states.  */
      len = (len + ((uint)ptr & bytes_per_state_round) + bytes_per_state_round)
            >> log_bytes_per_state;
      
      /* The processus is split into 3 parts: the first byte of rights,
         the middle and the last.  This is due to alignment.  */
      /* The first byte of right.  */
      if (bitmem.bit_offset > 0)
	{
	  /* Len must be divided.  */
	  if (len + bitmem.bit_offset < states_per_bbm)
	    {
	      /* Only one byte of right.  */
	      val = RIGHT_MUL ((tab_beg_offset[bitmem.bit_offset] -
		     tab_beg_offset[bitmem.bit_offset + len]), right);
	      mask = tab_mask_offset[bitmem.bit_offset] -
		      tab_mask_offset[bitmem.bit_offset + len];
              if (round_flag & 1)
                mask &= tab_mask_offset[bitmem.bit_offset + 1];
              if (round_flag & 2)
                mask &= ~tab_mask_offset[bitmem.bit_offset + len - 1];
	      len = 0;		/* finish */
	    }
	  else
	    {
	      val = RIGHT_MUL (tab_beg_offset[bitmem.bit_offset], right);
	      mask = RIGHT_MUL (tab_beg_offset[bitmem.bit_offset], CHKR_RW);
	      if (round_flag & 1)
	        mask &= tab_mask_offset[bitmem.bit_offset + 1];
	      len -= states_per_bbm - bitmem.bit_offset;
	    }
	  mask = ~mask;
	  base[bitmem.bm_offset] &= mask;
	  base[bitmem.bm_offset] |= val;
	  /* If finish, then return.  */
	  if (len <= 0)
	    return;
	  /* next byte to set */
	  bitmem.bm_offset++;
	}
	
      /* Now, we set bytes of bitmap.  So len is decrease by bytes_per_bbm.  */
      if (len > states_per_bbm_round)
	{
	  val = val_right[right]; /* = 0x55 * right; */
	  while (len > states_per_bbm_round)
	    {
	      base[bitmem.bm_offset++] = val;
	      len -= states_per_bbm;
	    }
	  if (len <= 0)
	    return;
	}
      /* Now, the end.  Set the last byte of bitmap.  */
      val = RIGHT_MUL (tab_end_offset[len], right);
      mask = RIGHT_MUL (tab_end_offset[len], CHKR_RW);
      mask = ~mask;
      base[bitmem.bm_offset] &= mask;
      base[bitmem.bm_offset] |= val;
      return;
      
    case SegPage:
      /* FIXME.  */
      return;
    }
  /* NOT REACHED.  */
  chkr_abort ();
}


/* Check the right of a zone of length len, starting at ptr.  */
/*				rights in bitmap
 *                   +-----------+-------------+-------------+-------------+
 *                   |  CHKR_UN  |   CHKR_RO   |   CHKR_WO   |   CHKR_RW   |
 *       +-----------+-----------+-------------+-------------+-------------+
 *  r    |  CHKR_UN  |                   not accepted                      |
 *  i    +-----------+-----------+-------------+-------------+-------------+
 *  g    |  CHKR_RO  |   error   |     OK      |    error    |     OK      |
 *  h    +-----------+-----------+-------------+-------------+-------------+
 *  t    |  CHKR_WO  |   error   |    error    | -> CHKR_RW  |     OK      |
 *       +-----------+-----------+-------------+-------------+-------------+
 *  t    |  CHKR_TW  |   error   |    error    |     OK      |     OK      |
 *  e    +-----------+-----------+-------------+-------------+-------------+
 *  s    |  CHKR_RW  |   error   |    error    |    error    |     OK      |
 *  t    +-----------+-----------+-------------+-------------+-------------+
 *  e    |  CHKR_MW  |   error   |    error    |     OK      | -> CHKR_WO  |
 *  d    +-----------+-----------+-------------+-------------+-------------+
 *
 *  Well, optimized version of this function must be written.
 */
void
chkr_check_addr (const PTR ptr, int len, int right1)
{
  struct BitMem bitmem;
  uchar *base;			/* Bitmap base.  */
  uchar val = 0;		/* Val to set in the bitmap.  */
  uchar mask;			/* mask for evolution.  */
  int size = len;		/* save.  */
  int right;			/* Good Value of right1.  */

  /* Be sure about right.  Right is 2 bits length.  */
  right = right1 & CHKR_MASK;

  /* Checks for stupidities.  Could be removed.  */
  if (right == 0)
    {
      chkr_perror (M_I_IEB_MA_ET); /* Bad check, must never happen.  */
      chkr_printf ("Bad right for ptr=%p, len=%d, right=%d\n", ptr, len, right1);
      chkr_abort ();
    }

#if 0
  /* Verbose... */
  if (flag_verbose)
    {
      chkr_printf ("chkr_check_addr (0x%08x, %d, %d)\n", (uint)ptr, len, right1);
      __chkr_disp_right (ptr, len);
    }
#endif

  /* This test must be done only for syscalls, otherwise it is an error.  */
  if (len == 0)
    {
      chkr_perror (M_I_IEB_MA_ET);
      chkr_printf ("Bad length for ptr=%p, len=%d, right=%d\n", ptr, len, right1);
      return;
    }
  
  /* Search info about ptr.  */
  SetBitMem (ptr, &bitmem);
  base = bitmem.info->bmbase;

  switch (bitmem.info->type)
    {
    case SegNormal:
      if (len > bitmem.length)
        {
          chkr_perror (M_A_SBV_SG_ET);
          return;
        }
        
      /* Convert a number of bytes to a number of states.  */
      len = (len + ((uint)ptr & bytes_per_state_round) + bytes_per_state_round)
            >> log_bytes_per_state;
      
      if (bitmem.bit_offset > 0)
	{
	  /* Len must be divided.  */
	  if (len + bitmem.bit_offset < states_per_bbm)
	    {
	      val = RIGHT_MUL ((tab_beg_offset[bitmem.bit_offset] -
		     tab_beg_offset[bitmem.bit_offset + len]), right);
	      mask = (tab_beg_offset[bitmem.bit_offset] -
		      tab_beg_offset[bitmem.bit_offset + len]);
	      len = 0;		/* finish */
	    }
	  else
	    {
	      val = RIGHT_MUL (tab_beg_offset[bitmem.bit_offset], right);
	      mask = tab_beg_offset[bitmem.bit_offset];
	      len -= states_per_bbm - bitmem.bit_offset;
	    }
	  if ((base[bitmem.bm_offset] & val) != val)
	    {
	      /* len = size for chkr_access_error(), since len was decreased
                 too early.  */
	      if (chkr_access_error (ptr, right, &bitmem, val, size, size))
		return;		/* Check fails.  */
	    }
	  /* Handle the evolutions.  */
	  if (right1 == CHKR_WO)
	    base[bitmem.bm_offset] |= RIGHT_MUL (mask, CHKR_RW);
	  else if (right1 == CHKR_MW)
	    {
	      base[bitmem.bm_offset] &= ~RIGHT_MUL (mask, CHKR_RW);
	      base[bitmem.bm_offset] |= RIGHT_MUL (mask, CHKR_WO);
	    }
	  /* If finish, then return.  */
	  if (len <= 0)
	    return;
	  /* Next byte to set.  */
	  bitmem.bm_offset++;
	}
      /* Now, we set bytes of bitmap.  So len is decrease by bytes_per_bbm.  */
      if (len >= states_per_bbm)
	{
	  val = val_right[right]; /* = 0x55 * right; */
	  while (len >= states_per_bbm)
	    {
	      if ((base[bitmem.bm_offset] & val) != val)
		{
		  if (chkr_access_error (ptr, right, &bitmem, val, size, len << log_bytes_per_state))
		    return;	/* Check fails.  */
		}
	      if (right1 == CHKR_WO)
		base[bitmem.bm_offset] |= RIGHT_MUL (0x55, CHKR_RW);
              else if (right1 == CHKR_MW)
                base[bitmem.bm_offset] = RIGHT_MUL (0x55, CHKR_WO);
	      bitmem.bm_offset++;
	      len -= states_per_bbm;
	    }
	  if (len <= 0)
	    return;
	}
      /* Now, the end.  Set the last byte of bitmap.  */
      val = RIGHT_MUL (tab_end_offset[len], right);
      if ((base[bitmem.bm_offset] & val) != val)
	{
	  if (chkr_access_error (ptr, right, &bitmem, val, size, len << log_bytes_per_state))
	    return;		/* Check fails.  */
	}
      if (right1 == CHKR_WO)
	base[bitmem.bm_offset] |= RIGHT_MUL (tab_end_offset[len], CHKR_RW);
      else if (right1 == CHKR_MW)
        {
          base[bitmem.bm_offset] &= ~RIGHT_MUL (tab_end_offset[len], CHKR_RW);
          base[bitmem.bm_offset] |= RIGHT_MUL (tab_end_offset[len], CHKR_WO);
        }
      return;

    case SegFinish:
    case SegVoid:
#if 0
      if (flag_verbose)
        chkr_printf("SegFinish/SegVoid\n");
#endif
      chkr_access_error (ptr, right, &bitmem, 0, size, 0);
      return;			/* Always return.  */

    case SegText:
      if (!is_text_writable)
	if (right != CHKR_RO)
	  chkr_access_error (ptr, right, &bitmem, RIGHT_MUL (CHKR_RO, 0x55), size, 0);
      return;			/* Always return.  */

    case SegRW:
      return;
      
    case SegMmap:
      {
        int real_size = size;
        int len_bis = len;
        PTR real_ptr = (PTR)ptr;
        
        while (len > 0)
          {
            if (len > bitmem.length)
              len = bitmem.length;
            if (ptr >= bitmem.info->base + bitmem.info->info.mmapinfo->real_length)
              chkr_access_error (real_ptr, right, &bitmem, 0, real_size, 0);
            if ((right & bitmem.info->right) != right)
              chkr_access_error (real_ptr, right, &bitmem, 0, real_size, 0);
            len_bis -= bitmem.length;
            len = len_bis;
            ptr += bitmem.length;
            if (len > 0)
              SetBitMem(ptr, &bitmem);
          }
        return;
      }
    
    case SegShm:
      /* FIXME */
      if ((right & bitmem.info->right) != right)
        chkr_access_error (ptr, right, &bitmem, 0, size, 0);
      return;

    case SegROnly:
      if (right != CHKR_RO)
        chkr_access_error (ptr, right, &bitmem, CHKR_RO * 0x55, size, 0);
      return;			/* Always return.  */
      
    case SegWOnly:
      chkr_abort ();		/* Not implemented.  */
      
    case SegPage:
      /* Round LEN.  We do as if PTR was on a page boundary.  */
      len += (uint)ptr & (CHKR_PAGESIZE - 1);
      
      /* BASE points to the first byte right.  */
      base += (ptr - bitmem.info->base) >> LOG_PAGESIZE;
      
      while (len > 0)
        {
          if ((*base & right) != right)
            chkr_access_error (ptr, right, &bitmem, *base, size, 0);
            
          /* One page was checked.  */
          len -= CHKR_PAGESIZE;
          base++;
        }
      return;
      
    default:
      chkr_abort ();
    }
    
  /* NOT REACHED.  */
  chkr_abort ();
}

/* Check for a string.  Is this implementation poor ?  */
void
chkr_check_str (const PTR ptr, int right)
{
  /* FIXME: bound violations. */
  chkr_check_addr (ptr, strlen (ptr) + 1, right);
}

/* Check the zone doesn't contain any red zones.  */
void
chkr_assert_not_red_zone (const PTR ptr, int len)
{
  struct BitMem bitmem;
  uchar *base;			/* Bitmap base.  */
  uchar val = 0;		/* Val to set in the bitmap.  */
  int size = len;		/* save.  */

  /* This test must be done only for syscalls, otherwise it is an error.  */
  if (len == 0)
    {
      chkr_perror (M_I_IEB_MA_ET);
      chkr_printf ("Bad length for ptr=%p, len=%d, right=%d\n", ptr, len, CHKR_NOT_RZ);
      return;
    }
  
  /* Search info about ptr.  */
  SetBitMem (ptr, &bitmem);
  base = bitmem.info->bmbase;

  switch (bitmem.info->type)
    {
    case SegNormal:
      if (len > bitmem.length)
        {
          chkr_perror (M_A_SBV_SG_ET);
          return;
        }
        
      /* Convert a number of bytes to a number of states.  */
      len = (len + ((uint)ptr & bytes_per_state_round) + bytes_per_state_round)
            >> log_bytes_per_state;
      
      if (bitmem.bit_offset > 0)
	{
	  /* Len must be divided.  */
	  if (len + bitmem.bit_offset < states_per_bbm)
	    {
	      val = RIGHT_MUL ((tab_beg_offset[bitmem.bit_offset] -
		     tab_beg_offset[bitmem.bit_offset + len]), CHKR_RW);
	      len = 0;		/* finish */
	    }
	  else
	    {
	      val = RIGHT_MUL (tab_beg_offset[bitmem.bit_offset], CHKR_RW);
	      len -= states_per_bbm - bitmem.bit_offset;
	    }
	  if (!(base[bitmem.bm_offset] & val))
	    {
	      /* len = size for chkr_access_error(), since len was decreased
                 too early.  */
	      if (chkr_access_error (ptr, CHKR_NOT_RZ, &bitmem, val, size, size))
		return;		/* Check fails.  */
	    }
	  /* If finish, then return.  */
	  if (len <= 0)
	    return;
	  /* Next byte to set.  */
	  bitmem.bm_offset++;
	}
      /* Now, we set bytes of bitmap.  So len is decrease by bytes_per_bbm.  */
      if (len >= states_per_bbm)
	{
	  val = val_right[CHKR_RW]; /* = 0x55 * right; */
	  while (len >= states_per_bbm)
	    {
	      if (!(base[bitmem.bm_offset] & val))
		{
		  if (chkr_access_error (ptr, CHKR_NOT_RZ, &bitmem, val, size, len << log_bytes_per_state))
		    return;	/* Check fails.  */
		}
	      bitmem.bm_offset++;
	      len -= states_per_bbm;
	    }
	  if (len <= 0)
	    return;
	}
      /* Now, the end.  Set the last byte of bitmap.  */
      val = RIGHT_MUL (tab_end_offset[len], CHKR_RW);
      if (!(base[bitmem.bm_offset] & val))
	{
	  if (chkr_access_error (ptr, CHKR_NOT_RZ, &bitmem, val, size, len << log_bytes_per_state))
	    return;		/* Check fails.  */
	}
      return;

    case SegFinish:
    case SegVoid:
#if 0
      if (flag_verbose)
        chkr_printf("SegFinish/SegVoid\n");
#endif
      chkr_access_error (ptr, CHKR_NOT_RZ, &bitmem, 0, size, 0);
      return;			/* Always return.  */

    case SegText:
      return;			/* Always return.  */

    case SegRW:
      return;
      
    case SegMmap:
      {
        int real_size = size;
        int len_bis = len;
        PTR real_ptr = (PTR)ptr;
        
        while (len > 0)
          {
            if (len > bitmem.length)
              len = bitmem.length;
            if (ptr >= bitmem.info->base + bitmem.info->info.mmapinfo->real_length)
              chkr_access_error (real_ptr, CHKR_NOT_RZ, &bitmem, 0, real_size, 0);
            if (!(bitmem.info->right & CHKR_MASK))
              chkr_access_error (real_ptr, CHKR_NOT_RZ, &bitmem, 0, real_size, 0);
            len_bis -= bitmem.length;
            len = len_bis;
            ptr += bitmem.length;
            if (len > 0)
              SetBitMem(ptr, &bitmem);
          }
        return;
      }
    
    case SegShm:
      /* FIXME */
      if (!(bitmem.info->right & CHKR_MASK))
        chkr_access_error (ptr, CHKR_NOT_RZ, &bitmem, 0, size, 0);
      return;

    case SegROnly:
      return;			/* Always return.  */
      
    case SegWOnly:
      chkr_abort ();		/* Not implemented.  */
      
    case SegPage:
      /* Round LEN.  We do as if PTR was on a page boundary.  */
      len += (uint)ptr & (CHKR_PAGESIZE - 1);
      
      /* BASE points to the first byte right.  */
      base += (ptr - bitmem.info->base) >> LOG_PAGESIZE;
      
      while (len > 0)
        {
          if (!(*base & CHKR_MASK))
            chkr_access_error (ptr, CHKR_NOT_RZ, &bitmem, *base, size, 0);
            
          /* One page was checked.  */
          len -= CHKR_PAGESIZE;
          base++;
        }
      return;
      
    default:
      chkr_abort ();
    }
    
  /* NOT REACHED.  */
  chkr_abort ();
}

/* Check PTR is aligned on ALIGN boundary.
   ALIGN must be a power of 2.  */
void
chkr_check_align (const PTR ptr, int align, int size)
{
  if ((uint)ptr & (align - 1))
    {
      chkr_report (M_M_BAL_AL_ET);
      chkr_printf (M_ACC_DATA_ALIGN, (uint)ptr, size, align);
      chkr_printf (M_PROD_BUS_ERR);
      chkr_disp_call_chain ();
    }
}

#if 0	/* To be removed.  Replaced by chkr_copy_bitmap */
/* Copy a part of bitmap.  Must be aligned.
 * This is used by realloc.
 */
void
chkr_copy_aligned_bm (PTR dest, PTR src, uint len)
{
  struct BitMem bmsrc, bmdest;
  static uchar tab_offset[] =
  {0x00, 0x03, 0x0f, 0x3f};
  uint bm_len;

  if (len == 0)
    return;
    
  SetBitMem (src, &bmsrc);
  if (bmsrc.info->type != SegNormal || bmsrc.length < len)
    {
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_SRC_IN_CP_BM);
      return;
    }
  SetBitMem (dest, &bmdest);
  if (bmdest.info->type != SegNormal || bmdest.length < len)
    {
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_DEST_IN_CP_BM);
      return;
    }
  if (bmsrc.bit_offset != 0 || bmdest.bit_offset != 0)
    {
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_ALIGN_IN_CP_BM);
      return;
    }
  bm_len = len / bytes_per_bbm;
  memcpy (&(bmdest.info->bmbase[bmdest.bm_offset]),
	  &(bmsrc.info->bmbase[bmsrc.bm_offset]),
	  bm_len);
  if ((len & bytes_per_bbm_round) == 0)
    return;
  bmdest.info->bmbase[bmdest.bm_offset + bm_len] &= ~(tab_offset[len & bytes_per_bbm_round]);
  bmdest.info->bmbase[bmdest.bm_offset + bm_len] |=
    tab_offset[len & bytes_per_bbm_round] & bmsrc.info->bmbase[bmsrc.bm_offset + bm_len];
}
#endif

/* Copy a part of bitmap.  Can be not aligned.  The source must not contain
   any red-zone, the destination must be writable.  */
void
chkr_copy_bitmap (PTR dest, PTR src, uint orig_len)
{
  struct BitMem bmsrc, bmdest;
  uchar right = 0;
  uchar right0 = 0;
  uint len = orig_len;
  
  if (len == 0)
    return;
  
  SetBitMem (src, &bmsrc);
  if (bmsrc.length < len)
    {
      chkr_printf (M_SRC_IN_CP_BM);
      return;
    }
  switch (bmsrc.info->type)
    {
      case SegFinish:
      case SegVoid:
        chkr_printf (M_SRC_IN_CP_BM);
        return;
      case SegText:
        if (is_text_writable)
          right0 = CHKR_RW;
        else
          right0 = CHKR_RO;
        break;
      case SegRW:
        right0 = CHKR_RW;
        break;
      case SegROnly:
        right0 = CHKR_RO;
        break;
      case SegWOnly:
        right0 = CHKR_WO;
        break;
      case SegMmap:
        right0 = bmsrc.info->right;
        break;
      case SegShm:
        right0 = bmsrc.info->right;
        break;
      case SegNormal:
        break;
      case SegPage:
        break;
      default:
        chkr_abort ();
    }
  right &= CHKR_MASK;
  
  SetBitMem (dest, &bmdest);
  if (bmdest.length < len)
    {
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_DEST_IN_CP_BM);
      return;
    } 
  switch (bmdest.info->type)
    {
      case SegFinish:
      case SegVoid:
        chkr_report (M_C_MES_CK_ET);
        chkr_printf (M_DEST_IN_CP_BM);
        return;
      case SegText:
        if (!is_text_writable)
          {
            chkr_report (M_C_MES_CK_ET);
            chkr_printf (M_DEST_IN_CP_BM);
          }
        return;
      case SegRW:
      case SegWOnly:
      case SegShm:
      case SegMmap:
      case SegROnly:
      case SegPage:
        return;
      case SegNormal:
        break;
    }
  len = (len + ((uint)src & bytes_per_state_round) + bytes_per_state_round)
            >> log_bytes_per_state;
  while (len > 0)
    {
      switch (bmsrc.info->type)
        {
      case SegNormal:
          /* Read a right.  */
          right = bmsrc.info->bmbase[bmsrc.bm_offset];
          right >>= 2 * bmsrc.bit_offset;
          right &= CHKR_MASK;
          /* Update for the next right */
          if (bmsrc.bit_offset == 3)
            {
              bmsrc.bit_offset = 0;
              bmsrc.bm_offset++;
            }
          else
            bmsrc.bit_offset++;
          break;
      case SegPage:
          right = bmsrc.info->bmbase[(src - bmsrc.info->base) >> LOG_PAGESIZE];
          right &= CHKR_MASK;
          break;
      default:
          right = right0;
          break;
        }
      if (!right)
        chkr_access_error (src, CHKR_NOT_RZ, &bmsrc, 0, orig_len, len << log_bytes_per_state);
      if (!(bmdest.info->bmbase[bmdest.bm_offset] &
        (tab_offset[bmdest.bit_offset] & val_right[CHKR_WO])))
        chkr_access_error (dest, CHKR_WO, &bmdest, 0, orig_len, len << log_bytes_per_state);
      /* Write the right.  */
      right <<= 2 * bmdest.bit_offset;
      /* The `write' bit is cleared only if right == 0 (red zone).  */
      bmdest.info->bmbase[bmdest.bm_offset] &= 
        ~tab_offset[bmdest.bit_offset] | val_right[CHKR_WO];
      bmdest.info->bmbase[bmdest.bm_offset] |= right;
      /* Update for the next right.  */
      if (bmdest.bit_offset == 3)
        {
          bmdest.bit_offset = 0;
          bmdest.bm_offset++;
        }
      else
        bmdest.bit_offset++;
      len--;
    }
  return;
}

#if 0
/* Copy a part of bitmap.  Can be not aligned.  No checks.  */
void
chkr_raw_copy_of_bitmap (PTR dest, PTR src, uint len)
{
  struct BitMem bmsrc, bmdest;
  uchar right = 0;
  uchar right0 = 0;
  
  if (len == 0)
    return;
  
  SetBitMem (src, &bmsrc);
  if (bmsrc.length < len)
    {
      chkr_printf (M_SRC_IN_CP_BM);
      return;
    }
  switch (bmsrc.info->type)
    {
      case SegFinish:
      case SegVoid:
        chkr_printf (M_SRC_IN_CP_BM);
        return;
      case SegText:
        if (is_text_writable)
          right0 = CHKR_RW;
        else
          right0 = CHKR_RO;
        break;
      case SegRW:
        right0 = CHKR_RW;
        break;
      case SegROnly:
        right0 = CHKR_RO;
        break;
      case SegWOnly:
        right0 = CHKR_WO;
        break;
      case SegMmap:
        right0 = bmsrc.info->right;
        break;
      case SegShm:
        right0 = bmsrc.info->right;
        break;
      case SegNormal:
        break;
      case SegPage:
        break;
      default:
        chkr_abort ();
    }
  right &= CHKR_MASK;
  
  SetBitMem (dest, &bmdest);
  if (bmdest.length < len)
    {
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_DEST_IN_CP_BM);
      return;
    } 
  switch (bmdest.info->type)
    {
      case SegFinish:
      case SegVoid:
        chkr_report (M_C_MES_CK_ET);
        chkr_printf (M_DEST_IN_CP_BM);
        return;
      case SegText:
        if (!is_text_writable)
          {
            chkr_report (M_C_MES_CK_ET);
            chkr_printf (M_DEST_IN_CP_BM);
          }
        return;
      case SegRW:
      case SegWOnly:
      case SegShm:
      case SegMmap:
      case SegROnly:
      case SegPage:
        return;
      case SegNormal:
        break;
    }
  len = (len + ((uint)src & bytes_per_state_round) + bytes_per_state_round)
            >> log_bytes_per_state;
  while (len > 0)
    {
      switch (bmsrc.info->type)
        {
      case SegNormal:
          /* Read a right.  */
          right = bmsrc.info->bmbase[bmsrc.bm_offset];
          right >>= 2 * bmsrc.bit_offset;
          right &= CHKR_MASK;
          /* Update for the next right */
          if (bmsrc.bit_offset == 3)
            {
              bmsrc.bit_offset = 0;
              bmsrc.bm_offset++;
            }
          else
            bmsrc.bit_offset++;
          break;
      case SegPage:
          right = bmsrc.info->bmbase[(src - bmsrc.info->base) >> LOG_PAGESIZE];
          right &= CHKR_MASK;
          break;
      default:
          right = right0;
          break;
        }
      /* Write the right.  */
      right <<= 2 * bmdest.bit_offset;
      /* The `write' bit is cleared only if right == 0 (red zone).  */
      bmdest.info->bmbase[bmdest.bm_offset] &= 
        ~tab_offset[bmdest.bit_offset] | (right == 0 ? 0 : val_right[CHKR_WO]);
      bmdest.info->bmbase[bmdest.bm_offset] |= right;
      /* Update for the next right.  */
      if (bmdest.bit_offset == 3)
        {
          bmdest.bit_offset = 0;
          bmdest.bm_offset++;
        }
      else
        bmdest.bit_offset++;
      len--;
    }
  return;
}
#endif


#ifdef GCCCHECKER
void
__gcc_chkr_set_right (const PTR ptr, int len, int right)
{
  ENTER_CHECKER;
  chkr_set_right (ptr, len, right);
  LEAVE_CHECKER;
}

void
__gcc_chkr_check_addr (const PTR ptr, int len, int right)
{
  ENTER_CHECKER;
  chkr_check_addr (ptr, len, right);
  LEAVE_CHECKER;
}

void
__gcc_chkr_check_exec (const PTR ptr)
{
  ENTER_CHECKER;
  chkr_check_exec (ptr);
  LEAVE_CHECKER;
}

void
__gcc_chkr_copy_bitmap (PTR dest, PTR src, unsigned int len)
{
  ENTER_CHECKER;
  chkr_copy_bitmap (dest, src, len);
  LEAVE_CHECKER;
}

void
stubs_chkr_set_right (const PTR ptr, int len, int right)
{
  ENTER_CHECKER;
  chkr_set_right (ptr, len, right);
  LEAVE_CHECKER;
}

int
stubs_chkr_check_addr (const PTR ptr, int len, int right, char *mes)
{
  int old = nbr_rep_access_error;
  ENTER_CHECKER;
  chkr_check_addr (ptr, len, right);
  LEAVE_CHECKER;
  return nbr_rep_access_error > old;
}

int
stubs_chkr_check_exec (const PTR ptr, char *mes)
{
  int old = nbr_rep_access_error;
  ENTER_CHECKER;
  chkr_check_exec (ptr);
  LEAVE_CHECKER;
  return nbr_rep_access_error > old;
}

int
stubs_chkr_copy_bitmap (PTR dest, PTR src, unsigned int len, char *mes)
{
  int old = nbr_rep_access_error;
  ENTER_CHECKER;
  chkr_copy_bitmap (dest, src, len);
  LEAVE_CHECKER;
  return nbr_rep_access_error > old;
}

int
stubs_chkr_check_str (const PTR ptr, int right, char *mes)
{
  int old = nbr_rep_access_error;
  ENTER_CHECKER;
  chkr_check_str (ptr, right);
  LEAVE_CHECKER;
  return nbr_rep_access_error > old;
}
#endif /* GCCCHECKER */

#ifdef CHKR_PROFILE
/* Display profile.  Called by chkr_do_end() only if profile_flag is true.  */
void
display_profile (void)
{
  register MapInfo *tmpinfo = Fmapinfo;
  unsigned int total = 0;

  while (tmpinfo->type != SegFinish)
    {
      chkr_printf (M_HANDLED_N_TIME, tmpinfo->name,
		   tmpinfo->addressed, (tmpinfo->addressed > 1) ? "s" : "");
      total += tmpinfo->addressed;
      tmpinfo++;
    }
  chkr_printf (M_HANDLED_MEM, total);
  chkr_printf (M_BM_CALLED, adjust_bm_called);
  chkr_printf (M_ACC_ERR_REP, nbr_rep_access_error);
  chkr_printf (M_ACC_ERR_TOT, nbr_tot_access_error);
  chkr_printf (M_ACC_MALLOC_CALLS, nbr_malloc_calls);
  chkr_printf (M_ACC_FREE_CALLS, nbr_free_calls);
  chkr_printf (M_ACC_REALLOC_CALLS, nbr_realloc_calls);
}
#endif

#define ISBLANK(c) ((c) == ' ' || (c) == '\t')
#define MAX_FUNCTION_SUPPRESSED 20

/* Parse 'suppress (err) f1; f2; f3' expressions.  This is called by 
   parse-args.c.  */
int
parse_suppress (char *str)
{
  struct func_suppress *f;
  struct func_name *f_name;
  char *funcs[MAX_FUNCTION_SUPPRESSED];
  int type;
  int n_funcs;
  char *b;
  char *e;
 
  /* Skip blanks.  */
  while (ISBLANK (*str))
    str++;
 
  /* Type should be (abc).  */
  if (str[0] != '(' || strlen (str) < 5 || str[4] != ')')
    return -1;
   
  /* Get the type number.  */
  if (strcmp (str, "all") == 0)
    type = -1;
  else
    {
      type = get_error_type_by_name (str + 1);
      if (type == -1)
        return -1;
    }
   
  str += 5;
  b = str;
  n_funcs = 0;
  f_name = (struct func_name*) 0;
  while (*b)
    {
      while (ISBLANK (*b))
        b++;
      if (*b == '\0' || *b == '\n')
        break;
      e = b;
      while (*e && *e != ';' && *e != ' ' && *e != '\t')
        e++;
      funcs[n_funcs] = sys_malloc (e - b + 1);
      strncpy (funcs[n_funcs], b, e - b);
      funcs[n_funcs][e-b] = '\0';
      b = e;
      n_funcs++;
      if (n_funcs == MAX_FUNCTION_SUPPRESSED)
        break;
      while (*b && (ISBLANK (*b) || *b == ';'))
        b++;
    }
  
  
  if (n_funcs == 0)
    {
      /* No function names: disable the error.  */
      disable_error (type);
    }
  else
    {
      f = (struct func_suppress*) sys_malloc (sizeof (struct func_suppress)
                                              + n_funcs * sizeof (char*));
      f->type = type;
      f->nbr_funcs = n_funcs;
      memcpy (f->funcs, funcs, n_funcs * sizeof(char*));
      record_suppress (type, n_funcs);
      f->next = suppress;
      suppress = f;
    }
  return 0;
}

/* Display each suppressed errors/functions.  This can be used when the symtab
   is not available. */
void
__chkr_dump_suppress (void)
{
 struct func_suppress *fs;
 int i;
 
 /* Nothing to dump.  */
 if (!suppress)
   return;
   
 chkr_report (M_C_MES_CK_ET);
 
 for (fs = suppress; fs; fs = fs->next)
   {
     chkr_printf ("\tsuppress (%s)", get_error_name_by_number (fs->type));
     for (i = 0; i < fs->nbr_funcs; i++)
       chkr_printf (" %s;", fs->funcs[i]);
     chkr_printf ("\n");
   }
}
     
/* Disable errors.  STR is an address or a range.  STR can be modified.
   Called by error.c.  */
int
register_disable_range (uint low, uint high)
{
  struct range_disable *m;

  m = (struct range_disable*) sys_malloc (sizeof (struct range_disable));
  m->first = low;
  m->last = high;
  m->next = disable;
  disable = m;
  return 1;	/* OK */
}
  

/* Functions for Mmap */
/* This function is a member of a descriptor and is used to specify the
   error.  Called only by chkr_access_error().  */
static int
ename_mmapseg (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  /* If PTR is after the end of the zone, this is a violation.  */
  if (bmi->ptr >= (bm->info->base + bm->info->info.mmapinfo->real_length))
    return M_U_BVM_MM_ET;
    
  /* if PTR + SIZE ...  */
  if ((bmi->ptr + bmi->size) > (bm->info->base + bm->info->info.mmapinfo->real_length))
    return M_U_BVM_MM_ET;
    
  /* Otherwise, the user has not this right.  */
  return M_U_APD_MM_ET;
}

/* Just add a new descriptor at the end of the list (but before the last).
   It returns an index for mapinfo[].  The mapinfo[] table is updated.  */
static int
add_slot (void)
{
  int shift;		/* To update the tab */
  int i;
  MapInfo *old_Fmapinfo;
  
  if (nbr_mapinfo == max_mapinfo)
    {
      max_mapinfo += 4;
      old_Fmapinfo = Fmapinfo;
      Fmapinfo = (MapInfo *) sys_realloc ((PTR)Fmapinfo, max_mapinfo * sizeof (MapInfo));
      shift = (uint)Fmapinfo - (uint)old_Fmapinfo;
      mapinfo = (MapInfo **) sys_realloc ((PTR)mapinfo, max_mapinfo * sizeof (MapInfo*));
      
      /* Update the always-defined pointers.  */
      for (i = 0; i < nbr_mapinfo; i++)
        if (mapinfo[i])
          mapinfo[i] = (MapInfo*)((PTR)mapinfo[i] + shift);
          
      /* Initialize the other pointers.  */
      for (i = nbr_mapinfo; i < max_mapinfo; i++)
        mapinfo[i] = NULL;
    }
  Fmapinfo[nbr_mapinfo] = Fmapinfo[nbr_mapinfo-1];
  nbr_mapinfo++;
  
  /* Find a free slot in mapinfo[].  */
  for (i = 0; i < max_mapinfo; i++)
    if (mapinfo[i] == NULL)
      break;
  mapinfo[i] = &Fmapinfo[nbr_mapinfo - 2];
  return i;
}

/* Just remove a descriptor.  */
static void
remove_slot (MapInfo *tinfo)
{
  int i;
  
  for (i = 0; i < max_mapinfo; i++)
    if (mapinfo[i] == tinfo)
      {
        mapinfo[i] = NULL;
        break;
      }
  *tinfo = Fmapinfo[nbr_mapinfo-2];
  Fmapinfo[nbr_mapinfo-2] = Fmapinfo[nbr_mapinfo-1];
  for (i = 0; i < nbr_mapinfo; i++)
    if (mapinfo[i] == &Fmapinfo[nbr_mapinfo - 2])
      {
        mapinfo[i] = tinfo;
        break;
      }
  nbr_mapinfo--;
}

#if 1
/* Called by mprotect to change the rights. */
void
seg_mprotect (PTR addr, uint len, int prot)
{
  MapInfo *tmpinfo = Fmapinfo;
  int new_right;
  uint offset;
  
  new_right =  (prot & PROT_READ ? CHKR_RO : 0)
                | (prot & PROT_WRITE ? CHKR_WO : 0)
                | (prot & PROT_EXEC ? CHKR_EXEC : 0);

  for (; tmpinfo->type != SegFinish; tmpinfo++)
    if (tmpinfo->type == SegPage 
        && addr >= tmpinfo->base
        && addr <= tmpinfo->base + tmpinfo->length)
      {
        /* Round len. */
        len += ((uint)addr & (CHKR_PAGESIZE - 1)) + CHKR_PAGESIZE - 1;
        len >>= LOG_PAGESIZE;
        offset = (tmpinfo->base - addr) >> LOG_PAGESIZE;
        memset (tmpinfo->bmbase + offset, new_right, len); /* FIXME: mask. */
        return;
      }
}

/* Called usually by munmap() and new_segmap() when an mmap zone has
 *  desappeared.  The history is also forgotten FIXME. */
void
remove_mmap (PTR addr, uint len)
{
  MapInfo *tmpinfo = Fmapinfo;
  uint round_len = (len + CHKR_PAGESIZE - 1) & ~(CHKR_PAGESIZE-1);

  for (; tmpinfo->type != SegFinish; tmpinfo++)
    if (tmpinfo->type == SegPage)
      {
        /* The zone removed can: */
        /*  * recover a zone */ 
        if (addr <= tmpinfo->base 
            && (addr + round_len) >= (tmpinfo->base + tmpinfo->length))
          {
            /* Remove this mapinfo (aka descriptor). */
            sys_free ((PTR)tmpinfo->info.pageinfo);
            remove_slot (tmpinfo);
            tmpinfo--;	/* Oh yes! that's the question :-) */
          }
        /*  * recover the beginning of a zone */
        else if (addr <= tmpinfo->base
            && (addr + round_len) > tmpinfo->base
            && (addr + round_len) < (tmpinfo->base + tmpinfo->length))
          {
            tmpinfo->length -= addr + round_len - tmpinfo->base;
            tmpinfo->base = addr + round_len;
            memmove (tmpinfo->bmbase,
                     tmpinfo->bmbase + (round_len >> LOG_PAGESIZE),
                     (tmpinfo->length + CHKR_PAGESIZE - 1) >> LOG_PAGESIZE);
          }
        /*  * recover the end of a zone */
        else if (addr > tmpinfo->base
            && addr < (tmpinfo->base + tmpinfo->length)
            && (addr + round_len) >= (tmpinfo->base + tmpinfo->length)) 
          {
            tmpinfo->length = addr - tmpinfo->base;
          }
        /*  * be inside a zone */
        else if (addr > tmpinfo->base
            && (addr + round_len) < (tmpinfo->base + tmpinfo->length))
          {
            memset (tmpinfo->bmbase + ((addr - tmpinfo->base) >> LOG_PAGESIZE),
                    0,
                    (round_len + CHKR_PAGESIZE - 1) >> LOG_PAGESIZE);
          }
      }
}

/* Create a new descriptor for a mmap zone.  Called just after mmap().  */
void
new_segmmap (PTR addr, uint len, int prot, int flags, int filedes, uint off)
{
  MapInfo *minfo;
  MapInfo *before_info;
  MapInfo *after_info;
  uchar *bp;
  uint new_right;
  uint round_len = (len + CHKR_PAGESIZE - 1) & ~(CHKR_PAGESIZE-1);

  if (len == 0)
    return;	/* Nothing to do */
   
  /* Remove the zones recovered.  */
  remove_mmap (addr, round_len);
  
  /* Set the right according to the args.  */
  new_right =   (prot & PROT_READ ? CHKR_RO : 0)
              | (prot & PROT_WRITE ? CHKR_WO : 0)
              | (prot & PROT_EXEC ? CHKR_EXEC : 0);


#if 0
  /* Compute the len.  */
#ifdef HAVE_ANONYMOUS
  if (flags & MAP_ANONYMOUS)
    len_with_file = len;
  else
#else
  if (1)
#endif
    {
      static dev_t dev_zero_dev = 0;
      static ino_t dev_zero_ino = 0;
      
      /* The memory mapped is a file.  The real length is determined by the
         file length.  */
      struct stat buf;
      
      /* Note that `/dev/zero' is particular: size is infinite.  */
      if (dev_zero_ino == 0)
        {
          stat ("/dev/zero", &buf);
          dev_zero_dev = buf.st_dev;
          dev_zero_ino = buf.st_ino;
        }
        
      fstat (filedes, &buf);
      
      if (buf.st_dev != dev_zero_dev || buf.st_ino != dev_zero_ino)
        if (len + off > buf.st_size)
          {
            if (off > buf.st_size)
              len_with_file = 0;	/* Must never happen. */
            else
              len_with_file = buf.st_size - off; /* Set the new real length. */
          }
    }
#endif

  /* Look if the mapinfo can be gathered with another one.  */
  /* A MapInfo just before the block.  */
  before_info = (MapInfo *) 0;
  /* A MapInfo just after the block.  */
  after_info = (MapInfo *) 0;
  
  for (minfo = Fmapinfo; minfo->type != SegFinish; minfo++)
    if (minfo->type == SegPage)
      {
        if (addr + round_len == minfo->base)
          after_info = minfo;
        if (addr == (PTR)minfo->base + minfo->length)
          before_info = minfo;
      }
  
  if (before_info || after_info)
    {
      uint new_len = round_len;
      
      /* At first, create the new bitmap.  */
      if (before_info)
        new_len += before_info->length;
      if (after_info)
        new_len += after_info->length;
      bp = (uchar *) sys_malloc (new_len >> LOG_PAGESIZE);
      if (before_info)
        {
          memcpy (bp, before_info->bmbase, before_info->length >> LOG_PAGESIZE);
          new_len = before_info->length >> LOG_PAGESIZE;
        }
      else
        new_len = 0;
      memset (bp + new_len, new_right, round_len >> LOG_PAGESIZE);
      new_len += round_len >> LOG_PAGESIZE;
      if (after_info)
        memcpy (bp + new_len, after_info->bmbase, after_info->length >> LOG_PAGESIZE);
      
      /* Merge the info. */
      if (before_info)
        {
          before_info->length += round_len;
          if (after_info)
            {
              before_info->length += after_info->length;
              sys_free ((PTR)after_info->info.pageinfo);
              remove_slot (after_info);
            }
        }
      else
        {
	  /* after_info only.  */
          after_info->length += round_len;
          after_info->base -= round_len;
          after_info->real_base = after_info->base;
	  before_info = after_info;
        }     
      /* The MapInfo for the block is now in before_info.  */
      sys_free (before_info->bmbase);
      before_info->bmbase = bp;
    }
  else
    {        
      uint slot;
      struct Pageinfo *pinfo;
      
      /* Create the bitmap.  */
      bp = (uchar *) sys_malloc (round_len >> LOG_PAGESIZE);
      memset (bp, new_right, round_len >> LOG_PAGESIZE);
      
      /* Add a new descriptor and fill it.  */
      slot = add_slot();
      mapinfo[slot]->name = M_MMAP_SEGMENT;
      mapinfo[slot]->base = addr;
      mapinfo[slot]->real_base = addr;
      mapinfo[slot]->length = round_len;
      mapinfo[slot]->bmbase = bp;
      mapinfo[slot]->type = SegPage;
      mapinfo[slot]->idfunc = ename_mmapseg;
      mapinfo[slot]->efunc = chkr_access_error_mmap;
      mapinfo[slot]->right = new_right;
  
      /* Alloc space for more infos and the history.  */
      pinfo = (struct Pageinfo*) sys_malloc (sizeof (struct Pageinfo) + HISTORY_DEPTH * sizeof(void*));
      pinfo->history = (PTR*)(pinfo + 1);
#ifdef CHKR_SAVESTACK
      chkr_get_history (pinfo->history, 0, HISTORY_DEPTH);
#endif
      mapinfo[slot]->info.pageinfo = pinfo;
    }


}

#else

/* Split a mmap zone into the (old and shorter) zone and a new zone.
 * Create a new descriptor. */
static int 
split_segmmap (MapInfo *tinfo, uint len)
{
  int slot;

  /* Avoid stupid error */
  if (len >= tinfo->length)
    chkr_abort ();

  /* Duplicate the entry */  
  slot = add_slot ();
  memcpy (mapinfo[slot], tinfo, sizeof (MapInfo));

  /* Duplicate the history. */
  mapinfo[slot]->info.mmapinfo = (struct Mmapinfo*) sys_dupalloc ((PTR)tinfo->info.mmapinfo);

  /* Now split */
  mapinfo[slot]->base += len;
  mapinfo[slot]->length -= len;
  tinfo->length -= len;
  return slot;
}

/* Called by mprotect to change the rights. */
void
seg_mprotect (PTR addr, uint len, int prot)
{
  MapInfo *tmpinfo = Fmapinfo;
  uint round_len = (len + CHKR_PAGESIZE - 1) & (~(CHKR_PAGESIZE-1));
  int slot;
  int new_right;
  
  new_right =  (prot & PROT_READ ? CHKR_RO : 0)
                | (prot & PROT_WRITE ? CHKR_WO : 0)
                | (prot & PROT_EXEC ? CHKR_EXEC : 0);

  for (; tmpinfo->type != SegFinish; tmpinfo++)
    if (tmpinfo->type == SegMmap && tmpinfo->right != new_right)
      {
        /* The zone removed can: */
        /*  * recover a zone */ 
        if (addr <= tmpinfo->base 
            && (addr + round_len) >= (tmpinfo->base + tmpinfo->length))
          {
            tmpinfo->right = new_right;
          }
        /*  * recover the beginning of a zone */
        else if (addr <= tmpinfo->base
            && (addr + round_len) > tmpinfo->base
            && (addr + round_len) < (tmpinfo->base + tmpinfo->length))
          {
            split_segmmap(tmpinfo, (tmpinfo->base + tmpinfo->length) - (addr + round_len));
            tmpinfo->right = new_right;
          }
        /*  * recover the end of a zone */
        else if (addr > tmpinfo->base
            && addr < (tmpinfo->base + tmpinfo->length)
            && (addr + round_len) >= (tmpinfo->base + tmpinfo->length)) 
          {
            slot = split_segmmap(tmpinfo, (tmpinfo->base + tmpinfo->length) - addr);
            mapinfo[slot]->right = new_right;
          }
        /*  * be inside a zone */
        else if (addr > tmpinfo->base
            && (addr + round_len) < (tmpinfo->base + tmpinfo->length))
          {
            /* In this case, the zone must be split. */
            split_segmmap(tmpinfo, addr + round_len - tmpinfo->base);
            slot = split_segmmap(tmpinfo, addr - tmpinfo->base);
            mapinfo[slot]->right = new_right;
          }
      }
}

/* Called usually by munmap() and new_segmap() when an mmap zone has
 *  desappeared.  The history is also forgotten FIXME.  */
void
remove_mmap (PTR addr, uint len)
{
  MapInfo *tmpinfo = Fmapinfo;
  uint round_len = (len + CHKR_PAGESIZE - 1) & (~(CHKR_PAGESIZE-1));
  int slot;

  for (; tmpinfo->type != SegFinish; tmpinfo++)
    if (tmpinfo->type == SegMmap)
      {
        /* The zone removed can: */
        /*  * recover a zone */ 
        if (addr <= tmpinfo->base 
            && (addr + round_len) >= (tmpinfo->base + tmpinfo->length))
          {
            /* Remove this mapinfo (aka descriptor). */
            sys_free ((PTR)tmpinfo->info.mmapinfo);
            remove_slot (tmpinfo);
            tmpinfo--;	/* Oh yes! that's the question */
          }
        /*  * recover the beginning of a zone */
        else if (addr <= tmpinfo->base
            && (addr + round_len) > tmpinfo->base
            && (addr + round_len) < (tmpinfo->base + tmpinfo->length))
          {
            tmpinfo->length -= addr + round_len - tmpinfo->base;
            tmpinfo->base = addr + round_len;
          }
        /*  * recover the end of a zone */
        else if (addr > tmpinfo->base
            && addr < (tmpinfo->base + tmpinfo->length)
            && (addr + round_len) >= (tmpinfo->base + tmpinfo->length)) 
          {
            tmpinfo->length = addr - tmpinfo->base;
          }
        /*  * be inside a zone */
        else if (addr > tmpinfo->base
            && (addr + round_len) < (tmpinfo->base + tmpinfo->length))
          {
            /* In this case, the zone must be split. */
            slot = split_segmmap (tmpinfo, addr - tmpinfo->base);
            mapinfo[slot]->base = addr + round_len;
            mapinfo[slot]->length -= round_len;
          }
      }
}

/* Create a new descriptor for a mmap zone.  Called just after mmap(). */
void
new_segmmap (PTR addr, uint len, int prot, int flags, int filedes, uint off)
{
  static dev_t dev_zero_dev = 0;
  static ino_t dev_zero_ino = 0;
  uint round_len = (len + CHKR_PAGESIZE - 1) & ~(CHKR_PAGESIZE-1);
  struct Mmapinfo *minfo;
  int slot;

  if (len == 0)
    return;	/* Nothing to do */
   
  /* Remove the zones recovered. */
  remove_mmap (addr, round_len);
  
  /* Add a new descriptor and fill it. */
  slot = add_slot();
  mapinfo[slot]->name = M_MMAP_SEGMENT;
  mapinfo[slot]->base = addr;
  mapinfo[slot]->real_base = addr;
  mapinfo[slot]->length = round_len;
  mapinfo[slot]->bmbase = (uchar *) 0;
  mapinfo[slot]->type = SegMmap;
  mapinfo[slot]->idfunc = ename_mmapseg;
  mapinfo[slot]->efunc = chkr_access_error_mmap;
  
  /* Alloc space for more infos and the history. */
  minfo = (struct Mmapinfo*) sys_malloc (sizeof (struct Mmapinfo) + HISTORY_DEPTH * sizeof(void*));
  minfo->history = (PTR*)(minfo + 1);
#ifdef CHKR_SAVESTACK
  chkr_get_history (minfo->history, 3, HISTORY_DEPTH);
#endif
  mapinfo[slot]->info.mmapinfo = minfo;
  
  /* compute the len */
#ifdef HAVE_ANONYMOUS
  if (!(flags & MAP_ANONYMOUS))
#else
  if (1)
#endif
    {
      /* The memory mapped is a file.  The real length is determined by the
       *  file length. */
      struct stat buf;
      
      /* Note that `/dev/zero' is particular: size is infinite. */
      if (dev_zero_ino == 0)
        {
          stat ("/dev/zero", &buf);
          dev_zero_dev = buf.st_dev;
          dev_zero_ino = buf.st_ino;
        }
        
      fstat (filedes, &buf);
      
      if (buf.st_dev != dev_zero_dev || buf.st_ino != dev_zero_ino)
        if (len + off > buf.st_size)
          {
            if (off > buf.st_size)
              len = 0;	/* Must never happen. */
            else
              len = buf.st_size - off;	/* Set the new real length. */
          }
    }
  minfo->real_length = len;
  
  /* Set the right according to the args. */
  mapinfo[slot]->right = (prot & PROT_READ ? CHKR_RO : 0)
                         | (prot & PROT_WRITE ? CHKR_WO : 0)
                         | (prot & PROT_EXEC ? CHKR_EXEC : 0);
}
#endif

/* For the heap. */
static int
ename_heap (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  struct malloc_header *block;
  PTR ptr;
  int mask;
  int i;

#ifndef GNU_MALLOC
  /* If there is no heap, this is really strange. */
  if (_firstmdesc == NULL_MDESC)
    return M_IE_ET;
#endif
    
  /* PTR contains the exact address. */
  ptr = bmi->ptr + bmi->size - bmi->remain;
  /* The good right in the bitmap must be watched: we can have this:
   * RW RW RW RO
   * ^
   * PTR
   */
  if (bmi->size > 1)
    {
      mask = 3;
      for (i = 0; i < 4; i++)
	{
	  if (((bm->info->bmbase)[bm->bm_offset] & (bmi->val & mask))
	      != (bmi->val & mask))
	    {
	      ptr += i << log_bytes_per_state;
	      mask = 0;		/* a flag */
	      break;
	    }
	  mask = (mask << 2) | 3;
	}
      if (i == 3 && mask)
	chkr_abort ();
      if (bmi->size == bmi->remain)
	ptr -= bm->bit_offset << log_bytes_per_state;
    }

  /* Look the block where PTR points *inside* (not in a red zone).  */
  block = find_header ((struct mdesc*)0, ptr, 0);
  if (block == NULL_HEADER)
    return M_U_BVH_HE_ET;
  switch (block->state)
    {
    case MDFREE:
    case MDAGED:
      switch (bmi->arq)
	{
	case CHKR_RO:
	case CHKR_NOT_RZ:
	  return M_R_RFB_HE_ET;	/* Read inside a Free Block. */
	case CHKR_RW:
	case CHKR_WO:
	  return M_W_WFB_HE_ET;	/* Write inside a Free Block. */
	default:
	  return M_IE_ET;	/* Internal Error. */
	}
    case MDBRK:
      return M_IE_ET;		/* BRK zone are always RW. */
    case MDBUSY:
      switch (bmi->arq)
	{
	case CHKR_RW:
	case CHKR_RO:
	  return M_R_RUH_HE_ET;	/* Read Unitialized data. */
	default:
	  return M_IE_ET;
	}
    default:
      return M_IE_ET;
    }
}

/* Search a free zone between MM_LOW and MM_HIGH which may contains a bitmap.
 *  BASE is used to compute a first address.  While this address is bad, we
 *  look further. */
char *
find_bmbase (PTR base)
{
  PTR res;
  int i;
  int npages;
#define PAGE_ALIGN(x)  (((unsigned int)(x) + CHKR_PAGESIZE - 1) & ~(CHKR_PAGESIZE-1))
  res = (PTR)MM_HEAP + PAGE_ALIGN (((uint)base - objects->next->end) >> log_bytes_per_bbm);
 again:
  if (res < (PTR)MM_LOW || res > (PTR)MM_HIGH)
    chkr_abort ();
  for (i = 0; i < nbr_mapinfo; i++)
    {
      if (Fmapinfo[i].type != SegNormal)
        continue;
      npages = PAGE_ALIGN (Fmapinfo[i].length >> log_bytes_per_bbm);
      if (res >= (PTR)Fmapinfo[i].bmbase && res <= ((PTR)Fmapinfo[i].bmbase + npages))
        {
          res += npages;
          goto again;
        }
    }
  if (res < (PTR)MM_LOW || res > (PTR)MM_HIGH)
    chkr_abort ();
  return res;
}

/* Add a new descriptor for an heap.
 * If BRK is true, this is an heap based on sbrk().  This arg is only used to
 *   set the name of the segment.
 * BMBASE is the base of the bitmap. 
 * It return a slot number for mapinfo[]. */
int
new_heap (int brk, struct mdesc *mdp)
{
  MapInfo *res;
  int slot = add_slot ();
  res = mapinfo[slot];
  res->info.heapinfo = (struct Heapinfo*) sys_malloc (sizeof (struct Heapinfo));
#ifdef GNU_MALLOC
  res->info.heapinfo->mdp = __mmalloc_default_mdp;
  res->name = M_HEAP_BRK_SEGMENT;
  res->base = heap_base;
  res->real_base = heap_base;
  res->length = heap_bm_size;
  res->bmbase = heap_bm_base;
  res->type = SegNormal;
  res->efunc = chkr_access_error_heap;
  res->idfunc = ename_heap;
  res->right = CHKR_RW;
#else
  res->info.heapinfo->mdp = mdp;
  res->name = brk ? M_HEAP_BRK_SEGMENT : M_HEAP_MAP_SEGMENT;
  res->base = mdp->base;
  res->real_base = mdp->base;
  res->length = mdp->breakval - mdp->base;
  res->bmbase = mdp->info.inmem.bitmap->base;
  res->type = SegNormal;
  res->efunc = chkr_access_error_heap;
  res->idfunc = ename_heap;
  res->right = CHKR_RW;
#endif
  return slot;
}

/* Remove an heap. */
void
remove_heap (int slot)
{
  MapInfo *map = mapinfo[slot];
  sys_free ((PTR)map->info.heapinfo);
  remove_slot (map);
}


#ifdef HAVE_SHM
/* Functions for shm */
/* Function to specify an error.  */
static int
ename_shmseg (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  /* FIXME */
  return M_U_WRS_SH_ET;
}

/* Create a new descriptor for a shm (SHared Memory).  */
void
new_segshm (int shmid, PTR addr, int flags)
{
  struct Shminfo *sinfo;
  struct shmid_ds shmbuf;
  int slot;

  /* Used to know the size of the zone.  */
  shmctl (shmid, IPC_STAT, &shmbuf);
  
  /* Create a new descriptor and fill it.  */
  slot = add_slot ();  
  mapinfo[slot]->name = M_SHM_SEGMENT;
  mapinfo[slot]->base = addr;
  mapinfo[slot]->real_base = addr;
  mapinfo[slot]->length = shmbuf.shm_segsz;
  mapinfo[slot]->bmbase = (uchar *) 0;
  mapinfo[slot]->type = SegShm;
  mapinfo[slot]->idfunc = ename_shmseg;
  mapinfo[slot]->efunc = chkr_access_error_shm;
  /* Alloc space for more infos and the history. */
  sinfo = (struct Shminfo*) sys_malloc (sizeof (struct Shminfo) + HISTORY_DEPTH * sizeof(void*));
  sinfo->history = (PTR*)(sinfo + 1);
#ifdef CHKR_SAVESTACK
  chkr_get_history (sinfo->history, 0, HISTORY_DEPTH);
#endif  
  mapinfo[slot]->info.shminfo = sinfo;
  sinfo->real_length = shmbuf.shm_segsz;
  mapinfo[slot]->right = (flags & SHM_RDONLY ? CHKR_RO : CHKR_RW);
  sinfo->shmid = shmid;
}

/* Remove a descriptor of shm.  */
void
remove_shm (PTR addr)
{
  MapInfo *tmpinfo = Fmapinfo;

  while (tmpinfo->type != SegFinish)
    if (tmpinfo->type == SegShm && tmpinfo->base == addr)
      {
        /* Free the additional infos.  */
        sys_free ((PTR)tmpinfo->info.shminfo);
        remove_slot (tmpinfo);
        break;
      }
    else
      tmpinfo++;
}
#endif /* HAVE_SHM */

/* Functions to identify error types. */
static int
ename_object (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  switch (bmi->arq)
    {
    case CHKR_RO:
    case CHKR_WO:
    case CHKR_RW:
    case CHKR_NOT_RZ:
      return M_U_NZA_NZ_ET;
    default:
      return M_IE_ET;	/* Internal Error. */
    }
}

/* For the NULL zone. */
static int
ename_null (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  switch (bmi->arq)
    {
    case CHKR_RO:
    case CHKR_WO:
    case CHKR_RW:
    case CHKR_NOT_RZ:
      return M_U_NZA_NZ_ET;
    default:
      return M_IE_ET;	/* Internal Error. */
    }
}

/* For the text segment. */
static int
ename_textseg (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  switch (bmi->arq)
    {
    case CHKR_WO:
    case CHKR_RW:
      return M_W_WOB_TE_ET;
    default:
      return M_IE_ET;
    }
}

/* For the data segment. */
static int
ename_dataseg (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  return M_IE_ET;		/* Internal Error at this time */
}

/* For the stack segment. */
static int
ename_stack (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  int mask;
  int fl = 0;
  int i;

  /* PTR contains the exact addr.  See ename_heap(). */
  mask = 0xc0;
  for (i = 0; i < 4; i++)
    {
      if (((bm->info->bmbase)[bm->bm_offset] & (bmi->val & mask))
	  != (bmi->val & mask))
	{
	  fl = 1;
	  break;
	}
      mask >>= 2;
    }
  if (!fl)
    chkr_abort ();

  fl = (bm->info->bmbase)[bm->bm_offset] & mask;
  while (mask != 3)
    {
      mask >>= 2;
      fl >>= 2;
    }
  /* now fl contains the right of the error */
  switch (bmi->arq)
    {
    case CHKR_RO:
      switch (fl)
	{
	case CHKR_WO:
	  return M_R_RUS_ST_ET;		/* Read Uninitialized bytes. */
	case 0:
	  return M_R_RZS_ST_ET;		/* Read in a red Zone. */
	default:
	  return M_IE_ET;
	}
    case CHKR_WO:
      if (fl == 0)
	return M_W_WZS_ST_ET;		/* Write into a red Zone. */
      else
	return M_IE_ET;
    case CHKR_RW:
      switch (fl)
	{
	case CHKR_WO:
	  return M_M_WUS_ST_ET;		/* Modify */
	case 0:
	  return M_M_MZS_ST_ET;
	default:
	  return M_IE_ET;
	}
    case CHKR_NOT_RZ:
      return M_R_RZS_ST_ET;		/* Read in a red Zone. */
    default:
      return M_IE_ET;
    }
}

/* Initialize the descriptors.  */
static void
init_mapinfos (void)
{
  int i;
  unsigned int text_beg;
  struct object *obj;
  
  max_mapinfo = 6;
  
  Fmapinfo = (MapInfo *) sys_malloc (max_mapinfo * sizeof (MapInfo));
  mapinfo = (MapInfo**) sys_malloc (max_mapinfo * sizeof (MapInfo*));
  for (i = 0; i < max_mapinfo; i++)
    mapinfo[i] = NULL;
  
  /* Set the always-defined pointers.  */
  i = 0;
#ifdef CHKR_STACKBITMAP
  mapinfo[STACKBM] = &Fmapinfo[i++];
#endif
#ifdef CHKR_DATABITMAP
  mapinfo[DATABM] = &Fmapinfo[i++];
#endif
  mapinfo[TEXTBM] = &Fmapinfo[i++];
  mapinfo[NULLBM] = &Fmapinfo[i++];

  Fmapinfo[i].type = SegFinish;
  
  nbr_mapinfo = i + 1;
  text_beg = objects->org ? objects->org : null_pointer_zone;

  /* NULL zone.  */
  mapinfo[NULLBM]->name = M_NULL_ZONE;
  mapinfo[NULLBM]->base = (PTR) 0x0;
  mapinfo[NULLBM]->real_base = (PTR) 0x0;  
  mapinfo[NULLBM]->length = text_beg;
  mapinfo[NULLBM]->bmbase = (uchar *) 0;
  mapinfo[NULLBM]->type = SegVoid;
  mapinfo[NULLBM]->idfunc = ename_null;
  mapinfo[NULLBM]->efunc = chkr_access_error_null;
  mapinfo[NULLBM]->right = 0;

  /* Text segment.  */
  mapinfo[TEXTBM]->name = M_TEXT_SEGMENT;
  mapinfo[TEXTBM]->base = (PTR) text_beg;
  mapinfo[TEXTBM]->real_base = (PTR) text_beg;
  mapinfo[TEXTBM]->length = (uint)(objects->end) - text_beg;
  mapinfo[TEXTBM]->bmbase = (uchar *) 0;
  mapinfo[TEXTBM]->type = SegText;
  mapinfo[TEXTBM]->idfunc = ename_textseg;
  mapinfo[TEXTBM]->efunc = chkr_access_error_text;
  mapinfo[TEXTBM]->right = CHKR_RO | CHKR_EXEC | (objects->rights & OBJECT_WRIT ? CHKR_WO : 0);

#ifdef CHKR_DATABITMAP
  /* Data segment.  */
  mapinfo[DATABM]->name = M_DATA_SEGMENT;
  mapinfo[DATABM]->base = (PTR)(objects->next->org);
  mapinfo[DATABM]->real_base = (PTR)(objects->next->org);
  mapinfo[DATABM]->length = (uint)(objects->next->end) - (uint)(objects->next->org);
  mapinfo[DATABM]->bmbase = (uchar *) 0;
  mapinfo[DATABM]->type = SegRW; /*SegNormal;*/
  mapinfo[DATABM]->idfunc = ename_dataseg;
  mapinfo[DATABM]->right = CHKR_RW | (objects->next->rights & OBJECT_EXEC ? CHKR_EXEC : 0);
#endif
#ifdef CHKR_STACKBITMAP
  /* The stack.  */
  mapinfo[STACKBM]->name = M_STACK_SEGMENT;
  mapinfo[STACKBM]->base = (PTR) STACK_BASE;
  mapinfo[STACKBM]->real_base = (PTR) STACK_BASE;
  mapinfo[STACKBM]->length = 0;
  mapinfo[STACKBM]->bmbase = (uchar *) 0;
  mapinfo[STACKBM]->type = SegNormal;
  mapinfo[STACKBM]->efunc = chkr_access_error_stack;
  mapinfo[STACKBM]->idfunc = ename_stack;
  mapinfo[STACKBM]->right = CHKR_RW | CHKR_EXEC;
#endif

  /* Shared libraries.  */
  for (obj = objects->next->next; obj; obj = obj->next)
    {
      uint round_org = obj->org & ~(CHKR_PAGESIZE - 1);
      i = add_slot();
      mapinfo[i]->name = obj->name;
      mapinfo[i]->base = (PTR)round_org;
      mapinfo[i]->real_base = (PTR)round_org;
      mapinfo[i]->length = (uint)obj->end - round_org;
      mapinfo[i]->bmbase = (uchar*)0;
      mapinfo[i]->type = obj->rights & OBJECT_WRIT ? SegRW : SegROnly;
      mapinfo[i]->efunc = chkr_access_error_object;
      mapinfo[i]->idfunc = ename_object;
      mapinfo[i]->right =   (obj->rights & OBJECT_READ ? CHKR_RO : 0)
      			  | (obj->rights & OBJECT_WRIT ? CHKR_WO : 0)
      			  | (obj->rights & OBJECT_EXEC ? CHKR_EXEC : 0);
    }
}


/* Function called to give more informations. 
 */
static void
chkr_access_error_object (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  chkr_printf ("Error in object\n");
}

static void
chkr_access_error_null (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  chkr_printf (M_USE_NULL_PTR);
  chkr_printf (M_PROD_SEG_FAULT);
}

static void
chkr_access_error_text (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  chkr_printf (M_CANT_MODIFY_IT);
  chkr_printf (M_PROD_SEG_FAULT);
}

static void
chkr_access_error_mmap (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  chkr_printf ("Base: 0x%08x, 0x%08x length: 0x%08x, 0x%08x\n",
  	 (uint)bm->info->base, (uint)bm->info->real_base, bm->info->length, bm->info->info.mmapinfo->real_length);
#ifdef CHKR_SAVESTACK
  /* Display the history. */
  chkr_load_symtab ();
  chkr_printf (M_MAPPED_BY);
  disp_block_history (bm->info->info.mmapinfo->history);
#endif
  /* FIXME: violation */
}

#ifdef HAVE_SHM
static void
chkr_access_error_shm (struct BitMem const *bm, struct BitMemInfo *bmi)
{
#ifdef CHKR_SAVESTACK
  /* Display the history. */
  chkr_load_symtab ();
  chkr_printf (M_ATTACHED_BY);
  disp_block_history (bm->info->info.shminfo->history);
#endif
}
#endif /* HAVE_SHM */

#ifdef CHKR_STACKBITMAP
static void
chkr_access_error_stack (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  if (flag_verbose)
    {
#ifdef SET_KNOWN_STACK_LIMIT
      SET_KNOWN_STACK_LIMIT;
#endif
      chkr_printf (M_KNOW_STACK_LIM, known_stack_limit);
      __chkr_disp_right (bmi->ptr, 24);
    }
}
#endif

static void
chkr_access_error_heap (struct BitMem const *bm, struct BitMemInfo *bmi)
{
  struct malloc_header *block;
  struct mdesc *mdp = bm->info->info.heapinfo->mdp;
  PTR *ptr_on_block;
  int block_size;
  PTR ptr = bmi->ptr;

#ifndef GNU_MALLOC
  if (_lastmdesc == NULL_MDESC)
    return;
#endif
  if (ptr < low_addr_heap || ptr > high_addr_heap)
    return;			/* Not inside the heap. */

  block = find_header (mdp, ptr, 1);
  if (block == NULL_HEADER)
    {
      chkr_printf (M_AFTER_LAST_BLOCK);
      return;
    }

  /* Don't forget that ptr can point anywhere. */
  if (block->state == MDFREE)
    {
      /* Pointer to a free block. */
      chkr_printf (M_INSIDE_FBLOCK);
      return;
    }

  /* Compute the size of the block. */
  if (block->state == MDAGED)
    block_size = block->size - be_red_zone - af_red_zone - block->s_diff;
  else
    block_size = block->info.busy.real_size;

  if (ptr < ((PTR) block + be_red_zone + HEADER_SIZE))
    chkr_printf (M_BEFORE_BLOCK,
	       (int) ((int) block + be_red_zone + HEADER_SIZE - (int) ptr));
  else if (ptr < ((PTR) block + be_red_zone + HEADER_SIZE + block_size))
    chkr_printf (M_INSIDE_BLOCK,
	     (int) (ptr) - (int) ((int) block + be_red_zone + HEADER_SIZE));
  else
    chkr_printf (M_AFTER_BLOCK,
		 (int) (ptr) -
	  (int) ((PTR) block + be_red_zone + HEADER_SIZE + block_size));
	  
  /* Display size of the block and its beginning. */
  chkr_printf (M_BLOCK_ID, (uint) block + HEADER_SIZE + be_red_zone,
  			   block_size,
  			   mdp == __mmalloc_default_mdp ? 0 : mdp);
	  
#ifdef CHKR_SAVESTACK
  /* Display the history. */
  ptr_on_block = (PTR *) ((int) block + block->size + HEADER_SIZE - af_red_zone);
  chkr_load_symtab ();
  chkr_printf (M_BLOCK_ALLO_FRM);
  disp_block_history (ptr_on_block);
  if (block->state == MDAGED)
    {
      chkr_printf (M_N_FREE_CALLED, get_age ((struct mdesc*)0, block));
      ptr_on_block = (PTR *) ((int) block + HEADER_SIZE);
      disp_block_history (ptr_on_block);
    }
#if 0   
  if (mdp != __mmalloc_default_mdp)
    {
      chkr_printf (M_MDESC_CREATED_BY);
      disp_block_history (bm->info->info.heapinfo.history);
    }
#endif
#endif /* CHKR_SAVESTACK */
  return;
}

/* The user can set a breakpoint to this function, which is called just
 * after a report.
 */
void
__chkr_pause (void)
{
 /* nothing */
}

/* This function is called by chkr_check_addr, when an error has been
   discovered.  It calls chkr_access_error_*
   PTR is the first address checked.
   ARQ is the right required
   BM is the BitMem structure for PTR
   VAL is the byte that contains the bad right
   SIZE is the number of bytes that are checked.
   LEN is the number of bytes that remains to check.
   So SIZE - LEN is the offset of the error.
   return 0: check other errors.
   return 1: don't.  */
static int
chkr_access_error (const PTR ptr, int arq, struct BitMem const *bm,
		   char val, int size, int len)
{
  struct BitMemInfo bmi;
  int i;

#ifdef NEED_CHKR_LD_OPTIONS
  /* Without inserted code.  */
  if (!(chkr_ld_options & 2)
      || ((chkr_ld_options & 1) && bm->info->type != SegNormal))
    return 1;
#endif

  /* An error has been found!  */
  nbr_tot_access_error++;

#ifdef GET_CURRENT_IP
  if (disable)
    {
      struct range_disable *dis;
      PTR current_ip;
    
      GET_CURRENT_IP (current_ip);

      /* Check for disabled ip ('--disable=' option).  */
      if (current_ip)
	for (dis = disable; dis; dis = dis->next)
	  {
	    if ((int) current_ip >= dis->first
	       && (int) current_ip <= dis->last)
	      return 1;			/* The ip doesn't change.  */
	  }
    }
#endif /* GET_CURRENT_IP */

  bmi.ptr = (PTR)ptr;
  bmi.arq = arq;
  bmi.remain = len;
  bmi.size = size;
  bmi.val = val;
  
  /* Call the function which identify the error.  */
  if (bm->info->idfunc)
    i = (bm->info->idfunc) (bm, &bmi);
  else
    i = M_U_NMA_VD_ET;	/* FIXME */
    
  /* If the error is disabled, return immediately.  */
  if (is_error_disable (i))
    return 0;

#ifdef CHKR_SAVESTACK
  /* Suppression of errors.  */
  if (get_max_nbr_funcs_suppressed (i))
    {
      struct func_suppress *f;
      for (f = suppress; f; f = f->next)
        {
          if (f->type != i)
            continue;
          if (same_history (f->funcs, f->nbr_funcs))
            return 0;
        }
    }
#endif
    
  /* The error is going to be reported.  */
  nbr_rep_access_error++;
  chkr_report (i);
  if (arq == CHKR_NOT_RZ)
    i = CHKR_RO;
  else
    i = arq;
  chkr_printf (M_ACC_ERR_WHERE, right_name[i], size, ptr, bm->info->name);
  if (bm->info == &null_mapinfo)
    {
      chkr_printf (M_PROD_SEG_FAULT);
      chkr_printf ("Known_stack_limit = 0x%08x\n", (uint)known_stack_limit);
    }
  else if (bm->info->efunc)
    (bm->info->efunc) (bm, &bmi);
    
#ifdef CHKR_SAVESTACK
  chkr_get_history (0, 0, 0);
#endif /* CHKR_SAVESTACK */

  /* Allow the user to set a break point.  */
  __chkr_pause();
  return 1;
}

#else /* !MDCHECKER */

/* Return the size of the memory used.  */
uint
get_total_mem (void)
{
  uint total;
  total = (uint)chkr_sbrk(0) - (uint)(objects->next->org);
  return total;
}

#endif /* !MDCHECKER */

#if 1
#include "macc-mmap.h"
#else
#include "macc-brk.h"
#endif
