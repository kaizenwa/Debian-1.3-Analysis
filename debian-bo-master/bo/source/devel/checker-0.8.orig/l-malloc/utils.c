/* Check the internal state of Malloc and miscellanous
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written December 1993 by Tristan Gingold

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

#define _MALLOC_INTERNAL
#include "malloc.h"
#include "message.h"
#include "errlist.h"

/* The sizes of the red zones.  Must be a multiple of sizeof (void*).  */
unsigned int be_red_zone = 8;
unsigned int af_red_zone = 32;

/* The malloc (0) behavior.  */
/* malloc (0) returns 0.  */
#define MALLOC0_IS_ANSI 0

/* malloc (0) is replaced by malloc (1).  */
#define MALLOC0_IS_MALLOC1 1

/* malloc (0) produces a warning.  */
#define MALLOC0_MASK 2
#define MALLOC0_IS_WARNING 2

static int malloc0_behavior = MALLOC0_IS_ANSI;

/* Search the mdesc containing PTR.
   Return 0 in case of error.  */
static struct mdesc *
find_mdesc (const PTR ptr)
{
  struct mdesc *mdp;
  
  mdp = _firstmdesc;
  while (mdp != (struct mdesc*)0)
    {
      if (ptr >= (PTR)(mdp->_firstblock) 
          && ptr <= (PTR)((uint) mdp->_lastblock + mdp->_lastblock->size))
        break;
      mdp = mdp->info.inmem.next_mdesc;
    }
  return mdp;
}
  
/*
 * Blocks must be like that:
 * +--------+-------------+--------------------+-------+-------------+
 * | header | be_red_zone |       block        |  pad  | af_red_zone |
 * |        |             |                    |       |             |
 * +--------+-------------+--------------------+-------+-------------+
 *                        |<--- real_size ---->|
 *          |<------------------------ size ------------------------>|
 */
/* Search the header for an address in a block of MDESC.
 * If MDESC is null, find at first the mdesc.
 * Return NULL_HEADER if not found
 * If SOFT is not set, PTR must point inside the block area (and not inside
 *   a red_zone or a header).
 */
struct malloc_header *
find_header (struct mdesc *mdp, const PTR ptr, int soft)
{
  struct malloc_header *res;
  if (mdp == (struct mdesc *)0)
    {
      mdp = find_mdesc (ptr);
      if (mdp == (struct mdesc *)0)
        return NULL_HEADER;
    }
  if (mdp->_firstblock == NULL_HEADER)
    return NULL_HEADER;
  res = mdp->_firstblock;
  if (soft)
    {
      /* Easy */
      uint low = (uint) ptr;
      if (low < (uint) res)
	return NULL_HEADER;
      for (; res != NULL_HEADER; res = res->next)
	{
	  if (low < ((uint) res + res->size))
	    return res;
	}
    }
  else
    {
      /* `ptr' points to block (struct malloc_header *) BLK if:
       *      ptr >= BLK + HEADER_SIZE + be_red_zone
       *   && ptr < BLK + HEADER_SIZE + BLK->size - af_red_zone
       * This is equivalent to:
       *      ptr - HEADER_SIZE - be_red_zone >= BLK			(1)
       *   && ptr - HEADER_SIZE + af_red_zone < BLK + BLK->size		(2)
       * In the code: (1): low >= BLK
       *              (2): high < BLK + BLK->size
       *
       * Note that if BLK is a MD_BUSY block, (2) can be replaced by:
       *      ptr < BLK + HEADER_SIZE + be_red_zone + BLK->info.busy.real_size
       * ie   ptr - HEADER_SIZE - be_red_zone < BLK + BLK->info.busy.real_size
       */
      uint high = (uint)ptr - HEADER_SIZE + af_red_zone;
      uint high_busy = (uint)ptr - HEADER_SIZE - be_red_zone;
      uint low = (uint)ptr - HEADER_SIZE - be_red_zone;
      for (; res != NULL_HEADER; res = res->next)
	{
	  if ((low >= (uint)res) 
	      && (res->state == MDBUSY 
	          ? high_busy < (uint)res + res->info.busy.real_size
	          : high < (uint)res + res->size))
	    return res;
	}
    }
  return NULL_HEADER;
}

/* Get the age of block.  If no age, returns -1.  */
int
get_age (struct mdesc *mdp, struct malloc_header *block)
{
#ifdef NO_AGE
  return -1;
#else
  struct malloc_header *tmp;
  int age = 0;

  if (mdp == (struct mdesc *)0)
    {
      mdp = find_mdesc ((PTR)block);
      if (mdp == (struct mdesc*)0)
        return -1;
    }
    
  if (block->state != MDAGED || mdp->_youngerblock == NULL_HEADER)
    return -1;
  tmp = mdp->_youngerblock;
  do
    {
      if (tmp == block)
	return age;
      age++;
      tmp = tmp->info.aged.next;
    }
  while (tmp);
  return -1;
#endif
}

/* Disp the history.  */
void
disp_block_history (PTR *ptr)
{
  for (; *ptr; ptr++)
    chkr_show_addr (*ptr);
}

/* Set the malloc(0) behavior.  Called by parse-args.c.  */
void
set_malloc0_behavior (const char *opt)
{
  if (opt[0] == '0' && opt[1] == '\0')
    {
      malloc0_behavior &= MALLOC0_MASK;
      malloc0_behavior |= MALLOC0_IS_ANSI;
    }
  else if (opt[0] == '1' && opt[1] == '\0')
    {
      malloc0_behavior &= MALLOC0_MASK;
      malloc0_behavior |= MALLOC0_IS_MALLOC1;
    }
  else if (strncmp (opt, "warning", strlen (opt)) == 0)
    {
      malloc0_behavior |= MALLOC0_IS_WARNING;
    }
  else if (strncmp (opt, "no-warning", strlen (opt)) == 0)
    {
      malloc0_behavior &= ~MALLOC0_IS_WARNING;
    }
  else
    {
      chkr_printf (M_UNKNOWN_MALLOC0, opt);
    }
}

/* Modify SIZE (if necessary) according to the behavior of malloc (0).
   ID identify the caller.  Returns the new size.  */
size_t
test_malloc0 (size_t size, char *id)
{
  if (size)
    return size;
  if (malloc0_behavior & MALLOC0_IS_WARNING)
    {
      chkr_report (M_C_MES_CK_ET);
      chkr_printf (M_MALLOC0_WARNING, id);
      chkr_disp_call_chain ();
    }
  if (malloc0_behavior & MALLOC0_IS_MALLOC1)
    return 1;
  else
    return 0;
}

uint
get_total_aged_size (struct mdesc *mdp)
{
#ifndef NO_AGE
  struct malloc_header *tmp;
  uint res = 0;
  
  tmp = mdp->_youngerblock;
  if (mdp->_agedblock)
    while (tmp != NULL_HEADER)
      {
        res += tmp->size - tmp->s_diff;
        tmp = tmp->info.aged.next;
      }
  return res;
#else /* !NO_AGE */
  return 0;
#endif /* NO_AGE */
}

  
/* Check internal structure of an heap. If something bad is found, display it.
 * This is very useful when you debug malloc
 */
void
__chkr_check_mdesc (struct mdesc *mdp)
{
  struct malloc_header *tmp;
  struct malloc_header *tmp1;
  int i;

  chkr_report (M_C_MES_CK_ET);
  
  /* If MDP is NULL, the user wants to check the standard heap.  We don't
   * call MD_TO_MDP because we don't create the sbrk-heap.  */
  if (mdp == NULL_MDESC)
    {
      mdp = __mmalloc_default_mdp;
      if (mdp == NULL_MDESC)
        {
          chkr_printf (M_C_NO_BLOCKS);
          return;
        }
    }
    
  /* Do nothing if there are no mdesc.  */
  if (_firstmdesc == NULL_MDESC || _lastmdesc == NULL_MDESC)
    {
      if (_firstmdesc != _lastmdesc)
        chkr_printf (M_C_FM_OR_LM_BAD);
      else
        chkr_printf (M_C_NO_BLOCKS);
      return;
    }
  
  /* Yet another message for the user.  */
  chkr_printf (M_C_CHECK_MDESC, mdp);
  
  /* Do nothing if 'mmalloc' was never called.  */
  if (mdp->_firstblock == NULL_HEADER ||
      mdp->_lastblock == NULL_HEADER)
    {
      if (mdp->_firstblock != mdp->_lastblock)
	chkr_printf (M_C_FB_OR_LB_BAD);
      else
	chkr_printf (M_C_NO_BLOCKS);
      return;
    }

  /* First checks.  */
  if (mdp->_firstblock->prev != NULL_HEADER)
    chkr_printf (M_C_FBLOCK_BAD);
  if (mdp->_lastblock->next != NULL_HEADER)
    chkr_printf (M_C_LBLOCK_BAD);

  /* Check for linearity: each next block must be after the block.
   * bl->next and bl->prev are checked here.  */
  tmp1 = NULL_HEADER;
  for (tmp = mdp->_firstblock; tmp; tmp = tmp->next)
    {
      if (tmp < tmp1)
	chkr_printf (M_C_BAD_LINEARITY, tmp, tmp1);
      if (tmp->prev != tmp1)
	chkr_printf (M_C_BAD_PLINK, tmp);
      if (tmp->next && ((char *) tmp->next - (char *) tmp != (tmp->size + HEADER_SIZE)))
	chkr_printf (M_C_BAD_BLOCK_SIZE, tmp);
      tmp1 = tmp;
    }
  if (tmp1 != mdp->_lastblock)
    {
      chkr_printf (M_C_BAD_LAST_BLOCK);
      chkr_printf (M_C_STOP_HERE);
      return;
    }

  /* Clean the MDCHECK flag.  */
  for (tmp = mdp->_firstblock; tmp; tmp = tmp->next)
    tmp->state &= ~MDCHECK;

#ifndef NO_AGE
  /* Check all aged blocks.  */
  i = 0;
  tmp = mdp->_youngerblock;
  if (mdp->_agedblock == 0)
    {
      if (tmp != NULL_HEADER)
	chkr_printf (M_C_U_AGED_BLOCK, tmp);
    }
  else
    {
      if (tmp->info.aged.prev != NULL_HEADER)
	chkr_printf (M_C_BAD_PL_YOUNGB, tmp);
      while (tmp != NULL_HEADER)
	{
	  if (tmp->state != MDAGED)
	    chkr_printf (M_C_NON_AGED_BLOCK, tmp);
	  tmp->state |= MDCHECK;
	  tmp = tmp->info.aged.next;
	  i++;
	}
    }
  if (i != mdp->_agedblock)
    chkr_printf (M_C_BAD_NUM_AGED_B, i, mdp->_agedblock);
  if (i > aged_queue_length)
    chkr_printf (M_C_TOO_AGED_BLOCK, i, aged_queue_length);
#endif /* NO_AGE */

  /* Check all free blocks.  */
  for (i = 0; i < HASH_SIZE; i++)
    {
      tmp = mdp->_heapinfo[i];
      if (tmp == NULL_HEADER)
	continue;
      if (tmp->info.free.prev != NULL_HEADER)
	chkr_printf (M_C_BAD_PL_F_FREEB, tmp);
      tmp1 = NULL_HEADER;
      while (tmp)
	{
	  if (tmp->state != MDFREE)
	    {
	      chkr_printf (M_C_FREEB_BAD_STAT, tmp);
	      break;
	    }
	  if (log_size (tmp->size) != i)
	    chkr_printf (M_C_BLOCK_BAD_LIST, tmp);
	  tmp->state |= MDCHECK;
	  if (tmp->info.free.prev != tmp1)
	    chkr_printf (M_C_BAD_PLIN_FREEB, tmp);
	  tmp1 = tmp;
	  tmp = tmp->info.free.next;
	}
#ifndef NO_HEAPINDEX
      if (mdp->_heapindex < i)
	chkr_printf (M_C_BAD_HEAPINDEX);
#endif /* NO_HEAPINDEX */
    }

  /* Are there other free blocks ?  */
  for (tmp = mdp->_firstblock; tmp; tmp = tmp->next)
    {
      if (tmp->state == MDFREE)
	chkr_printf (M_C_UNKNOWN_FREEB, tmp);
      /* Clean the flag.  */
      tmp->state &= ~MDCHECK;
    }

  /* Are there consecutive free blocks ?  */
  for (tmp = mdp->_firstblock; tmp; tmp = tmp->next)
    {
      if (tmp->state == MDFREE && tmp->next != NULL_HEADER && tmp->next->state == MDFREE)
	chkr_printf (M_C_FREE_AND_FREE, tmp);
    }
  /* TODO : check for no coalision
           good order in free lists */
}

/* Check each heap.  */
void
__chkr_check_intern (void)
{
  struct mdesc *tmp1;
  struct mdesc *tmp;
  
  chkr_report (M_C_MES_CK_ET);
  
  /* Do nothing if there are no mdesc.  */
  if (_firstmdesc == NULL_MDESC || _lastmdesc == NULL_MDESC)
    {
      if (_firstmdesc != _lastmdesc)
        chkr_printf (M_C_FM_OR_LM_BAD);
      else
        chkr_printf (M_C_NO_BLOCKS);
      return;
    }
    
  /* Check for links.  */
  tmp1 = NULL_MDESC;
  for (tmp = _firstmdesc; tmp; tmp = tmp->info.inmem.next_mdesc)
    {
      if (tmp->info.inmem.prev_mdesc != tmp1)
	chkr_printf (M_C_BAD_MPLINK, tmp);
      tmp1 = tmp;
    }
  if (tmp1 != _lastmdesc)
    {
      chkr_printf (M_C_BAD_LAST_MDESC);
      chkr_printf (M_C_STOP_HERE);
      return;
    }
  
  /* Check each heap.  */
  for (tmp = _firstmdesc; tmp; tmp = tmp->info.inmem.next_mdesc)
    __chkr_check_mdesc (tmp);
}

/* Display some infos about each heap.  */
void
__chkr_disp_heaps (void)
{
  struct mdesc *mdp;
  
  chkr_report (M_C_MES_CK_ET);
  
  mdp = _firstmdesc;
  if (mdp == NULL_MDESC)
    chkr_printf (M_C_NO_BLOCKS);
  else
    {
      chkr_printf(" mdesc*    |       base-breakval   | fd\n");
      while (mdp)
        {
          chkr_printf ("0x%08x : 0x%08x-0x%08x | ", 
          		(int)mdp, (int)mdp->base, (int)mdp->breakval);
          if (mdp->flags & MMALLOC_DEVZERO)
            chkr_printf ("/dev/zero\n");
          else if (mdp->flags & MMALLOC_SBRK_HEAP)
            chkr_printf ("sbrk()\n");
          else
            chkr_printf ("%d\n", mdp->fd);
          mdp = mdp->info.inmem.next_mdesc;
        }
    }
}

/* Disp a graph of MDP. */
#define VIDEO_INVERSE "\033[7m"
#define VIDEO_NORMAL  "\033[27m"
void
__chkr_disp_heapgraph (struct mdesc *mdp)
{
#define NBR_GRAPH (80*20)
  struct malloc_header *tmp;
  char graph[NBR_GRAPH + 2];
  int char_len;
  int total_len;
  int size[2];
  int counter;
  char *g;
  char *graph_str;

  chkr_report (M_C_MES_CK_ET);
  
  if (mdp == NULL_MDESC)
    {
      mdp = __mmalloc_default_mdp;
      if (mdp == NULL_MDESC)
        {
          chkr_printf (M_C_NO_BLOCKS);
          return;
        }
    }
    
  tmp = mdp->_firstblock;
  if (tmp == NULL_HEADER)
    {
      chkr_printf (M_C_NO_BUSY_BLOCK);
      return;
    }
 
  total_len = (int)mdp->breakval - (int)mdp->base;
  char_len = total_len / (NBR_GRAPH) + 1;
  if (!char_len)
    char_len = 1;
  g = graph;
  size[0] = 0;
  size[1] = 0;
    
  chkr_printf ("Heapgraph:  ` ': free block `*': busy block  Each char is %d bytes\n", char_len);
  while (tmp != NULL)
    {
      if (tmp->state == MDBUSY)
        size[1] += tmp->size;
      else
        size[0] += tmp->size;
      if ((size[0] + size[1]) >= char_len)
        {
          if (size[1] >= size[0])
            graph_str = VIDEO_INVERSE "%s" VIDEO_NORMAL; /* busy: so */ 
          else
            graph_str = "%s";
          g = graph;
          counter = size[0] + size[1];
          size[0] = size[1] = 0;
          while (counter >= char_len)
            {
              *g++ = ' ';
              counter -= char_len;
            }
          *g = '\0';
          if (graph[1])
            {
              graph[0] = '[';
              *(g-1) = ']';
            }
          else
            graph[0] = '|';
          chkr_printf(graph_str, graph);
        }
      tmp = tmp->next;
    }
  chkr_printf ("\n");
}

/* Disp statistics for MDP.  */
void
__chkr_disp_statistics (struct mdesc *mdp)
{
  struct malloc_header *tmp;
  uint nbr_blocks_used = 0;
  uint nbr_blocks_free = 0;
  uint size_free_blocks = 0;
  uint size_used_blocks = 0;
  uint frag_index = 0;
  uint largest_used_block = 0;
  uint largest_free_block = 0;
  int prev_block_state = -1;
  uint total_len;

  chkr_report (M_C_MES_CK_ET);
 
  if (mdp == NULL_MDESC)
    {
      mdp = __mmalloc_default_mdp;
      if (mdp == NULL_MDESC)
        {
          chkr_printf (M_C_NO_BLOCKS);
          return;
        }
    }
    
  tmp = mdp->_firstblock;
  if (tmp == NULL_HEADER)
    {
      chkr_printf (M_C_NO_BUSY_BLOCK);
      return;
    }
 
  total_len = (int)mdp->breakval - (int)mdp->base;
  while (tmp != NULL)
    {
      if (tmp->state == MDFREE)
        {
          nbr_blocks_free++;
          size_free_blocks += tmp->size;
          if (tmp->size > largest_free_block)
            largest_free_block = tmp->size;
          if (prev_block_state == 1)
            frag_index++;
          prev_block_state = 0;
        }
      else
        {
          nbr_blocks_used++;
          size_used_blocks += tmp->size;
          if (tmp->size > largest_used_block)
            largest_used_block = tmp->size;
          if (prev_block_state == 0)
            frag_index++;
          prev_block_state = 1;
        }
      tmp = tmp->next;
    }
  chkr_printf ("Statistics for mdp 0x%08x:\n", (uint)mdp);
  chkr_printf ("Total length: %d\n", total_len);
  chkr_printf ("Free blocks: %d\n", nbr_blocks_free);
  chkr_printf ("Total size of free blocks: %d\n", size_free_blocks);
  chkr_printf ("Size of the largest free block: %d\n", largest_free_block);
  chkr_printf ("Busy blocks: %d\n", nbr_blocks_used);
  chkr_printf ("Total size of used blocks: %d\n", size_used_blocks);
  chkr_printf ("Size of the largest used block: %d\n", largest_used_block);
  if (nbr_blocks_used)
    chkr_printf ("Average size of used blocks: %d\n", size_used_blocks / nbr_blocks_used);
  total_len = nbr_blocks_used + nbr_blocks_free;
  if (total_len < 2)
    total_len = 0;
  else
    total_len = (100 * frag_index) / (total_len - 1);
  chkr_printf ("Fragmentation level: %d%%\n", total_len);
}
      
void
__chkr_dump_heap (struct mdesc *mdp)
{
  struct malloc_header *tmp;

  chkr_report (M_C_MES_CK_ET);
 
  if (mdp == NULL_MDESC)
    {
      mdp = __mmalloc_default_mdp;
      if (mdp == NULL_MDESC)
        {
          chkr_printf (M_C_NO_BLOCKS);
          return;
        }
    }
    
  tmp = mdp->_firstblock;
  if (tmp == NULL_HEADER)
    {
      chkr_printf (M_C_NO_BUSY_BLOCK);
      return;
    }

  chkr_printf (M_C_BUSY_BLOCK_ARE);

#if 0
  while (tmp != NULL)
    {
      if (tmp->state == MDBUSY)
	chkr_printf (M_C_BLOC_N_IS_BUSY, tmp, tmp->size / 1024);
      else if (tmp->state == MDINTRN)
	chkr_printf ("MDINTRN: block at 0x%x, size %dkb\n", tmp, tmp->size / 1024);
      tmp = tmp->next;
    }
#else
  while (tmp != NULL)
    {
      chkr_printf ("%p (start: %p): ", tmp, (char*)tmp + HEADER_SIZE);
      switch (tmp->state)
	{
	case MDBUSY:
	  chkr_printf ("BUSY ");
	  switch (tmp->garbage_t)
	    {
	    case POINT_NOT:
	      chkr_printf ("(no) ");
	      break;
	    case POINT_SURE:
	      chkr_printf ("(sure) ");
	      break;
	    case POINT_MAYBE:
	      chkr_printf ("(maybe) ");
	      break;
	    case POINT_LEAK:
	      chkr_printf ("(leak) ");
	      break;
	    case POINT_SEEN:
	      chkr_printf ("(seen) ");
	      break;
	    default:
	      chkr_printf ("(%d?) ", tmp->garbage_t);
	   }
	  break;
	case MDFREE:
	  chkr_printf ("FREE ");
	  break;
	case MDINTRN:
	  chkr_printf ("INTERN ");
	  break;
	case MDAGED:
	  chkr_printf ("AGED ");
	  break;
	case MDBRK:
	  chkr_printf ("BRK ");
	  break;
	}
      chkr_printf ("size %dkb (%d bytes)\n", (uint) tmp->size / 1024,
      					     (uint) tmp->size);
      tmp = tmp->next;
    }
#endif
}

/* Display the free blocks list */
void
__chkr_dump_free (struct mdesc *mdp)
{
  int i;
  struct malloc_header *tmp;
  
  chkr_report (M_C_MES_CK_ET);
  
  if (mdp == NULL_MDESC)
    {
      mdp = __mmalloc_default_mdp;
      if (mdp == NULL_MDESC)
        {
          chkr_printf (M_C_NO_BLOCKS);
          return;
        }
    }

  for (i = 0; i < HASH_SIZE; i++)
    {
      tmp = mdp->_heapinfo[i];
      if (tmp == NULL_HEADER)
	continue;
      chkr_printf (M_C_BLOCK_SIZ_LESS, 1 << i);
      while (tmp != NULL_HEADER)
	{
	  chkr_printf (M_C_BLOCK_AT_SIZE, tmp, tmp->size / 1024);
	  tmp = tmp->info.free.next;
	}
    }
}

/* Display all informations about a block.  */
void
__chkr_dump_block (PTR block)
{
  struct mdesc *mdp;
  struct malloc_header *blk;
  PTR *ptr_on_block;

  mdp = find_mdesc(block);
  if (mdp) 
    blk = find_header (mdp, block, 1);
  else
    blk = NULL_HEADER;
  
  chkr_report (M_C_MES_CK_ET);
  
  if (blk == NULL_HEADER)
    {
      chkr_printf ("Block %p not found.\n", block);
      return;
    }
  chkr_printf ("Block %p (in fact %p):\n", block, blk);
  chkr_printf ("  prev = %p\n", blk->prev);
  chkr_printf ("  next = %p\n", blk->next);
  chkr_printf ("  size = 0x%x\n", (uint) blk->size);
  switch (blk->state)
    {
    case MDBUSY:
      chkr_printf ("  BUSY:\n");
      chkr_printf ("    real_size = 0x%x; real_begin = %p\n",
      		   blk->info.busy.real_size,
      		   (PTR) blk + HEADER_SIZE + be_red_zone);
      break;
    case MDFREE:
      chkr_printf ("   FREE:\n");
      chkr_printf ("    next = %p\n", blk->info.aged.next);
      chkr_printf ("    prev = %p\n", blk->info.aged.prev);
      break;
    case MDINTRN:
      chkr_printf ("   INTERN:\n");
      break;
    case MDAGED:
      chkr_printf ("   AGED:\n");
      chkr_printf ("    next = %p\n", blk->info.aged.next);
      chkr_printf ("    prev = %p\n", blk->info.aged.prev);
      break;
    case MDBRK:
      chkr_printf ("   BRK:\n");
      break;
    }
#if CHKR_SAVESTACK
  ptr_on_block = (PTR *) ((int) blk + blk->size + HEADER_SIZE - af_red_zone);
  chkr_load_symtab ();
  chkr_printf (M_BLOCK_ALLO_FRM);
  disp_block_history (ptr_on_block);
  if (blk->state == MDAGED)
    {
      chkr_printf (M_N_FREE_CALLED, get_age (mdp, blk));
      ptr_on_block = (PTR *) ((int) blk + HEADER_SIZE);
      disp_block_history (ptr_on_block);
    }
#endif /* CHKR_SAVESTACK */
}

/* Disp info about shared libs.  */
void
__chkr_disp_shlibs (void)
{
 struct object *obj;
 
 chkr_report (M_C_MES_CK_ET);
 
 chkr_printf ("begin    end      rights name path\n");
 for (obj = objects; obj; obj = obj->next)
   chkr_printf ("%08x %08x %c%c %c%c%c %s %s\n",
   	obj->org, obj->end,
   	obj->rights & OBJECT_TEXT ? 'T' : '-',
   	obj->rights & OBJECT_DATA ? 'D' : '-',
   	obj->rights & OBJECT_READ ? 'r' : '-',
   	obj->rights & OBJECT_WRIT ? 'w' : '-',
   	obj->rights & OBJECT_EXEC ? 'x' : '-',
   	obj->name,
   	obj->path);
}
