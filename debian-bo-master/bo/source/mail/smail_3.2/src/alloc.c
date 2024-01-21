/*
#ident	"@(#)smail/src:RELEASE-3_2:alloc.c,v 1.7 1996/05/29 18:48:14 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * alloc.c:
 *	storage allocation with panics on errors
 *	    The functions in this file may be replaced in the future when
 *	    daemon mode is implemented, to make it easier to reclaim
 *	    storage.
 *
 *	block storage allocation
 *	    This allows a given stroage allocation to be associated 
 *	    with a group of other storage allocations.  It is
 *	    possible to free or test for existence of the class.
 *
 *	    A block a pointer to a chain of segments.  Each segment
 *	    refers to one storage allocation.  A block also contains
 *	    the total number of bytes allocated.  If this number is
 *	    zero, then no stroage is associated with the block.
 *
 *	external functions: xmalloc, xrealloc, xfree, 
 *			    balloc, brealloc, bfree,
 *			    alloc_block, re_block, free_block
 */
#include <sys/types.h>
#include <stdio.h>
#include "defs.h"
#include "smail.h"
#include "exitcodes.h"
#include "log.h"
#include "alloc.h"
#ifndef DEPEND
# include "extern.h"
#endif

/* the master block of blocks */
static struct block master = {(struct bseg *)0, 0};

/* the block for pmalloc(), prealloc() and pfree() macros */
static struct block perm_block = {(struct bseg *)0, 0};
struct block *perm = &perm_block;

/* default block for 0 based life time */
static struct block default_block = {(struct bseg *)0, 0};
static struct block *def_block = &default_block;

/* variable exported for the X_CHECK macro */
int x_dont_panic = TRUE;              /* if TRUE, X_CHECK won't call panic */

extern char *malloc();
extern char *realloc();
extern void free();				/* int on SunOS */


/*
 * xmalloc, xrealloc, xfree - per message malloc and realloc and free
 *			      with aborts on error
 *
 * The following two routines are an interface to xmalloc and xrealloc
 * which frees other functions from having to worry about whether
 * a memory full error occurs.  There is seldom anything graceful
 * to do when this happens, and it happens seldom on machines with
 * a reasonable VM space.  So just panic about the problem and don't
 * bother returning.
 *
 * In the future, the x-style allocation routines will be used for
 * allocation of spaces which will be reclaimed after processing one
 * message.
 */
char *
xmalloc(size)
    unsigned size;			/* size of region to allocate */
{
    register char *ret;			/* region returned by malloc */

    ret = malloc(size + sizeof(ALIGNED_TYPE));
    if (!ret) {
	panic(EX_SOFTWARE, "malloc(%d) returned NULL", size);
	/*NOTREACHED*/
    }
    *(ALIGNED_TYPE *)ret = X_MAGIC;
    return ret + sizeof(ALIGNED_TYPE);
}

char *
xrealloc(region, size)
    char *region;			/* region to be realloc'd */
    unsigned size;			/* realloc size */
{
    register char *ret;			/* region returned by realloc */

    if (X_CHECK(region) == FAIL) {
	/* what to do with realloc that is reasonable to avoid a panic? */
	/* copy the region to an entirely new malloc'd area */
	ret = xmalloc(size);
	memcpy(ret, region, (size_t) size);
	return ret;
    }

    ((ALIGNED_TYPE *)region)[-1] = 0;	/* clear the magic number */
    ret = realloc(region - sizeof(ALIGNED_TYPE), size + sizeof(ALIGNED_TYPE));
    if (!ret) {
	panic(EX_SOFTWARE, "realloc(0x%lx, %u) returned NULL",
	      (long)region, size);
	/*NOTREACHED*/
    }
    *(ALIGNED_TYPE *)ret = X_MAGIC;	/* put a magic number back */
    return ret + sizeof(ALIGNED_TYPE);
}

void
xfree(region)
    char *region;
{
    if (X_CHECK(region) == FAIL) {
	/* it should be safe to ignore double free's */
	return;
    }
    ((ALIGNED_TYPE *)region)[-1] = 0;
    (void)free(region - sizeof(ALIGNED_TYPE));
}


/*
 * bmalloc - allocate memory and associate it with a block
 *
 * input:
 *	size - number of bytes to allocate
 *	block - the block to associate it with, 0 ==> master
 *
 * output:
 *	returns a pointer to the allocated storage
 */
char *
bmalloc(size, block)
    unsigned size;		/* number of bytes to allocate */
    struct block *block;	/* block that data belongs to */
{
    struct bseg *newseg;	/* the new segment for this alloc */

    /* a zero block implies master block */
    if (block == 0) {
	block = def_block;
    }

    /* add a new segment to the block */
    if ((newseg = (struct bseg *)malloc(sizeof(struct bseg))) == NULL) {
	panic(EX_SOFTWARE, "bmalloc: malloc of bseg failed");
	/* NOTREACHED */
    }
    newseg->next = block->next;
    block->next = newseg; 

    /* allocate the storage */
    if ((newseg->data = malloc(size)) == NULL) {
	panic(EX_SOFTWARE, "bmalloc: malloc of %d bytes failed", size);
	/* NOTREACHED */
    }
    ++block->cnt;

    /* return the pointer */
    return newseg->data;
}

/*
 * brealloc - reallocate stroage associated with a block
 *
 * Resize a storagg from a block, and perform all needed
 * accounting adjustments.
 *
 * input:
 *	data - pointer to data being realloced
 *	size - the new size
 *	block - the block that data belongs
 *
 * output:
 *	returns a pointer to the re-allocated storage
 */
char *
brealloc(data, size, block)
    char *data;			/* data being re-allocated */
    unsigned size;		/* number of bytes to re-allocate */
    struct block *block;	/* block that data belongs to */
{
    struct bseg *oldseg;	/* old segment associated with data */

    /* a zero block implies master block */
    if (block == 0) {
	block = def_block;
    }

    /* find the segment */
    for (oldseg=block->next;
	 oldseg != NULL && oldseg->data != data;
	 oldseg=oldseg->next) {
    }
    if (oldseg == NULL) {
	panic(EX_SOFTWARE, 
	    "brealloc: seg (at 0x%lx) not in block (at 0x%lx, cnt:%d)",
	    (long)oldseg, (long)block, block->cnt);
	/* NOTREACHED */
    }

    /* reallocate */
    if ((oldseg->data = realloc(data, size)) == NULL) {
	panic(EX_SOFTWARE, 
	    "brealloc: realloc to %d bytes (at 0x%lx) failed",
	    size, (long)oldseg->data);
	/* NOTREACHED */
    }

    /* return the pointer */
    return oldseg->data;
}

/*
 * bfree - free a signle segment from a block
 *
 * Free a storage from a block, and perform all needed accounting adjustments.
 *
 * input:
 *	data - pointer to data being freed
 *	block - the block that data belongs
 */
void
bfree(data, block)
    char *data;			/* data being freed */
    struct block *block;	/* block that data belongs to */
{
    struct bseg *oldseg;	/* old segment associated with data */
    struct bseg *lastseg;	/* the segment before the deleted segment */

    /* firewall */
    if (block == NULL) {
	panic(EX_SOFTWARE, "brealloc: block is NULL");
	/* NOTREACHED */
    }

    /* delete the segment from the block */
    for (lastseg=NULL, oldseg=block->next; 
	 oldseg!=NULL && oldseg->data!=data;
	 lastseg=oldseg, oldseg=oldseg->next) {
    }
    if (oldseg == NULL) {
	panic(EX_SOFTWARE, 
	    "bfree: data (at 0x%lx) not in block (at 0x%lx, cnt: %d)",
	    (long)data, (long)block, block->cnt);
	/* NOTREACHED */
    }
    if (lastseg == NULL) {
	block->next = oldseg->next;
    } else {
	lastseg->next = oldseg->next;
    }
    --block->cnt;

    /* free the data and segment */
    (void) free(oldseg->data);
    (void) free((char *)oldseg);
}


/*
 * malloc_block - form a new storage block
 *
 * Start a new storage block with no storage allociated with it.
 *
 * NOTE: the block structore becomes a segment from the master block
 *
 * input:
 *	none
 *
 * output:
 *	returns a pointer to the new block
 */
struct block *
malloc_block()
{
    struct block *block;	/* ptr to the new block */

    /* allocate the block */
    block = (struct block *) bmalloc((unsigned) sizeof(struct block), master);
    block->cnt = 0;
    block->next = NULL;

    /* return the new block */
    return block;
}

/*
 * realloc_block - move a segment from one block to another
 *
 * input:
 *	data - the data being moved
 *	oldblock - the block where the segment currently resides
 *	newblock - the block that will hold the new segment
 */
void
realloc_block(data, oldblock, newblock)
    char *data;			/* the data to move */
    struct block *oldblock;	/* the old block */
    struct block *newblock;	/* the new block */
{
    struct bseg *oldseg;	/* old segment associated with data */
    struct bseg *lastseg;	/* the segment before the deleted segment */

    /* firewall */
    if (oldblock == NULL && newblock == NULL) {
	panic(EX_SOFTWARE, "realloc_block: oldblock or newblock is NULL");
	/* NOTREACHED */
    }

    /* delete the segment from the oldblock */
    for (lastseg=NULL, oldseg=oldblock->next; 
	 oldseg!=NULL && oldseg->data!=data;
	 lastseg=oldseg, oldseg=oldseg->next) {
    }
    if (oldseg == NULL) {
	panic(EX_SOFTWARE, 
	    "realloc_block: data (at 0x%lx) not in block (at 0x%lx, cnt: %d)",
	    (long)data, (long)oldblock, oldblock->cnt);
	/* NOTREACHED */
    }
    if (lastseg == NULL) {
	oldblock->next = oldseg->next;
    } else {
	lastseg->next = oldseg->next;
    }
    --oldblock->cnt;

    /* add the segment to the new block */
    oldseg->next = newblock->next;
    newblock->next = oldseg; 
    ++newblock->cnt;
}

/*
 * free_block - free everything in a block
 *
 * input:
 *	block - the block to free all segments on
 */
void
free_block(block)
    struct block *block;	/* the new to free all segments on */
{
    struct bseg *oldseg;	/* the segement to free */

    /* firewall */
    if (block == NULL) {
	panic(EX_SOFTWARE, "free_block: block is NULL");
	/* NOTREACHED */
    }

    /* free the chain */
    for(oldseg = block->next; 
	block->cnt > 0 && block->next != NULL;
	--block->cnt, block->next = oldseg->next) {

	/* free the data */
	(void) free(block->next->data);
	/* free the segemtn */
	(void) free((char *)block->next);
	--block->cnt;
    }

    /* firewall */
    if (block->cnt != 0 || block->next != NULL) {
	panic(EX_SOFTWARE, 
	  "free_block: freed block (at 0x%lx) has cnt:%d, next:0x%lx",
	  (long)block, block->cnt, (long)block->next);
	/* NOTREACHED */
    }
}
