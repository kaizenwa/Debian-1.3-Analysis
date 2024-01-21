/*
#ident	"@(#)smail/src:RELEASE-3_2:alloc.h,v 1.7 1996/05/29 18:48:16 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * alloc.h:
 *	block storage allocation
 * 	    This allows a given stroage allocation to be associated 
 *	    with a group of other storage allocations.  It is
 *	    possible to free or test for existence of the class.
 *
 *	    A block a pointer to a chain of segments.  Each segment
 *	    refers to one storage allocation.  A block also contains
 *	    the total number of segments allocated.  If this number is
 *	    zero, then no stroage is associated with the block.
 */

/*
 * block allocation data structure
 */
struct block {
    struct bseg *next;	/* if cnt > 0 then next holds segment chain */
    int cnt;		/* number of segments in the block */
};
struct bseg {
    struct bseg *next;	/* if != NULL, then next alloc in block */
    char *data;		/* the storage allocated */
};

/*
 * handy macros to determine if a block is active
 */
#define is_memory(block_ptr) ((struct block *)(block_ptr)->cnt)
#define is_free(block_ptr) (!(struct block *)(block_ptr)->cnt)

/*
 * backward compat macros for pmalloc() - XXX
 */
#define pmalloc(size) (bmalloc((size),perm))
#define prealloc(data,size) (brealloc((data),(size),perm))
#define pfree(data) (bfree((data),perm))

/*
 * X_CHECK - check for the xmalloc magic number
 *
 * As a debugging aid, the integer X_MAGIC is stored at the beginning
 * of each block allocated with xmalloc().  This integer is then
 * cleared after a call to xfree().  This macro checks for the magic
 * characters and calls panic if they do not exist.
 */
#define X_MAGIC	  ((ALIGNED_TYPE)0xe8f987b1)
#define X_CHECK(p)							\
	(((ALIGNED_TYPE *)(p))[-1] != X_MAGIC?					\
		(write_log(LOG_PANIC,					\
			"X_CHECK failed!  ptr=0x%lx, line=%d, file=%s", \
			(long)(p), __LINE__, __FILE__),			\
		 x_dont_panic?						\
		    FAIL:						\
		    (abort(), 0))					\
	    : SUCCEED)
 
/* use these macros to panic code which should not generate a panic */
#define X_NO_PANIC() (x_dont_panic = TRUE)
#define X_PANIC_OKAY() (x_dont_panic = FALSE)
