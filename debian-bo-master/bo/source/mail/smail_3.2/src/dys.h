/*
#ident	"@(#)smail/src:RELEASE-3_2:dys.h,v 1.8 1996/02/28 14:26:25 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * dys.h:
 *	macros for dynamic string region functions.
 *
 *	The macros in this file provide a simple method of building strings in
 *	an environment where the final length requirements are not known.
 *	Thus, these macros automatically support growing of strings with
 *	xrealloc() when the current allocation is deemed insufficient.
 *
 *	The 'str' structure has three values: a character pointer, an index,
 *	and an allocation size.  The index is an offset from the character
 *	pointer to the current location being copied into and the allocation
 *	size is the current limit for the index, beyond which a call to
 *	xrealloc() is required.
 */

/* type for a dynamic string region */
struct str {
    char *p;				/* xmalloc'd text region */
    unsigned int i;			/* index into text region */
    unsigned int a;			/* current allocation size */
};

/* STR - access the string in question directly */
#define STR(sp)		((sp)->p)

/* STR_LEN - since we have the value handy and pre-calculated */
#define STR_LEN(sp)	((sp)->i)	/* remember, not i-1, zero offset! */

/* STR_CHECK - call X_CHECK (from alloc.h) for a string */
#define STR_CHECK(sp)	X_CHECK((sp)->p)

/* STR_BUMP - the basic quantum of allocation space */
#define STR_BUMP	64

/*
 * STR_INIT - initialize the variables for a dynamic string region
 * this macro should be called with the variables to be passed to
 * other macros in this package before those other macros are used
 */
#define STR_INIT(sp)					\
	(((sp)->a = STR_BUMP - sizeof(long)),		\
	 ((sp)->i = 0),					\
	 ((sp)->p = xmalloc((sp)->a)))

/*
 * STR_NEXT - allow access to the next character in a dynamic string
 * region, growing the region if no successor character currently
 * is allocated.  This can be used in the form:
 *
 *	STR_NEXT(p, character expression);
 *
 * to load successive characters into the string.
 */
#define STR_NEXT(sp, c) 				\
	{						\
	    if ((sp)->i >= (sp)->a) {			\
		(sp)->a += STR_BUMP;			\
		(sp)->p = xrealloc((sp)->p, (sp)->a);	\
	    }						\
	    (sp)->p[(sp)->i++] = (c);			\
	}

/*
 * STR_CAT - concatenate a string onto the end of a dynamic string
 * region, growing as necessary.
 *
 * This is now implemented as a function in string.c.
 */
#define STR_CAT(sp, s) 	str_cat((sp), (s))

/*
 * STR_DONE - finish building a dynamic string region.  This is not
 * required, though it will xrealloc a region to minimum length, which
 * may be useful if xmalloc and xrealloc call something besides the
 * stock malloc and realloc functions.
 */
#define STR_DONE(sp)	((sp)->p = xrealloc((char *) ((sp)->p),		\
					    (unsigned) ((sp)->i + 1)),	\
			 (sp)->a = (sp)->i + 1)

/*
 * STR_FREE - free a region, returning its storage to the free pool
 */
#define STR_FREE(sp)	(xfree((sp)->p))

/*
 * STR_ALIGN - if region index is not aligned add bytes to align it.
 */
#define STR_ALIGN(sp)	{ while ((sp)->i%BYTES_PER_ALIGN) STR_NEXT((sp), 0); }

/*
 * COPY_STRING - copy a C-style string to a new xmalloc'd region
 */
#define COPY_STRING(s)	(strcpy(xmalloc((unsigned) (strlen((s)) + 1)), (s)))
