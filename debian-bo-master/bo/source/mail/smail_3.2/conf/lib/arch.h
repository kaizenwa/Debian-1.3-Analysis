/* @(#) arch.h,v 1.5 1992/09/06 03:01:09 tron Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * arch:
 *	deal with hardware/architectural issues
 *
 *	Architecure-independent aspects of architecture configuration.
 *	To specify a new architecture, create a file under the smail
 *	conf/arch directory, and refer to the new architecture in the
 *	conf/EDIT_ME file.
 */

/*
 * Derive bit counts extended to other types
 *
 * If BITS_PER_LONG < 32 be careful!  Along with other things, the hash_str()
 * function in hash.c will need to be changed.
 */
#define BITS_PER_LONG	(sizeof(long)*BITS_PER_CHAR)
#define BITS_PER_INT	(sizeof(int)*BITS_PER_CHAR)
#define BITS_PER_SHORT	(sizeof(short)*BITS_PER_CHAR)

/*
 * pointer - same size as a pointer to a structure
 *
 * Setup the typedef so that `pointer' is the same size as a pointer to
 * a structure.  On most machines, this is a long.  On some machines where
 * long is larger than an int, `pointer' is an unsigned int.  Woe to machines
 * with hardware addresses < 19 bits.  (smail might not even fit in that case!)
 *
 * Assume:
 *	typedef long pointer;
 *	struct foo *p;		<-- some j-random pointer
 *	long v;			<-- same type as the typedef
 *	union offptr {
 *		struct foo *ptr;
 *		pointer addr;
 *	} x;			<-- union of a pointer and the typedef
 *
 * then the following must be true:
 *	x.addr = v;			  implies   v == (pointer)(x.ptr)
 *	x.addr = (pointer)p;		  implies   p == x.ptr
 *	x.addr == (pointer)(x.ptr)
 *	x.ptr  = p;			  implies   p == (struct foo *)(x.addr)
 *	x.ptr  = (struct foo *)v;	  implies   v == x.addr
 *	x.ptr  == (struct foo *)(x.addr)
 */
typedef POINTER_TYPE pointer;

/*
 * even bytes and odd bytes
 *
 * The hash system used by hash.c (on disk and in memory) relies on the
 * fact that BYTES_PER_ALIGN (see an arch file) will align things to even
 * byte boundary.  Any hash `pointer' that refers to an odd value (2n+1)
 * is taken to mean an offset rather than a real address.  Note that odd
 * valued pointers are never dereferenced.
 *
 * We need to be able to distinguish between even and odd addresses, and
 * to convert to/from even/odd addresseses.  On most machines this is a
 * trivial marco.  Brain damaged machines with brain damaged segment
 * addressing may have to form their own creative macros.
 *
 * Assume:
 *	#define NULL 0		<-- for the sake of this example
 *	struct foobar *ptr;	<-- ptr points to a BYTES_PER_ALIGN object
 *	pointer addr;		<-- addr is of a BYTES_PER_ALIGN object
 *
 * Then these conversion macros must perform the following:
 *   to_odd() and to_even() do not distroy data:
 *	addr = to_odd(ptr)  ==>  ptr == to_even(addr)
 *	addr = to_even(ptr) ==>  ptr == to_odd(addr)
 *   is_even() and is_odd() are reflexive:
 *	is_odd(addr)  != 0  ==>  is_even(to_even(addr)) == 0
 *	is_odd(ptr)   != 0  ==>  is_even(to_even(ptr)) == 0
 *	is_even(addr) != 0  ==>  is_odd(to_odd(addr)) == 0
 *	is_even(ptr)  != 0  ==>  is_odd(to_odd(ptr)) == 0
 *   NULL is an even pointer:
 *	is_even(NULL) != 0
 */
#define is_odd(addr) ((pointer)(addr)&0x1)	/* non-zero if odd pointer */
#define is_even(addr) (! is_odd(addr))		/* non-zero if even pointer */
#define to_odd(addr) ((pointer)(addr)|0x1)	/* from even or odd to odd */
#define to_even(addr) ((pointer)(addr)&(~0x1))	/* from even or odd to even */

/*
 * set_ptr(ptr,addr) places the numeric value of `addr' into the pointer `ptr'.
 * That is, we borrow the pointer `ptr' to store numeric data.
 *
 * get_ptr(ptr) converts a pointer `ptr' back into the value which was stored
 * into it by set_ptr().
 */
#define get_ptr(ptr)      ( *(pointer *)&(ptr) )
#define set_ptr(ptr,addr) ( get_ptr(ptr) = (pointer)(addr) )

/* if USE_ASCII, rewrite the tolower function */
#ifdef USE_ASCII
# ifdef ANSI_C
extern unsigned char lowcase[];		/* lower case conversion table */
extern unsigned char upcase[];		/* upper case conversion table */
# else	/* not ANSI_C */
extern char lowcase[];			/* lower case conversion table */
extern char upcase[];			/* upper case conversion table */
# endif	/* ANSI_C */
# define lowercase(c) (lowcase[c])	/* quick table lookup for lower case */
# define uppercase(c) (upcase[c])	/* quick table lookup for upper case */
#else
/* slower NON-ASCII way of case conversion */
# define lowercase(c) (islower(c) ? (char)(c) : (char)tolower(c))
# define uppercase(c) (isupper(c) ? (char)(c) : (char)toupper(c))
#endif
