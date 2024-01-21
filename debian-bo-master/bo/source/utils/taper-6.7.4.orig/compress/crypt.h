/*
   Time-stamp: <96/07/19 20:15:19 yusuf>

   $Id: crypt.h,v 1.2 1996/07/27 20:42:28 yusuf Exp $	

*/



/* crypt.h (dummy version) -- do not perform encryption
 * Hardly worth copyrighting :-)
 */

#ifdef CRYPT
#  undef CRYPT      /* dummy version */
#endif

#define RAND_HEAD_LEN  12  /* length of encryption random header */

#define zencode
#define zdecode
