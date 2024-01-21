#ifndef _BYTESEX_H
#define _BYTESEX_H

#undef __BYTE_ORDER

#if defined(__i386__)
# define __BYTE_ORDER	1234
#elif defined(__mc68000__)
# define __BYTE_ORDER	4321
#else
# error architecture not supported by Linux C library
#endif

#endif /* _BYTESEX_H */
