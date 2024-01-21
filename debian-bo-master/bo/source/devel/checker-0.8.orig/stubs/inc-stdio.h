/* The purpose of this file is to undefine some internal macros.  */
#ifndef __CHKR_STDIO_H__
#define __CHKR_STDIO_H__
#include_next <stdio.h>

#ifdef __CHECKER__
#undef	getc_unlocked
#undef	putc_unlocked
#undef	getchar_unlocked
#undef	putchar_unlocked
#undef	getc
#undef	putc
#if 0
#undef	getchar
#undef	putchar
#endif
#undef	clearerr
#undef	feof
#undef	ferror
#undef	fileno
#undef	_bufend
#undef	_bufsiz
#endif /* __CHECKER__ */
#endif /* __CHKR_STDIO_H__ */
