/*
 * This function is needed to support vwscanw
 */

#if HAVE_CONFIG_H
#include <config.h>
#endif

#if !HAVE_VSSCANF
#include <stdarg.h>

#if defined(_IOREAD) && defined(_NFILE)
int vsscanf(const char *str, const char *format, va_list ap)
{
	va_list ap;

	/*
	 * This code should work on anything descended from AT&T SVr1.
	 */
	va_start(ap);
	FILE	strbuf;

	strbuf._flag = _IOREAD;
	strbuf._ptr = strbuf._base = (unsigned char*)str;
	strbuf._cnt = strlen(str);
	strbuf._file = _NFILE;

	return(vfscanf(&strbuf, format, ap));
}
#else
int vsscanf(const char *str, const char *format, va_list ap)
{
	/*
	 * You don't have a native vsscanf(3), and you don't have System-V
	 * compatible stdio internals.  You're probably using a BSD 
	 * older than 4.4 or a really old Linux.  You lose.  Upgrade
	 * to a current C library to win.
	 */
	return -1;	/* not implemented */
}
#endif
#else
extern void _nc_vsscanf(void);	/* quiet's gcc warning */
void _nc_vsscanf(void) { } /* nonempty for strict ANSI compilers */
#endif /* !HAVE_VSSCANF */
