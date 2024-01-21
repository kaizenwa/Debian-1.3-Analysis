
#ifndef _myerror_h
#define _myerror_h

/* Generic error message routines */

#include <stdio.h>
#include <stdarg.h>
#include <errno.h>

#ifdef __WIN95__
#include <windows.h>


inline void error(char *fmt, ...)
{
	char mesgbuf[BUFSIZ];
	va_list ap;

	va_start(ap, fmt);
	vsprintf(mesgbuf, fmt, ap);
	MessageBox(GetActiveWindow(), mesgbuf, "Error", MB_OK);
	va_end(ap);
}

inline void mesg(char *fmt, ...)
{
	char mesgbuf[BUFSIZ];
	va_list ap;

	va_start(ap, fmt);
	vsprintf(mesgbuf, fmt, ap);
	MessageBox(GetActiveWindow(), mesgbuf, "Note", MB_OK);
	va_end(ap);
}
#else

inline void error(char *fmt, ...)
{
	char mesg[BUFSIZ];
	va_list ap;

	va_start(ap, fmt);
	vsprintf(mesg, fmt, ap);
	fputs(mesg, stderr);
	va_end(ap);
}

inline void mesg(char *fmt, ...)
{
	char mesg[BUFSIZ];
	va_list ap;

	va_start(ap, fmt);
	vsprintf(mesg, fmt, ap);
	fputs(mesg, stdout);
	va_end(ap);
}

#endif /* Win95 */

#define perror(msg)	myperror(msg)

inline void myperror(char *msg)
{
	char buffer[BUFSIZ];

	if ( *msg ) {
		sprintf(buffer, "%s: %s\n", msg, strerror(errno));
		error(buffer);
	} else
		error(strerror(errno));
}

#endif /* _myerror_h */

