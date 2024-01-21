#ifndef _SLRN_TTYMSG_H
#define _SLRN_TTYMSG_H

#include <stdio.h>
#include <stdarg.h>

extern void slrn_tty_vmessage (FILE *, char *, va_list);
extern void slrn_tty_error (char *, ...);
extern void slrn_tty_message (char *, ...);
extern int slrn_message (char *, ...);
extern int slrn_message_now (char *, ...);
extern void slrn_error (char *, ...);
extern void slrn_error_now (char *, ...);

#endif				       /* _SLRN_TTYMSG_H */
