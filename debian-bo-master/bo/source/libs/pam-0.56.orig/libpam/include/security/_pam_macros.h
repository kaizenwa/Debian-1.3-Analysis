#ifndef PAM_MACROS_H
#define PAM_MACROS_H

/*
 * All kind of macros used by PAM, but usable in some other
 * programs too.
 * Organized by Cristian Gafton <gafton@sorosis.ro>
 */

/* Good policy to strike out passwords with some characters not just
   free the memory */

#define _pam_overwrite(x) \
{ \
     register char *__xx__; \
     if ((__xx__=x)) \
          while (*__xx__) \
               *__xx__++ = '\0'; \
}

/*
 * Don't just free it, forget it too.
 */

#define _pam_drop(X) \
if (X) { \
    free(X); \
    X=NULL; \
}

/* some debugging code */

#ifdef DEBUG

/*
 * This provides the necessary function to do debugging in PAM.
 * Cristian Gafton <gafton@sorosis.ro>
 */

#include <stdio.h>
#include <sys/types.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>

/*
 * This is for debugging purposes ONLY. DO NOT use on live systems !!!
 * You have been warned :-) - CG
 *
 * to get automated debugging to the log file, it must be created manually.
 * _PAM_LOGFILE must exist, mode 666
 */

#ifndef _PAM_LOGFILE
#define _PAM_LOGFILE "/tmp/pam-debug.log"
#endif

static void _pam_output_debug_info(const char *file, const char *fn
				   , const int line)
{
    FILE *logfile;
    int must_close = 1;
    
    if (!(logfile = fopen(_PAM_LOGFILE,"a"))) {
        logfile = stderr;
        must_close = 0;
    }
    fprintf(logfile,"[%s:%s(%d)] ",file, fn, line);
    if (must_close) {
        fflush(logfile);
        fclose(logfile);
    }
}

static void _pam_output_xdebug_info(const char *last_fn, const char *last_call
				    , const int last_line
				    , const char *last_file)
{
    FILE *logfile;
    int must_close = 1;
    
    if (!(logfile = fopen(_PAM_LOGFILE,"a"))) {
        logfile = stderr;
        must_close = 0;
    }
    fprintf(logfile, "[%s:%s(%d)->%s()] ",
	    last_file, last_call, last_line, last_fn);
    if (must_close) {
        fflush(logfile);
        fclose(logfile);
    }
}

static void _pam_output_debug(const char *format, ...)
{
    va_list args;
    FILE *logfile;
    int must_close = 1;
    
    va_start(args, format);

    if (!(logfile = fopen(_PAM_LOGFILE,"a"))) {
        logfile = stderr;
        must_close = 0;
    }
    vfprintf(logfile, format, args);
    fprintf(logfile, "\n");
    if (must_close) {
        fflush(logfile);
        fclose(logfile);
    }

    va_end(args);
}
#undef _PAM_LOGFILE
#define D(x) { \
    _pam_output_debug_info(__FILE__, __FUNCTION__, __LINE__); \
    _pam_output_debug x ; \
} 

#else

#define D(x)

#endif /* DEBUG */

#endif  /* PAM_MACROS_H */
