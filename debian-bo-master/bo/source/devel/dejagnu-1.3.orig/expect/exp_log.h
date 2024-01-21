/* exp_log.h */

#include "expect_comm.h"
#include "exp_printify.h"

/* special version of log for non-null-terminated strings which */
/* never need printf-style formatting. */
#define logn(buf,length)  { \
			  if (logfile) fwrite(buf,1,length,logfile); \
			  if (debugfile) fwrite(buf,1,length,debugfile); \
			  }

#define dprintify(x)	((is_debugging || debugfile)?exp_printify(x):0)
/* in circumstances where "debuglog(printify(...))" is written, call */
/* dprintify instead.  This will avoid doing any formatting that would */
/* occur before debuglog got control and decided not to do anything */
/* because (is_debugging || debugfile) was false. */

EXTERN void exp_errorlog EXP_PROTOV(EXP_VARARGS(char *, arg1));
EXTERN void exp_log EXP_PROTOV(EXP_VARARGS(int, arg1));
EXTERN void exp_debuglog EXP_PROTOV(EXP_VARARGS(char *, arg1));
EXTERN void exp_nflog EXP_PROTO((char *buf, int force_stdout));
EXTERN void exp_nferrorlog EXP_PROTO((char *buf, int force_stdout));

extern FILE *debugfile;
extern FILE *logfile;
extern int logfile_all;

extern int is_debugging;	/* useful to know for avoid debug calls */
