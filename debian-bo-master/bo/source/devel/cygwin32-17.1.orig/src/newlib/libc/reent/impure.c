#include <reent.h>

/* For the PowerPC eabi, force the section to be .sdata, so that we can link
   the code with -msdata.  */
#ifndef _REENT_ATTR
#if defined(__PPC__) && defined(_CALL_SYSV)
#define _REENT_ATTR __attribute__((__section__(".sdata")))
#else
#define _REENT_ATTR
#endif
#endif

static struct _reent inpure_data = _REENT_INIT (inpure_data);
struct _reent *_REENT_ATTR _impure_ptr = &inpure_data;
