
#include "irix5.h"
#undef UNIXSAVE

#define UNIXSAVE "unexsgi.c"
#ifdef IN_UNIXSAVE
#define emacs 
#define round_up round_up1
#define bss_end core_end
#endif
