/* work around until gcc's <float.h> is correct */

#ifndef _MIN_EVAL_FORMAT
#define _MIN_EVAL_FORMAT  2
#define _WIDEST_NEED_EVAL 0 /* does not matter anyway */
#endif

#include_next <float.h>
