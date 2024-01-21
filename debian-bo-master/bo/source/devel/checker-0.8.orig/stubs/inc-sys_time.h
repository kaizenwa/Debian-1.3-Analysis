#ifndef __CHKR_SYS_TIME_H__
#define __CHKR_SYS_TIME_H__
#include_next <sys/time.h>

#ifdef __CHECKER__
#undef FD_CLR
#undef FD_ISSET
#undef FD_SET
#undef FD_ZERO
#endif /* __CHECKER__ */
#endif /* __CHKR_SYS_TIME_H__ */
