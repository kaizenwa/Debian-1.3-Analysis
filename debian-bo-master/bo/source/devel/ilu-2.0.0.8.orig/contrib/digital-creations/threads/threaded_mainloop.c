/* $Id */

#include "threaded_mainloop.h"
/*#include <ilu.hh>*/

void 
initialize_threaded_mainloop(void)
{
  ILU_ERRS((bad_param, no_memory)) err;

  ilu_SetWaitTech(&threaded_wt);
  ilu_SetLockTech(&threaded_lt, &err); /* should I test for an error here? */
  ilu_SetMainLoop(&threaded_ml);
}

#ifdef C_THREADS_SUPPORT
void
initialize_threaded_C_runtime(void)
{
  initialize_threaded_mainloop();
  ILU_C_SetFork(threaded_ml_fork);
}
#endif /* C_THREADS_SUPPORT */

#if 0
void 
initialize_threaded_CPP_runtime(void)
{
  ILU_ERRS((bad_param, no_memory)) err;

  ilu_SetWaitTech(&threaded_wt);
  ilu_SetLockTech(&threaded_lt, &err);
  iluServer::iluSetMainLoop(&threaded_ml);
  iluServer::SetFork(threaded_ml_fork);
}
#endif

