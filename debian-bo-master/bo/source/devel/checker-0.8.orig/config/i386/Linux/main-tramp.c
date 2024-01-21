/* Used to build main-tramp.S.  Do not remove main-tramp.S.  */
#include "checker_api.h"

int
startup_call_main (int argc, char *argv[], char *envp[])
{
  chkr_set_right (&argc, sizeof (argc), CHKR_RW);
  chkr_set_right (&argv, sizeof (argv), CHKR_RW);
  chkr_set_right (&envp, sizeof (envp), CHKR_RW);
  
  main (argc, argv, envp);
}
