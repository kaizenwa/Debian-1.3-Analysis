/* Errors list of Checker.
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written August 1993 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/
/* Note about the errors:
   The user can disable or suppress an error.
    * disabled in any case
    * suppressed by a function history
 */
#include "checker.h"
#include "errlist.mes"
#include <signal.h>

/* chkr_errno totaly replaces errno: the syscalls use chkr_errno.  By not
   using errno, we don't have to save it.  */
int chkr_errno;

/* This is the list of errors.  Private.  */
struct error_t
{
  char *name;
  int disable;		/* True if disabled.  */
  int nbr_funcs;	/* Not null if suppressed.  */
  int always_enable;	/* True if can't be suppressed/disabled.  */
};

struct error_t error_type[] = {
{ M_IE_ET, 0, 0, 0},	   /* "*internal error*" */
{ M_I_IEI_CC_ET, 0, 0, 1}, /**"(iei) internal error: instruction unknown." */
{ M_U_NZA_NZ_ET, 0, 0, 0}, /**"(nza) null zone addressed." */
{ M_W_WOB_TE_ET, 0, 0, 0}, /**"(wro) write/modify read-only byte(s)." */
{ M_U_BVH_HE_ET, 0, 0, 0}, /**"(bvh) block bounds violation in the heap." */
{ M_R_RFB_HE_ET, 0, 0, 0}, /**"(rfb) read in a free block." */
{ M_W_WFB_HE_ET, 0, 0, 0}, /**"(wfb) write/modify a free block." */
{ M_R_RUH_HE_ET, 0, 0, 0}, /**"(ruh) read uninitialized byte(s) in a block." */
{ M_R_RUS_ST_ET, 0, 0, 0}, /**"(rus) read uninitialized byte(s) in the stack." */
{ M_R_RZS_ST_ET, 0, 0, 0}, /**"(rsz) read on a red zone in the stack." */
{ M_W_WZS_ST_ET, 0, 0, 0}, /**"(wzs) write on a red zone in the stack." */
{ M_M_WUS_ST_ET, 0, 0, 0}, /**"(wus) modify uninitialized byte(s) in the stack." */
{ M_M_MZS_ST_ET, 0, 0, 0}, /**"(mzs) modify a red zone in the stack." */
{ M_U_NMA_VD_ET, 0, 0, 0}, /**"(nma) no memory addressed." */
{ M_U_BVM_MM_ET, 0, 0, 0}, /**"(bvm) bounds violation in mapped memory." */
{ M_U_APD_MM_ET, 0, 0, 0}, /**"(apd) access permission denied in mapped memory." */
{ M_U_WRS_SH_ET, 0, 0, 0}, /**"(wrs) write in a read-only shared memory." */
{ M_E_ZNE_VD_ET, 0, 0, 0}, /**"(zne) zone not executable." */
{ M_A_SBV_SG_ET, 0, 0, 1}, /**"(sbv) segment bound violation." */
{ M_M_FBA_FR_ET, 0, 0, 1}, /**"(fba) free called with an address not obtained from malloc." */
{ M_M_FFB_FR_ET, 0, 0, 1}, /**"(ffb) free an already free block." */
{ M_M_FNL_FR_ET, 0, 0, 1}, /* "(fnl) free called with a null argument." */
{ M_M_SBA_BR_ET, 0, 0, 1}, /**"(sba) sbrk called with a bad offset." */
{ M_M_FBM_FR_ET, 0, 0, 1}, /**"(fbm) free called before malloc." */
{ M_M_OOM_MC_ET, 0, 0, 0}, /**"(oom) warning: virtual memory exhausted." */
{ M_M_MNA_AL_ET, 0, 0, 1}, /**"(mna) memalign called with a null alignment." */
{ M_M_MBA_AL_ET, 0, 0, 0}, /**"(mba) alignment is not a power of 2." */
{ M_I_IEB_MA_ET, 0, 0, 1}, /**"(ieb) internal error while checking." */
{ M_I_BBS_MA_ET, 0, 0, 1}, /**"(bbs) bad bytes_per_state value. Use default value." */
{ M_I_BKC_MA_ET, 0, 0, 0}, /**"(bkc) brk/sbrk has been called." */
{ M_I_OOS_SM_ET, 0, 0, 1}, /**"(oos) out of system memory." */
{ M_I_BRS_MA_ET, 0, 0, 1}, /**"(brs) bad red zone size." */
{ M_I_TMS_SG_ET, 0, 0, 1}, /**"(tms) too many signal catched at the same time." */
{ M_I_IES_SC_ET, 0, 0, 1}, /**"(ies) internal error: can't check this syscall." */
{ M_I_BFD_SC_ET, 0, 0, 1}, /**"(bfd) file descriptor reserved by Checker is used." */
{ M_I_EFD_SC_ET, 0, 0, 1}, /* "(efd) file descriptor out of range."  */
{ M_I_SFD_SC_ET, 0, 0, 1}, /**"(sfd) the system has resturned a file descriptor reserved by Checker." */
{ M_C_MES_CK_ET, 0, 0, 1}, /**"(mes) debugging message for the user." */
{ M_I_REC_MA_ET, 0, 0, 1}, /**"(rec) recursive use of malloc." */
{ M_I_SIG_SG_ET, 0, 0, 1}, /**"(sig) signal." */
{ M_I_BOC_SG_ET, 0, 0, 1}, /**"(bda) bad option for Checker." */
{ M_I_GAR_MA_ET, 0, 0, 1}, /**"(gar) garbage detector results." */
{ M_I_PRF_MA_ET, 0, 0, 1}, /**"(prf) profil results." */
{ M_M_BAL_AL_ET, 0, 0, 1}, /**"(bal) bad alignment." */
{ M_C_FUN_LB_ET, 0, 0, 1}, /**"(fun) function warning." */
{ 0            , 0, 0, 0}};

/* Abort routine.  Call when Checker can't be run anymore.  */
void
chkr_abort (void)
{
#ifndef MDCHECKER
  struct sigaction action;
  sigset_t set;
  static int in_abort;

  /*  The current history could be interesting.  */
  if (flag_verbose && !in_abort)
    {
      in_abort = 1;
      chkr_get_history (0, 0, 0);
    }
    
  /* Do the last actions (clean up).  */
  chkr_do_end ();

  /* A last warning.  */
#define MESSAGE "Internal error in Checker.  Abort.\n"
  write (chkr_out, MESSAGE, sizeof (MESSAGE) - 1);
  
  /* Be sure about the behavior of SIGABRT.  */
  action.sa_handler = SIG_DFL;
  chkr_sigaction (SIGABRT, &action, 0);
  __sigemptyset (&set);
  __sigaddset (&set, SIGABRT);
  sigprocmask (SIG_UNBLOCK, &set, 0);
  kill (getpid (), SIGABRT);
  
  /* Why not.  */
  _exit (250);
#else
  abort();
#endif /* MDCHECKER */

  /* This is because of ((noreturn)) */
  while (1)
    ;
}

/* Return the type number of the error NAME.  NAME must be a (at least) 3 
   letter string.  */
int
get_error_type_by_name (const char *name)
{
 struct error_t *et = error_type;
 int i;
 
 for (i = 0; (et->name)[0] != '\0'; i++, et++)
   {
     if (strncmp (name, et->name + 1, 3) == 0)
       return i;
   }
 return -1;
}

/* Return the error name by a number.  The string is kept in a static
   buffer.  */
char *
get_error_name_by_number (int num)
{
  struct error_t *et;
  static char buf[4];
  
  if (num == -1)
    return "all";
  et = &error_type[num];
  buf[0] = et->name[0];
  buf[1] = et->name[1];
  buf[2] = et->name[2];
  buf[3] = '\0';
  return buf;
}

/* Record that the error TYPE can be disable by function history.  n_funcs is
   the maximum depth of the history.  */
void
record_suppress (int type, int n_funcs)
{
  if (error_type[type].nbr_funcs < n_funcs)
    error_type[type].nbr_funcs = n_funcs;
}

/* Return the maximum depth history for suppress.  */
int
get_max_nbr_funcs_suppressed (int type)
{
  return error_type[type].nbr_funcs;
}

/* Return TRUE if the error E is disabled.  */
inline int
is_error_disable (int e)
{
  return error_type[e].disable;
}

/* Disable error E.  */
inline void
disable_error (int e)
{
  if (e != -1 && !error_type[e].always_enable)
    error_type[e].disable = 1;
}

/* Disable an error.  STR can be a type (3 letters), an address or a range.
   Called by parse-args.c.  */
int
parse_disable (const char *str)
{
  /* If STR begins with a letter, it is a type.  The action is to disable the
     type.  */
  if (str[0] >= 'a' && str[0] <= 'z')
    {
      int n;
      
      /* STR is a type and should be 3 letters.  */
      if (strlen (str) != 3)
        return 0;

      n = get_error_type_by_name (str);
      if (n != -1)
        {
          disable_error (n);
          return 1;
        }
      else
        return 0;
    }
  else
#ifdef MDCHECKER
    return 0;
#else
    {
      char *arg;
      char *c;
      uint low, high;
      
      /* We work on a local copy.  */
      arg = alloca (strlen (str) + 1);
      strcpy (arg, str);
  
      c = strchr (arg, '-');
      if (c == (char*) 0)
        low = high = atod (arg);
      else
        {
          *c = 0;
          low = atod (arg);
          high = atod (c + 1);
        }
#ifndef GET_CURRENT_IP
      chkr_printf ("Can't handle `%s' (I don't know the current ip).\n", str);
      return 0;
#else
      return register_disable_range (low, high);
#endif
    }
#endif
}


/* This is a dummy function called after each error report.  The user can set
   a breakpoint on it.  */
#ifdef MDCHECKER
void
__chkr_pause (void)
{
}
#endif

/* Display the name of the error and the current stack frames.  Return 0 if
   it is disabled.  */
int
chkr_perror (int e)
{
 /* Don't display it if disabled (and could be disabled).  */
 if (is_error_disable (e))
   return 0;
   
 chkr_header (error_type[e].name);
 chkr_printf ("\n");
 chkr_disp_call_chain ();
 return 1;
}

/* Display the error E if it is not disable.  Return 0 if it is disabled.  */
int
chkr_report (int e)
{
  if (is_error_disable (e))
    return 0;
  chkr_header (error_type[e].name);
  chkr_printf ("\n");
  return 1;
}

/* Disp the current call chain.  */
int
chkr_disp_call_chain (void)
{
#ifdef CHKR_SAVESTACK
 chkr_get_history (0, 0, 0);
#endif  /* CHKR_SAVESTACK */
 __chkr_pause ();
 return 1;
}
