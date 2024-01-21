/* yForth? - Written by Luca Padovani (C) 1996/97
 * ------------------------------------------------------------------------
 * This software is FreeWare as long as it comes with this header in each
 * source file, anyway you can use it or any part of it whatever
 * you want. It comes without any warranty, so use it at your own risk.
 * ------------------------------------------------------------------------
 * Module name:     vm.c
 * Abstract:        The Virtual Machine on which is based the whole
 *                  forth interpreter.
 */

#include <stdio.h>
#include <signal.h>
#include "yforth.h"
#include "core.h"

/* "ip" is the Instruction Pointer of the Virtual Machine. "ip" points to
 * an array of "pfp", which stands for "primitive function pointer",
 * in other words an array of pointers to primitive functions.
 * Roughly speaking, primitive functions are the valid instructions of
 * the Virtual Machine.
 */

pfp *ip;			/* Instruction Pointer */

Cell *sp, *sp_top, *sp_base;	/* various stack pointers... */
Cell *rp, *rp_top, *rp_base;
Real *fp, *fp_top, *fp_base;
Cell *bp;

#ifdef DCELL_MEM
static union double_cell dcell;	/* Used for double-cell transfer */
#endif

/* stacks_recovery: called when an exception occurs, it sets all stack
 * ptrs to their original value.
 */
void 
stacks_recovery (void)
{
  sp = sp_top;
  rp = rp_top;
  fp = fp_top;
}

/* If double-cell transfer is realized with memory-copying, the following
 * auxiliary procedures are needed
 */
#ifdef DCELL_MEM
DCell 
get_dcell (Cell * ptr)
{
  dcell.d2.high = *ptr;
  dcell.d2.low = *(ptr + 1);
  return (dcell.d1);
}

void 
put_dcell (Cell * ptr, DCell d)
{
  dcell.d1 = d;
  *ptr = dcell.d2.high;
  *(ptr + 1) = dcell.d2.low;
}
#endif

/* sig_fpe_handler: signal handler for math exceptions */
void 
sig_fpe_handler (int sig)
{
  signal (SIGFPE, sig_fpe_handler);
  _error = E_FPE;
  _view_error_msg();
  longjmp(warm_start_jump, 1);
}

/* sig_segv_handler: signal handler for segmentation violation */
void 
sig_segv_handler (int sig)
{
  signal (SIGSEGV, sig_segv_handler);
  _error = E_SEGV;
  _view_error_msg();
  longjmp(warm_start_jump, 1);
}

/* init_signal: initialize signal handlers */
void 
init_signals ()
{
  signal (SIGFPE, sig_fpe_handler);
  signal (SIGSEGV, sig_segv_handler);
}
