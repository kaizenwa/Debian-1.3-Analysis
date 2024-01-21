/* ============================================================================
 *
 * File:	ot.h
 * Project:	ot - Optimizing Tcl 
 * Started:	12.02.94
 * Changed:	12.02.94
 *
 * Description:	This header file contains a few macro function definitions
 *              for simplifying the use of the ot and Tcl/Tk C libraries.
 *              And of course it contains prototypes for functions and
 *              tcl commands implemented by ot.
 *
 * ========================================================================= */

#ifndef _OT_H
#define _OT_H

#include <stdlib.h>
#include <tcl.h>
#include <tk.h>

/* ===== Defines */

#define round(x) (int)(((x) >= 0) ? (double)(x) + 0.5 : (double)(x) - 0.5)


/* Macros for tcl optimization (these assume the variable interp to
   point to the Tcl interpreter): */

#define tcl_invoke(cmd) \
  if (Ot_TclInvoke (interp, (cmd)) == TCL_ERROR) return TCL_ERROR;

#define tcl_getvar(var, x) \
  if (!((x) = Tcl_GetVar (interp, (var), TCL_LEAVE_ERR_MSG))) \
      return TCL_ERROR;

#define tcl_getvar2(var, ind, x) \
  if (!((x) = \
	Tcl_GetVar2 (interp, (var), (ind), TCL_LEAVE_ERR_MSG))) \
      return TCL_ERROR;

#define tcl_getint(str, x) \
  if (Tcl_GetInt (interp, (str), (int *)(x)) == TCL_ERROR) \
      return TCL_ERROR;

#define tcl_getdouble(str, x) \
  if (Tcl_GetDouble (interp, (str), (double *)(x)) == TCL_ERROR) \
      return TCL_ERROR;

#define tcl_getboolean(str, x) \
  if (Tcl_GetBoolean (interp, (str), (int *)(x)) == TCL_ERROR) \
      return TCL_ERROR;

#define ot_getint(var, ind, x) \
  if (Ot_GetInt (interp, (var), (ind), (int *)(x)) == TCL_ERROR) \
      return TCL_ERROR;

#define ot_getdouble(var, ind, x) \
  if (Ot_GetDouble (interp, (var), (ind), (double *)(x)) == TCL_ERROR) \
      return TCL_ERROR;

#define ot_getboolean(var, ind, x) \
  if (Ot_GetBoolean (interp, (var), (ind), (int *)(x)) == TCL_ERROR) \
      return TCL_ERROR;


/* ===== Prototypes of Utility Functions */

EXTERN int Ot_TclInvoke _ANSI_ARGS_((Tcl_Interp *interp, char *cmd));
EXTERN int Ot_GetInt _ANSI_ARGS_((Tcl_Interp *interp, char *var,
				  char *index, int *val));
EXTERN int Ot_GetDouble _ANSI_ARGS_((Tcl_Interp *interp, char *var,
				     char *index, double *val));
EXTERN int Ot_GetBoolean _ANSI_ARGS_((Tcl_Interp *interp, char *var,
				      char *index, int *val));


/* ===== Prototypes of Tcl Commands */
     
EXTERN int Ot_Init _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN Tcl_CmdProc ot_timer_Cmd;
EXTERN Tcl_CmdProc ot_maplist_Cmd;
#ifndef NO_TK
EXTERN Tcl_CmdProc ot_warp_pointer_Cmd;
#endif

#endif /* _OT_H */
