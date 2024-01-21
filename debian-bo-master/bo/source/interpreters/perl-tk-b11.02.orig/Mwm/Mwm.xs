/*
  Copyright (c) 1995 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.
*/

#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include "tkGlue.def"

#include "pTk/tkPort.h"
#include "pTk/tkInt.h"
#include "pTk/tkVMacro.h"
#include "pTk/tix.h"
#include "pTk/tixInt.h"
#include "tkGlue.h"
#include "tkGlue.m"

extern int Tix_MwmCmd _ANSI_ARGS_((ClientData,Tcl_Interp *,int, Arg *));

DECLARE_VTABLES;

MODULE = Tk::Mwm	PACKAGE = Tk::Mwm

PROTOTYPES: DISABLE

BOOT:
 {
  IMPORT_VTABLES;
  /* Initialize the display item types */
  Lang_TkCommand("mwm",Tix_MwmCmd);
 }
