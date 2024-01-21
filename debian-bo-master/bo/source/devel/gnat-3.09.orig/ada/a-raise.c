/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                              A - R A I S E                               */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                            $Revision: 1.28 $                             */
/*                                                                          */
/*          Copyright (C) 1992-1997, Free Software Foundation, Inc.         */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* As a  special  exception,  if you  link  this file  with other  files to */
/* produce an executable,  this file does not by itself cause the resulting */
/* executable to be covered by the GNU General Public License. This except- */
/* ion does not  however invalidate  any other reasons  why the  executable */
/* file might be covered by the  GNU Public License.                        */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/* Routines to support runtime exception handling */

/* ??? We need this to define malloc on those machines that need it, but
   this is the wrong file when this is built for libgnat.a.  */

#include "config.h"
#include "a-ada.h"
#include "a-types.h"
#include "a-raise.h"
#include <stdio.h>

extern void (*system__tasking_soft_links__lock_task) ();
extern void (*system__tasking_soft_links__unlock_task) ();
extern void (*system__tasking_soft_links__abort_defer) ();  
extern Exception_Id system__task_specific_data__get_gnat_exception ();
extern int system__task_specific_data__get_message_length ();
extern void* system__task_specific_data__get_exc_stack_addr ();
extern void system__task_specific_data__set_message_length (int);
extern char *system__task_specific_data__get_message_addr ();
extern int *system__task_specific_data__get_jmpbuf_address ();

extern struct Exception_Data constraint_error;
extern struct Exception_Data  program_error;
extern struct Exception_Data  storage_error;

/* procedure called when an exception is raised and no handler will take care 
   of, the name of the exception is printed as well as the message if any */
void
__gnat_unhandled_exception (except)
     Exception_Id except;
{
  int msg_len = system__task_specific_data__get_message_length ();
  char *msg;
  static int adafinal_called = 0;

  /* we know we won't be able to recover so let's terminate cleanly */

  if (adafinal_called == 0) 
    {
      adafinal_called = 1;
      adafinal (); 
    }

  /* then print the unhandled exception name and message if any */

  fputs ("\nraised ", stdout);

  if (except->Full_Name)
    fputs (except->Full_Name, stdout);

  /* this should never happen */
  else
    fputs ("unhandled exception", stdout);


  if (msg_len > 0) 
    {
      msg = system__task_specific_data__get_message_addr ();
      msg [msg_len] = 0;
      fputs (" : ", stdout);
      fputs (msg, stdout);
    }

  fputs ("\n", stdout);

}

/* procedure that actually raise an exception using the longjmp method */ 
void
__gnat_raise_nodefer_with_msg (except)
     Exception_Id except;
{
  int *ptr = system__task_specific_data__get_jmpbuf_address ();

#if defined (sgi)
asm ("__gnat_raise_nodefer__check_jmpbuf:");
asm (".globl    __gnat_raise_nodefer__check_jmpbuf");
#endif

  system__task_specific_data__set_gnat_exception (except);
  if (ptr)
    {
      __builtin_longjmp ((void *) ptr, 1);
    }
  else 
    {
      __gnat_unhandled_exception (except);
#ifdef VMS
      {
	long prvhnd;
	void *sp;
	void *xsp = system__task_specific_data__get_exc_stack_addr ();

	/* Remove the exception vector so it won't intercept the call to
	   lib$stop() below. */
	sys$setexv (1, 0, 3, &prvhnd);

	/* Determine if we are on the exception stack or not. If on, then
           return, which will cause the original stack to be restored and
	   the original exception to be resignalled, making
           the traceback meaningful.  If not then call lib$stop() which
	   will trigger a traceback from here. */

	/* WARNING: Coordinated any change to the size of the exception
	   stack (8192) with the RTL. */
	asm ("bis $30,$30,%0" : "=r" (sp));
	if (sp < xsp && sp > xsp - 8192)
	  return;
	else
	  /* Inhibit the initial message line by passing the high order
	     bit.  This will prevent a confusing and meaningless message
             on why the traceback was triggered. */
	  lib$stop (0x10000000);
      }
#else
      exit (1);
#endif
    }
}

/* procedure called when an exception is raised with no message */
void
__gnat_raise_nodefer (except)
     Exception_Id except;
{
  /* note set to zero has special semantics for Assert_Failure case */
  system__task_specific_data__set_message_length (0);
  __gnat_raise_nodefer_with_msg (except);
}

/* procedure called when an exception is raised with no message and abort 
   must be defered */
void 
__gnat_raise (except)
     Exception_Id except;
{
  (*system__tasking_soft_links__abort_defer) ();
  __gnat_raise_nodefer (except);
}

/* procedure called when an exception is raised with a message and abort 
   must be defered */
void 
__gnat_raise_with_msg (except)
     Exception_Id except;
{
  (*system__tasking_soft_links__abort_defer) ();
  __gnat_raise_nodefer_with_msg (except);
}

void
__gnat_reraise (flag)
     int flag;
{
  Exception_Id except = system__task_specific_data__get_gnat_exception ();

  if (flag)
    __gnat_raise (except);
  else
    __gnat_raise_nodefer (except);
}

void
__gnat_raise_constraint_error ()
{
  __gnat_raise (&constraint_error);
}

void
__gnat_raise_program_error ()
{
  __gnat_raise (&program_error);
}

void *
__gnat_malloc (size)
     __SIZE_TYPE__ size;
{
  void *result;

  /* If size is -1, it means we have an overflow situation.   */
  if (size == (__SIZE_TYPE__) -1)
    __gnat_raise (&storage_error);

  /* Change size from zero to non-zero. We still want a proper pointer
     for the zero case because pointers to zero length objects have to
     be distinct, but we can't just go ahead and allocate zero bytes,
     since some malloc's return zero for a zero argument */

  if (size == 0)
    size = 1;

  (*system__tasking_soft_links__lock_task) ();

  result = (char *) malloc (size);

  (*system__tasking_soft_links__unlock_task) ();

  if (result == 0)
    __gnat_raise (&storage_error);

  return result;
}

void
__gnat_free (void *ptr)
{

  (*system__tasking_soft_links__lock_task) ();

  free (ptr);

  (*system__tasking_soft_links__unlock_task) ();

}
