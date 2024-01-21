/* Generic implementation of system-dependent functions.
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

#include "checker.h"
#include "machine.h"
#include "message.h"

/* To save or display the current history, we can use some GCC built in
   functions.  Define this macro to use them, if they are available on this
   machine.  */
#define USE_GCC_BUILTIN 1

/* The stack base.  */
#ifdef STACK_BASE
#define stack_base STACK_BASE
#else
/* Initialized in chkr_init_machine().  */
static PTR stack_base;
#endif

/* Uggly. */
#define MIN_ADDRESS	0x40

/* For is_ok_frame.  */
static PTR min_address = (PTR) MIN_ADDRESS;

extern void chkr_address (PTR ptr, int sure);

#ifdef USE_GCC_BUILTIN
/* Check to see if the stack frame is still cool.  The frame pointer T must 
   be inside the stack!  */
static int
is_ok_frame (PTR *t)
{
  int retval;
#ifdef STACK_GROWS_DOWNWARD
  retval = (t >= (PTR *) &t) && (t <= (PTR *) stack_base);
#else
  retval = (t >= (PTR *) stack_base) && (t <= (PTR *) &t);
#endif
  return retval;
}

/* The generic configuration.  GCC provides some builtins to fetch return
   addresses or frame pointers.  However, there is no builtin to fetch the
   next frame or the return address from a frame.  Furthermore, the builtins
   only accept an constant argument.
   As a result, the current history is at first saved in a static array, and
   then this array is used.  */
/* Maximum number of frames saved in this array.  If you change this value,
   you have to change save_current_history.  */
#define NBR_FRAMES 30

/* The static array.  */
static PTR return_address_array[NBR_FRAMES];

/* Save the return address N in the array, or 0 if it is not available.  */
#define SAVE_RETURN_ADDRESS(n)						\
do									\
  {									\
    if (!n || is_ok_frame (__builtin_frame_address (n - 1)))		\
      return_address_array[n] = __builtin_return_address (n);		\
    else								\
      return;								\
  }									\
while (0)

/* Save the current history in the array.  */
static void
save_current_history (void)
{
  int i;
  
  /* Initialize the array.  */
  for (i = 0; i < NBR_FRAMES; i++)
    return_address_array[i] = NULL;
    
#if 0
  /* We cannot use this code since the __builtin functions must have a
     constant argument.  */
  for (i = 0; i < NBR_FRAMES - 1; i++)
    SAVE_RETURN_ADDRESS (i);
#else
  /* So, we just expand the loop.  */
  SAVE_RETURN_ADDRESS (0);
  SAVE_RETURN_ADDRESS (1);
  SAVE_RETURN_ADDRESS (2);
  SAVE_RETURN_ADDRESS (3);
  SAVE_RETURN_ADDRESS (4);
  SAVE_RETURN_ADDRESS (5);
  SAVE_RETURN_ADDRESS (6);
  SAVE_RETURN_ADDRESS (7);
  SAVE_RETURN_ADDRESS (8);
  SAVE_RETURN_ADDRESS (9);
  SAVE_RETURN_ADDRESS (10);
  SAVE_RETURN_ADDRESS (11);
  SAVE_RETURN_ADDRESS (12);
  SAVE_RETURN_ADDRESS (13);
  SAVE_RETURN_ADDRESS (14);
  SAVE_RETURN_ADDRESS (15);
  SAVE_RETURN_ADDRESS (16);
  SAVE_RETURN_ADDRESS (17);
  SAVE_RETURN_ADDRESS (18);
  SAVE_RETURN_ADDRESS (19);
  SAVE_RETURN_ADDRESS (20);
  SAVE_RETURN_ADDRESS (21);
  SAVE_RETURN_ADDRESS (22);
  SAVE_RETURN_ADDRESS (23);
  SAVE_RETURN_ADDRESS (24);
  SAVE_RETURN_ADDRESS (25);
  SAVE_RETURN_ADDRESS (26);
  SAVE_RETURN_ADDRESS (27);
  SAVE_RETURN_ADDRESS (28);
#endif
  return_address_array[NBR_FRAMES - 1] = NULL;
}
#endif /* USE_GCC_BUILTIN */

/* Search pointer inside registers... */
void
search_register (void)
{
  return;
}

/* Search pointer inside the stack segment.
   Check between &dummy and the top of the stack */
void
search_stack (void)
{
 PTR dummy;	/* OK, we could use ptr or min, but dummy is clearer */
 PTR *ptr;

#ifdef STACK_GROWS_DOWNWARD
 for (ptr = &dummy; ptr < (PTR *) stack_base; ptr++)
#else
 for (ptr = (PTR *) stack_base; ptr < &dummy; ptr++)
#endif
   {
     if (*ptr >= low_addr_heap && *ptr < high_addr_heap) /* does not point on heap */
       chkr_address (*ptr, 1);
   }    
}

/* Save the stack (in fact, only the return addresses) or disp it.  */
void
chkr_get_history (PTR *ptr, int forget, int num)
{
 int index;

 if (ptr)
   {
     /* Return if no address to save.  */
     if (num == 0)
       {
         *ptr = (PTR) 0;
         return;
       }
   }
 else
   {
     /* Initial message. */
     chkr_printf (M_STACK_FRAMES_ARE);
     chkr_load_symtab ();
   }

#ifdef USE_GCC_BUILTIN

 /* Save the current history in the array.  */
 save_current_history ();
 
 index = 0;

 /* Skip frames inside Checker.  */ 
 while (return_address_array[index])
   if (return_address_array[index] > (PTR) &checker_text_begin
       && return_address_array[index] < (PTR) &checker_text_end)
     index++;
   else
     break;

 /* Skip frames inside the stubs, except the last one.  */
 while (return_address_array[index])
   if (return_address_array[index] > (PTR) &checker_text_stubs_begin
       && return_address_array[index] < (PTR) &checker_text_stubs_end
       && return_address_array[index+1] > (PTR) &checker_text_stubs_begin
       && return_address_array[index+1] < (PTR) &checker_text_stubs_end)
     index++;
   else
     break;
 
 /* Special case: Get the current ip.  */
 if (ptr && num == -1)
   {
     *ptr = return_address_array[index];
     return;
   }
   
 /* Save the following return address.  */
 for (num--; num || !ptr; num--)
   {
     if (!return_address_array[index])
       break;
     if (ptr)
       *ptr++ = return_address_array[index];
     else
       chkr_show_addr (return_address_array[index]);
     index++;
   }
#endif /* USE_GCC_BUILTIN */

 if (ptr)
   {
     /* Delimitator.  */
     *ptr = (PTR) 0;
   }
 return;
}

/* fonction called by malloc.c:initialize() 
 * It is called once time, and can use malloc/free (e.g. getcwd)
 * search the full name. 
 */
void
chkr_initialize (void)
{
 /* If checker is already initialized, return now.  */
 if (chkr_is_init)
   return;
}

/* This is called by parse-args.c(___chkr_init_chkr) */
void
chkr_init_machine (int *argc, char *argv[], char *envp[])
{
#ifndef STACK_BASE
 /* STACK_BASE can be not defined since it can be floating.  However, the
  * stack direction is not floating.  */
#ifdef STACK_GROWS_DOWNWARD
#define BASE(a,b) ((a) > (b) ? (a) : (b))
#else
#define BASE(a,b) ((a) < (b) ? (a) : (b))
#endif
  stack_base = (char*) argc;
  for (i = 0; i < argc; i++)
    {
      p = argv[i];
      stack_base = BASE (stack_base, p);
      p = argv[i] + strlen (argv[i]) + 1;
      stack_base = BASE (stack_base, p);
    }
  for (i = 0; envp[i]; i++)
    {
      p = envp[i];
      stack_base = BASE (stack_base, p);
      p = envp[i] + strlen (envp[i]) + 1;
      stack_base = BASE (stack_base, p);
    }
  stack_base = (char*) ((int)stack_base + ps - 1) & ~(ps-1);
#endif

 /* Must be set now */
 low_addr_heap = (PTR) (objects->next->end);
 min_address = (PTR) (objects->org + MIN_ADDRESS);
}
