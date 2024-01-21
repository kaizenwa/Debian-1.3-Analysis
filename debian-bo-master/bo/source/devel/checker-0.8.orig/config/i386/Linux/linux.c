/* Memory function for Linux systems.
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
#include "message.h"

/* Uggly */
#define MIN_ADDRESS	0x60

/* For is_ok_frame. */
static PTR min_address = (PTR) MIN_ADDRESS;

#ifndef linux
#error Your OS is not Linux. See README file
#endif

/*
 * Check to see if the stack frame is still cool.  We use the fact
 * that linux's stack is always in the same place to know when we've
 * strayed off the stack.  At this point continuing is rather useless,
 * so we punt.  This avoids a core dump and is about as unportable as
 * it gets.
 */
#define LINUX_STACK_MASK 0xbff00000
static int
is_ok_frame (PTR *t)
{
  if ((PTR)t < (PTR)&t)
    return 0;
  if (((unsigned int) t & LINUX_STACK_MASK) != LINUX_STACK_MASK)
    return 0;
  if (t[1] <= min_address)
    return 0;
  return 1;
}

/* Search pointer inside the stack segment.
 * Check between &dummy and the top of the stack */
void
search_stack (void)
{
 PTR dummy;	/* OK, we could use ptr or min, but dummy is clearer */
 PTR *ptr;

 /* This code is not really portable.
   We can know how the stack is growing
   We know where the stack pointer is
   But I don't know how to know the top of the stack.
   This top can change only while a process is created. (exec, fork, clone)
   So, just after main, we can insert code to know the top.
   An other way is to make a core-file ... 
   Or to try and waiting for SIGSEGV */
 for (ptr = &dummy; ptr < (PTR *)STACK_BASE; ptr++)
   {
     if (*ptr >= low_addr_heap && *ptr < high_addr_heap) /* does not point on heap */
       chkr_address (*ptr, 1);
   }    
}

/* Search pointer inside the stack segment.  However, they should be saved.  */
void
search_register (void)
{
  return;
}

void
chkr_get_history (PTR *ptr, int forget, int num)
{
#ifndef PLCHECKER
 PTR *frame;
 PTR *frame1;
 
 if (ptr)
   {
     if (!num)
       {
         *ptr = (PTR) 0;
         return;
       }
   }
 else
   {
     chkr_printf (M_STACK_FRAMES_ARE);
     chkr_load_symtab ();
   }
 
 /* Really configuration dependant. 
    We must know:
    1) How to access to the frame pointer from an arg
    2) How does the stack grows (easy)
    3) What is the first frame (difficult)
    For linux, I check the return address of the frame. If this address
     is inside (!?) crt0.o, this is the last frame.
    Of course, I could check the next frame pointer, but it is hazardous
      and I must known the top of the stack...
  */
 /* Get the stack pointer from &frame.  */
 frame1 = frame = (PTR*) &frame + 1;  /* 1 is 4 bytes */
 

 /* We must skip frames inside the Checker code.  */
 while (1)
   {
     if (!is_ok_frame (frame))
       goto end;
     if ((PTR) frame[1] < (PTR) &checker_text_begin
         || (PTR) frame[1] > (PTR) &checker_text_end)
       break;
     frame1 = frame;
     frame = (PTR*) *frame;
   }

#ifndef GCCCHECKER
   if (forget)
     frame = frame1;
#endif

#ifdef GCCCHECKER
 /* We must skip frames inside the stubs, except the last one.  */
 while (1)
   {
     if (!is_ok_frame (frame))
       goto end;
     frame1 = (PTR*) *frame;
     if ((PTR) frame[1] < (PTR) &checker_text_stubs_begin
         || (PTR) frame[1] > (PTR) &checker_text_stubs_end
         || (PTR) frame1[1] < (PTR) &checker_text_stubs_begin
         || (PTR) frame1[1] > (PTR) &checker_text_stubs_end)
       break;
     frame = frame1;
   }
#endif

 /* Special case: get the current ip.  */
 if (ptr && num == -1)
   {
     *ptr = (PTR) frame[1];
     return;
   }
   
 /* Save NUM frames.  */
 for (num--; num || !ptr; num--)
   {
     if (!is_ok_frame (frame))
       break;
     if (ptr)
       *ptr++ = (PTR) frame[1];
     else
       chkr_show_addr ((PTR) frame[1]);
     frame = (PTR *) *frame;
   }
   
 end:
#endif /* PLCHECKER */
 if (ptr)
   *ptr = (PTR) 0;
 return;
}

/* Fonction called by malloc.c:initialize() 
   It is called once time, and can use malloc/free (e.g. getcwd)
   search the full name.  */
void
chkr_initialize (void)
{
  /* Nothing to do.  */
  return;
}

/* This is called by parse-args.c(___chkr_init_chkr) */
void
chkr_init_machine (int *argc, char *argv[], char *envp[])
{
 /* Must be set now */
 low_addr_heap = (PTR)(objects->next->end);
 min_address = (PTR)(objects->org + MIN_ADDRESS);
}
