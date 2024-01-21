/* Memory function for Sparc Solaris2 systems.
   Copyright 1995 Tristan Gingold
		  Written April 1995 by Tristan Gingold

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

#include <sys/frame.h>
#include "checker.h"
#include "machine.h"
#include "message.h"

/* Uggly.  */
#define MIN_ADDRESS	0x60

/* For is_ok_frame.  */
static PTR min_address = (PTR) MIN_ADDRESS;

extern void chkr_address (PTR ptr, int sure);

/* Don't ask why ! This epsilon was found after several hundred hours of
   work :-) (on gdb).  */
#define MAGIC_NUMBER_FOR_PC 8
#define FRAME_TO_IP(FP) (PTR) (FP->fr_savpc + MAGIC_NUMBER_FOR_PC)

#ifdef SIMCHECKER
extern uint pc;

static struct frame *
raw_save_frames (void)
{
  return (struct frame*) (regs + 16);	/* L0 = 16 */
}

#else

static struct frame *
raw_save_frames (void)
{
  struct frame * res;
  asm ("ta 0x3\n"
       "\tmov %%sp, %0" : "=r" (res));
  return res;
}
#endif

/* Search pointer inside the stack segment.
 * Check between &dummy and the top of the stack */
void
search_stack (void)
{
#ifndef SIMCHECKER
 PTR dummy;	/* OK, we could use ptr or min, but dummy is clearer.  */
#endif 
 PTR *ptr;

 /* This code is not really portable.
   We can know how the stack is growing
   We know where the stack pointer is
   But I don't know how to know the top of the stack.
   This top can change only while a process is created. (exec, fork, clone)
   So, just after main, we can insert code to know the top.
   An other way is to make a core-file ... 
   Or to try and waiting for SIGSEGV */
#ifdef SIMCHECKER
  ptr = (PTR*) regs[SP];
#else
  ptr = &dummy;
#endif
 for(; ptr < (PTR *) STACK_BASE; ptr++)
   {
     /* Does not point on an heap.  */
     if (*ptr >= low_addr_heap && *ptr < high_addr_heap)
       chkr_address (*ptr, 1);
   }
}

/* Search pointer inside the stack segment.  However, they should be saved.  */
void
search_register (void)
{
  return;
}

/* Save the stack (in fact, only the return addresses)
 * used by malloc.
 */
void
chkr_get_history (PTR *ptr, int forget, int num)
{
 struct frame *fp;
 int i;

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
   
 fp = raw_save_frames ();

#ifdef SIMCHECKER
 if (ptr)
   {
     *ptr++ = (PTR) pc;
     if (num == -1)
       chkr_abort ();
     else
       num--;
   }
 else
   chkr_show_addr ((PTR) pc);
#endif

#ifdef GCCCHECKER
 {
   PTR p;
   PTR p1;
   
   /* Skip frames inside Checker.  */
   for (; fp->fr_savfp; fp = fp->fr_savfp)
     {
       p = FRAME_TO_IP (fp);
       if (p < (PTR) &checker_text_begin || p > (PTR) &checker_text_end)
         break;
     }
 
   /* Skip frames inside the stubs.  */
   for (; fp->fr_savfp && fp->fr_savfp->fr_savfp; fp = fp->fr_savfp)
     {
       p = FRAME_TO_IP (fp);
       p1 = FRAME_TO_IP (fp->fr_savfp);
       if (p < (PTR) &checker_text_stubs_begin
           || p > (PTR) &checker_text_stubs_end
           || p1 < (PTR) &checker_text_stubs_begin
           || p1 > (PTR) &checker_text_stubs_end)
         break;
     }
 }
#endif
 
 /* Special case: Get the current ip.  */
 if (ptr && num == -1)
   {
     *ptr = FRAME_TO_IP (fp);
     return;
   }
   
 /* Save num frames.  */
 for (i = num - 1; (i > 0 || !ptr) && fp->fr_savfp; i--)
   {
     if (ptr)
       *ptr++ = FRAME_TO_IP (fp);
     else
       chkr_show_addr (FRAME_TO_IP (fp));
     fp = fp->fr_savfp;
   }
 
 if (ptr)
   *ptr = (PTR) 0;
}

/* fonction called by malloc.c:initialize() 
 * It is called once time, and can use malloc/free (e.g. getcwd)
 * search the full name. 
 */
void
chkr_initialize (void)
{
#if 0
 PTR *frame;
#endif
 
 /* If checker is already initialized, return now */
 if (chkr_is_init)
   return;

#if 0   
 /* search the frame of 'main' */
 frame = (PTR*) &frame + 1; /* get the stack pointer from &frame */ 
			       /* 1 is 4 bytes */
 while (is_ok_frame (frame))
   frame = (PTR*) *frame;
 
 /* frame[1] is the return address
    frame[2] is argc
    frame[3] is argv
    frame[4] is argp
 */
 if (frame)
   ___chkr_init_chkr ((int)frame[2], (char**)frame[3], (char**)frame[4]);
 else
   chkr_header (M_CANT_FIND_MAIN);

 return;
#endif
}

/* This is called by parse-args.c(___chkr_init_chkr) */
void
chkr_init_machine (int *argc, char *argv[], char *envp[])
{
 /* Must be set now.  */
 low_addr_heap = (PTR) (objects->next->end);
 min_address = (PTR) (objects->org + MIN_ADDRESS);
}
