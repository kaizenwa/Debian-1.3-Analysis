/* Winsup exception support.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "winsup.h"
#include "exceptions.h"

extern "C" void __stack_trace (void);

extern "C" exception_handler __cygwin_exception_handler;

static BOOL ctrl_c_handler (DWORD);
static void really_exit (int);

static const struct
{
  unsigned int code;
  const char *name;
}
status_info[] =
{
#define X(s) s, #s
  { X (STATUS_ABANDONED_WAIT_0) },
  { X (STATUS_ACCESS_VIOLATION) },
  { X (STATUS_ARRAY_BOUNDS_EXCEEDED) },
  { X (STATUS_BREAKPOINT) },
  { X (STATUS_CONTROL_C_EXIT) },
  { X (STATUS_DATATYPE_MISALIGNMENT) },
  { X (STATUS_FLOAT_DENORMAL_OPERAND) },
  { X (STATUS_FLOAT_DIVIDE_BY_ZERO) },
  { X (STATUS_FLOAT_INEXACT_RESULT) },
  { X (STATUS_FLOAT_INVALID_OPERATION) },
  { X (STATUS_FLOAT_OVERFLOW) },
  { X (STATUS_FLOAT_STACK_CHECK) },
  { X (STATUS_FLOAT_UNDERFLOW) },
  { X (STATUS_GUARD_PAGE_VIOLATION) },
  { X (STATUS_ILLEGAL_INSTRUCTION) },
  { X (STATUS_INTEGER_DIVIDE_BY_ZERO) },
  { X (STATUS_INTEGER_OVERFLOW) },
  { X (STATUS_INVALID_DISPOSITION) },
  { X (STATUS_IN_PAGE_ERROR) },
  { X (STATUS_NONCONTINUABLE_EXCEPTION) },
  { X (STATUS_NO_MEMORY) },
  { X (STATUS_PENDING) },
  { X (STATUS_PRIVILEGED_INSTRUCTION) },
  { X (STATUS_SINGLE_STEP) },
  { X (STATUS_STACK_OVERFLOW) },
  { X (STATUS_TIMEOUT) },
  { X (STATUS_USER_APC) },
  { X (STATUS_WAIT_0) },
  { 0, 0 }
#undef X
};

/* Initialization code.  */

#ifdef __i386__

// Set up the exception handler for the current thread.  The PowerPC & Mips
// use compiler generated tables to set up the exception handlers for each
// region of code, and the kernel walks the call list until it finds a region
// of code that handles exceptions.  The x86 on the other hand uses segment
// register fs, offset 0 to point to the current exception handler.

asm (".equ __except_list,0");

extern exception_list *_except_list asm ("%fs:__except_list");

static void
init_exception_handler (exception_list *el)
{
  el->handler = __cygwin_exception_handler;
  el->prev = _except_list;
  _except_list = el;
}

#define INIT_EXCEPTION_HANDLER(el) init_exception_handler (el)
#endif

void
init_exceptions (exception_list *el)
{
#ifdef INIT_EXCEPTION_HANDLER
  INIT_EXCEPTION_HANDLER (el);
#endif

  SetConsoleCtrlHandler (ctrl_c_handler, 1);
}

/* Utilities for dumping the stack, etc.  */

#ifdef __PPC__
static char *
hexify (DWORD value, int which)
{
  static char buffer[8][16];
  char *ret = &buffer[which][0];
  char *p = ret;
  int i;

  *p = '0';
  *++p = 'x';
  for (i = 28; i >= 0; i -= 4)
    *++p = "0123456789abcdef"[ (value >> i) & 0xf ];

  *++p = '\0';
  return ret;
}
#endif

static void
dump_status (EXCEPTION_RECORD *e,  CONTEXT *in)
{
#ifdef __PPC__
#define HAVE_STATUS
  system_printf ("Exception trapped!\n");
  if (e)
    system_printf ("exception 0x%x at %s\n", e->ExceptionCode, hexify (in->Iar, 0));
  else
    system_printf ("Null exception record at %s\n", hexify (in->Iar, 0));

  system_printf (" r0: %s  sp: %s toc: %s  r3: %s\n",
		 hexify (in->Gpr0, 0), hexify (in->Gpr1, 1),
		 hexify (in->Gpr2, 2), hexify (in->Gpr3, 3));

  system_printf (" r4: %s  r5: %s  r6: %s  r7: %s\n",
		 hexify (in->Gpr4, 0), hexify (in->Gpr5, 1),
		 hexify (in->Gpr6, 2), hexify (in->Gpr7, 3));

  system_printf (" r8: %s  r9: %s r10: %s r11: %s\n",
		 hexify (in->Gpr8,  0), hexify (in->Gpr9,  1),
		 hexify (in->Gpr10, 2), hexify (in->Gpr11, 3));

  system_printf ("r12: %s r13: %s r14: %s r15: %s\n",
		 hexify (in->Gpr12, 0), hexify (in->Gpr13, 1),
		 hexify (in->Gpr14, 2), hexify (in->Gpr15, 3));

  system_printf ("r16: %s r17: %s r18: %s r19: %s\n",
		 hexify (in->Gpr16, 0), hexify (in->Gpr17, 1),
		 hexify (in->Gpr18, 2), hexify (in->Gpr19, 3));

  system_printf ("r20: %s r21: %s r22: %s r23: %s\n",
		 hexify (in->Gpr20, 0), hexify (in->Gpr21, 1),
		 hexify (in->Gpr22, 2), hexify (in->Gpr23, 3));

  system_printf ("r24: %s r25: %s r26: %s r27: %s\n",
		 hexify (in->Gpr24, 0), hexify (in->Gpr25, 1),
		 hexify (in->Gpr26, 2), hexify (in->Gpr27, 3));

  system_printf ("r28: %s r29: %s r30: %s r31: %s\n",
		 hexify (in->Gpr28, 0), hexify (in->Gpr29, 1),
		 hexify (in->Gpr30, 2), hexify (in->Gpr31, 3));

  system_printf (" cr: %s xer: %s  lr: %s ctr: %s\n",
		 hexify (in->Cr, 0), hexify (in->Xer, 1),
		 hexify (in->Lr, 2), hexify (in->Ctr, 3));
#endif

#ifdef __i386__
#define HAVE_STATUS
  system_printf ("Exception trapped!\n");
  system_printf ("exception %x at %x\n", e->ExceptionCode, in->Eip);
  system_printf ("exception: ax %x bx %x cx %x dx %x\n",
		 in->Eax, in->Ebx, in->Ecx, in->Edx);
  system_printf ("exception: si %x di %x bp %x sp %x\n",
		 in->Esi, in->Edi, in->Ebp, in->Esp);
#endif

#ifndef HAVE_STATUS
  system_printf ("Had an exception\n");
#endif

  if (e)
    {
      for (int i = 0; status_info[i].name; i++)
	{
	  if (status_info[i].code == e->ExceptionCode)
	    {
	      system_printf ("exception is: %s\n", status_info[i].name);
	      break;
	    }
	}
    }
}

#if 0 /* not used anywhere */
#define SIZE 30

static int oldstack[SIZE];

void dump_stack (int s)
{
  int v[1];
  int j;
  small_printf ("Stackd dump\n");
  for (j = 1; j < SIZE; j++)
    { 
      small_printf ("%08x ", v[j]);
      if ((j &3) == 0)
	small_printf ("\n");
      if (s)
	oldstack[j] = v[j];
      else
	{
	  if (oldstack[j] != v[j])
	    small_printf ("!");
	  else
	    small_printf (" ");
	}
    }
  small_printf ("\n");

}
#endif

/* Print a stack backtrace.
   This is defined as a C function so it's callable from C.
   Please keep the output the same across all platforms.  */

extern "C" {
#ifdef __i386__
#define HAVE_STACK_TRACE
void
__stack_trace (void)
{
  register int i;
  register char **ebp;

  asm ("mov %%ebp,%0": "=r" (ebp):);

  system_printf ("Stack trace:\n");
  for (i = 0; i < 16; ++i)
    {
      system_printf ("frame %d: sp = %p, pc = %p\n",
		     i, ebp, ebp[1]);
      /* Top of stack?  */
      if (ebp[1] == 0)
	break;
      ebp = ((char ***) ebp)[0];
    }
  system_printf ("End of stack trace%s\n",
		 i == 16 ? " (more stack frames may be present)" : "");
}
#endif /* __i386__ */

#ifdef __PPC__
#define HAVE_STACK_TRACE
void
__stack_trace (void)
{
  void **fp = (void **) __builtin_frame_address (1);
  int i;

  system_printf ("Stack trace:\n");
  for (i = 0; i < 16; ++i)
    {
      system_printf ("frame %d: sp = %s, pc = %s\n",
		    i, hexify ((DWORD)fp, 0), hexify ((DWORD)fp[-1], 1));
      fp = (void **) *fp;
      if (! fp)
	break;
    }
  system_printf ("End of stack trace%s\n",
		 i == 16 ? " (more stack frames may be present)" : "");
}
#endif /* __PPC__ */

#ifndef HAVE_STACK_TRACE
void
__stack_trace (void)
{
  system_printf ("Stack trace not yet supported on this machine.\n");
}
#endif
}

/* Utilities to call a user supplied exception handler.  */

#ifdef __i386__
#define HAVE_CALL_HANDLER

unsigned rethere;
_sig_func_ptr sigfunc;
int sigarg;

static void
call_handler (_sig_func_ptr pfunc, int n, EXCEPTION_RECORD *e, CONTEXT *in)
{
  CONTEXT orig;
  int res;

  /* Suspend the running thread, grab its context somewhere safe
     and run the exception handler in the context of the thread -
     we have to do that since sometimes they don't return - and if
     this thread doesn't return, you won't ever get another exception. */

  res =   SuspendThread (u->self->hThread);
  debug_printf ("suspend said %d\n", res);
  orig.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
  res =   GetThreadContext (u->self->hThread,  &orig);

  orig.Eip =  (unsigned)&&wrap;
  SetThreadContext (u->self->hThread,  &orig); /* Restart the thread */
  sigfunc = pfunc;
  sigarg = n;
  rethere = orig.Eip;
  ResumeThread (u->self->hThread);
  return;
  /* This code is run in the standard thread space */
 wrap:
  asm ("orl	$0x0,(%esp)\n"  /* probe out enough room to play */
      "orl	$0x0,0x800(%esp)\n"
      "orl	$0x0,0x1000(%esp)\n"
      "pushl	_rethere\n"
      "pusha\n"
      "pushf\n"
      "pushl 	%ebp\n"
      "movl 	%esp,%ebp\n"
      "pushl 	_sigarg\n"
      "call 	*_sigfunc\n"
      "leave\n"
      "popf\n"
      "popa\n"
      "ret");
  
}
#endif /* i386 */

#ifdef __PPC__
#define HAVE_CALL_HANDLER
// Given a function pointer (descriptor), return the real function address
static inline DWORD
descriptor_to_function (void (*pfunc)())
{
  DWORD *ptr = (DWORD *)(void *)pfunc;
#if 0
  debug_printf ("descriptor_to_function: pfunc = %s, ptr = %s, ptr[0] = %s\n",
		hexify ((DWORD)pfunc, 0), hexify ((DWORD)ptr, 1), hexify ((DWORD)ptr[0], 2));

#endif
  return ptr[0];
}

// Given a function pointer (descriptor), return the GOT used by that function
static inline DWORD
descriptor_to_gotaddr (void (*pfunc)())
{
  DWORD *ptr = (DWORD *)(void *)pfunc;
#if 0
  debug_printf ("descriptor_to_gotaddr: pfunc = %s, ptr = %s, ptr[1] = %s\n",
		hexify ((DWORD)pfunc, 0), hexify ((DWORD)ptr, 1), hexify ((DWORD)ptr[1], 2));

#endif
  return ptr[1];
}

// Stub started from call_handler to actually call the user function.
static void
call_stub (void (*pfunc)(), int n, CONTEXT *orig_context)
{
  DWORD function_address = descriptor_to_function (pfunc);
  debug_printf ("Call_stub before calling user function 0x%x\n",
		(int)function_address);

  (* (void (*)(int)) pfunc)(n);			// call the user function

  // If the main thread returns, we have to restore our original context.
  debug_printf ("got return from calling user function 0x%x\n",
		(int)function_address);
#if 0
  dump_status ((EXCEPTION_RECORD *)0,  orig_context);
  really_exit (255);
#else
  orig_context->ContextFlags = CONTEXT_FULL;
  SetThreadContext (u->self->hThread, orig_context);
  ResumeThread (u->self->hThread);
  debug_printf ("Should never get here!!\n");
#endif
}

// Call the user function (in the user's thread) with argument n.
static void
call_handler (_sig_func_ptr pfunc, int n, EXCEPTION_RECORD *e, CONTEXT *in)
{
  CONTEXT *orig_context, context;
  int res;
  void (*stub_ptr)() = (void (*)())call_stub;
  void **old_sp;
  const size_t context_size = ((sizeof (context) + 15) / 16) * 16;
  const DWORD redzone_size = 224;			// max size compiler could write beyond end of SP aligned to 16 bytes
  const DWORD regsave_size = 8 * sizeof (void *);	// size to save 8 GPRS
  const DWORD tos_size = 6 * sizeof (void *);		// size of area callee must allocate

  /* Suspend the running thread, grab its context somewhere safe
     and run the exception handler in the context of the thread -
     we have to do that since sometimes they don't return - and if
     this thread doesn't return, you won't ever get another exception. */

  res = SuspendThread (u->self->hThread);
  debug_printf ("suspend said %d (hThread = %d)\n", res, (int)u->self->hThread);

  context.ContextFlags = CONTEXT_FULL;
  res = GetThreadContext (u->self->hThread,  &context);
  debug_printf ("get context said %d\n", res);

  if (strace () & (_STRACE_DEBUG | 1))
    dump_status (e, &context);

  /* now here comes the nasty stuff.  We push some stuff onto the stack of the
     main thread to pretend that it had a signal in a unixy way.  Under PowerPC
     NT, we have to advance the stack pointer, because we may have been
     interrupted while the function is in the prologue, and hasn't advanced the
     stack pointer yet.  In order that we don't have a race condition with
     another signal while we are processing the current one, copy the context
     structure to space on the main thread's stack, rather than use static
     storage.  */

  orig_context = (CONTEXT *)(context.Gpr1 - redzone_size - context_size);
  *orig_context = context;
  old_sp = (void **) context.Gpr1;
  context.Gpr1 -= redzone_size + regsave_size + tos_size + context_size;
  (* (void **)context.Gpr1) = *old_sp;			// update back chain in case there is a traceback
  context.Iar = descriptor_to_function (stub_ptr);
  context.Lr = orig_context->Iar;
  context.Gpr2 = descriptor_to_gotaddr (stub_ptr);
  context.Gpr3 = (DWORD) pfunc;				// arg 1
  context.Gpr4 = n;					// arg 2
  context.Gpr5 = (DWORD) orig_context;			// arg 3
  debug_printf ("about to restart thread at %s, new sp = %s\n",
	       hexify (context.Iar, 0), hexify (context.Gpr1, 1));

  debug_printf ("args = (%s, %s, %s)\n", hexify (context.Gpr3, 0),
	       hexify (context.Gpr4, 1), hexify (context.Gpr5, 2));

  res = SetThreadContext (u->self->hThread,  &context);	// Restart thread
  debug_printf ("SetThreadContext said %d\n", res);

  res = ResumeThread (u->self->hThread);
  debug_printf ("ResumeThread said %d\n", res);
  return;
}
#endif /* ppc */

#ifndef HAVE_CALL_HANDLER
#error "Need to supply machine dependent call_handler"
#endif

/* Main exception handler.

   This function apparently needs to be exported for the ppc port.
   FIXME: Need to say why here!  */

void
__cygwin_exception_handler (EXCEPTION_RECORD *e, void *arg, CONTEXT *in, void *x)
{
  int sig;

  /* FIXME: The original version had code here to reinitialize the exception
     handler under the guise of initializing thread exceptions.  It's not
     clear what this code was ever intended to do.  */

  /* FIXME: Will eventually want to only print this if the exception won't
     be handled by us.  */
  system_printf ("In cygwin_except_handler\n");

  /* Coerce win32 value to posix value.  */
  switch (e->ExceptionCode)
    {
    case STATUS_FLOAT_DENORMAL_OPERAND:
    case STATUS_FLOAT_DIVIDE_BY_ZERO:
    case STATUS_FLOAT_INEXACT_RESULT:
    case STATUS_FLOAT_INVALID_OPERATION:
    case STATUS_FLOAT_OVERFLOW:
    case STATUS_FLOAT_STACK_CHECK:
    case STATUS_FLOAT_UNDERFLOW:
    case STATUS_INTEGER_DIVIDE_BY_ZERO:
    case STATUS_INTEGER_OVERFLOW:
      sig = SIGFPE;
      break;

    case STATUS_ILLEGAL_INSTRUCTION:
    case STATUS_PRIVILEGED_INSTRUCTION:
      sig = SIGILL;
      break;

    case STATUS_TIMEOUT:
      sig = SIGALRM;
      break;

    default:
      sig = SIGSEGV;
      break;
    }

  if (u->self && u->self->sigs[sig].sa_mask & (1<<sig))
    {
      syscall_printf ("signal %d masked 0x%x\n",
		      sig, u->self->sigs[sig].sa_mask);
    }

  if (! u->self
      || (void *) u->self->sigs[sig].sa_handler == (void *) SIG_DFL
      || (void *) u->self->sigs[sig].sa_handler == (void *) SIG_IGN
      || (void *) u->self->sigs[sig].sa_handler == (void *) SIG_ERR)
    {
      static int traced = 0;

      /* Another exception could happen while tracing or while exiting.
	 Only do this once.  */
      if (traced)
	{
	  system_printf ("Error while dumping state (probably corrupted stack)\n");
	}
      else
	{
	  traced = 1;
	  dump_status (e, in);
	  __stack_trace ();
	}
      really_exit (2);
    }

  call_handler (u->self->sigs[sig].sa_handler, sig, e, in);
}

/* Keyboard interrupt handler.  */

static BOOL
ctrl_c_handler (DWORD type)
{
  int sig;
  /* Counter of number of ^C's.  If too many, exit program immediately.  */
  static int k;

  /* FIXME: The original version had code here to reinitialize the exception
     handler under the guise of initializing thread exceptions.  It's not
     clear what this code was ever intended to do.  */

  switch (type)
    {
    case CTRL_C_EVENT:
      sig = SIGINT;
      break;
    case CTRL_BREAK_EVENT:
      sig = SIGINT;
      break;
    case CTRL_CLOSE_EVENT:
      sig = SIGQUIT;
      break;
    case CTRL_LOGOFF_EVENT:
      sig = SIGHUP;
      break;
    default:
    case CTRL_SHUTDOWN_EVENT:
      sig = SIGQUIT;
      break;
    }

  debug_printf ("ctrl_c_handler: type %d, sig %d\n", type, sig);

  if (u->self->sigs[sig].sa_mask & (1<<sig))
    return 1;

  if ((void *)u->self->sigs[sig].sa_handler == (void *)SIG_DFL)
    {
      /* We encode the signal stuff in the high 8 bits.
	 (sorta like the reverse of a standard wait)
	 This is so that ordinary dos progs can look at our
	 exit value. */
      /* FIXME: exit calls atexit handlers.  */
      exit ((sig<<8) | 0x10000);
    }
  if ((void *)u->self->sigs[sig].sa_handler == (void *)SIG_IGN)
    {
      return 1;
    }
  if ((void *)u->self->sigs[sig].sa_handler == (void *)SIG_ERR)
    {
      exit (4);
    }

  debug_printf ("ctrl_c_handler: type %d, sig %d, about to call %p\n",
		type, sig, u->self->sigs[sig].sa_handler);

  call_handler (u->self->sigs[sig].sa_handler, sig,
		(EXCEPTION_RECORD *)0, (CONTEXT *)0);

  debug_printf ("ctrl_c_handler: call_handler returned\n");

  k++;

  if (k == 13)
    ExitProcess (0);

  /* FIXME: The original version had code here to reinitialize the exception
     handler under the guise of initializing thread exceptions.  It's not
     clear what this code was ever intended to do.  */

  debug_printf ("ctrl_c_handler: returning 1\n");
  return 1;
}

/* Cover function to `_exit' to handle exiting even in presence of more
   exceptions.  We use to call exit, but a SIGSEGV shouldn't cause atexit
   routines to run.  */

static void
really_exit (int rc)
{
  /* If the exception handler gets a trap, we could recurse awhile.
     If this is non-zero, skip the cleaning up and exit NOW.  */
  static int exit_already = 0;

  if (exit_already)
    ExitProcess (rc);
  exit_already = 1;

  _exit (rc);
}
