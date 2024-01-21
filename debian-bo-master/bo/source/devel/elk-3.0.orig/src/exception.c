#include "kernel.h"

int Intr_Was_Ignored;
unsigned long Intr_Level;

#ifdef POSIX_SIGNALS
sigset_t Sigset_Old, Sigset_Block;
#else
#ifdef BSD_SIGNALS
int Sigmask_Old, Sigmask_Block;
#endif
#endif

static Object V_Interrupt_Handler;

/* Make sure temp files are removed on hangup and broken pipe.
 */
/*ARGSUSED*/
void Signal_Exit (sig) int sig; {
    Exit_Handler ();
    exit (1);
}

Init_Exception () {
    Define_Variable (&V_Interrupt_Handler, "interrupt-handler", Null);
#ifdef POSIX_SIGNALS
    sigemptyset (&Sigset_Block);
    sigaddset (&Sigset_Block, SIGINT);
    (void)sigprocmask (0, (sigset_t *)0, &Sigset_Old);
#else
#ifdef BSD_SIGNALS
    Sigmask_Block = sigmask (SIGINT);
    Sigmask_Old = sigblock (0);
#endif
#endif
    (void)signal (SIGHUP, Signal_Exit);
    (void)signal (SIGPIPE, Signal_Exit);
}

/*ARGSUSED*/
void Intr_Handler (sig) int sig; {
    Object fun;

#ifndef BSD_SIGNALS
    (void)signal (SIGINT, Intr_Handler);
#endif
    Set_Error_Tag ("interrupt-handler");
    Reset_IO (1);
    fun = Var_Get (V_Interrupt_Handler);
    if (TYPE(fun) == T_Compound && COMPOUND(fun)->min_args == 0)
	(void)Funcall (fun, Null, 0);
    Format (Curr_Output_Port, "~%\7Interrupt!~%", 15, 0, (Object *)0);
    Reset ();
    /*NOTREACHED*/
}

void Install_Intr_Handler () {
    if (signal (SIGINT, SIG_IGN) == SIG_IGN)
	Intr_Was_Ignored = 1;
    else
	(void)signal (SIGINT, Intr_Handler);
}

Object P_Disable_Interrupts () {
    Disable_Interrupts;
    return Make_Unsigned_Long (Intr_Level);
}

Object P_Enable_Interrupts () {
    Enable_Interrupts;
    return Make_Unsigned_Long (Intr_Level);
}
