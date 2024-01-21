/**************************************************************************
 **************************************************************************
 **                                                                      **
 ** termstate.c     terminal state functions                             **
 ** ===========                                                          **
 **                                                                      **
 ** Purpose:        Provides functions to simplifify management of some  **
 **                 terminal states.                                     **
 **                                                                      **
 ** Author:         Garrett D'Amore <garrett@sciences.sdsu.edu>          **
 **                                                                      **
 ** Copyright:      1994, Garrett E. D'Amore                             **
 **                                                                      **
 ** NO WARRANTY:    This program is provided entirely without warranty.  **
 **                 The user assumes full responsibility for the use of  **
 **                 this program, and agrees to indemnify the author and **
 **                 the copyright holder from any damage or loss that    **
 **                 may result from the use of or inability to use this  **
 **                 program.  In simple language: YOU USE THIS PROGRAM   **
 **                 AT YOUR OWN RISK!                                    **
 **                                                                      **
 ** Warning:        None.                                                **
 **                                                                      **
 ** Restrictions:   None.                                                **
 **                                                                      **
 ** Algorithm:      None.                                                **
 **                                                                      **
 ** References:     None.                                                **
 **                                                                      **
 ** File formats:   None.                                                **
 **                                                                      **
 ** Rev. History:   June 4, 1994    Garrett D'Amore                      **
 **                 -- Initial coding.                                   **
 **                                                                      **
 ** Notes:          None.                                                **
 **                                                                      **
 **************************************************************************
 **************************************************************************/

/* >>>>>>>>>> Headers <<<<<<<<<< */

#include <errno.h>

#if defined (SGTTY)
#include <sgtty.h>
#elif defined (SYSV_TERMIO)
#include <termio.h>
#else /* POSIX_TERMIOS */
#include <termios.h>
#endif

#include <unistd.h>

#include "termstate.h"

/* >>>>>>>>>> Functions <<<<<<<<<< */

/***************************************
 *
 *  save_termstate  Save terminal state.
 *
 *  Purpose         Saves the terminal state so that we can restore
 *                  it later.  
 *
 *  Parameters      fd:     filedescriptor to save
 *                  state:  pointer to area to save state at
 *
 *  Returns         zero on success,
 *                  non-zero on failure.
 *
 */

int save_termstate (int fd, termstate *state)
{
    if (!isatty (fd)) return (ENOTTY);

#if defined (SGTTY)
    return (gtty (fd, state));
#elif defined (SYSV_TERMIO)
    return (ioctl (fd, TCGETA, state));
#else /* POSIX_TERMIOS */
    return (tcgetattr (fd, state));
#endif 

}

/***************************************
 *
 *  restore_termstate   Restore terminal state.
 *
 *  Purpose         Restores a terminal state saved with
 *                  save_termstate.
 *
 *  Parameters      fd:     file descriptor associated with terminal
 *                  state:  previously saved termstate
 *
 *  Returns         zero on success,
 *                  non-zero on error.
 *
 */

int restore_termstate (int fd, termstate *state)
{
    if (!isatty (fd)) return (ENOTTY);

#if defined (SGTTY)
    return (stty (fd, state));
#elif defined (SYSV_TERMIO)
    return (ioctl (fd, TCSETAW, state)); 
#else /* POSIX_TERMIOS */
    return (tcsetattr (fd, TCSADRAIN, state));
#endif 

}

/***************************************
 *
 *  raw_termstate   Sets a termstate to raw mode.
 *
 *  Purpose         Modifies a termstate structure so that
 *                  it has no ouput postprocessing.
 *
 *  Parameters      state:  terminal state structure to modify
 *
 *  Returns         None.
 *
 */

void raw_termstate (termstate *state)
{

#ifdef SGTTY
    state->sg_flags |= RAW;
#else /* SYSV_TERMIO || POSIX_TERMIOS */
    state->c_oflag |= OPOST;
#endif

    return;
}

