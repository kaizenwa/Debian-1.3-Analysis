/**************************************************************************
 **************************************************************************
 **                                                                      **
 ** termstate.h     terminal state functions                             **
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

#if defined (SGTTY)
#include <sgtty.h>
#elif defined (SYSV_TERMIO)
#include <termio.h>
#else /* POSIX_TERMIOS */
#include <termios.h>
#endif

/* >>>>>>>>>> Types <<<<<<<<<< */

#if defined (SGTTY)
typedef struct sgttyb termstate;
#elif defined (SYSV_TERMIO)
typedef struct termio termstate;
#else /* POSIX_TERMIOS */
typedef struct termios termstate;
#endif

/* >>>>>>>>>> Prototypes <<<<<<<<<< */

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

int save_termstate (int fd, termstate *state);

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

int restore_termstate (int fd, termstate *state);

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

void raw_termstate (termstate *state);

