/**************************************************************************
 **************************************************************************
 **                                                                      **
 ** escape.h        Routine for performing escape code string expansion. **
 ** ========                                                             **
 **                                                                      **
 ** Purpose:        Expand strings with C style escapes in them.         **
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
 ** References:     Kernighan & Ritchie, "The C Programming Language"    **
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

#ifndef ESCAPE_H
#define ESCAPE_H

/* >>>>>>>>>> Prototypes <<<<<<<<<< */

/***************************************
 *
 *  escape          Convert a C style format string to actual value.
 *
 *  Purpose         Provides a mechanism for expanding C-style escape
 *                  codes in a string.
 *
 *  Parameters      source:     String to expand.
 *
 *  Returns         pointer to converted string on success,
 *                  NULL on (parse) error.
 *
 */

char *escape (char *source);

#endif /* ESCAPE_H */
