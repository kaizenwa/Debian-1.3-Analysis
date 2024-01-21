/**************************************************************************
 **************************************************************************
 **                                                                      **
 ** arglist.c       Argument list functions                              **
 ** =========                                                            **
 **                                                                      **
 ** Purpose:        Simplifies handling of argument lists that may       **
 **                 appear in argv, and environment variable, or else-   **
 **                 where.                                               **
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
 ** Rev. History:   June 2, 1994    Garrett D'Amore                      **
 **                 -- Initial coding.                                   **
 **                                                                      **
 ** Notes:          None.                                                **
 **                                                                      **
 **************************************************************************
 **************************************************************************/

/* >>>>>>>>>> Headers <<<<<<<<<< */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "arglist.h"

/* >>>>>>>>>> Functions <<<<<<<<<< */

/***************************************
 *
 *  string2arglist  Convert a string to an arglist.
 *
 *  Purpose         Used to split a string up into an
 *                  argv-like arglist.
 *
 *  Parameters      string:     the string to convert from
 *                  args:       pointer to struct to hold result
 *
 *  Returns         zero on success,
 *                  non-zero on error (ENOMEM).
 *
 */

int string2arglist (char *string, arglist *args)
{
    char *st;

    /* clear the arglist to a clean state */
    if (args->argv) free (args->argv);
    args->argc = 1;
    args->alloc = 0;

    st = string;

    while (1)
    {
        /* resize array if necessary */
        if (args->argc >= args->alloc)
        {
            if (!(args->argv))
                args->argv = (char **) malloc (ARGLISTINC * sizeof (char *));
            else
                args->argv = (char **) realloc (args->argv,
                    (args->alloc + ARGLISTINC) * sizeof (char *));
            if (args->argv == NULL) return (ENOMEM);
            args->alloc += ARGLISTINC;
        }

        /* get string and set it up, exit loop if not found */
        if (!(args->argv [args->argc] = strtok (st, "\t "))) break;

        /* needed for additional calls to strtok */
        st = NULL;

        args->argc++;
    }
    return 0;
}

/***************************************
 *
 *  argv2arglist    Fill an arglist from argv array.
 *
 *  Purpose         Converts argv[] array to arglist structure.
 *
 *  Parameters      argc:       number of entries in argv[]
 *                  argv:       argv argument to main()
 *
 *  Returns         zero on success,
 *                  nonzero on error (ENOMEM).
 *
 */

int argv2arglist (int argc, char *argv[], arglist *args)
{
    int i;

    /* clear any old memory */
    if (args->argv) free (args->argv);

    args->argv = (char **) malloc ((argc + 1) * sizeof (char *));
    if (!args->argv) return (ENOMEM);
    
    args->alloc = argc;
    
    for (i = 0; i < argc; i++)
    {
        args->argv[i+1] = argv[i+1];
    }
    args->argc = argc;

    return 0;
}

