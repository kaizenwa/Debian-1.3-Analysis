/**************************************************************************
 **************************************************************************
 **                                                                      **
 ** arglist.h       Argument list functions                              **
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

#ifndef ARGLIST_H
#define ARGLIST_H

/* >>>>>>>>>> Headers <<<<<<<<<< */

/* >>>>>>>>>> Defines <<<<<<<<<< */

#define ARGLISTINC  (40)

/* >>>>>>>>>> Types <<<<<<<<<< */

typedef struct alist    /* The actual argument list */
{
    int     alloc;      /* amount of memory allocated to an arglist */
    int     argc;       /* analogous to the global argc */
    char    **argv;     /* analogous to the global argv[] */
} arglist;

/* >>>>>>>>>> Variables <<<<<<<<<< */

/* >>>>>>>>>> Prototypes <<<<<<<<<< */

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

int string2arglist (char *string, arglist *args);

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

int argv2arglist (int argc, char *argv[], arglist *args);

/* >>>>>>>>>> Functions <<<<<<<<<< */

#endif /* ARGLIST_H */
