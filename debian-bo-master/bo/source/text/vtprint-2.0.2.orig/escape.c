/**************************************************************************
 **************************************************************************
 **                                                                      **
 ** escape.c        Routine for performing escape code string expansion. **
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

/* >>>>>>>>>> Headers <<<<<<<<<< */

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>

#include "escape.h"

/* >>>>>>>>>> Functions <<<<<<<<<< */

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

char *escape (char *source)
{
    char    *dest;
    char    *probe;
    long    l;

    if (!source) return NULL;
    dest = source;

    for (probe = source; *probe; probe++)
    {
        /* skip bare double quotes, used as delimiters */
        if (*probe == '\"') continue;

        if (*probe != '\\') 
        {
            *dest = *probe;
            dest++;
            continue;
        }

        probe++;    /* assume we are escaped now! */
        switch (*probe)
        {
            case 'a'    : { *dest = '\x07'; dest++; break; }
            case 'b'    : { *dest = '\x08'; dest++; break; }
            case 'f'    : { *dest = '\x0C'; dest++; break; }
            case 'n'    : { *dest = '\x0A'; dest++; break; }
            case 'r'    : { *dest = '\x0D'; dest++; break; }
            case 't'    : { *dest = '\x09'; dest++; break; }
            case 'v'    : { *dest = '\x0B'; dest++; break; }
            case '\\'   : 
            case '\''   : 
            case '\"'   : 
            case '\?'   : { *dest = *probe; dest++; break; }
            case '0'    : {
                            l = strtol (probe, &probe, 8);
                            *dest = (char) l;
                            dest++;
                            probe--;
                            break;
                          }
            case '1'    :
            case '2'    :
            case '3'    :
            case '4'    :
            case '5'    :
            case '6'    :
            case '7'    :
            case '8'    :
            case '9'    : {
                            l = strtol (probe, &probe, 10);
                            *dest = (char) l;
                            dest++;
                            probe--;
                            break;
                          }
            case 'X'    :
            case 'x'    : {
                            /* check for valid character */
                            if ((!isdigit(probe[1])) && 
                                ((toupper (probe[1]) > 'F') ||
                                (toupper (probe [1]) < 'A')))
                            return NULL;
                            l = strtol (probe, &probe, 16);
                            *dest = (char) l;
                            dest++;
                            probe--;
                            break;
                          }
            default     : return NULL;
        }
    }
    *dest = '\0';
    return (source);
}
