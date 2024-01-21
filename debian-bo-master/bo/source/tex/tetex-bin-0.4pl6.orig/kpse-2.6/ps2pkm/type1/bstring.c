/*
 * A simple bzero() for AMIGA/Aztec, MSDOS/TurboC, . . .
 */

#include "c-auto.h"

#ifndef HAVE_BZERO
bzero(char *b, int length)
{
   while (length--) *(b++) = '\0';
}
#endif
