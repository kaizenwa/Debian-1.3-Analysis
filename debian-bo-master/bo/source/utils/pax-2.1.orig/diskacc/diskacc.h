/* DISKACC.H
 *
 * Autor:    Kai Uwe Rommel
 * Datum:    Thu 28-Dec-1989
 *
 * Compiler: MS C ab 5.00
 * System:   PC/MS-DOS ab 3.20, OS/2 ab 1.1
 *
 */

#ifndef APIENTRY
#include <os2def.h>
#endif
 
INT APIENTRY DskOpen(USHORT drive, PUSHORT sides,
                     PUSHORT tracks, PUSHORT sectors);

VOID APIENTRY DskClose(USHORT handle);

INT APIENTRY DskRead(USHORT handle, USHORT side, USHORT  track,
                     USHORT sector, USHORT nsects, PVOID buf);

INT APIENTRY DskWrite(USHORT handle, USHORT side, USHORT  track,
                      USHORT sector, USHORT nsects, PVOID buf);

/* Ende DISKACC.H */
