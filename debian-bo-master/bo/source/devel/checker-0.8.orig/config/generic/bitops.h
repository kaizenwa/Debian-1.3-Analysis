/* bitops - operation with bits, used by malloc
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written December 1993 by Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/

/* If you want speed, you can define log_size with machine instructions.
   Consequently, the fonction is inline and faster.
   log_size return the offset of the higher bit set to one. If this offset
   is greather than 19, the function must return 19.
   e.g: log_size(32) is 5
        log_size(48) is 5
   There is a C equivalent function below.
 */

#ifdef __GNUC__
/* Return log2 of size.
 * If the log is greather (or equal) than HASH_SIZE, returns HASH_SIZE - 1
 * See bitops.h
 */
static INLINE int
log_size (size_t size)
{
  int log = 0;
  while ((size >>= 1) != 0)
    log++;
  if (log >= HASH_SIZE)
    log = HASH_SIZE - 1;
  return log;
}

static INLINE uint
mutex_atomic_swap(volatile uint *p, uint newval)
{
  uint semval = *p;
  
  /* Higly bad... */
  *p = newval;
  return semval;
}
#else
extern int log_size (size_t size);
extern uint mutex_atomic_swap(volatile uint *p, uint newval);
#endif
