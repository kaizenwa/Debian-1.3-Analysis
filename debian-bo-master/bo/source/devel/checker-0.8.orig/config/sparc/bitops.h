/* bitops - operation with bits, used by malloc
   Copyright 1995 Tristan Gingold
		  Written June 1995 by Tristan Gingold

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

#ifndef __BITOPS_H__
#define __BITOPS_H__
static __inline__ uint
mutex_atomic_swap(volatile uint *p, uint newval)
{
  asm __volatile__ (
  	"swap  %0, [%2]\n"
	: "=r" (newval)
	: "0" (newval), "r" (p)); /* p is a var, containing an address */
  return newval;
}
#endif /* __BITOPS_H__ */
