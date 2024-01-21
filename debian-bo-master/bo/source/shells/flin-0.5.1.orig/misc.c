/*
Copyright (C) 1995  Brian Cully

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 675 Mass
Ave, Cambridge, MA 02139, USA.

please send patches or advice to: `shmit@kublai.com'
*/
#include <stdio.h>
#include <stdlib.h>

/* Print an error message (str) on stderr and exit */
void error(char *str, int line) {
   fprintf(stderr, "%i: %s", line, str);
   exit(1);
}

/*
 * chk_alloc()
 *
 * Input - size - number of bytes to allocate.
 * Returns - pointer to allocated space.
 *
 * This is a simple wrapper for malloc, so that we don't
 * need to worry about the return value of malloc being NULL
 */
void	*chk_alloc(size_t size)
{
	void	*retval;

	if ((retval = malloc(size)) == NULL)
		error("Out Of Memory!\n", 0);

	return(retval);
}
