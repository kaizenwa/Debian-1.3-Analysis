/************************************************************************
 * utils.h -- various utilities						*
 *									*
 * A little RPN (Reverse Polish Notation) calculator,                   *
 * rudimentary emulating a HP 28S. 					*
 * 								        *
 * rpncalc is (c) David Frey, 1993, 1994, 1995				*
 *								        * 
 * This program is free software; you can redistribute it and/or modify *
 * it under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or    *
 * (at your option) any later version.                                  *
 *									* 
 * This program is distributed in the hope that it will be useful,      *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 * GNU General Public License for more details.                         *
 *									* 
 * You should have received a copy of the GNU General Public License    *
 * along with this program; if not, write to the Free Software          *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.            *
 *									* 
 ************************************************************************/

/* $Id: utils.h,v 1.1 1996/02/11 20:44:55 david Rel $
 * $Log: utils.h,v $
 * Revision 1.1  1996/02/11 20:44:55  david
 * Initial revision
 **/

void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t size);

unsigned char *getline(FILE *stream);
