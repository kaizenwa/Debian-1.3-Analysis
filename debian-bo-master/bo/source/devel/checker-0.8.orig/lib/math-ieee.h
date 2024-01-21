/* IEEE layout.
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

union ieee754_float
{
  float f;
  struct
    {
      unsigned int negative : 1;
      unsigned int exponent : 8;
      unsigned int mantissa : 23;
    }
  ieee;
  unsigned int word[1];
};

union ieee754_double
{
  double d;
  struct
    {
      unsigned int negative : 1;
      unsigned int exponent : 11;
      unsigned int mantissa0 : 20;
      unsigned int mantissa1 : 32;
    }
  ieee;
  unsigned int word[2];
};

union ieee754_extended
{
  long double e;
  struct
    {
      unsigned int negative : 1;
      unsigned int exponent : 15;
      unsigned int res : 16;
      unsigned int hidden : 1;
      unsigned int mantissa0 : 31;
      unsigned int mantissa1 : 32;
      unsigned int res1 : 32;
    }
  ieee;
  unsigned int word[4];
};
