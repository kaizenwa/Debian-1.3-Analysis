// "utils.C"

/*    Copyright (C) 1994,1995,1996  Steve Hardt

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Steve Hardt 
    hardts@mit.edu (valid until Nov. 1996)
    hardts@netscape.com
    hardts@alum.mit.edu
    http://graphics.lcs.mit.edu/~hardts/xevil.html
*/

#ifndef NO_PRAGMAS
#pragma implementation "utils.h"
#endif


// Include Files
#include <iostream.h>
#include "utils.h"



Pixel Xvars::allocNamedColor(int dpyNum,const char *name,Pixel def)
{
  XColor actual,database;

  // Check for monochrome display.
  // Hack, not supposed to look at c_class member of visual.
  Status status = 1;
  if (((visual[dpyNum]->c_class == PseudoColor)  ||
       (visual[dpyNum]->c_class == StaticColor) ||
       (visual[dpyNum]->c_class == DirectColor) ||
       (visual[dpyNum]->c_class == TrueColor)) && 
      (status = 
       XAllocNamedColor(dpy[dpyNum],cmap[dpyNum],(char *)name,&actual,&database)))
    return actual.pixel;
  else
    {
      if (!status)
	cerr << "Warning:: unable to allocate color " << ((char *)name) 
	     << "." << endl;
      return (def == (Pixel)-1) ? white[dpyNum] : def;
    }
}



const char *Xvars::humanColorNames[Xvars::HUMAN_COLORS_NUM] =
{
  "blue",
  "brown",
  "black",
  "purple",
  "green4",
  "pink3",
};



void Utils::insertionSort(int arry[],int numElements)
{
  for (int j = 0; j  < numElements - 1; j++) 
    // Set arry[j] to be the minimum from arry[j]..arry[numElements-1].
      for (int i = j + 1; i < numElements; i++)
	if (arry[i] < arry[j])
	  {
	    int tmp = arry[i];
	    arry[i] = arry[j];
	    arry[j] = tmp;
	  }
}



void Utils::randomList(int arry[],int num)
{
  int n;
  for (n = 0; n < num; n++)
    arry[n] = n;

  for (n = num - 1; n > 0; n--)
    {
      int index = choose(n);
      int tmp = arry[index];
      arry[index] = arry[n];
      arry[n] = tmp;
    }
}



int Utils::minimum(int arry[],int size)
{
  Boolean anySet = False;
  int ret = 0;

  for (int n = 0; n < size; n++)
    if (!anySet || (arry[n] < ret))
      {
	ret = arry[n];
	anySet = True;
      }

  assert(anySet);
  return ret;
}




int Utils::minimum(int arry[],Boolean oks[],int size)
{
  Boolean anySet = False;
  int ret = -1;

  for (int n = 0; n < size; n++)
    if (oks[n] && (!anySet || (arry[n] < ret)))
      {
	ret = arry[n];
	anySet = True;
      }

  return ret;
}
