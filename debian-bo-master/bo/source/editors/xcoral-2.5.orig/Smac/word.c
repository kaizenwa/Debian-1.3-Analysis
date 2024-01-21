/* ########################################################################

				 word.c

   File: word.c
   Path: /home/fournigault/c/X11/xcoral-2.33/Smac/word.c
   Description: 
   Created: Tue Feb 21 13:04:00 MET 1995
   Author: Bruno Pages
   Modified: Tue Feb 21 13:04:00 MET 1995
   Last maintained by: Bruno Pages

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Bruno Pages

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#include <stdio.h>

#define THEFILE "wordisgood"

int goodp()
{
  FILE * fp = fopen(THEFILE, "r");
  
  if (fp) {
    fclose(fp);
    unlink(THEFILE);
    return 1;
  }
  return 0;
}

void itisgood()
{
  FILE * fp = fopen(THEFILE, "w");
  
  fputc('\n', fp);
  fclose(fp);
}


/* Characters and integers are memorized and acceded in a field of
   type Object to accelerate access and assignement (some compilers
   introduce useless internal function calls with unions using).
   But in this case a variable reference is the Object field address
   only when the lower byte is equals with the Object's address.
   We compute here the correction of the Object address to have the
   right address depending of the effective object type. */
  
void main()
{
  int int_correction;
  int char_correction;
  int check_int_pointer;
  int check_pointer_pointer;
  
  fprintf(stderr,
	  "\nWarning : during the word.h generation, two cores may be momentarily created\n\n");
  
  if (sizeof(int) == sizeof(char *)) {
    int dec;
    union {
      int i;
      char c[sizeof(int)];
    } u;
    
    u.i = 1;
    
    for (dec = 0; !u.c[dec]; dec += 1);
    int_correction = 0;
    char_correction = dec;
  }
  else {
    int dec;
    union {
      long l;
      int i[(sizeof(long) + sizeof(int) - 1) / sizeof(int)];
      char c[sizeof(long)];
    } u;
    
    u.l = 1;
    
    for (dec = 0; !u.i[dec]; dec += 1);
    int_correction = dec;
    
    for (dec = 0; !u.c[dec]; dec += 1);
    char_correction = dec;
  }

  {
    /* Compute the number of lower bits which must be null in
       a pointer to a character, integer or an other pointer */
    
    int ti[3];
    char * tp[3];
    int mask = 0;
    long add;
    int ajout_add;
    
    mask = 0;
    add = ((long) &ti[1]);
    for (ajout_add = 1;
	 ((add + ajout_add) != ((long) &ti[2])) &&
	 ((add + ajout_add) != ((long) &ti[0]));
	 ajout_add <<= 1) {
      
      if (fork()) {
	wait(0);
	
	if (! goodp()) {
	  /* erreur */
	  unlink("core");
	  mask |= ajout_add;
	}
      }
      else {
	int i = *((int *) (((long) &ti[1]) + ajout_add));
	
	itisgood();
	exit(i);
      }
    }
    check_int_pointer = mask;
    
    mask = 0;
    add = ((long) &tp[1]);
    for (ajout_add = 1;
	 ((add + ajout_add) != ((long) &tp[2])) &&
	 ((add + ajout_add) != ((long) &tp[0]));
	 ajout_add <<= 1) {
      
      if (fork()) {
	wait(0);
	
	if (! goodp()) {
	  /* erreur */
	  unlink("core");
	  mask |= ajout_add;
	}
      }
      else {
	int i = (*((char **) (((long) &tp[1]) + ajout_add))) != 0;
	
	itisgood();
	exit(i);
      }
    }
    check_pointer_pointer = mask;
  }

  
  /* Compute the type Object, `int' is favorized because `long'
     can be greater than a pointer and take more than one word */
  
  if (sizeof(int) == sizeof(char *))
    printf("#define Object int\n");
  else
    printf("#define Object long\n");
  
  printf("#define INT_CORRECTION %d\n", int_correction);
  printf("#define CHAR_CORRECTION %d\n", char_correction);
  printf("#define CHECK_INT_POINTER %d\n", check_int_pointer);
  printf("#define CHECK_POINTER_POINTER %d\n", check_pointer_pointer);
  
  exit(0);
}

