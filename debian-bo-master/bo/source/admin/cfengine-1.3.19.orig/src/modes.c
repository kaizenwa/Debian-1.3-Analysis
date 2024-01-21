/* cfengine for GNU
 
        Copyright (C) 1995
        Free Software Foundation, Inc.
 
   This file is part of GNU cfengine - written and maintained 
   by Mark Burgess, Dept of Computing and Engineering, Oslo College,
   Dept. of Theoretical physics, University of Oslo
 
   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

*/
 

#include "cf.defs.h"
#include "cf.extern.h"


/***************************************************************/
/* Modestring toolkit                                          */
/***************************************************************/

  enum modestate
     {
     who,
     which
     };

ParseModeString(modestring,plusmask,minusmask)

char *modestring;
mode_t *plusmask, *minusmask;

{ char modebuffer[maxvarsize], *sp; 
 int affected = 0, value = 0, gotaction;
  char action = '=';
  enum modestate state = who;

Debug1("ParseModeString(%s)\n",modestring);

gotaction = false;
*plusmask = *minusmask = 0;

for (sp = modestring; true ; *sp++)
   {
   switch (*sp)
      {
      case 'a': CheckModeState(who,state,*sp);
                affected |= 07777;
                break;

      case 'u': CheckModeState(who,state,*sp);
                affected |= 04700;
                break;

      case 'g': CheckModeState(who,state,*sp);
                affected |= 02070;
                break;

      case 'o': CheckModeState(who,state,*sp);
                affected |= 00007;
                break;

      case '+':
      case '-':
      case '=': if (gotaction)
                   {
                   yyerror("Too many +-= in mode string");
                   }
                action = *sp;
                state = which;
                gotaction = true;
                break;

      case 'r': CheckModeState(which,state,*sp);
                value |= 0444 & affected;
                break;

      case 'w': CheckModeState(which,state,*sp);
                value |= 0222 & affected;
                break;

      case 'x': CheckModeState(which,state,*sp);
                value |= 0111 & affected;
                break;

      case 's': CheckModeState(which,state,*sp);
                value |= 06000 & affected;
                break;

      case 't': CheckModeState(which,state,*sp);
                value |= 01000;
                break;

      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7': state = which;
                sscanf(sp,"%o",&value);
                while (isdigit(*sp) && (*sp != '\0'))
		   {
                   sp++;
                   }
                sp--;
                break;

      case ',':
                SetMask(action,value,plusmask,minusmask);
                action = '=';
                affected = 0;
                value = 0;
                gotaction = false;
                state = who;
                break;

      case '\0':
                if (state == who || value == 0)
                   {
                   Warning("mode string is incomplete");
                   }

                SetMask(action,value,plusmask,minusmask);
                Debug1("[PLUS=%o][MINUS=%o]\n",*plusmask,*minusmask);
                return;

      default:  yyerror ("Invalid mode string");
                break;
      }
   }
}

/*********************************************************/

CheckModeState(stateA,stateB,ch)

enum modestate stateA;
int stateB;
char ch;

{
if ((int)stateA != stateB)
   {
   sprintf(VBUFF,"Mode string constant (%c) used out of context",ch);
   yyerror(VBUFF);
   }
return;
}

/*********************************************************/

SetMask(action,value,p,m)

char action;
int value;
mode_t *p,*m;

{
Debug1("SetMask(%c%o)\n",action,value);

switch(action)
   {
   case '+':
             *p |= value;
             *m |= 0;
             return;
   case '-':
             *p |= 0;
             *m |= value;
             return;
   case '=':
             *p |= value;
             *m |= (~value) & 07777;
             return;
   default:
             sprintf(VBUFF,"Mode directive %c is unknown",action);
             yyerror(VBUFF);
             return;
   }
}

