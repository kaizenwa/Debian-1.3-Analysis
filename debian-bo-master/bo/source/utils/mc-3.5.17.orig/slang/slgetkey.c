/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */
#include <config.h>
#include <stdio.h>
#include "slang.h"
#include "_slang.h"

int SLang_Input_Buffer_Len = 0;
unsigned char SLang_Input_Buffer [MAX_INPUT_BUFFER_LEN];

int SLang_Abort_Char = 7;
volatile int SLKeyBoard_Quit = 0;
int SLang_Ignore_User_Abort = 0;


unsigned int SLang_getkey (void)
{
   int imax;
   unsigned int ch;
   
   if (SLang_Input_Buffer_Len)
     {
	ch = (unsigned int) *SLang_Input_Buffer;
	SLang_Input_Buffer_Len--;
	imax = SLang_Input_Buffer_Len;
   
	MEMCPY ((char *) SLang_Input_Buffer, 
		(char *) (SLang_Input_Buffer + 1), imax);
     }
   else
     {
	ch = SLsys_getkey ();
     }	
   return(ch);
}


void SLang_ungetkey_string (unsigned char *s, int n)
{
   register unsigned char *bmax, *b, *b1;
   if (SLang_Input_Buffer_Len > MAX_INPUT_BUFFER_LEN - 3 - n) return;

   b = SLang_Input_Buffer;
   bmax = b + (SLang_Input_Buffer_Len - 1);
   b1 = bmax + n;
   while (bmax >= b) *b1-- = *bmax--;
   bmax = b + n;
   while (b < bmax) *b++ = *s++;
   SLang_Input_Buffer_Len += n;
}

void SLang_buffer_keystring (unsigned char *s, int n)
{

   if (n + SLang_Input_Buffer_Len > MAX_INPUT_BUFFER_LEN - 3) return;
   
   MEMCPY ((char *) SLang_Input_Buffer + SLang_Input_Buffer_Len, 
	   (char *) s, n);
   SLang_Input_Buffer_Len += n;
}

void SLang_ungetkey (unsigned char ch)
{
   SLang_ungetkey_string(&ch, 1);
}

int SLang_input_pending (int tsecs)
{
   int n;
   unsigned char c;
   if (SLang_Input_Buffer_Len) return SLang_Input_Buffer_Len;
   
   n = SLsys_input_pending (tsecs);
   if (n)
     {
	c = (unsigned char) SLang_getkey ();
	SLang_ungetkey_string (&c, 1);
     }
   return n;
}

void SLang_flush_input (void)
{
   int quit = SLKeyBoard_Quit;
   
   SLang_Input_Buffer_Len = 0;
   SLKeyBoard_Quit = 0;   
   while (SLsys_input_pending (0) > 0) (void) SLsys_getkey ();
   SLKeyBoard_Quit = quit;
}
