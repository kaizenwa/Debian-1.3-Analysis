/* vprintf replacement for Checker.
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written September 1993 Tristan Gingold

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

#include <stdarg.h>
#include "chkr-string.h"
#include "syscall.h"

#define BUFLEN 2048
#define FD 2

extern int flag_verbose;

#define flush_buf()				\
do						\
  {						\
    buffer[Bindex] = '\0';			\
    sim_write(FD, buffer, sim_strlen(buffer));	\
    Bindex = 0;					\
  }						\
while(0)

#define add_to_buf(c)				\
do						\
  {						\
    buffer[Bindex++] = c;			\
    if (Bindex >= (BUFLEN - 4))			\
      flush_buf();				\
  }						\
while(0)

#define clear_buf()  Bindex = 0

/* Some flags. */
#define SIGNED_FL 1		/* The value is signed. */
#define ZERO_JUSTIFY_FL 2	/* Must justify with '0'. */
#define LEFT_JUSTIFY_FL 4	/* Must justify on the left. */
static int
sim_convert(char *buffer, int Bindex, int flags, int base, int width, unsigned int p);

/* A simple vprintf(). */
void
sim_vprintf(const char *message, void **param)
{
 int i;
 int flags;
 int width;
 int len;
 char buffer[BUFLEN];
 int Bindex;
  
 clear_buf();
 len = sim_strlen(message);
 if (len >= sizeof(buffer))
   return;
 for(i = 0; message[i]; i++)
   {
     if (message[i] != '%')
       {
         add_to_buf(message[i]);
         continue;
       }
     i++;
     /* A '%' has been found. */
     if (message[i] == '\0')
       continue;	/* forget the '%' at the end */
     if (message[i] == '%')
       {
         add_to_buf('%');	/* `%%' is replaced by `%'. */
         continue;
       }
     flags = 0;
     width = 0;		/* length of the field. */
     /* If '-' follows '%', jutify on the left. */
     if (message[i] == '-')
       {
         flags |= LEFT_JUSTIFY_FL;
         i++;
       }
     /* If '0' follows '%', pads will be inserted. */
     if (message[i] == '0')
       {
         flags |= ZERO_JUSTIFY_FL;
         i++;
       }
     /* Compute the field length. */
     while (message[i] >= '0' && message[i] <= '9')
       {
         width *= 10;
         width += message[i++] - '0';
       }
     if (message[i] == 'l')
       i++;
     switch(message[i])
       {
         case 'd':	/* %d */
           flags |= SIGNED_FL;
           Bindex = sim_convert(buffer, Bindex, flags, 10, width, (int)*param);
     	   param++;
           break;
         case 'u':	/* %u */
     	   Bindex = sim_convert(buffer, Bindex, flags, 10, width, (unsigned int)*param);
           param++;
           break;
         case 'p':	/* %p */
         case 'x':	/* %x */
           Bindex = sim_convert(buffer, Bindex, flags, 16, width, (unsigned int)*param);
           param++;
           break;
         case 'c':	/* %c */
           add_to_buf((int)(*param));
           param++;
           break;
         case 's':	/* %s */
           if ((char*)*param == (char*)0)
             *param = "(null)";
           if (width == 0)
             {
               flush_buf();
               sim_write(FD, (char*)*param, sim_strlen((char*)*param));
             }
           else
             {
               int l = sim_strlen( (char*)*param );
               sim_strncpy(&buffer[Bindex], (char*)*param, width);
               if (l < width)
                 sim_memset(&buffer[Bindex + l], 
                        flags & ZERO_JUSTIFY_FL ? '0' : ' ', width - l);
               Bindex += width;
     	     }
     	   param++;
     	   break;
         default:
           break;
       }
   }
 flush_buf();
} 	

/* Write P into the buffer according to these args:
 *  If SIGN is true, p is a signed.
 *  BASE if the base.
 *  If WITH_ZERO is true, '0' must be added.
 *  WIDTH is the width of the field.
 */
static int
sim_convert(char *buffer, int Bindex, int flags, int base, int width, unsigned int p)
{
 static char buf[40];
 int ind=0;
 int i;
 char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
 
 if (base < 2 || base > 36)
   return Bindex;
 
 if((flags & SIGNED_FL) && (signed int)p < 0)
   {
     p = abs((signed int)p);
     add_to_buf('-');
   }
 
 if (p == 0)
   buf[ind++] = '0';
 else
   while(p > 0)
     {
       buf[ind++] = digits[p % base];
       p /= base;
     }
 if (width > 0 && !(flags & LEFT_JUSTIFY_FL))
   {
     for(; ind < width; ind++)
       buf[ind]= flags & ZERO_JUSTIFY_FL ? '0': ' ';
   }
 /* reverse copy to buffer */
 for(i = ind -1; i >= 0; i--)
   add_to_buf (buf[i]);
 if (width > 0 && (flags & LEFT_JUSTIFY_FL))
   {
     for(; ind < width; ind++)
       add_to_buf (flags & ZERO_JUSTIFY_FL ? '0': ' ');
   }
  return Bindex;
}

/* A simple printf() which use chkr_vprintf(). */
void
sim_printf(const char *message, ...)
{
 if (flag_verbose)
   {
     va_list param;
     va_start(param, message);
     sim_vprintf(message, (void**)param);
     va_end(param);
   }
}
