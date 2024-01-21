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
/* the %f option is allowed only if VPRINTF_FLOAT is defined.  In this case,
   chkr_vprintf need to call ecvt().  */
   
#include <stdarg.h>
#include "checker.h"
#include "message.h"
#ifdef VPRINTF_FLOAT
#include "math-ieee.h"
#endif

#ifdef CHECK_OUTPUT_FILE
/* Be sure the ouput file is ok, ie it was not closed by the user (and reopen)
 */
extern void check_output_spec (void);
#endif

/* Buffer for chkr_vprintf.  This code is not really beautiful...  */
#define BUFLEN 2048
static char buffer[BUFLEN];
static int Bindex=0;

static INLINE void
flush_buf (void)
{
  buffer[Bindex] = '\0';
  (*___chkr_trap)(buffer);
  Bindex = 0;
}

static INLINE void
add_to_buf (char c)
{
  buffer[Bindex++] = c;
  if (Bindex >= (BUFLEN - 1))
    flush_buf ();
}

static INLINE void
clear_buf (void)
{
  Bindex = 0;
}

/* Some flags.  */
#define SIGNED_FL 1		/* The value is signed. */
#define ZERO_JUSTIFY_FL 2	/* Must justify with '0'. */
#define LEFT_JUSTIFY_FL 4	/* Must justify on the left. */
static void convert(int flags, int base, int width, unsigned int);

/* A simple vprintf().  */
void
chkr_vprintf (const char *message, PTR *param)
{
 int i;
 int flags;
 int width;
 int len;
  
 clear_buf ();
 len = strlen (message);
 if (len >= sizeof (buffer))
   {
     (*___chkr_trap)("message too long in chkr_vprintf\n");
     chkr_abort ();
   }
 for (i = 0; message[i]; i++)
   {
     if (message[i] != '%')
       {
         add_to_buf (message[i]);
         continue;
       }
     i++;
     /* A '%' has been found. */
     if (message[i] == '\0')
       break;		/* forget the '%' at the end */
     if (message[i] == '%')
       {
         add_to_buf ('%');	/* `%%' is replaced by `%'. */
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
     switch (message[i])
       {
         case 'd':	/* %d */
           flags |= SIGNED_FL;
           convert (flags, 10, width, (int)*param);
     	   param++;
           break;
         case 'f':
#ifdef VPRINTF_FLOAT
           {
             int sign;
             int decpt;
             char *b;
             int j;
             double d;
             
             d = *((double*) param);
             param += 2;
             b = ecvt (d, 8, &decpt, &sign);
             add_to_buf (sign ? '-' : '+');
             add_to_buf (b[0]);
             add_to_buf ('.');
             for (j = 1; j < 8; j++)
               add_to_buf (b[j]);
             add_to_buf ('e');
             convert (ZERO_JUSTIFY_FL | SIGNED_FL, 10, 3, decpt - 1);
           }
#else
	   add_to_buf ('\n');
	   flush_buf ();
	   chkr_printf ("!!! %%f not supported if VPRINTF_FLOAT is not defined.\n");
	   chkr_abort ();
#endif /* VPRINTF_FLOAT */
           break;
         case 'u':	/* %u */
     	   convert (flags, 10, width, (unsigned int)*param);
           param++;
           break;
         case 'p':	/* %p */
           add_to_buf ('0');
           add_to_buf ('x');
         case 'x':	/* %x */
           convert (flags, 16, width, (unsigned int)*param);
           param++;
           break;
         case 'c':	/* %c */
           add_to_buf ((int)(*param));
           param++;
           break;
         case 's':	/* %s */
           if ((char*)*param == (char*)0)
             *param = "(null)";
           if (width == 0)
             {
               int l;
               l = strlen ((char*)*param);
               if (Bindex + l < BUFLEN - 1)
                 {
                   strcpy (buffer + Bindex, (char*)*param);
                   Bindex += l;
                 }
               else
                 {
                   flush_buf ();
                   (*___chkr_trap)((char*)*param);
                 }
             }
           else
             {
               int l;
               char *e = memchr ((char*)*param, 0, width);
               if (e)
                 l = e - ((char*)*param);
               else
                 l = width;
                 
               if (l + Bindex > BUFLEN - 1)
                 flush_buf ();
                 
               /* FIXME */
               strncpy (&buffer[Bindex], (char*)*param, width);
               if (l < width)
                 memset (&buffer[Bindex + l], 
                        flags & ZERO_JUSTIFY_FL ? '0' : ' ', width - l);
               Bindex += width;
     	     }
     	   param++;
     	   break;
         default:
           break;
       }
   }
 flush_buf ();
} 	

/* Write P into the buffer according to these args:
 *  If SIGN is true, p is a signed.
 *  BASE if the base.
 *  If WITH_ZERO is true, '0' must be added.
 *  WIDTH is the width of the field.
 */
static void
convert (int flags, int base, int width, unsigned int p)
{
 static char buf[40];
 int ind=0;
 int i;
 char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
 
 if (base < 2 || base > 36)
   return;
 
 if ((flags & SIGNED_FL) && (signed int)p < 0)
   {
     p = abs ((signed int)p);
     add_to_buf ('-');
   }
 
 if (p == 0)
   buf[ind++] = '0';
 else
   while (p > 0)
     {
       buf[ind++] = digits[p % base];
       p /= base;
     }
 if (width > 0 && !(flags & LEFT_JUSTIFY_FL))
   {
     for(; ind < width; ind++)
       buf[ind] = (flags & ZERO_JUSTIFY_FL) ? '0': ' ';
   }
 /* Reverse copy to buffer.  */
 for (i = ind -1; i >= 0; i--)
   add_to_buf (buf[i]);
 if (width > 0 && (flags & LEFT_JUSTIFY_FL))
   {
     for(; ind < width; ind++)
       add_to_buf ((flags & ZERO_JUSTIFY_FL) ? '0': ' ');
   }
}

/* A simple printf() which use chkr_vprintf().  */
void
chkr_printf (const char *message, ...)
{
 va_list param;
 va_start (param, message);
 chkr_vprintf (message, (PTR*) param);
 va_end (param);
}

/* This function must be called at the beginning of each message.  */
void
chkr_header (const char *message, ...)
{
 va_list param;
 va_start (param, message);
 chkr_printf (M_FROM_CHECKER, my_pid_c);
 chkr_vprintf (message, (PTR*)param);
 va_end (param);
}

void
chkr_puts (const char *str)
{
  (*___chkr_trap) (str);
}

/* The function which write. */
static void
__default_chkr_trap (const char *message)
{
#ifdef CHKR_OUTPUT_FILE
 check_output_spec ();
#endif
 write (chkr_out, message, strlen (message));
}

void (*___chkr_trap) (const char *message) = __default_chkr_trap;
