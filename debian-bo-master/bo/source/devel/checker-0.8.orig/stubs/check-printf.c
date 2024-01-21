/* Check arguments for printf, scanf ...
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

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

#include <stddef.h>
#include <ctype.h>
#include <sys/types.h>
#include "check-printf.h"
#include "checker_api.h"

#define	LONGINT  1
#define	SHORTINT 2
#define LONGDBL  4
#define WIDTH    8
#define SUPPRESS 16

#define CONV_CHAR 1
#define CONV_INT  2
#define CONV_STRING 3
#define CONV_DOUBLE 4
#define CONV_POINTER 5

size_t
strnlen (const char *s, size_t n)
{
  size_t res = 0;
  while (*s++ && res < n)
    res++;
  if (res < n)
    res++;
  return res;
}

#ifdef CHECK_INCOMING_ARGS
#undef CHECK_INCOMING_ARGS
#define CHECK_INCOMING_ARGS 1
#else
#define CHECK_INCOMING_ARGS 0
#endif

void
check_printf_format (char const *name, char const *fmt, va_list ap, int type, int va)
{
  int flags;			/* flags as above */
  char c;
  int width;
  int conv;
  int num;

  num = 0;
  
  /* Scan the format for conversions (`%' character). */
  while (1)
    {
      while (*fmt && *fmt != '%')
	fmt++;

      if (*fmt == '\0')
	return;

      fmt++;			/* skip over '%' */

      flags = 0;
      width = 0;
      conv = 0;
      if (type >= 0 && type == num)
        return;
      else
        num++;

    rflag:
      switch ((c = *fmt++))
	{
	case '%':
	  continue;	/* ignore this char */
	case ' ':
	case '#':
	case '-':
	case '+':
	  goto rflag;
	case '*':
	  if (type == TYPE_PRINTF)
	    {
	      int *addr = &va_arg (ap, int);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (int), CHKR_RO, "%*");
	    }
	  else
	    flags |= SUPPRESS;
	  goto rflag;
	case '.':
	  if (type == TYPE_PRINTF && *fmt++ == '*')
	    {
	      int *addr = &va_arg (ap, int);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (int), CHKR_RO, "%*");
	      width = *addr;
	      flags |= WIDTH;
	      goto rflag;
	    }
	  goto rflag;
	case '0':
	  goto rflag;
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	    do
	      {
		width = width * 10 + (c - '0');
		c = *fmt++;
	      }
	    while (isdigit (c));
	    fmt--;
	    flags |= WIDTH;
	  goto rflag;
	case 'L':
	  flags |= LONGDBL;
	  goto rflag;
	case 'h':
	  flags |= SHORTINT;
	  goto rflag;
	case 'l':
	  flags |= LONGINT;
	  goto rflag;
	case 'c':
	  conv = CONV_CHAR;
	  break;
	case 'D':
	  flags |= LONGINT;
	case 'd':
	case 'i':
	  conv = CONV_INT;
	  break;
	case 'e':
	case 'E':
	case 'f':
	case 'F':
	case 'g':
	case 'G':
	  conv = CONV_DOUBLE;
	  break;
	case 'n':
	  if (type != TYPE_PRESCANF)
	    {
	      if (flags & LONGINT)
		{
		  long **addr;
		  addr = &va_arg (ap, long *);
		  if (va || CHECK_INCOMING_ARGS)
		    stubs_chkr_check_addr (addr, sizeof (long *), CHKR_RO, "%n");
		  stubs_chkr_check_addr (*addr, sizeof (long), CHKR_WO, "%n");
		}
	      else if (flags & SHORTINT)
		{
		  short **addr;
		  addr = &va_arg (ap, short *);
		  if (va || CHECK_INCOMING_ARGS)
		    stubs_chkr_check_addr (addr, sizeof (short *), CHKR_RO, "%n");
		  stubs_chkr_check_addr (*addr, sizeof (short), CHKR_WO, "%n");
		}
	      else
		{
		  int **addr;
		  addr = &va_arg (ap, int *);
		  if (va || CHECK_INCOMING_ARGS)
		    stubs_chkr_check_addr (addr, sizeof (int *), CHKR_RO, "%n");
		  stubs_chkr_check_addr (*addr, sizeof (int), CHKR_WO, "%n");
		}
	    }
	  continue;		/* no output */
	case 'O':
	  flags |= LONGINT;
	case 'o':
	  conv = CONV_INT;
	  break;
	case 'p':
	  conv = CONV_POINTER;
	  break;
	case 's':
	  conv = CONV_STRING;
	  break;
	case 'U':
	  flags |= LONGINT;
	case 'u':
	  conv = CONV_INT;
	  break;
	case 'X':
	case 'x':
	  conv = CONV_INT;
	  break;
	case '[':
	  if (type != TYPE_PRINTF)
	    {
	      if (*fmt++ == '^')
		fmt++;		/* skip the character after the '^' */
	      while (*fmt && *fmt != ']')
		fmt++;
	      if (!*fmt)
	        {
	          chkr_report (M_C_FUN_LB_ET);
	          chkr_printf ("%s: Bad format after '['\n", name);
	          chkr_disp_call_chain ();
		  return;
		}
	      break;
	    }
	  /* FALL THROUGH */
	default:
	  chkr_report (M_C_FUN_LB_ET);
	  chkr_printf ("%s: Unknown type '%c'\n", name, c);
	  chkr_disp_call_chain ();
	  return;
	}

      if (type == TYPE_PRINTF)
	switch (conv)
	  {
	  case CONV_CHAR:
	    {
	      int *addr;
	      addr = &va_arg (ap, int);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (int), CHKR_RO, "%c");
	    }
	    break;
	  case CONV_INT:
	    if (flags & LONGINT)
	      {
		long *addr;
		addr = &va_arg (ap, long);
		if (va || CHECK_INCOMING_ARGS)
		  stubs_chkr_check_addr (addr, sizeof (long), CHKR_RO, "%ld");
	      }
	    else if (flags & SHORTINT)
	      {
		int *addr;
		addr = &va_arg (ap, int);
		if (va || CHECK_INCOMING_ARGS)
		  stubs_chkr_check_addr (addr, sizeof (short), CHKR_RO, "%h");
	      }
	    else
	      {
		int *addr;
		addr = &va_arg (ap, int);
		if (va || CHECK_INCOMING_ARGS)
		  stubs_chkr_check_addr (addr, sizeof (int), CHKR_RO, "%d");
	      }
	    break;
	  case CONV_DOUBLE:
	    {
	      double *addr;
	      addr = &va_arg (ap, double);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (double), CHKR_RO, "%g");
	    }
	    break;
	  case CONV_POINTER:
	    {
	      void **addr;
	      addr = &va_arg (ap, void *);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (void *), CHKR_RO, "%p");
	    }
	    break;
	  case CONV_STRING:
	    {
	      char **addr;
	      addr = &va_arg (ap, char *);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (char *), CHKR_RO, "%s");
	      if (flags & WIDTH)
	        {
	          if (width)
	            {
	              width = strnlen (*addr, width);
	              stubs_chkr_check_addr (*addr, width, CHKR_RO, "%s");
	            }
	        }
	      else
	        stubs_chkr_check_str (*addr, CHKR_RO, "%s");
	    }
	    break;
	  }
      else if (type == TYPE_PRESCANF && !(flags & SUPPRESS))
	switch (conv)
	  {
	  case CONV_CHAR:
	    {
	      char **addr;
	      addr = &va_arg (ap, char*);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (char *), CHKR_RO, "%c");
	      stubs_chkr_check_addr (*addr, width ? width * sizeof(char) : sizeof(char), CHKR_MW, "%c");
	    }
	    break;
	  case CONV_INT:
	    if (flags & LONGINT)
	      {
		long **addr;
		addr = &va_arg (ap, long*);
		if (va || CHECK_INCOMING_ARGS)
		  stubs_chkr_check_addr (addr, sizeof (long*), CHKR_RO, "%ld");
		stubs_chkr_check_addr (*addr, sizeof (long), CHKR_MW, "%ld");
	      }
	    else if (flags & SHORTINT)
	      {
		int **addr;
		addr = &va_arg (ap, int*);
		if (va || CHECK_INCOMING_ARGS)
		  stubs_chkr_check_addr (addr, sizeof (short*), CHKR_RO, "%h");
		stubs_chkr_check_addr (*addr, sizeof (short), CHKR_MW, "%h");
	      }
	    else
	      {
		int **addr;
		addr = &va_arg (ap, int*);
		if (va || CHECK_INCOMING_ARGS)
		  stubs_chkr_check_addr (addr, sizeof (int*), CHKR_RO, "%d");
		stubs_chkr_check_addr (*addr, sizeof (int), CHKR_MW, "%d");
	      }
	    break;
	  case CONV_DOUBLE:
	    {
	      double **addr;
	      addr = &va_arg (ap, double*);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (double*), CHKR_RO, "%e");
	      stubs_chkr_check_addr (*addr, sizeof (double), CHKR_MW, "%e");
	    }
	    break;
	  case CONV_POINTER:
	    {
	      void ***addr;
	      addr = &va_arg (ap, void **);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (void **), CHKR_RO, "%p");
	      stubs_chkr_check_addr (*addr, sizeof (void *), CHKR_MW, "%p");
	    }
	    break;
	  case CONV_STRING:
	    {
	      char **addr;
	      addr = &va_arg (ap, char *);
	      if (va || CHECK_INCOMING_ARGS)
	        stubs_chkr_check_addr (addr, sizeof (char *), CHKR_RO, "%s");
	      if (!width)
	        {
	          chkr_report (M_C_FUN_LB_ET);
	          chkr_printf("%s: No width field for string.\n", name);
	          chkr_disp_call_chain ();
	        }
	      else
	        stubs_chkr_check_addr (*addr, width, CHKR_MW, "%s");
	    }
	    break;
	  }
      else if (!(flags & SUPPRESS))
	switch (conv)
	  {
	  case CONV_CHAR:
	    {
	      char **addr;
	      addr = &va_arg (ap, char*);
	      stubs_chkr_set_right (*addr, width ? width * sizeof(char) : sizeof(char), CHKR_RW);
	    }
	    break;
	  case CONV_INT:
	    if (flags & LONGINT)
	      {
		long **addr;
		addr = &va_arg (ap, long*);
		stubs_chkr_set_right (*addr, sizeof (long), CHKR_RW);
	      }
	    else if (flags & SHORTINT)
	      {
		int **addr;
		addr = &va_arg (ap, int*);
		stubs_chkr_set_right (*addr, sizeof (short), CHKR_RW);
	      }
	    else
	      {
		int **addr;
		addr = &va_arg (ap, int*);
		stubs_chkr_set_right (*addr, sizeof (int), CHKR_RW);
	      }
	    break;
	  case CONV_DOUBLE:
	    {
	      double **addr;
	      addr = &va_arg (ap, double*);
	      stubs_chkr_set_right (*addr, sizeof (double), CHKR_RW);
	    }
	    break;
	  case CONV_POINTER:
	    {
	      void ***addr;
	      addr = &va_arg (ap, void **);
	      stubs_chkr_set_right (*addr, sizeof (void *), CHKR_RW);
	    }
	    break;
	  case CONV_STRING:
	    {
	      char **addr;
	      int n;
	      addr = &va_arg (ap, char *);
	      n = strlen(*addr) + 1;	/* FIXME */
	      stubs_chkr_set_right (*addr, n * sizeof (char), CHKR_RW);
	    }
	    break;
	  }
    }
}

#ifdef TEST
int
my_printf (char *format,...)
{
  va_list param;
  va_start (param, format);
  check_printf_format ("my_printf", format, param, TYPE_PRINTF);
  return vprintf (format, param);
}

int
my_scanf (char *format,...)
{
  va_list param;
  int n;
  va_start (param, format);
  check_printf_format ("my_scanf", format, param, TYPE_PRESCANF);
  n = vscanf (format, param);
  if (n != EOF)
    check_printf_format ("my_scanf", format, param, n);
  return n;
}

void
stubs_chkr_check_addr (const PTR ptr, int len, int right)
{
  printf ("Check at %p for %d bytes (%d)\n", ptr, len, right);
}

void
stubs_chkr_check_str (const PTR ptr, int right)
{
  printf ("Check string at %p (%d)\n", ptr, right);
}

void
stubs_chkr_set_right (const PTR ptr, int len, int right)
{
  printf ("Set right at %p for %d bytes (%d)\n", ptr, len, right);
}

int
main (int argc, char *argv[])
{
  int n;
  char c[20];
  my_printf ("Hello 100%%\n");
  my_printf ("%d + %d = %d\n", 1, 4, 4 + 1);
  my_printf ("%s: %s\n", argv[0], "Hello");
  my_printf ("%s: %s%n\n", argv[0], "Hello", &n);
  my_printf ("The last line has %d chars\n", n);
  my_printf ("Please enter a small string:\n");
  my_scanf ("%s", c);
  my_scanf ("%20s", c);
  my_printf ("Please enter a number:\n");
  my_scanf ("%d", &n);
  my_printf ("Please enter a number, a string and another number:\n");
  my_scanf ("%d%10s%d", &n, c, &n);
  return 0;
}

#endif /* TEST */
