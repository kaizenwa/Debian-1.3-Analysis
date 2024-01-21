/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Error handling routines.

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include "rts.h"
#include "varargs.h"

#define ORIG ((int)99999999)

struct errrec 
{
	int	errnum;
	char	*errnam;
};

static struct errrec errs[] = 
{
	{0, 	"An error which was reported during compilation"},
	{1,	"Array index out of bounds"},
	{2,	"Variant access error"},
	{3, 	"Attempt to dereference NIL pointer"},
	{4,	"Attempt to dereference undefined pointer"},
	{7,	"Scalar parameter out of bounds"},
	{8,	"Set parameter out of bounds"},
	{17,	"Input data out of bounds"},
	{18,	"Output data out of bounds"},
	{23,	"Dispose applied to NIL pointer"},
	{24,	"Dispose applied to undefined pointer"},
	{26,	"Index parameter of Pack out of bounds"},
	{29,	"Index parameter of Unpack out of bounds"},
	{32,	"Argument of Sqr out of range"},
	{37,	"Argument of Chr out of range"},
	{38,	"Argument of Succ out of range"},
	{39,	"Argument of Pred out of range"},
	{43,	"Attempt to use an undefined value"},
	{46,	"Second operand of MOD is <= 0"},
	{48,	"Function undefined upon return"},
	{49,	"Value to be assigned is out of bounds"},
	{50,	"Set value to be assigned is out of bounds"},
	{51,	"CASE selector value matches no case constant"},
	{52,	"Initial value of FOR control variable out of range"},
	{53,	"Final value of FOR control variable out of range"},
	{55,	"Integer data out of range"},
	{58,	"Field width must be positive"},
	{59,	"Index type of conformant array out of range"},
	{60,	"Field width can not be negative"}, /* can be zero in Extended Pascal */

	{40,	"Eof tested for unopened file"},
	{41,	"Eoln tested for unopened file"},
	{42,	"Eoln tested when Eof is on"},
	{61,	"Eoln applied to a non TEXT file"},
	{44,	"Floating divide by zero"},
	{45,	"Integer divide by 0"},
	{47,	"Integer overflow"},
	{100,	"Floating overflow"},
	{101,	"Floating underflow"},
	{200,	"Stack overflow"},

	{33,	"Argument of Ln is <= 0"},
	{34,	"Argument of Sqrt is < 0"},
	{102,	"Significancy lost in Cos - result set to zero"},
	{103,	"Significancy lost in Sin - result set to zero"},

	{201,	"Heap overflow"},
	{500,	"Call to predefined procedure 'bug'"},
	{501,	"Assert failure"},
	{502,	"Set element out of range"},

	{503,	"Heap overflow"},
	{504,	"Attempt to use undefined value of ordinal type"},
	{505,	"Attempt to use undefined value of set type"},
	{506,	"Attempt to use undefined value of integer type"},
	{507,	"Attempt to use undefined value of real type"},
	{508,	"Attempt to use undefined value of pointer type"},
	{509,	"Empty set range"},

	{9,	"File is not open for writing"},
	{10,	"File must be opened before writing"},
	{13,	"Reset to nonexistent internal file"},
	{14,	"File is not open for reading"},
	{15,	"File must be opened before reading"},
	{16,	"Attempt to read past EOF"},
	{300,	"File not found"},
	{301,	"File cannot be accessed"},
	{302,	"Attempt to open internal file as external"},
	{303,	"File is write protected"},

	{400,	"Operation available only to random access files"},
/*	{401,	"Random access to internal file"}, 		    Why not? */
	{402,	"Random access file is not open for writing"},
	{403,	"Random access file is not open for reading"},
	{404,	"DefineSize requires write access"},
	{405,	"Writing past current size of random access file"},
	{406,	"Attempt to read past end of random access file"},
	{407,	"File has not been opened"},
	{408,	"Attempt to test EOF for random access file"},
	{409,	"File is not opened for Seek* operations"},
	{410,	"Attempt to access elements before beginning of random access file"},
	{411,   "Attempt to modify a read only file"},
	{412,   "Random access file back stepping failed"},

	{600,   "Digit expected"},
	{601,   "Syntax error in floating point number"},
	{602,   "Attempt to use disposed pointer"},
	{603,   "Attempt to use disposed object"},
	{604,   "Digit expected after sign"},
	{605,   "Sign or digit expected"},
        {606,   "Overflow while reading integer"},
	{607,   "Digit expected after decimal point"},
        {608,   "Digit expected while reading exponent"},
	{609,	"Exponent out of range"},

	{621, 	"Overflow in exponentiation"},
	{622, 	"In x**y, x must be >= 0 if y < 0 and y is not an integer"},
	{623, 	"The exponent of 0 must be greater than 0"},

	{700,   "(gpc-rts) Internal file namei wrong"},
	{701,   "(gpc-rts) _p_initfdr() has not been called for file"},
	{702,   "(gpc-rts) _p_initfdr() called for an initialized file?"},
	{703,   "(gpc-rts) _p_initfdr() External file has no name"},
	{704,   "(gpc-rts) _p_bcmp invalid string length"},
	{705,   "(gpc-rts) invalid set element mask"},

	{0,	0},	/* End of error message table */
};

#ifdef notdef
_p_funny()
{
    fprintf(stderr,"Gpc :-) Hello world!\n");
}
#endif

/*
 * For GNU malloc.
 */
void
malloc_warning(str)
char *str;
{
    fprintf(stderr, "%%Gpc heap warning: %s\n", str);
}

char *
_p_errmsg(n)
int	n;
{
    struct errrec	*p;

    for(p = errs; p->errnam; p++)
      if (p->errnum == n)
	return(p->errnam);

    return (char *)NULL;
}

void
_p_prmessage(str, num, Warning)
char *str;
int num;
int Warning;
{
    _p_fflush(TRUE); /* Flush terminal output buffers */
    if (num <= 0) fprintf(stderr, "\n");
    fprintf(stderr,"%s: ", (Warning ?
			    "%GPC runtime warning" : "?GPC runtime error"));
    if (str)
	fprintf(stderr,"%s (#%d)",str,num);
    else
	fprintf(stderr,"runtime error (#%d)", num);
    fprintf(stderr, "\n");
}

static void
_p_igeneric(n, Warning)
int n;
int Warning;
{
    D(1, fprintf(stderr, "_p_igeneric: Warning=%d\n", Warning));

    if (! Warning || Gpc_warn || Gpc_debug)
      _p_prmessage(_p_errmsg(n), n, Warning);

    if (Warning || Gpc_debug)
      return;

    exit(1);
}

/* Return value only to make some macros simpler */
int
_p_generic(n)
int n;
{
  _p_igeneric(n, FALSE);

  /* The return value does not actually mean anything */
  return 1;
}

void
_p_warning(str)
     char *str;
{
  if (Gpc_warn || Gpc_debug)
    fprintf (stderr, "%s\n", str);
}

/* Print a message of some given class.

   The first argument HOW is the class, the seconds is used
   as a fprintf FORMAT string. The rest are arguments
   to that format string.

   If HOW is:
   IGNORE : return;
   REPORT : print %Gpc: STRING and return
   ABORT  : print ?Gpc: STRING and exit
            (If Gpc_debug is set, do not exit; return instead)
 */

/* VARARGS */
void
_p_error (va_alist)
va_dcl
{
  va_list args;
  int how;
  char *format;
  char *wtype = "?Gpc: ";

  va_start (args);

  _p_fflush(FALSE);

  how = va_arg (args, int);
  if (how == IGNORE)
    return;

  if (how == REPORT)
    wtype = "%Gpc: ";

  write (fileno (stderr), wtype, strlen (wtype));
  
  format = va_arg (args, char *);

  vfprintf (stderr, format, args);
  fprintf (stderr, "\n");

#if 0
  /* This does not give any information */
  if (how != REPORT)
    _p_generic(ORIG);
#endif

  if (!Gpc_debug && how == ABORT)
    exit(1);

  va_end (args);
}
