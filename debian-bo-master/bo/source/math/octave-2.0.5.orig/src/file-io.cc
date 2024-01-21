/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

// Originally written by John C. Campbell <jcc@bevo.che.wisc.edu>
//
// Thomas Baier <baier@ci.tuwien.ac.at> added the original versions of
// the following functions:
//
//   popen
//   pclose
//   execute       (now popen2.m)
//   sync_system   (now merged with system)
//   async_system  (now merged with system)

// Completely rewritten by John W. Eaton <jwe@bevo.che.wisc.edu>,
// April 1996.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <climits>

#include <iostream.h>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "defun.h"
#include "error.h"
#include "file-ops.h"
#include "help.h"
#include "lo-ieee.h"
#include "oct-fstrm.h"
#include "oct-iostrm.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-prcstrm.h"
#include "oct-stream.h"
#include "oct-strstrm.h"
#include "pager.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"

void
initialize_file_io (void)
{
  octave_istream *stdin_stream
    = new octave_istream (&cin, "stdin");

  // This uses octave_stdout (see pager.h), not cout so that Octave's
  // standard output stream will pass through the pager.

  octave_ostream *stdout_stream
    = new octave_ostream (&octave_stdout, "stdout");

  octave_ostream *stderr_stream
    = new octave_ostream (&cerr, "stderr");

  octave_stream_list::insert (stdin_stream);
  octave_stream_list::insert (stdout_stream);
  octave_stream_list::insert (stderr_stream);
}

void
close_files (void)
{
  octave_stream_list::clear ();
}

static void
gripe_invalid_file_id (const char *fcn)
{
  ::error ("%s: invalid file id", fcn);
}

static int
fopen_mode_to_ios_mode (const string& mode)
{
  int retval = 0;

  if (! mode.empty ())
    {
      // Could probably be faster, but does it really matter?

      if (mode == "r")
	retval = ios::in;
      else if (mode == "w")
	retval = ios::out | ios::trunc;
      else if (mode == "a")
	retval = ios::out | ios::app;
      else if (mode == "r+")
	retval = ios::in | ios::out;
      else if (mode == "w+")
	retval = ios::in | ios::out | ios::trunc;
      else if (mode == "a+")
	retval = ios::in | ios::out | ios::app;
      else if (mode == "rb")
	retval = ios::in | ios::bin;
      else if (mode == "wb")
	retval = ios::out | ios::trunc | ios::bin;
      else if (mode == "ab")
	retval = ios::out | ios::app | ios::bin;
      else if (mode == "r+b")
	retval = ios::in | ios::out | ios::bin;
      else if (mode == "w+b")
	retval = ios::in | ios::out | ios::trunc | ios::bin;
      else if (mode == "a+b")
	retval = ios::in | ios::out | ios::app | ios::bin;
      else
	::error ("invalid mode specified");
    }

  return retval;
}

DEFUN (fclose, args, ,
  "fclose (FILENUM): close a file")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      retval = (double) octave_stream_list::remove (args(0));

      if (retval < 0)
	gripe_invalid_file_id ("fclose");
    }
  else
    print_usage ("fclose");

  return retval;
}

DEFUN (fflush, args, ,
  "fflush (FILENUM): flush buffered data to output file")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      // XXX FIXME XXX -- any way to avoid special case for stdout?

      int fid = octave_stream_list::get_file_number (args (0));

      if (fid == 1)
	{
	  flush_octave_stdout ();

	  retval = 0.0;
	}
      else
	{
	  octave_stream *os = octave_stream_list::lookup (fid);

	  if (os)
	    retval = (double) os->flush ();
	  else
	    gripe_invalid_file_id ("fflush");
	}
    }
  else
    print_usage ("fflush");

  return retval;
}

DEFUN (fgetl, args, ,
  "[STRING, LENGTH] = fgetl (FILENUM [, LENGTH])\n\
\n\
read a string from a file")
{
  octave_value_list retval;

  retval(1) = 0.0;
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	{
	  octave_value len_arg = (nargin == 2)
	    ? args(1) : octave_value ((double) INT_MAX);

	  bool err = false;

	  string tmp = os->getl (len_arg, err);

	  if (! err)
	    {
	      retval(1) = (double) tmp.length ();
	      retval(0) = tmp;
	    }
	}
      else
	gripe_invalid_file_id ("fgetl");
    }
  else
    print_usage ("fgetl");

  return retval;
}

DEFUN (fgets, args, ,
  "[STRING, LENGTH] = fgets (FILENUM [, LENGTH])\n\
\n\
read a string from a file")
{
  octave_value_list retval;

  retval(1) = 0.0;
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	{
	  octave_value len_arg = (nargin == 2)
	    ? args(1) : octave_value ((double) INT_MAX);

	  bool err = false;

	  string tmp = os->gets (len_arg, err);

	  if (! err)
	    {
	      retval(1) = (double) tmp.length ();
	      retval(0) = tmp;
	    }
	}
      else
	gripe_invalid_file_id ("fgets");
    }
  else
    print_usage ("fgets");

  return retval;
}

static octave_base_stream *
do_stream_open (const string& name, const string& mode,
		const string& arch, int& fid)
{
  octave_base_stream *retval = 0;

  fid = -1;

  int md = fopen_mode_to_ios_mode (mode);

  if (! error_state)
    {
      oct_mach_info::float_format flt_fmt =
	oct_mach_info::string_to_float_format (arch);

      if (! error_state)
	retval = new octave_fstream (name, md, flt_fmt);
    }

  return retval;
}

static octave_base_stream *
do_stream_open (const octave_value& tc_name, const octave_value& tc_mode,
		const octave_value& tc_arch, const char *fcn, int& fid)
{
  octave_base_stream *retval = 0;

  fid = -1;

  string name = tc_name.string_value ();

  if (! error_state)
    {
      string mode = tc_mode.string_value ();

      if (! error_state)
	{
	  string arch = tc_arch.string_value ();

	  if (! error_state)
	    retval = do_stream_open (name, mode, arch, fid);
	  else
	    ::error ("%s: architecture type must be a string", fcn);
	}
      else
	::error ("%s: file mode must be a string", fcn);
    }
  else
    ::error ("%s: file name must be a string", fcn);

  return retval;
}

DEFUN (fopen, args, ,
  "[FILENUM, ERRMSG] = fopen (FILENAME, MODE [, ARCH]): open a file\n\
\n\
  FILENAME is a string specifying the name of the file.\n\
\n\
  MODE is a string specifying whether the file should be opened for\n\
  reading, writing, or both.  Valid values for MODE include:\n\
\n\
    r  : open text file for reading\n\
    w  : open text file for writing; discard previous contents if any\n\
    a  : append; open or create text file for writing at end of file\n\
    r+ : open text file for update (i.e., reading and writing)\n\
    w+ : create text file for update; discard previous contents if any\n\
    a+ : append; open or create text file for update, writing at end\n\
\n\
  Update mode permits reading from and writing to the same file.\n\
\n\
  ARCH is a string specifying the default data format for the file.\n\
  Valid values for ARCH are:\n\
\n\
    native   --  the format of the current machine (this is the default)\n\
    ieee-le  --  IEEE big endian\n\
    ieee-be  --  IEEE little endian\n\
    vaxd     --  VAX D floating format\n\
    vaxg     --  VAX G floating format\n\
    cray     --  Cray floating format\n\
\n\
  however, conversions are currently only supported for ieee-be, and\n\
  ieee-le formats.\n\
\n\
\n\
  FILENUM is a number that can be used to refer to the open file.\n\
  If fopen fails, FILENUM is set to -1 and ERRMSG contains a\n\
  system-dependent error message")
{
  octave_value_list retval;

  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args(0).is_string () && args(0).string_value () == "all")
	retval(0) = octave_stream_list::open_file_numbers ();
      else
	{
	  string_vector tmp = octave_stream_list::get_info (args(0));

	  if (! error_state)
	    {
	      retval(2) = tmp(2);
	      retval(1) = tmp(1);
	      retval(0) = tmp(0);
	    }
	}

      return retval;
    }

  if (nargin > 0 && nargin < 4)
    {
      octave_value mode = (nargin == 2 || nargin == 3)
	? args(1) : octave_value ("r");

      octave_value arch = (nargin == 3)
	? args(2) : octave_value ("native");

      int fid = -1;

      octave_base_stream *os
	= do_stream_open (args(0), mode, arch, "fopen", fid);

      if (os)
	{
	  if (os->ok () && ! error_state)
	    {
	      retval(1) = "";
	      retval(0) = (double) octave_stream_list::insert (os);
	    }
	  else
	    {
	      int errno = 0;
	      retval(1) = os->error (false, errno);
	      retval(0) = -1.0;
	    }
	}
      else
	::error ("fopen: internal error");
    }
  else
    print_usage ("fopen");

  return retval;
}

DEFUN (freport, args, ,
  "freport (): list open files and their status")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    warning ("freport: ignoring extra arguments");

  octave_stdout << octave_stream_list::list_open_files ();

  return retval;
}

DEFUN (frewind, args, ,
  "frewind (FILENUM): set file position at beginning of file")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	retval = (double) os->rewind ();
      else
	gripe_invalid_file_id ("frewind");
    }
  else
    print_usage ("frewind");

  return retval;
}

DEFUN (fseek, args, ,
  "fseek (FILENUM, OFFSET [, ORIGIN])\n\
\n\
set file position for reading or writing.\n\
\n\
ORIGIN may be one of:\n\
\n\
  SEEK_SET : offset is relative to the beginning of the file (default)\n\
  SEEK_CUR : offset is relative to the current position\n\
  SEEK_END : offset is relative to the end of the file")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	{
	  octave_value origin_arg = (nargin == 3)
	    ? args(2) : octave_value (-1.0);

	  retval = (double) os->seek (args(1), origin_arg);
	}
      else
	::error ("fseek: invalid file id");
    }
  else
    print_usage ("fseek");

  return retval;
}

DEFUN (ftell, args, ,
  "POSITION = ftell (FILENUM): returns the current file position")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	retval = (double) os->tell ();
      else
	gripe_invalid_file_id ("ftell");
    }
  else
    print_usage ("ftell");

  return retval;
}

DEFUN (fprintf, args, ,
  "fprintf (FILENUM, FORMAT, ...)")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin > 1)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	{
	  if (args(1).is_string ())
	    {
	      string fmt = args(1).string_value ();

	      octave_value_list tmp_args;

	      if (nargin > 2)
		{
		  tmp_args.resize (nargin-2, octave_value ());

		  for (int i = 2; i < nargin; i++)
		    tmp_args(i-2) = args(i);
		}

	      retval = os->printf (fmt, tmp_args);
	    }
	  else
	    ::error ("fprintf: format must be a string");
	}
      else
	gripe_invalid_file_id ("fprintf");
    }
  else
    print_usage ("fprintf");

  return retval;
}

DEFUN (fputs, args, ,
  "fputs (FILENUM, STRING)")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	retval = os->puts (args(1));
      else
	gripe_invalid_file_id ("fputs");
    }
  else
    print_usage ("fputs");

  return retval;
}

DEFUN (sprintf, args, ,
  "[s, errmsg, status] = sprintf (FORMAT, ...)")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      retval(2) = -1.0;
      retval(1) = "unknown error";
      retval(0) = "";

      octave_ostrstream ostr;

      octave_stream os (&ostr, true);

      if (os)
	{
	  if (args(0).is_string ())
	    {
	      string fmt = args(0).string_value ();

	      octave_value_list tmp_args;

	      if (nargin > 1)
		{
		  tmp_args.resize (nargin-1, octave_value ());

		  for (int i = 1; i < nargin; i++)
		    tmp_args(i-1) = args(i);
		}

	      retval(2) = os.printf (fmt, tmp_args);
	      retval(1) = os.error ();
	      char *tmp = ostr.str ();
	      retval(0) = tmp;
	      delete [] tmp;
	    }
	  else
	    ::error ("sprintf: format must be a string");
	}
      else
	::error ("sprintf: unable to create output buffer");
    }
  else
    print_usage ("sprintf");

  return retval;
}

DEFUN (fscanf, args, ,
  "[A, COUNT] = fscanf (FILENUM, FORMAT [, SIZE])\n\
\n\
Read from FILENUM according to FORMAT, returning the result in the\n\
matrix A.  SIZE is optional.  If present, it can be one of\n\
\n\
       Inf : read as much as possible, returning a column vector\n\
             (unless doing all character conversions, in which case a\n\
             string is returned)\n\
        NR : read as much as possible, returning a matrix with NR rows\n\
  [NR, NC] : read up to NR x NC elements, returning a matrix with NR rows\n\
 [NR, Inf] : same as NR\n\
\n\
If it is omitted, a value of Inf is assumed.\n\
\n\
The number of items successfully read is returned in COUNT.\n\
\n\
[A, B, ...] = fscanf (FILENUM, FORMAT, \"C\")\n\
\n\
Read from FILENUM according to FORMAT, with each conversion specifier\n\
in FORMAT corresponding to a single scalar return value.  This form is\n\
more `C-like', and also compatible with previous versions of Octave")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 3 && args(2).is_string ())
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	{
	  if (args(1).is_string ())
	    {
	      string fmt = args(1).string_value ();

	      retval = os->oscanf (fmt);
	    }
	  else
	    ::error ("fscanf: format must be a string");
	}
      else
	gripe_invalid_file_id ("fscanf");
    }
  else
    {
      retval (1) = 0.0;
      retval (0) = Matrix ();

      if (nargin == 2 || nargin == 3)
	{
	  octave_stream *os = octave_stream_list::lookup (args(0));

	  if (os)
	    {
	      if (args(1).is_string ())
		{
		  string fmt = args(1).string_value ();

		  int count = 0;

		  Matrix size = (nargin == 3)
		    ? args(2).matrix_value () : Matrix (1, 1, octave_Inf);

		  if (! error_state)
		    {
		      octave_value tmp = os->scanf (fmt, size, count);

		      retval(1) = (double) count;
		      retval(0) = tmp;
		    }
		}
	      else
		::error ("fscanf: format must be a string");
	    }
	  else
	    gripe_invalid_file_id ("fscanf");
	}
      else
	print_usage ("fscanf");
    }

  return retval;
}

DEFUN (sscanf, args, ,
  "[A, COUNT, ERRMSG, INDEX] = sscanf (STRING, FORMAT, SIZE)\n\
\n\
Read from STRING according to FORMAT, returning the result in the\n\
matrix A.  SIZE is optional.  If present, it can be one of\n\
\n\
       Inf : read as much as possible, returning a column vector\n\
             (unless doing all character conversions, in which case a\n\
             string is returned)\n\
        NR : read as much as possible, returning a matrix with NR rows\n\
  [NR, NC] : read up to NR x NC elements, returning a matrix with NR rows\n\
 [NR, Inf] : same as NR\n\
\n\
If it is omitted, a value of Inf is assumed.\n\
\n\
The number of items successfully read is returned in COUNT.  If an\n\
error occurs, ERRMSG contains the text of the corresponding error\n\
message.  INDEX contains the index of the next character to be read\n\
from STRING\n\
\n\
[A, B, ...] = sscanf (STRING, FORMAT, \"C\")\n\
\n\
Read from STRING according to FORMAT, with each conversion specifier\n\
in FORMAT corresponding to a single scalar return value.  This form is\n\
more `C-like', and also compatible with previous versions of Octave")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 3 && args(2).is_string ())
    {
      if (args(0).is_string ())
	{
	  string data = args(0).string_value ();

	  octave_istrstream istr (data);

	  octave_stream os (&istr, true);

	  if (os)
	    {
	      if (args(1).is_string ())
		{
		  string fmt = args(1).string_value ();

		  retval = os.oscanf (fmt);
		}
	      else
		::error ("sscanf: format must be a string");
	    }
	  else
	    ::error ("sscanf: unable to create temporary input buffer");
	}
      else
	::error ("sscanf: first argument must be a string");
    }
  else
    {
      if (nargin == 2 || nargin == 3)
	{
	  retval(3) = -1.0;
	  retval(2) = "unknown error";
	  retval(1) = 0.0;
	  retval(0) = Matrix ();

	  if (args(0).is_string ())
	    {
	      string data = args(0).string_value ();

	      octave_istrstream istr (data);

	      octave_stream os (&istr, true);

	      if (os)
		{
		  if (args(1).is_string ())
		    {
		      string fmt = args(1).string_value ();

		      int count = 0;

		      Matrix size = (nargin == 3)
			? args(2).matrix_value () : Matrix (1, 1, octave_Inf);

		      octave_value tmp = os.scanf (fmt, size, count);

		      // XXX FIXME XXX -- is this the right thing to do?
		      // Extract error message first, because getting
		      // position will clear it.
		      string errmsg = os.error ();

		      retval(3) = (double) (os.tell () + 1);
		      retval(2) = errmsg;
		      retval(1) = (double) count;
		      retval(0) = tmp;
		    }
		  else
		    ::error ("sscanf: format must be a string");
		}
	      else
		::error ("sscanf: unable to create temporary input buffer");
	    }
	  else
	    ::error ("sscanf: first argument must be a string");
	}
      else
	print_usage ("sscanf");
    }

  return retval;
}

DEFUN (scanf, args, nargout,
  "scanf (FORMAT) is equivalent to fscanf (stdin, FORMAT)")
{
  int nargin = args.length ();

  octave_value_list tmp_args (nargin+1, octave_value ());

  tmp_args (0) = 0.0;
  for (int i = 0; i < nargin; i++)
    tmp_args (i+1) = args (i);

  return Ffscanf (tmp_args, nargout);
}

static octave_value
do_fread (octave_stream& os, const octave_value& size_arg,
	  const octave_value& prec_arg, const octave_value& skip_arg,
	  const octave_value& arch_arg, int& count)
{
  octave_value retval;

  count = -1;

  Matrix size = size_arg.matrix_value ();

  if (! error_state)
    {
      string prec = prec_arg.string_value ();

      if (! error_state)
	{
	  oct_data_conv::data_type dt
	    = oct_data_conv::string_to_data_type (prec);

	  if (! error_state)
	    {
	      double dskip = skip_arg.double_value ();

	      if (! error_state)
		{
		  if (D_NINT (dskip) == dskip)
		    {
		      int skip = NINT (dskip);

		      string arch = arch_arg.string_value ();

		      if (! error_state)
			{
			  oct_mach_info::float_format flt_fmt
			    = oct_mach_info::string_to_float_format (arch);

			  if (! error_state)
			    retval = os.read (size, dt, skip, flt_fmt, count);
			}
		      else
			::error ("fread: architecture type must be a string");
		    }
		  else
		    ::error ("fread: skip must be an integer");
		}
	      else
		::error ("fread: invalid skip specified");
	    }
	  else
	    ::error ("fread: invalid data type specified");
	}
      else
	::error ("fread: precision must be a string");
    }
  else
    ::error ("fread: invalid size specified");

  return retval;
}

DEFUN (fread, args, ,
  "[DATA, COUNT] = fread (FILENUM [, SIZE] [, PRECISION] [, SKIP] [, ARCH])\n\
\n\
Reads data in binary form of type PRECISION from a file.\n\
\n\
  FILENUM   : file number from fopen\n\
\n\
  SIZE      : size specification for the data matrix\n\
\n\
  PRECISION : string specifying type of data to read, valid types are\n\
\n\
   char, char*1, integer*1, int8  --  character\n\
   schar, signed char             --  signed character\n\
   uchar, unsigned char           --  unsigned character (default)\n\
   short                          --  short integer\n\
   ushort, unsigned short         --  unsigned short integer\n\
   int                            --  integer\n\
   uint, unsigned int             --  unsigned integer\n\
   long                           --  long integer\n\
   ulong, unsigned long           --  unsigned long integer\n\
   float, float32, real*4         --  single precision float\n\
   double, float64, real*8        --  double precision float\n\
   int16, integer*2               --  two byte integer\n\
   int32, integer*4               --  four byte integer\n\
\n\
  SKIP      : number of bytes to skip before each element is read\n\
              (default is 0)\n\
\n\
  ARCH      : string specifying the data format for the file.  Valid
              values are\n\
\n\
    native   --  the format of the current machine (default)\n\
    ieee-le  --  IEEE big endian\n\
    ieee-be  --  IEEE little endian\n\
    vaxd     --  VAX D floating format\n\
    vaxg     --  VAX G floating format\n\
    cray     --  Cray floating format\n\
\n\
              however, conversions are currently only supported for\n\
              ieee-be, and ieee-le formats.\n\
\n\
\n\
  DATA      : matrix in which the data is stored\n\
\n\
  COUNT     : number of elements read")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0 && nargin < 6)
    {
      retval(1) = -1.0;
      retval(0) = Matrix ();

      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	{
	  octave_value size = (nargin > 1)
	    ? args(1) : octave_value (octave_Inf);

	  octave_value prec = (nargin > 2)
	    ? args(2) : octave_value ("uchar");

	  octave_value skip = (nargin > 3)
	    ? args(3) : octave_value (0.0);

	  octave_value arch = (nargin > 4)
	    ? args(4) : octave_value ("unknown");

	  int count = -1;

	  octave_value tmp = do_fread (*os, size, prec, skip, arch, count);

	  retval(1) = (double) count;
	  retval(0) = tmp;
	}
      else
	gripe_invalid_file_id ("fread");
    }
  else
    print_usage ("fread");

  return retval;
}

static int
do_fwrite (octave_stream& os, const octave_value& data,
	   const octave_value& prec_arg, const octave_value& skip_arg,
	   const octave_value& arch_arg)
{
  int retval = -1;

  string prec = prec_arg.string_value ();

  if (! error_state)
    {
      oct_data_conv::data_type dt
	= oct_data_conv::string_to_data_type (prec);

      if (! error_state)
	{
	  double dskip = skip_arg.double_value ();

	  if (! error_state)
	    {
	      if (D_NINT (dskip) == dskip)
		{
		  int skip = NINT (dskip);

		  string arch = arch_arg.string_value ();

		  if (! error_state)
		    {
		      oct_mach_info::float_format flt_fmt
			= oct_mach_info::string_to_float_format (arch);

		      if (! error_state)
			retval = os.write (data, dt, skip, flt_fmt);
		    }
		  else
		    ::error ("fwrite: architecture type must be a string");
		}
	      else
		::error ("fwrite: skip must be an integer");
	    }
	  else
	    ::error ("fwrite: invalid skip specified");
	}
    }
  else
    ::error ("fwrite: precision must be a string");

  return retval;
}

DEFUN (fwrite, args, ,
  "COUNT = fwrite (FILENUM, DATA [, PRECISION] [, SKIP] [, ARCH])\n\
\n\
  Writes data to a file in binary form of size PRECISION\n\
\n\
  FILENUM   : file number from fopen\n\
\n\
  DATA      : matrix of elements to be written\n\
\n\
  PRECISION : string specifying type of data to read, valid types are\n\
\n\
   char, char*1, integer*1, int8  --  character\n\
   schar, signed char             --  signed character\n\
   uchar, unsigned char           --  unsigned character (default)\n\
   short                          --  short integer\n\
   ushort, unsigned short         --  unsigned short integer\n\
   int                            --  integer\n\
   uint, unsigned int             --  unsigned integer\n\
   long                           --  long integer\n\
   ulong, unsigned long           --  unsigned long integer\n\
   float, float32, real*4         --  single precision float\n\
   double, float64, real*8        --  double precision float\n\
   int16, integer*2               --  two byte integer\n\
   int32, integer*4               --  four byte integer\n\
\n\
  SKIP      : number of bytes to skip before each element is read\n\
              (the default is 0)\n\
\n\
  ARCH      : string specifying the data format for the file.  Valid
              values are\n\
\n\
    native   --  the format of the current machine (default)\n\
    ieee-le  --  IEEE big endian\n\
    ieee-be  --  IEEE little endian\n\
    vaxd     --  VAX D floating format\n\
    vaxg     --  VAX G floating format\n\
    cray     --  Cray floating format\n
\n\
  however, conversions are currently only supported for ieee-be, and\n\
  ieee-le formats.\n\
\n\
\n\
  COUNT     : number of elements written")
{
  octave_value retval = -1.0;

  int nargin = args.length ();

  if (nargin > 1 && nargin < 6)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	{
	  octave_value data = args(1);

	  octave_value prec = (nargin > 2)
	    ? args(2) : octave_value ("uchar");

	  octave_value skip = (nargin > 3)
	    ? args(3) : octave_value (0.0);

	  octave_value arch = (nargin > 4)
	    ? args(4) : octave_value ("unknown");

	  retval = do_fwrite (*os, data, prec, skip, arch);
	}
      else
	gripe_invalid_file_id ("fwrite");
    }
  else
    print_usage ("fwrite");

  return retval;
}

DEFUN (feof, args, ,
  "ERROR = feof (FILENUM)\n\
\n\
 Returns a non zero value for an end of file condition for the\n\
 file specified by FILENUM from fopen")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	retval = os->eof () ? 1.0 : 0.0;
      else
	gripe_invalid_file_id ("feof");
    }
  else
    print_usage ("feof");

  return retval;
}

DEFUN (ferror, args, ,
  "ERROR = ferror (FILENUM, [\"clear\"])\n\
\n\
 Returns a non zero value for an error condition on the\n\
 file specified by FILENUM from fopen")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_stream *os = octave_stream_list::lookup (args(0));

      if (os)
	{
	  bool clear = false;

	  if (nargin == 2)
	    {
	      string opt = args(1).string_value ();

	      if (! error_state)
		clear = (opt == "clear");
	      else
		return retval;
	    }

	  int error_number = 0;

	  string error_message = os->error (clear, error_number);

	  retval(1) = (double) error_number;
	  retval(0) = error_message;
	}
      else
	gripe_invalid_file_id ("ferror");
    }
  else
    print_usage ("ferror");

  return retval;
}

DEFUN (popen, args, ,
  "FILENUM = popen (FILENAME, MODE)\n\
\n\
  start a process and create a pipe.  Valid values for mode are:\n\
\n\
  \"r\" : connect stdout of process to pipe\n\
  \"w\" : connect stdin of process to pipe")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 2)
    {
      string name = args(0).string_value ();

      if (! error_state)
	{
	  string mode = args(1).string_value ();

	  if (! error_state)
	    {
	      if (mode == "r")
		{
		  octave_iprocstream *ips = new octave_iprocstream (name);

		  retval = octave_stream_list::insert (ips);
		}
	      else if (mode == "w")
		{
		  octave_oprocstream *ops = new octave_oprocstream (name);

		  retval = octave_stream_list::insert (ops);
		}
	      else
		::error ("popen: invalid mode specified");
	    }
	  else
	    ::error ("popen: mode must be a string");
	}
      else
	::error ("popen: name must be a string");
    }
  else
    print_usage ("popen");

  return retval;
}

DEFUN (pclose, args, ,
  "pclose (FILENUM)\n\
\n\
  Close a pipe and terminate the associated process")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      retval = (double) octave_stream_list::remove (args(0));

      if (retval < 0)
	gripe_invalid_file_id ("pclose");
    }
  else
    print_usage ("pclose");

  return retval;
}

DEFUN (tmpnam, args, ,
 "tmpnam ()\n\
Return unique temporary file name.")
{
  octave_value retval;

  if (args.length () == 0)
    retval = oct_tempnam ();
  else
    print_usage ("tmpnam");

  return retval;
}

DEFALIAS (octave_tmp_file_name, tmpnam);

static int
convert (int x, int ibase, int obase)
{
  int retval = 0;

  int tmp = x % obase;

  if (tmp > ibase - 1)
    ::error ("umask: invalid digit");
  else
    {
      retval = tmp;
      int mult = ibase;
      while ((x = (x - tmp) / obase))
	{
	  tmp = x % obase;
	  if (tmp > ibase - 1)
	    {
	      ::error ("umask: invalid digit");
	      break;
	    }
	  retval += mult * tmp;
	  mult *= ibase;
	}
    }

  return retval;
}

DEFUN (umask, args, ,
  "umask (MASK)\n\
\n\
Change the file permission mask for file creation for the current
process.  MASK is an integer, interpreted as an octal number.  If
successful, returns the previous value of the mask (as an integer to
be interpreted as an octal number); otherwise an error message is
printed.")
{
  octave_value_list retval;

  int status = 0;

  if (args.length () == 1)
    {
      double dmask = args(0).double_value ();

      if (error_state)
	{
	  status = -1;
	  ::error ("umask: expecting integer argument");
	}
      else
	{
	  int mask = NINT (dmask);

	  if ((double) mask != dmask || mask < 0)
	    {
	      status = -1;
	      ::error ("umask: MASK must be a positive integer value");
	    }
	  else
	    {
	      int oct_mask = convert (mask, 8, 10);

	      if (! error_state)
		status = convert (oct_umask (oct_mask), 10, 8);
	    }
	}
    }
  else
    print_usage ("umask");

  if (status >= 0)
    retval(0) = (double) status;

  return retval;
}

void
symbols_of_file_io (void)
{
  // NOTE: the values of SEEK_SET, SEEK_CUR, and SEEK_END have to be
  // this way for Matlab compatibility.

  DEFCONST (SEEK_SET, -1.0, 0, 0,
    "used with fseek to position file relative to the beginning");

  DEFCONST (SEEK_CUR, 0.0, 0, 0,
    "used with fseek to position file relative to the current position");

  DEFCONST (SEEK_END, 1.0, 0, 0,
    "used with fseek to position file relative to the end");

  DEFCONSTX ("stdin", SBV_stdin, 0.0, 0, 0,
    "file number of the standard input stream");

  DEFCONSTX ("stdout", SBV_stdout, 1.0, 0, 0,
    "file number of the standard output stream");

  DEFCONSTX ("stderr", SBV_stderr, 2.0, 0, 0,
    "file number of the standard error stream");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
