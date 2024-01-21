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

/*

The functions listed below were adapted from a similar functions
from GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991
Free Software Foundation, Inc.

  polite_directory_format  absolute_pathname
  base_pathname
  make_absolute            pathname_backup
  change_to_directory      get_working_directory

*/ 

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <cstdio>
#include <cstddef>
#include <cstdlib>
#include <cstring>

#include <string>

#include <strstream.h>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "oct-glob.h"
#include "str-vec.h"

#include "defun.h"
#include "dir-ops.h"
#include "dirfns.h"
#include "error.h"
#include "file-ops.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "pager.h"
#include "pathlen.h"
#include "procstream.h"
#include "pt-plot.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// The current working directory.
string Vcurrent_directory;

// Non-zero means follow symbolic links that point to directories just
// as if they are real directories.
static int follow_symbolic_links = 1;

// Non-zero means that pwd always give verbatim directory, regardless
// of symbolic link following.
static int verbatim_pwd = 1;

// Remove the last N directories from PATH.  Do not PATH blank.
// PATH must contain enough space for MAXPATHLEN characters.

static void
pathname_backup (string& path, int n)
{
  if (path.empty ())
    return;

  size_t i = path.length () - 1;

  while (n--)
    {
      while (path[i] == '/' && i > 0)
	i--;

      while (path[i] != '/' && i > 0)
	i--;

      i++;
    }

  path.resize (i);
}

// Return a pretty pathname.  If the first part of the pathname is the
// same as $HOME, then replace that with `~'.

string
polite_directory_format (const string& name)
{
  string retval;

  size_t len = Vhome_directory.length ();

  if (len > 1 && Vhome_directory.compare (name, 0, len) == 0
      && (name.length () == len || name[len] == '/'))
    {
      retval = "~";
      retval.append (name.substr (len));
    }
  else
    retval = name;

  return retval;
}

// Return 1 if STRING contains an absolute pathname, else 0.

static int
absolute_pathname (const string& s)
{
  if (s.empty ())
    return 0;

  if (s[0] == '/')
    return 1;

  if (s[0] == '.')
    {
      if (s[1] == '\0' || s[1] == '/')
	return 1;

      if (s[1] == '.')
	if (s[2] == '\0' || s[2] == '/')
	  return 1;
    }

  return 0;
}

// Return the `basename' of the pathname in STRING (the stuff after
// the last '/').  If STRING is not a full pathname, simply return it.

string
base_pathname (const string& s)
{
  if (! absolute_pathname (s))
    return s;

  size_t pos = s.rfind ('/');

  if (pos == NPOS)
    return s;
  else
    return s.substr (pos+1);
}

// Turn STRING (a pathname) into an absolute pathname, assuming that
// DOT_PATH contains the symbolic location of '.'.

string
make_absolute (const string& s, const string& dot_path)
{
#if defined (__EMX__)
  if (s.length () > 1 && s[1] == ':')
    return s;
#endif

  if (dot_path.empty () || s[0] == '/' || s.empty ())
    return s;

  string current_path = dot_path;

  if (current_path.empty ())
    current_path = "./";

  size_t pos = current_path.length () - 1;

  if (current_path[pos] != '/')
    current_path.append ("/");

  size_t i = 0;
  size_t slen = s.length ();

  while (i < slen)
    {
      if (s[i] == '.')
	{
	  if (i + 1 == slen)
	    return current_path;

	  if (s[i+1] == '/')
	    {
	      i += 2;
	      continue;
	    }

	  if (s[i+1] == '.' && (i + 2 == slen || s[i+2] == '/'))
	    {
	      i += 2;

	      if (i != slen)
		i++;

	      pathname_backup (current_path, 1);

	      continue;
	    }
	}

      size_t tmp = s.find ('/', i);

      if (tmp == NPOS)
	{
	  current_path.append (s, i, tmp-i);
	  break;
	}
      else
	{
	  current_path.append (s, i, tmp-i+1);
	  i = tmp + 1;
	}
    }

  return current_path;
}

// Return a consed string which is the current working directory.
// FOR_WHOM is the name of the caller for error printing.

string
get_working_directory (const string& for_whom)
{
  if (! follow_symbolic_links)
    Vcurrent_directory = "";

  if (Vcurrent_directory.empty ())
    {
      Vcurrent_directory = octave_getcwd ();

      if (Vcurrent_directory.empty ())
	warning ("%s: can't find current directory!", for_whom.c_str ());
    }

  return Vcurrent_directory;
}

// Do the work of changing to the directory NEWDIR.  Handle symbolic
// link following, etc.

static int
change_to_directory (const string& newdir)
{
  string tmp;

  if (follow_symbolic_links)
    {
      if (Vcurrent_directory.empty ())
	get_working_directory ("cd_links");

      if (Vcurrent_directory.empty ())
	tmp = newdir;
      else
	tmp = make_absolute (newdir, Vcurrent_directory);

      // Get rid of trailing `/'.

      size_t len = tmp.length ();

      if (len > 1)
	{
	  if (tmp[--len] == '/')
	    tmp.resize (len);
	}

      if (octave_chdir (tmp) < 0)
	return 0;
      else
	{
	  Vcurrent_directory = tmp;
	  return 1;
	}
    }
  else
    return (octave_chdir (newdir) < 0) ? 0 : 1;
}

static int
octave_change_to_directory (const string& newdir)
{
  int cd_ok = change_to_directory (newdir);

  if (cd_ok)
    do_external_plotter_cd (newdir);
  else
    error ("%s: %s", newdir.c_str (), strerror (errno));

  return cd_ok;
}

DEFUN_TEXT (cd, args, ,
  "cd [dir]\n\
\n\
change current working directory\n\
if no arguments are given, the current directory is changed to the\n\
users home directory")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("cd");

  if (error_state)
    return retval;

  if (argc > 1)
    {
      string dirname = oct_tilde_expand (argv[1]);

      if (dirname.length () > 0
	  && ! octave_change_to_directory (dirname))
	{
	  return retval;
	}
    }
  else
    {
      if (Vhome_directory.empty ()
	  || ! octave_change_to_directory (Vhome_directory))
	{
	  return retval;
	}
    }

  string directory = get_working_directory ("cd");
  bind_builtin_variable ("PWD", directory, 1);

  return retval;
}

DEFALIAS (chdir, cd);

// Get a directory listing.

static void
cleanup_iprocstream (void *p)
{
  delete (iprocstream *) p;
}

DEFUN_TEXT (ls, args, ,
  "ls [options]\n\
\n\
print a directory listing")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("ls");

  if (error_state)
    return retval;

  ostrstream ls_buf;

  ls_buf << "ls -C ";
  for (int i = 1; i < argc; i++)
    ls_buf << oct_tilde_expand (argv[i]) << " ";

  ls_buf << ends;
  char *ls_command = ls_buf.str ();

  iprocstream *cmd = new iprocstream (ls_command);

  delete [] ls_command;

  add_unwind_protect (cleanup_iprocstream, cmd);

  if (cmd && *cmd)
    {
      int ch;
      while ((ch = cmd->get ()) != EOF)
	octave_stdout << (char) ch;
    }
  else
    error ("couldn't start process for ls!");

  run_unwind_protect ();

  return retval;
}

DEFALIAS (dir, ls);

DEFUN (pwd, , nargout,
  "pwd (): print current working directory")
{
  octave_value_list retval;
  string directory;

  if (verbatim_pwd)
    {
      directory = octave_getcwd ();

      if (directory.empty ())
	warning ("pwd: can't find working directory!");
    }
  else
    directory = get_working_directory ("pwd");

  if (! directory.empty ())
    {
      if (nargout == 0)
	octave_stdout << directory << "\n";
      else
	retval = directory;
    }

  return retval;
}

DEFUN (readdir, args, ,
  "[FILES, STATUS, MSG] = readdir (NAME)\n\
\n\
Return an array of strings containing the list of all files in the
named directory in FILES, or an empty matrix if an error occurs\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(2) = string ();
  retval(1) = -1.0;
  retval(0) = Matrix ();

  if (args.length () == 1)
    {
      string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("readdir", args(0));
      else
	{
	  dir_entry dir (oct_tilde_expand (dirname));

	  if (dir)
	    {
	      string_vector dirlist = dir.read ();
	      retval(0) = dirlist.qsort ();
	      retval(1) = 0.0;
	    }
	  else
	    {
	      retval(2) = dir.error ();
	    }
	}
    }
  else
    print_usage ("readdir");

  return retval;
}

// XXX FIXME XXX -- should probably also allow second arg to specify
// mode.

DEFUN (mkdir, args, ,
  "[STATUS, MSG] = mkdir (NAME)\n\
\n\
Create the directory named by NAME.\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  if (args.length () == 1)
    {
      string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("mkdir", args(0));
      else
	{
	  string msg;

	  int status = oct_mkdir (oct_tilde_expand (dirname),
				  0777, msg);

	  retval(0) = (double) status;

	  if (status < 0)
	    retval(1) = msg;
	}
    }
  else
    print_usage ("mkdir");

  return retval;
}

DEFUN (rmdir, args, ,
  "[STATUS, MSG] = rmdir (NAME)\n\
\n\
Remove the directory named by NAME.\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  if (args.length () == 1)
    {
      string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("rmdir", args(0));
      else
	{
	  string msg;

	  int status = oct_rmdir (oct_tilde_expand (dirname), msg);

	  retval(0) = (double) status;

	  if (status < 0)
	    retval(1) = msg;
	}
    }
  else
    print_usage ("rmdir");

  return retval;
}

DEFUN (rename, args, ,
  "[STATUS, MSG] = rename (FROM, TO)\n\
\n\
Rename a file.\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  if (args.length () == 2)
    {
      string from = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("rename", args(0));
      else
	{
	  string to = args(1).string_value ();

	  if (error_state)
	    gripe_wrong_type_arg ("rename", args(1));
	  else
	    {
	      string msg;

	      int status = oct_rename (from, to, msg);

	      retval(0) = (double) status;

	      if (status < 0)
		retval(1) = msg;
	    }
	}
    }
  else
    print_usage ("rename");

  return retval;
}

DEFUN (glob, args, ,
  "glob (PATTERN)\n\
\n\
Given an array of strings in PATTERN, return the list of file names\n\
that any of them, or an empty string if no patterns match.  Tilde\n\
expansion is performed on each of the patterns before looking for\n\
matching file names.")
{
  octave_value retval;

  if (args.length () == 1)
    {
      string_vector pat = args(0).all_strings ();

      if (error_state)
	gripe_wrong_type_arg ("glob", args(0));
      else
	{
	  glob_match pattern (oct_tilde_expand (pat));

	  string_vector list = pattern.glob ();

	  if (list.empty ())
	    retval = "";
	  else
	    retval = list;
	}
    }
  else
    print_usage ("glob");

  return retval;
}

DEFUN (fnmatch, args, ,
  "fnmatch (PATTERN, STRING)\n\
\n\
Return 1 or zero for each element of STRING that matches any of the\n\
elements of the string array PATTERN, using the rules of filename\n\
pattern matching.")
{
  octave_value retval;

  if (args.length () == 2)
    {
      string_vector pat = args(0).all_strings ();
      string_vector str = args(1).all_strings ();

      if (error_state)
	gripe_wrong_type_arg ("fnmatch", args(0));
      else
	{
	  glob_match pattern (oct_tilde_expand (pat));

	  Array<bool> tmp = pattern.match (str);

	  int n = tmp.length ();

	  ColumnVector result (n);

	  for (int i = 0; i < n; i++)
	    result(i) = tmp(i);

	  retval = octave_value (result, true);
	}
    }
  else
    print_usage ("fnmatch");

  return retval;
}

static int
pwd (void)
{
  int status = 0;

  string s = builtin_string_variable ("PWD");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("PWD");
      status = -1;
    }
  else
    Vcurrent_directory = s;

  return status;
}

void
symbols_of_dirfns (void)
{
  DEFCONST (PWD, get_working_directory ("initialize_globals"), 0, pwd,
    "current working directory");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
