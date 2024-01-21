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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-map.h"
#include "ov.h"
#include "oct-obj.h"
#include "utils.h"

// Password file functions.  (Why not?)

static octave_value
mk_pw_map (struct passwd *pw)
{
  octave_value retval;

  if (pw)
    {
      Octave_map m;

      m ["name"] = pw->pw_name;
      m ["passwd"] = pw->pw_passwd;
      m ["uid"] = STATIC_CAST (double, pw->pw_uid);
      m ["gid"] = STATIC_CAST (double, pw->pw_gid);
      m ["gecos"] = pw->pw_gecos;
      m ["dir"] = pw->pw_dir;
      m ["shell"] = pw->pw_shell;

      retval = m;
    }
  else
    retval = 0.0;

  return retval;
}

DEFUN_DLD (getpwent, args, ,
 "getpwent ()\n\
\n\
Read an entry from the password-file stream, opening it if necessary.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
#ifdef HAVE_GETPWENT
      retval = mk_pw_map (getpwent ());
#else
      gripe_not_supported ("getpwent");
#endif
    }
  else
    print_usage ("getpwent");

  return retval;
}

DEFUN_DLD (getpwuid, args, ,
  "getpwuid (UID)\n\
\n\
Search for a password entry with a matching user ID.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
#ifdef HAVE_GETPWUID
      double dval = args(0).double_value ();

      if (! error_state)
	{
	  if (D_NINT (dval) == dval)
	    {
	      uid_t uid = STATIC_CAST (uid_t, dval);

	      retval = mk_pw_map (getpwuid (uid));
	    }
	  else
	    error ("getpwuid: argument must be an integer");
	}
#else
      gripe_not_supported ("getpwuid");
#endif
    }
  else
    print_usage ("getpwuid");

  return retval;
}

DEFUN_DLD (getpwnam, args, ,
  "getpwnam (NAME)\n\
\n\
Search for password entry with a matching username.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
#ifdef HAVE_GETPWNAM
      string s = args(0).string_value ();

      if (! error_state)
	retval = mk_pw_map (getpwnam (s.c_str ()));
#else
      gripe_not_supported ("getpwnam");
#endif
    }
  else
    print_usage ("getpwnam");

  return retval;
}

DEFUN_DLD (setpwent, args, ,
  "setpwent ()\n\
\n\
Rewind the password-file stream.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
#ifdef HAVE_SETPWENT
      setpwent ();
#else
      gripe_not_supported ("setpwent");
#endif
    }
  else
    print_usage ("setpwent");

  return retval;
}

DEFUN_DLD (endpwent, args, ,
  "endpwent ()\n\
\n\
Close the password-file stream.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
#ifdef HAVE_ENDPWENT
      endpwent ();
#else
      gripe_not_supported ("endpwent");
#endif
    }
  else
    print_usage ("endpwent");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
