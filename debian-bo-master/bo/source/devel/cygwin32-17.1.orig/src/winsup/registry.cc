/* registry interface for winsup.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <windows.h>
#include <winbase.h>
#include "winsup.h"
#include "registry.h"
#include "version.h"

reg_session::reg_session ()
{
  majorkey_.init ("SOFTWARE", HKEY_CURRENT_USER);
  minorkey_.init ("Cygnus Support", majorkey_.get_key ());
  versionkey_.init ("CYGWIN.DLL setup", minorkey_.get_key ());
  vminorkey_.init (CYGWIN_REGISTRY_VERSION, versionkey_.get_key ());
}

reg_key &
reg_session::get_key ()
{
  return vminorkey_;
}

reg_session::~reg_session ()
{
}

void
reg_key::init (const char *name, HKEY r)
{
  DWORD disp;
  int res = RegCreateKeyExA (r,
			     name,
			     0,
			     "cygnus",
			     REG_OPTION_NON_VOLATILE,
			     KEY_ALL_ACCESS,
			     0,
			     &key_,
			     &disp);

  if (res != ERROR_SUCCESS) 
    {
      key_ = (HKEY)INVALID_HANDLE_VALUE;
      __sys_printf ("fatal error in the registry\n");
      exit (1);
    }
}

int
reg_key::open(const char *name, HKEY r, REGSAM access)
{
  int res = (int)RegOpenKeyExA(r,
			  name,
			  0,
			  access,
			  &key_);
  if(res != ERROR_SUCCESS)
    {
      key_ = (HKEY)INVALID_HANDLE_VALUE;
    }
  return res;
}

int
reg_key::get_int (const char *name, int def)
{
  DWORD type;
  DWORD dst;
  DWORD size = sizeof (dst);

  LONG res = RegQueryValueExA (key_, 
			       name,
			       0, 
			       &type,
			       (unsigned char *)&dst, &size);

  if (type != REG_DWORD || res != ERROR_SUCCESS)
    {
      return def;
    }
  return dst;
}

int
reg_key::set_int (const char *name, int val)
{
  DWORD value = val;
  return (int)RegSetValueExA (key_, 
		  name,
		  0, REG_DWORD,(unsigned char *)  &value,  sizeof (value));
}

int
reg_key::get_string (const char *name, char *dst, size_t max, const char * def)
{
  DWORD size = max;
  DWORD type;
  LONG res = RegQueryValueExA (key_, name, 0, &type, dst, &size);

  if ((def != 0) && ((type != REG_SZ) || (res != ERROR_SUCCESS)))
    {
      strcpy (dst, def);
    }
  return (int)res;
}

int
reg_key::set_string (const char *name, const char *src)
{
  return (int)RegSetValueExA (key_,name, 0, REG_SZ, (unsigned char*) src,
		  strlen (src)+1);
}

int
reg_key::setone_string (const char *src, const char *name)
{
  return (int)RegSetValueExA (key_, name, 0, REG_SZ, src, strlen (src)+1);
}

reg_key::reg_key () 
{
  key_ = (HKEY)INVALID_HANDLE_VALUE;
}

HKEY
reg_key::get_key ()
{
  return key_;
}

reg_key::reg_key (reg_key &parent, const char *name)
{
  init (name, parent.get_key ());
}

void
reg_key::kill (const char *name)
{
  RegDeleteKeyA(key_, name);
}

void
reg_key::close()
{
  if (key_ != (HKEY)INVALID_HANDLE_VALUE)
    RegCloseKey (key_);
  key_ = (HKEY)INVALID_HANDLE_VALUE;
}

reg_key::~reg_key ()
{
  close();
}
