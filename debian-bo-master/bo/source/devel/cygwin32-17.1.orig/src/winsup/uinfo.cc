/* user info (uid,gid,etc.)

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <pwd.h>
#include <unistd.h>
#include "winsup.h"

#define MAX_USER_NAME 20

void
uinfo_init ()
{
  struct passwd *p;

  if ((p = getpwnam (getlogin ())) != NULL)
    {
      u->self->uid = p->pw_uid;
      u->self->gid = p->pw_gid;
    }
  else
    {
      u->self->uid = DEFAULT_UID;
      u->self->gid = DEFAULT_GID;
    }
}

char *
getlogin (void)
{
  static char username[MAX_USER_NAME];
  DWORD username_len = MAX_USER_NAME;

  if (GetUserName (username, &username_len))
    return username;
  else
    return "unknown";
}

uid_t
getuid (void)
{
  return u->self->uid;
}

gid_t 
getgid (void)
{
  return u->self->gid;
}

uid_t 
geteuid (void)
{
  return getuid ();
}

gid_t 
getegid (void)
{
  return getgid ();
}

/* Not quite right - cuserid can change, getlogin can't */
char *
cuserid (char *src)
{
  if (src) 
    {
      strcpy (src, getlogin ());
      return src;
    }
  else
    {
      return getlogin ();
    }
}
