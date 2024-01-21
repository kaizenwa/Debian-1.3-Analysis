/* umount for win32.

   THIS SOFTWARE IS NOT COPYRIGHTED

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <stdio.h>
#include <sys/mount.h>
#include <mntent.h>

int
main(int argc, char **argv)
{
  if (argc != 2)
    {
      fprintf (stderr, "Usage: %s path\n", argv[0]);
      return 1;
    }

  if (umount (argv[1]) != 0)
    {
      perror ("umount");
      return 1;
    }
    
  return 0;
}
