/*
 * hfsutils - tools for reading and writing Macintosh HFS volumes
 * Copyright (C) 1996, 1997 Robert Leslie
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

# include <stdio.h>
# include <unistd.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <fcntl.h>
# include <errno.h>

# include "hfsck.h"

int options;

extern int optind;

/*
 * NAME:	usage()
 * DESCRIPTION:	display usage message
 */
static
int usage(char *argv[])
{
  fprintf(stderr, "Usage: %s [-v] [-n] [-a] device-path\n", argv[0]);

  return 1;
}

/*
 * NAME:	main()
 * DESCRIPTION:	program entry
 */
int main(int argc, char *argv[])
{
  char *path;
  int pnum, result;
  hfsvol vol;

  options = HFSCK_REPAIR;

  while (1)
    {
      int opt;

      opt = getopt(argc, argv, "vna");
      if (opt == EOF)
	break;

      switch (opt)
	{
	case '?':
	  return usage(argv);

	case 'v':
	  options |= HFSCK_VERBOSE;
	  break;

	case 'n':
	  options &= ~HFSCK_REPAIR;
	  break;

	case 'a':
	  options |= HFSCK_YES;
	  break;
	}
    }

  if (argc - optind != 1)
    return usage(argv);

  path = argv[optind];
  pnum = 1;

  vol.flags  = 0;
  vol.pnum   = pnum;
  vol.vstart = 0;
  vol.vlen   = 0;
  vol.lpa    = 0;
  vol.vbm    = 0;
  vol.cwd    = HFS_CNID_ROOTDIR;

  vol.refs   = 0;
  vol.files  = 0;
  vol.dirs   = 0;
  vol.prev   = 0;
  vol.next   = 0;

  vol.ext.map     = 0;
  vol.ext.mapsz   = 0;
  vol.ext.flags   = 0;
  vol.ext.compare = r_compareextkeys;

  vol.cat.map     = 0;
  vol.cat.mapsz   = 0;
  vol.cat.flags   = 0;
  vol.cat.compare = r_comparecatkeys;

  vol.fd = open(path, O_RDWR);
  if (vol.fd < 0 &&
      (errno == EROFS || errno == EACCES))
    {
      options &= ~HFSCK_REPAIR;

      vol.flags |= HFS_READONLY;
      vol.fd = open(path, O_RDONLY);
    }

  if (vol.fd < 0)
    {
      perror(path);
      return 1;
    }

  if (l_lockvol(&vol) < 0 ||
      l_readblock0(&vol) < 0 ||
      l_readmdb(&vol) < 0)
    {
      perror(path);
      close(vol.fd);
      return 1;
    }

  result = hfsck(&vol);

  if (result == 0 && v_flush(&vol, 1) < 0)
    {
      perror("flushing volume");
      close(vol.fd);
      return 1;
    }

  if (close(vol.fd) < 0)
    {
      perror("closing volume");
      return 1;
    }

  return result;
}
