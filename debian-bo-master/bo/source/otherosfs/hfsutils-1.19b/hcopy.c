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
# include <stdlib.h>
# include <string.h>
# include <sys/stat.h>
# include <unistd.h>
# include <fcntl.h>
# include <errno.h>

# include "hfs.h"
# include "hcwd.h"
# include "hfsutil.h"
# include "glob.h"
# include "hcopy.h"
# include "copyin.h"
# include "copyout.h"

extern int optind;

typedef int (*cpifunc)(char *, hfsvol *, char *);
typedef int (*cpofunc)(hfsvol *, char *, char *);

/*
 * NAME:	ufs->automode()
 * DESCRIPTION:	automatically choose copyin transfer mode
 */
static
cpifunc ufs_automode(char *path)
{
  int i;
  struct {
    char *ext;
    cpifunc func;
  } exts[] = {
    { ".bin", cpi_macb },
    { ".hqx", cpi_binh },

    { ".txt", cpi_text },
    { ".c",   cpi_text },
    { ".h",   cpi_text },

    { ".sit", cpi_raw  },
    { ".sea", cpi_raw  },
    { ".cpt", cpi_raw  },
    { ".tar", cpi_raw  },
    { ".gz",  cpi_raw  },
    { ".Z",   cpi_raw  },
    { ".gif", cpi_raw  },
    { ".jpg", cpi_raw  },
    { 0,      0        }
  };

  path += strlen(path);

  for (i = 0; exts[i].ext; ++i)
    {
      if (strcasecmp(path - strlen(exts[i].ext), exts[i].ext) == 0)
	return exts[i].func;
    }

  return cpi_raw;
}

/*
 * NAME:	do_copyin()
 * DESCRIPTION:	copy files from UNIX to HFS
 */
static
int do_copyin(hfsvol *vol, int argc, char *argv[], char *dest, int mode)
{
  hfsdirent ent;
  cpifunc copyfile;
  int i, result = 0;

  if (argc > 1 && (hfs_stat(vol, dest, &ent) < 0 ||
		   ! (ent.flags & HFS_ISDIR)))
    {
      hfs_error = 0;
      errno = ENOTDIR;
      hfs_perrorp(dest);

      return 1;
    }

  switch (mode)
    {
    case 'm':
      copyfile = cpi_macb;
      break;

    case 'b':
      copyfile = cpi_binh;
      break;

    case 't':
      copyfile = cpi_text;
      break;

    case 'r':
      copyfile = cpi_raw;
      break;
    }

  for (i = 0; i < argc; ++i)
    {
      if (mode == 'a')
	copyfile = ufs_automode(argv[i]);

      if (copyfile(argv[i], vol, dest) < 0)
	{
	  hfs_error = cpi_error;
	  hfs_perrorp(argv[i]);

	  result = 1;
	}
    }

  return result;
}

/*
 * NAME:	hfs->automode()
 * DESCRIPTION:	automatically choose copyout transfer mode
 */
static
cpofunc hfs_automode(hfsvol *vol, char *path)
{
  hfsdirent ent;

  if (hfs_stat(vol, path, &ent) < 0)
    return cpo_macb;

  if (strcmp(ent.type, "TEXT") == 0 ||
      strcmp(ent.type, "ttro") == 0)
    return cpo_text;
  else if (ent.rsize == 0)
    return cpo_raw;

  return cpo_macb;
}

/*
 * NAME:	do_copyout()
 * DESCRIPTION:	copy files from HFS to UNIX
 */
static
int do_copyout(hfsvol *vol, int argc, char *argv[], char *dest, int mode)
{
  struct stat sbuf;
  cpofunc copyfile;
  int i, result = 0;

  if (argc > 1 && (stat(dest, &sbuf) < 0 ||
		   ! S_ISDIR(sbuf.st_mode)))
    {
      hfs_error = 0;
      errno = ENOTDIR;
      hfs_perrorp(dest);

      return 1;
    }

  switch (mode)
    {
    case 'm':
      copyfile = cpo_macb;
      break;

    case 'b':
      copyfile = cpo_binh;
      break;

    case 't':
      copyfile = cpo_text;
      break;

    case 'r':
      copyfile = cpo_raw;
      break;
    }

  for (i = 0; i < argc; ++i)
    {
      if (mode == 'a')
	copyfile = hfs_automode(vol, argv[i]);

      if (copyfile(vol, argv[i], dest) < 0)
	{
	  hfs_error = cpo_error;
	  hfs_perrorp(argv[i]);

	  result = 1;
	}
    }

  return result;
}

/*
 * NAME:	usage()
 * DESCRIPTION:	display usage message
 */
static
int usage(void)
{
  fprintf(stderr, "Usage: %s [-m|-b|-t|-r|-a] source-path [...] target-path\n",
	  argv0);

  return 1;
}

/*
 * NAME:	hcopy->main()
 * DESCRIPTION:	implement hcopy command
 */
int hcopy_main(int argc, char *argv[])
{
  int nargs, mode = 'a', result;
  char *target;
  int fargc;
  char **fargv;
  hfsvol *vol;
  int (*copy)(hfsvol *, int, char *[], char *, int);

  while (1)
    {
      int opt;

      opt = getopt(argc, argv, "mbtra");
      if (opt == EOF)
	break;

      switch (opt)
	{
	case '?':
	  return usage();

	default:
	  mode = opt;
	}
    }

  nargs = argc - optind;

  if (nargs < 2)
    return usage();

  target = argv[argc - 1];

  if (strchr(target, ':') && target[0] != '.' && target[0] != '/')
    {
      vol = hfs_remount(hcwd_getvol(-1), O_RDWR);
      if (vol == 0)
	return 1;

      copy  = do_copyin;
      fargc = nargs - 1;
      fargv = &argv[optind];
    }
  else
    {
      vol = hfs_remount(hcwd_getvol(-1), O_RDONLY);
      if (vol == 0)
	return 1;

      copy  = do_copyout;
      fargv = hfs_glob(vol, nargs - 1, &argv[optind], &fargc);
    }

  if (fargv == 0)
    {
      fprintf(stderr, "%s: globbing error\n", argv0);
      result = 1;
    }
  else
    result = copy(vol, fargc, fargv, target, mode);

  if (hfs_umount(vol) < 0 && result == 0)
    {
      hfs_perror("Error closing HFS volume");
      result = 1;
    }

  if (fargv && fargv != &argv[optind])
    free(fargv);

  return result;
}
