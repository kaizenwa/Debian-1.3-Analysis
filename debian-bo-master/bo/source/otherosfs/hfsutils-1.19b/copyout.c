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

# include <unistd.h>
# include <sys/stat.h>
# include <fcntl.h>
# include <string.h>

# include "hfs.h"
# include "data.h"
# include "copyout.h"
# include "binhex.h"
# include "crc.h"

char *cpo_error = "no error";

# define MACB_BLOCKSZ	128

/* Copy Routines =========================================================== */

/*
 * NAME:	do_macb()
 * DESCRIPTION:	perform copy using MacBinary II translation
 */
static
int do_macb(hfsfile *ifile, int ofile)
{
  hfsdirent ent;
  unsigned char buf[HFS_BLOCKSZ * 4];
  long bytes;
  unsigned long total;

  if (hfs_fstat(ifile, &ent) < 0)
    {
      cpo_error = hfs_error;
      return -1;
    }

  cpo_error = "error writing data";

  memset(buf, 0, MACB_BLOCKSZ);

  buf[1] = strlen(ent.name);
  strcpy((char *) &buf[2], ent.name);

  memcpy(&buf[65], ent.type,    4);
  memcpy(&buf[69], ent.creator, 4);

  buf[73] = ent.fdflags >> 8;

  d_putl(&buf[83], ent.dsize);
  d_putl(&buf[87], ent.rsize);

  d_putl(&buf[91], d_tomtime(ent.crdate));
  d_putl(&buf[95], d_tomtime(ent.mddate));

  buf[101] = ent.fdflags & 0xff;
  buf[122] = buf[123] = 129;

  d_putw(&buf[124], crc_macb(buf, 124, 0x0000));

  if (write(ofile, buf, MACB_BLOCKSZ) < MACB_BLOCKSZ)
    return -1;

  /* data fork */

  total = 0;
  while (1)
    {
      bytes = hfs_read(ifile, buf, sizeof(buf));
      if (bytes < 0)
	{
	  cpo_error = hfs_error;
	  return -1;
	}
      else if (bytes == 0)
	break;

      if (write(ofile, buf, bytes) < bytes)
	return -1;

      total += bytes;
    }

  if (total != ent.dsize)
    {
      cpo_error = "data fork length changed";
      return -1;
    }

  bytes = total % MACB_BLOCKSZ;
  if (bytes)
    {
      memset(buf, 0, MACB_BLOCKSZ);
      if (write(ofile, buf, MACB_BLOCKSZ - bytes) < MACB_BLOCKSZ - bytes)
	return -1;
    }

  /* resource fork */

  if (hfs_fork(ifile, 1) < 0)
    {
      cpo_error = hfs_error;
      return -1;
    }

  total = 0;
  while (1)
    {
      bytes = hfs_read(ifile, buf, sizeof(buf));
      if (bytes < 0)
	{
	  cpo_error = hfs_error;
	  return -1;
	}
      else if (bytes == 0)
	break;

      if (write(ofile, buf, bytes) < bytes)
	return -1;

      total += bytes;
    }

  if (total != ent.rsize)
    {
      cpo_error = "resource fork length changed";
      return -1;
    }

  bytes = total % MACB_BLOCKSZ;
  if (bytes)
    {
      memset(buf, 0, MACB_BLOCKSZ);
      if (write(ofile, buf, MACB_BLOCKSZ - bytes) < MACB_BLOCKSZ - bytes)
	return -1;
    }

  return 0;
}

/*
 * NAME:	binhx()
 * DESCRIPTION:	auxiliary BinHex routine
 */
static
int binhx(hfsfile *ifile)
{
  hfsdirent ent;
  unsigned char byte, word[2], lword[4], buf[HFS_BLOCKSZ * 4];
  long bytes;
  unsigned long total;

  if (hfs_fstat(ifile, &ent) < 0)
    {
      cpo_error = hfs_error;
      return -1;
    }

  cpo_error = "error writing data";

  byte = strlen(ent.name);
  if (bh_insert(&byte, 1) < 0 ||
      bh_insert(ent.name, byte + 1) < 0)
    {
      cpo_error = bh_error;
      return -1;
    }

  if (bh_insert(ent.type, 4) < 0 ||
      bh_insert(ent.creator, 4) < 0)
    {
      cpo_error = bh_error;
      return -1;
    }

  d_putw(word, ent.fdflags);
  if (bh_insert(word, 2) < 0)
    {
      cpo_error = bh_error;
      return -1;
    }

  d_putl(lword, ent.dsize);
  if (bh_insert(lword, 4) < 0)
    {
      cpo_error = bh_error;
      return -1;
    }

  d_putl(lword, ent.rsize);
  if (bh_insert(lword, 4) < 0)
    {
      cpo_error = bh_error;
      return -1;
    }

  if (bh_insertcrc() < 0)
    {
      cpo_error = bh_error;
      return -1;
    }

  /* data fork */

  total = 0;
  while (1)
    {
      bytes = hfs_read(ifile, buf, sizeof(buf));
      if (bytes < 0)
	{
	  cpo_error = hfs_error;
	  return -1;
	}
      else if (bytes == 0)
	break;

      if (bh_insert(buf, bytes) < 0)
	{
	  cpo_error = bh_error;
	  return -1;
	}

      total += bytes;
    }

  if (total != ent.dsize)
    {
      cpo_error = "data fork length changed";
      return -1;
    }

  if (bh_insertcrc() < 0)
    {
      cpo_error = bh_error;
      return -1;
    }

  /* resource fork */

  if (hfs_fork(ifile, 1) < 0)
    {
      cpo_error = hfs_error;
      return -1;
    }

  total = 0;
  while (1)
    {
      bytes = hfs_read(ifile, buf, sizeof(buf));
      if (bytes < 0)
	{
	  cpo_error = hfs_error;
	  return -1;
	}
      else if (bytes == 0)
	break;

      if (bh_insert(buf, bytes) < 0)
	{
	  cpo_error = bh_error;
	  return -1;
	}

      total += bytes;
    }

  if (total != ent.rsize)
    {
      cpo_error = "resource fork length changed";
      return -1;
    }

  if (bh_insertcrc() < 0)
    {
      cpo_error = bh_error;
      return -1;
    }

  return 0;
}

/*
 * NAME:	do_binh()
 * DESCRIPTION:	perform copy using BinHex translation
 */
static
int do_binh(hfsfile *ifile, int ofile)
{
  int result;

  if (bh_start(ofile) < 0)
    {
      cpo_error = bh_error;
      return -1;
    }

  result = binhx(ifile);

  if (bh_end() < 0 && result == 0)
    {
      cpo_error = bh_error;
      result = -1;
    }

  return result;
}

/*
 * NAME:	do_text()
 * DESCRIPTION:	perform copy using text translation
 */
static
int do_text(hfsfile *ifile, int ofile)
{
  unsigned char buf[HFS_BLOCKSZ * 4], *ptr;
  long bytes;

  while (1)
    {
      bytes = hfs_read(ifile, buf, sizeof(buf));
      if (bytes < 0)
	{
	  cpo_error = hfs_error;
	  return -1;
	}
      else if (bytes == 0)
	break;

      for (ptr = buf; ptr < buf + bytes; ++ptr)
	{
	  if (*ptr == '\r')
	    *ptr = '\n';
	}

      if (write(ofile, buf, bytes) < bytes)
	{
	  cpo_error = "error writing data";
	  return -1;
	}
    }

  return 0;
}

/*
 * NAME:	do_raw()
 * DESCRIPTION:	perform copy using no translation
 */
static
int do_raw(hfsfile *ifile, int ofile)
{
  unsigned char buf[HFS_BLOCKSZ * 4];
  long bytes;

  while (1)
    {
      bytes = hfs_read(ifile, buf, sizeof(buf));
      if (bytes < 0)
	{
	  cpo_error = hfs_error;
	  return -1;
	}
      else if (bytes == 0)
	break;

      if (write(ofile, buf, bytes) < bytes)
	{
	  cpo_error = "error writing data";
	  return -1;
	}
    }

  return 0;
}

/* Utility Routines ======================================================== */

/*
 * NAME:	opensrc()
 * DESCRIPTION:	open the source file; set hint for destination filename
 */
static
hfsfile *opensrc(hfsvol *vol, char *srcname, char **dsthint, char *ext)
{
  hfsfile *file;
  hfsdirent ent;
  static char name[36], *ptr;

  file = hfs_open(vol, srcname);
  if (file == 0)
    {
      cpo_error = hfs_error;
      return 0;
    }

  if (hfs_fstat(file, &ent) < 0)
    {
      hfs_close(file);
      cpo_error = hfs_error;
      return 0;
    }

  strcpy(name, ent.name);

  for (ptr = name; *ptr; ++ptr)
    {
      switch (*ptr)
	{
	case '/':
	  *ptr = '-';
	  break;

	case ' ':
	  *ptr = '_';
	  break;
	}
    }

  if (ext)
    strcat(name, ext);

  *dsthint = name;

  return file;
}

/*
 * NAME:	opendst()
 * DESCRIPTION:	open the destination file
 */
static
int opendst(char *dstname, char *hint)
{
  struct stat sbuf;
  int fd, dirfd;

  if (stat(dstname, &sbuf) >= 0 &&
      S_ISDIR(sbuf.st_mode))
    {
      dirfd = open(".", O_RDONLY);
      if (dirfd < 0)
	{
	  cpo_error = "error getting current directory";
	  return -1;
	}

      if (chdir(dstname) < 0)
	{
	  cpo_error = "error setting current directory";
	  close(dirfd);
	  return -1;
	}

      dstname = hint;
    }

  if (strcmp(dstname, "-") == 0)
    fd = dup(STDOUT_FILENO);
  else
    fd = open(dstname, O_WRONLY | O_CREAT | O_TRUNC, 0666);

  if (fd < 0)
    {
      cpo_error = "error opening destination file";

      if (dstname == hint)
	{
	  fchdir(dirfd);
	  close(dirfd);
	}

      return -1;
    }

  if (dstname == hint)
    {
      fchdir(dirfd);
      close(dirfd);
    }

  return fd;
}

/*
 * NAME:	openfiles()
 * DESCRIPTION:	open source and destination files
 */
static
int openfiles(hfsvol *vol, char *srcname, char *dstname, char *ext,
	      hfsfile **ifile, int *ofile)
{
  char *dsthint;

  *ifile = opensrc(vol, srcname, &dsthint, ext);
  if (*ifile == 0)
    return -1;

  *ofile = opendst(dstname, dsthint);
  if (*ofile < 0)
    {
      hfs_close(*ifile);
      return -1;
    }

  return 0;
}

/*
 * NAME:	closefiles()
 * DESCRIPTION:	close source and destination files
 */
static
void closefiles(hfsfile *ifile, int ofile, int *result)
{
  if (close(ofile) < 0 && *result == 0)
    {
      cpo_error = "error closing destination file";
      *result = -1;
    }

  if (hfs_close(ifile) < 0 && *result == 0)
    {
      cpo_error = hfs_error;
      *result = -1;
    }
}

/* Interface Routines ====================================================== */

/*
 * NAME:	cpo->macb()
 * DESCRIPTION:	copy an HFS file to a UNIX file using MacBinary II translation
 */
int cpo_macb(hfsvol *vol, char *srcname, char *dstname)
{
  hfsfile *ifile;
  int ofile, result = 0;

  if (openfiles(vol, srcname, dstname, ".bin", &ifile, &ofile) < 0)
    return -1;

  result = do_macb(ifile, ofile);
  closefiles(ifile, ofile, &result);

  return result;
}

/*
 * NAME:	cpo->binh()
 * DESCRIPTION:	copy an HFS file to a UNIX file using BinHex translation
 */
int cpo_binh(hfsvol *vol, char *srcname, char *dstname)
{
  hfsfile *ifile;
  int ofile, result;

  if (openfiles(vol, srcname, dstname, ".hqx", &ifile, &ofile) < 0)
    return -1;

  result = do_binh(ifile, ofile);
  closefiles(ifile, ofile, &result);

  return result;
}

/*
 * NAME:	cpo->text()
 * DESCRIPTION:	copy an HFS file to a UNIX file using text translation
 */
int cpo_text(hfsvol *vol, char *srcname, char *dstname)
{
  hfsfile *ifile;
  int ofile, result = 0;

  if (openfiles(vol, srcname, dstname, ".txt", &ifile, &ofile) < 0)
    return -1;

  result = do_text(ifile, ofile);
  closefiles(ifile, ofile, &result);

  return result;
}

/*
 * NAME:	cpo->raw()
 * DESCRIPTION:	copy the data fork of an HFS file to a UNIX file
 */
int cpo_raw(hfsvol *vol, char *srcname, char *dstname)
{
  hfsfile *ifile;
  int ofile, result = 0;

  if (openfiles(vol, srcname, dstname, 0, &ifile, &ofile) < 0)
    return -1;

  result = do_raw(ifile, ofile);
  closefiles(ifile, ofile, &result);

  return result;
}
