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
# include <string.h>
# include <stdlib.h>
# include <stdio.h>
# include <time.h>
# include <fcntl.h>
# include <errno.h>
# include <ctype.h>
# include <tcl.h>

# include "tclhfs.h"
# include "hfs.h"
# include "glob.h"
# include "copyin.h"
# include "copyout.h"
# include "version.h"

# define SIZE(type, n)		((size_t) (sizeof(type) * (n)))
# define ALLOC(type, n)		((type *) malloc(SIZE(type, n)))
# define ALLOCX(type, n)	((n) ? ALLOC(type, n) : (type *) 0)
# define FREE(ptr)		((ptr) ? (void) free((void *) ptr) : (void) 0)

# define REALLOC(ptr, type, n)  \
    ((type *) ((ptr) ? realloc(ptr, SIZE(type, n)) : malloc(SIZE(type, n))))
# define REALLOCX(ptr, type, n)  \
    ((n) ? REALLOC(type, n) : (FREE(ptr), (type *) 0))

static int err_umount, err_close;

typedef struct {
  hfsvol *vol;
  long cwd;
} volref;

typedef struct {
  hfsfile *file;
  Tcl_Interp *interp;
  Tcl_Command cmd;
} fileref;

Tcl_HashTable volumes;	/* set containing mounted volumes (no values) */
Tcl_HashTable files;	/* mapping of frefs -> vrefs */

/*
 * NAME:	error()
 * DESCRIPTION:	return a Tcl error for an HFS error
 */
static
int error(Tcl_Interp *interp, char *msg)
{
  char *str, c[2];

  str  = strerror(errno);
  c[0] = tolower(*str);
  c[1] = 0;

  Tcl_ResetResult(interp);

  if (msg)
    Tcl_AppendResult(interp, msg, ": ", (char *) 0);

  if (hfs_error == 0)
    Tcl_AppendResult(interp, c, str + 1, (char *) 0);
  else
    Tcl_AppendResult(interp, hfs_error, " (", str, ")", (char *) 0);

  return TCL_ERROR;
}

/*
 * NAME:	direntstr()
 * DESCRIPTION:	return a Tcl stat list for a single HFS entity
 */
static
char *direntstr(hfsdirent *ent)
{
  char cnid[12], parid[12], rsize[12], dsize[12], crdate[12], mddate[12];
  char *argv[22];
  register int argc;
  int locked, invis;

  sprintf(cnid,   "%lu", ent->cnid);
  sprintf(parid,  "%lu", ent->parid);
  sprintf(rsize,  "%lu", ent->rsize);
  sprintf(dsize,  "%lu", ent->dsize);
  sprintf(crdate, "%lu", ent->crdate);
  sprintf(mddate, "%lu", ent->mddate);

  argc = 0;

  argv[argc++] = "name";
  argv[argc++] = ent->name;

  argv[argc++] = "parid";
  argv[argc++] = parid;

  argv[argc++] = "kind";

  if (ent->flags & HFS_ISDIR)
    {
      argv[argc++] = "directory";

      argv[argc++] = "dirid";
      argv[argc++] = cnid;

      argv[argc++] = "size";
      argv[argc++] = dsize;

      argv[argc++] = "flags";
      argv[argc++] = (ent->fdflags & HFS_FNDR_ISINVISIBLE) ? "invis" : "";
    }
  else
    {
      argv[argc++] = "file";

      argv[argc++] = "fileid";
      argv[argc++] = cnid;

      argv[argc++] = "type";
      argv[argc++] = ent->type;

      argv[argc++] = "creator";
      argv[argc++] = ent->creator;

      locked = ent->flags & HFS_ISLOCKED;
      invis  = ent->fdflags & HFS_FNDR_ISINVISIBLE;

      argv[argc++] = "flags";
      argv[argc++] = (locked && invis) ? "locked invis" :
	(locked ? "locked" : (invis ? "invis" : ""));

      argv[argc++] = "rsize";
      argv[argc++] = rsize;

      argv[argc++] = "dsize";
      argv[argc++] = dsize;
    }

  argv[argc++] = "crdate";
  argv[argc++] = crdate;

  argv[argc++] = "mddate";
  argv[argc++] = mddate;

  return Tcl_Merge(argc, argv);
}

/*
 * NAME:	getdir()
 * DESCRIPTION:	collect and return the contents of an HFS directory
 */
static
int getdir(Tcl_Interp *interp, volref *vref, char *path)
{
  hfsvol *vol = vref->vol;
  hfsdir *dir;
  hfsdirent ent;
  char *str;

  if (hfs_setcwd(vol, vref->cwd) < 0 ||
      hfs_stat(vol, path, &ent) < 0)
    return error(interp, 0);

  if (ent.flags & HFS_ISDIR)
    {
      dir = hfs_opendir(vol, path);
      if (dir == 0)
	return error(interp, 0);

      while (hfs_readdir(dir, &ent) >= 0)
	{
	  str = direntstr(&ent);
	  if (str == 0)
	    {
	      hfs_closedir(dir);
	      Tcl_SetResult(interp, "out of memory", TCL_STATIC);
	      return TCL_ERROR;
	    }

	  Tcl_AppendElement(interp, str);

	  free(str);
	}

      if (hfs_closedir(dir) < 0)
	return error(interp, 0);
    }
  else  /* ! HFS_ISDIR */
    {
      str = direntstr(&ent);
      if (str == 0)
	{
	  interp->result = "out of memory";
	  return TCL_ERROR;
	}

      Tcl_AppendElement(interp, str);

      free(str);
    }

  return TCL_OK;
}

/*
 * NAME:	file->del()
 * DESCRIPTION:	called by Tcl when a file reference is deleted
 */
static
void file_del(ClientData clientData)
{
  fileref *fref = clientData;
  Tcl_HashEntry *entry;

  entry = Tcl_FindHashEntry(&files, (char *) fref);
  if (entry)
    Tcl_DeleteHashEntry(entry);

  err_close = hfs_close(fref->file);

  FREE(fref);
}

/*
 * NAME:	file->cmd()
 * DESCRIPTION:	operate on an HFS file
 */
static
int file_cmd(ClientData clientData, Tcl_Interp *interp,
	     int argc, char *argv[])
{
  fileref *fref = clientData;
  hfsfile *file = fref->file;

  switch (argc)
    {
    case 1:
      interp->result = "missing command";
      return TCL_ERROR;

    case 2:
      if (strcmp(argv[1], "close") == 0)
	{
	  Tcl_DeleteCommand(interp, argv[0]);
	  if (err_close < 0)
	    return error(interp, 0);
	}
      else if (strcmp(argv[1], "tell") == 0)
	{
	  long offset;

	  offset = hfs_lseek(file, 0, SEEK_CUR);
	  if (offset < 0)
	    return error(interp, 0);

	  sprintf(interp->result, "%lu", offset);
	}
      else if (strcmp(argv[1], "stat") == 0)
	{
	  hfsdirent ent;
	  char *str;

	  if (hfs_fstat(file, &ent) < 0)
	    return error(interp, 0);

	  str = direntstr(&ent);
	  if (str == 0)
	    {
	      interp->result = "out of memory";
	      return TCL_ERROR;
	    }

	  Tcl_SetResult(interp, str, TCL_DYNAMIC);
	}
      else
	{
	  Tcl_AppendResult(interp, "bad command \"", argv[1],
			   "\" or wrong # args", (char *) 0);
	  return TCL_ERROR;
	}
      break;

    case 3:
      if (strcmp(argv[1], "fork") == 0)
	{
	  int fork;

	  if (strcmp(argv[2], "data") == 0)
	    fork = 0;
	  else if (strcmp(argv[2], "rsrc") == 0 ||
		   strcmp(argv[2], "resource") == 0)
	    fork = 1;
	  else
	    {
	      interp->result = "bad arg to fork: must be data or rsrc";
	      return TCL_ERROR;
	    }

	  hfs_fork(file, fork);
	}
      else if (strcmp(argv[1], "seek") == 0)
	{
	  long offset;

	  if (Tcl_ExprLong(interp, argv[2], &offset) != TCL_OK)
	    return TCL_ERROR;

	  offset = hfs_lseek(file, offset, SEEK_SET);
	  if (offset < 0)
	    return error(interp, 0);

	  sprintf(interp->result, "%lu", offset);
	}
      else if (strcmp(argv[1], "read") == 0)
	{
	  long bytes;
	  char *mem;

	  if (Tcl_ExprLong(interp, argv[2], &bytes) != TCL_OK)
	    return TCL_ERROR;

	  if (bytes <= 0)
	    {
	      interp->result = "size must be > 0";
	      return TCL_ERROR;
	    }

	  mem = malloc(bytes + 1);
	  if (mem == 0)
	    {
	      interp->result = "out of memory";
	      return TCL_ERROR;
	    }

	  bytes = hfs_read(file, mem, bytes);
	  if (bytes < 0)
	    {
	      free(mem);
	      return error(interp, 0);
	    }

	  mem[bytes] = 0;
	  Tcl_SetResult(interp, mem, TCL_DYNAMIC);
	}
      else if (strcmp(argv[1], "write") == 0)
	{
	  if (hfs_write(file, argv[2], strlen(argv[2])) < 0)
	    return error(interp, 0);
	}
      else
	{
	  Tcl_AppendResult(interp, "bad command \"", argv[1],
			   "\" or wrong # args", (char *) 0);
	  return TCL_ERROR;
	}
      break;

    case 4:
      if (strcmp(argv[1], "seek") == 0)
	{
	  long offset;
	  int whence;

	  if (Tcl_ExprLong(interp, argv[2], &offset) != TCL_OK)
	    return TCL_ERROR;

	  if (strcmp(argv[3], "start") == 0 ||
	      strcmp(argv[3], "set") == 0)
	    whence = SEEK_SET;
	  else if (strcmp(argv[3], "current") == 0 ||
		   strcmp(argv[3], "cur") == 0)
	    whence = SEEK_CUR;
	  else if (strcmp(argv[3], "end") == 0)
	    whence = SEEK_END;
	  else
	    {
	      interp->result = "bad arg 3: must be start, current, or end";
	      return TCL_ERROR;
	    }

	  offset = hfs_lseek(file, offset, whence);
	  if (offset < 0)
	    return error(interp, 0);

	  sprintf(interp->result, "%lu", offset);
	}
      else
	{
	  Tcl_AppendResult(interp, "bad command \"", argv[1],
			   "\" or wrong # args", (char *) 0);
	  return TCL_ERROR;
	}
      break;

    default:
      Tcl_AppendResult(interp, "bad command \"", argv[1],
		       "\" or wrong # args", (char *) 0);
      return TCL_ERROR;
    }

  return TCL_OK;
}

/*
 * NAME:	do_copynative()
 * DESCRIPTION:	perform native HFS file copy
 */
static
int do_copynative(Tcl_Interp *interp, hfsfile *ifile, hfsfile *ofile)
{
  char buf[HFS_BLOCKSZ * 8];
  long bytes;

  /* data fork */

  while (1)
    {
      bytes = hfs_read(ifile, buf, sizeof(buf));
      if (bytes < 0)
	return error(interp, "error reading source file");
      else if (bytes == 0)
	break;

      if (hfs_write(ofile, buf, bytes) < bytes)
	return error(interp, "error writing destination file");
    }

  /* resource fork */

  if (hfs_fork(ifile, 1) < 0 ||
      hfs_fork(ofile, 1) < 0)
    return error(interp, "error opening resource fork");

  while (1)
    {
      bytes = hfs_read(ifile, buf, sizeof(buf));
      if (bytes < 0)
	return error(interp, "error reading source file");
      else if (bytes == 0)
	break;

      if (hfs_write(ofile, buf, bytes) < bytes)
	return error(interp, "error writing destination file");
    }

  return TCL_OK;
}

/*
 * NAME:	copynative()
 * DESCRIPTION:	copy an HFS file to another HFS volume
 */
static
int copynative(Tcl_Interp *interp, volref *srcvref, char *argv[])
{
  volref *dstvref;
  Tcl_CmdInfo info;
  Tcl_HashEntry *entry = 0;
  hfsdirent ent;
  char *srcname, *dstname;
  hfsfile *ifile, *ofile;
  int result;
  long cnid;

  if (Tcl_GetCommandInfo(interp, argv[3], &info))
    entry = Tcl_FindHashEntry(&volumes, (char *) info.clientData);

  if (entry == 0)
    {
      Tcl_AppendResult(interp, "unknown volume \"", argv[3], "\"", (char *) 0);
      return TCL_ERROR;
    }

  dstvref = info.clientData;

  srcname = argv[2];
  dstname = argv[4];

  if (hfs_setcwd(srcvref->vol, srcvref->cwd) < 0)
    return error(interp, 0);

  ifile = hfs_open(srcvref->vol, srcname);
  if (ifile == 0)
    return error(interp, "can't open source file");

  if (hfs_setcwd(dstvref->vol, dstvref->cwd) < 0)
    {
      error(interp, 0);
      hfs_close(ifile);
      return TCL_ERROR;
    }

  cnid = 0;

  if (hfs_stat(dstvref->vol, dstname, &ent) >= 0)
    {
      if (ent.flags & HFS_ISDIR)
	{
	  if (hfs_setcwd(dstvref->vol, ent.cnid) < 0)
	    {
	      error(interp, 0);
	      hfs_close(ifile);
	      return TCL_ERROR;
	    }

	  dstname = srcname;

	  if (hfs_stat(dstvref->vol, dstname, &ent) >= 0)
	    cnid = ent.cnid;
	}
      else
	cnid = ent.cnid;
    }

  if (hfs_fstat(ifile, &ent) < 0)
    {
      error(interp, "can't stat source file");
      hfs_close(ifile);
      return TCL_ERROR;
    }

  if (srcvref->vol == dstvref->vol &&
      ent.cnid == cnid)
    {
      interp->result = "source and destination files are the same";
      hfs_close(ifile);
      return TCL_ERROR;
    }

  hfs_delete(dstvref->vol, dstname);
  if (hfs_create(dstvref->vol, dstname, ent.type, ent.creator) < 0)
    {
      error(interp, "can't create destination file");
      hfs_close(ifile);
      return TCL_ERROR;
    }

  ofile = hfs_open(dstvref->vol, dstname);
  if (ofile == 0)
    {
      error(interp, "can't open destination file");
      hfs_close(ifile);
      return TCL_ERROR;
    }

  result = do_copynative(interp, ifile, ofile);

  ent.fdflags &= ~(HFS_FNDR_ISONDESK | HFS_FNDR_HASBEENINITED);

  if (result == TCL_OK && hfs_fsetattr(ofile, &ent) < 0)
    result = error(interp, "can't set destination file attributes");

  if (hfs_close(ofile) < 0 && result == TCL_OK)
    result = error(interp, "error closing destination file");

  if (hfs_close(ifile) < 0 && result == TCL_OK)
    result = error(interp, "error closing source file");

  return result;
}

/*
 * NAME:	copyin()
 * DESCRIPTION:	copy a UNIX file into an HFS volume
 */
static
int copyin(Tcl_Interp *interp, hfsvol *vol, char *argv[])
{
  int (*copyfile)(char *, hfsvol *, char *);

  if (strcmp(argv[2], "macbinary") == 0 ||
      strcmp(argv[2], "macb") == 0)
    copyfile = cpi_macb;
  else if (strcmp(argv[2], "binhex") == 0 ||
	   strcmp(argv[2], "binh") == 0)
    copyfile = cpi_binh;
  else if (strcmp(argv[2], "text") == 0)
    copyfile = cpi_text;
  else if (strcmp(argv[2], "raw") == 0 ||
	   strcmp(argv[2], "data") == 0)
    copyfile = cpi_raw;
  else
    {
      interp->result = "bad mode: must be macb, binh, text, or raw";
      return TCL_ERROR;
    }

  if (copyfile(argv[3], vol, argv[4]) < 0)
    {
      hfs_error = cpi_error;
      return error(interp, 0);
    }

  return TCL_OK;
}

/*
 * NAME:	copyout()
 * DESCRIPTION:	copy an HFS file out to a UNIX file
 */
static
int copyout(Tcl_Interp *interp, hfsvol *vol, char *argv[])
{
  int (*copyfile)(hfsvol *, char *, char *);

  if (strcmp(argv[2], "macbinary") == 0 ||
      strcmp(argv[2], "macb") == 0)
    copyfile = cpo_macb;
  else if (strcmp(argv[2], "binhex") == 0 ||
	   strcmp(argv[2], "binh") == 0)
    copyfile = cpo_binh;
  else if (strcmp(argv[2], "text") == 0)
    copyfile = cpo_text;
  else if (strcmp(argv[2], "raw") == 0 ||
	   strcmp(argv[2], "data") == 0)
    copyfile = cpo_raw;
  else
    {
      interp->result = "bad mode: must be macb, binh, text, or raw";
      return TCL_ERROR;
    }

  if (copyfile(vol, argv[3], argv[4]) < 0)
    {
      hfs_error = cpo_error;
      return error(interp, 0);
    }

  return TCL_OK;
}

/*
 * NAME:	vol->del()
 * DESCRIPTION:	called by Tcl when a volume reference is deleted
 */
static
void vol_del(ClientData clientData)
{
  volref *vref = clientData;
  Tcl_HashEntry *entry;

  entry = Tcl_FindHashEntry(&volumes, (char *) vref);
  if (entry)
    Tcl_DeleteHashEntry(entry);

  do
    {
      Tcl_HashSearch search;

      for (entry = Tcl_FirstHashEntry(&files, &search); entry;
	   entry = Tcl_NextHashEntry(&search))
	{
	  if (Tcl_GetHashValue(entry) == vref)
	    {
	      fileref *fref = (fileref *) Tcl_GetHashKey(&files, entry);

	      Tcl_DeleteCommand(fref->interp,
				Tcl_GetCommandName(fref->interp, fref->cmd));
	      break;
	    }
	}
    }
  while (entry);

  err_umount = hfs_umount(vref->vol);

  FREE(vref);
}

/*
 * NAME:	vol->cmd()
 * DESCRIPTION:	operate on an HFS volume
 */
static
int vol_cmd(ClientData clientData, Tcl_Interp *interp,
	    int argc, char *argv[])
{
  volref *vref = clientData;
  hfsvol *vol  = vref->vol;

  switch (argc)
    {
    case 1:
      interp->result = "missing command";
      return TCL_ERROR;

    case 2:
      if (strcmp(argv[1], "vname") == 0)
	{
	  hfsvolent ent;

	  hfs_vstat(vol, &ent);
	  Tcl_SetResult(interp, ent.name, TCL_VOLATILE);
	}
      else if (strcmp(argv[1], "size") == 0)
	{
	  hfsvolent ent;

	  hfs_vstat(vol, &ent);
	  sprintf(interp->result, "%lu %lu", ent.totbytes, ent.freebytes);
	}
      else if (strcmp(argv[1], "crdate") == 0)
	{
	  hfsvolent ent;

	  hfs_vstat(vol, &ent);
	  sprintf(interp->result, "%lu", ent.crdate);
	}
      else if (strcmp(argv[1], "mddate") == 0)
	{
	  hfsvolent ent;

	  hfs_vstat(vol, &ent);
	  sprintf(interp->result, "%lu", ent.mddate);
	}
      else if (strcmp(argv[1], "islocked") == 0)
	{
	  hfsvolent ent;

	  hfs_vstat(vol, &ent);
	  if (ent.flags & HFS_ISLOCKED)
	    interp->result = "1";
	  else
	    interp->result = "0";
	}
      else if (strcmp(argv[1], "umount") == 0)
	{
	  Tcl_DeleteCommand(interp, argv[0]);
	  if (err_umount < 0)
	    return error(interp, 0);
	}
      else if (strcmp(argv[1], "cwd") == 0)
	sprintf(interp->result, "%lu", vref->cwd);
      else if (strcmp(argv[1], "path") == 0)
	{
	  char name[32];
	  long id;
	  int listc, i;
	  char **listv;
	  char *result;

	  id = vref->cwd;
	  while (id != HFS_CNID_ROOTPAR)
	    {
	      if (hfs_dirinfo(vol, &id, name) < 0)
		return error(interp, 0);

	      Tcl_AppendElement(interp, name);
	    }

	  /* reverse the resulting list */

	  if (Tcl_SplitList(interp, interp->result, &listc, &listv) != TCL_OK)
	    return TCL_ERROR;

	  for (i = 0; i < listc / 2; ++i)
	    {
	      char *tmp;

	      tmp = listv[i];
	      listv[i] = listv[listc - 1 - i];
	      listv[listc - 1 - i] = tmp;
	    }

	  result = Tcl_Merge(listc, listv);
	  free(listv);

	  Tcl_SetResult(interp, result, TCL_DYNAMIC);
	}
      else if (strcmp(argv[1], "dir") == 0)
	{
	  if (getdir(interp, vref, ":") != TCL_OK)
	    return TCL_ERROR;
	}
      else if (strcmp(argv[1], "flush") == 0)
	{
	  if (hfs_flush(vol) < 0)
	    return error(interp, 0);
	}
      else if (strcmp(argv[1], "sepchar") == 0)
	interp->result = ":";
      else
	{
	  Tcl_AppendResult(interp, "bad command \"", argv[1],
			   "\" or wrong # args", (char *) 0);
	  return TCL_ERROR;
	}
      break;

    case 3:
      if (strcmp(argv[1], "cd") == 0 ||
	  strcmp(argv[1], "chdir") == 0)
	{
	  if (hfs_setcwd(vol, vref->cwd) < 0 ||
	      hfs_chdir(vol, argv[2]) < 0)
	    return error(interp, 0);

	  vref->cwd = hfs_getcwd(vol);
	}
      else if (strcmp(argv[1], "dirinfo") == 0)
	{
	  long id;
	  char name[32], idstr[12];

	  if (Tcl_ExprLong(interp, argv[2], &id) != TCL_OK)
	    return TCL_ERROR;

	  if (hfs_dirinfo(vol, &id, name) < 0)
	    return error(interp, 0);

	  sprintf(idstr, "%lu", id);
	  Tcl_AppendElement(interp, name);
	  Tcl_AppendElement(interp, idstr);
	}
      else if (strcmp(argv[1], "dir") == 0)
	{
	  if (getdir(interp, vref, argv[2]) != TCL_OK)
	    return TCL_ERROR;
	}
      else if (strcmp(argv[1], "open") == 0)
	{
	  static int id = 0;
	  hfsfile *file;
	  fileref *fref;
	  Tcl_CmdInfo info;
	  Tcl_HashEntry *entry;
	  int new;

	  fref = ALLOC(fileref, 1);
	  if (fref == 0)
	    {
	      interp->result = "out of memory";
	      return TCL_ERROR;
	    }

	  if (hfs_setcwd(vol, vref->cwd) < 0 ||
	      (file = hfs_open(vol, argv[2])) == 0)
	    {
	      error(interp, "can't open file");
	      FREE(fref);
	      return TCL_ERROR;
	    }

	  do
	    sprintf(interp->result, "hfsfile%d", id++);
	  while (Tcl_GetCommandInfo(interp, interp->result, &info));

	  fref->file   = file;
	  fref->interp = interp;
	  fref->cmd    = Tcl_CreateCommand(interp, interp->result,
					   file_cmd, fref, file_del);

	  entry = Tcl_CreateHashEntry(&files, (char *) fref, &new);
	  Tcl_SetHashValue(entry, vref);
	}
      else if (strcmp(argv[1], "stat") == 0)
	{
	  hfsdirent ent;
	  char *str;

	  if (hfs_setcwd(vol, vref->cwd) < 0 ||
	      hfs_stat(vol, argv[2], &ent) < 0)
	    return error(interp, 0);

	  str = direntstr(&ent);
	  if (str == 0)
	    {
	      interp->result = "out of memory";
	      return TCL_ERROR;
	    }

	  Tcl_SetResult(interp, str, TCL_DYNAMIC);
	}
      else if (strcmp(argv[1], "mkdir") == 0)
	{
	  if (hfs_setcwd(vol, vref->cwd) < 0 ||
	      hfs_mkdir(vol, argv[2]) < 0)
	    return error(interp, 0);
	}
      else if (strcmp(argv[1], "rmdir") == 0)
	{
	  if (hfs_setcwd(vol, vref->cwd) < 0 ||
	      hfs_rmdir(vol, argv[2]) < 0)
	    return error(interp, 0);
	}
      else if (strcmp(argv[1], "delete") == 0)
	{
	  if (hfs_setcwd(vol, vref->cwd) < 0 ||
	      hfs_delete(vol, argv[2]) < 0)
	    return error(interp, 0);
	}
      else if (strcmp(argv[1], "touch") == 0)
	{
	  hfsdirent ent;

	  if (hfs_setcwd(vol, vref->cwd) < 0 ||
	      hfs_stat(vol, argv[2], &ent) < 0)
	    return error(interp, 0);
	}
      else if (strcmp(argv[1], "glob") == 0)
	{
	  int listc, fargc;
	  char **listv, **fargv, *result;

	  if (hfs_setcwd(vol, vref->cwd) < 0)
	    return error(interp, 0);

	  if (Tcl_SplitList(interp, argv[2], &listc, &listv) != TCL_OK)
	    return TCL_ERROR;

	  fargv = hfs_glob(vol, listc, listv, &fargc);
	  free(listv);

	  if (fargv == 0)
	    {
	      interp->result = "globbing error";
	      return TCL_ERROR;
	    }

	  result = Tcl_Merge(fargc, fargv);
	  free(fargv);

	  Tcl_SetResult(interp, result, TCL_DYNAMIC);
	}
      else
	{
	  Tcl_AppendResult(interp, "bad command \"", argv[1],
			   "\" or wrong # args", (char *) 0);
	  return TCL_ERROR;
	}
      break;

    case 4:
      if (strcmp(argv[1], "rename") == 0)
	{
	  if (hfs_setcwd(vol, vref->cwd) < 0 ||
	      hfs_rename(vol, argv[2], argv[3]) < 0)
	    return error(interp, 0);
	}
      else
	{
	  Tcl_AppendResult(interp, "bad command \"", argv[1],
			   "\" or wrong # args", (char *) 0);
	  return TCL_ERROR;
	}
      break;

    case 5:
      if (strcmp(argv[1], "create") == 0)
	{
	  if (hfs_setcwd(vol, vref->cwd) < 0 ||
	      hfs_create(vol, argv[2], argv[3], argv[4]) < 0)
	    return error(interp, 0);
	}
      else if (strcmp(argv[1], "copy") == 0)
	return copynative(interp, vref, argv);
      else if (strcmp(argv[1], "copyin") == 0)
	{
	  if (hfs_setcwd(vol, vref->cwd) < 0)
	    return error(interp, 0);

	  return copyin(interp, vol, argv);
	}
      else if (strcmp(argv[1], "copyout") == 0)
	{
	  if (hfs_setcwd(vol, vref->cwd) < 0)
	    return error(interp, 0);

	  return copyout(interp, vol, argv);
	}
      else
	{
	  Tcl_AppendResult(interp, "bad command \"", argv[1],
			   "\" or wrong # args", (char *) 0);
	  return TCL_ERROR;
	}
      break;

    default:
      Tcl_AppendResult(interp, "bad command \"", argv[1],
		       "\" or wrong # args", (char *) 0);
      return TCL_ERROR;
    }

  return TCL_OK;
}

/*
 * NAME:	cmd->hfs()
 * DESCRIPTION:	Tcl HFS command callback
 */
static
int cmd_hfs(ClientData clientData, Tcl_Interp *interp,
	    int argc, char *argv[])
{
  static int id = 0;

  if (argc < 2)
    {
      interp->result = "wrong # args";
      return TCL_ERROR;
    }

  if (strcmp(argv[1], "mount") == 0)
    {
      int partno = 1;
      hfsvol *vol;
      volref *vref;
      Tcl_CmdInfo info;
      Tcl_HashEntry *entry;
      int new;

      if (argc < 3 || argc > 4)
	{
	  interp->result = "wrong # args";
	  return TCL_ERROR;
	}

      if (argc == 4 &&
	  Tcl_GetInt(interp, argv[3], &partno) != TCL_OK)
	return TCL_ERROR;

      vref = ALLOC(volref, 1);
      if (vref == 0)
	{
	  interp->result = "out of memory";
	  return TCL_ERROR;
	}

      vol = hfs_mount(argv[2], partno, O_RDWR);
      if (vol == 0)
	{
	  error(interp, "can't mount volume");
	  FREE(vref);
	  return TCL_ERROR;
	}

      vref->vol = vol;
      vref->cwd = HFS_CNID_ROOTDIR;

      entry = Tcl_CreateHashEntry(&volumes, (char *) vref, &new);

      do
	sprintf(interp->result, "hfsvol%d", id++);
      while (Tcl_GetCommandInfo(interp, interp->result, &info));

      Tcl_CreateCommand(interp, interp->result,
			vol_cmd, vref, vol_del);
    }
  else if (strcmp(argv[1], "format") == 0)
    {
      int partno = 0;

      if (argc != 5)
	{
	  interp->result = "wrong # args";
	  return TCL_ERROR;
	}

      if (Tcl_GetInt(interp, argv[3], &partno) != TCL_OK)
	return TCL_ERROR;

      if (hfs_format(argv[2], partno, argv[4]) < 0)
	return error(interp, 0);
    }
  else if (strcmp(argv[1], "flushall") == 0)
    hfs_flushall();
  else if (strcmp(argv[1], "version") == 0)
    {
      if (argc != 2)
	{
	  interp->result = "wrong # args";
	  return TCL_ERROR;
	}

      interp->result = VERSION;
    }
  else if (strcmp(argv[1], "copyright") == 0)
    {
      if (argc != 2)
	{
	  interp->result = "wrong # args";
	  return TCL_ERROR;
	}

      interp->result = COPYRIGHT;
    }
  else if (strcmp(argv[1], "author") == 0)
    {
      if (argc != 2)
	{
	  interp->result = "wrong # args";
	  return TCL_ERROR;
	}

      interp->result = AUTHOR;
    }
  else if (strcmp(argv[1], "license") == 0)
    {
      if (argc != 2)
	{
	  interp->result = "wrong # args";
	  return TCL_ERROR;
	}

      interp->result = LICENSE;
    }
  else
    {
      Tcl_AppendResult(interp, "bad hfs command \"", argv[1],
		       "\": should be one of "
		       "mount, format, flushall, "
		       "version, copyright, author, license",
		       (char *) 0);
      return TCL_ERROR;
    }

  return TCL_OK;
}

/*
 * NAME:	cmd->ctime()
 * DESCRIPTION:	implementation for ctime Tcl command
 */
static
int cmd_ctime(ClientData clientData, Tcl_Interp *interp,
	      int argc, char *argv[])
{
  long ldate;
  time_t date;
  static char str[26];

  if (argc != 2)
    {
      interp->result = "wrong # args";
      return TCL_ERROR;
    }

  if (Tcl_ExprLong(interp, argv[1], &ldate) != TCL_OK)
    return TCL_ERROR;

  date = ldate;
  strcpy(str, ctime(&date));
  str[24] = 0;

  interp->result = str;

  return TCL_OK;
}

/*
 * NAME:	cmd->exit()
 * DESCRIPTION:	called to terminate; clean up volume state
 */
static
int cmd_exit(ClientData clientData, Tcl_Interp *interp,
	     int argc, char *argv[])
{
  int status = 0;

  if (argc > 2)
    {
      interp->result = "wrong # args: should be \"exit ?returnCode?\"";
      return TCL_ERROR;
    }

  if (argc == 2 && Tcl_GetInt(interp, argv[1], &status) != TCL_OK)
    return TCL_ERROR;

  hfs_umountall();

  exit(status);

  return TCL_OK;
}

/*
 * NAME:	Hfs->Init()
 * DESCRIPTION:	initialize Tcl components for HFS handling
 */
int Hfs_Init(Tcl_Interp *interp)
{
  Tcl_InitHashTable(&volumes, TCL_ONE_WORD_KEYS);
  Tcl_InitHashTable(&files,   TCL_ONE_WORD_KEYS);

  Tcl_CreateCommand(interp, "hfs",   cmd_hfs,   0, 0);
  Tcl_CreateCommand(interp, "ctime", cmd_ctime, 0, 0);

  Tcl_CreateCommand(interp, "exit",  cmd_exit,  0, 0);

  return TCL_OK;
}
