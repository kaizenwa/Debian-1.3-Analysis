/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Random access file routines.

The GNU Pascal Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU Pascal Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU Pascal Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include "rts.h"
/* File Descriptor Record definition */
#include "rts-fdr.h"
#include <sys/stat.h>

/* NOTE:
 *   Extended Pascal defined the following operations only to
 * DIRECT ACCESS FILE types:
 *
 *   SeekRead, SeekWrite, SeekUpdate, Empty, Position, LastPosition
 *   and Update (in not writing to end of file)
 *
 * Direct access file is defined by: FILE [ indextype ] OF type 
 * (the ord(a) in assertions means the smallest value of indextype)
 *
 * However, Gpc does not currently implement direct access files, and
 * anyhow maybe we should allow the operations also to other
 * files capable of seek(2). These non-direct access files may be
 * thought of the following direct access file type:
 *
 * TYPE Natural = 0..MaxInt;
 *      gpcfiles = FILE Natural OF <type>
 *
 * DefineSize is a GPC extension.
 *
 */

static void
_p_direct_warn (note)
     char *note;
{
  if (_p_force_direct_files)
    _p_error (ABORT, note);
  else
    _p_warning (note);
}

/*
 * Recalculate the m_FISIZE for file.
 * Take care you flush buffers properly!
 */
static void
_p_resize (File)
     FDR File;
{
  struct stat finfo;
    
  if (fstat(fileno(m_FILNUM(File)), &finfo) == 0)
    m_FISIZE(File) = NUMBYTE (File, finfo.st_size);
  else
    _p_error (REPORT, "Could not stat file %s", m_NAM(File));
}

/* DEFINESIZE (This is a GPC extension)
 *
 * May be applied only to random access files and files opened
 * for writing.
 *
 * Define files size as count of its component type units
 */
void
_p_definesize(File, NewSize)
     FDR File;
     int NewSize;
{
  long ByteNum;

  if (m_STATUS(File) == FiNOP)
    _p_generic(407);
  else if (is_RONLY(File))
    _p_error (ABORT, "`DefineSize' to a read only file `%s'", m_NAM(File));

  if (NewSize < m_FISIZE(File))
    {
      ByteNum = BYTENUM(File, NewSize);
      D(3, printf("DefineSize: trucating file to %ld bytes\n", ByteNum));

#ifdef HAVE_FTRUNCATE
      if (ftruncate(fileno(m_FILNUM(File)), ByteNum) == -1)
	_p_error(ABORT, "DefineSize truncation failed");
#else
      /* Maybe this is not worth reporting?
       * The call is not in 10206 pascal anyway...
       */
      _p_error (REPORT, "ftruncate needed by definesize()");

      /* The way to go without FTRUNCATE might be to copy the
       * NewSize first elements of the file to a temp file,
       * then do some file renaming, but I won't do it now.
       */
#endif
    }
  else
    _p_seek (File, NewSize, 0, 1);

  m_FISIZE(File) = NewSize;
}

/* SEEKREAD
 * pre-assertion: (neither f0.L nor f0.R is undefined) and
 *		  ( 0<= ord(n)-ord(a) <= length(f0.L~f0.R))
 * post-assertion:(f.M = Inspection) and (f.L~f.R = f0.L~f0.R) and
 *		  (if length(f0.L~f0.R) > ord(n)-ord(a) then
 *		     ((length(f.L) =  ord(n)-ord(a)) and
 *		      (f^ = f.R.first))
 *		   else
 *		     ((f.R = S() and f^ is undefined)))
 *
 * NEWPLACE is an offset from ZERO to the correct location.
 */
void
_p_seekread(File, NewPlace)
     FDR File;
     int NewPlace;	
{
  if (is_WONLY (File))
    _p_error (ABORT, "`SeekRead' to a write only file");
  else if (NewPlace < 0)
    _p_generic(410);

  _p_open (File, NULL, M_UPDATE | M_READ, 0);

  if (NewPlace > m_FISIZE(File))
    _p_generic(406);

  if (!tst_DIRECT(File))
    _p_direct_warn ("Direct access routine `SeekRead' applied to a regular file");

  D(3, printf("SeekRead: seek to element %ld\n", (long)NewPlace));

  if (_p_seek (File, NewPlace, 0, 1))
    _p_error(ABORT, "SeekRead seek failed");

  /* Change the current status of file to INSPECTION */
  CLR_STATUS (File, FiANY);
  SET_STATUS (File, FiORE);

  clr_EOF (File);
  _p_get(File);

  /* Seek back to the place where we were before the
     GET. It's m_SIZ(File) before the place we are now at */

  if (! tst_EOF (File) && _p_seek (File, -1, 1, 1))
    _p_error (ABORT, "`SeekRead' failed to reset file position");
}

/* SEEKWRITE
 * pre-assertion: (neither f0.L nor f0.R is undefined) and
 *		  ( 0<= ord(n)-ord(a) <= length(f0.L~f0.R))
 * post-assertion:(f.M = Generation) and (f.L~f.R = f0.L~f0.R) and
 *		  (length(f.L) = ord(n)-ord(a)) and (f^ is undefined)
 *
 * Note: this definition DOES NOT WRITE anything. It just moves the
 * file pointer and changes the MODE to GENERATION.
 *
 * NEWPLACE is an offset from ZERO to the correct location.
 */
void
_p_seekwrite(File, NewPlace)
FDR	File;
int NewPlace;
{
  if (is_RONLY (File))
    _p_generic (411);
  else if (NewPlace < 0)
    _p_generic(410);

  _p_open (File, NULL, M_UPDATE | M_WRITE, 0);

  if (NewPlace > m_FISIZE(File)) {
    /* It fails always in 10206.
     */
    _p_error (REPORT, "GPC extension: Extending file %s in `SeekWrite' to %d elements",
	      m_NAM (File), NewPlace);
    
    _p_definesize(File, NewPlace);
  }

  if (!tst_DIRECT(File))
    _p_direct_warn ("Direct access routine `SeekWrite' applied to a regular file");

  D(3, printf("SeekWrite: seek to element %ld\n", (long)NewPlace));
  if (_p_seek (File, NewPlace, 0, 1))
    _p_error(ABORT, "SeekWrite seek failed");

  /* Change the mode to generation */
  CLR_STATUS (File, FiANY);
  SET_STATUS (File, FiWRI);
}

/* SEEKUPDATE
 * pre-assertion: (neither f0.L nor f0.R is undefined) and
 *		  ( 0<= ord(n)-ord(a) <= length(f0.L~f0.R))
 * post-assertion:(f.M = Update) and (f.L~f.R = f0.L~f0.R) and
 *		  (if length(f0.L~f0.R) > ord(n)-ord(a) then
 *		     ((length(f.L) =  ord(n)-ord(a)) and
 *		      (f^ = f.R.first))
 *		   else
 *		     ((f.R = S()) and (f^ is undefined)))
 *
 * The (only) difference with SEEKREAD is that this leaves f.M to
 * UPDATE mode.
 */
void
_p_seekupdate(File, NewPlace)
     FDR File;
     int NewPlace;
{
  if (is_RONLY (File))
    _p_error (ABORT, "`SeekUpdate' to a read only file");
  else if (NewPlace < 0)
    _p_generic(410);

  _p_open (File, NULL, M_UPDATE | M_READ, 0);

  if (NewPlace > m_FISIZE(File))
    _p_generic(406);

  if (!tst_DIRECT(File))
    _p_direct_warn ("Direct access routine `SeekUpdate' applied to a regular file");

  D(3, printf("SeekUpdate: seek to element %ld\n", (long)NewPlace));
  if (_p_seek (File, NewPlace, 0, 1))
    _p_error(ABORT, "`SeekUpdate' seek failed");

  CLR_STATUS (File, FiANY);
  SET_STATUS (File, FiRND);

  clr_EOF(File);
  _p_get(File);
    
  /* Seek back to the place where we were before the
     GET. It's m_SIZ(File) before the place we are now at */

  if (! tst_EOF (File) && _p_seek (File, -1, 1, 0))
    _p_error (ABORT, "`SeekUpdate' failed to reset file position");
}

/* EMPTY 
 *
 * Returns True if file is Empty, otherwise False
 */
int
_p_empty (File)
     FDR File;
{
  if (! (m_STATUS(File) & FiANY))
    _p_generic(407);

  if (!tst_DIRECT(File))
    _p_direct_warn ("Direct access routine `Empty' applied to a regular file");

  return tst_EMPTY(File);
}

/* UPDATE
 * pre-assertion: (f0.M = Generation or f0.M = Update) and
 *		  (neither f0.L nor f0.R is undefined) and
 *		  (f is a direct access file type) and
 *		  (f0^ is not undefined)
 * post-assertion:(f.M = f0.M) and (f.L = f0.L) and
 *		  (if f0.R = S() then
 *		     (f.R = S(f0^))
 *		   else
 *		     (f.R = S(f0^)~f0.R.rest)) and
 *		   (f^ = f0^).
 *
 * i.e. Write the stuff in, and leave it also in the file buffer.
 * don't advance the file pointer from the pre-assert state!
 */
void
_p_update (File)
     FDR File;
{
  int is_random;
    
  if (!tst_DIRECT(File))
    _p_direct_warn ("Direct access routine `Update' applied to a regular file");

  /* If the file buffer contents is lazy, validate it */
  if (tst_LGET (File))
    {
      clr_LGET (File);
      _p_get (File);
    }

#if 0
  /* @@@@@ Ooops: Currently assigning a value to a file buffer
     does not clear the UND bit in the status word.
     Disable this check -> UNDefined file buffers may be written with
     update...
   */
  if (tst_UND (File))
    _p_error (ABORT, "`Update(%s)' with an undefined file buffer",
	      m_NAM (File));
#endif
  
  is_random = TST_STATUS (File, FiRND);
  if (is_random)
    {
      /* Change the mode to generation, prevents implicit GET
       * Yes, PUT in UPDATE mode gets the next element by default.
       */
      CLR_STATUS (File, FiANY);
      SET_STATUS (File, FiWRI);
    }

  /* Write to the current location.
   * _p_put does not clobber file buffer.
   */
  _p_put (File);

  if (is_random)
    {
      /* Change the mode back to random access */
      CLR_STATUS (File, FiANY);
      SET_STATUS (File, FiRND);
    }
  
  /* The file buffer is still f0^ */
  clr_UND (File);

  /* Seek back to the place where we were before the
   * PUT. It's m_SIZ(File) before the place we are now at
   */
  if (_p_seek (File, -1, 1, 1))
    _p_error (ABORT, "`Update' failed to reset the file position");
}

/* LASTPOSITION
 * LastPosition(f) = succ(a, length(f.L~f.R)-1);
 */
int
_p_lastposition(File)
     FDR File;
{
  if (m_STATUS(File) == FiNOP)
    _p_generic(407);

  if (!tst_DIRECT(File))
    _p_direct_warn ("Direct access routine `Update' applied to a regular file");

  _p_resize (File);

  return m_FISIZE (File)-1;
}

/* POSITION
 * Position(f) = succ(a, length(f.L));
 *
 * This is the element number always counting from ZERO
 *
 * (Since the run time system does not know the low bound
 *  of the direct access file type)
 *
 * The returned value is an offset from A, so the compiler needs to
 * adjust the value before it is returned to the user.
 */
int
_p_position(File)
     FDR File;
{
  long NumBytes;

  if (m_STATUS(File) == FiNOP)
    _p_generic(407);
  
  if (!tst_DIRECT(File))
    _p_direct_warn ("Direct access routine `Update' applied to a regular file");

  NumBytes = ftell (m_FILNUM (File));

  return NUMBYTE (File, NumBytes);
}
