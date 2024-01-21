/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   Generic file handling routines and their support.

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

/* Routine to call when you wish to flush the file buffers
 * from your pascal source. (OBSOLETE?)
 */
void
_p_flush (File)
     FDR File;
{
  if (is_WRITABLE (File))
    fflush (m_FILNUM (File));
}

/* Call this if you need to use the (FILE *) pointer in your pascal program
 * for external C routines.
 */
FILE *
_p_getfile (File)
     FDR File;
{
  return m_FILNUM (File);
}

void
_p_initfdr(File, Name, Size, flags)
     FDR   File; /* address of FDR variable */
     char *Name;  /* address of internal name in program */
     int   Size;  /* file buffer size; in bits, if Packed, else in bytes */
     int   flags; /* Direct (16), Lazy (8), Ext (4), Pack(2), Text(1) Flags */
{
    m_FILSTA(File) = 0;
    m_STATUS(File) = FiNOP;
    m_NXTFDR(File) = NULL;

    if (flags & (1 << fkind_TEXT))
      set_TXT(File);
    if (flags & (1 << fkind_PACKED))
      set_PCK(File);
    if (flags & (1 << fkind_EXTERN))
      set_EXT(File);
    if (flags & (1 << fkind_LAZY))
      set_LAZY (File);
    if (flags & (1 << fkind_DIRECT))
      set_DIRECT (File);
    if (flags & (1 << fkind_BYTE))
      set_BYTE (File);	/* Unused */
    
    if (! Name)
      _p_error (ABORT, "File has no internal name?");

    m_NAM (File)   = Name;
    m_EXTNAM (File) = NULL;
    m_BINDING (File)= NULL;

    if (Size)
      {
	m_SIZ (File) = Size;
	if (tst_PCK (File))
	  {
	    /* Convert to unpacked file, since we don't yet support
	     * PACKED files. This may be done even if the compiler
	     * thinks we support them :-)
	     */
	    m_SIZ(File) = (m_SIZ(File) + 7) / 8;
	    clr_PCK(File);
	  }

#if 1
	/* Allocate file buffer */
	m_FILBPTR(File) = _p_malloc (m_SIZ(File));
#else
	/* @@@@ Why was this? */
	/* Always allocate at least one word for the file buffer */
	{
	  int s = m_SIZ (File);
	  if (s < BYTES_PER_WORD)
	    s = BYTES_PER_WORD;

	  /* Allocate file buffer */
	  m_FILBPTR(File) = _p_malloc (s);
	}
#endif /* 1 */
      }

    /* Mark the file buffer contents undefined */
    set_UND(File);

    m_FILNUM(File) = NULL;
    D(2, _p_printfdr(File,"INITFDR"));
}

#ifndef HAVE_STRCASECMP
#ifdef HAVE_STRICMP
#define strcasecmp(s,d) stricmp(s,d)
#else
/* Very slow strcasecmp(). Should use the one in the C library. */
int
strcasecmp(s1, s2)
char *s1, *s2;
{
    char *S1 = _p_strdup(s1), *S2 = _p_strdup(s2);
    int res;
    
    s1 = S1;
    s2 = S2;
    if (s1)
	while(*s1) {
	    if (isupper(*s1))
		*s1 = tolower(*s1);
	    s1++;
	}

    if (s2)
	while(*s2) {
	    if (isupper(*s2))
		*s2 = tolower(*s2);
	    s2++;
	}
	
    res = strcmp(S1, S2);
    free(S1); free(S2);
    return res;
}
#endif /* HAVE_STRICMP */
#endif /* HAVE_STRCASECMP */

void
_p_initialize_std_files ()
{
  FDR ofile = &_p_stdout;
  FDR ifile = &_p_stdin;

  /* Open standard output */

  _p_initfdr (ofile, OUTPUT_FILE_NAME, 8, 0xf); 
  _p_rewrite (ofile, NULL, 0);

  /* Open standard input */

  _p_initfdr (ifile, INPUT_FILE_NAME, 8, 0xf); 
  _p_reset   (ifile, NULL, 0);
}


static char *
_p_nameit(File, mode)
FDR	File;
int	mode;
{
    int	tty;
    int	n, in, out;
    assoc	*ap;
    char	b[512];

    if (!tst_EXT(File)) {
	_p_makename(b);
	D(2, fprintf(stderr,"Opening internal file %s\n",b));
	return(_p_strdup(b));
    }
    for(ap = Gpc_assoc; ap->int_name ; ap++) {
	if (strcasecmp(m_NAM(File),ap->int_name) == 0) {
	   ap->int_name = "?KukkuuRESET?"; /* Allow close(a); reset(a) to access next one */
	   m_EXTNAM (File) = _p_strdup (ap->ext_name);
	   return(m_EXTNAM (File));
        }
    }
    if (tst_TXT(File) &&
	(strcmp(m_NAM(File),INPUT_FILE_NAME) == 0 ||
	 strcmp(m_NAM(File),OUTPUT_FILE_NAME) == 0))
	return(0);

    /* Try to write filename prompts to /dev/tty and try to read responces
       from there also, to avoid mungling with stdin & stdout.

       However, if everything fails, try stdin & stdout, if they don't
       work, abort. You can also use -a switch to associate internal names
       to external ones. It only needs to be documented...
     */

    if ((tty = open ("/dev/tty", O_RDWR)) < 0) {
	_p_error (REPORT, "Failed to open terminal for file name read, using stdin & stdout");
	in  = 0;
	out = 1;
    } else 
	in = out = tty;

    sprintf(b,"%s file %s: ",
	    (mode & M_READ) ? "Input" : (mode & M_WRITE) ? "Output" : "Expand",
	    m_NAM(File));
    if ((n = write (out,b,strlen(b))) != strlen(b)) {
	if (out != 1)
	    _p_error (REPORT, "Writing file name prompt to /dev/tty failed, using stdout");
	if (out == 1 || (n = write (1,b,strlen(b))) < 0)
	    _p_error (ABORT, "Can't prompt user for external file name bindings");
    }
    if ((n = read (in,b,sizeof(b))) < 0) {
	if (in) {
	    _p_error (REPORT, "Reading filename from /dev/tty failed, trying stdin");
	    /* B should be ok still, since read failed. */
	    (void) write (tty,b,strlen(b));
	}
	if ( ! in || (n = read (0,b,sizeof(b))) < 0)
	    _p_error (ABORT, "Can't query user for external file name bindings");
    }

    if (b[0] == EOT)
      _p_error (ABORT, "EOT character given for file name query -- aborting");

    b[n] = '\0';
    if (n > 0 && b[n-1] == '\n') b[n-1] = '\0';
    close (tty);
    m_EXTNAM (File) = _p_strdup (b);
    return m_EXTNAM (File);
}

static void
_p_fdrchain(Fdr, What)
FDR	Fdr;
int What;
{
    FDR scan = NULL;

    switch (What) {
    case TRUE:
	if (!FirstFdr) {
	    FirstFdr = LastFdr = Fdr;
	    m_NXTFDR(Fdr) = NULL;
	    D(4, fprintf(stderr, "Adding %s to empty fdr list\n",m_NAM(Fdr)));
	} else {
	    for(scan=FirstFdr; scan; scan = m_NXTFDR(scan)) {
		if (scan == Fdr) /* FDR already in list, do nothing */
		    break;
	    }
	    if (!scan) {
		D(4, fprintf(stderr, "Adding %s to fdr list\n", m_NAM(Fdr)));
		if (m_NXTFDR(LastFdr))
		    _p_error(REPORT, "LastFdr->nxtfdr is not NULL\n");
		m_NXTFDR(LastFdr) = Fdr;
		m_NXTFDR(Fdr) = NULL;
		LastFdr = Fdr;
	    }
	}
	break;
    case FALSE:
	if (FirstFdr == Fdr) {  /* First in the Fdr list */
	    FirstFdr = m_NXTFDR(Fdr);
	    m_NXTFDR(Fdr) = NULL;
	    if (LastFdr == Fdr) /* The only element */
		LastFdr = NULL;
	    D(4, fprintf(stderr, "Removed fdr %s (first) from list\n",
			 m_NAM(Fdr)));
	} else {
	    /* Look for the element before Fdr in the list */
	    for(scan=FirstFdr; m_NXTFDR(scan) != Fdr; scan = m_NXTFDR(scan))
		if (!m_NXTFDR(scan)) {
		    scan = NULL;
		    break;
		}
	    if (scan) {
		D(4, fprintf(stderr, "Removed fdr %s from list\n",m_NAM(Fdr)));
		m_NXTFDR(scan) = m_NXTFDR(Fdr); /* skip over */
		m_NXTFDR(Fdr)  = NULL;
		if (LastFdr == Fdr) LastFdr = scan;
	    } else
		_p_error(REPORT, "Fdr not found in list\n");
	}
	break;
    }
}

/* Move the file pointer to the requested pascal record of the FILE.
 * RECORD specifies how much to move, negative is backward, positive
 * is forward.
 * RELATIVE is 0 if this is an absolute move, 1 if relative, 2 if
 * counting starts from end of file.
 *
 * Note: The FSEEK 3rd parameter matches these values in UN*X systems.
 * @@@@@@@@@@@@@@@ Fix this!
 *
 * The file is flushed before the move is attempted.
 */
int
_p_seek (File, record, relative, flush_it)
     FDR File;
     int record;
     int relative;
     int flush_it;
{
  long bytenum;

  if (flush_it)
    {
      D(3, printf("flush before seek\n"));
      fflush (m_FILNUM(File));
    }
  
  if (relative)
    bytenum = record*m_SIZ(File);
  else
    bytenum = BYTENUM (File, record);
      
  D(3, printf("seek to byte %ld relative=%d\n", bytenum, relative));
  
  if (fseek (m_FILNUM(File), bytenum, relative))
    return -1;

  return 0;
}

/*
 * Open a FILE named FILENAME in MODE.
 *
 * MODE is M_APPEND, M_READ, M_WRITE, (M_READ | M_UPDATE) or (M_WRITE | M_UPDATE)
 *
 * LENGTH specifies the maximum legth of FILENAME.
 *
 * Trailing spaces are stripped from FILENAME
 */

void
_p_open (File, Filename, mode, length)
     FDR      File;
     char    *Filename;
     int      mode;
     int      length;
{
    char *errstr = (char *) NULL;
    char *filename = (char *) NULL;
    struct stat finfo;

    /* Standard way of binding a file to an external entity */
    m_BINDING (File) = _p_get_binding (File);

    /* takes precedence over extensions and everything else in file naming */
    if (m_BINDING (File))
      {
	if (length)
	  _p_error (REPORT, "Filename parameter ignored because file has a binding");

	filename = m_BINDING (File)->name;
      }
    else
      {
	/* Pascal: 1..length, C: 0..(length-1) */
	if (length > 0)
	  {
	    /* Strip trailing spaces */
	    while (length > 0 && Filename[ length - 1 ] == ' ')
	      length--;
	  }

	if (length > 0)
	  {
	    filename = _p_malloc (length+1);
	    (void) strncpy (filename, Filename, length);
	    filename[length] = '\000';
	  }
      }
    

    if (m_SIZ (File) == 0)
      _p_generic (701);	/* FDR not initialized */

    if (filename)
      {
	_p_close(File);
	m_EXTNAM (File) = filename;
      }

    if (m_STATUS(File) != FiNOP)
      {
	/* File is currently open in Pascal program */

	if (! (mode & M_READ) && is_RONLY (File))
	  _p_error (ABORT, "Can't write to a read only file");
	
	if (mode == M_APPEND)
	  _p_seek (File, 0, 2, is_WRITABLE(File)); /* Start appending */
	else if (mode & M_READ || mode & M_UPDATE)
	  _p_seek (File, 0, 0, is_WRITABLE(File)); /* Start reading or update */
	else
	  {
#ifdef HAVE_FTRUNCATE
	    /* We have ftruncate () */
	    _p_seek (File, 0, 0, is_WRITABLE(File)); /* Start writing */

	    if (ftruncate(fileno(m_FILNUM(File)), 0) == -1)
	      _p_error (ABORT, "ftruncate failed when re-opening file with `Rewrite'");
#else
	    /* If you don't have ftruncate() emulate the behaviour */
	    filename = m_EXTNAM(File);
	    m_EXTNAM(File) = NULL;
	    _p_close(File);

	    m_EXTNAM(File) = filename;

	    /* Let the code below re-open the same external file for writing */
	    /* If the file is internal, it will not be the same, but who cares. */
#endif
	  }
      }

    if (m_STATUS(File) == FiNOP)
      {
	while(m_FILNUM(File) == NULL)
	  {
	    char *how;

	    if ((mode & M_READ) && !(tst_EXT(File) || m_BINDING(File)))
	      _p_error (ABORT, "`Reset', SeekUpdate' or `SeekRead' to nonexistent internal file `%s'",
			m_NAM(File));

	    if (!filename)
	      filename = _p_nameit(File, mode);

	    if (!filename)
	      m_FILNUM(File) = (mode == M_READ ? current_stdin : stdout);
	    else if (! *filename)
	      m_FILNUM(File) = NULL;
	    else
	      {
		switch (mode) {
		case M_READ|M_UPDATE:
		case M_READ:
#if defined (__MSDOS__) || defined (_WIN32) || defined (__EMX__)
		    if (! tst_TXT(File))
		    	how = "rb+";
		    else
#endif
			how = "r+";
		    errstr = "File can not be opened for reading";
		    break;
		case M_WRITE|M_UPDATE:
		case M_WRITE:
#if defined (__MSDOS__) || defined (_WIN32) || defined (__EMX__)
		    if (! tst_TXT(File))
		    	how = "wb+";
		    else
#endif
			how = "w+";
		    errstr = "File can not be opened for writing";
		    break;
		case M_APPEND:
#if defined (__MSDOS__) || defined (_WIN32) || defined (__EMX__)
		    if (! tst_TXT(File))
		    	how = "ab+";
		    else
#endif
			how = "a+";
		    errstr = "File can not be extended";
		    break;
		default:
		    abort ();
		}
		m_FILNUM(File) = fopen (filename, how);

		/* Now, if the file could not be opened, but we only want
		   to read it, check if that is possible
		*/
		if (! m_FILNUM (File))
		  if (mode == M_READ)
		    {
#if defined (__MSDOS__) || defined (_WIN32) || defined (__EMX__)
		      if (! tst_TXT(File))
			m_FILNUM(File) = fopen(filename,"rb");
		      else
#endif
		        m_FILNUM(File) = fopen(filename,"r");
		      if (m_FILNUM(File))
			{
			  SET_STATUS (File, FiRONLY);
			  _p_warning ("File is read only");
			}
		    }
		  else if (mode == M_WRITE)
		    {
#if defined (__MSDOS__) || defined (_WIN32) || defined (__EMX__)
		      if (! tst_TXT(File))
			m_FILNUM(File) = fopen(filename,"wb");
		      else
#endif
		        m_FILNUM(File) = fopen(filename,"w");
		      if (m_FILNUM(File))
			{
			  SET_STATUS (File, FiWONLY);
			  _p_warning ("File is write only");
			}
		    }
		  else if (mode == M_APPEND)
		    {
#if defined (__MSDOS__) || defined (_WIN32) || defined (__EMX__)
		      if (! tst_TXT(File))
			m_FILNUM(File) = fopen(filename,"ab");
		      else
#endif
		        m_FILNUM(File) = fopen(filename,"a");
		      if (m_FILNUM(File))
			{
			  SET_STATUS (File, FiWONLY);
			  _p_warning ("File is write only");
			}
		    }
	      }

	    if (! m_FILNUM(File))
	      {
		if (m_BINDING (File))
		  {
		    _p_error (REPORT, "Could't open bound file. Doing `unbind(%s)'\n",
			      m_NAM(File));
		    _p_unbind (File);
		  }
		else if (filename)
		  {
		    (void) free (filename);
		    filename = (char *)NULL;
		  }

		_p_error(REPORT, errstr);
	      }
	    else
	      {
		if (!tst_EXT(File) && !m_BINDING (File))
		  {
		    extern char *Gpc_tmpname;
		    if (strncmp (filename,Gpc_tmpname, strlen(Gpc_tmpname)))
		      _p_generic(700);
		    else if (!Gpc_debug)
		      unlink(filename);
		  }
	      }
	  }
      }

    if (!m_BINDING (File) && filename && !m_EXTNAM(File))
      (void) free (filename);

    /* Clear file status bits */
    fil_clr (File, ~(STATUS_KEEP));

    set_UND(File);	     /* Mark the file buffer contents undefined */

    m_FISIZE(File) = 0;

    if (fstat(fileno(m_FILNUM(File)), &finfo) == 0)
      {
	if (mode != M_WRITE)
	  {
	    m_FISIZE(File) = NUMBYTE (File, finfo.st_size);
	    
	    if (! m_FISIZE(File))
	      set_EMPTY(File);
	  }

	if ((finfo.st_mode & S_IFMT) == S_IFCHR)
	  {
#ifdef HAVE_DEVNULL
	    if (finfo.st_rdev == Gpc_DEVNULL)
	      set_NUL(File);
	    else
#endif
	      if (isatty(fileno(m_FILNUM(File))))
		set_TTY(File);
	  }
      }

    _p_fdrchain(File, TRUE); /* Add to FDR chain */
}

/* pre-assertion: true.
 * post-assertion: (f.L = f.R = S()) and (f.M = Generation) and
 * (f^ is undefined)
 */
void
_p_rewrite (File, filename, length)
FDR File;
char *filename;
int length;
{
    _p_open (File, filename, M_WRITE, length);

    set_EOF(File);
    clr_EOLN(File);

    SET_STATUS(File, FiWRI);
    
    D(2, _p_printfdr(File,"REWRITE"));
}

/* pre-assertion: f0.L and f0.R are not undefined
 *
 * post-assertion: (f.M = Generation) and (f.L = f0.L~f0.R~X)
 *		   and (f.R = S())
 *		   and (f^ is undefined)
 *
 * where, if F is of type TEXT, and f0.L~f0.R is not empty and
 * if (f0.L~f0.R).last is not an end-of-line, then X shall be a sequence
 * having an end-of-line component as its only component;
 * otherwise X = S().
 */
void
_p_extend(File, filename, length)
     FDR   File;
     char *filename;
     int   length;
{
  _p_open (File, filename, M_APPEND, length);

  set_EOF(File);
  clr_EOLN(File);

  SET_STATUS(File, FiWRI);

  if (tst_TXT (File) && !tst_EMPTY(File))
    if (is_WONLY(File))
      _p_error (REPORT, "Write only text file `%s' append. Trailing EOLN not checked",
		m_NAM(File));
    else
      if (_p_seek (File, -1, 2, 0))
	_p_error (REPORT, "Can't check trailing EOLN when appending `%s'",
		  m_NAM(File));
      else
	{
	  int ch = getc (m_FILNUM (File));
	  
	  /* file pointer is now at EOF */
	  if (ch != NEWLINE && (putc(NEWLINE, m_FILNUM (File)) == EOF))
	    _p_error (ABORT, "Can't append implicit EOF to `%s' in append mode",
		      m_NAM (File));
	}
  D(1, _p_printfdr(File,"EXTEND"));
}

/* pre-assertion: The components f0.L and f0.R are not undefined
 * post-assertion: (f.L = S()) and (f.R = (f0.L~f0.R~X))
 * and (f.M = Inspection)
 * and (if f.R = S() then (f^ is undefined) else (f^ = f^.R.first))
 *
 * where, if F is of type TEXT, and f0.L~f0.R is not empty and
 * if (f0.L~f0.R).last is not an end-of-line, then X shall be a sequence
 * having an end-of-line component as its only component;
 * otherwise X = S().
 */
void
_p_reset(File, filename, length)
     FDR   File;
     char *filename;
     int   length;
{
    _p_open(File, filename, M_READ, length);

    clr_EOF(File);
    clr_EOLN(File);

    SET_STATUS(File, FiORE);

    /* Please do not remove this. It's here as an inside joke */
    D(1, fprintf(stderr,"Kukkuu RESET (TM)\n"));
    
    clr_UND (File);

    if (tst_TXT(File) && tst_TTY(File))
      {
	clr_EMPTY(File);

	/* This is implementation dependent! */
	
	if (tst_LAZY(File))
	  {
	    D(1, fprintf(stderr, "set_LGET lazy terminal\n"));
	    set_LGET (File);

	    if (_p_eoln_reset_hack)
	      set_EMPTY (File); /* Mark for EOLN; nothing has been read yet */
	  }
	else
	  {
	    D(1, fprintf(stderr, "Setting implicit EOLN to terminal\n"));
	    
	    m_FILBUF(File) = ' ';
	    set_EOLN(File);
	  }
      }
    else
      _p_get(File);	/* Read in the first buffer contents */
    
    D(2, _p_printfdr(File,"RESET"));
}

void
_p_close(File)
     FDR File;
{
  if (m_STATUS(File) == FiNOP)
    return;

  set_EOF(File);

  if (TST_STATUS(File, FiANY))
    {
      if (is_WRITABLE (File))
	fflush (m_FILNUM (File));

      fclose(m_FILNUM(File));
      m_FILNUM(File) = NULL;
    }

  if (m_EXTNAM(File))
    {
      if (! m_BINDING (File))
	(void) free (m_EXTNAM(File));

      m_EXTNAM (File) = NULL;
    }
  
  _p_fdrchain(File, FALSE); /* Remove from chain */
  
  _p_initfdr(File, m_NAM (File), 0,
	     (  tst_TXT(File)   << fkind_TEXT
	      | tst_PCK(File)   << fkind_PACKED
	      | tst_EXT(File)   << fkind_EXTERN
	      | tst_LAZY(File)  << fkind_LAZY
	      | tst_DIRECT(File)<< fkind_DIRECT
	      | tst_BYTE(File)  << fkind_BYTE
	      ));

  D(2, _p_printfdr(File, "CLOSED"));
}

#ifdef DEBUG
void
_p_printfdr(File,s)
FDR	File;
char	*s;
{

    if (s) fprintf(stderr,"%s: ",s);

    fprintf (stderr,"FDR of   %.10s\r\n",m_NAM(File));

    if (Gpc_debug > 2 || !s) {
      fprintf(stderr,"\tofnum  %d\r\n", (m_FILNUM(File) ?
					 fileno(m_FILNUM(File)) :
					 -1));
	fprintf(stderr,"\tUND    %d",  tst_UND(File));
	fprintf(stderr,"\tLAZY   %d",  tst_LAZY(File));
	fprintf(stderr,"\tEOF    %d",  tst_EOF(File));
	fprintf(stderr,"\tEOLN   %d\r\n",tst_EOLN(File));
	fprintf(stderr,"\tEXT    %d",  tst_EXT(File));
	fprintf(stderr,"\tPCK    %d",  tst_PCK(File));
	fprintf(stderr,"\tTXT    %d",  tst_TXT(File));
	fprintf(stderr,"\tEMPTY  %d\r\n",tst_EMPTY(File));
	fprintf(stderr,"\tEXT    %d",  tst_EXT(File));
	fprintf(stderr,"\tLGET   %d",  tst_LGET(File));
	fprintf(stderr,"\tDIRECT %d",  tst_DIRECT(File));
	fprintf(stderr,"\tEOFOK  %d",  tst_EOFOK(File));

	fprintf(stderr,"\r\n\tSTATUS 0x%x\r\n",m_STATUS(File));
	fprintf(stderr,"\tSIZ     %d\r\n",m_SIZ(File));
	fprintf(stderr,"\tExt name %s\r\n",m_EXTNAM(File) ? m_EXTNAM(File) : "<none>");
	fprintf(stderr,"\tElem    %d\r\n",m_FISIZE(File));  
        fprintf(stderr,"\tBinding 0x%lx\r\n", (long)m_BINDING(File));

	if (Gpc_debug > 3 || !s)
	  {
	    fprintf(stderr,"\tthis fdr adr: 0x%lx\r\n", (long)File);
	    fprintf(stderr,"\tnext fdr adr: 0x%lx\r\n", (long)m_NXTFDR(File));
	  }
	/* Output the file buffer contents if this is a text file */
	if (tst_TXT(File))
	  fprintf(stderr,"\tTXT file buffer: '%c' (0x%x)\r\n",
		  m_FILBUF(File), m_FILBUF(File));
        else if (m_SIZ(File) <= sizeof (long))
	  fprintf(stderr,"\tBIN file buffer: %ld (0x%lx)\r\n",
		  (long)m_FILBUF(File), (long)m_FILBUF(File));
    }
}
# endif DEBUG
