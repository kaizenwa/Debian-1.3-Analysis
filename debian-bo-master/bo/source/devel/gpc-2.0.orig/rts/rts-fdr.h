/* Copyright (C) 1991,1993 Free Software Foundation, Inc.

   This file is part of GNU Pascal Library.

   File Descriptor Record definition.

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

/* This version does not do internal
 * buffering of its own
 *
 * no files are actually packed
 *
 * The compiler must pass the address of the FDR struct to the
 * RTS routines that needs files.
 */

/* RtsSta bit definitions */

#define FiNOP    0 /* File has not been opened */
#define FiRONLY  1 /* File opened but is read only */
#define FiORE    2 /* File open for reading */
#define FiWRI    4 /* File open for writing */
#define FiRND    8 /* File open for random access */
#define FiWONLY 16 /* File opened but is write only */
                   /* RtsSta: Device specific bits */
#define FiTTY	1024	/* /dev/tty* flush output before GET */
#define FiNUL   2048	/* /dev/null */
#define FiPIPE  4096	/* Output into pipe/Read from pipe (not implemented) */

/* m_STATUS bits altered by Seek* routines */
#define FiANY (FiORE | FiWRI | FiRND)

/* m_FILSTA bits that must be preserved by the run time system
 * when files are manipulated.
 */
#define STATUS_KEEP (FiTxt | FiExt | FiPck | FiLzy | FiDacc | FiByte)

/* some macro definitions */

#define m_FILBUF(file)	(*((file)->FilBuf))
#define m_FILBPTR(file)	((file)->FilBuf)
#define m_FILNUM(file)  ((file)->FilJfn)
#define m_FISIZE(file)  ((file)->FilElem)
#define m_NXTFDR(file)  ((file)->NxtFdr)
#define m_SIZ(file)	((file)->FilSiz)
#define m_NAM(file)	((file)->FilNam)
#define m_EXTNAM(file)	((file)->ExtNam)
#define m_FILSTA(file)	((file)->FilSta)
#define m_STATUS(file)	((file)->RtsSta)
#define m_BINDING(file)	((BINDING *)((file)->Binding))

#define fil_tst(File, bit)	(!!(m_FILSTA(File) &  (bit)))
#define fil_set(File, bit)	(m_FILSTA(File)   |=  (bit))
#define fil_clr(File, bit)	(m_FILSTA(File)   &= ~(bit))

#define tst_UND(file)	 fil_tst (file, FiUnd)
#define tst_LAZY(file)	 fil_tst (file, FiLzy)
#define tst_EOF(file)	 fil_tst (file, FiEof)
#define tst_EOLN(file)	 fil_tst (file, FiEln)
#define tst_EOFOK(file)  fil_tst (file, FiEofOK)
#define tst_TXT(file)	 fil_tst (file, FiTxt)
#define tst_EXT(file)	 fil_tst (file, FiExt)
#define tst_PCK(file)	 fil_tst (file, FiPck)
#define tst_EMPTY(file)  fil_tst (file, FiClr)
#define tst_DIRECT(file) fil_tst (file, FiDacc)
#define tst_LGET(file)   fil_tst (file, FiLget)
#define tst_BYTE(file)   fil_tst (file, FiByte)

#define set_UND(file)	 fil_set (file, FiUnd)
#define set_LAZY(file)	 fil_set (file, FiLzy)
#define set_EOF(file)	 fil_set (file, FiEof)
#define set_EOLN(file)	 fil_set (file, FiEln)
#define set_EOFOK(file)  fil_set (file, FiEofOK)
#define set_TXT(file)	 fil_set (file, FiTxt)
#define set_EXT(file)	 fil_set (file, FiExt)
#define set_PCK(file)	 fil_set (file, FiPck)
#define set_EMPTY(file)  fil_set (file, FiClr)
#define set_DIRECT(file) fil_set (file, FiDacc)
#define set_LGET(file)   fil_set (file, FiLget)
#define set_BYTE(file)   fil_set (file, FiByte)

#define clr_UND(file)	 fil_clr (file, FiUnd)
#define clr_LAZY(file)	 fil_clr (file, FiLzy)
#define clr_EOF(file)	 fil_clr (file, FiEof)
#define clr_EOLN(file)	 fil_clr (file, FiEln)
#define clr_EOFOK(file)  fil_clr (file, FiEofOK)
#define clr_TXT(file)	 fil_clr (file, FiTxt)
#define clr_EXT(file)	 fil_clr (file, FiExt)
#define clr_PCK(file)	 fil_clr (file, FiPck)
#define clr_EMPTY(file)  fil_clr (file, FiClr)
#define clr_DIRECT(file) fil_clr (file, FiDacc)
#define clr_LGET(file)   fil_clr (file, FiLget)
#define clr_BYTE(file)   fil_clr (file, FiByte)

/* TTY & NUL are never cleared in an open file */
#define tst_TTY(file)	TST_STATUS(file, FiTTY)
#define tst_NUL(file)	TST_STATUS(file, FiNUL)

#define set_TTY(file)	SET_STATUS(file, FiTTY)
#define set_NUL(file)	SET_STATUS(file, FiNUL)


#define is_READABLE(file) TST_STATUS(file, FiORE | FiRONLY | FiRND)
#define is_WRITABLE(file) TST_STATUS(file, FiWRI | FiWONLY | FiRND)

#define is_RONLY(file) TST_STATUS(file, FiRONLY)
#define is_WONLY(file) TST_STATUS(file, FiWONLY)

#define ok_EOF(file)   (!tst_EOF(file) || _p_generic(16))
#define ok_READ(file)  (is_READABLE(file) || _p_generic(14))
#define ok_WRITE(file) ((is_WRITABLE(file) || _p_generic(9)) && !tst_NUL(file))

/* Clear Test and Set status bits in Rts Status word */
#define CLR_STATUS(file,bit)	(m_STATUS(file) &= ~(bit))
#define TST_STATUS(file,bit)	(m_STATUS(file) &   (bit))
#define SET_STATUS(file,bit)	(m_STATUS(file) |=  (bit))

/* BYTENUM calculates the byte where NumE'th element starts in file.
   First possible element is 1. */
#define BYTENUM(File, NumE)     (tst_PCK(File) ? ((NumE)-1) / (8 / m_SIZ(File)) : \
				 ((NumE) * m_SIZ(File)))

/* Opposite of BYTENUM. Calculates the number of Pascal file component
   the byte is in.
 */
#define NUMBYTE(File, NumBytes) (tst_PCK(File) ? ((8 / m_SIZ(File)) * NumBytes) : \
				 (NumBytes / m_SIZ(File)))

/* Pointers to the FDR chain */
extern FDR LastFdr;
extern FDR FirstFdr;

/* mode bits for _p_open() */
#define M_READ		1
#define M_WRITE		2
#define M_APPEND	4
#define M_UPDATE	8
