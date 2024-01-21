C****************************************************************************
C* NCSA HDF                                                                 *
C* Software Development Group                                               *
C* National Center for Supercomputing Applications                          *
C* University of Illinois at Urbana-Champaign                               *
C* 605 E. Springfield, Champaign IL 61820                                   *
C*                                                                          *
C* For conditions of distribution and use, see the accompanying             *
C* hdf/COPYING file.                                                        *
C*                                                                          *
C****************************************************************************
C
C mfanff.f,v 1.3 1996/01/11 20:55:13 georgev Exp
C
C------------------------------------------------------------------------------
C File:     mfanff.f
C Purpose:  Fortran stub routines for multi-file annotations
C Invokes:  mfanf.c for C-stubs
C Contents: 
C           afstart   -- start annotation access on file and return file
C                        handle
C------------------------------------------------------------------------------

C------------------------------------------------------------------------------
C Name:    afstart
C Purpose: start annotation access on file and return file handle
C Inputs:  filename: name of HDF file
C          accmode:  access mode
C Returns: file handle on success, -1 on failure 
C Users:   HDF users, utilities, other routines
C Invokes: acstart
C------------------------------------------------------------------------------

C     integer function afstart(fname, accmode)
C      character *(*) fname
C      integer accmode, acstart

C      afstart = acstart(fname, accmode, len(fname))
C      return
C      end

