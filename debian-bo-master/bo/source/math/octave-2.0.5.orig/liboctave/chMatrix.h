/*

Copyright (C) 1996 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_chMatrix_int_h)
#define octave_chMatrix_int_h 1

#if defined (__GNUG__)
#pragma interface
#endif

// For FILE...
#include <cstdio>

#include <string>

#include "MArray2.h"

#include "mx-defs.h"
#include "str-vec.h"

class
charMatrix : public MArray2<char>
{
friend class ComplexMatrix;

public:

  charMatrix (void) : MArray2<char> () { }
  charMatrix (int r, int c) : MArray2<char> (r, c) { }
  charMatrix (int r, int c, char val) : MArray2<char> (r, c, val) { }
  charMatrix (const MArray2<char>& a) : MArray2<char> (a) { }
  charMatrix (const charMatrix& a) : MArray2<char> (a) { }
  charMatrix (const char *s);
  charMatrix (const string& s);
  charMatrix (const string_vector& s);

  charMatrix& operator = (const charMatrix& a)
    {
      MArray2<char>::operator = (a);
      return *this;
    }

  bool operator == (const charMatrix& a) const;
  bool operator != (const charMatrix& a) const;

  // destructive insert/delete/reorder operations

  charMatrix& insert (const char *s, int r, int c);
  charMatrix& insert (const charMatrix& a, int r, int c);

  string row_as_string (int r, bool strip_trailing_whitespace = false) const;

#if 0
  Matrix& insert (const RowVector& a, int r, int c);
  Matrix& insert (const ColumnVector& a, int r, int c);
  Matrix& insert (const DiagMatrix& a, int r, int c);

  Matrix& fill (char val);
  Matrix& fill (char val, int r1, int c1, int r2, int c2);

  Matrix append (const Matrix& a) const;
  Matrix append (const RowVector& a) const;
  Matrix append (const ColumnVector& a) const;
  Matrix append (const DiagMatrix& a) const;

  Matrix stack (const Matrix& a) const;
  Matrix stack (const RowVector& a) const;
  Matrix stack (const ColumnVector& a) const;
  Matrix stack (const DiagMatrix& a) const;
#endif

  charMatrix transpose (void) const;

#if 0
  friend Matrix real (const ComplexMatrix& a);
  friend Matrix imag (const ComplexMatrix& a);

  // resize is the destructive equivalent for this one

  Matrix extract (int r1, int c1, int r2, int c2) const;

  // extract row or column i.

  RowVector row (int i) const;
  RowVector row (char *s) const;

  ColumnVector column (int i) const;
  ColumnVector column (char *s) const;

  Matrix inverse (void) const;
  Matrix inverse (int& info) const;
  Matrix inverse (int& info, double& rcond) const;

  Matrix pseudo_inverse (double tol = 0.0);

  ComplexMatrix fourier (void) const;
  ComplexMatrix ifourier (void) const;

  ComplexMatrix fourier2d (void) const;
  ComplexMatrix ifourier2d (void) const;

  DET determinant (void) const;
  DET determinant (int& info) const;
  DET determinant (int& info, double& rcond) const;

  Matrix solve (const Matrix& b) const;
  Matrix solve (const Matrix& b, int& info) const;
  Matrix solve (const Matrix& b, int& info, double& rcond) const;

  ComplexMatrix solve (const ComplexMatrix& b) const;
  ComplexMatrix solve (const ComplexMatrix& b, int& info) const;
  ComplexMatrix solve (const ComplexMatrix& b, int& info, double& rcond) const;

  ColumnVector solve (const ColumnVector& b) const;
  ColumnVector solve (const ColumnVector& b, int& info) const;
  ColumnVector solve (const ColumnVector& b, int& info, double& rcond) const;

  ComplexColumnVector solve (const ComplexColumnVector& b) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info,
			     double& rcond) const;

  Matrix lssolve (const Matrix& b) const;
  Matrix lssolve (const Matrix& b, int& info) const;
  Matrix lssolve (const Matrix& b, int& info, int& rank) const;

  ComplexMatrix lssolve (const ComplexMatrix& b) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info,
			 int& rank) const;

  ColumnVector lssolve (const ColumnVector& b) const;
  ColumnVector lssolve (const ColumnVector& b, int& info) const;
  ColumnVector lssolve (const ColumnVector& b, int& info, int& rank) const;

  ComplexColumnVector lssolve (const ComplexColumnVector& b) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info,
			       int& rank) const;

  Matrix& operator += (const Matrix& a);
  Matrix& operator -= (const Matrix& a);

  Matrix& operator += (const DiagMatrix& a);
  Matrix& operator -= (const DiagMatrix& a);

  // unary operations

  Matrix operator ! (void) const;

  // column vector by row vector -> matrix operations

  friend Matrix operator * (const ColumnVector& a, const RowVector& a);

  // diagonal matrix by scalar -> matrix operations

  friend Matrix operator + (const DiagMatrix& a, double s);
  friend Matrix operator - (const DiagMatrix& a, double s);

  // scalar by diagonal matrix -> matrix operations

  friend Matrix operator + (double s, const DiagMatrix& a);
  friend Matrix operator - (double s, const DiagMatrix& a);

  // matrix by diagonal matrix -> matrix operations

  friend Matrix operator + (const Matrix& a, const DiagMatrix& b);
  friend Matrix operator - (const Matrix& a, const DiagMatrix& b);
  friend Matrix operator * (const Matrix& a, const DiagMatrix& b);

  // diagonal matrix by matrix -> matrix operations

  friend Matrix operator + (const DiagMatrix& a, const Matrix& b);
  friend Matrix operator - (const DiagMatrix& a, const Matrix& b);
  friend Matrix operator * (const DiagMatrix& a, const Matrix& b);

  // matrix by matrix -> matrix operations

  friend Matrix operator * (const Matrix& a, const Matrix& b);

  // other operations

  friend Matrix map (d_d_Mapper f, const Matrix& a);
  friend Matrix map (d_c_Mapper f, const ComplexMatrix& a);

  void map (d_d_Mapper f);

  Matrix all (void) const;
  Matrix any (void) const;

  Matrix cumprod (void) const;
  Matrix cumsum (void) const;
  Matrix prod (void) const;
  Matrix sum (void) const;
  Matrix sumsq (void) const;

  ColumnVector diag (void) const;
  ColumnVector diag (int k) const;

  ColumnVector row_min (void) const;
  ColumnVector row_min_loc (void) const;

  ColumnVector row_max (void) const;
  ColumnVector row_max_loc (void) const;

  RowVector column_min (void) const;
  RowVector column_min_loc (void) const;

  RowVector column_max (void) const;
  RowVector column_max_loc (void) const;

  // i/o

  friend ostream& operator << (ostream& os, const Matrix& a);
  friend istream& operator >> (istream& is, Matrix& a);

  int read (FILE *fptr, const char *type);
  int write (FILE *fptr, const char *type);
#endif

private:

  charMatrix (char *ch, int r, int c) : MArray2<char> (ch, r, c) { }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
