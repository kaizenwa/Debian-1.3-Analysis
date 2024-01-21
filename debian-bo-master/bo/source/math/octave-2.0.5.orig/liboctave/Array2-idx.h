// Template array classes
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

#include "Array-flags.h"
#include "idx-vector.h"
#include "lo-error.h"

template <class T>
Array2<T>
Array2<T>::value (void)
{
  Array2<T> retval;

  int n_idx = index_count ();

  if (n_idx == 2)
    {
      idx_vector *tmp = get_idx ();
      idx_vector idx_i = tmp[0];
      idx_vector idx_j = tmp[1];

      return index (idx_i, idx_j);
    }
  else if (n_idx == 1)
    {
      idx_vector *tmp = get_idx ();
      idx_vector idx = tmp[0];

      return index (idx);
    }
  else
    (*current_liboctave_error_handler)
      ("invalid number of indices for matrix expression");

  clear_index ();

  return retval;
}

template <class T>
Array2<T>
Array2<T>::index (idx_vector& idx) const
{
  Array2<T> retval;

  int nr = d1;
  int nc = d2;

  if (nr == 1 && nc == 1)
    {
      Array<T> tmp = Array<T>::index (idx);

      int len = tmp.length ();

      if (len == 0)
	retval = Array2<T> (0, 0);
      else
	{
	  if (liboctave_pcv_flag)
	    retval = Array2<T> (tmp, len, 1);
	  else
	    retval = Array2<T> (tmp, 1, len);
	}
    }
  else if (nr == 1 || nc == 1)
    {
      int result_is_column_vector = (nc == 1);

      if (liboctave_dfi_flag && idx.is_colon ())
	    result_is_column_vector = 1;

      Array<T> tmp = Array<T>::index (idx);

      int len = tmp.length ();

      if (len == 0)
	retval = Array2<T> (0, 0);
      else
	{
	  if (result_is_column_vector)
	    retval = Array2<T> (tmp, len, 1);
	  else
	    retval = Array2<T> (tmp, 1, len);
	}
    }
  else if (liboctave_dfi_flag)
    {
      // This code is only for indexing matrices.  The vector
      // cases are handled above.

      idx.freeze (nr * nc, "matrix", liboctave_pzo_flag);

      if (idx)
	{
	  int result_nr = idx.orig_rows ();
	  int result_nc = idx.orig_columns ();

	  if (idx.is_colon ())
	    {
	      result_nr = nr * nc;
	      result_nc = 1;
	    }
	  else if (idx.one_zero_only ())
	    {
	      result_nr = idx.ones_count ();
	      result_nc = (result_nr > 0 ? 1 : 0);
	    }

	  retval.resize (result_nr, result_nc);

	  int k = 0;
	  for (int j = 0; j < result_nc; j++)
	    {
	      for (int i = 0; i < result_nr; i++)
		{
		  int ii = idx.elem (k++);
		  int fr = ii % nr;
		  int fc = (ii - fr) / nr;
		  retval.elem (i, j) = elem (fr, fc);
		}
	    }
	}
      // idx_vector::freeze() printed an error message for us.
    }
  else
    (*current_liboctave_error_handler)
      ("single index only valid for row or column vector");

  return retval;
}

template <class T>
Array2<T>
Array2<T>::index (idx_vector& idx_i, idx_vector& idx_j) const
{
  Array2<T> retval;

  int nr = d1;
  int nc = d2;

  int n = idx_i.freeze (nr, "row", liboctave_pzo_flag);
  int m = idx_j.freeze (nc, "column", liboctave_pzo_flag);

  if (idx_i && idx_j)
    {
      if (idx_i.orig_empty () || idx_j.orig_empty ())
	{
	  retval.resize (n, m);
	}
      else if (n == 0)
	{
	  if (m == 0)
	    retval.resize (0, 0);
	  else if (idx_j.is_colon_equiv (nc, 1))
	    retval.resize (0, nc);
	  else
	    (*current_liboctave_error_handler) ("invalid row index = 0");
	}
      else if (m == 0)
	{
	  if (n == 0)
	    retval.resize (0, 0);
	  else if (idx_i.is_colon_equiv (nr, 1))
	    retval.resize (nr, 0);
	  else
	    (*current_liboctave_error_handler) ("invalid column index = 0");
	}
      else if (idx_i.is_colon_equiv (nr) && idx_j.is_colon_equiv (nc))
	{
	  retval = *this;
	}
      else
	{
	  retval.resize (n, m);

	  for (int j = 0; j < m; j++)
	    {
	      int jj = idx_j.elem (j);
	      for (int i = 0; i < n; i++)
		{
		  int ii = idx_i.elem (i);
		  retval.elem (i, j) = elem (ii, jj);
		}
	    }
	}
    }

  // idx_vector::freeze() printed an error message for us.

  return retval;
}

template <class T>
void
Array2<T>::maybe_delete_elements (idx_vector& idx_i, idx_vector& idx_j)
{
  int nr = d1;
  int nc = d2;

  if (nr == 0 && nc == 0)
    return;

  if (idx_i.is_colon_equiv (nr, 1))
    {
      if (idx_j.is_colon_equiv (nc, 1))
	resize (0, 0);
      else
	{
	  int num_to_delete = idx_j.length (nc);

	  if (num_to_delete != 0)
	    {
	      if (nr == 1 && num_to_delete == nc)
		resize (0, 0);
	      else
		{
		  int new_nc = nc;

		  int idx = 0;

		  for (int j = 0; j < nc; j++)
		    if (j == idx_j.elem (idx))
		      {
			idx++;
			new_nc--;
		      }

		  if (new_nc > 0)
		    {
		      T *new_data = new T [nr * new_nc];

		      int jj = 0;
		      idx = 0;
		      for (int j = 0; j < nc; j++)
			{
			  if (j == idx_j.elem (idx))
			    idx++;
			  else
			    {
			      for (int i = 0; i < nr; i++)
				new_data[nr*jj+i] = elem (i, j);
			      jj++;
			    }
			}

		      if (--rep->count <= 0)
			delete rep;

		      rep = new ArrayRep (new_data, nr * new_nc);

		      d2 = new_nc;

		      set_max_indices (2);
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("A(idx) = []: index out of range");
		}
	    }
	}
    }
  else if (idx_j.is_colon_equiv (nc, 1))
    {
      if (idx_i.is_colon_equiv (nr, 1))
	resize (0, 0);
      else
	{
	  int num_to_delete = idx_i.length (nr);

	  if (num_to_delete != 0)
	    {
	      if (nc == 1 && num_to_delete == nr)
		resize (0, 0);
	      else 
		{
		  int new_nr = nr;

		  int idx = 0;

		  for (int i = 0; i < nr; i++)
		    if (i == idx_i.elem (idx))
		      {
			idx++;
			new_nr--;
		      }

		  if (new_nr > 0)
		    {
		      T *new_data = new T [new_nr * nc];

		      int ii = 0;
		      idx = 0;
		      for (int i = 0; i < nr; i++)
			{
			  if (i == idx_i.elem (idx))
			    idx++;
			  else
			    {
			      for (int j = 0; j < nc; j++)
				new_data[new_nr*j+ii] = elem (i, j);
			      ii++;
			    }
			}

		      if (--rep->count <= 0)
			delete rep;

		      rep = new ArrayRep (new_data, new_nr * nc);

		      d1 = new_nr;

		      set_max_indices (2);
		    }
		  else
		    (*current_liboctave_error_handler)
		      ("A(idx) = []: index out of range");
		}
	    }
	}
    }
}

#define MAYBE_RESIZE_LHS \
  do \
    { \
      if (liboctave_rre_flag) \
	{ \
	  int max_row_idx = idx_i_is_colon ? rhs_nr : idx_i.max () + 1; \
	  int max_col_idx = idx_j_is_colon ? rhs_nc : idx_j.max () + 1; \
 \
	  int new_nr = max_row_idx > lhs_nr ? max_row_idx : lhs_nr; \
	  int new_nc = max_col_idx > lhs_nc ? max_col_idx : lhs_nc; \
 \
	  lhs.resize (new_nr, new_nc, 0.0); \
	} \
    } \
  while (0)

template <class LT, class RT>
int
assign (Array2<LT>& lhs, const Array2<RT>& rhs)
{
  int retval = 1;

  int n_idx = lhs.index_count ();

  int lhs_nr = lhs.rows ();
  int lhs_nc = lhs.cols ();

  int rhs_nr = rhs.rows ();
  int rhs_nc = rhs.cols ();

  if (n_idx == 2)
    {
      idx_vector *tmp = lhs.get_idx ();

      idx_vector idx_i = tmp[0];
      idx_vector idx_j = tmp[1];

      int n = idx_i.freeze (lhs_nr, "row", liboctave_pzo_flag,
			    liboctave_rre_flag);

      int m = idx_j.freeze (lhs_nc, "column", liboctave_pzo_flag,
			    liboctave_rre_flag);

      int idx_i_is_colon = idx_i.is_colon ();
      int idx_j_is_colon = idx_j.is_colon ();

      if (idx_i_is_colon)
	n = lhs_nr > 0 ? lhs_nr : rhs_nr;

      if (idx_j_is_colon)
	m = lhs_nc > 0 ? lhs_nc : rhs_nc;

      if (idx_i && idx_j)
	{
	  if (rhs_nr == 0 && rhs_nc == 0)
	    {
	      lhs.maybe_delete_elements (idx_i, idx_j);
	    }
	  else
	    {
	      if (rhs_nr == 1 && rhs_nc == 1 && n > 0 && m > 0)
		{
		  MAYBE_RESIZE_LHS;

		  RT scalar = rhs.elem (0, 0);

		  for (int j = 0; j < m; j++)
		    {
		      int jj = idx_j.elem (j);
		      for (int i = 0; i < n; i++)
			{
			  int ii = idx_i.elem (i);
			  lhs.elem (ii, jj) = scalar;
			}
		    }
		}
	      else if (n == rhs_nr && m == rhs_nc)
		{
		  MAYBE_RESIZE_LHS;

		  for (int j = 0; j < m; j++)
		    {
		      int jj = idx_j.elem (j);
		      for (int i = 0; i < n; i++)
			{
			  int ii = idx_i.elem (i);
			  lhs.elem (ii, jj) = rhs.elem (i, j);
			}
		    }
		}
	      else
		{
		  (*current_liboctave_error_handler)
    ("A(I, J) = X: X must be a scalar or the number of elements in I must");
		  (*current_liboctave_error_handler)
    ("match the number of rows in X and the number of elements in J must");
		  (*current_liboctave_error_handler)
    ("match the number of columns in X");

		  retval = 0;
		}
	    }
	}
      // idx_vector::freeze() printed an error message for us.
    }
  else if (n_idx == 1)
    {
      int lhs_is_empty = lhs_nr == 0 || lhs_nc == 0;

      if (lhs_is_empty || (lhs_nr == 1 && lhs_nc == 1))
	{
	  idx_vector *tmp = lhs.get_idx ();

	  idx_vector idx = tmp[0];

	  int lhs_len = lhs.length ();

	  int n = idx.freeze (lhs_len, 0, liboctave_pzo_flag,
			      liboctave_rre_flag);

	  if (idx)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		{
		  if (n != 0 && (lhs_nr != 0 || lhs_nc != 0))
		    {
		      idx_vector tmp (':');
		      lhs.maybe_delete_elements (idx, tmp);
		    }
		}
	      else if (! liboctave_dfi_flag && lhs_is_empty
		       && idx.is_colon ()
		       && ! (rhs_nr == 1 || rhs_nc == 1))
		{
		  (*current_liboctave_error_handler)
		    ("A(:) = X: X must be a vector");
		}
	      else
		{
		  if (assign ((Array<LT>&) lhs, (Array<RT>&) rhs))
		    {
		      int len = lhs.length ();

		      if (len > 0)
			{
			  int idx_nr = idx.orig_rows ();
			  int idx_nc = idx.orig_columns ();

			  // lhs_is_empty now means that lhs was
			  // *originally* empty.

			  if (liboctave_dfi_flag
			      || (idx_nr == 1 && idx_nc == 1))
			    {
			      if (liboctave_pcv_flag)
				{
				  lhs.d1 = lhs.length ();
				  lhs.d2 = 1;
				}
			      else
				{
				  lhs.d1 = 1;
				  lhs.d2 = lhs.length ();
				}
			    }
			  else if (lhs_is_empty && idx.is_colon ())
			    {
			      lhs.d1 = rhs.d1;
			      lhs.d2 = rhs.d2;
			    }
			  else if (idx_nr == 1 && rhs_nr == 1)
			    {
			      lhs.d1 = 1;
			      lhs.d2 = lhs.length ();
			    }
			  else if (idx_nc == 1 && rhs_nc == 1)
			    {
			      lhs.d1 = lhs.length ();
			      lhs.d2 = 1;
			    }
			  else
			    (*current_liboctave_error_handler)
      ("A(I) = X: X must be a scalar or a matrix with the same size as I");
			}
		      else
			{
			  lhs.d1 = 0;
			  lhs.d2 = 0;
			}
		    }
		  else
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (lhs_nr == 1)
	{
	  idx_vector *tmp = lhs.get_idx ();

	  idx_vector idx = tmp[0];

	  idx.freeze (lhs_nc, "vector", liboctave_pzo_flag,
		      liboctave_rre_flag);

	  if (idx)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		{
		  idx_vector tmp (':');
		  lhs.maybe_delete_elements (tmp, idx);
		}
	      else
		{
		  if (assign ((Array<LT>&) lhs, (Array<RT>&) rhs))
		    lhs.d2 = lhs.length ();
		  else
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (lhs_nc == 1)
	{
	  idx_vector *tmp = lhs.get_idx ();

	  idx_vector idx = tmp[0];

	  idx.freeze (lhs_nr, "vector", liboctave_pzo_flag,
		      liboctave_rre_flag);

	  if (idx)
	    {
	      if (rhs_nr == 0 && rhs_nc == 0)
		{
		  idx_vector tmp (':');
		  lhs.maybe_delete_elements (idx, tmp);
		}
	      else
		{
		  if (assign ((Array<LT>&) lhs, (Array<RT>&) rhs))
		    lhs.d1 = lhs.length ();
		  else
		    retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else if (liboctave_dfi_flag)
	{
	  idx_vector *tmp = lhs.get_idx ();
	  idx_vector idx = tmp[0];

	  int len = idx.freeze (lhs_nr * lhs_nc, "matrix",
				liboctave_pzo_flag);

	  if (idx)
	    {
	      if (len == rhs_nr * rhs_nc)
		{
		  int k = 0;
		  for (int j = 0; j < rhs_nc; j++)
		    {
		      for (int i = 0; i < rhs_nr; i++)
			{
			  int ii = idx.elem (k++);
			  int fr = ii % lhs_nr;
			  int fc = (ii - fr) / lhs_nr;
			  lhs.elem (fr, fc) = rhs.elem (i, j);
			}
		    }
		}
	      else
		{
		  (*current_liboctave_error_handler)
      ("A(I) = X: X must be a scalar or a matrix with the same size as I");

		  retval = 0;
		}
	    }
	  // idx_vector::freeze() printed an error message for us.
	}
      else
	{
	  (*current_liboctave_error_handler)
	    ("single index only valid for row or column vector");

	  retval = 0;
	}
    }
  else
    {
      (*current_liboctave_error_handler)
	("invalid number of indices for matrix expression");

      retval = 0;
    }

  lhs.clear_index ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
