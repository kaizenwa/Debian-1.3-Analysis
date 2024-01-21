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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cmath>
#include <cstring>

#include <strstream.h>

#ifndef NPSOL_MISSING

#include "NPSOL.h"
#include "dMatrix.h"
#include "f77-fcn.h"
#include "sun-utils.h"

extern "C"
{
  int F77_FCN (npoptn, NPOPTN) (const char*, long);

  int F77_FCN (npsol, NPSOL) (int&, int&, int&, int&, int&, int&,
			      double*, double*, double*,
			      int (*)(int&, const int&, const int&,
				      const int&, int*, double*,
				      double*, double*, int*),
			      int (*)(int&, const int&, double*,
				      double*, double*, int*),
			      int&, int&, int*, double*, double*,
			      double*, double&, double*, double*,
			      double*, int*, int&, double*, int&);
}

// XXX FIXME XXX -- would be nice to not have to have this global
// variable.
// Nonzero means an error occurred in the calculation of the objective
// function, and the user wants us to quit.
int npsol_objective_error = 0;

static Objective::objective_fcn user_phi;
static Objective::gradient_fcn user_grad;
static NLFunc::nonlinear_fcn user_g;
static NLFunc::jacobian_fcn user_jac;

int
npsol_objfun (int& mode, const int& n, double *xx, double *objf,
	      double *objgrd, int *)
{
  ColumnVector tmp_x (n);

  npsol_objective_error = 0;

  for (int i = 0; i < n; i++)
    tmp_x.elem (i) = xx[i];

  if (mode == 0 || mode == 2)
    {
      double value = (*user_phi) (tmp_x);

      if (npsol_objective_error)
	{
	  mode = -1;
	  return 0;
	}

#if defined (sun) && defined (__GNUC__)
      assign_double (objf, value);
#else
      *objf = value;
#endif
    }

  if ((mode == 1 || mode == 2) && user_grad)
    {
      ColumnVector tmp_grad (n);

      tmp_grad = (*user_grad) (tmp_x);

      if (tmp_grad.length () == 0)
	mode = -1;
      else
	{
	  for (int i = 0; i < n; i++)
	    objgrd[i] = tmp_grad.elem (i);
	}
    }

  return 0;
}

int
npsol_confun (int& mode, const int& ncnln, const int& n,
	      const int& nrowj, int *, double *xx, double *cons,
	      double *cjac, int *)
{
  ColumnVector tmp_x (n);
  ColumnVector tmp_c (ncnln);

  for (int i = 0; i < n; i++)
    tmp_x.elem (i) = xx[i];

  tmp_c = (*user_g) (tmp_x);

  if (tmp_c.length () == 0)
    {
      mode = -1;
      return 0;
    }
  else
    {
      for (int i = 0; i < ncnln; i++)
	cons[i] = tmp_c.elem (i);
    }

  if (user_jac)
    {
      Matrix tmp_jac (ncnln, n);

      tmp_jac = (*user_jac) (tmp_x);

      if (tmp_jac.rows () == 0 || tmp_jac.columns () == 0)
	mode = -1;
      else
	{
	  int ld = nrowj;
	  for (int j = 0; j < n; j++)
	    for (int i = 0; i < ncnln; i++)
	      cjac[i+j*ld] = tmp_jac (i, j);
	}
    }

  return 0;
}

ColumnVector
NPSOL::do_minimize (double& objf, int& inform, ColumnVector& lambda)
{
  // Dimensions of various things.

  int n     = x.capacity ();
  int nclin = lc.size ();
  int ncnln = nlc.size ();
  int nrowa = 1 > nclin ? 1 : nclin;
  int nrowj = 1 > ncnln ? 1 : ncnln;
  int nrowr = n;

  // Informative stuff.

  int iter;

  Array<int> aistate (n+nclin+ncnln);
  int *istate = aistate.fortran_vec ();

  // User defined function stuff is defined above in the functions
  // npsol_confun() and npsol_objfun();

  // Constraint stuff.

  double bigbnd = infinite_bound ();

  Matrix clin = lc.constraint_matrix ();
  double *pclin = clin.fortran_vec ();

  ColumnVector aclow (n+nclin+ncnln);
  ColumnVector acup (n+nclin+ncnln);

  if (bnds.size () > 0)
    {
      aclow.insert (bnds.lower_bounds (), 0);
      acup.insert (bnds.upper_bounds (), 0);
    }
  else
    {
      aclow.fill (-bigbnd, 0, n-1);
      acup.fill (bigbnd, 0, n-1);
    }

  if (nclin > 0)
    {
      aclow.insert (lc.lower_bounds (), n);
      acup.insert (lc.upper_bounds (), n);
    }

  if (ncnln > 0)
    {
      aclow.insert (nlc.lower_bounds (), n+nclin);
      acup.insert (nlc.upper_bounds (), n+nclin);
    }

  double *clow = aclow.fortran_vec ();
  double *cup = acup.fortran_vec ();

  Array<double> ac (ncnln);
  double *c = ac.fortran_vec ();

  Array<double> acjac (nrowj*n);
  double *cjac = acjac.fortran_vec ();

  // Objective stuff.

  Array<double> aobjgrd (n);
  double *objgrd = aobjgrd.fortran_vec ();

  // Other stuff.

  Array<double> ar (n*n);
  double *r = ar.fortran_vec ();

  lambda.resize (n+nclin+ncnln);
  double *pclambda = lambda.fortran_vec ();

  // Decision variable stuff.

  double *px = x.fortran_vec ();

  // Workspace parameters.

  int lenw;
  int leniw = 3 * n + nclin + 2 * ncnln;
  if (nclin == 0 && ncnln == 0)
    lenw = 20*n;
  else if (ncnln == 0)
    lenw = 2*n*(10 + n) + 11*nclin;
  else
    lenw = 2*n*(n + 10) + nclin*(n + 11) + ncnln*(2*n + 21);

  Array<int> aiw (leniw);
  int *iw = aiw.fortran_vec ();

  Array<double> aw (lenw);
  double *w = aw.fortran_vec ();

  user_phi  = phi.objective_function ();
  user_grad = phi.gradient_function ();
  user_g    = nlc.function ();
  user_jac  = nlc.jacobian_function ();

  pass_options_to_npsol ();

  if (! user_jac && ! user_grad)
    F77_FCN (npoptn, NPOPTN) ("Derivative Level 0", 18L);
  else if (! user_jac && user_grad)
    F77_FCN (npoptn, NPOPTN) ("Derivative Level 1", 18L);
  else if (user_jac && ! user_grad)
    F77_FCN (npoptn, NPOPTN) ("Derivative Level 2", 18L);
  else if (user_jac && user_grad)
    F77_FCN (npoptn, NPOPTN) ("Derivative Level 3", 18L);

  attempt = 0;
  while (attempt++ < 5)
    {

      F77_XFCN (npsol, NPSOL,
		(n, nclin, ncnln, nrowa, nrowj, nrowr, pclin, clow,
		 cup, npsol_confun, npsol_objfun, inform, iter,
		 istate, c, cjac, pclambda, objf, objgrd, r, px, iw,
		 leniw, w, lenw));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in npsol");
      else if (inform == 6 || inform == 1)
	continue;
      else
	break;
    }

  return x;
}

void
NPSOL_options::init (void)
{
  x_central_difference_interval = -1.0;
  x_crash_tolerance = 0.1;
  x_difference_interval = -1.0;
  x_function_precision = pow (DBL_EPSILON, 0.9);
  x_infinite_bound = 1.0e+30;
  x_infinite_step = 1.0e+30;
  x_linear_feasibility_tolerance = sqrt (DBL_EPSILON);
  x_linesearch_tolerance = 0.9;
  x_nonlinear_feasibility_tolerance = sqrt (DBL_EPSILON);
  x_optimality_tolerance = pow (DBL_EPSILON, 0.8);
  x_derivative_level = 0;
  x_major_iteration_limit = -1;
  x_minor_iteration_limit = -1;
  x_major_print_level = 0;
  x_minor_print_level = 0;
  x_start_objective_check = 1;
  x_start_constraint_check = 1;
  x_stop_objective_check = -1;
  x_stop_constraint_check = -1;
  x_verify_level = 0;
}

void
NPSOL_options::set_options (const NPSOL_options& opt)
{
  x_central_difference_interval = opt.x_central_difference_interval;
  x_crash_tolerance = opt.x_crash_tolerance;
  x_difference_interval = opt.x_difference_interval;
  x_function_precision = opt.x_function_precision;
  x_infinite_bound = opt.x_infinite_bound;
  x_infinite_step = opt.x_infinite_step;
  x_linear_feasibility_tolerance = opt.x_linear_feasibility_tolerance;
  x_linesearch_tolerance = opt.x_linesearch_tolerance;
  x_nonlinear_feasibility_tolerance = opt.x_nonlinear_feasibility_tolerance;
  x_optimality_tolerance = opt.x_optimality_tolerance;
  x_derivative_level = opt.x_derivative_level;
  x_major_iteration_limit = opt.x_major_iteration_limit;
  x_minor_iteration_limit = opt.x_minor_iteration_limit;
  x_major_print_level = opt.x_major_print_level;
  x_minor_print_level = opt.x_minor_print_level;
  x_start_objective_check = opt.x_start_objective_check;
  x_start_constraint_check = opt.x_start_constraint_check;
  x_stop_objective_check = opt.x_stop_objective_check;
  x_stop_constraint_check = opt.x_stop_constraint_check;
  x_verify_level = opt.x_verify_level;
}

void
NPSOL_options::pass_options_to_npsol (void)
{
  F77_FCN (npoptn, NPOPTN) ("Nolist", 6L);
  F77_FCN (npoptn, NPOPTN) ("Defaults", 8L);

  if (x_central_difference_interval > 0.0)
    set_option ("Central Difference", x_central_difference_interval);

  set_option ("Crash Tolerance", x_crash_tolerance);

  if (x_difference_interval > 0.0)
    set_option ("Difference Interval", x_difference_interval);

  set_option ("Function Precision", x_function_precision);

  set_option ("Infinite Bound", x_infinite_bound);

  set_option ("Infinite Step", x_infinite_step);

  set_option ("Linear Feasibility", x_linear_feasibility_tolerance);

  set_option ("Linesearch Tolerance", x_linesearch_tolerance);

  set_option ("Nonlinear Feasibility", x_nonlinear_feasibility_tolerance);

  set_option ("Optimality Tolerance", x_optimality_tolerance);

  set_option ("Derivative Level", x_derivative_level);

  if (x_major_iteration_limit > 0)
    set_option ("Major Iteration", x_major_iteration_limit);

  if (x_minor_iteration_limit > 0)
    set_option ("Minor Iteration", x_minor_iteration_limit);

  set_option ("Major Print", x_major_print_level);

  set_option ("Minor Print", x_minor_print_level);

  set_option ("Start Objective", x_start_objective_check);

  set_option ("Start Constraint", x_start_constraint_check);

  if (x_stop_objective_check > 0)
    set_option ("Stop Objective", x_stop_objective_check);

  if (x_stop_constraint_check > 0)
    set_option ("Stop Constraint", x_stop_constraint_check);

  set_option ("Verify Level", x_verify_level);
}

void
NPSOL_options::set_option (const string& key, int opt)
{
  ostrstream buf;
  buf << key << " " << opt << ends;
  char *command = buf.str ();
  size_t len = strlen (command);
  F77_FCN (npoptn, NPOPTN) (command, (long) len);
  delete [] command;
}

void
NPSOL_options::set_option (const string& key, double opt)
{
  ostrstream buf;
  buf << key << " " << opt << ends;
  char *command = buf.str ();
  size_t len = strlen (command);
  F77_FCN (npoptn, NPOPTN) (command, (long) len);
  delete [] command;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
