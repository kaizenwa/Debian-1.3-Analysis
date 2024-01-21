.TH SLAMCH l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH SLAMCH l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLAMCH - determine single precision machine parameters
.SH SYNOPSIS
.TP 14
REAL FUNCTION
SLAMCH( CMACH )
.TP 14
.ti +4
CHARACTER
CMACH
.SH PURPOSE
SLAMCH determines single precision machine parameters.

.SH ARGUMENTS
.TP 8
CMACH   (input) CHARACTER*1
Specifies the value to be returned by SLAMCH:
.br
= 'E' or 'e',   SLAMCH := eps
.br
= 'S' or 's ,   SLAMCH := sfmin
.br
= 'B' or 'b',   SLAMCH := base
.br
= 'P' or 'p',   SLAMCH := eps*base
.br
= 'N' or 'n',   SLAMCH := t
.br
= 'R' or 'r',   SLAMCH := rnd
.br
= 'M' or 'm',   SLAMCH := emin
.br
= 'U' or 'u',   SLAMCH := rmin
.br
= 'L' or 'l',   SLAMCH := emax
.br
= 'O' or 'o',   SLAMCH := rmax

where
.TP 6
eps   = relative machine precision
sfmin = safe minimum, such that 1/sfmin does not overflow
base  = base of the machine
prec  = eps*base
t     = number of (base) digits in the mantissa
rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
emin  = minimum exponent before (gradual) underflow
rmin  = underflow threshold - base**(emin-1)
emax  = largest exponent before overflow
rmax  = overflow threshold  - (base**emax)*(1-eps)
