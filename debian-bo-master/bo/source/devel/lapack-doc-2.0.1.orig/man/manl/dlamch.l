.TH DLAMCH l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH DLAMCH l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
DLAMCH - determine double precision machine parameters
.SH SYNOPSIS
.TP 17
DOUBLE PRECISION
FUNCTION DLAMCH( CMACH )
.TP 17
.ti +4
CHARACTER
CMACH
.SH PURPOSE
DLAMCH determines double precision machine parameters.

.SH ARGUMENTS
.TP 8
CMACH   (input) CHARACTER*1
Specifies the value to be returned by DLAMCH:
.br
= 'E' or 'e',   DLAMCH := eps
.br
= 'S' or 's ,   DLAMCH := sfmin
.br
= 'B' or 'b',   DLAMCH := base
.br
= 'P' or 'p',   DLAMCH := eps*base
.br
= 'N' or 'n',   DLAMCH := t
.br
= 'R' or 'r',   DLAMCH := rnd
.br
= 'M' or 'm',   DLAMCH := emin
.br
= 'U' or 'u',   DLAMCH := rmin
.br
= 'L' or 'l',   DLAMCH := emax
.br
= 'O' or 'o',   DLAMCH := rmax

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
