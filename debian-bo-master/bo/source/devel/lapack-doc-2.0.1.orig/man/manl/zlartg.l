.TH ZLARTG l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH ZLARTG l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
ZLARTG - generate a plane rotation so that   [ CS SN ] [ F ] [ R ]  [ __ ] 
.SH SYNOPSIS
.TP 19
SUBROUTINE ZLARTG(
F, G, CS, SN, R )
.TP 19
.ti +4
DOUBLE
PRECISION CS
.TP 19
.ti +4
COMPLEX*16
F, G, R, SN
.SH PURPOSE
ZLARTG generates a plane rotation so that
   [ -SN  CS  ]     [ G ]     [ 0 ]
.br

This is a faster version of the BLAS1 routine ZROTG, except for
the following differences:
.br
   F and G are unchanged on return.
.br
   If G=0, then CS=1 and SN=0.
.br
   If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
      floating point operations.
.br

.SH ARGUMENTS
.TP 8
F       (input) COMPLEX*16
The first component of vector to be rotated.
.TP 8
G       (input) COMPLEX*16
The second component of vector to be rotated.
.TP 8
CS      (output) DOUBLE PRECISION
The cosine of the rotation.
.TP 8
SN      (output) COMPLEX*16
The sine of the rotation.
.TP 8
R       (output) COMPLEX*16
The nonzero component of the rotated vector.
