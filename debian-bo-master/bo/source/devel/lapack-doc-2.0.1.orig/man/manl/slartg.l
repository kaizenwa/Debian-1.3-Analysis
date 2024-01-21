.TH SLARTG l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH SLARTG l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SLARTG - generate a plane rotation so that   [ CS SN ] 
.SH SYNOPSIS
.TP 19
SUBROUTINE SLARTG(
F, G, CS, SN, R )
.TP 19
.ti +4
REAL
CS, F, G, R, SN
.SH PURPOSE
SLARTG generate a plane rotation so that
   [ -SN  CS  ]     [ G ]     [ 0 ]
.br

This is a slower, more accurate version of the BLAS1 routine SROTG,
with the following other differences:
.br
   F and G are unchanged on return.
.br
   If G=0, then CS=1 and SN=0.
.br
   If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
      floating point operations (saves work in SBDSQR when
      there are zeros on the diagonal).
.br

If F exceeds G in magnitude, CS will be positive.
.br

.SH ARGUMENTS
.TP 8
F       (input) REAL
The first component of vector to be rotated.
.TP 8
G       (input) REAL
The second component of vector to be rotated.
.TP 8
CS      (output) REAL
The cosine of the rotation.
.TP 8
SN      (output) REAL
The sine of the rotation.
.TP 8
R       (output) REAL
The nonzero component of the rotated vector.
