.TH SGEEV l "08 October 1994" "LAPACK version 2.0" "LAPACK driver routine (version 2.0)"
.TH SGEEV l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
SGEEV - compute for an N-by-N real nonsymmetric matrix A, the eigenvalues and, optionally, the left and/or right eigenvectors
.SH SYNOPSIS
.TP 18
SUBROUTINE SGEEV(
JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR,
LDVR, WORK, LWORK, INFO )
.TP 18
.ti +4
CHARACTER
JOBVL, JOBVR
.TP 18
.ti +4
INTEGER
INFO, LDA, LDVL, LDVR, LWORK, N
.TP 18
.ti +4
REAL
A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
WI( * ), WORK( * ), WR( * )
.SH PURPOSE
SGEEV computes for an N-by-N real nonsymmetric matrix A, the
eigenvalues and, optionally, the left and/or right eigenvectors.

The right eigenvector v(j) of A satisfies
.br
                 A * v(j) = lambda(j) * v(j)
.br
where lambda(j) is its eigenvalue.
.br
The left eigenvector u(j) of A satisfies
.br
              u(j)**H * A = lambda(j) * u(j)**H
.br
where u(j)**H denotes the conjugate transpose of u(j).
.br

The computed eigenvectors are normalized to have Euclidean norm
equal to 1 and largest component real.
.br

.SH ARGUMENTS
.TP 8
JOBVL   (input) CHARACTER*1
= 'N': left eigenvectors of A are not computed;
.br
= 'V': left eigenvectors of A are computed.
.TP 8
JOBVR   (input) CHARACTER*1
.br
= 'N': right eigenvectors of A are not computed;
.br
= 'V': right eigenvectors of A are computed.
.TP 8
N       (input) INTEGER
The order of the matrix A. N >= 0.
.TP 8
A       (input/output) REAL array, dimension (LDA,N)
On entry, the N-by-N matrix A.
On exit, A has been overwritten.
.TP 8
LDA     (input) INTEGER
The leading dimension of the array A.  LDA >= max(1,N).
.TP 8
WR      (output) REAL array, dimension (N)
WI      (output) REAL array, dimension (N)
WR and WI contain the real and imaginary parts,
respectively, of the computed eigenvalues.  Complex
conjugate pairs of eigenvalues appear consecutively
with the eigenvalue having the positive imaginary part
first.
.TP 8
VL      (output) REAL array, dimension (LDVL,N)
If JOBVL = 'V', the left eigenvectors u(j) are stored one
after another in the columns of VL, in the same order
as their eigenvalues.
If JOBVL = 'N', VL is not referenced.
If the j-th eigenvalue is real, then u(j) = VL(:,j),
the j-th column of VL.
If the j-th and (j+1)-st eigenvalues form a complex
conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
.br
u(j+1) = VL(:,j) - i*VL(:,j+1).
.TP 8
LDVL    (input) INTEGER
The leading dimension of the array VL.  LDVL >= 1; if
JOBVL = 'V', LDVL >= N.
.TP 8
VR      (output) REAL array, dimension (LDVR,N)
If JOBVR = 'V', the right eigenvectors v(j) are stored one
after another in the columns of VR, in the same order
as their eigenvalues.
If JOBVR = 'N', VR is not referenced.
If the j-th eigenvalue is real, then v(j) = VR(:,j),
the j-th column of VR.
If the j-th and (j+1)-st eigenvalues form a complex
conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
.br
v(j+1) = VR(:,j) - i*VR(:,j+1).
.TP 8
LDVR    (input) INTEGER
The leading dimension of the array VR.  LDVR >= 1; if
JOBVR = 'V', LDVR >= N.
.TP 8
WORK    (workspace/output) REAL array, dimension (LWORK)
On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
.TP 8
LWORK   (input) INTEGER
The dimension of the array WORK.  LWORK >= max(1,3*N), and
if JOBVL = 'V' or JOBVR = 'V', LWORK >= 4*N.  For good
performance, LWORK must generally be larger.
.TP 8
INFO    (output) INTEGER
= 0:  successful exit
.br
< 0:  if INFO = -i, the i-th argument had an illegal value.
.br
> 0:  if INFO = i, the QR algorithm failed to compute all the
eigenvalues, and no eigenvectors have been computed;
elements i+1:N of WR and WI contain eigenvalues which
have converged.
