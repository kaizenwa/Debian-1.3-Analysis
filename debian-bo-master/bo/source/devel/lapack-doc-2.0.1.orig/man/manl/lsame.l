.TH LSAME l "08 October 1994" "LAPACK version 2.0" "LAPACK auxiliary routine (version 2.0)"
.TH LSAME l "08 October 1994" "BLAS routine" "BLAS routine"
.SH NAME
LSAME - return .TRUE
.SH SYNOPSIS
.TP 17
LOGICAL FUNCTION
LSAME( CA, CB )
.TP 17
.ti +4
CHARACTER
CA, CB
.SH PURPOSE
LSAME returns .TRUE. if CA is the same letter as CB regardless of
case.
.br

.SH ARGUMENTS
.TP 8
CA      (input) CHARACTER*1
CB      (input) CHARACTER*1
CA and CB specify the single characters to be compared.
