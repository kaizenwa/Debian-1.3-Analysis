.TH LSAME l "16 October 1992" "LAPACK version 1.0" "LAPACK auxiliary routine (version 1.0)"
.TH LSAME l "16 October 1992" "BLAS routine" "BLAS routine"
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

.. Intrinsic Functions ..

..
.. Local Scalars ..

..
.. Executable Statements ..

Test if the characters are equal

Now test for equivalence if both characters are alphabetic.

Use 'Z' rather than 'A' so that ASCII can be detected on Prime
machines, on which ICHAR returns a value with bit 8 set.
ICHAR('A') on Prime machines returns 193 which is the same as
ICHAR('A') on an EBCDIC machine.

ASCII is assumed - ZCODE is the ASCII code of either lower or
upper case 'Z'.

EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
upper case 'Z'.

ASCII is assumed, on Prime machines - ZCODE is the ASCII code
plus 128 of either lower or upper case 'Z'.

RETURN

End of LSAME
