.SH NAME

.SH SYNOPSIS
.TP 31
subroutine cscal(n,ca,cx,incx)
.TP 31
.ti +4
c
scales a vector by a constant.
.TP 31
.ti +4
c
jack dongarra, linpack, 3/11/78.
.TP 31
.ti +4
c
modified to correct problem with negative increment, 8/21/90.
.TP 31
.ti +4
complex
ca,cx(1)
.TP 31
.ti +4
integer
i,incx,ix,n
.TP 31
.ti +4
if(n.le.0)return
.TP 31
.ti +4
if(incx.eq.1)go
to 20
.TP 31
.ti +4
c
code for increment not equal to 1
.TP 31
.ti +4
ix
= 1
.TP 31
.ti +4
if(incx.lt.0)ix
= (-n+1)*incx + 1
.TP 31
.ti +4
do
10 i = 1,n
.TP 31
.ti +4
cx(ix)
= ca*cx(ix)
.TP 31
.ti +4
ix
= ix + incx
.TP 31
.ti +4
10
continue
.TP 31
.ti +4
return
.TP 31
.ti +4
c
code for increment equal to 1
.TP 31
.ti +4
20
do 30 i = 1,n
.TP 31
.ti +4
cx(i)
= ca*cx(i)
.TP 31
.ti +4
30
continue
.TP 31
.ti +4
return
.TP 31
.ti +4
end
.SH PURPOSE
