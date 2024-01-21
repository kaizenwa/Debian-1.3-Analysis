.SH NAME

.SH SYNOPSIS
.TP 28
subroutine zrotg(ca,cb,c,s)
.TP 28
.ti +4
double
complex ca,cb,s
.TP 28
.ti +4
double
precision c
.TP 28
.ti +4
double
precision norm,scale
.TP 28
.ti +4
double
complex alpha
.TP 28
.ti +4
if
(cdabs(ca) .ne. 0.0d0) go to 10
.TP 28
.ti +4
c
= 0.0d0
.TP 28
.ti +4
s
= (1.0d0,0.0d0)
.TP 28
.ti +4
ca
= cb
.TP 28
.ti +4
go
to 20
.TP 28
.ti +4
10
continue
.TP 28
.ti +4
scale
= cdabs(ca) + cdabs(cb)
.TP 28
.ti +4
norm
= scale*dsqrt((cdabs(ca/dcmplx(scale,0.0d0)))**2 +
.TP 28
.ti +4
*
(cdabs(cb/dcmplx(scale,0.0d0)))**2)
.TP 28
.ti +4
alpha
= ca /cdabs(ca)
.TP 28
.ti +4
c
= cdabs(ca) / norm
.TP 28
.ti +4
s
= alpha * dconjg(cb) / norm
.TP 28
.ti +4
ca
= alpha * norm
.TP 28
.ti +4
20
continue
.TP 28
.ti +4
return
.TP 28
.ti +4
end
.SH PURPOSE
