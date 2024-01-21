# a good one
a.b

# dots
.a.b	bad dot(s) in name
a.b.	bad dot(s) in name
a..b	bad dot(s) in name
.	bad dot(s) in name

# odd starts
0.a	name does not begin with a letter
+.a	name does not begin with a letter
A.a	uppercase letter(s) in name
control.foo	name starts with control or junk
junk.blah	name starts with control or junk

# bad components
comp.cpu.6809	all-numeric name component
comp.cpu.mc6809
comp.lang.c.++	name component starts with non-alphanumeric
comp.lang.c++
comp.lang.C.++	uppercase letter(s) in name
comp.lang.0++	name component does not contain letter
comp.lang.all	`all' or `ctl' used as name component
comp.lang.ctl	`all' or `ctl' used as name component
comp.lang.verylonglanguage	name component longer than 14 characters
comp.lang.verylonglanguag	name component longer than 14 characters
comp.lang.verylonglangua
comp.lang.C++	uppercase letter(s) in name
comp.lang.c_2
comp.lang.c-2
comp.lang.c!	illegal character(s) in name
comp.lang.c%	illegal character(s) in name
comp.lang.c++
comp.lang.c++x	repeated punctuation in name
comp.lang.c--	repeated punctuation in name
comp.lang.c__	repeated punctuation in name
comp.lang.c.c	repeated component(s) in name
comp.c.lang.c
comp.lang.=c	name component resembles encoded word but isn't one
comp.lang.=?a?b?x?	name component resembles encoded word but isn't one
comp.lang.=?a?b?x??	name component resembles encoded word but isn't one
comp.lang.=?=a?b?x?=	name component resembles encoded word but isn't one
comp.lang.=?a?=b?x?=	name component resembles encoded word but isn't one
comp.lang.=?a?b?x??=	name component resembles encoded word but isn't one
comp.lang.=?a?b?x?=
comp.lang.=?a?b?%x?=	bad character in encoded name component
comp.lang.=?a?a?x?=	encoded name component does not use b encoding
comp.lang.=?a?bb?x?=	encoded name component does not use b encoding
