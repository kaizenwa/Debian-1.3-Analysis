$B!X(Bsignature.el $B@bL@=q!Y(B
by. $B2,It(B $B<wCK(B
1994$BG/(B8$B7n(B1$BF|(B

  signature.el $B$O(B signature $B$N<+F0@ZBX$((B tool $B$G$9!#(B*insert-signature*
$B$r9T$&$3$H$K$h$j!"(B`signature-insert-at-eof' $B$NCM$,(B non-nil $B$N$H$-%P%C(B
$B%U%!$NKvHx$K!"(Bnil $B$N$H$-$O%+%l%s%H%]%$%s%H$K!"(Bsignature $B%U%!%$%k$,FI$_(B
$B9~$^$l$^$9!#I8=`E*$J(Bsignature $B$O!"JQ?t(B `signature-file-name' $B$G;XDj$7(B
$B$F2<$5$$(B($B%G%U%)%k%HCM$O(B "~/.signature")$B!#(Bmessage header $B$N(B field $B$K9g(B
$B$o$;$F(B signature $B$N<+F0@ZBX$r9T$J$$$?$$>l9g$O(B .emacs $B$K0J2<$N$h$&$J$b(B
$B$N$rF~$l$F2<$5$$!#(B

[[E
----------------------------------------------------------------------
(setq signature-file-alist
      '(
	(("Newsgroups" . "jokes")       . "~/.signature-jokes")
	(("Newsgroups" . ("zxr" "nzr")) . "~/.signature-sun")
	(("To" . ("ishimaru" "z-suzuki")) . "~/.signature-sun")
	(("To" . "tea")		        . "~/.signature-jokes")
	(("To" . ("sim" "oku" "takuo")) . "~/.signature-formal")
	))
----------------------------------------------------------------------
]]E

  $B$^$?!"(B*insert-signature* $B$K?t0z?t$rM?$($k$3$H$K$h$j!"BPOCE*$K(B
`signature-file-name'-DISTRIBUTION $B$N7A$NL>A0$r;}$D%U%!%$%k$r;XDj(B
$B$9$k$3$H$,$G$-$^$9!#Nc$($P(B `C-u C-c C-s'(*insert-signature*) $B$H(B
$BF~NO$9$k$H!"%_%K%P%C%U%!$G(B

	Insert your signature: ~/.signature-

$B$HJ9$$$F$-$^$9$N$G!"@ZBXMQ$KMQ0U$5$l$?%U%!%$%k$N$J$+$+$i!"5a$a$k$b$N$r(B
$B%3%s%W%j!<%7%g%s$r;H$C$FMF0W$KA*Br$9$k$3$H$,$G$-$^$9!#6uJ8;zNs$rF~NO$9(B
$B$l$P!"(B`signature-file-name' $B$G;XDj$5$l$k%U%!%$%k$,FI$_9~$^$l$^$9!#(B
