$B!X(Btm $B$K$*$1$kJ8;z(B code $B$N<h$j07$$$K$D$$$F!Y(B
by $B<i2,(B $BCNI'(B
1996$BG/(B7$B7n(B18$BF|(B

* charset $B<h$j07$$$K4X$9$k4pK\86B'(B

  tm $B$G$O86B'$H$7$F(B MIME $B$GDj$a$i$l$?(B charset $B5!9=$K4p$E$$$FJ8;z(B code 
$B$r<h$j07$&!#$3$l$r<B8=$9$k$?$a$K!"(Bmessage $B$rI=<($9$k$?$a$N(B buffer
(preview buffer; cooked buffer) $B$NB>$K@8$N!JB($A!"(Bcode $BJQ49$5$l$F$*$i(B
$B$:(B network code $B$N$^$^$N!K(Bmessage $B$,F~$C$?(B buffer (original article
buffer, raw buffer) $B$rMQ$$$k$3$H$H$9$k!#$3$N>r7o$,K~$?$5$l$l$P!"(Bbase64 
$B$d(B quoted-printable $BEy$N(B encode $B$r9T$J$C$F$$$J$$>l9g$G$b!"#1$D$N(B 
message $B$KJ#?t$NJ8;z(B code $B$r:.:_$5$;$k$3$H$,$G$-$k!#(B

  $B$7$+$7$J$,$i!"8=<B$K$O4v$D$+$NLdBj$,B8:_$9$k!##1$D$OHs(B MIME message 
$B$N<h$j07$$$G$"$j!"$b$&0l$D$O>e5-$N$h$&$JJ}K!$r$H$k$3$H$,:$Fq$J(B MUA $B$r(B
$BMxMQ$9$k>l9g$G$"$k!#Hs(B MIME message $B$KBP$7$F$O!"(Btm $B$O(B default $B$N(B 
charset $B$r2>Dj$9$k$3$H$GBP=h$7$F$$$k!#8e<T$NLdBj$K4X$7$F$O(B MUA $BKh$K(B 
code $BJQ49(B program $B$r@_Dj$7!"(Braw buffer $B$r:n@.$9$k$3$H$,:$Fq$J(B MUA $B$N>l(B
$B9g!"(B7bit, 8bit, binary $B$N$h$&$J(B encode $B$5$l$F$$$J$$(B message $B$N(B charset 
$B$K$h$k(B code $BJQ49$r$d$a$k$h$&$K$9$k$3$H$GBP=h$7$F$$$k!#(B

  $B0J2<!"<B:]$N5!9=$K4X$7$F@bL@$9$k!#(B


* $BJ8;z(B code $B$N<h$j07$$$K4X$9$k(B layer $B9=B$(B

  tm $B$O(B

[[E
	(4) user interface layer (tm)
	(3) non-interactive MIME feature layer (tm)
	(2) MIME encoding layer (MEL) / utility function layer (tl)
	(1) emu API layer
	(0) (API of emacs variants)
]]E

$B$H$$$&#5AX9=B$$K$J$C$F$$$k$,!"J8;z(B code $B$N<h$j07$$$bF1MM$G$"$k!#(B

  $BBh#1AX$G$"$k(B emu API $B$GA4$F$N(B emacs variant $B$GMxMQ2DG=$J4pK\4X?t$rDs(B
$B6!$7!"Bh#3AX$G(B tm-view $B$GMxMQ$9$k$?$a$N4pK\E*$J5!G=$r<B8=$7!"Bh#4AX$G(B
$B$=$N3&LL$rDj5A$9$k!#(B


* $BJ8;z(B code $B$K4X$9$k(B emu API $B$N5!G=(B

  EMACS $B$K$O!"85Mh!"J8;z$H$$$&35G0$OB8:_$7$J$+$C$?!#$h$C$F!"$=$l$r07$&(B
$B$?$a$N5!G=$OB8:_$7$J$+$C$?!#(B

  emacs $B$N@$3&$KJ8;z(B code $B$H$$$&35G0$r;}$A9~$s$@$N$O(B NEmacs $B$G$"$k!JB?(B
$BJ,(B(^_^;$B!K!#(BNEmacs $B$O(B `JIS', `EUC', `Shift JIS'$B!J$*$h$SL5JQ49!K$H$$$&#3(B
$B<oN`$N!X4A;z(B code$B!Y$rF3F~$7$?!#$=$7$F!"$3$l$r?tCM$GI=8=$7$?!#(B

  NEmacs $B$OF|K\8l!J$N(B subset$B!K$7$+07$&I,MW$,$J$+$C$?$N$G9b!9#3<oN`$NJ8(B
$B;z(B code $B$,;H$($l$P==J,$G$"$C$?$,!"$3$l$rB?8@8l2=$7$?(B MULE $B$G$O$h$jB??t(B
$B$NJ8;z(B code $B$,07$&$3$H$,MW5a$5$l!"$^$?!"(Buser $B$,J8;z(B code $B$rDj5A$G$-$k(B
$B$3$H$bI,MW$G$"$C$?!#$3$N$?$a!"?tCM$G$O$J$/!"@hF,$HKvHx$K(B `*' $B$rIU$1$?(B
$B<+8JI>2A7A<0$NDj?t$GI=8=$5$l$k(B coding-system $B$H$$$&35G0$,F3F~$5$l$?!#(B

  XEmacs $B$K(B mule $B5!G=$rF3F~$7$h$&$H$7$??MC#$O<+8JI>2A7A<0$NDj?t$NBe$o(B
$B$j$K(B coding-system $B$rI=$9?7$?$J7?$rF3F~$9$k$3$H$K$7$?!#$^$?!"$3$N7?$K(B
$BBP1~$9$kL>A0$r(B symbol $B$GM?$($?!#$3$N$?$a!"(Bcoding-system $B$rI=$9BgNL$NDj(B
$B?t$O0lA]$5$l$?!#(B

  $B$3$N$h$&$K!"8=B8$9$k(B emacs variants $B$N4V$G$OJ8;z(B code $B$NI=8=K!$O$5$^(B
$B$6$^$G$"$k!#$b$H$b$H!"(Bemu $B$O$3$NJ,Ln$G$b$C$H$b5!G=$N9b$$(B MULE $B$r$*<jK\(B
$B$K$3$l$N5!G=$r(B emulate $B$9$k4X?t$r3F(B emacs variant $B$KBP$7$FDj5A$9$kJ}K!(B
$B$r:N$C$?$,!"(BMULE $B$r85$K(B EMACS/MULE $BE}9gHG$d(B XEmacs/mule $B$,@8$^$l$k$K=>(B
$B$$!"(Bcoding-system $B5!G=$r$=$N$^$^;H$&$N$OF@:v$G$O$J$$$HH=CG$9$k$K;j$C$?!#(B
$BBe$o$j$K$h$jCj>]EY$N9b$$(B MIME charset $B$K4X$9$k(B API $B$rMQ0U$9$k$3$H$K$7(B
$B$?!#(B

  MIME charset $B$OJ8;z(B code $B$rI=$9$b$N$G$"$j!"(BMULE $B$G$$$&$H$3$m$N(B 
leading-char $B$d(B EMACS/MULE $B$d(B XEmacs/mule $B$G$$$&$H$3$m$N(B charset $B$KAj(B
$BEv$9$k$b$N$G$O$J$/!"(Bmule $B$N(B coding-system $B$KAjEv$9$k5!G=$G$"$k!#<B:]!"(B
mule $B$N(B coding-system $B$NL>A0$H(B MIME charset $B$NL>A0$O6&DL$9$k$b$N$b>/$J(B
$B$/$J$$!#(B

  MIME charset $B$NI=8=$N;EJ}$H$7$F!"(Bemu API $B$G$O(B XEmacs/mule $B$HF1MM!"(B
symbol $B$rMQ$$$k$3$H$K$7$?!#(BMULE $B$N$h$&$JJQ?t$r;H$&$HB+G{$K4X$9$k(B check 
$B$,I,MW$G$"$k$,!"(Bsymbol $B$G$O$3$N$h$&$J(B check $B$OITMW$G$"$k!#$^$?!">-MhE*(B
$B$K!"(BXEmacs/mule $B$NJ}K!$KE}9g$5$l$k$3$H$r4|BT$9$k0UL#$b$"$k!#<B:]!"(B
XEmacs/mule $B$H(B emu API $B$N4V$N(B gap $B$O>/$J$/!">-Mh!"A4$F$N(B MIME charset 
$B$,(B XEmacs/mule $B$N(B coding-system $B$H$7$FDj5A$5$l$l$P!"(Bemu $B$H$7$F$3$l$rMQ(B
$B0U$9$kI,MW$O$J$/$J$k$@$m$&!#(B

  $B$5$F!"0J2<$G$O<B:]$N5!G=$r2r@b$9$k!#(B

[$BJQ?t(B] charsets-mime-charset-alist

	Key $BIt$K(B charset $B$N(B list, value $BIt$K(B mime-charset $B$r$H$kO"A[%j(B
	$B%9%H!#4X?t(B charsets-to-mime-charset $B$d4X?t(B 
	detect-mime-charset-region $B$,$3$NJQ?t$r;2>H$9$k!#(B

[$BJQ?t(B] default-mime-charset

	$BJQ?t(B charsets-mime-charset-alist $B$GDj5A$5$l$F$$$J$$(B charset $B$N(B 
	list $B$KBP$7$F$D$1$k(B MIME charset $B$r<($9!#4X?t(B 
	charsets-to-mime-charset $B$d4X?t(B detect-mime-charset-region $B$,(B
	$B$3$NJQ?t$r;2>H$9$k!#(B

[$B4X?t(B] charsets-to-mime-charset		CHARSETS

	charset $B$N(B list `CHARSETS' $B$KBP1~$9$k(B MIME charset $B$rJV$9!#(B

[$B4X?t(B] detect-mime-charset-region	start end

	`START' $B$H(B `END' $B$G0O$^$l$?NN0h$KBP1~$9$k(B MIME charset $B$rJV$9!#(B

[$B4X?t(B] encode-mime-charset-region	START END CHARSET

	`START' $B$H(B `END' $B$G0O$^$l$?NN0h$r(B MIME charset `CHARSET' $B$G(B 
	encode $B$9$k!#(B

[$B4X?t(B] encode-mime-charset-string	STRING CHARSET

	$BJ8;zNs(B `STRING' $B$r(B MIME charset `CHARSET' $B$G(B encode $B$9$k!#(B

[$B4X?t(B] decode-mime-charset-string	STRING CHARSET

	$BJ8;zNs(B `STRING' $B$r(B MIME charset `CHARSET' $B$G(B decode $B$9$k!#(B


* $BJ8;z(B code $B$K4X$9$k(B tm $B$N<BAu(B

  $B$b$7!"AGD>$K(B charset $B$r<B8=$9$k$@$1$J$i(B emu API $B$r$=$N$^$^;H$($PNI$$(B
$B$N$@$,!"<B:]$K$O(B VM $B$d(B RMAIL$B!J$*$h$S@N$N(B GNUS, Gnus$B!K$H$$$C$?(B raw
buffer $B$r<B8=$7$K$/$$(B MUA $B$G$O(B charset $B$KBP1~$9$k$3$H$,:$Fq$G$"$k!#$3(B
$B$N$?$a!"(Btm-view $B$O$3$N$h$&$J(B MUA $B$G$O=c?h$J0UL#$G$N(B raw buffer $B$N<B8=(B
$B$rD|$a(B original article buffer $BCf$K4{$K(B charset $B$K0M$i$:$K(B code $BJQ49$7(B
$B$?(B message $B$rF~$l$k$3$H$rG'$a$F$$$k!#(B

  $B$h$C$F!"0J2<$N#2<oN`$N(B code $BJQ494X?t$r;H$$J,$1$kI,MW$,$"$k!#(B

[$B4X?t(B] mime-charset/decode-buffer	charset &optional encoding

	current buffer $B$r(B MIME charset `charset' $B$GFbIt(B code $B$KJQ49$9(B
	$B$k!#$3$N4X?t$O(B `encoding' $B$K0M$i$:I,$:(B code $BJQ49$r9T$J$&!#(B

[$B4X?t(B] mime-charset/maybe-decode-buffer	charset &optional encoding

	`encoding' $B$,(B nil, "7bit", "8bit", "binary" $B0J30$N;~!"B($A!"(B
	ASCII printable $B$J(B encode $B$,;\$5$l$F$$$k;~$N$_!"<B:]$K!"(B
	current buffer $B$r(B MIME charset `charset' $B$GFbIt(B code $B$KJQ49$9(B
	$B$k!#$=$l0J30$N;~$O!"2?$b$7$J$$!#(B


* $BJ8;z(B code $B$K4X$9$k(B tm $B$N3&LL(B

  MUA $B$K$h$C$FJ8;z(B code $B$r;H$$J,$1$k$?$a$K(B tm $B$O$3$3$G=R$Y$k$h$&$J3&LL(B
$B$rDs6!$9$k!#(B

[$BJQ?t(B] mime-viewer/code-converter-alist

	$B$3$NJQ?t$O(B key $BIt$r(B major-mode, value $BIt$r(B code $BJQ494X?t$H$9$k(B
	$BO"A[(B list $B$G$"$k!#$3$l$rMQ$$$F3F(B MUA $BKh$N(B code $BJQ494X?t$r@_Dj(B
	$B$9$k!#(Bcode $BJQ494X?t$OA4@a$G=R$Y$?<BAu$rMQ$$$k$3$H!#(B

[$BJQ?t(B] mime::article/code-converter

	method $B$GMQ$$$i$l$k(B code $BJQ494X?t$r;XDj$9$k$?$a$N(B original
	article buffer $B$N(B buffer local $BJQ?t$G$"$k!#JQ?t(B 
	`mime-viewer/code-converter-alist' $B$h$j$b9b$$M%@hEY$G;2>H$5$l(B
	$B$k!#(B

[$B4X?t(B] mime-preview/decode-text-buffer	charset encoding

	MIME charset `charset', MIME encoding `encoding' $B$GId9f2=$5$l(B
	$B$?(B current buffer $B$r(B decode $B$9$k!#(Bcode $BJQ49$OJQ?t(B 
	`mime::article/code-converter' $B$*$h$S(B 
	`mime-viewer/code-converter-alist' $B$r;2>H$7$FF@$i$l$?4X?t$K$h$C(B
	$B$F<B9T$5$l$k!#(B


* $BJ8;z(B code $B4X78$N4X?t$N;H$$J}$N;X?K(B

  tm-view $B$N(B method $B$,$"$k(B content $B$r$"$k(B charset, encoding $B$K=>$C$F(B 
decode $B$9$k>l9g$O4X?t(B mime-preview/decode-text-buffer $B$rMQ$$$k$3$H$H$9(B
$B$k!#(B

  tm-view $B$N(B method $B$K$*$$$F!"$"$k(B content $B$KBP$7$F:F5"E*$K(B preview
buffer $B$r:n@.$9$k$h$&$J>l9g$G!"JQ?t(B mime::article/code-converter $B$r@_(B
$BDj$9$kI,MW$,$"$k>l9g$O!"4X?t(B mime-charset/decode-buffer $B$*$h$S(B 
mime-charset/maybe-decode-buffer $B$rMQ$$$k$3$H$H$9$k!#(B

  $B$=$l0J30$N>l9g$K$*$$$F!"<B:]$K(B code $BJQ49$r9T$J$&$h$&$J>l9g$O!"(Bemu
API $B$N5!G=$rMQ$$$k$3$H$H$9$k!#(B
