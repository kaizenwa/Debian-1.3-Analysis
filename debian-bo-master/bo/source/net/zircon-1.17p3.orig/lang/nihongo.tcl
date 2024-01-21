#
# $Source:$
# $Date:$
# $Revision:$
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1996 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
array set ztrans {
    abort	$B@ZCG(B
    accept	$B<u<h(B
    action	$BF0:n(B
    actions	$BF0:n(B
    append	$BDI2C(B
    away	$BN1<i(B
    back	$BLa(B
    ban		Ban
    brb		BRB
    busy	$BK;$7$$(B
    buttons	$B%\%?%s(B  
    call	$B8F=P(B
    cancel	$B2r=|(B
    channel	$B%A%c%s%M%k(B
    channels	$B%A%c%s%M%k(B
    chanop	$B%A%c%s%M%k(BOp
    chat	$BOC(B
    clear	$B%/%j%"!<(B
    close	$BJD(B
    connect	$B%3%s%M%/%H(B
    crypt	$B0E9f(B
    ctcp	CTCP
    dcc		DCC
    default	$B%G%U%)!<%k%H(B
    delete	$B<h>C(B
    dismiss	$BJD(B
    draw	$B0z(B
    empty	$B6u(B
    error	$B%(%i!<(B
    exec	$B<B9T(B
    favourites	$B?M5$$b$N(B
    finger	$B%U%#%s%,!<(B
    flush	$B%U%i%C%7%e(B
    get		$B<h(B
    help	$B%X%k%W(B
    history	$B%R%9%H%j!<(B
    hostname	$B%[!<%9%HL>(B
    info	$B%$%s%U%)%a!<%7%g%s(B
    invite	$B>7BT(B
    irc		IRC
    ircname	Irc$BL>(B
    join	$B;22C(B
    jump	$B%8%c%s%W(B
    keep	$B%-!<%W(B
    key		$B%-!<(B
    kick	$B=3(B
    kill	$B;&(B
    leave	$BJL(B
    log		$B%m%0(B
    limit	$B%j%_%C%H(B
    list	$B%j%9%H(B
    message	$B%a%C%;!<%8(B
    messages	$B%a%C%;!<%8(B
    mode	$B%b!<%I(B
    moderated	$B%b%G%l!<%F%C%I(B
    monitor	$B%b%K%?!<(B
    name	$BL>A0(B
    names	$BL>A0(B
    new		$B?7(B
    nickname	$B0&>N(B
    nocase	$B%N!<%1%9(B
    notice	$BCm0U(B
    notify	$BDLCN(B
    offer	$B?==P(B
    ok		OK
    open	$B%*!<%W%s(B
    operator	$B%*%Z%l!<%?!<(B
    parameter	$B%Q%i%a!<%?!<(B
    parameters	$B%Q%i%a!<%?!<(B
    password	$B%Q%9%o!<%I(B
    pattern	$B%Q%?!<%s(B
    people	$B?M!9(B
    plugin	$B%W%i%0%$%s(B
    port	$B%]!<%H(B
    private	$B8D?ME*(B
    quiet	$BL58@(B
    quit	$B=*N;(B
    reconnecting	$B:F@\B3(B
    refresh	$B%j%U%l%C%7%e(B
    register	$B5-O?(B
    reject	$B5q@d(B
    script	$B%9%/%j%W%H(B
    secret	$BHkL)(B
    send	$BAw(B
    server	$B%5!<%P!<(B
    servers	$B%5!<%P!<(B
    service	$B%5!<%S%9(B
    services	$B%5!<%S%9(B
    set		$B%;%C%H(B
    shutdown	$BJD:?(B
    signoff	$B%5%$%s%*%C%U(B
    sound	$B2;(B
    speak	$BOC(B
    text	$B%F%-%9%H(B
    time	$B;~4V(B
    topic	$BOCBj(B
    unban	Unban
    user	$B%f!<%6!<(B
    users	$B%f!<%6!<(B
    view	$B4Q;!(B
    who		$BC/(B
    whois	$B:#C/(B
    whowas	$BC/$G$7$?(B
    windows	$B%t%#%s%I!<%:(B
}
#
# Compounds of the above that are no language dependent
#
array set ztrans "
    bankick	{$ztrans(ban)+$ztrans(kick)}
"
