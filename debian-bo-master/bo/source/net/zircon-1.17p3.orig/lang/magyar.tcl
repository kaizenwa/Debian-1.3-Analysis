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
    abort	Abort�l
    accept	Elfogad
    action	Akci�
    actions	Akci�k
    append	Hozz�f�z
    away	T�voll�t
    back	Vissza
    ban		Ban
    brb		BRB
    busy	Elfoglalt
    buttons	Gombok  
    call	Hiv�s
    cancel	T�rl�s
    channel	Csatorna
    channels	Csatorn�k
    chanop	Csat.Op.
    chat	Duma
    clear	Let�rl�s
    close	Becsuk
    connect	Csatlakoz�s
    crypt	Titkos�t�s
    ctcp	CTCP
    dcc		DCC
    default	Alap�llapot
    delete	T�rl�s
    dismiss	Elt�ntet�s
    draw	H�z�s
    empty	�res
    error	Hiba
    exec	V�grehajt�s
    favourites	Kedvencek
    finger	Fingerel�s
    flush	�r�t�s
    get		V�tel
    help	Seg�ts�g
    history	Nyomok
    hostname	H�sztn�v
    info	Inf�
    invite	Megh�v
    irc		IRC
    ircname	Ircn�v
    join	Bekapcsol�d�s
    jump	Ugr�s
    keep	Tart�s
    key		Kulcs
    kick	Kirug�s
    kill	Meg�l�s
    leave	Elhagy�s
    log		Log
    limit	Limit
    list	Lista
    message	�zenet
    messages	�zenetek
    mode	M�d
    moderated	Moder�lt
    monitor	Monitor
    name	N�v
    names	Nevek
    new		�j
    nickname	Becen�v
    nocase	Nocase
    notice	Megjegyz�s
    notify	�rtes�t
    offer	Aj�nlat
    ok		OK
    open	Kinyit
    operator	Oper�tor
    parameter	Param�ter
    parameters	Param�terek
    password	Jelsz�
    pattern	Minta
    people	Emberek
    plugin	Plug-In
    port	Porta
    private	Priv�t
    quiet	Csendes
    quit	Kil�p�s
    reconnecting	Visszacsatlakoz�s
    refresh	Friss�t�s
    register	Regiszter
    reject	Elutas�t�s
    script	Szkript
    secret	Titkos
    send	Tov�bb�t
    server	Szerver
    servers	Szerverek
    service	Szolg�ltat�s
    services	Szolg�ltat�sok
    set		Be�ll�t�s
    shutdown	Lez�r�s
    signoff	Kil�p�s
    sound	Hang
    speak	Besz�d
    text	Sz�veg
    time	Id�
    topic	T�ma
    unban	Unban
    user	Felhaszn�l�
    users	Felhaszn�l�k
    view	Megfigyel�s
    who		Ki
    whois	{Ki most}
    whowas	{Ki volt}
    windows	Ablakok
}
#
# Compounds of the above that are no language dependent
#
array set ztrans "
    bankick	{$ztrans(ban)+$ztrans(kick)}
"
