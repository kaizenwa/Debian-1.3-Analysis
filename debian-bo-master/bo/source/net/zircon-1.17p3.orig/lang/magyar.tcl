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
    abort	Abortál
    accept	Elfogad
    action	Akció
    actions	Akciók
    append	Hozzáfûz
    away	Távollét
    back	Vissza
    ban		Ban
    brb		BRB
    busy	Elfoglalt
    buttons	Gombok  
    call	Hivás
    cancel	Törlés
    channel	Csatorna
    channels	Csatornák
    chanop	Csat.Op.
    chat	Duma
    clear	Letörlés
    close	Becsuk
    connect	Csatlakozás
    crypt	Titkosítás
    ctcp	CTCP
    dcc		DCC
    default	Alapállapot
    delete	Törlés
    dismiss	Eltüntetés
    draw	Húzás
    empty	Üres
    error	Hiba
    exec	Végrehajtás
    favourites	Kedvencek
    finger	Fingerelés
    flush	Ürítés
    get		Vétel
    help	Segítség
    history	Nyomok
    hostname	Hósztnév
    info	Infó
    invite	Meghív
    irc		IRC
    ircname	Ircnév
    join	Bekapcsolódás
    jump	Ugrás
    keep	Tartás
    key		Kulcs
    kick	Kirugás
    kill	Megölés
    leave	Elhagyás
    log		Log
    limit	Limit
    list	Lista
    message	Üzenet
    messages	Üzenetek
    mode	Mód
    moderated	Moderált
    monitor	Monitor
    name	Név
    names	Nevek
    new		Új
    nickname	Becenév
    nocase	Nocase
    notice	Megjegyzés
    notify	Értesít
    offer	Ajánlat
    ok		OK
    open	Kinyit
    operator	Operátor
    parameter	Paraméter
    parameters	Paraméterek
    password	Jelszó
    pattern	Minta
    people	Emberek
    plugin	Plug-In
    port	Porta
    private	Privát
    quiet	Csendes
    quit	Kilépés
    reconnecting	Visszacsatlakozás
    refresh	Frissítés
    register	Regiszter
    reject	Elutasítás
    script	Szkript
    secret	Titkos
    send	Továbbít
    server	Szerver
    servers	Szerverek
    service	Szolgáltatás
    services	Szolgáltatások
    set		Beállítás
    shutdown	Lezárás
    signoff	Kilépés
    sound	Hang
    speak	Beszéd
    text	Szöveg
    time	Idõ
    topic	Téma
    unban	Unban
    user	Felhasználó
    users	Felhasználók
    view	Megfigyelés
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
