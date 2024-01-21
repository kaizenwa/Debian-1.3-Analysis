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
    abort	Aborti
    accept	Akcepti
    action	Ago
    actions	Agoj
    append	Almeti
    away	For
    back	Reveninta
    ban		Interdikti
    brb		BRB
    busy	Okupato
    buttons	Butonoj  
    call	Voki
    cancel	Nuligi
    channel	Vojo
    channels	Vojoj
    chanop	ChanOp
    chat	Babili
    clear	Senigi
    close	Fini
    connect	Kunigi
    crypt	Cifri
    ctcp	CTCP
    dcc		DCC
    default	Default
    delete	Forstreki
    dismiss	Maldungi
    draw	Desegni
    empty	Empty
    error	Malplenigi
    exec	Exec
    favourites	Favoratoj
    finger	Finger
    flush	Traakvumi
    get		Akiri
    help	Helpi
    history	Historio
    hostname	{Nomo de masino}
    info	Info
    invite	Inviti
    irc		IRC
    ircname	{Nomo de IRC}
    join	Kunigi
    jump	Salti
    keep	Konservi
    key		Slosilo
    kick	Piedi
    kill	Mortigi
    leave	Lasi
    log		Taglibro
    limit	Limigi
    list	Listigi
    message	Mesago
    messages	Mesagoj
    mode	Modo
    moderated	Moderigita
    monitor	Mentoro
    name	Nomo
    names	Nomoj
    new		Nova
    nickname	Moknomo
    nocase	{Sen literfako}
    notice	Anonco
    notify	Avizi
    offer	Proponi
    ok		OK
    open	Malfermi
    operator	Metiisto
    parameter	Parameter
    parameters	Parameters
    password	Signalvorto
    pattern	Modelo
    people	Personoj
    plugin	Enstopo
    port	Aperturo
    private	Privata
    quiet	Silenta
    quit	Lasi
    reconnecting	Rekuniganta
    refresh	Refresigi
    register	Registri
    reject	Rejeti
    script	Programo
    secret	Sekreta
    send	Sendi
    server	Ser
    servers	Servers
    service	Prezento
    services	Prezentoj
    set		Meti
    shutdown	Fermi
    signoff	Signoff
    sound	Sono
    speak	Paroli
    text	Teksto
    time	Horo
    topic	Temo
    unban	Malinterdikti
    user	Uzino
    users	Uzinoj
    view	Vidi
    who		Kiu
    whois	{Kiu estis}
    whowas	{Kiu estas}
    windows	Fenestroj
}
#
# Compounds of the above that are no language dependent
#
array set ztrans "
    bankick	{$ztrans(ban)+$ztrans(kick)}
"
