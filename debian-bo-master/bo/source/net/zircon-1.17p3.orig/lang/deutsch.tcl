#
# $Source: german.tcl$
# $Date: 21.04.96$
# $Revision: 1.0$
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
#   TRANSLATOR: Klaus Luft <Klaus.Luft@stud.uni-karlsruhe.de>
# ----------------------------------------------------------------------
# Copyright 1996 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
#
# Compounds of the above that are no language dependent
#
array set ztrans {
    action	Aktion
    actions	Aktionen
    away	"Bin weg"
    back	"Wieder da"
    ban		Verbannen
    brb		BRB
    busy	Besch�ftigt
    buttons	Buttons
    call        Aufrufen
    cancel	Abbrechen
    channel	Kanal
    channels	Kan�le
    chanop	ChanOp
    chat	Chat
    clear	L�schen
    close	Schlie�en
    connect	Verbinden
    crypt	Verschl�sseln
    ctcp	CTCP
    dcc		DCC
    dismiss	Verlassen
    draw	Zeichnen
    empty	Leeren
    error	Fehler
    exec	Ausf�hren
    favourites	Favoriten
    finger	Finger
    flush	Auffrischen
    get		Nehmen
    help	Hilfe
    history	Historie
    info	Info
    invite	Einladen
    irc		IRC
    ircname	Ircname
    join	Anw�hlen
    jump	Springen
    keep	Behalten
    key		Schl�ssel
    kick	Kicken
    kill	Killen
    leave	Verlassen
    log		Log
    limit	Limit
    list	Liste
    message	Nachricht
    mode	Modus
    moderated	Moderiert
    monitor	Monitor
    names	Namen
    new		Neu
    nickname	Spitzname
    notice	Mitteilung
    notify	Benachrichtigen
    offer	Anbieten
    ok		OK
    open	�ffnen
    operator	Operator
    parameters	Parameter
    password	Passwort
    people	Personen
    port	Port
    private	Privat
    quiet	Still
    quit	Beenden
    reconnecting	Reconnecting
    refresh	Wiederherstellen
    script	Skript
    secret	Geheim
    send	Senden
    server	Server
    servers	Server
    service	Service
    services	Services
    shutdown	Shutdown
    speak	Redner
    time	Zeit
    topic	Thema
    user	Teilnehmer
    users	Teilnehmer
    who		Wer
    whois	"Wer ist"
    whowas	"Wer war"
    windows	Fenster
}
array set ztrans "
    bankick	{$ztrans(ban)+$ztrans(kick)}
"
