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
# Translated by Jeremie Petit <petit@eurecom.fr>
#
array set ztrans {
    accept	Accepter
    action	Action
    actions	Actions
    append	Ajouter
    away	Ailleurs
    back	{De Retour}
    ban		Bannir
    brb		{Je R'viens}
    busy	Occupé
    buttons	Boutons  
    call	Appeler
    cancel	Annuler
    channel	Canal
    channels	Canaux
    chanop	Opérateur
    chat	Discuter
    clear	Effacer
    close	Fermer
    connect	Connecter
    crypt	Crypter
    ctcp	CTCP
    dcc		DCC
    default	Défaut
    delete	Enlever
    dismiss	Abandonner
    draw	Dessiner
    empty	Vide
    error	Erreur
    exec	Executer
    favourites	Favoris
    finger	Finger
    flush	Flusher
    get		Récupérer
    help	Aide
    history	Historique
    hostname	{Nom de Machine}
    info	Info
    invite	Inviter
    irc		IRC
    ircname	{Nom IRC}
    join	{Aller à}
    jump	Sauter
    keep	Garder
    key		Touche
    kick	Virer
    kill	Tuer
    leave	Partir
    log		Enregistrer
    limit	Limiter
    list	Lister
    message	Message
    messages	Messages
    mode	Mode
    moderated	Modéré
    monitor	Monitorer
    name	Nom
    names	Noms
    new		Nouveau
    nickname	Surnom
    nocase	{Pas de Majuscules}
    notice	Notification
    notify	Notifier
    offer	Offrir
    ok		OK
    open	Ouvrir
    operator	Operateur
    parameter	Paramètre
    parameters	Paramètres
    password	{Mot de Passe}
    pattern	Pattern
    people	Personnes
    plugin	Module
    port	Port
    private	Privé
    quiet	Silencieux
    quit	Quitter
    reconnecting	Reconnexion
    refresh	Rafraîchir
    register	{S'Enregistrer}
    reject	Rejeter
    script	Script
    secret	Secret
    send	Envoyer
    server	Serveur
    servers	Serveurs
    service	Service
    services	Services
    set		Appliquer
    shutdown	Arrêt
    signoff	Disparition
    sound	Son
    speak	Parler
    text	Texte
    time	Heure
    topic	Sujet
    unban	Débannir
    user	Utilisateur
    users	Utilisateurs
    view	Voir
    who		Qui
    whois	{Qui Est}
    whowas	{Qui Était}
    windows	Fenêtres
}
#
# Compounds of the above that are no language dependent
#
array set ztrans "
    bankick	{$ztrans(ban)+$ztrans(kick)}
"

