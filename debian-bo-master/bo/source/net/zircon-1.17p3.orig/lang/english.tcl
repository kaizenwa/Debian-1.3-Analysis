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
    abort	Abort
    accept	Accept
    action	Action
    actions	Actions
    append	Append
    away	Away
    back	Back
    ban		Ban
    brb		BRB
    busy	Busy
    buttons	Buttons  
    call	Call
    cancel	Cancel
    channel	Channel
    channels	Channels
    chanop	ChanOp
    chat	Chat
    clear	Clear
    close	Close
    connect	Connect
    crypt	Crypt
    ctcp	CTCP
    dcc		DCC
    default	Default
    delete	Delete
    dismiss	Dismiss
    draw	Draw
    empty	Empty
    error	Error
    exec	Exec
    favourites	Favourites
    finger	Finger
    flush	Flush
    get		Get
    help	Help
    history	History
    hostname	Hostname
    info	Info
    invite	Invite
    irc		IRC
    ircname	Ircname
    join	Join
    jump	Jump
    keep	Keep
    key		Key
    kick	Kick
    kill	Kill
    leave	Leave
    log		Log
    limit	Limit
    list	List
    message	Message
    messages	Messages
    mode	Mode
    moderated	Moderated
    monitor	Monitor
    name	Name
    names	Names
    new		New
    nickname	Nickname
    nocase	Nocase
    notice	Notice
    notify	Notify
    offer	Offer
    ok		OK
    open	Open
    operator	Operator
    parameter	Parameter
    parameters	Parameters
    password	Password
    pattern	Pattern
    people	People
    plugin	{Plug In}
    port	Port
    private	Private
    quiet	Quiet
    quit	Quit
    reconnecting	Reconnecting
    refresh	Refresh
    register	Register
    reject	Reject
    script	Script
    secret	Secret
    send	Send
    server	Server
    servers	Servers
    service	Service
    services	Services
    set		Set
    shutdown	Shutdown
    signoff	Signoff
    sound	Sound
    speak	Speak
    text	Text
    time	Time
    topic	Topic
    unban	Unban
    user	User
    users	Users
    view	View
    who		Who
    whois	Whois
    whowas	Whowas
    windows	Windows
}
#
# Compounds of the above that are no language dependent
#
array set ztrans "
    bankick	{$ztrans(ban)+$ztrans(kick)}
"
