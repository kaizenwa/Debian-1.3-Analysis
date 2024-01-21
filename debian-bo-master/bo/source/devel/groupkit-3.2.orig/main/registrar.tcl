#
#
# MODULE:   registrar.tcl (Central conference registrar)
# 
#


#########################################################################
# initialization stuff and some utility routines
#

_gk_initGkErrorHandling

gk_newenv userprefs
gk_newenv registrar
gk_newenv -server regclients

catch {source ~/.groupkitrc}

# catch command line argument for using a different port
#
foreach i $argv { 
  if {[string range $i 0 1] == "-p"} {
    registrar port [string range $i 2 end]
  }
}
if {[registrar port] == ""} {registrar port 9068}


# here is our environment holding conferences and users
#
gk_newenv confs


# create our server so people can connect to us
#
gk_createServer regclients [registrar port] ""


# counter so that all conferences and users have a unique number
#
registrar myID 0

proc _gk_newId {} {
  registrar myID [expr [registrar myID]+1]
  return [registrar myID]
}



#########################################################################
# routines which inspect and modify registrar's lists
#

# create a new conference;  the parameter is a keyed list containing 
#    information about the conference;  we assign the conference number
# 
proc _gk_newConference conf {	
  confs import c.[_gk_newId] $conf
}


# delete a conference given its id number
#
proc _gk_deleteConference id {
  confs delete c.$id
}


# send the list of conferences to all of our connected registrar clients.
# no changes to the conference list (from the previous two routines) are
# broadcast until this routine is called
#
proc _gk_dispConference {} { 
  foreach i [regclients keys remote] {
    dp_RDO [regclients remote.$i.filedesc] _gk_confList [confs c]
  }
}

# add a user to a particular conference.  the user parameter is a keyed
#    list containing information about the user; we assign the user number
#
proc _gk_addUser {conf user} {	
  confs import c.$conf.users.[_gk_newId] $user
}


# delete a user given a conference and user number
#
proc _gk_deleteUser {conf user} {
  confs delete c.$conf.users.$user
}


# send the list of users of the given conference to all connected registrar
#    clients.  no changes to the list (from the previous two routines) are
#    broadcast until this routine is called
#
proc _gk_dispUsers {conf} {	
  foreach i [regclients keys remote] {
    dp_RDO [regclients remote.$i.filedesc] _gk_userList $conf [confs c.$conf.users]
  }
}


# provide info about a regclient
# 

proc _gk_provideRCinfo {key val} {
  foreach i [regclients keys remote] {
    if {[rpcFile]==[regclients remote.$i.filedesc]} {
       regclients remote.$i.$key $val
    }
  }
}

proc _gk_changeConfInfo {confnum info val} {
  if {[member $confnum [confs keys c]]} {
    confs c.$confnum.$info $val
  }
  foreach i [regclients keys remote] {
    dp_RDO [regclients remote.$i.filedesc] _gk_dochangeConfInfo $confnum $info $val
  }
}


proc _gk_relayToRC {rc args} {
  dp_RDO [regclients remote.$rc.filedesc] eval $args
}

vwait xx
