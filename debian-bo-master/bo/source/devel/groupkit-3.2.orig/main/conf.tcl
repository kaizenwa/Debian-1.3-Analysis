#
# 
# MODULE: conf.tcl (Generic Conference support)
#
#


#########################################################################
# initialization stuff and some utility routines
#

# this routine initializes the conference application, and should be the
# first thing called by the application conference.   it sets up all the
# information about the conference and local user, and connects back to
# our registrar clien to receive messages about newcomers etc.
#
proc gk_initConf {argv} { 
    gk_initApplication

    gk_newenv users
    userprefs host [_gk_getHostname]
    userprefs port [gk_createServer users 0 usernum $argv]
    userprefs isConference yes

    if {[users local.color]!=""} {
	userprefs color [users local.color]
    }

    gk_on {[gk_event type]=="clientDisconnected"} {
      _gk_removeUser [gk_event key]
    }  

    keylset msg confnum [users local.confnum] host [userprefs host] \
	port [userprefs port] usernum [users local.usernum]
    registrar fd [gk_connectToServer [users local.reghost] \
		  [users local.regport] $msg]  
    
    if {[users local.will_persist]=="yes"} {after 1 _gk_confDoPersist}

    wm title . [users local.confname]
}


# when a new user arrives, decide if we should update them
#

gk_on {[gk_event type]=="clientConnected"} {
   set user [gk_event key]
   keylset event type newUserArrived usernum $user
   gk_postEvent $event
   _gk_doUpdateEntrant $user
}

#########################################################################
# respond to events about users coming and going
#


# pick a "unique" user in the conference (currently the one with the
# smallest user number)
proc _gk_getUniqueUser {} {
  set smallest [users local.usernum]
  foreach i [users keys remote] {
    if {$i<$smallest} {set smallest $i}
  }
  return $smallest
}

# when newcomers join, the lowest numbered existing user is chosen to update
# the newcomer; post an "updateEntrant" event for the chosen user
#
proc _gk_doUpdateEntrant newcomer {
    if {[_gk_getUniqueUser]==[users local.usernum]} {
      keylset event type updateEntrant usernum $newcomer
      gk_postEvent $event
    }
}


# the registrar client requests us to connect to a new user and send
# them our own information
#

proc _gk_connectTo whom {
  gk_connectToPeer [keylget whom host] [keylget whom port]
}


# these calls are needed by GKSM based applications; for standard
# GroupKit, we ignore them

proc _gksm_adduser {args} { }
proc _gksm_removeuser {args} { }


# the registrar client requests us to delete the indicated user; do so and
# post a "userDeleted" event.  if the user is ourself, kill the process
#
proc _gk_removeUser usernum { 
    if {[member $usernum [users keys remote]]} {
        if {[userprefs quitInProgress]!="true"} {
	  users delete remote.$usernum
	  keylset event type userDeleted usernum $usernum
          gk_postEvent $event
        }
    }
    if {[users local.usernum] == $usernum} {_gk_deleteConf}
}


# called under various circumstances to delete the conference; nothing special
#
proc _gk_deleteConf {} {
    exit
}



#########################################################################
# routines to send messages between processes
#   - batches a set of messages together in one tcl-dp request
#

set _gk_afterSet 0

proc _gk_queueSend {who args} {
  global _gk_pending _gk_afterSet
  eval lappend _gk_pending($who) $args
  if {$_gk_afterSet==0} {
    set _gk_afterSet 1
    after 1 "_gk_doDeferredSend"
  }
}


set _gk_afterEval 0
set _gk_toBeRun ""

proc _gk_eval {args} {
  foreach i $args {
    uplevel #0 $i
  }
}

proc NEW_gk_eval {args} {
  global _gk_toBeRun _gk_afterEval
  foreach i $args {
    lappend _gk_toBeRun $i
  }
  if {$_gk_afterEval==0} {
    after 50 _gk_bgExec
    set _gk_afterEval 1
  }
}

proc _gk_bgExec {} {
  global _gk_toBeRun _gk_afterEval
  set run $_gk_toBeRun 
  set _gk_toBeRun ""
  foreach i $run {
    uplevel #0 $i
  }
  set _gk_afterEval 0  
}

proc _gk_doDeferredSend {} {
  global _gk_pending _gk_afterSet
  foreach i [array names _gk_pending] {
      eval dp_RDO $i _gk_eval $_gk_pending($i)
      unset _gk_pending($i)
  }
  set _gk_afterSet 0
}


# send the command in args to all users, including the local user.
#
proc gk_toAll args { 
    foreach i [users keys remote] { 
      _gk_queueSend [users remote.$i.filedesc] $args
    }
    uplevel #0 $args 
}


# send the command in args to remote users only.
#
proc gk_toOthers args { 
    foreach i [users keys remote] { 
      _gk_queueSend [users remote.$i.filedesc] $args
    }
}


# send the command in args to a specific user
#
proc gk_toUserNum {usernum args}  {
   if {$usernum=="persist_server"} {
     global _gk_persistServerFD
     set fd $_gk_persistServerFD
   } elseif {$usernum==[users local.usernum]} {
     uplevel #0 $args
     return
   } else {
     set fd [users remote.$usernum.filedesc]
   }
   if {$fd!=""} {
     _gk_queueSend $fd $args
   } else {
     error "no such user $usernum"
   }
}


# make sure a comment is executed in the same order on all
# processes, by serializing through a single user
proc gk_serialize args {
  if {[users local.usernum]==[_gk_getUniqueUser]} {
    eval gk_toAll $args
  } else {
    eval gk_toUserNum [_gk_getUniqueUser] gk_serialize $args
  }
}

#########################################################################
# routines to get and set information about users
#  - these will disappear soon
#

# get an attribute of a particular user
#
proc gk_getUserAttrib {usernum attrib} {
  if     {[users local.usernum]==$usernum} {return [users local.$attrib]} \
  elseif {[member $usernum [users keys remote]]} {
              return [users remote.$usernum.$attrib]
  } else {error "gk_getUserAttrib called for non-existant user"}
}


# set attribute for a particular user
#
proc gk_setUserAttrib {usernum attrib value} {
  if {[users local.usernum]==$usernum} {users local.$attrib $value} \
  else {users remote.$usernum.$attrib $value}
}

 
# get attribute of local user
#
proc gk_getLocalAttrib {attrib} {
  return [users local.$attrib]
}


# set attribute of local user
#
proc gk_setLocalAttrib {attrib value} {
  users local.$attrib $value
}

 
# predicate to determine if this process is conference originator
#
proc gk_amOriginator {} {
  set host [users local.reghost]
  set port [users local.regport]
  set orig $host$port
  if {[users local.originator]==$orig} {
      if {[users local.first_time]=="yes"} {
	  return 1
      }
  } 
  return 0
}


# return the keyed list of information on a particular user
#  - this will disappear soon
#
proc gk_findUser idnum {  
  if     {[users local.usernum]==$idnum}  {return [users local]} \
  elseif {[member $idnum [users keys remote]]} {return [users remote.$idnum]} \
  else   {return ""}
}


##############################################################
# routine to handle quitting and persistence

proc _gk_QuitConference {} {
  if {[_gk_HandlePersistence]==1} {
    userprefs quitInProgress true
    gk_shutdownServer
    after 1 "destroy ."
  }
}



##############################################################
# routines to support bind-style events as well as gk_on ones

gk_notifier _gk_notifier

proc gk_bind {event script} {
  return [_gk_notifier bind $event $script]
}

proc gk_delbind {binding} {
  return [_gk_notifier delete $binding]
}

proc gk_notify {event params} {
  _gk_notifier notify $event $params
}

gk_on 1 _gk_confGenBindEvent

proc _gk_confGenBindEvent {} {
  switch [gk_event type] {
    newUserArrived {
      _gk_notifier notify newUserArrived [list [list U [gk_event usernum]]]
    }
    userDeleted {
      _gk_notifier notify userDeleted [list [list U [gk_event usernum]]]
    }
    updateEntrant {
      _gk_notifier notify updateEntrant [list [list U [gk_event usernum]]]
    }
  }
}

