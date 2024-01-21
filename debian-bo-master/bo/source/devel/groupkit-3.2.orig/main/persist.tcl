#
# handle conference persistence
#


# initialization; set up default values for persistence host etc.
# if not supplied 

proc _gk_setup_persist {} {
  if {[users local.persistenceStyle]==""} {
    if {[userprefs persist_style]!=""} {
      users local.persistenceStyle [userprefs persist_style]
    } else {
      users local.persistenceStyle prompt
    }
  }
  if {[users local.persistenceHost]==""} {
    users local.persistenceHost [users local.persisthost]
  }
  if {[users local.persistencePort]==""} {
    users local.persistencePort 8888
  }
}


# handle persistence; main routine, invoked by conf.tcl when the last
# user leaves the conference.  checks the persistence style and acts
# accordingly. 
#  - returns 1 if the conference should then quit, 0 otherwise

proc _gk_HandlePersistence {} {
  global _gk_persistFlag _gk_persistServerFD
  set _gk_persistFlag none

  _gk_setup_persist

  if {[users keys remote]==""} {
    set persStyle [users local.persistenceStyle]
    set persHost [users local.persistenceHost]
    set persPort [users local.persistencePort]
    if {($persHost=="")||($persPort=="")} {
      set persStyle none
    }
    switch $persStyle {
      always {set _gk_persistFlag yes}
      prompt {_gk_promptPersist; tkwait window .persist}
      default {set _gk_persistFlag no}
    }

    if {$_gk_persistFlag=="yes"} {
      keylset event type userDeleted usernum [users local.usernum]
      gk_postEvent $event

      set _gk_persistServerFD [gk_connectToServer $persHost $persPort ""]
      gk_toUserNum persist_server gk_persist_startRecording \
	[users local.confnum]
      keylset event type updateEntrant usernum persist_server
      gk_postEvent $event
      gk_toUserNum persist_server gk_persist_stopRecording
      after 5000 close $_gk_persistServerFD
      catch {
          dp_RDO [registrar fd] _gk_reqChangeConfInfo [users local.confnum] \
	    will_persist yes
      }
    } elseif {$_gk_persistFlag=="no"} {
      catch {
          dp_RDO [registrar fd] _gk_reqChangeConfInfo [users local.confnum] \
	    will_persist no
      }
    }
  }
  if {$_gk_persistFlag!="cancel"} {
    return 1
  } else {
    return 0
  }
}


# prompt the user if they would like the conference to persist
# modify the global _gk_persistFlag when done

proc _gk_promptPersist {} {
  toplevel .persist
  message .persist.msg -aspect 600 -text "You are the last person in this conference.  Save its contents before quitting?"
  frame .persist.buttons
  button .persist.buttons.save -text "Save Contents" -command "_gk_persistResult yes"
  button .persist.buttons.del -text "Delete Conference" -command "_gk_persistResult no"
  button .persist.buttons.cancel -text Cancel -command "_gk_persistResult cancel"
  pack append .persist.buttons \
    .persist.buttons.save {left padx 10 pady 10} \
    .persist.buttons.del {left padx 10 pady 10} \
    .persist.buttons.cancel {left padx 10 pady 10}
  pack append .persist \
    .persist.msg {top padx 10 pady 10} \
    .persist.buttons {top padx 10 pady 10}
}

proc _gk_persistResult result {  global _gk_persistFlag
  set _gk_persistFlag $result
  destroy .persist
}


# reload a previously-saved conference from the persistence server

proc _gk_confDoPersist {} {
  _gk_setup_persist
  set fd [gk_connectToServer [users local.persistenceHost] \
	  [users local.persistencePort] ""]
  dp_RDO $fd gk_persist_playBack [users local.confnum]
  after 5000 close $fd
}

