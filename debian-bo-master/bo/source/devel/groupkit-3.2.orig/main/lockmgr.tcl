## NOTE: - callbacks should be per request, not per lockname
##       - shouldn't need a separate message to send callback on successful set
##         (should just pass along id)
##       - id should have usernum associated with it

proc gk_lockmanager { name } {

    gk_env $name
    $name option set ignore_errors yes

    # Set the environment to create the following commands:
    #	1.  request -->		request a lock
    #	2.  release -->		release a lock
    #	3.  owner -->		return owner of lock
    #	4.  locks -->		return list of current locks
    #	5.  destroy -->		destroy the lockmanager (use builtin)
    #	6.  setconflictproc --> conflict resolution procedure
    #	7.  delay -->		delay hook for demo purposes
    #	8.  status -->		The local status of a request
    $name command set request		_gk_lock_request
    $name command set release		_gk_lock_release
    $name command set owner		_gk_lock_getowner
    $name command set locks		_gk_lock_getall_locks
    $name command set setconflictproc	_gk_lock_setconflictproc
    $name command set delay		_gk_lock_delay
    $name command set status		_gk_lock_status

    # Set the default conflict resolution procedure.
    $name set conflictproc		_gk_lock_default_conflictproc

    # Set some handy "directories" in the environment.
    # The set of all locks.
    $name set locks	""

    # The set of all callbacks.
    $name set callbacks	""

    # The set of status of "requests".
    $name set done	""

    # Set the global counter for transaction identifiers.
    $name set counter 0

    gk_bind updateEntrant "_gk_lock_update_entrant $name %U"
    gk_bind userDeleted "_gk_lock_UserHasDeparted $name %U"

    $name set misc.delay ""
}

proc _gk_lock_request { env cmd lockname callback {timeout 100000}} {

    # Store the callback in the environment.
    $env set callbacks.$lockname $callback

    # Set the "done" to be requested but not fulfilled.  Used in timeout.
    set id [$env get counter]
    $env set done.$id Pending

    # Send this message to the unique user, which may be myself.
    gk_toUserNum [_gk_getUniqueUser] \
	    _gk_lock_serialize_set $env $lockname [users local.usernum] $id

    # Set the timeout time and begin counting.  If we do time out, check where
    # or not the request was processed.
    after $timeout "if { [$env get done.$id] == \"Pending\" } {
	[$env get callbacks.$lockname] \"Failed\"}"
	
    $env set counter [expr $id + 1]

    return $id
}

proc _gk_lock_status { env cmd id } {
    return [$env get done.$id]
}

proc _gk_lock_serialize_set { env lockname usernum id } {
    gk_debug "SERIALIZER:\tSetting $lockname."

    # Check to see if we have a lock conflict.
    if { [[$env get conflictproc] $env $lockname] == 0 } {
	# Lock is not set, set it on all hosts.
	gk_toAll _gk_lock_set $env $lockname $usernum
	after 1 gk_toUserNum $usernum _gk_lock_exec_callback \
		$env $lockname $id "Succeeded"
    } else {
	# Lock is set, signal failed on request host.
	if {[$env get misc.delay]!=""} {
	    after [$env get misc.delay] gk_toUserNum $usernum \
		    _gk_lock_exec_callback $env $lockname $id "Failed"

	} else {
	    after 1 gk_toUserNum $usernum _gk_lock_exec_callback $env \
		    $lockname $id "Failed"
	}
    }
}

proc _gk_lock_exec_callback { env lockname id status } {
    $env set done.$id $status

    if [member $lockname [$env keys callbacks]] {
	eval "[$env get callbacks.$lockname] $status"
    }
}

proc _gk_lock_delay { env lockname delay } {
    $env set misc.delay $delay
}

proc _gk_lock_set { env lockname usernum } {
    # Signal we are "done".
    $env set done.$lockname Succeeded

    # Set the lock.
    # $env set locks.$lockname $lockname

    # Set the owner.
    $env set locks.$lockname.owner $usernum
}

proc _gk_lock_release { env cmd lockname } {
    # Send this message to the unique user, which may be myself.
    gk_toUserNum [_gk_getUniqueUser] \
	    _gk_lock_serialize_release $env $lockname [users local.usernum]
}

proc _gk_lock_serialize_release { env lockname usernum } {
    if { [_gk_lock_getowner $env "" $lockname] == $usernum } {
	gk_debug "SERIALIZER:\tReleasing $lockname"
	gk_toAll _gk_lock_del $env $lockname
    } else {
	gk_debug "SERIALIZER:\t\[users remote.$usernum.username] does not own $lockname."
    }
}

proc _gk_lock_del { env lockname } {
    if [member $lockname [$env keys locks]] {
	$env delete locks.$lockname
	$env delete done.$lockname
	$env delete callbacks.$lockname
    } else {
	gk_debug "LockMgr ERROR:  Unable to delete nonexistent lock $lockname."
    }


}

proc _gk_lock_getowner { env cmd lockname } {
    if { [member $lockname [$env keys locks]] == 0 } {
	# Lock does not exist.
	return ""
    } else {
	# If the conflictproc found a lock, it exists.  Return its owner.
	return [$env get locks.$lockname.owner]
    }
}

# Return the set of all locks from the local cache.  This may be somehat
# out of date; however, with a relatively fast network, this will be 
# very infrequent.
proc _gk_lock_getall_locks { env cmd } {
    return [$env keys locks]
}

# Sets the conflict resolution procedure.  If this procedure returns
# with the value 1 if the lock is found in the env, or 0 if it is not.
proc _gk_lock_default_conflictproc { env lockname } {
    return [member $lockname [$env keys locks]]
}

proc _gk_lock_setconflictproc { env cmd newproc } {
    $env set conflictproc	$newproc
}

# Discard.
# Send the locking environment to any new users.
proc _gk_lock_UpdateNewUser { env cmd usernum } {
    if {[_gk_getUniqueUser] == [users local.usernum]} {
	# Update the new client by sending the current lock environment.
	gk_toUserNum $usernum $env _recv [$env updateEntrant]
    }
}

# Delete any locks owned by a user that has departed.
proc _gk_lock_UserHasDeparted { env usernum } {
    set all_locks [$env locks]

    # If a user departs, then delete all of the user's currently held locks.
    foreach i $all_locks {
	if { [$env get locks.$i.owner] == $usernum } {
	    $env delete locks.$i
	    $env delete done.$i
	    $env delete callbacks.$i
	}
    }
}

proc _gk_lock_update_env { env list } {
    set listlength [llength $list]
    for { set i 0 } { $i < $listlength } { set i [expr $i + 2] } {
	$env set [lindex $list $i] [lindex $list [expr $i + 1]]
    }
}

proc _gk_lock_update_entrant { env usernum } {
    gk_toUserNum $usernum _gk_lock_update_env $env [$env updateEntrant]
}
