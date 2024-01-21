proc gk_newenv {args} {
  set share no; set notify no; set client no; set server no; set bind no;
  set serialize no
  foreach i $args {
    if {[string compare $i "-notify"]==0} {set notify yes}
    if {[string compare $i "-share"]==0} {set share yes}
    if {[string compare $i "-client"]==0} {set client yes}
    if {[string compare $i "-server"]==0} {set server yes}
    if {[string compare $i "-bind"]==0} {set bind yes}
    if {[string compare $i "-serialize"]==0} {set serialize yes}
  }
  set env [lindex $args [expr [llength $args]-1]]
  gk_env $env
  $env command rename _implicit unknown
  $env command set import _gk_envimport
#  $env command set _receive _gk_shareenv_receive
  $env command set _recv _gk_newrcv
  $env option set ignore_errors yes
  if {$notify=="yes"} {_gk_env_initNotification $env}
  if {$bind=="yes"} {_gk_env_initBinding $env}
  if {$share=="yes"} {_gk_env_initSharing $env}
  if {$client=="yes"} {_gk_env_initClient $env}
  if {$server=="yes"} {_gk_env_initServer $env}
  if {$serialize=="yes"} {_gk_env_initSerialized $env}
}

#
#
# procedure to handle importing of information into environments
#

proc _gk_envimport {env cmd key val} {
  if {$val!=""} {_gk_auximport $env $key $val "" "" set}
}

proc _gk_auximport {env key val base next setcmd} {
  if {($base!="")&&($next!="")} {set name $base.$next} else {set name $base$next}
  set err [catch {set keys [keylkeys val $name]}]
  if {$err!=0} {
    if {$key=="-root"} {set target $base} else {set target $key.$base}
    $env $setcmd $target [keylget val $base]
  } else {
    foreach i $keys {_gk_auximport $env $key $val $name $i $setcmd}
  }
}

proc _gk_shareenv_receive {env cmd data} {
  $env option set inhibit_notify yes
  $env import -root $data
  $env option set expect_update no
  foreach i [$env option get pending] {
    $env set [lindex $i 0] [lindex $i 1]
  }
  $env option set inhibit_notify no
  if {[$env option get notify]=="yes"} {
    $env _notify envReceived ""
  }
}

proc _gk_newrcv {env cmd data} {
  $env option set inhibit_notify yes
  set len [llength $data]
    for {set i 0} {$i<$len} {incr i 2} {
	set key [lindex $data $i]
	set value [lindex $data [expr $i+1]]
	$env set $key $value
    }
  $env option set expect_update no
#  foreach i [$env option get pending] {
#    $env set [lindex $i 0] [lindex $i 1]
#  }
  $env option set inhibit_notify no
  if {[$env option get notify]=="yes"} {
    $env _notify envReceived ""
  }
}

#
#
# hooks to provide for notification
#

proc _gk_env_initNotification env {
    $env command rename delete _delete
    $env command set delete _gk_notifyenv_del
    $env command rename set _set  
    $env command set set _gk_notifyenv_set
    $env command set _notify _gk_notifyenv_notify
    $env option set notify yes
}

proc _gk_notifyenv_del {env cmd key} {
  if {[$env exists $key]==1} {
    $env _delete $key
    $env _notify deleteEnvInfo $key
  }
}

proc _gk_notifyenv_set {env cmd key val} {
  set exists [$env exists $key]
  $env _set $key $val
  set evtype [expr {$exists?"changeEnvInfo":"addEnvInfo"}]
  $env _notify $evtype $key
}

proc _gk_notifyenv_notify {env cmd type key} {
  if {[$env option get inhibit_notify]!="yes"} {
    keylset event type $type key $key env $env
    gk_postEvent $event
  }
}


#
#
# hooks to provide notification via bindings
#

proc _gk_env_initBinding env {
  if {![member _notify [$env command list]]} {
    _gk_env_initNotification $env
  } 
  $env command set _notify _gk_bindenv_notify
  $env command set bind _gk_bindenv_bind
  $env command set delbind _gk_bindenv_delbind
  $env option set binding_table [gk_notifier -anonymous]
}

proc _gk_bindenv_notify {env cmd type key} {
  if {[$env option get inhibit_notify]!="yes"} {
    [$env option get binding_table] notify $type [list [list K $key]]
  }
}

proc _gk_bindenv_bind {env cmd event script} {
  return [[$env option get binding_table] bind $event $script]
}

proc _gk_bindenv_delbind {env cmd binding} {
  [$env option get binding_table] delete $binding
}


#
#
# hooks to provide for environment sharing
#

proc _gk_env_initSharing env {
    $env command rename set _doset 
    $env command set set _gk_shareenv_set
    $env command rename delete _dodelete
    $env command set delete _gk_shareenv_del
    $env option set share_policy no_concurrency
#    gk_bind updateEntrant "_gk_shareenv_update $env %U"
    gk_bind updateEntrant "_gk_shareenv_newupdate $env %U"
    gk_bind newUserArrived "_gk_shareenv_newuserhack $env %U"
    if {![gk_amOriginator]} {$env option set expect_update yes}
}


proc _gk_shareenv_set {env cmd key val} {
  if {[$env option get expect_update]=="yes"} {
    $env _doset $key $val 
    if {[$env option get inhibit_notify]!="yes"} {
      set pending [$env option get pending]
      set new [list $key $val]
      lappend pending $new
      $env option set pending $pending
    }
  } else {
    gk_toAll $env _doset $key $val
  }
}

proc _gk_shareenv_del {env cmd key} {
  gk_toAll $env _dodelete $key
}

proc _gk_shareenv_update {env usernum} {
  gk_toUserNum $usernum $env _receive [$env get -root]
}

proc _gk_shareenv_newupdate {env usernum} {
  gk_toUserNum $usernum $env _recv [$env updateEntrant]
}

### if a new user connects up to us, who has a smaller user number
### than we do, then send them our pending set messages.
proc _gk_shareenv_newuserhack {env usernum} {
    if {$usernum<[users local.usernum]} {
	foreach i [$env option get pending] {
	    gk_toUserNum $usernum $env _doset [lindex $i 0] [lindex $i 1]
	}
    }
}


#
#
# hooks to create a "server" (centralized) environment
#

proc _gk_env_initServer env {
  $env command rename set _doset
  $env command set set _gk_serverenv_set
  $env command rename delete _dodel
  $env command set delete _gk_serverenv_del
  $env command set _newclient _gk_serverenv_newclient
  $env command set _unlink _gk_serverenv_delclient
}

proc _gk_serverenv_set {env cmd key val} {
  $env _doset $key $val
  foreach i [$env option get server.clients] {
    dp_RDO $i $env _doset $key $val
  }
}

proc _gk_serverenv_del {env cmd key} {
  $env _dodel $key
  foreach i [$env option get server.clients] {
    dp_RDO $i $env _dodel $key
  }
} 

proc _gk_serverenv_newclient {env cmd} {
  set list [$env option get server.clients]
  lappend list [rpcFile]
  $env option set server.clients $list
  dp_RDO [rpcFile] $env _recv [$env updateEntrant]
  dp_atclose [rpcFile] append "$env _unlink [rpcFile]"
}

proc _gk_serverenv_delclient {env cmd fd} {
  set list [$env option get server.clients]
  set posn [lsearch $list $fd]
  if {$posn!=-1} {
    $env option set server.clients [lreplace $list $posn $posn]
  }
}

#
#
# hook to create a "client" (centralized) environment
#

proc _gk_env_initClient env {
  $env command rename set _doset
  $env command set set _gk_clientenv_set
  $env command rename delete _dodel
  $env command set delete _gk_clientenv_del
  $env command set server _gk_clientenv_server
  $env command set import _gk_clientenv_import
}

proc _gk_clientenv_set {env cmd key val} {
  # send to owner
  dp_RDO [$env option get client.owner] $env set $key $val
}

proc _gk_clientenv_del {env cmd key} {
  # send to owner
  dp_RDO [$env option get client.owner] $env delete $key
}

proc _gk_clientenv_server {env cmd filedesc} {
  # inform owner
  $env option set client.owner $filedesc
  dp_RDO $filedesc $env _newclient
}

proc _gk_clientenv_import {env cmd key val} {
  # assume FOR NOW imports only come from owner!!!!
  if {$val!=""} {_gk_auximport $env $key $val "" "" _doset}
}


#
#
# hook to create serialized environments in a groupkit conference
#

proc _gk_env_initSerialized env {
  if {[$env option get share_policy]==""} {
    _gk_env_initSharing $env
  }
  $env command rename set _broadcast_set
  $env command rename delete _broadcast_delete
  $env command set set _gk_serialized_set
  $env command set delete _gk_serialized_del
  $env command set _receive _gk_serialized_receive
  }

proc _gk_serialized_set {env cmd key val} {
  if {[_gk_getUniqueUser]==[users local.usernum]} {
    $env _broadcast_set $key $val
  } else {
    gk_toUserNum [_gk_getUniqueUser] $env $cmd $key $val
  }
}

proc _gk_serialized_del {env cmd key} {
  if {[_gk_getUniqueUser]==[users local.usernum]} {
    $env _broadcast_delete $key
  } else {
    gk_toUserNum [_gk_getUniqueUser] $env $cmd $key 
  }
}

proc _gk_serialized_receive {env cmd data} {
  $env option set inhibit_notify yes
  _gk_auximport $env -root $data "" "" _set
  $env option set expect_update no
  $env option set inhibit_notify no
  if {[$env option get notify]=="yes"} {
    $env _notify envReceived ""
  }
}
