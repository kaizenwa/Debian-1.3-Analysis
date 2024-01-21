#
# Module: server.tcl
# 
# Description:
#   This module provides a level on top of Tcl-DP's MakeRPCClient/Server
#   routines.  The extra functionality is there to maintain a list of 
#   clients connected to the server, along with information about each
#   of these clients.  See the user documentation for usage information.
#
# Internals:
#   The internal environment "gk_server" on the server side is used to 
#   hold: the name of the environment containing information on each
#   user (env); the port number the server is running on (port); the
#   keyfield for incoming client information (key), and a counter used
#   for assigning keys when a keyfield is not specified (nextid).
#
# Obsoletes:
#   This obsoletes the "connections" list formerly maintained in rpc.tcl,
#   as well as some of the previous code found in the registrar clients
#   and conferences.
#

gk_newenv gk_server

proc gk_createServer {env port keyfield {info ""}} {
  gk_server port [dp_MakeRPCServer $port]
  gk_server env $env
  gk_server key $keyfield
  gk_server nextid 1
  $env import local $info
  return [gk_server port]  
}

proc gk_connectToServer {host port info {echo 0}} {
  set fd [dp_MakeRPCClient $host $port]
  dp_RDO $fd _gk_serverAcceptConnection $info $echo
  return $fd
}

proc gk_connectToPeer {host port} {
  set fd [gk_connectToServer $host $port [[gk_server env] local] 1]
  return $fd
}

proc gk_shutdownServer {} {
  set env [gk_server env]
  foreach i [$env keys remote] {
    catch {close [$env remote.$i.filedesc]}
  }
}

proc _gk_serverAcceptConnection {info {echo 0}} {
  dp_atclose [rpcFile] append "_gk_serverRemoveConnection [rpcFile]"  
  keylset info filedesc [rpcFile]
  if {[gk_server key]==""} {
    set key [gk_server nextid]
    gk_server nextid [expr $key+1]
  } else {
    set key [keylget info [gk_server key]]
  }
  [gk_server env] import remote.$key $info
  if {$echo==1} {
    dp_RDO [rpcFile] _gk_serverAcceptConnection [[gk_server env] local]
  }
  keylset event type clientConnected key $key
  gk_postEvent $event
}


proc _gk_serverRemoveConnection {filedesc} {
  set env [gk_server env]
  foreach i [$env keys remote] {
    if {[$env remote.$i.filedesc]==$filedesc} {
      keylset event type clientDisconnected key $i
      gk_postEvent $event
      $env delete remote.$i
      return	
    }
  }
}

proc gk_serverQueryID callback {
  set env [gk_server env] 
  foreach i [$env keys remote] {
    if {[$env remote.$i.filedesc]==[rpcFile]} {
      eval "dp_RDO [rpcFile] $callback $i"
    }
  }
}
