proc gk_views {w id} {
    if { [info commands views] == "" } { 
	# HACK ALERT - for some reason this needs to be done at 
	#              level 0 - otherwise it doesn't start rocessing
	#              events for this environment.
	uplevel #0 gk_newenv -share -bind views 
    }

    _gkViewsDoBindings $w $id

    # This little baby ensures that everybody adds all of the  users
    foreach i [views keys $id] {
	after idle gk_viewsCallAddUser $w $id $i
    }
}

proc gk_viewsCallAddUser {w id user} {
    if { [set coords [views get $id.$user.coords]] != "" } {
	$w adduser $id $user $coords
    }
}

#################### Commands to interace with environment #################

# The first five procecures either set the environment values or retrieve
# the values.
proc gk_viewsSetCoords {id user list} {
    views $id.$user.coords $list
}

proc gk_viewsGetCoords {id user} {
    return [views $id.$user.coords]
}

proc gk_viewsSetAttribute {id user attribute value} {
    views $id.$user.attrs.$attribute $value
}

proc gk_viewsGetAttribute {id user attribute} {
    return [views $id.$user.attrs.$attribute]
}

proc gk_viewsAttributes {id user} {
    return [views keys $id.$user.attrs]
}

# return the sorted list of users so that it is consistent between
# the different users.
proc gk_viewsUsers {id} {
    return [lsort [views keys $id]]
}

# Create the new environment.  Then copy everything from the old
# environment to the new one.
proc gk_viewsCopy {w old new} {
     gk_views $w $new
    _gkViewsDoBindings $w $new

    foreach user [views keys $old] {
	foreach i [views keys $old.$user.attrs] {
	    views set $new.$user.attrs.$i [views get $old.$user.attrs.$i]
	}
	views $new.$user.coords [views $old.$user.coords]
    }
}

# Delete a user from gk_views or all of the users for a particular
# id.  If all of the users are removed then the default bindings are
# also removed.
proc gk_viewsDelete { id {user {}} } {
    if { $user == "" } {
	_gkViewsDeleteBindings $id [users local.usernum]
	views delete $id 
    } else {
	views delete $id $user
    }
}

################## Default Environment Bindings #######################

# The default bindings for the environment.  It is expected that
# the calling widget will have the following subcommands:
#         adduser, moveuser, deleteuser, and attributechanged
#
# These are in part how the application interacts with this environment.
#
# The binding value returned are stored in two sections of the environment
# so that they can be removed, if need be.

proc _gkViewsDoBindings {w id} {
    set user [users local.usernum]
    set tags {}
    lappend tags [views bind addEnvInfo "_gkViews_addInfo $w $id %K"]
    lappend tags [views bind changeEnvInfo "_gkViews_changeInfo $w $id %K"]
    lappend tags [views bind deleteEnvInfo "_gkViews_deleteInfo $w $id %K"]
    lappend tags [views bind envReceived "_gkViews_UpdateEnvironment $w $id"]
    views $id.$user.tags $tags

    set deltag [gk_bind userDeleted "_gkViews_deleteUser $id %U"]
    views $id.$user.deltag $deltag

    # When window is resized we need to update the coordinates of everyone
    bind $w <Configure> "_gkViews_updateCoords $id"
}

# Removes all of the default bindings for the environment, all of the
# bindings set in the procedure _gkViewsDoBindings
proc _gkViewsDeleteBindings {id user} {
    foreach tag [views $id.$user.tags] {
	views delbind $tag
    }
    if { [set tag [views $id.$user.deltag]] != ""} { gk_delbind $tag }
}

####################### Event Handlers ################################

proc _gkViews_addInfo {w id key} {
    # if we are not interested in the event we do nothing.
    if ![_gkViews_interested $id $key] { return }

    # Retrieve all of the information needed
    set who [_gkViews_evalKey who $key]
    set what [_gkViews_evalKey what $key]

    if { ($what=="coords") && ([set coords [views $id.$who.coords]] != "")} {
	# call add user subcommand for $w when coords are added to env.
	$w adduser $id $who $coords
    } elseif { $what=="attrs" } {
	# call change
	set which [lindex [split $key .] 3]
	$w attributechanged $id $who $which [views get $id.$who.attrs.$which]
    }
}

proc _gkViews_changeInfo {w id key} {
    # if we are not interested in the event we do nothing.
    if ![_gkViews_interested $id $key] { return }

    # Call the corresponding subcommand for the widget "w".
    set who [_gkViews_evalKey who $key] 
    switch -exact [_gkViews_evalKey what $key] {
	attrs   { $w attributechanged $id $who [lindex [split $key .] 3] \
		[views get $key] }
	coords  { $w moveuser $id $who [views $id.$who.coords] }
	tags    { # do Nothing }
	deltag  { # do Nothing }
	default { error "Invalid views key: $key" }
    }
}

proc _gkViews_deleteInfo {w id key} {
    # if we are not interested in the event we do nothing.
    if ![_gkViews_interested $id $key] { return }

    #Call the "deleteuser" subcommand for the widget "w".
    $w deleteuser $id [_gkViews_evalKey who $key]
}

# Remove the users from the environment.
proc _gkViews_deleteUser {id who} {
    if { [lsearch [views keys $id] $who] != -1 } { views delete $id.$who}
}

# Duplicate the environment for the new user.
# NOTE: we take advantage of the fact that this is executed by the
#       new user.  Since this will be new information for the individual
#       he will be adding users.
proc _gkViews_UpdateEnvironment {w id} {
    foreach user [views keys $id] {
	if { $user != [users local.usernum] } { 
	    $w adduser $id $user [views $id.$user.coords]
	} else {
	    # make sure others know about the new guy.
	    foreach attr [views keys $id.$user.attrs]
	    gk_toOthers views $id.$user.attrs.$attr \
		    [views $id.$user.attrs.$attr]
	    gk_toOthers $w adduser $id $user [views $id.$user.coords]
	}
    }
}

####################### Internal Procedures ##########################

# if the id matched that first element of the key then we are
# interested in the event.  Return 1 if we are interested and
# 0 if we aren't.
proc _gkViews_interested {id key} {

    if { $id != "[lindex [split $key .] 0]" } {
	return 0
    } else {
	return 1
    }
}

# A little routine that will break apart the key and return either
# who's key or what portion (coord or attribute).
proc _gkViews_evalKey {need key} {
    set key [split $key .]
    switch -exact $need {
	who  { return [lindex $key 1] }
	what { return [lindex $key 2] }
    }
}

# Used when the window is resized.  Just goes through each
# user and resets their coordinates.
proc _gkViews_updateCoords {id} {
    foreach user [views keys $id] {
	views $id.$user.coords [views $id.$user.coords]
    }
}

###############################################################
