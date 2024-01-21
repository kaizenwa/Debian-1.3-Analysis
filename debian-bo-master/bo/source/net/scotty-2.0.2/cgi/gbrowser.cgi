#! /usr/local/bin/scotty -nf
##
## A CGI script for the CERN httpd to browse GDMO files through WWW.
##
## Copyright (c) 1995
##
## M. Kernchen (e-mail: M.Kernchen@tu-bs.de)
## TU Braunschweig, Germany
## Institute for Operating Systems and Computer Networks
##
## Permission to use, copy, modify, and distribute this
## software and its documentation for any purpose and without
## fee is hereby granted, provided that the above copyright
## notice appear in all copies.  The University of Braunschweig
## makes no representations about the suitability of this
## software for any purpose.  It is provided "as is" without
## express or implied warranty.
##

# path for the cgi-script (see http.conf, this script should stay there!):
set gdmobrowser /ibr/cgi-bin/gbrowser.cgi

# because a class contains everything that follows (except namebindings)
set icon(class) \
	"<IMG ALT=\"\[CLASS\]\" SRC=\"/httpd-internal-icons/directory.xbm\">"
# because a package is an index to the other elements
set icon(package) \
	"<IMG ALT=\"\[PACKG\]\" SRC=\"/httpd-internal-icons/index.xbm\">"
# because a parameter is the smallest piece of GDMO-information
set icon(parameter) \
	"<IMG ALT=\"\[PARAM\]\" SRC=\"/httpd-internal-icons/binary.xbm\">"
# because a namebinding should solve the naming problems
set icon(namebinding) \
	"<IMG ALT=\"\[NAMEB\]\" SRC=\"/httpd-internal-icons/index2.xbm\">"
# because a attribute is a very small unit
set icon(attribute) \
	"<IMG ALT=\"\[ATTRB\]\" SRC=\"/httpd-internal-icons/gzip.xbm\">"
# because a group are tared attributes
set icon(group) \
	"<IMG ALT=\"\[GROUP\]\" SRC=\"/httpd-internal-icons/tar.xbm\">"
# because in a movie there is often action
set icon(action) \
	"<IMG ALT=\"\[ACTIO\]\" SRC=\"/httpd-internal-icons/movie.xbm\">"
# because a notification should alarm you like a sound
set icon(notification) \
	"<IMG ALT=\"\[NOTIF\]\" SRC=\"/httpd-internal-icons/sound.xbm\">"
# because a behaviour only consist of text
set icon(behaviour) \
	"<IMG ALT=\"\[BEHAV\]\" SRC=\"/httpd-internal-icons/text.xbm\">"

##
## Escape the special meanings of some HTML special characters
##

proc escapeHTML { text } {
    ## XXX maybe that this escapes are not complete!
    ## be careful with the right order
    regsub -all {&} $text {\&amp;} text 
    regsub -all {<} $text {\&lt;} text
    regsub -all {>} $text {\&gt;} text
    regsub -all {"} $text {\&quot;} text

    return $text
}
    
##
## Start a new html page. Writes a suitable header section.
##

proc StartPage { title } {

    puts "Content-type: text/html"
    puts ""

    puts "<HTML>"
    puts "<HEAD><title>$title</title></HEAD>"
    puts "<BODY>"

    flush stdout
}

##
## End a page and the script.
##

proc EndPage {} {
    puts "</BODY></HTML>"
    exit
}

##
## puts a link for label to browse content of its definition
##

proc PutsHREF { file {type {}} {label {}} } {
    global gdmobrowser icon

    puts "$icon($type)"
    ## remember the problems with " "communicationError !! (-nonewline)
    puts -nonewline "<A HREF=\"$gdmobrowser?FILE=$file&TYPE=$type&"
    puts "LABEL=$label\">$label</A>"
}

##
## some procedures to simply list template-labels (plus some more infos)
##

proc ListMOClasses { file {label {}} {title {}} } {

    if {$label == ""} {
	set result [gdmo class]
	foreach elem [lsort $result] {
	    PutsHREF $file class $elem
	    puts "<BR>"
	}
    } else {
	set result [gdmo class $label superior]
	if {$result != ""} {
	    puts "<DL>"
	    puts "<DT>$title"
	    foreach moclass $result {
		puts "<DD>"
		PutsHREF $file class $moclass
	    }
	    puts "</DL>"
	}
    }
}

proc ListNameBindings { file } {

    set result [gdmo namebinding]
    foreach elem [lsort $result] {
	PutsHREF $file namebinding $elem
        puts "<BR>"
    }
}

proc ListPackages { file {label {}} {option {}} {title {}} } {

    if {$label == ""} {
	set result [gdmo package]
	foreach pckg [lsort $result] {
	    PutsHREF $file package $pckg
	    puts "<BR>"
	}
    } else {
	set result [gdmo class $label $option]
	if {$result != ""} {
	    puts "<DL>"
	    puts "<DT>$title"
	    if {$option != "conditional"} {
		foreach pckg $result {
		    puts "<DD>"
		    PutsHREF $file package $pckg
		}
	    } else {
		puts "<DL>"
		foreach pckg $result {
		    puts "<DT>"
		    PutsHREF $file package [lindex $pckg 0]
		    puts "<DD>"
		    puts "<B>PRESENT IF</B>"
		    puts "[lindex $pckg 1]"
		}
		puts "</DL>"
	    }
	    puts "</DL>"
	}
    }
}

proc ListBehaviours { file {type {}} {label {}} {title {}} } {

    if {$label == ""} {
	set result [gdmo behaviour]
	foreach behav [lsort $result] {
	    PutsHREF $file behaviour $behav
	    puts "<BR>"
	}
    } else {
	set result [gdmo $type $label behaviours]
	if {$result != ""} {
	    puts "<DL>"
	    puts "<DT>$title"
	    foreach behav $result {
		puts "<DD>"
		PutsHREF $file behaviour $behav
	    }
	    puts "</DL>"
	}
    }
}

proc ListAttributes { file {type {}} {label {}} {title {}} } {

    if {$label == ""} {
	set result [gdmo attribute]
	foreach attr [lsort $result] {
	    PutsHREF $file attribute $attr
	    puts "<BR>"
	}
    } else {
	set result [gdmo $type $label attributes]
	if {$result != ""} {
	    puts "<DL>"
	    puts "<DT>$title"
	    puts "<DL>"
	    foreach attr $result {
		puts "<DT>"
		PutsHREF $file attribute [lindex $attr 0]
		if {[lindex $attr 1] != "" || [lindex $attr 2] != ""} {
		    foreach elem [lindex $attr 1] {
			if {[lindex $elem 0] != ""} {
			    puts "<DD>"
			    ## ??
			    puts "<B>[lindex $elem 0]</B>"
			    if {[lindex $elem 1] != ""} {
				puts "[lindex $elem 1]"
			    }
			}
		    }
		    foreach param [lindex $attr 2] {
			puts "<DD>"
			PutsHREF $file parameter $param
		    }
		}
	    }
	    puts "</DL></DL>"
	}
    }
}

proc ListGroups { file {label {}} {title {}} } {

    if {$label == ""} {
	set result [gdmo group]
	foreach group [lsort $result] {
	    PutsHREF $file group [lindex $group 0]
	    puts "<BR>"
	}
    } else {
	set result [gdmo package $label groups]
	if {$result != ""} {
	    puts "<DL>"
	    puts "<DT>$title"
	    foreach group $result {
		puts "<DD>"
		PutsHREF $file group [lindex $group 0]
		if {[lindex $group 1] != ""} {
		    puts "<DL>"
		    foreach param [lindex $group 1] {
			puts "<DD>"
			PutsHREF $file parameter $param
		    }
		    puts "</DL>"
		}
	    }
	    puts "</DL>"
	}
    }
}

proc ListActions { file {label {}} {title {}} } {

    if {$label == ""} {
	set result [gdmo action]
	foreach action [lsort $result] {
	    PutsHREF $file action [lindex $action 0]
	    puts "<BR>"
	}
    } else {
	set result [gdmo package $label actions]
	if {$result != ""} {
	    puts "<DL>"
	    puts "<DT>$title"
	    foreach action $result {
		puts "<DD>"
		PutsHREF $file action [lindex $action 0]
		if {[lindex $action 1] != ""} {
		    puts "<DL>"
		    foreach param [lindex $action 1] {
			puts "<DD>"
			PutsHREF $file parameter $param
		    }
		    puts "</DL>"
		}
	    }
	    puts "</DL>"
	}
    }
}

proc ListNotifications { file {label {}} {title {}} } {

    if {$label == ""} {
	set result [gdmo notification]
	foreach notif [lsort $result] {
	    PutsHREF $file notification [lindex $notif 0]
	    puts "<BR>"
	}
    } else {
	set result [gdmo package $label notifications]
	if {$result != ""} {
	    puts "<DL>"
	    puts "<DT>$title"
	    foreach notif $result {
		puts "<DD>"
		PutsHREF $file notification [lindex $notif 0]
		if {[lindex $notif 1] != ""} {
		    puts "<DL>"
		    foreach param [lindex $notif 1] {
			puts "<DD>"
			PutsHREF $file parameter $param
		    }
		    puts "</DL>"
		}
	    }
	    puts "</DL>"
	}
    }
}

proc ListParameters { file {type {}} {label {}} {title {}} } {

    if {$label == ""} {
	set result [gdmo parameter]
	foreach param [lsort $result] {
	    PutsHREF $file parameter $param
	    puts "<BR>"
	}
    } else {
	set result [gdmo $type $label parameters]
	if {$result != ""} {
	    puts "<DL>"
	    puts "<DT>$title"
	    foreach param $result {
		puts "<DD>"
		PutsHREF $file parameter $param
	    }
	    puts "</DL>"
	}
    }
}

##
## some procedures to list the definition for template-labels
## (plus the crossreferences to them)
##

proc BrowseClass { file type label } {
    puts "<H2>MANAGED OBJECT CLASS: $label</H2>"
    ListMOClasses $file $label "<B>DERIVED FROM</B>"
    ListPackages $file $label mandatory "<B>CHARACTERIZED BY</B>"
    ListPackages $file $label conditional "<B>CONDITIONAL PACKAGES</B>"
    puts "<DL><DT><B>REGISTERED AS</B>"
    puts "<DD>[gdmo $type $label oid]</DL>"

    puts "<HR>"

    puts "<H3>Crossreference for $label</H3>"
    # and now search for cross-references
    # superclass of another MOClass
    # superclass in a namebinding
    # subclass in a namebinding

    set putTitle 1
    foreach elem [lsort [gdmo $type]] {
	if {[lsearch -exact [gdmo $type $elem superior] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Subordinate Managed Object Class(es)</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file $type $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo namebinding]]
    ## puts "<DL><DT><B>Subclass in Name Binding</B>"
    foreach elem $result {
	if {[lsearch -exact \
		[gdmo namebinding $elem subordinate] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Subordinate Object Class in Name Binding</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file namebinding $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    ## puts "<DL><DT><B>Superclass in Name Binding</B>"
    foreach elem $result {
	if {[lsearch -exact [gdmo namebinding $elem superior] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Superior Object Class in Name Binding</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file namebinding $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }
}

proc BrowsePackage { file type label } {
    puts "<H2>PACKAGE: $label</H2>"
    ListBehaviours $file $type $label "<B>BEHAVIOUR</B>"
    ListAttributes $file $type $label "<B>ATTRIBUTES</B>"
    ListGroups $file $label "<B>ATTRIBUTE GROUPS</B>"
    ListActions $file $label "<B>ACTIONS</B>"
    ListNotifications $file $label "<B>NOTIFICATIONS</B>"
    set result [gdmo $type $label oid]
    if {$result != ""} {
	puts "<DL><DT><B>REGISTERED AS</B>"
	puts "<DD>$result</DL>"
    }

    puts "<HR>"

    puts "<H3>Crossreference for $label</H3>"
    # and now search for cross-references
    # mandatory package in MOClass
    # conditional package in MOClass

    set putTitle 1
    set result [lsort [gdmo class]]
    foreach elem $result {
	if {[lsearch -exact [gdmo class $elem mandatory] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Mandatory Package in Managed Object Class</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file class $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    foreach elem $result {
	set found 0
	foreach pair [gdmo class $elem conditional] {
	    if {[lindex $pair 0] == $label} { set found 1 }
	}
	if {$found} {
	    if {$putTitle} {
		puts "<DL>"
		puts "<DT><B>Conditional Package in Managed Object Class</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file class $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }
}

proc BrowseParameter { file type label } {
    puts "<H2>PARAMETER: $label</H2>"
    set result [gdmo $type $label context]
    ## $result kann auch token sein <B>$result</B> !
    ## sind die folgenden drei zeilen korrekt?
    if {[regsub {^ACTION-INFO$|^ACTION-REPLY$|^EVENT-INFO$|^EVENT-REPLY$\
	    |^SPECIFIC-ERROR$} $result {<B>&</B>} dummy] == 1} {
        set result $dummy
    }
    puts "<DL><DT><B>CONTEXT</B> <DD>$result</DL>"
    set result [gdmo $type $label choice]
    puts "<DL>"
    if {[lindex $result 0] != ""} {
	puts "<DT><B>WITH SYNTAX</B> <DD>[lindex $result 0]"
    } else {
	puts "<DT><B>ATTRIBUTE</B>"
	puts "<DD>"
	PutsHREF $file attribute [lindex $result 1]
    }
    puts "</DL>"
    ListBehaviours $file $type $label "<B>BEHAVIOUR</B>"
    set result [gdmo $type $label oid]
    if {$result != ""} {
	puts "<DL><DT><B>REGISTERED AS</B>"
	puts "<DD>$result</DL>"
    }

    puts "<HR>"

    puts "<H3>Crossreference for $label</H3>"
    # and now search for cross-references
    # mandatory package in MOClass
    # conditional package in MOClass

    set putTitle 1
    set result [lsort [gdmo package]]
    ## puts "<DL><DT><B>Parameter in Package</B>"
    foreach elem $result {
	set found 0
	foreach attrlist [gdmo package $elem attributes] {
	    if {[lsearch -exact [lindex $attrlist 2] $label] != -1} {
		set found 1
	    }
	}
	foreach actionlist [gdmo package $elem actions] {
	    if {[lsearch -exact [lindex $actionlist 1] $label] != -1} {
		set found 1
	    }
	}
	foreach notiflist [gdmo package $elem notifications] {
	    if {[lsearch -exact [lindex $notiflist 1] $label] != -1} {
		set found 1
	    }
	}
	if {$found} {
	    if {$putTitle} {
		puts "<DL><DT><B>Package</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file package $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo namebinding]]
    ## puts "<DL><DT><B>Parameter in Name Binding</B>"
    foreach elem $result {
	set found 0
	if {[lsearch -exact \
		[lindex [gdmo namebinding $elem create] 1] $label] != -1} {
	    set found 1
	}
	if {[lsearch -exact \
		[lindex [gdmo namebinding $elem delete] 1] $label] != -1} {
	    set found 1
	}
	if {$found} {
	    if {$putTitle} {
		puts "<DL><DT><B>Name Binding</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file namebinding $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo attribute]]
    ## puts "<DL><DT><B>Parameter in Attribute</B>"
    foreach elem $result {
	if {[lsearch -exact [gdmo attribute $elem parameters] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Attribute</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file attribute $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo action]]
    ## puts "<DL><DT><B>Parameter in Action</B>"
    foreach elem $result {
	if {[lsearch -exact [gdmo action $elem parameters] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Action</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file action $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo notification]]
    ## puts "<DL><DT><B>Parameter in Notification</B>"
    foreach elem $result {
	if {[lsearch -exact \
		[gdmo notification $elem parameters] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Notification</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file notification $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }
}

proc BrowseNamebinding { file type label } {
    puts "<H2>NAME BINDING: $label</H2>"
    set result [gdmo $type $label subordinate]
    puts "<DL><DT><B>SUBORDINATE OBJECT CLASS</B>"
    puts "<DL><DT>"
    PutsHREF $file class [lindex $result 0]
    if {[lindex $result 1]} {
	puts "<DD><B>AND SUBCLASSES</B>"
    }
    puts "</DL></DL>"
    set result [gdmo $type $label superior]
    puts "<DL><DT><B>NAMED BY SUPERIOR OBJECT CLASS</B>"
    puts "<DL><DT>"
    PutsHREF $file class [lindex $result 0]
    if {[lindex $result 1]} {
	puts "<DD><B>AND SUBCLASSES</B>"
    }
    puts "</DL></DL>"
    set result [gdmo $type $label attribute]
    puts "<DL><DT><B>WITH ATTRIBUTE</B>"
    puts "<DD>"
    PutsHREF $file attribute $result
    puts "</DL>"
    ListBehaviours $file $type $label "<B>BEHAVIOUR</B>"
    set result [gdmo $type $label create]
    if {$result != ""} {
	puts "<DL><DT><B>CREATE</B>"
	foreach modifier [lindex $result 0] {
	    puts "<DD><B>$modifier</B>"
	}
	foreach param [lindex $result 1] {
	    puts "<DD>"
	    PutsHREF $file parameter $param
	}
	puts "</DL>"
    }
    set result [gdmo $type $label delete]
    if {$result != ""} {
	puts "<DL><DT><B>DELETE</B>"
	if {[lindex $result 0] != ""} {
	    puts "<DD><B>[lindex $result 0]</B>"
	}
	foreach param [lindex $result 1] {
	    puts "<DD>"
	    PutsHREF $file parameter $param
	}
	puts "</DL>"
    }
    puts "<DL><DT><B>REGISTERED AS</B>"
    puts "<DD>[gdmo $type $label oid]</DL>"
}

proc BrowseAttribute { file type label } {
    puts "<H2>ATTRIBUTE: $label</H2>"
    set result [gdmo $type $label choice]
    puts "<DL>"
    if {[lindex $result 0] != ""} {
	puts "<DT><B>DERIVED FROM</B>"
	puts "<DD>"
	PutsHREF $file attribute [lindex $result 0]
    } else {
	puts "<DT><B>WITH ATTRIBUTE SYNTAX</B>"
	puts "<DD>[lindex $result 1]"
    }
    puts "</DL>"
    set result [gdmo $type $label matchesfor]
    if {$result != ""} {
	puts "<DL><DT><B>MATCHES FOR</B>"
	foreach quali $result {
	    puts "<DD><B>$quali</B>"
	}
	puts "</DL>"
    }
    ListBehaviours $file $type $label "<B>BEHAVIOUR</B>"
    ListParameters $file $type $label "<B>PARAMETERS</B>"
    set result [gdmo $type $label oid]
    if {$result != ""} {
	puts "<DL><DT><B>REGISTERED AS</B>"
	puts "<DD>$result</DL>"
    }
    puts "<HR>"

    puts "<H3>Crossreference for $label</H3>"

    set putTitle 1
    set result [lsort [gdmo package]]
    ## puts "<DL><DT><B>Attribute in Package</B>"
    foreach elem $result {
	set found 0
	foreach attrlist [gdmo package $elem attributes] {
	    if {[lindex $attrlist 0] == $label} {
		set found 1
	    }
	}
	foreach grouplist [gdmo package $elem groups] {
	    if {[lsearch -exact [lindex $grouplist 1] $label] != -1} {
		set found 1
	    }
	}
	if {$found} {
	    if {$putTitle} {
		puts "<DL><DT><B>Package</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file package $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo parameter]]
    ## puts "<DL><DT><B>Attribute in Parameter</B>"
    foreach elem $result {
	if {[lindex [gdmo parameter $elem choice] 1] == $label} {
	    if {$putTitle} {
		puts "<DL><DT><B>Parameter</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file parameter $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo namebinding]]
    ## puts "<DL><DT><B>Attribute in Name Binding</B>"
    foreach elem $result {
	if {[gdmo namebinding $elem attribute] == $label} {
	    if {$putTitle} {
		puts "<DL><DT><B>Name Binding</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file namebinding $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo $type]]
    ## puts "<DL><DT><B>Superior Attribute from Attribute</B>"
    foreach elem $result {
	if {[lindex [gdmo $type $elem choice] 0] == $label} {
	    if {$putTitle} {
		puts "<DL><DT><B>Attribute</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file attribute $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo group]]
    ## puts "<DL><DT><B>Attribute in Group</B>"
    foreach group $result {
	if {[lsearch -exact [gdmo group $group attributes] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Group</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file group $group
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    ## nicht so trivial noch testen XXX
    set result [lsort [gdmo notification]]
    ##puts "<DL><DT><B>And Attribute IDS in Notification</B>"
    foreach elem $result {
	set found 0
	foreach attrids [lindex [gdmo notification $elem infosyntaxandids] 1] {
	    if {[lindex $attrids 1] == $label} {
		set found 1
	    }
	}
	if {$found} {
	    if {$putTitle} {
		puts "<DL><DT><B>Notification</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file notification $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }
}

proc BrowseGroup { file type label } {
    puts "<H2>ATTRIBUTE GROUP: $label</H2>"
    ListAttributes $file $type $label "<B>GROUP ELEMENTS</B>"
    if {[gdmo $type $label fixed]} {
	puts "<DL><DT><B>FIXED</B></DL>"
    }
    set result [gdmo $type $label description]
    if {$result != ""} {
	puts "<DL><DT><B>DESCRIPTION</B>"
	puts "<DD>[escapeHTML [lindex $result 0]]</DL>"
    }
    puts "<DL><DT><B>REGISTERED AS</B>"
    puts "<DD>[gdmo $type $label oid]</DL>"

    puts "<HR>"

    puts "<H3>Crossreference for $label</H3>"

    set putTitle 1
    set result [lsort [gdmo package]]
    ## puts "<DL><DT><B>Group in Package</B>"
    foreach elem $result {
	set found 0
	foreach grouplist [gdmo package $elem groups] {
	    if {[lindex $grouplist 0] == $label} {
		set found 1
	    }
	}
	if {$found} {
	    if {$putTitle} {
		puts "<DL><DT><B>Package</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file package $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }
}

proc BrowseBehaviour { file type label } {
    puts "<H2>BEHAVIOUR: $label</H2>"
    puts "<DL><DT><B>DEFINED AS</B>"
## OLD  puts "<DD>[escapeHTML [lindex [gdmo $type $label definition] 0]] </DL>"
    puts "<DD>[escapeHTML [gdmo $type $label definition]] </DL>"

    puts "<HR>"

    puts "<H3>Crossreference for $label</H3>"

    set putTitle 1
    set result [lsort [gdmo package]]
    ## puts "<DL><DT><B>Behaviour in Package</B>"
    foreach elem $result {
	set found 0
	if {[lsearch -exact [gdmo package $elem behaviours] $label] != -1} {
	    set found 1
	}
	foreach attrlist [gdmo package $elem attributes] {
	    # XXX don't forget the value-specifiers !?
	    # eventuell noch das erste listen element auf DERIVATION RULE !
	    if {[lindex [lindex $attrlist 1] 1] == $label} {
		set found 1
	    }
	}
	if {$found} {
	    if {$putTitle} {
		puts "<DL><DT><B>Package</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file package $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo parameter]]
    ## puts "<DL><DT><B>Behaviour in Parameter</B>"
    foreach elem $result {
	if {[lsearch -exact [gdmo parameter $elem behaviours] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Parameter</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file parameter $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo namebinding]]
    ## puts "<DL><DT><B>Behaviour in Name Binding</B>"
    foreach elem $result {
	if {[lsearch -exact \
		[gdmo namebinding $elem behaviours] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Name Binding</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file namebinding $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo attribute]]
    ## puts "<DL><DT><B>Behaviour in Attribute</B>"
    foreach elem $result {
	if {[lsearch -exact [gdmo attribute $elem behaviours] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Attribute</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file attribute $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo action]]
    ## puts "<DL><DT><B>Behaviour in Action</B>"
    foreach elem $result {
	if {[lsearch -exact [gdmo action $elem behaviours] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Action</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file action $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }

    set putTitle 1
    set result [lsort [gdmo notification]]
    ## puts "<DL><DT><B>Behaviour in Notification</B>"
    foreach elem $result {
	if {[lsearch -exact \
		[gdmo notification $elem behaviours] $label] != -1} {
	    if {$putTitle} {
		puts "<DL><DT><B>Notification</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file notification $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }
}

proc BrowseAction { file type label } {
    puts "<H2>ACTION: $label</H2>"
    ListBehaviours $file $type $label "<B>BEHAVIOUR</B>"
    if {[gdmo $type $label mode]} {
	puts "<DL><DT><B>MODE CONFIRMED</B></DL>"
    }
    ListParameters $file $type $label "<B>PARAMETERS</B>"
    set result [gdmo $type $label infosyntax]
    if {$result != ""} {
	puts "<DL><DT><B>WITH INFORMATION SYNTAX</B>"
	puts "<DD>$result</DL>"
    }
    set result [gdmo $type $label replysyntax]
    if {$result != ""} {
	puts "<DL><DT><B>WITH REPLY SYNTAX</B>"
	puts "<DD>$result</DL>"
    }
    puts "<DL><DT><B>REGISTERED AS</B>"
    puts "<DD>[gdmo $type $label oid]</DL>"

    puts "<HR>"

    puts "<H3>Crossreference for $label</H3>"

    set putTitle 1
    set result [lsort [gdmo package]]
    ## puts "<DL><DT><B>Action in Package</B>"
    foreach elem $result {
	set found 0
	foreach actionlist [gdmo package $elem actions] {
	    if {[lindex $actionlist 0] == $label} {
		set found 1
	    }
	}
	if {$found} {
	    if {$putTitle} {
		puts "<DL><DT><B>Package</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file package $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }
}

proc BrowseNotification { file type label } {
    puts "<H2>NOTIFICATION: $label</H2>"
    ListBehaviours $file $type $label "<B>BEHAVIOUR</B>"
    ListParameters $file $type $label "<B>PARAMETERS</B>"
    set result [gdmo $type $label infosyntaxandids]
    if {$result != ""} {
	puts "<DL><DT><B>WITH INFORMATION SYNTAX</B>"
	puts "<DD>[lindex $result 0]</DL>"
	if {[lindex $result 1] != ""} {
	    ## XXX mind the AND ATTRIBUTE IDS !
	    puts "<DL><DT><B>AND ATTRIBUTE IDS</B>"
	    foreach pair [lindex $result 1] {
		puts "<DD>[lindex $pair 0]"
		PutsHREF $file attribute [lindex $pair 1]
	    }
	    puts "</DL>"
	}
    }
    set result [gdmo $type $label replysyntax]
    if {$result != ""} {
	puts "<DL><DT><B>WITH REPLY SYNTAX</B>"
	puts "<DD>$result</DL>"
    }
    puts "<DL><DT><B>REGISTERED AS</B>"
    puts "<DD>[gdmo $type $label oid]</DL>"

    puts "<HR>"

    puts "<H3>Crossreference for $label</H3>"

    set putTitle 1
    set result [lsort [gdmo package]]
    ## puts "<DL><DT><B>Notification in Package</B>"
    foreach elem $result {
	set found 0
	foreach notiflist [gdmo package $elem notifications] {
	    if {[lindex $notiflist 0] == $label} {
		set found 1
	    }
	}
	if {$found} {
	    if {$putTitle} {
		puts "<DL><DT><B>Package</B>"
		set putTitle 0
	    }
	    puts "<DD>"
	    PutsHREF $file package $elem
	}
    }
    if {!$putTitle} { puts "</DL>" }
}

##
## some procedures to draw the Inheritance- or Naming-trees
##

proc ListSubClasses { file subs children } {
    upvar $children childarray

    puts "<DL>"
    foreach moclass [lsort $subs] {
	puts "<DD>"
	PutsHREF $file class $moclass
	if {[info exists childarray($moclass)]} {
	    ListSubClasses $file $childarray($moclass) childarray
	}
    }
    puts "</DL>"
}

proc ListInheritanceTree { file } {

    puts "<H2>Inheritance Tree</H2>"

    set result [gdmo class]
    if {$result != ""} {
	if {[lsearch -exact $result top] != -1} {
	    foreach moclass $result {
		set supermos [gdmo class $moclass superior]
		foreach elem $supermos {
		    lappend children($elem) $moclass
		}
	    }
	    PutsHREF $file class top
	    ListSubClasses $file $children(top) children
	} else {
	    # XXX problems, when there is no class "top" shouldn't happen:
	    set children(top) $result
	    ListSubClasses $file $children(top) children
	}
    }
}

proc ListNamingTree { file } {

    puts "<H2>Naming Tree</H2>"

    set result [gdmo namebinding]
    if {$result != "" && [lsearch -exact [gdmo class] system] != -1} {
	foreach label $result {
	    set superClass [lindex [gdmo namebinding $label superior] 0]
	    set subClass [lindex [gdmo namebinding $label subordinate] 0]
	    if {[info exists children($superClass)]} {
		if {[lsearch -exact $children($superClass) $subClass] == -1} {
		    lappend children($superClass) $subClass
		}
	    } else {
		set children($superClass) $subClass
	    }
	}
	PutsHREF $file class system
	ListSubClasses $file $children(system) children
    }
}

##
## List the templates of GDMO.
##

proc ListPath { file } {
    global gdmobrowser icon

    puts "<H2>GDMO Templates</H2>"
    puts "<DL>"
    puts -nonewline "<DT>$icon(class) <A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=class&LABEL=\">class</A>"
    puts "<DD>Managed Object Class Definitions"
    puts "<P>"
    puts -nonewline "<DT>$icon(package) <A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=package&LABEL=\">package</A>"
    puts "<DD>Package Definitions"
    puts "<P>"
    puts -nonewline "<DT>$icon(parameter) <A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=parameter&LABEL=\">parameter</A>"
    puts "<DD>Parameter Definitions"
    puts "<P>"
    puts -nonewline "<DT>$icon(namebinding) <A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=namebinding&LABEL=\">namebinding</A>"
    puts "<DD>Name Binding Definitions"
    puts "<P>"
    puts -nonewline "<DT>$icon(attribute) <A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=attribute&LABEL=\">attribute</A>"
    puts "<DD>Attribute Definitions"
    puts "<P>"
    puts -nonewline "<DT>$icon(group) <A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=group&LABEL=\">group</A>"
    puts "<DD>Attribute Group Definitions"
    puts "<P>"
    puts -nonewline "<DT>$icon(action) <A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=action&LABEL=\">action</A>"
    puts "<DD>Action Definitions"
    puts "<P>"
    puts -nonewline "<DT>$icon(behaviour) <A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=behaviour&LABEL=\">behaviour</A>"
    puts "<DD>Behaviour Definitions"
    puts "<P>"
    puts -nonewline "<DT>$icon(notification) <A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=notification&LABEL=\">notification</A>"
    puts "<DD>Notification Definitions"
    puts "</DL>"
    puts "<HR>"
    puts "<H2>Hidden Structures</H2>"
    puts "<UL>"
    puts -nonewline "<LI><A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=inheritance&LABEL=\">Inheritance Tree</A>"
    puts "<P>"
    puts -nonewline "<LI><A HREF=\"$gdmobrowser?FILE=$file&"
    puts "TYPE=naming&LABEL=\">Naming Tree</A>"
    puts "</UL>"
}

##
## And here we start. The main program at the end of this file calls
## the following procs whenever a request is received without any
## further arguments. This is the toplevel dialog page.
##

proc Welcome { } {
    global gdmobrowser

    StartPage "GDMO Browser"

    puts "<H1>WWW GDMO Browser</H1>"

    puts "Welcome to the WWW GDMO Brower. This browser is a simple"
    puts "<A HREF=\"http://www.cs.tu-bs.de/~schoenw/gbrowser.cgi\">CGI "
    puts "script</A> written using the "
    ## <A HREF=\""
    ## puts "http://www.cs.tu-bs.de/ibr/projects/nm/scotty/\">scotty</A>"
    puts "<B>gdmotcl</B>"
    puts "interpreter. Report bugs or any other comments to "
    puts "<TT>M.Kernchen@tu-bs.de</TT>"
    
    puts "<HR>"

    puts "<H2>GDMO Examples:</H2>"

    puts "<DL>"
    puts "<DT><A HREF=\"$gdmobrowser?FILE=X.721\">X.721</A>"
    puts "<DD>The GDMO-definition from Appendix of CCITT Rec. X.721."
    puts "<DT><A HREF=\"$gdmobrowser?FILE=X.722\">X.722</A>"
    puts "<DD>The example GDMO-definition from Appendix of CCITT Rec. X.722."
    puts "<DT><A HREF=\"$gdmobrowser?FILE=X.739\">X.739</A>"
    puts "<DD>Metric Objects and Attributes (ITU-T X.739 / ISO 10164-11)"
    puts "<P>"
    puts "<DT><A HREF=\"$gdmobrowser?FILE=all\">ALL</A>"
    puts "<DD>All the X.7xx / ISO 10164-x standards at once."
    puts "<P>"
    puts "<DT><A HREF=\"$gdmobrowser?FILE=oim\">OIM</A>"
    puts "<DD>The Official Internet MIB as GDMO-definition."
    puts "</DL>"

    EndPage
}

##
## Browse the GDMO-template(s) of file given by type and/or label.
##

proc Browse { file type label } {

    StartPage "[string toupper $file] - GDMO Browser"

    switch $type {
	class {
	    if {$label == ""} {
		puts "<H2>MANAGED OBJECT CLASSES</H2>"
		ListMOClasses $file
	    } else {
		BrowseClass $file $type $label
	    }
	} 
	package {
	    if {$label == ""} {
		puts "<H2>PACKAGES</H2>"
		ListPackages $file
	    } else {
		BrowsePackage $file $type $label
	    }
	} 
	parameter {
	    if {$label == ""} {
		puts "<H2>PARAMETERS</H2>"
		ListParameters $file "" "" ""
	    } else {
		BrowseParameter $file $type $label
	    }
	} 
	namebinding {
	    if {$label == ""} {
		puts "<H2>NAME BINDINGS</H2>"
		ListNameBindings $file
	    } else {
		BrowseNamebinding $file $type $label
	    }
	} 
	attribute {
	    if {$label == ""} {
		puts "<H2>ATTRIBUTES</H2>"
		ListAttributes $file
	    } else {
		BrowseAttribute $file $type $label
	    }
	}
	group {
	    if {$label == ""} {
		puts "<H2>ATTRIBUTE GROUPS</H2>"
		ListGroups $file
	    } else {
		BrowseGroup $file $type $label
	    }
	}
	behaviour {
	    if {$label == ""} {
		puts "<H2>BEHAVIOURS</H2>"
		ListBehaviours $file
	    } else {
		BrowseBehaviour $file $type $label
	    }
	}
	action {
	    if {$label == ""} {
		puts "<H2>ACTIONS</H2>"
		ListActions $file
	    } else {
		BrowseAction $file $type $label
	    }
	}
	notification {
	    if {$label == ""} {
		puts "<H2>NOTIFICATIONS</H2>"
		ListNotifications $file
	    } else {
		BrowseNotification $file $type $label
	    }
	}
	inheritance {
	    ListInheritanceTree $file
	}
	naming {
	    ListNamingTree $file
	}
	default {
	    ListPath $file
	} 
    }
    EndPage
}

##
## The main program starts here. It checks the environment variable
## QUERY_STRING to decide what should be done. After parsing and checking 
## the parameters, we call the appropriate proc to do the job.
##

if {![info exists env(QUERY_STRING)] || ($env(QUERY_STRING) == "")} {
    Welcome
} else {
    set query [split $env(QUERY_STRING) &]
    set file ""
    set type ""
    set label ""
    foreach av $query {
	set a [lindex [split $av =] 0]
	set v [lindex [split $av =] 1]
	switch $a {
	    FILE   { set file $v }
	    TYPE   { set type $v }
	    LABEL  { set label $v }
	    *      { lappend args [list $a $v] }
	}
    }
    switch $file {
	X.722 {
	    gdmo load X722.gdmo
	}
	X.721 {
	    gdmo load X721.gdmo
	}
	X.739 {
	    gdmo load X739.gdmo
	}
	oim {
	    gdmo load OIM.gdmo
	}
	default {
	    gdmo load X721.gdmo
	    gdmo load X739.gdmo
	}
    }
    Browse $file $type $label
}
exit
