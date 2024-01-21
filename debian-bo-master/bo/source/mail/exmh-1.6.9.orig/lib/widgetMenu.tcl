#
# This was hacked upon by <John@LoVerso.Southborough.MA.US>, 4/94.
#
# From: allan@piano.sta.sub.org (Allan Brighton)
# Newsgroups: comp.lang.tcl
# Subject: Re: How do I post a menu from a canvas item ??
# Message-ID: <1895@piano.stasys.UUCP>
# Date: 23 Mar 94 10:41:06 GMT
#
#In an article mh@awds.imsd.contel.com (Michael Hoegeman) writes:
#>How do you get a menu posted from a canvas and make it behave like one
#>that was posted from a menubutton and get cascades and the like to work
#>properly? I'd much rather use someone else's soultion rather than slog it
#>out myself.
#
#I've been using these routines to deal with menus in a canvas.
#They are not perfect, but they work. Suggestions for improvements
#are welcome.
#
# setup canvas bindings
#
# $canvas bind $tag <ButtonPress-1> "menu_post $menu $canvas %X %Y"
# $canvas bind $tag <ButtonRelease-1> "menu_unpost $menu $canvas"

#
# To avoid internal text tag grab problems, use these bindings
#	bind $tw <ButtonPress-3> {text_menu_post %W %x %y %X %Y}
#	bind $tw <Any-ButtonRelease-3> {text_menu_unpost %W}
# and use a tag to determine which menu to invoke.
#

proc text_menu_post {w wx wy x y} {
	global tk_priv
	set tags [$w tag names @$wx,$wy]
	Exmh_Debug $tags
	set tk_priv(textmenu) {}
	foreach tag $tags {
	    catch {
		if {[winfo class $w.$tag] == "Menu"} {
		    set tk_priv(textmenu) $w.$tag
		}
	    }
	}
	catch {menu_post $tk_priv(textmenu) $w $x $y}
}
proc text_menu_unpost {w} {
	global tk_priv
	catch {menu_unpost $tk_priv(textmenu) $w}
	catch {unset tk_priv(textmenu)}
}

# post the given menu at the given position in the widget (or canvas) w

proc menu_post {menu w x y} {
	global tk_priv

	$menu activate none
	$menu post $x $y
	set tk_priv(cursor) [lindex [$w config -cursor] 4]
	$w config -cursor arrow
	grab set $w
}


# unpost the given menu in the widget

proc menu_unpost {menu w} {
	global tk_priv

	$menu unpost
	catch {$w config -cursor $tk_priv(cursor)}
	grab release $w
}


# invoke the selected item in the menu, if any

proc menu_invoke {menu w} {
    set i [$menu index active]
    menu_unpost $menu $w
    if {$i != "none"} {
	$menu invoke $i
    }
}


# setup the bindings for a local widget menu

proc menu_bind {menu w} {
    global tk_version
    if {$tk_version >= 4.0} {
	bindtags $menu $menu
    }
    bind $menu <Any-ButtonPress> {}
    bind $menu <Any-ButtonRelease> "menu_invoke $menu $w"
    bind $menu <2> { }
    bind $menu <B2-Motion> { }
    bind $menu <Any-Motion> {%W activate @%y}
    bind $menu <Any-Enter> {%W activate @%y}
    bind $menu <Any-Leave> {%W activate none}
}
