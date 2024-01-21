##############################################################################
#
# filedlg.tcl - procedures for providing a file browser
#
# Copyright (C) 1996-1997 Stewart Allen
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

##############################################################################
#

proc vTcl:get_file {mode initfile {title File}} {
    global vTcl tk_version tcl_platform tcl_version
    if {[string tolower $mode] == "open"} {
        set vTcl(file,mode) "Open"
    } else {
        set vTcl(file,mode) "Save"
    }
    if {$tk_version >= 4.2 && !($tcl_platform(platform) == "windows" && $tk_version < 8)} {
        switch $mode {
            open {
                set file [tk_getOpenFile -defaultextension .tcl \
                    -initialfile $initfile]
            }
            save {
                set file [tk_getSaveFile -defaultextension .tcl \
                    -initialfile $initfile]
            }
            return $file
        }
    } else {
        Window show .vTcl.file
        wm title .vTcl.file "$title"
        vTcl:list_files [pwd] "*.tcl"
        tkwait window .vTcl.file
        return $vTcl(file,cur)
    }
}

proc vTclWindow.vTcl.file {args} {
    global vTcl
    set base .vTcl.file
    if {[winfo exists .vTcl.file] == 1} {
        wm deiconify .vTcl.file; return
    }
    toplevel .vTcl.file \
        {-background} {#d9d9d9} {-borderwidth} {0} {-height} {0} \
        {-highlightbackground} {#d9d9d9} {-highlightcolor} {black} \
        {-highlightthickness} {0} {-relief} {flat} {-takefocus} {0} {-width} \
        {0} {-class} {vTcl}
    wm focusmodel .vTcl.file passive
    wm geometry .vTcl.file 429x255+22+137
    wm maxsize .vTcl.file 1028 753
    wm minsize .vTcl.file 104 1
    wm overrideredirect .vTcl.file 0
    wm resizable .vTcl.file 0 0
    wm deiconify .vTcl.file
    wm title .vTcl.file "File"
    frame .vTcl.file.f \
        {-background} {#d9d9d9} {-borderwidth} {2} {-height} {0} \
        {-highlightbackground} {#d9d9d9} {-highlightcolor} {black} \
        {-highlightthickness} {0} {-relief} {groove} {-takefocus} {0} {-width} \
        {0} 
    place .vTcl.file.f \
        {-x} {5} {-relx} {0} {-y} {5} {-rely} {0} {-width} {418} {-relwidth} \
        {} {-height} {241} {-relheight} {} {-anchor} {nw} 
    label .vTcl.file.f.look \
        {-anchor} {w} {-background} {#d9d9d9} {-borderwidth} {0} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {black} {-height} {0} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {black} {-highlightthickness} {0} {-justify} \
        {center} {-padx} {1} {-pady} {1} {-relief} {flat} {-takefocus} {0} \
        {-text} {Location} {-underline} {-1} {-width} {0} {-wraplength} {0} 
    place .vTcl.file.f.look \
        {-x} {10} {-relx} {0} {-y} {5} {-rely} {0} {-width} {60} {-relwidth} \
        {} {-height} {20} {-relheight} {} {-anchor} {nw} {-bordermode} \
        {ignore} 
    frame .vTcl.file.f.loc \
        {-background} {#d9d9d9} {-borderwidth} {2} {-height} {0} \
        {-highlightbackground} {#d9d9d9} {-highlightcolor} {Black} \
        {-highlightthickness} {0} {-relief} {sunken} {-takefocus} {0} {-width} \
        {0} 
    place .vTcl.file.f.loc \
        {-x} {75} {-relx} {0} {-y} {5} {-rely} {0} {-width} {276} {-relwidth} \
        {} {-height} {20} {-relheight} {} {-anchor} {nw} {-bordermode} \
        {ignore} 
    entry .vTcl.file.f.loc.e \
        {-background} {#d9d9d9} {-borderwidth} {0} {-cursor} {xterm} \
        {-exportselection} {1} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {Black} {-highlightbackground} {#d9d9d9} {-highlightcolor} {Black} \
        {-highlightthickness} {0} {-insertbackground} {Black} \
        {-insertborderwidth} {0} {-insertofftime} {300} {-insertontime} {600} \
        {-insertwidth} {2} {-justify} {left} {-relief} {flat} \
        {-selectbackground} {#c3c3c3} {-selectborderwidth} {1} \
        {-selectforeground} {Black} {-state} {disabled} {-textvariable} \
        {vTcl(file,base)} {-width} {20} 
    pack .vTcl.file.f.loc.e \
        {-in} {.vTcl.file.f.loc} {-anch} {center} {-exp} {1} {-fill} {both} \
        {-ipadx} {0} {-ipady} {0} {-padx} {0} {-pady} {0} {-side} {left} 
    button .vTcl.file.f.loc.s \
        {-activebackground} {#ececec} {-activeforeground} {Black} {-anchor} \
        {center} {-background} {#d9d9d9} {-borderwidth} {2} \
        {-disabledforeground} {#a3a3a3} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {Black} {-height} {0} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {Black} {-highlightthickness} {0} {-justify} \
        {center} {-padx} {2} {-pady} {0} {-relief} {raised} {-state} {normal} \
        {-text} {v} {-underline} {-1} {-width} {0} {-wraplength} {0} \
        {-image} {file_down} {-command} {
            vTclWindow.vTcl.filedir [winfo rootx .vTcl.file.f.loc] \
                [expr [winfo rooty .vTcl.file.f.loc] + \
                [winfo height .vTcl.file.f.loc]]
        }
    pack .vTcl.file.f.loc.s \
        {-in} {.vTcl.file.f.loc} {-anchor} {center} {-expand} {0} {-fill} {y} \
        {-ipadx} {0} {-ipady} {0} {-padx} {0} {-pady} {0} {-side} {right} 
    frame .vTcl.file.f.list \
        {-background} {#d9d9d9} {-borderwidth} {2} {-height} {30} \
        {-highlightbackground} {#d9d9d9} {-highlightcolor} {black} \
        {-highlightthickness} {0} {-relief} {groove} {-takefocus} {0} {-width} \
        {30} 
    place .vTcl.file.f.list \
        {-x} {10} {-relx} {0} {-y} {35} {-rely} {0} {-width} {395} {-relwidth} \
        {} {-height} {140} {-relheight} {} {-anchor} {nw} {-bordermode} \
        {ignore} 
    listbox .vTcl.file.f.list.l \
        {-background} {#d9d9d9} {-borderwidth} {1} {-exportselection} {1} \
        -font fixed {-foreground} {Black} {-height} {5} \
        {-highlightbackground} {#d9d9d9} {-highlightcolor} {Black} \
        {-highlightthickness} {2} {-relief} {sunken} {-selectbackground} \
        {#c3c3c3} {-selectborderwidth} {1} {-selectforeground} {Black} \
        {-selectmode} {browse} {-setgrid} {0} {-width} {20} {-yscrollcommand} \
        {.vTcl.file.f.list.sb set} 
    pack .vTcl.file.f.list.l \
        {-in} {.vTcl.file.f.list} {-anch} {center} {-exp} {1} {-fill} {both} \
        {-ipadx} {0} {-ipady} {0} {-padx} {0} {-pady} {0} {-side} {left} 
    bind .vTcl.file.f.list.l <ButtonRelease> {
        set vTcl(y) [string range [.vTcl.file.f.fname get] 3 end]
        vTcl:click_file
        break
    }
    bind .vTcl.file.f.list.l <Double-ButtonRelease> {
        set vTcl(file,name) $vTcl(y)
        vTcl:dbl_click_file
        break
    }
    scrollbar .vTcl.file.f.list.sb \
        {-activebackground} {#d9d9d9} {-activerelief} {raised} {-background} \
        {#d9d9d9} {-borderwidth} {1} {-command} {.vTcl.file.f.list.l yview} \
        {-elementborderwidth} {-1} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {Black} {-highlightthickness} {2} {-jump} {0} \
        {-orient} {vertical} {-relief} {sunken} {-repeatdelay} {300} \
        {-repeatinterval} {100} {-troughcolor} {#c3c3c3} {-width} {10} 
    pack .vTcl.file.f.list.sb \
        {-in} {.vTcl.file.f.list} {-anchor} {center} {-expand} {0} {-fill} {y} \
        {-ipadx} {0} {-ipady} {0} {-padx} {0} {-pady} {0} {-side} {right} 
    button .vTcl.file.f.short \
        {-activebackground} {#ececec} {-activeforeground} {black} {-anchor} \
        {center} {-background} {#d9d9d9} {-borderwidth} {1} {-command} \
        {vTcl:list_files . $vTcl(file,type)} \
        {-disabledforeground} {#a3a3a3} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {black} {-height} {0} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {black} {-highlightthickness} {0} {-justify} \
        {center} {-padx} {11} {-pady} {4} {-relief} {raised} {-state} {normal} \
        {-text} {.} {-underline} {-1} {-width} {0} {-wraplength} {0} 
    place .vTcl.file.f.short \
        {-x} {360} {-relx} {0} {-y} {5} {-rely} {0} {-width} {20} {-relwidth} \
        {} {-height} {20} {-relheight} {} {-anchor} {nw} {-bordermode} \
        {ignore} 
    button .vTcl.file.f.long \
        {-activebackground} {#ececec} {-activeforeground} {black} {-anchor} \
        {center} {-background} {#d9d9d9} {-borderwidth} {1} {-command} \
        {vTcl:list_files .. $vTcl(file,type)} \
        {-disabledforeground} {#a3a3a3} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {black} {-height} {0} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {black} {-highlightthickness} {0} {-justify} \
        {center} {-padx} {11} {-pady} {4} {-relief} {raised} {-state} {normal} \
        {-text} {..} {-underline} {-1} {-width} {0} {-wraplength} {0} 
    place .vTcl.file.f.long \
        {-x} {380} {-relx} {0} {-y} {5} {-rely} {0} {-width} {20} {-relwidth} \
        {} {-height} {20} {-relheight} {} {-anchor} {nw} {-bordermode} \
        {ignore} 
    label .vTcl.file.f.lab3 \
        {-anchor} {w} {-background} {#d9d9d9} {-borderwidth} {0} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {black} {-height} {0} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {black} {-highlightthickness} {0} {-justify} \
        {center} {-padx} {1} {-pady} {1} {-relief} {flat} {-takefocus} {0} \
        {-text} {File Name} {-underline} {-1} {-width} {0} {-wraplength} {0} 
    place .vTcl.file.f.lab3 \
        {-x} {10} {-relx} {0} {-y} {185} {-rely} {0} {-width} {60} {-relwidth} \
        {} {-height} {20} {-relheight} {} {-anchor} {nw} {-bordermode} \
        {ignore} 
    label .vTcl.file.f.lab4 \
        {-anchor} {w} {-background} {#d9d9d9} {-borderwidth} {0} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {black} {-height} {0} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {black} {-highlightthickness} {0} {-justify} \
        {center} {-padx} {1} {-pady} {1} {-relief} {flat} {-takefocus} {0} \
        {-text} {File Type} {-underline} {-1} {-width} {0} {-wraplength} {0} 
    place .vTcl.file.f.lab4 \
        {-x} {10} {-relx} {0} {-y} {210} {-rely} {0} {-width} {60} {-relwidth} \
        {} {-height} {20} {-relheight} {} {-anchor} {nw} {-bordermode} \
        {ignore} 
    entry .vTcl.file.f.fname \
        {-background} {#d9d9d9} {-borderwidth} {2} {-cursor} {xterm} \
        {-exportselection} {1} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {black} {-highlightbackground} {#d9d9d9} {-highlightcolor} {black} \
        {-highlightthickness} {0} {-insertbackground} {black} \
        {-insertborderwidth} {0} {-insertofftime} {300} {-insertontime} {600} \
        {-insertwidth} {2} {-justify} {left} {-relief} {sunken} \
        {-selectbackground} {#c3c3c3} {-selectborderwidth} {1} \
        {-selectforeground} {Black} {-state} {normal} {-textvariable} \
        {vTcl(file,name)} {-width} {20} 
    bind .vTcl.file.f.fname <Return> {
        vTcl:list_files [.vTcl.file.f.fname get] $vTcl(file,type)
        break
    }
    place .vTcl.file.f.fname \
        {-x} {75} {-relx} {0} {-y} {185} {-rely} {0} {-width} {236} \
        {-relwidth} {} {-height} {20} {-relheight} {} {-anchor} {nw} \
        {-bordermode} {ignore} 
    entry .vTcl.file.f.ftype \
        {-background} {#d9d9d9} {-borderwidth} {2} {-cursor} {xterm} \
        {-exportselection} {1} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {black} {-highlightbackground} {#d9d9d9} {-highlightcolor} {black} \
        {-highlightthickness} {0} {-insertbackground} {black} \
        {-insertborderwidth} {0} {-insertofftime} {300} {-insertontime} {600} \
        {-insertwidth} {2} {-justify} {left} {-relief} {sunken} \
        {-selectbackground} {#c3c3c3} {-selectborderwidth} {1} \
        {-selectforeground} {Black} {-state} {normal} {-textvariable} \
        {vTcl(file,type)} {-width} {20} 
    bind .vTcl.file.f.ftype <Return> {
        vTcl:list_files $vTcl(file,base) $vTcl(file,type)
        break
    }
    place .vTcl.file.f.ftype \
        {-x} {75} {-relx} {0} {-y} {210} {-rely} {0} {-width} {236} \
        {-relwidth} {} {-height} {20} {-relheight} {} {-anchor} {nw} \
        {-bordermode} {ignore} 
    button .vTcl.file.f.ok \
        {-activebackground} {#ececec} {-activeforeground} {black} {-anchor} \
        {center} {-background} {#d9d9d9} {-borderwidth} {2} \
        {-disabledforeground} {#a3a3a3} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {black} {-height} {0} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {black} {-highlightthickness} {0} {-justify} \
        {center} {-padx} {11} {-pady} {4} {-relief} {raised} {-state} {normal} \
        {-text} {OK} {-underline} {-1} {-width} {0} {-wraplength} {0} \
        {-command} {vTcl:list_files    [.vTcl.file.f.fname get] $vTcl(file,type)}
    place .vTcl.file.f.ok \
        {-x} {330} {-relx} {0} {-y} {183} {-rely} {0} {-width} {70} \
        {-relwidth} {} {-height} {22} {-relheight} {} {-anchor} {nw} \
        {-bordermode} {ignore} 
    button .vTcl.file.f.cancel \
        {-activebackground} {#ececec} {-activeforeground} {black} {-anchor} \
        {center} {-background} {#d9d9d9} {-borderwidth} {2} \
        {-disabledforeground} {#a3a3a3} {-font} \
        {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} {-foreground} \
        {black} {-height} {0} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {black} {-highlightthickness} {0} {-justify} \
        {center} {-padx} {11} {-pady} {4} {-relief} {raised} {-state} {normal} \
        {-text} {Cancel} {-underline} {-1} {-width} {0} {-wraplength} {0} \
        {-command} {set vTcl(file,cur) ""; destroy .vTcl.file}
    place .vTcl.file.f.cancel \
        {-x} {330} {-relx} {0} {-y} {209} {-rely} {0} {-width} {70} \
        {-relwidth} {} {-height} {22} {-relheight} {} {-anchor} {nw} \
        {-bordermode} {ignore} 
}

proc vTclWindow(post).vTcl.file {args} {
    global vTcl
    vTcl:set_balloon .vTcl.file.f.short "reload current directory"
    vTcl:set_balloon .vTcl.file.f.long "go to parent directory"
}

proc vTcl:click_file {} {
    global vTcl
    set vTcl(x) [.vTcl.file.f.list.l curselection]
    if {$vTcl(x) != ""} {
        set vTcl(file,name) \
            [string range [.vTcl.file.f.list.l get $vTcl(x)] 3 end]
        set vTcl(file,cur) [file join [pwd] $vTcl(file,name)]
    }
}

proc vTcl:dbl_click_file {} {
    global vTcl

    if {[file isdir $vTcl(file,cur)] == 1} {
        vTcl:list_files $vTcl(file,cur) $vTcl(file,type)
    } else {
        destroy .vTcl.file
    }
}

proc vTcl:find_dirs {list} {
    set output ""
    foreach i $list {
        if {[file isdir $i] == 1} {
            lappend output $i
        }
    }
    return [lsort $output]
}

proc vTcl:list_files {base type} {
    global env
    global vTcl
    if {[file isdir $base] == 1} {
        cd $base
        set vTcl(file,base) [pwd]
        set vTcl(file,list) [vTcl:find_files [pwd] $type]
        .vTcl.file.f.list.l delete 0 end
        foreach i $vTcl(file,list) {
            if [file isdir $i] {
                set dir {[] }
            } else {
                set dir {   }
            }
            set j [file split $i]
            .vTcl.file.f.list.l insert end $dir[lindex $j end]
        }
    } else {
        destroy .vTcl.file
        set vTcl(file,cur) $base
    }
}

proc vTclWindow.vTcl.filedir {x y} {
    set base .vTcl.filedir
    if {[winfo exists .vTcl.filedir] == 1} {
        wm deiconify .vTcl.filedir; return
    }
    toplevel .vTcl.filedir \
        {-background} {#d9d9d9} {-borderwidth} {0} {-height} {0} \
        {-highlightbackground} {#d9d9d9} {-highlightcolor} {Black} \
        {-highlightthickness} {0} {-relief} {flat} {-takefocus} {0} {-width} \
        {0} {-cursor} {arrow} {-class} {vTcl}
    .vTcl.filedir conf -bd 2 -relief raised
    bind .vTcl.filedir <ButtonRelease> {
        set vTcl(x) [.vTcl.filedir.lis4 cursel]
        if {$vTcl(x) != ""} {
            vTcl:list_files [.vTcl.filedir.lis4 get $vTcl(x)] $vTcl(file,type)
            grab release .vTcl.filedir
            destroy .vTcl.filedir
        }
    }
    wm focusmodel .vTcl.filedir passive
    wm geometry .vTcl.filedir 276x106+${x}+${y}
    wm maxsize .vTcl.filedir 1137 870
    wm minsize .vTcl.filedir 1 1
    wm overrideredirect .vTcl.filedir 1
    wm resizable .vTcl.filedir 1 1
    wm deiconify .vTcl.filedir
    wm title .vTcl.filedir "top1"
    scrollbar .vTcl.filedir.scr3 \
        {-activebackground} {#ececec} {-activerelief} {raised} {-background} \
        {#d9d9d9} {-borderwidth} {1} {-command} {.vTcl.filedir.lis4 yview} \
        {-elementborderwidth} {-1} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {Black} {-highlightthickness} {0} {-jump} {0} \
        {-orient} {vertical} {-relief} {sunken} {-repeatdelay} {300} \
        {-repeatinterval} {100} {-troughcolor} {#c3c3c3} {-width} {10}
    pack .vTcl.filedir.scr3 \
        {-in} {.vTcl.filedir} {-anch} {center} {-exp} {0} {-fill} {y} {-ipadx} \
        {0} {-ipady} {0} {-padx} {0} {-pady} {0} {-side} {right}
    listbox .vTcl.filedir.lis4 \
        {-background} {#d9d9d9} {-borderwidth} {1} {-exportselection} {1} \
        {-font} {-Adobe-Helvetica-Medium-R-Normal-*-*-120-*-*-*-*-*-*} \
        {-foreground} {Black} {-height} {10} {-highlightbackground} {#d9d9d9} \
        {-highlightcolor} {Black} {-highlightthickness} {0} {-relief} {sunken} \
        {-selectbackground} {#c3c3c3} {-selectborderwidth} {1} \
        {-selectforeground} {Black} {-selectmode} {browse} {-setgrid} {0} \
        {-width} {20} {-yscrollcommand} {.vTcl.filedir.scr3 set}
    pack .vTcl.filedir.lis4 \
        {-in} {.vTcl.filedir} {-anchor} {center} {-expand} {1} {-fill} \
        {both} {-ipadx} {0} {-ipady} {0} {-padx} {0} {-pady} {0} {-side} {top}
    foreach i [vTcl:dir_tree [pwd]] {
        .vTcl.filedir.lis4 insert end $i
    }
    update idletasks
    grab -global .vTcl.filedir
}

proc vTcl:dir_tree {dir} {
    set split [file split $dir]
    set out  ""
    set len [llength $split]
    for {set i 0} {$i < $len} {incr i} {
        set dir ""
        foreach j [lrange $split 0 $i] {
            set dir [file join $dir $j]
        }
        set out [linsert $out 0 $dir]
    }
    return $out
}

