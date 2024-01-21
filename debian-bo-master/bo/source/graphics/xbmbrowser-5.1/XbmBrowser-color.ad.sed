!
! XbmBrowser Resources (version 5.0)
!
XbmBrowser.width:      500
XbmBrowser.height:     600
XbmBrowser.iconName:   browser

! These are preset during make -- change at own risk
!XbmBrowser.cmd_rc:      /config/to/force/load 
!XbmBrowser.user_rc:     ==USERS_RC==
!XbmBrowser.library_rc:  ==LIBRARY_RC==

! Buttons along the top
*mainmenu.label:      Main Menu
*options.label:       Options
*rescan.label:        Rescan
*scan.label:          Full Scan
*mainmenu.leftBitmap: menu12
*options.leftBitmap:  menu12

! ------------------------------------
! Options Menu Defaults
!
! Space for check box bitmaps
*optmenu*leftMargin:        24
! Xaw3d Library Resource (all menus)
*SimpleMenu*shadowWidth:     1
!
! --- Display Sytle ---
*optmenu.solid_bgnd.label:  Shaped Windows
*optmenu.shape_syms.label:  Shape Symbols
*optmenu.label_all.label:   Label All Files
*optmenu.label_icons.label: Label Icons
*optmenu.label_syms.label:  Label Symbols
*optmenu.label_dirs.label:  Label Dirs
! --- Show What Files ---
*optmenu.icons_only.label:  Icons Only
*optmenu.show_dir.label:    Directories
*optmenu.show_xpmbad.label: Bad X Pixmaps
*optmenu.show_other.label:  Other Files
*optmenu.show_hidden.label: Hidden Files
! --- Scanning Method ---
*optmenu.recursive.label:   Recursive Scan

! ------------------------------------
! Option Menu default settings
!
! --- Display Sytle ---
! Defaults depends on your display type
!XbmBrowser.solid_bgnd:         False
!XbmBrowser.shape_syms:         False
!XbmBrowser.label_all:          False
!XbmBrowser.label_icons:        False
!XbmBrowser.label_syms:         False
!XbmBrowser.label_dirs:         False
! --- Show What Files ---
!XbmBrowser.icons_only:         False
!XbmBrowser.show_dir:           True
!XbmBrowser.show_xpmbad:        True
!XbmBrowser.show_other:         False
!XbmBrowser.show_hidden:        False
! --- Scanning Method --- 
! Ensure recursive scan is off at start
!XbmBrowser.recursive:          False

%==COLOR_SETTINGS_DIVIDER==
! ------------------------------------
! Color settings symbols and bitmap icons
XbmBrowser.sym_foreground:      black
XbmBrowser.sym_background:      lemon chiffon
XbmBrowser.stipple_background:  pale green
!
! Background Color Settings
XbmBrowser.icon_foreground:     black
XbmBrowser.icon_background:     white
XbmBrowser.icon_transparent:    beige
XbmBrowser.solid_background:    MediumSeaGreen
!
! Try these backgrounds instead
!XbmBrowser.icon_foreground:    black
!XbmBrowser.icon_background:    grey
!XbmBrowser.icon_transparent:   grey
!XbmBrowser.solid_background:   grey

! Simple Color Selections for Xt Programs using the
! Standard Color Table of Anthony's Icon Library
*Command.background:        sky blue
*Toggle.background:         sky blue
*MenuButton.background:     sky blue
*Label.background:          lemon chiffon
*Box.background:            lemon chiffon
*Form.background:           lemon chiffon
*SimpleMenu*background:     wheat
*List.background:           wheat
*Dialog.background:         wheat
*Dialog.Label.background:   wheat
*Text*background:           white

