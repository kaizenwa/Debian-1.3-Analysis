#
# Default user menu tutorial for  xbmbrowser  (version 5.0)
#
# Dated:    9 July 1995
#
# Original Programmer:  Ashly Roll         ash@cit.gu.edu.au
# Current Programmer:   Anthony Thyssen    anthony@cit.gu.edu.au
#
# ----------------------------------------------------------------------------
#
# The purpose of this file to to define a set of default user menus to
# perform a number of actions.  This file is an example only.  Users making
# heavy use of the xbmbrowser program is expected to make a copy of this
# file into a file named ".xbmbrowserrc" in thier own home directory.
#
# Each line of this file consists of either :-
#
#  # comment line
#      A comment line which is completely ignored.
#
#  menu "main"  
#  menu "main" "Main Menu"
#      Add any new menu elements (see below) to this menu.
#      The menu so defined will then popup as and when the appropriate
#      mouse button is pressed. Currently only the following user defined
#      menus can be used by the user. If the menu is not defined the program
#      will `beep' the user as an indication of that fact :-
#
#        "main"       The menu which pops up under the menu button when
#                       pressed. Generally this is used a menu of directories
#                       the user likes to visit. Warning no file is selected
#                       by the user when using this menu so some substitutions
#                       may be empty strings. (See Substitions below)
#        "global"     A menu of global actions which will popup when either
#                       the first two mouse buttons are are pressed on a
#                       displayed icon or file symbol or any mouse button on
#                       the background of the icon area. (See NOTE below)
#                       If the pointer was not over a displayed icon or
#                       symbol, no filename, basename, or suffix will be
#                       defined. (See function `selected()' below)
#        "bitmap"     Display this menu on any displayed X bitmap with the
#                       right most (menu) mouse button.
#        "pixmap"     As "bitmap", but for any X pixmap (or bad load pixmap)
#        "directory"  Same again, but for directory symbols.
#        "other"      Again, for any other file symbol (text, binary..)
#
#      NOTE: If button 1 (leftmost or select mouse button) is pressed
#      on a directory symbol, the browser will automaticaly decend into that
#      directory, instead of poping up the "global" menu.
#
#  line
#      Just insert a line into the menu at this point.
#
#  item "Delete"   confirm("Really delete %f?") \
#                        exec("rm '%f'")      rescan()
#      Insert a item into the current menu which will execute the sequence
#      builtin functions (see below). As it is posible for a very long
#      sequences to be required for some menu items, the menu lines can be
#      continued onto the next line by `backslashing' or `escaping' the
#      return character at the end of the line.
#
#      Each function may or may-not require some quoted string arguments,
#      with the quote being either single or double, allowing the other
#      quote to be used freely with the argument.
#
#      Each argument can contain any number of macro substitutions which
#      consist of a % character followed by a single letter. A percent
#      character can be substituted with %%.
#
#   
# The following builtin functions are currently available :-
#
#     quit()            Exit xbmbrowser. Need I say more?
#     scan()            Completely scan the current directory (Again)
#                         NOTE: This is the only way to try to load BadXpm's
#     rescan()          Do a fast rescan of the directory
#     chdir("dir")      Change directory to the given directory (will scan())
#     exec("command")   Execute the given bourne shell command(s)
#     confirm("prompt") Ask the user to confirm action before continuing on
#                         the next function given for this menu item.
#     input("prompt","initial")
#                       Ask the user for some input, with the prompt given
#                         and a default initial string.
#     selected()        If the user pointer was NOT over an icon or symbol
#                        then abort the current sequence with a popup error.
#                        This function is not usful in anything but the
#                        "global" menu, as in the other menus an icon is
#                        either always or never selected.
#
# The folowing are substition macros you can use within the string
# arguments.
# 
#        %d - The current directory of the browser (location of the file)
#        %f - The filename of the icon (or file) the user menu in on
#        %b - The basename (suffix removed) of the current filename
#        %s - The suffix of the current filename  EG: ".xbm"
#        %i - The result of the last input() query to the user.
#        %h - The Users Home directory (do NOT use ~ in an argument for this)
#        %D - The Initial Startup directory (Where Xbmbrowser Started)
#        %% - Substitutes a percent character.
#
# NOTES: 
#    The Full path of a selected file is   %d%f
#    Also   %f  and  %b%s  results in the same substition.
#
# WARNING: the substitution macros %f, %b and %s will be an empty string if
# the users pointer was not over a display icon or file symbol. See the
# function selected() above.
#
# ----------------------------------------------------------------------

menu "main"
#
# The main menu appears under the menu button at the top of xbmbrowser.
# It provides a place for general controls of the program such as
# quiting the program, opening a new browser, and selecting directories
# which the user likes to jump to on a regular basis.
#
# NOTE: I do not supply a title to this menu as the menu button itself is
# the title in this case (it is not a springloaded menu like the others).
#
# WARNING: %f, %b and %s substition macros will all be empty for
# any function sequence in this menu. Also the function selected()
# will always abort the function sequence for the same reason.
#

# A table of menu items of places the user likes to jump to.
# Add your own directory jumps here.
item "CD .."       chdir("..")
item "CD Start"    chdir("%D")
item "CD Home"     chdir("~/")
item "CD Bitmaps"  chdir("==X_BITMAPS==")

# Place a line accross the menu to seperate the next items
line

# Execute a simple command to start a new xbmbrowser
#    Assuming it is on your command PATH.
#    NOTE the use of input to set the %i substitute string
item "New Browser.."   input("New Browser","%d") exec("xbmbrowser '%i' &")

# More Global Commands not requiring any specific file selection
item "Text Editor.."   input("File to Edit","") \
                         exec("xterm -g 80x40 -name Vi -T Vi -n Vi -e vi %i &")
item "Make Dir.."      input("Make Directory","") exec("mkdir '%i'")
item "Execute Cmd.."   input("Command to Execute","") exec('%i')
line

# Provide a means for the user to exit the program.
#    NOTE: this item may be automatically added in a future
#    version of xbmbrowser. But not yet!
item "Quit Browser.."  confirm("Really Quit?") quit()


# -------------------------
menu "global" "Global Menu"
#
# The global menu can be poped up anywhere within the icon display area.
# and is generally used for general actions common to all file types.
# IE: general file commands such as renaming, deleting and text editing.
#
# The global menu may or may not have a file selected when poped up by the
# user.  As such the substitions %f, %b and %s could be empty strings.  The
# selected() function prevents the user getting himself into trouble
# because of this.
#

# Rename or Move a file 
#   NOTE the appropiate quoting for the shell command)
#   and the use of selected() here to ensure a file is pointed to.
item "Rename"        selected() input( "Rename File", '%f') \
                                exec("mv '%f' '%i'") rescan()

# Copy a file elsewhere
item "Copy"          selected() input("Copy File",'%f') \
                                exec("cp '%f' '%i'") rescan()

# Duplicate this file (no user input required)
#    NOTE: use of base and suffix
item "Duplicate"     selected() exec("cp '%f' '%b_2%s'") rescan()

# Delete file with confirm
item "Delete"        selected() confirm("Really Delete `%f' ?") \
                                exec("rm '%f'") rescan()

line

# General Command Execute
#    NOTE: that selected is not needed to prevent %f being an empty string
item "Execute"       input("Command to Execute","  %f") exec("%i")

# Make a test editor available globally (don't wait for it either)
#    NOTE that no quotes are used around %f so that the user can't
#    edit a empty file in case no file is selected by the user.
item "Text Editor"   exec("xterm -e ${VISUAL:-${EDITOR:-vi}} %f &")


# ----------------------------------------------------------------------
# popup the appropriate menu below on third mouse button
# ----------------------------------------------------------------------

# -------------------------
menu "bitmap" "Bitmap Menu"

item "Bitmap Edit"   exec("bitmap '%f' &")
item "XV Edit"       exec("xv '%f' &")
item "SetRoot"       exec("xsetroot -bitmap '%f'")
item "SetRoot Inv"   exec("xsetroot -rv -bitmap '%f'")

line    # PbmPlus Filters
line
# NOTE: the next item uses multi line shell arguemnt!
#   Don't backslash the end of line within an argument.
#   While this is allowed do not make it too long or an error may result
item "Invert"  \
   exec( "xbmtopbm '%f' | pnminvert | pbmtoxbm > '%b.tmp';
           if [ -s '%b.tmp' ]; then mv '%b.tmp' '%f'; else rm '%b.tmp'; fi" ) \
   rescan()
#
# This is better as it is unlikly to make xbmbrowser to barf on large filenames
item "Crop"  \
  exec( "xbmtopbm '%f' | pnmcrop 2>/dev/null | pbmtoxbm > '%b.tmp'" )  \
  exec( "if [ -s '%b.tmp' ]; then mv '%b.tmp' '%f'; else rm '%b.tmp'; fi" )\
  rescan()
#
# the following is a `tight' shell code to do the same thing
item "Add Margin" \
  exec("xbmtopbm '%f'|pnmmargin 5|pbmtoxbm>'%b.t'&&mv '%b.t' '%f'||rm '%b.t'")\
  rescan()
item "Flip Horz" \
  exec("xbmtopbm '%f'|pnmflip -lr|pbmtoxbm>'%b.t'&&mv '%b.t' '%f'||rm '%b.t'")\
  rescan()
item "Flip Vert" \
  exec("xbmtopbm '%f'|pnmflip -tb|pbmtoxbm>'%b.t'&&mv '%b.t' '%f'||rm '%b.t'")\
  rescan()
item "Create Mask" \
  exec("xbmtopbm '%f' | pbmmask | pbmtoxbm > '%b_mask%s'")\
  rescan()
item "`' Mask Ext" \
  exec("xbmtopbm '%f' | pbmmask -expand | pbmtoxbm > '%b_mask%s'")\
  rescan()
#
## The better technique is to use a set shell script filters.
##
## Some example filters   xbm-cmd  and  xbm-resize  is provided as
## an example of what you can do in a shell script.  These filters
## are cutdown versions of various programs provided with release 1.6
## of  ``Anthony's Icon Library''  from ftp.x.org:/contrib/icons/AIcons.
##
## These filters works with both X bitmaps and X pixmaps without change.
## Using scripts like these result in very simple and easy menu setups
##                                                      -- Anthony Thyssen
#
#item "Invert"      exec("xbm-cmd '%f' pnminvert")    rescan()
#item "Flip Horz"   exec("xbm-cmd '%f' pnmflip -lr")  rescan()
#item "Flip Vert"   exec("xbm-cmd '%f' pnmflip -tb")  rescan()
#item "Mask"        input("Mask Name", '%f') \
#                   exec("cp '%f' '%i'; xbm-cmd '%i' pbmmask") rescan()
#item "Mask Ext"    input("Mask Name", '%f') \
#                   exec("cp '%f' '%i'; xbm-cmd '%i' pbmmask -expand") rescan()
#line 
#item "Resize crop"  exec("xbm-cmd '%f' pnmcrop 2>/dev/null")   rescan()
#item "Resize expd"  exec("xbm-cmd '%f' pnmmargin 5")           rescan()
#item "Resize std"   exec("xbm-resize '%f' >/dev/null")         rescan()
#item "Resize 16"    exec("xbm-resize -16 '%f' >/dev/null")     rescan()
#item "Resize 32"    exec("xbm-resize -32 '%f' >/dev/null")     rescan()
#item "Resize 48"    exec("xbm-resize -48 '%f' >/dev/null")     rescan()
#item "Resize 64"    exec("xbm-resize -64 '%f' >/dev/null")     rescan()
#item "Resize 64x38" exec("xbm-resize -64x38 '%f' >/dev/null")  rescan()
#item "Resize input" input("Resize to","") \
#                    exec("xbm-resize -%i '%f' >/dev/null")     rescan()
#
line  # PbmPlus Converters
line
item ">Pixmap"      exec("xbmtopbm '%f' | ppmtoxpm > '%b.xpm'" )  rescan()

# -------------------------
menu "pixmap" "Pixmap Menu"

item "Pixmap Edit" exec("pixmap -f '%f' &")
item "XPaint Edit" exec("xpaint '%f' &")
item "SetRoot"     exec("xloadimage -quiet -onroot '%f' &")
item "SetRoot xv"  exec("xv -root -noresetroot '%f' -quit &")
# Use this version if xv does not understand X pixmaps (v3.01 and up)
#item "SetRoot xv"  exec("xpmtoppm '%f' | xv -root -noresetroot - -quit &")

line  # PbmPlus Filters
line
item "Crop" \
  exec("xpmtoppm '%f'| pnmcrop |ppmtoxpm>'%b.t'&&mv '%b.t' '%f'||rm '%b.t'")\
  rescan()
item "Add Margin" \
  exec("xpmtoppm '%f'|pnmmargin 5|ppmtoxpm>'%b.t'&&mv '%b.t' '%f'||rm '%b.t'")\
  rescan()
item "Flip Horz" \
  exec("xpmtoppm '%f'|pnmflip -lr|ppmtoxpm>'%b.t'&&mv '%b.t' '%f'||rm '%b.t'")\
  rescan()
item "Flip Vert" \
  exec("xpmtoppm '%f'|pnmflip -tb|ppmtoxpm>'%b.t'&&mv '%b.t' '%f'||rm '%b.t'")\
  rescan()

line # PbmPlus Converters
line
item ">Xbm Thr" exec("xpmtoppm '%f' | ppmtopgm | 
                pgmtopbm -threshold | pbmtoxbm > '%b.xbm'" )  rescan()
item ">Xbm Dth" exec("xpmtoppm '%f' | ppmtopgm | pgmnorm 2>/dev/null |
                pgmtopbm -fs | pbmtoxbm > '%b.xbm'" )         rescan()
item ">Gif"     exec("xpmtoppm '%f' | ppmtogif > '%b.gif'" )  rescan()

# -------------------------
menu "directory" "Dir Menu"

item "Goto.."      chdir('%f')
item "New Browser" exec("xbmbrowser '%f' &")
line
item "Delete"      exec("rmdir '%f'")   # this is fairly safe
#item "Delete Dir"  confirm("DANGER -- Really Delete ALL of `%f' -- DANGER")\
#                   exec("rm -fr '%f'")   # this is very dangerous!

# -----------------------
menu "other" "Other Menu"

item "Text Edit"  exec("xterm -e ${VISUAL:-${EDITOR:-vi}} '%f' &")
item "XV Image"   exec("xv '%f' &")

line  # PbmPlus Convertors
line
item "Icon -> Xbm" \
  exec("icontopbm '%f' | pbmtoxbm > '%b.xbm' ")     rescan()
item "Gif -> Xpm" \
  exec("giftopnm '%f' | ppmquant 256 2>/dev/null |
        ppmtoxpm 2>/dev/null > '%b.xpm' ")  rescan()

# -----------------------

