#
# Default user menus for  xbmbrowser  (version 5.0)
#
# For more information please look at the tutorial version of this
# file which should be located at  "==LIBRARY_RC==.tut".
#
# Dated:    9 July 1995
# 
# Original Programmer:  Ashly Roll         ash@cit.gu.edu.au
# Current Programmer:   Anthony Thyssen    anthony@cit.gu.edu.au
#

# ------------------------
menu "main"

item "CD .."         chdir("..")
item "CD Start"      chdir("%D")
item "CD Home"       chdir("~/")
item "CD X Bitmaps"  chdir("==X_BITMAPS==")
line
item "New Browser.."   input("New Browser","%d") exec("xbmbrowser '%i' &")
item "Text Editor.."   input("File to Edit","") \
                         exec("xterm -g 80x40 -name Vi -T Vi -n Vi -e vi %i &")
item "Make Dir.."      input("Make Directory","") exec("mkdir '%i'")
item "Execute Cmd.."   input("Command to Execute","") exec('%i')
line
item "Quit Browser.."  confirm("Really Quit?") quit()

# -------------------------
menu "global" "Global Menu"

item "Rename"        selected() input("Rename File", '%f') \
				exec("mv '%f' '%i'") rescan()
item "Copy"          selected() input("Copy File",'%f') \
				exec("cp '%f' '%i'") rescan()
item "Duplicate"     selected() exec("cp '%f' '%b.%s'") rescan()
item "Delete"        selected() confirm("Really Delete `%f' ?") \
				exec("rm '%f'") rescan()
line
item "Execute"       input("Command to Execute","  %f") exec("%i")
item "Text Editor"   exec("xterm -e ${VISUAL:-${EDITOR:-vi}} %f &")

# -------------------------
menu "bitmap" "Bitmap Menu"

item "Bitmap Edit"   exec("bitmap '%f' &")
item "XV Edit"       exec("xv '%f' &")
item "SetRoot"       exec("xsetroot -fg Black -bg PaleGreen -bitmap '%f'")
item "SetRoot Inv"   exec("xsetroot -rv -fg Black -bg PaleGreen -bitmap '%f'")

line    # PbmPlus Filters
line
item "Invert"  \
  exec("xbmtopbm '%f'|pnminvert|pbmtoxbm>'%b.t'&&mv '%b.t' '%f'||rm '%b.t'")\
  rescan()
item "Crop" \
  exec("xbmtopbm '%f'|pnmcrop|pbmtoxbm>'%b.t'&&mv '%b.t' '%f'||rm '%b.t'")\
  rescan()
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
item "Rename Dir"   input("Rename Dir","%f")  exec("mv '%f' '%i'")
item "Make Dir"     input("Make Directory","") exec("mkdir '%i'")
item "Delete"       exec("rmdir '%f'")   # this is fairly safe
#item "Delete Dir"  confirm("DANGER -- Really Delete ALL of `%f' -- DANGER")\
#		    exec("rm -fr '%f'")   # this is very dangerous!

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

