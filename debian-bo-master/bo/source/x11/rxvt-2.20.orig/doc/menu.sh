#!/bin/sh --
# not necessarily useful, but an example of using the rxvt menuBar
toggle_menu () { echo -n "[?10t" }
menucmd () { echo -n "]10;$@" }
menu_clear () { menucmd "-/"; menucmd '=' }

menu_title () { menucmd '=' $1 }

show_menu () { echo -n "[?10h" }

menu () { menucmd '+' $@ }
rm_menu () { menucmd '-' $1 }

menuitem () { menucmd "+{$1} $2" }
rm_menuitem () { menucmd "-{$1}" }

separator () { menucmd "+{-}" }
rm_separator () { menucmd "-{-}" }

#-------------------------------------------------------------------------
menu_clear		# clear all menus
menu_title "rxvt-%v"

menu "/Programs"
if [ ! -z "$EDITOR" ]; then
    menuitem "Editor}{`basename $EDITOR`"	"$EDITOR\\r"
else
    menuitem "vi}{vi"	"vi\\r"
fi
menuitem "Mail"		"Mail\\r"
menuitem "News"		"slrn\\r"
menuitem "Rxvt-News"	"rxvt -e slrn&"
menuitem "Commander"	"mc\\r"
menu "../"

exit

menu "/Shell/{finger} finger\\r"
if [ ! -z $BASH ]; then
    separator
    menuitem "Version}{^X^V"
fi
menuitem ".name"	"invisible label!"
menu "../"

menu "/Terminal/Font"
menuitem "Normal"	"^@\\E]50;#\\a"
menuitem "Larger"	"^@\\E]50;#+\\a"
menuitem "Smaller"	"^@\\E]50;#-\\a"
separator
menuitem "Font 1"	"^@\\E]50;#1\\a"
menuitem "Font 2"	"^@\\E]50;#2\\a"
menuitem "Font 3"	"^@\\E]50;#3\\a"
menuitem "Font 4"	"^@\\E]50;#4\\a"
menu "../Screen"
menuitem "ReverseVideo"	"^@\\E[?5t"
menuitem "Toggle Width"	"^@\\E[?3t"
menuitem "Cursor"	"^@\\E[?25t"
menuitem "Switch"	"^@\\E[?47t"
separator
menuitem "menuBar"	"^@\\E[?10t"
menuitem "scrollBar"	"^@\\E[?30t"
menu "../Keys"
menuitem "XTerm"	"^@\\E[?35t"
menuitem "Backspace"	"^@\\E[?36t"
menuitem "Cursor"	"^@\\E[?1t"
menuitem "KeyPad"	"^@\\E[?66t"
menu "../"
if [ "$COLORTERM" = "rxvt-xpm" ]; then
separator
menu "Background/Pixmap"
menuitem "None"		"^@\\E]20;\\a"
separator
# load .xpm files
menuitem "Wingdogs"	"^@\\E]20;wingdogs\\a"
menuitem "Mona"		"^@\\E]20;mona\\a"
menu "../Scaling"
menuitem "Full"		"^@\\E]20;;100x100+50+50;?\\a"
menuitem "Tiled"	"^@\\E]20;;0\\a"
separator
menuitem "400/25%"	"^@\\E]10;<b>\\^@\\\\E]20;;<l>400x0<r>25x0<u>0x400<d>0x25<e>;?\\^G\\a"
menuitem "250/40%"	"^@\\E]10;<b>\\^@\\\\E]20;;<l>250x0<r>40x0<u>0x250<d>0x40<e>;?\\^G\\a"
menuitem "200/50%"	"^@\\E]10;<b>\\^@\\\\E]20;;<l>200x0<r>50x0<u>0x200<d>0x50<e>;?\\^G\\a"
menuitem "125/80%"	"^@\\E]10;<b>\\^@\\\\E]20;;<l>125x0<r>80x0<u>0x125<d>0x80<e>;?\\^G\\a"
menu "../Position"
menuitem "5%}{5%"	"^@\\E]10;<b>\\^@\\\\E]20;;<r>+5+0<l>-5+0<u>+0-5<d>+0+5<e>;?\\^G\\a"
menuitem "10%}{10%"	"^@\\E]10;<b>\\^@\\\\E]20;;<r>+10+0<l>-10+0<u>+0-10<d>+0+10<e>;?\\^G\\a"
separator
menuitem "centre"	"^@\\E]20;;=+50+50;?\\a"
menu "../../"
fi	# COLORTERM
separator
menuitem "Version"	"^@\\E[8n"
menu "/"

show_menu

# some extra dumb menus to test that bounds are respected
exit
menu "/Terminal/Font/Font Sizes"
menuitem "Normal"	"^@\\E]50;#\\a"
menuitem "Larger"	"^@\\E]50;#+\\a"
menuitem "Smaller"	"^@\\E]50;#-\\a"
separator
menu "Specific"
menuitem "Font 1"	"^@\\E]50;#1\\a"
menuitem "...}"
menu "1"
menuitem "a"	"a"
menuitem "b"	"b"
menuitem "c"	"c"
menuitem "d"	"d"
menuitem "e"	"e"
menuitem "f"	"f"
menuitem "g"	"g"
menuitem "h"	"h"
menuitem "i"	"i"
menuitem "j"	"j"
menuitem "k"	"k"
menuitem "l"	"l"
menuitem "m"	"m"
menuitem "n"	"n"
menuitem "o"	"o"
menuitem "p"	"p"
menuitem "q"	"q"
menuitem "r"	"r"
menuitem "s"	"s"
menuitem "t"	"t"
menuitem "u"	"u"
menuitem "v"	"v"
menuitem "w"	"w"
menuitem "x"	"x"
menuitem "y"	"y"
menuitem "z"	"z"
menu "../../../{-}"
menuitem "junk" "junk\\r"
menu "/"
#--------------------------------------------------------------------- eof
