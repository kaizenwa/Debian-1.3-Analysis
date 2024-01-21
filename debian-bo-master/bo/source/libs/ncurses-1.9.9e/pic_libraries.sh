#!	/bin/sh

set -e

form_objects=""
menu_objects=""
ncurses_objects=""
panel_objects=""

for i in `sed '/^#/d' < ncurses/modules | cut -f 1`; do
	ncurses_objects=$ncurses_objects" obj_s/"$i".o "
done

for i in `sed '/^#/d' < form/modules | cut -f 1`; do
	form_objects=$form_objects" obj_s/"$i".o "
done

for i in `sed '/^#/d' < menu/modules | cut -f 1`; do
	menu_objects=$menu_objects" obj_s/"$i".o "
done

for i in `sed '/^#/d' < panel/modules | cut -f 1`; do
	panel_objects=$panel_objects" obj_s/"$i".o "
done

ar cqv tmp-pic/usr/lib/libform$1_pic.a $form_objects
ar cqv tmp-pic/usr/lib/libmenu$1_pic.a $menu_objects
ar cqv tmp-pic/usr/lib/libncurses$1_pic.a $ncurses_objects
ar cqv tmp-pic/usr/lib/libpanel$1_pic.a $panel_objects

exit 0
