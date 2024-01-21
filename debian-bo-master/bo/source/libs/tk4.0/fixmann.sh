#!/bin/sh

if [ $# -ne 2 ]; then
	echo usage: $0 directory extension
	exit 1
fi

if ! cd $1; then
	echo $0: can not cd to $1
	exit 1
fi

ext=$2

function fixman {
	if [ $# -lt 2 ]; then
		echo fixman: called with too few args
		exit 1
	fi

	local base=$1
	shift 1

	for f in $*; do
		if ! ln -sf $base.$ext $f.$ext; then
			echo fixman: cant ln $base.$ext to $f.$ext
		fi
	done
}

fixman dialog tk_dialog
fixman focusNext tk_focusNext tk_focusPrev tk_focusFollowsMouse
fixman menubar tk_menuBar tk_bindForTraversal
fixman optionMenu tk_optionMenu
fixman popup tk_popup
fixman tkvars tk_library tk_patchLevel tkPriv tk_strictMotif tk_version
