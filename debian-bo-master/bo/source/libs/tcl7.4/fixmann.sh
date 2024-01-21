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

fixman library auto_execok auto_load auto_mkindex auto_reset parray
fixman tclvars env errorCode errorInfo tcl_library tcl_precision tcl_version tcl_patchlevel
