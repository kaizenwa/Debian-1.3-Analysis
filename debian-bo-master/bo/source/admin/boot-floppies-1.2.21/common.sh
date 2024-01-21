#! /bin/bash

last_by_version () {
	local a="$1"
	local b="$2"
	

	if [ -z "$b" ]; then
		echo $a
		exit 0
	fi

	shift 2

	if dpkg --compare-versions `basename $a` gt `basename $b`; then
		last_by_version $a $@
	else
		last_by_version $b $@
	fi
}
 
make_paths() {
	exit=false
	local PATHS=""
	local i

	for i in $*; do
		local f=""
		local p
		local j
		for j in $archive/*/${i}_[^a-zA-Z]*.deb \
				updates/${i}_[^a-zA-Z]*.deb; do
			if [ -f $j ]; then
				f="$f $j"
			fi
		done
		p=`last_by_version $f`
		if [ ! -f $p ]; then
			echo "Can't find package" $p 1>& 2
			exit=true
		fi
		PATHS=$PATHS" "$p
	done
	echo $PATHS
	if $exit; then
		exit -1
	fi
}

