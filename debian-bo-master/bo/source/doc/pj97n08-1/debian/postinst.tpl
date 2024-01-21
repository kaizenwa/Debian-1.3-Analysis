#!/bin/bash
#
# Last modification: Tue, 11 Feb 1997 15:33:54 +0200
#
# postinst of pluto journal issue @@pkg@@

set -e

# adds the full index
add_full_index()
{
	if	[ -d $journal/.indexes ]
	then	if	[ -r $journal/$package/.indexes/$index.full.gz ]
		then	echo "  Installing the full index ($index) of this issue ($package)."
			gzip -dc $journal/$package/.indexes/$index.full.gz \
					>$journal/.indexes/$index.full
			# remove if added full
			if	[ -r $journal/.indexes/$index.blind ]
			then	echo "  Removing the blind index."
				rm -f $journal/.indexes/$index.blind
			fi
			echo "  Rebuilding the whole index in $journal"
			j_mkindex $journal
		fi
	fi
}

package="@@pkg@@";
index="@@idx@@"
journal="/usr/doc/pluto-journal"

if	[ "$1" = "configure" ]
then	add_full_index
fi

