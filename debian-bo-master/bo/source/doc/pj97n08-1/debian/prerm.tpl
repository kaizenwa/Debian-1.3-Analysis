#!/bin/bash
#
# Last modification: Tue, 11 Feb 1997 15:33:32 +0200
#
# prerm of pluto journal issue @@pkg@@

set -e

copy_blind_index()
{
	# adds the blind index and removes the full index
	if	[ -d $journal/.indexes ]
	then	if	[ -r $journal/$package/.indexes/$index.blind.gx ]
		then	echo "  Leaving a blind index ($index) for this issue ($package)."
			gzip -dc $journal/$package/.indexes/$index.full.gz \
					>$journal/.indexes/$index.full
		fi
		# remove anyway
		if	[ -r $journal/.indexes/$index.full ]
		then	echo "  Removing the full index ($index)."
			rm -f $journal/.indexes/$index.full
		fi
	fi
}

package="@@pkg@@";
index="@@idx@@"
journal="/usr/doc/pluto-journal"

if	[ "$1" = "remove" ]
then	copy_blind_index
fi
