#!/bin/bash
#
# Last modification: Tue, 11 Feb 1997 15:23:54 +0200
#
# postrm of pluto journal issue @@pkg@@

set -e

delete_blind_index()
{
	# removes the blind index
	if	[ -d $journal/.indexes ]
	then	if	[ -r $journal/.indexes/$index.blind ]
		then	echo "  Purging the index ($index)"
			rm -f $journal/.indexes/$index.blind
		fi
	fi
}

rebuild_index()
{
	if	[ -d $journal/.indexes ]
	then	
		j_mkindex $journal
	fi
}

package="@@pkg@@";
index="@@idx@@"
journal="/usr/doc/pluto-journal"

if	[ "$1" = "remove" ]
then	echo "  Rebuilding the whole index in $journal"
	rebuild_index
fi
if	[ "$1" = "purge" ]
then	delete_blind_index
	echo "  Rebuilding the index without the $package issue"
	rebuild_index
fi
