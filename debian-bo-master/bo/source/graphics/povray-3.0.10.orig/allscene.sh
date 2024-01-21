#!/bin/sh
# This will find all of the .pov scenes in the current directory tree,
# and render them.  It will also render the first frame of all the
# animations, but not the whole thing, because there is no common Unix
# animation utilities.  Customize to your liking.

# Be sure to remove complete.lst if you want to re-render the scenes.
# It is sort of a kuldge because we can't simply test for the existence
# of the output files, as this would prevent us from resuming interrupted
# renderings.
echo "" >> complete.lst
find . -name "*.pov" -exec nice povray allscene.ini +i \{\} \;
