#!/bin/sh

# This script extracts and compares the commands defined in the manual
# to those defined in the source code.

grep DEF_CMD $1/../*/*cmd.def | sed -e 's/\\\\/\\/' | sed -e 's/^[^(]*(\([^,]*\), "\([^"]*\)".*$/\1 \2/' | sed -e "s/C('\(.\)')/C-\1/" -e "s/ *'\(.\)' /\1/" -e 's/   0  /0/' | grep -v '0 D' | sort | uniq >src.cmds

grep -h "^\`@code"  $1/../doc/play.texi $1/../doc/*chap.texi | sed -e 's/@@/@/'| sed -e 's/^\`@code{\(.\)}. @code{\([^}]*\)}.*$/\1 \2/' >doc.cmds1

grep -h "^@code{[^C]"    $1/../doc/play.texi $1/../doc/*chap.texi | sed -e 's/@@/@/' | sed -e 's/^@code{\([^C][^}]*\)}.*$/0 \1/' >doc.cmds2

grep -h "^@code{C-" $1/../doc/play.texi $1/../doc/*chap.texi | sed -e 's/@@/@/' | sed -e 's/^@code{\(C-.\)} @code{\([^}]*\)}.*$/\1 \2/' >doc.cmds3

cat doc.cmds[123] | sort | uniq >doc.cmds

echo "Differences between commands in reference manual and in source code:"
echo "('<' - in documentation,  '>' - in sources)"

diff -w doc.cmds src.cmds

exit 0
