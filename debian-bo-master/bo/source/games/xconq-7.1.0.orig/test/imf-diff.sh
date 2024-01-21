#!/bin/sh

grep -h '(imf ' ../lib/*.imf |
sed -e 's/^(imf "\([-a-z0-9]*\)".*$/\1/' |
sort |
uniq >imf.defs

grep -h '(emblem-name ' ../lib/*.g |
sed -e 's/^.*(emblem-name "\([-a-z0-9]*\)").*$/\1/'  >bar
grep -h '(image-name ' ../lib/*.g |
sed -e 's/^.*(image-name "\([-a-z0-9]*\)").*$/\1/' >barb
grep -h ' image-name ' ../lib/*.g |
sed -e 's/^.* image-name "\([-a-z0-9]*\)").*$/\1/' >barc
cat bar barb barc >barx
sort barx |uniq > imf.uses

echo "Differences between defined and used images"
echo "('<' - definitions,  '>' - uses)"

diff -w imf.defs imf.uses

exit 0
