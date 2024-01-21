#!/bin/sh

echo 'dds2tar-test >' make
make

echo 'dds2tar-test >' make of?
if test ! -x ./dds2tar ; then exit 1 ; fi

echo 'dds2tar-test >' creating soft links
ln -sf ./dds2tar ./dds2index
ln -sf ./dds2tar ./mt-dds

echo 'dds2tar-test >' creating soft and hard links
ln -s dds2tar dds2tar-test-tape-link-soft
ln dds2tar dds2tar-test-tape-link-hard

echo 'dds2tar-test >' tar c .
tar c .

echo 'dds2tar-test >' tar -c --label dds2tar . ...
tar --label dds2tar --record-file index-of-tar -R -v -c -b 32 .

I=`grep 'number of the file' index-of-tar | cut -c 38-43`
I=`expr $I - 1`
echo file number of the archive written is $I
if test "$I" = "" ; then exit 1 ; fi

echo 'dds2tar-test >' tar -c . ...
tar --record-file index-of-tar-v -v -R -v -c -b 32 .

echo 'dds2tar-test >' mt rewind \; mt fsf $I
mt rewind ; mt fsf $I

echo 'dds2tar-test >' mt-dds tell
./mt-dds tell

echo 'dds2tar-test >' mt-dds label
./mt-dds label

echo 'dds2tar-test >' mt-dds
./mt-dds

echo 'dds2tar-test >' dds2index
./dds2index -t index-of-dds2index

echo 'dds2tar-test >' find '*tape*' using index-of-tar
./dds2tar -t index-of-tar '*tape*' | tar fvt -

echo 'dds2tar-test >' find '*tape*' using index-of-tar-v
./dds2tar -t index-of-tar-v '*tape*' | tar fvt -

echo 'dds2tar-test >' find '*tape*' using index-of-dds2index
./dds2tar -t index-of-dds2index '*tape*' | tar vft -

echo 'dds2tar-test >' dds2tar -t index-of-tar --body Changes '|wc -c'
./dds2tar -t index-of-tar --body Changes | wc -c
ls -l Changes

if test $I -eq 0 ; then
	echo 'dds2tar-test >' mt rewind
	mt rewind
else
	echo 'dds2tar-test >' mt rewind \; mt fsf $I
	mt rewind ; mt fsf $I
fi

echo 'dds2tar-test > mt-dds tell >'index-of-tar-t
mt-dds tell >index-of-tar-t
echo 'dds2tar-test > tar tR >>' index-of-tar-t
tar tR >> index-of-tar-t
echo 'dds2tar-test >' grep -v "'loc        '" '< index-of-tar-t > index-of-tar-t2'
grep -v 'loc        ' < index-of-tar-t > index-of-tar-t2
echo 'dds2tar-test >' find '*tape*' using index-of-tar-t2
./dds2tar -t index-of-tar-t2 '*tape*' | tar vft -

if test $I -eq 0 ; then
	echo 'dds2tar-test >' mt rewind
	mt rewind
else
	echo 'dds2tar-test >' mt rewind \; mt fsf $I
	mt rewind ; mt fsf $I
fi
echo 'dds2tar-test >' removing links
/bin/rm dds2tar-test-tape-link-soft
/bin/rm dds2tar-test-tape-link-hard
