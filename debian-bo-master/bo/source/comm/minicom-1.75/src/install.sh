#
# Install shell script for minicom and friends.
#
NAME="`whoami`" 2>/dev/null
if [ "$NAME" = "" ]
then
	echo "No whoami? Hmmm.. must be Coherent.. Trying \"who am i\""
	NAME="`who am i | cut -d' ' -f1`"
fi

if test "$NAME" != root
then
	echo "You must be root to install minicom."
	exit 1
fi

if test $# != 4
then
	echo "Usage: install.sh libdir bindir mandir docdir"
	exit 1
fi

if test ! -d $1
then
	mkdir $1
	if [ $? != 0 ]
	then
		echo "$1: No such directory"
		exit 1
	fi
fi

if test ! -d $2
then
	echo "$2: No such directory"
	exit 1
fi

if test ! -d $3
then
	echo "$3: No such directory"
	exit 1
fi

if test -f minicom
then
	echo "Installing minicom in $2"
	cp minicom $2/minicom
	chmod 755 $2/minicom
	chown root $2/minicom
	chgrp root $2/minicom
fi

for i in runscript xminicom ascii-xfr
do
  if test -f $i
  then
	echo "Installing $i in $2"
	cp $i $2/$i
	chmod 755 $2/$i
	chown root $2/$i
	chgrp root $2/$i
  fi
done

if test -f keyserv
then
	echo "Installing keyserv in $1"
	cp keyserv $1/keyserv
	chmod 755 $1/keyserv
	chown root $1/keyserv
	chgrp root $1/keyserv
fi

echo "Installing manpages in $3"
for i in minicom.1 runscript.1 ascii-xfr.1
do
	cp ../man/$i $3
	chmod 644 $3/$i
	chown root $3/$i
	chgrp root $3/$i
done

if [ ! -f $1/minicom.users ]
then
	echo "Installing sample config file minicom.users in $1"
	cp minicom.users $1
	chown root $1/minicom.users
	chgrp root $1/minicom.users
	chmod 644 $1/minicom.users
fi

if test -d $4
then
	for i in saralogin unixlogin htsalogin scriptdemo
	do
		echo "Installing script demo $i in $4"
		cp ../demos/$i $4/$i
		chmod 644 $4/$i
		chown root $4/$i
		chgrp root $4/$i
	done
else
	echo "You don't have a $4 directory - script examples not installed."
fi
	
echo "Minicom is NOT setuid yet - you must do this yourself by entering:"
echo
echo "chmod +s $2/minicom"
echo

exit 0
