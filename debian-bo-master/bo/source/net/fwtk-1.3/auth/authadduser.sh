#!/bin/sh
AUTHSRV="/usr/local/etc/authsrv -s"
AUTHSRV="authsrv -s"
while true; do
	echo -n "Enter Userid: "
	read userid || exit 1
	echo -n "Full Name: "
	read fullname || exit 1
	echo -n "Group: "
	read group || exit 1
	echo -n "Initial Password: "
	read pass || exit 1

	echo
	sure=0
	while [ $sure = 0 ]; do
		echo -n "Add user $user? (y/n)"
		read whyen || exit 1
		case $whyen in
			y*|Y*)
			(
				echo adduser $userid "$fullname"
				echo group $userid "$group"
				echo password "$pass" $userid
				echo enable $userid
				echo quit
			) | $AUTHSRV
			if [ $? -ne 0 ]; then
				echo "user not added correctly!"
			fi
			sure=1;
			;;
			n*|N*)
			sure=1;
			echo "User discarded"
			;;
			*)
			echo "Invalid response"
			;;
		esac
	done
done
