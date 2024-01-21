#!/bin/ksh -p
set -x
# remail lost mail to correct mailbox

PATH=/bin:/usr/bin:/usr/ucb:/usr/local/bin:/etc/:/usr/etc

alias echo="print -R"		# force BSD style echo

function filesize {
  /bin/ls -1ds "$1" | cut -c-5
}

# don't do anything if /usr/spool/alt_mail is nfs mounted

if [ "`/usr/local/bin/fstype /usr/spool/alt_mail`" != "/usr/spool/alt_mail: 4.2" ]; then
  echo /usr/spool/alt_mail is not on a 4.2 filesystem!
  exit
fi

# generate list of files *or* directories in /usr/spool/alt_mail

cd /usr/spool/alt_mail && {

 find . -fstype 4.2 ! -name '.' ! -name 'lost+found' -print | 
 while read name
 do
   name=${name##*/}

#   if grep "^$name:" /etc/passwd >/dev/null 2>&1
#   then
#     echo Local/guest account: $name.
#   else

     fileblocks=`filesize "$name"`

     DO_REMAIL=false

     if [ "$fileblocks" = "   0 " ]
     then
       rm -f "$name"
     else
       expname=`expn "$name"`
       expstat=$?

       if [ $expstat -gt 2 ]
       then
	 case "$name" in
	   *~|\#*\#) rm -f "$name" ;;	# delete backup files
	   *.lock) : ;;			# ignore mail lock files
	   *) echo -n "Potential non-user mail file in \"$name\" of size "
	      echo $fileblocks blocks. ;;
	 esac
       elif [ $expstat -gt 0 ]
       then
	 echo "Error expanding \"$name\": $expname."

       else
	 case `echo "$expname" | sed -n -e '/<\\\\'"*$name>/{p;q;}" -e '$p'` in

#          *"<$name>"*|*"<"\\"$name>"*) : mail for $name is ok here ;;

	   *@*)		# don't remail to remote unless address is okay
	     echo "Misplaced mail in \"$name\" of size $fileblocks blocks."
	     expname="${expname#*<}"; expname="${expname%>*}"
	     echo -n "Remailing to $expname..."
	     if exp2name=`expn "$expname" 2>&1`
	     then
	       echo okay.
	       DO_REMAIL=true
	     elif [ $? -lt 2 ]
	     then
	       echo "failed. (will try again later)"
	     elif [ $? -lt 3 ]
	     then
	       echo "failed SMTP verification!  Please remail manually."
	     else
	       echo "failed! (notifying postmaster)"
	       Mail -s "Bad forwarding for user $name" postmaster <<- EOF
		The lostmail script tried to remail misplaced mail for "$name"
		to the address <$expname>, but it does not seem to be
		working, since it failed with "$exp2name".
		
		Please delete this user or change their alias to a working one.
		Deleting their mail file will also prevent these messages.
		
		The /usr/local/adm/lostmail script.
		EOF
	     fi ;;

	   *)		# expn gave local expansion - another user or a file
	     DO_REMAIL=true ;;
	 esac

       fi

       if $DO_REMAIL
       then
	/usr/local/etc/lostmail.pl "$name"
       fi

     fi

#   fi

 done

}
