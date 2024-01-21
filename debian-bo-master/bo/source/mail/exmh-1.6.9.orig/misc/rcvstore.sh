--Boundary (ID DaViDjFaGaN)
Content-type: text/plain; charset=us-ascii

	This may have a few Fermi"ism's" but it will work.
	( fail to send to <grk@arlut.utexas.edu> ??? )

#!/bin/sh
# Name: fMHNewMail
#
# Description:
#  Automatically delivers New mail to system NEW mail box.
#
# Parameters:
#  1 = The path to be used for $HOME ($HOME is not set by sendmail)
#
#  Example:
#    1 = "/home/freak""
#
# Environment Variables:
#   MH Set to the name of the MH-Profile file. Default: $HOME/.mh_profile
#   MH_DIR Set to the installation directory of the MH commands. Default
#   is "/usr/local/products/mh/current/bin".
#
#  MH-Profile Variables:
#   "Path" and "Inbox" and "Msg-Protect" and "Folder-Protect"
#
# Return stats:
#  0 = mail delivered.
#  1 = Error delivering email.
#
# Install:
#  Put the absolute program name in your .forward file...
#   "|/this/program/name /home/freak"
#  chmod +rx on this file.
#
# Summary of Procedures:
#   exitproc...........Common prodedure for exiting.
#   getProfilekey......Gets settings from Mh-Profile by keyword.
#   getmessagenumber...Gets the next available message file number.
#
# Revision History:
#...........................................................................
# Set these variables:

# Begin source code.
trap "exitproc" 1 2 3 15
PATH="/usr/5bin:/usr/bin:/usr/ucb:/usr/bsd:/bin:/usr/etc:/usr/lib:/usr/bin/X11:${PATH}"
PATH="${MH_DIR:-/usr/local/products/mh/current/bin}:$PATH"; export PATH
# LD_LIBRARY_PATH for Suns, does not hurt on other machines...
LD_LIBRARY_PATH="/usr/local/lib/mh:$LD_LIBRARY_PATH";export LD_LIBRARY_PATH
PROGNAME=`basename ${0}`

# Procedures -----------------------------------------------------------------
exitproc() {
 exit $*
}

getProfilekey() {
 # Will extract the information from the $MH file
 # Takes one parameter, the Keyword to look for in the profile.
 # usage: value=`getProfilekey Keyname`
 # returns: 0
 echo `grep "^${1}:[ 	]*" ${MH} 2> /dev/null|tail -1 | \
 awk -F":" '{ print substr($0,(length($1)+3)) }'`
 return 0
}

which() {
 # Bourne shell version of the csh "which" command, looks at current
 # value of $PATH (not the one define in ~/.cshrc or ~/.login) to
 # determine which command will be executed from the $PATH definition.
 # Usage: cmd=`which {command}`
 # Returns 0, command was found in $PATH, or 1, command not in $PATH.
 tempvar1=$1
 oldIFS=$IFS
 IFS=":"
 set -- $PATH
 IFS="${oldIFS}"
 if [ $# -eq 0 ]; then
  return 1
 fi
 for tempvar2 in $*; do
  if [ -x ${tempvar2}/${tempvar1} ]; then
   echo ${tempvar2}/${tempvar1}
   return 0
  fi
 done
 return 1
}

usage() {
 Full_PROGNAME=`which ${PROGNAME}`
 cat << !EOF! >&2
 $PROGNAME distributed by Computing Division/UAS Fermilab, October 26, 1993
 Usage: $PROGNAME [-h|-install|-deinstall] HOMEDIR
   Example: $PROGNAME /home/koenen

 Description:
 This program is used in conjunction with mh and xmh to  automatically
 deliver in-coming mail to  the  MH  install  directory.  MH  normally
 requires the user to execute the "inc" command to include  new  mail.
 This program eliminates the need to do this.

 Installation:
 In your ${HOME}/.forward file, enter the following:
  "|${Full_PROGNAME} ${HOME}"

 The Quote characters should be included in the file. This program 
 will install itself if you use the "-install" option.
!EOF!
}

getmessagenumber() {
 # Reserves the next available message number file from directory $1.
 # Usage: filename=`getmessagenumber Foldername`
 # Returns: 0 if successful, else a 1
 number=`mhpath +${1} new`
 touch ${number}
 echo ${number}
 if [ ! -f ${number} ]; then
  return 1
 fi
 return 0
}

installself() {
 # Creates a .forward file to configure this program.
 # Saves any current .forward file as ".forward.bak"
 echo "Configuring ${PROGNAME}..." >&2
 Full_PROGNAME=`which ${PROGNAME}`
 if [ -s ${HOME}/.forward ]; then
  if [ "`head -1 ${HOME}/.forward`" = "\"|${Full_PROGNAME} ${HOME}\"" ]; then
   echo " ${PROGNAME} already installed, no change." >&2
  else
   echo "Saving current .forward file as .forward.bak" >&2
   mv -f $HOME/.forward $HOME/.forward.bak
   echo "\"|${Full_PROGNAME} ${HOME}\"" > ${HOME}/.forward
  fi
 else
  echo "\"|${Full_PROGNAME} ${HOME}\"" > ${HOME}/.forward
 fi
 echo "Done, $PROGNAME configured." >&2
}

deinstallself() {
 # Removes the .forward file, restores the .forward.bak file if it
 # exists.
 echo "De-installing $PROGNAME..." >&2
 Full_PROGNAME=`which ${PROGNAME}`
 if [ -s ${HOME}/.forward ]; then
  if [ "`head -1 ${HOME}/.forward`" = "\"|${Full_PROGNAME} ${HOME}\"" ]; then
   rm ${HOME}/.forward
   if [ -s ${HOME}/.forward.bak ]; then
    echo "Returning ${HOME}/.forward.bak to ${HOME}/.forward" >&2
    mv -f ${HOME}/.forward.bak ${HOME}/.forward
   fi
  else
   echo "${PROGNAME} not currently installed, no change." >&2
  fi
 else
  echo "${PROGNAME} not currently installed, no change." >&2
 fi
 echo "Done, $PROGNAME de-installed" >&2
}

# Main Program ---------------------------------------------------------------
if [ $# -ne 1 ]; then
 usage
 exitproc
fi

case "$1" in
 -h|-help)
   usage
   exitproc 0
 ;;
 -install)
   installself
   exitproc 0
 ;;
 -deinstall)
   deinstallself
   exitproc 0
 ;;
esac

HOME=$1;export HOME;cd ${HOME}
MH="${MH:-${HOME}/.mh_profile}"; export MH
# Get the MH-Path and MH-Inbox settings...
Path=`getProfilekey Path`
if [ -z "${Path}" ]; then
 cat - >> ${HOME}/.saved_email
 cat << !EOF! > /dev/console 2>&1
 $PROGNAME: Error locating Mh-Profile. Missing?: "${MH}"
            See ${HOME}/.saved_email.  `date`
 Aborting.
!EOF!
 # Get the message protection setting...
 Mask=`getProfilekey Msg-Protect`
 chmod ${Mask:-644} ${HOME}/.saved_email
 exitproc 1
fi
# Make the Path absolute, if it is not already...
if [ `echo "${Path}"|cut -c1` != "/" ]; then
 Path="${HOME}/${Path}"
fi

Inbox=`getProfilekey Inbox`
Inbox="${Inbox:-inbox}"

# If the directory does not exist, make it.
if [ ! -d ${Path}/${Inbox} ]; then
 mkdir -p ${Path}/${Inbox}
 Mask=`getProfilekey Folder-Protect`
 chmod ${Mask:-644} ${Path}/${Inbox}
 # Make sure the owner change "change to" this directory...
 chmod u+x ${Path}/${Inbox} 2> /dev/null
fi

outfile=`getmessagenumber ${Inbox}`
if [ $? -eq 1 ]; then
 echo "$PROGNAME: Error getting number for incoming email" > /dev/console
 echo "             See ${HOME}/.saved_email.  `date`" > /dev/console
 cat - >> ${HOME}/.saved_email
 # Get the message protection setting...
 Mask=`getProfilekey Msg-Protect`
 chmod ${Mask:-644} ${HOME}/.saved_email
else
 cat - >> $outfile
 # Get the message protection setting...
 Mask=`getProfilekey Msg-Protect`
 chmod ${Mask:-644} $outfile

 # Check to see if we are on vacation...
 if [ -s ${HOME}/.vacation_xmhsend ]; then
  # We must be on vacation...
  cat $outfile | $HOME/bin/vacation -r
 fi
fi

exitproc 0

                                        ,,,
                                       (o o)
-----------------------------------oOO--(_)--OOo--------------------------------
      * to err is human, to really foul things up use an AIX machine *
--------------------------------------------------------------------------------
David J. Fagan   | The Silicon Sorcerer  | Bitnet:    Fagan@fnal
   SGI-liaison                           | Liaison Requests use SGI-liaison@fnal
   * Feed the homeless to the hungry *   | Internet:  Fagan@large (.fnal.gov)
Fermi National Accelerator Laboratory    | MaBellnet: 1 (708) 840-2914
 WARNING: Please keep the flashlights away from the blondes!!!
	-=(Sid)=-   it's simple but my guns never overheat...
--------------------------------------------------------------------------------
                                   ooO       Ooo
--Boundary (ID DaViDjFaGaN)
Content-type: application/x-patch
Content-description: hold.patch

***************

