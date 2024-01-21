#! /bin/sh

PATH="/usr/sbin:/usr/bin:/sbin:/bin"

trap "" 1 2 3 15

export DPKG_NO_TSTP="yes"

rm -f /root/.bash_profile
mv /root/.bash_profile.real /root/.bash_profile

if [ ! -f /etc/kbd/default.map ]; then
	kbdconfig
	loadkeys /etc/kbd/default.map
	if [ $? -ne 0 ]; then
		echo "Loading the new keymap failed"
		read foo
	fi
fi


cat << END

  Before proceeding, you need to set a password for \`root', the system
administrative account.  (The root account is the account that you're
currently using).  The root password shouldn't be easy to guess, and
it shouldn't be a word found in the dictionary, or a word that could
be easily associated with you, like your middle name.  A good password
will contain a mixture of letters, numbers and punctuation and will be
changed at regular intervals.  The root password is changed by running
the \`passwd' program as root.
  Why such caution?  The root account doesn't have the restrictions that
normal user accounts have.  root can read, modify or erase any file on
the system, change the ownerships and permissions of any file, and run
programs that require special privileges (such as programs that format
the hard disk!).  A malicious or unqualified user with root access can
have disasterous results.
  For the same reason, it's a bad idea to use the root account for normal
day-to-day activities, such as the reading of electronic mail, because
even a small mistake can result in disaster.  As soon as you're logged
in as root, you should create a normal user account.  You should use the
normal user account whenever you're not maintaining the system.  (How to
do this will be described in a minute.)

END
passwd
pass=`cat /etc/passwd | fgrep root | cut -d : -f 2`
while [ -z "$pass" ]
do
{
  echo -e "\nPlease try again."
  passwd
  pass=`cat /etc/passwd | fgrep root | cut -d : -f 2`
}
done

cat << END

As mentioned above, it is a bad idea to use the root account except
in cases that you need special privileges (such as when you need to
mount a file system).  We will now create an account for you.  If you
want to create additional accounts, you may do so by typing \`adduser
<username>' as root, where <username> is an (eight-character or less)
user name, like \`imurdock' or \`rms'.

END

while true
do
{
  echo -n "Enter a username for your account: "
  read username
  echo
  adduser $username
  if [ $? = 0 ]
  then
    break
  fi
}
done

if [ -x /usr/sbin/shadowconfig ]; then
	echo -n "Configure shadow passwords

Shadow passwords make your system more secure because nobody is
able to view even encrypted passwords.  Passwords are stored in
a separate file that can only be read by special programs.  We
recommend the use of shadow passwords.  If you're going to use
NIS you could run into trouble.

Shall I install shadow passwords? [Y/n] "
	read answer
	case "$answer" in
	N|n)
		true
		;;
	*)
		shadowconfig on
	esac
fi


if [ -f /etc/inittab.real ]; then
	mv -f /etc/inittab.real /etc/inittab
	sync
	kill -HUP 1
fi

cat << END

I'm now going to start the \`dselect' program.  \`dselect' is used to select
which of the hundreds of packages included with Debian GNU/Linux to install
on the system.  (It is also used for general maintenance of the packages on
the system, but you'll be primarily interested in installation now.)

You should follow the on-line instructions to select any packages that you
want to have on your system, such as GNU Emacs, TeX or the X Window System.
A default set of packages are automatically selected that provide a fairly
complete Unix-like environment, so manual selection of packages is optional.

END
echo -n "Press <ENTER> to continue: "
read enter

dselect

cat << END

You may now login as \`root' at the  login:  prompt.  To create a normal
user account, you should run \`adduser' as root with a user name as an
argument.  For example, to create a user called "imurdock", you would
type this at the shell prompt: "adduser imurdock".

You may also take advantage of the multi-tasking features of Debian
GNU/Linux by pressing <Left Alt><Fn> to switch to a new "virtual
console", where n is the number of the virtual console to switch to.
For example, to switch to virtual console #3, you would press <Left
Alt><F3>, and to return to this virtual console (virtual console #1),
you would press <Left Alt><F1>.

Have fun!
END

exit
