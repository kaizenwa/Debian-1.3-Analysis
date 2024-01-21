# Shell interface to "dialog"
# Bruce Perens, November 1995
# This is free software under the GNU General Public License.

# Global options
#	The variable $BACKTITLE specifies the back title.
#	The variable $DIALOG_OPTIONS, initialized here to --clear, provides
#	options you want on the command line of each dialog invocation.
#	The variable $DIALOG_TEST can be set to "echo" to see the calls
#	to dialog without executing them.

# Ignore signals in shell scripts, but catch them in programs started
# by the scripts.
trap true 2 3 15

DIALOG_OPTIONS=""

# Pick the first word from a variable.
first () {
	echo "$1"
}

# Pick the second word from a variable.
second () {
	echo "$2"
}

# Pick the third word from a variable.
third () {
	echo "$3"
}

# Pick the fourth word from a variable.
fourth () {
	echo "$4"
}

# Make any dialogue box, with default settings and backtitle from
# $BACKTITLE in the environment.
#
# dialog --type arg arg ...
#
dialogBox () {
	local type="$1"
	shift
	local title=""
	local backtitle=""

	local text="$1"
	shift

	if [ $# -ge 1 ]; then
		title="$1"
		shift
	fi

	if [ -n "$BACKTITLE" ]; then
		backtitle="$BACKTITLE"
	fi

	$DIALOG_TEST dialog $DIALOG_OPTIONS --title "$title" --backtitle \
	 "$backtitle" "$type" "$text" 0 0 "$@" 2>&1 1>/dev/tty
	result=$?
	return $result
}

# Display a file.
#
# fileBox filename [title]
#
fileBox () {
	dialogBox --textbox "$1" "$2"
}

# textBox takes presents its standard input in a dialog box. This
# is useful for "here documents" and pipes.
#
# textBox [title]
#
textBox () {
	cat >/tmp/text.$$

	if [ $? -ne 0 ]; then
		echo "Can't make temporary file for dialog box." 1>&2
		exit -1
	fi

	# Note that dialog needs stdin to be the terminal, so I redirect here.
	< /dev/tty dialogBox --textbox /tmp/text.$$ "$1"
	result=$?
	rm -f /tmp/text.$$
	return $result
}

msgBox () {
	dialogBox --msgbox "$1" "$2"
}

infoBox () {
	dialogBox --infobox "$1" "$2"
}

yesNoBox () {
	dialogBox --yesno "$1" "$2"
}

inputBox () {
	dialogBox --inputbox "$1" "$2" "$3"
}

# menu text title tag1 item1 ...
menu () {
	text="$1"
	shift
	title="$1"
	shift
	dialogBox --menu "$text" "$title" 0 "$@"
}

# menu text title tag1 item1 status1 ...
checklist () {
	text="$1"
	shift
	title="$1"
	shift
	dialogBox --checklist "$text" "$title" 0 "$@"
}

# menu text title tag1 item1 status1 ...
radiolist () {
	text="$1"
	shift
	title="$1"
	shift
	dialogBox --radiolist "$text" "$title" 0 "$@"
}

demo () {
	BACKTITLE="Shell-Dialog Interface Demonstration"

	fileBox "/etc/motd" "Message of the Day"
radiolistls | textBox "ls output"
	
	yesNoBox "Are you a poofta?" \
	 "Question only Monty Python Fans would understand"

	msgBox "The yes-no box result was $?." "Result of Last Question"

	answer=`inputBox "What is the air-speed of an unladen swallow?" \
	 "Another question only Monty Python Fans would understand"`

	msgBox "The input box result was $answer." "Result of Last Question"

	infoBox "That's All, Folks!" "End of Demo"
}
