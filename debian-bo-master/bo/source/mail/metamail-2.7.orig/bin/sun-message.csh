#!/bin/csh -fb
# (The "-fb" might need to be changed to "-f" on some systems)
#
#
# This is sun-message.  It looks at $2 to figure out how to decode $1, then gives the
# user a short menu of choices, display, save, or quit.
#
mkdir /tmp/decode.$$
cd /tmp/decode.$$

if ($2 == "uuencode") then
    uudecode $1

    echo "The following file was uudecoded:"
    echo ""

    set defans = "1"
    while (1)
	ls -l
	set fn = *

	echo ""
	echo "Please choose one:"
	echo ""
	echo "1 -- Display it as ASCII text"
	echo "2 -- Save it as a file"
	echo "3 -- Quit this menu"
	echo ""
	echo -n "Which do you prefer (1 - 3)? [$defans] "

	set ans = $< 
	if (x$ans == x) then
	    set ans = $defans
	endif
	if ($ans == 3)  then
	    rm $1
	    cd /tmp; /bin/rm -rf /tmp/decode.$$
	    exit 0
	else if ($ans == 1) then
	    more $fn
	else if ($ans == 2) then
	    set nfn = ""
	    echo -n "Save as: $HOME/"
	    set nfn = $<
	    if (x$nfn != x) then
		/bin/cp $fn ${HOME}/$nfn
	    else
		echo "Not Saved."
	    endif
	else
	    echo "Invalid choice."
	endif
	set defans = "3"
    end
else
    more $1
endif


