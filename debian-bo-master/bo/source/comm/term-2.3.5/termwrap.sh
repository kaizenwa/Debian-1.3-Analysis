#!/bin/sh
# run a program with TERM networking (Solaris 2.x only)

if [ "$TERMWRAP" = "" ]
then
    TRY_DIRS=". /usr/local/lib /usr/local/lib/term $HOME/term $HOME/.term"

    for i in $TRY_DIRS
    do
        if [ -f $i/termwrap.so ]
        then
            TERMWRAP=$i/termwrap.so
            break
        fi
    done
 fi
if [ "$TERMWRAP" = "" ]
then
    echo "cannot find termwrap.so, please set TERMWRAP to its location" >&2
    exit 1
fi

if [ "$LD_PRELOAD" = "" ]
then
    LD_PRELOAD="$TERMWRAP" export LD_PRELOAD
else
    # maintain existing LD_PRELOADed objects as well...
    LD_PRELOAD="$TERMWRAP $LD_PRELOAD" export LD_PRELOAD
fi

exec $*
