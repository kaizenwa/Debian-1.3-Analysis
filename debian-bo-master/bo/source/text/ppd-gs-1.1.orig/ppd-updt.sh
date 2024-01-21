#! /bin/sh

# ppd-updt.sh: update installed PPD files.
#
# Yves Arrouye <Yves.Arrouye@marin.fdn.fr>, 1996

which() {
    prog=$1
    save_ifs="$IFS"; IFS=:
    set - ..:$PATH
    for d
    do
	if [ -x $d/$prog ]
	then
	    echo $d/$prog
	    break
	fi
    done
    IFS="$save_ifs"
    unset save_ifs prog
}

# Please 2>/dev/null your calls to the listppd function.

listppd() {
    _ppds=
    for d in `echo $PPDPATH | tr ':' ' '`
    do
	_ppds="$_ppds `ls $d/*.ppd $d/[a-z][a-z]/*.ppd
	    $d/[a-z][a-z]_[A-Z][A-Z]/*.ppd
	    $d/[a-z][a-z].*/*.ppd $d/[a-z][a-z]_[A-Z][A-Z].*/*.ppd
	    $d/[a-z][a-z],*/*.ppd
	    $d/[a-z][a-z]_[A-Z][A-Z],*.ppd
	    $d/[a-z][a-z]_[A-Z][A-Z].*,*/*.ppd`"
    done
    if [ ! -z "$iffwritable" ]
    then
	for _ppd in $_ppds
	do
	    if [ -r "$_ppd" -a -w "$_ppd" ]
	    then
		_wrppds="$_wrppds $_ppd"
	    fi
        done
	echo $_wrppds
	unset _wrppds _ppd
    else
	echo $_ppds
    fi
    unset _ppds
}

me=`basename $0`

trap "rm -f /tmp/$me.$$.updt"

usage() {
    >&2 echo usage: "$me [ -v, --verbose ] [ -i, --interactive ] [ --dontlocalize ] [ --iff-gs ] [ --iff-writable ] [ -a, --any-ppd ] [ -g, --gs-ppd ] [ -n, --nongs-ppd ] [ -b, --bg-update ] [ --signature sig ] [ -d, --destdir directory ] [ ppd-file ... ]"
    exit 1
}

# Echo.

if [ "`echo -n a`" = "-na" ]
then
    echon() {
	echo "$@\c"
    }
else
    echon() {
	echo -n "$@"
    }
fi

# Ask a simple question.

yorn() {
    _def="[$1]"

    _prompt="$2"
    _again="$3"

    : ${_again:="$_prompt"}

    boolean_answer=

    until [ ! -z "$boolean_answer" ]
    do
	echon "$_prompt" "$_def "
	read _ans
	case "$_ans" in
	    "")
		boolean_answer="$1"
		;;
	    y|Y)
		boolean_answer=y
		;;
	    n|N)
		boolean_answer=n
		;;
	esac
	if [ -z "$boolean_answer" ]
	then
	    echo 'Please answer y or n.'
	    _prompt="$_again"
	fi
    done

    unset _def _prompt _again
}

background=n
localize=y

ppdtype=any

signature="The $me script."

while test $# -ne 0
do
    case "$1" in
        -v|--verbose)
	    iopts="$iopts $1"
            ;;
	--iff-gs)
	    iff_gs=yes
	    ;;
	-b|--bg-update)
	    background=y
	    ;;
	-d|--destdir)
	    test $# -gt 1 || usage
	    iopts="$iopts $1 $2"
	    mopts="$mopts $1 $2"
            shift
	    ;;
	--signature)
	    test $# -gt 1 || usage
	    signature="$2"
	    shift
	    ;;
	-a|--any-ppd)
	    ppdtype=any
	    ;;
	-g|--gs-ppd)
	    ppdtype=gs
	    ;;
	-n|--nongs-ppd)
	    ppdtype=nongs
	    ;;
	--iff-writable)
	    iffwritable=y
	    ;;
	-i|--interactive)
	    interactive=yes
	    ;;
	--dontlocalize)
	    localize=n
	    ;;
  	-*)
	    usage
	    ;;
	*)
	    ppdfiles="$ppdfiles $1"
    esac
    shift
done

gs=`which gs`
if [ -z "$gs" ]
then
    if [ ! -z "$iff_gs" ]
    then
	if [ ! -z "$interactive" ]
	then
	    if [ $ppdtype != nongs ]
	    then
	        echo "After installing gs, it may be nice to run $me --gs-ppd."
	    fi
	fi
	exit 0
    fi
    gsopt=--nogs
fi


case "`uname -s | tr '[a-z]' '[A-Z]'`" in
    *NEXT*)
        nextstep=1;;
    *)
        nextstep=0;;
esac

if [ $nextstep -eq 1 ]
then
    for i in $HOME/Library /LocalLibrary /NextLibrary
    do
	for d in `2>/dev/null ls -d $i/PrinterTypes/*.lproj`
	do
	    if [ -d "$d" ]
	    then
		if [ -z "$__PPDPATH" ]
		then
		    __PPDPATH=$d
		else
		    __PPDPATH="${__PPDPATH}:$d"
		fi
	    fi
	done
    done
else
    for i in $HOME /usr/local /usr
    do
	add="$i/share/ppd:$i/share/postscript/ppd:$i/lib/ppd:$i/lib/postscript/ppd:$lib/ppd"
	if [ -z "$__PPDPATH" ]
	then
	    __PPDPATH=$add
	else
	    __PPDPATH="${__PPDPATH}:$add"
	fi
    done
    for i in /
    do
	__PPDPATH="$__PPDPATH:$i/lib/ppd:$i/lib/postscript/ppd"
    done
fi

if [ -z "$PPDPATH" ]
then
    PPDPATH="$__PPDPATH"
    export PPDPATH
else
    case "$PPDPATH" in
	:*)
	    PPDPATH="${__PPDPATH}${PPDPATH}"
	    ;;
    esac
    case "$PPDPATH" in
	*:)
	    PPDPATH="${PPDPATH}${__PPDPATH}"
	    ;;
    esac
    PPDPATH=`echo $PPDPATH | sed "s,::,:${__PPDPATH}:,g"`
fi

if [ -z "$ppdfiles" ]
then
    if [ ! -z "$interactive" ]
    then
        echon "Locating installed PPD files... "
    fi
    ppdfiles=`2>/dev/null listppd`
    if [ ! -z "$interactive" ]
    then
	echo done
    fi
fi

if [ ! -z "$ppdfiles" -a "$ppdtype" != any ]
then
    if [ ! -z "$interactive" ]
    then
        echon "Selecting appropriate PPD files... "
    fi

    theppds=""
    for p in $ppdfiles
    do
	if [ ! -r "$p" ]
	then
	    >&2 echo $me: cannot open \`$p\'
	else
	    gsdevice="`grep '^\*% GhostscriptDevice: ' \"$p\"`"
	    if [ -z "$gsdevice" -a $ppdtype = nongs ]
	    then
		theppds="$theppds $p"
	    else
		if [ ! -z "$gsdevice" -a $ppdtype = gs ]
		then
		    theppds="$theppds $p"
		fi
	    fi
	fi
    done
    ppdfiles="$theppds"

    if [ ! -z "$interactive" ]
    then
	echo done
    fi
fi

if [ ! -z "$ppdfiles" ]
then
    if [ ! -z "$interactive" ]
    then
	yorn y 'Would you like to adapt your PPD files to your installation?'
	doit=$boolean_answer
	if [ "$doit" != y ]
	then
	    echo "No changes made to the PPD files."
	    echo "Updating PPD directories for installed files."
	    ./ppd-inst.sh $mopts --dontchange $ppdfiles
	fi
    else
	doit=y
    fi
    if [ "$doit" = "y" ]
    then
	if [ ! -z "$interactive" ]
	then
	    if [ "$ppdtype" != nongs ]
	    then
	        echo "(Please run \`$me --gs-ppd' again whenever you reinstall gs. Thanks.)"
	    fi
	    yorn $localize 'By default, your existing localized PPD files will be overwritten by new
localized versions automatically translated from the english files, which
may be undesirable if you did modify the localized files by hand. Do you
want an automatic localization from the english files?' 'Overwrite existing localized files?'
	    localize=$boolean_answer
	fi
	if [ "$localize" = "n" ]
	then
	    iopts="$iopts --dontlocalize"
	fi
	mail=`which mail`
	if [ ! -z "$interactive" ]
	then
	    if [ ! -z "$mail" ]
	    then
		errlogexpl="you will receive a mail if there are some errors
during the update"
	    else
		errlogexpl="a log of errors occuring during the update will be
available as \`/tmp/$me.$$.updt'."
	    fi
	    yorn $background "The adaptation of the PPD files may take some time. Do you want it to be
run in the backgound ($errlogexpl)?" 'Background update of PPD files?'
	    background=$boolean_answer
	fi
	if [ "$background" = y ]
	then
	    (
	  	./ppd-inst.sh $iopts $gsopts $ppdfiles \
	     	    >/dev/null 2>/tmp/$me.$$.updt;
	     	if [ -s /tmp/$me.$$.updt ]
		then
		    if [ ! -z "$mail" ]
		    then
		        cat <<EOM | $mail \
			    -s 'Messages during PPD files adaptation' `whoami`
The adaptation of your PPD files yielded the following messages:

`cat /tmp/$me.$$.updt`

Hope these messages are clear enough...
$signature
EOM
		        rm -f /tmp/$me.$$.updt
		    fi
		fi
	    ) &
	    if [ ! -z "$interactive" ]
	    then
		echo Adaptation of PPD files proceeding in the background.
	    fi
	else
	    if [ ! -z "$interactive" ]
	    then
	        echo 'Adaptation of the PPD files proceeding (this may take a while).'
	    fi
            ./ppd-inst.sh $iopts $gsopt $ppdfiles
	    if [ ! -z "$interactive" ]
	    then
	        echo Adaptation of the PPD files done.
	    fi
	fi
    fi
fi

