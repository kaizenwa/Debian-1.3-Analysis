#! /bin/sh

# Script to make apsfilter work with TeX and dvips properly, cause
# there are version of dvips "on the net" floating around not working
# properly.

# Just some basic error-testing is done, perhaps more if the script is
# used more widely and the first bug-reports reach us :-)

read_mode() {
	echo " Please enter the mode metafont uses to create fonts for your "
	echo " Printer. Proper mode-strings can be found in your modes.mf "
	echo " database in your mf-Directory "
	echo " "
	echo -n "Metafont-Mode : "
	read MODE
}

check_valid_mode() {
	TEMPDIR=${TMP:-"/tmp"}
	TMPDIR=$TEMPDIR/aps$$
	test -d $TMPDIR || mkdir $TMPDIR
	cd $TMPDIR || return
	rm -f *gf 2> /dev/null
	mf '\mode='$MODE'; input logo10' >& /dev/null
	RES_X=`echo logo10*gf | sed 's/logo10.//;s/gf//'`
	RES=RES_X
	rm -f *gf 2> /dev/null
	mf '\mode='$MODE'; landscape; input logo10' >& /dev/null
	RES_Y=`echo logo10*gf | sed 's/logo10.//;s/gf//'`
	cd $TEMPDIR
	rm -rf $TMPDIR
   	if [ $RES_X = 2602 ]; then
		echo " "
		echo " Please check your Mode, Metafont dropped into proof-mode. "
		echo " This usually happens if the Mode isn't recognized. "
		echo " "
		exit 1
	fi
	EASY=true
	if [ $RES_Y != $RES_X ]; then
		EASY=false
		echo " "
		echo " Your printer uses a different resolution for X- and Y-direction. "
		echo " We will take care of this!"
		echo " "
		RES=${RES_X}x${RES_Y}
	else
		RES=$RES_X
	fi
}

read_config_dir() {
	echo " "
	echo " Please enter the directory-name where TeX keeps its PostScript "
	echo " Configuration-files. If you are not sure where to find it search "
	echo " for a file named config.ps, this is a good starting point! "
	echo " "
	echo -n " TeX-Configuration-Dir : "
	read CONFIG_DIR
}


check_config_dir() {
    if [ ! -d $CONFIG_DIR ];then
		echo " "
		echo " You didn't enter a valid directory-name, please check your input."
		echo " "
		exit 1
	fi
	if [ ! -f `dirname $CONFIG_DIR"/e"`/config.ps ]; then
		echo `dirname $CONFIG_DIR"/e"`/config.ps
		echo " "
		echo " The directory you entered does NOT contain the config.ps-file "
		echo " and so it is very unlikely the TeX-config-directory. "
		echo " Bye!! "
		exit 1
	fi
}

create_config_file() {
	echo " "
	echo " We create a config-file for a $RES DPI Printer in $CONFIG_DIR "
	echo " "
	echo " creating `dirname $CONFIG_DIR"/e"`/config.$RES "
	if [ $EASY = true ]; then
		echo "M $MODE" > `dirname $CONFIG_DIR"/e"`/config.$RES
		echo "R $RES_X" >> `dirname $CONFIG_DIR"/e"`/config.$RES
	else
		echo "M $MODE" > `dirname $CONFIG_DIR"/e"`/config.$RES
		echo "X $RES_X" >> `dirname $CONFIG_DIR"/e"`/config.$RES
		echo "Y $RES_Y" >> `dirname $CONFIG_DIR"/e"`/config.$RES
	fi
	echo " Done ! "
	echo " "
	echo " Much fun using apsfilter together with dvips! "
	echo " "
	exit 0
}


read_mode
check_valid_mode
read_config_dir
check_config_dir
create_config_file
