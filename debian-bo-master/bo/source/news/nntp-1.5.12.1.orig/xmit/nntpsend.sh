#!/bin/sh
# $Id: nntpsend.sh,v 1.4 1994/11/01 05:53:24 sob Exp sob $
# What we have here is a sh script for sending netnews to NNTP sites.
#
newsspool=/usr/spool/news
batchdir=/usr/spool/news/batch
libdir=/usr/spool/news/lib
PATH=${libdir}:/usr/ucb:/usr/bin:/bin:${PATH}
export PATH
pname=`basename $0`
echo ${pname}: "[$$]" begin `date`
#
# Go to where the action is
#
cd $batchdir
umask 022
#
#	For NNTP
#
#	Here "foo", "bar", and "zot" are the Internet names of
#	the machines to which to send.  We make the supposition
#	that the batch files will be a host's internet name.
#	So, for example "nike"'s internet name is "ames-titan.arpa".
#	Because of this, your sys file must have "ames-titan.arpa"
#	as the batch file output for the machine "nike".
#
for host in ${remotes=$*}
do
	lock=L.${host}
	tmp=${host}.tmp
	send=${host}.nntp
	if shlock -p $$ -f ${lock} ; then
		if test -f ${tmp} ; then
			cat ${tmp} >> ${send}
			rm ${tmp}
		fi
# we let the tmp file cool off for a while if there's other work to do
# and we pick it up again during next iteration
		if test -f ${host} ; then
			if test -f ${send} ; then
				mv ${host} ${tmp}
			else
				mv ${host} ${send}
			fi
		fi
		if test -f ${send} ; then
			echo ${pname}: "[$$]" begin ${host}
			(cd $newsspool; time nntpxmit ${host}:${batchdir}/${send})
			echo ${pname}: "[$$]" end ${host}
		fi
		rm -f ${lock}
	else
		echo ${pname}: "[$$]" ${host} locked by "[`cat ${lock}`]"
	fi
done
echo ${pname}: "[$$]" end `date`

