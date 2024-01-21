: see end of file for revision information
case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting newsnews (with variable substitutions)"
cat >newsnews <<!GROK!THIS!
			*** NEWS NEWS ***

Welcome to trn 3.6, which continues on the feature trail, adding support
for slow net connections, and squashing some bugs.  Trn is "threaded read
news", based on rn.

You can type 'h' at any prompt to display a summary of the commands that
are available.

This message will not be displayed again unless it is updated with new
information.

Wayne Davison  <davison@borland.com>
!GROK!THIS!
$eunicefix newsnews
#
# $Id: newsnews.SH,v 3.0 1991/09/09 20:23:31 davison Trn $
#
# This software is Copyright 1991 by Stan Barber. 
#
# Permission is hereby granted to copy, reproduce, redistribute or otherwise
# use this software as long as: there is no monetary profit gained
# specifically from the use or reproduction of this software, it is not
# sold, rented, traded or otherwise marketed, and this copyright notice is
# included prominently in any copy made. 
#
# The author make no claims as to the fitness or correctness of this software
# for any use whatsoever, and it is provided as is. Any use of this software
# is at the user's own risk. 

