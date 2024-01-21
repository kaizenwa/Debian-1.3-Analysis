: /bin/sh
# @(#) send.sh,v 1.2 1990/10/24 05:19:47 tron Exp
#
# Send mail to a remote host that is visible under the /net filesystem
# and that is not secure.
#
# Each site listed under the /net filesystem should have a message
# directory /usr/spool/smail/forpro/<remote-site> and a status
# directory /usr/spool/smail/forpro/<remote-site>/done, where there
# must be a <remote-site> and <remote-site>/done directory for each
# site that can talk to that host, thus, for a network consisting of
# the machines herman, frank and joe, the site herman must have the
# directories frank, joe, frank/done and joe/done.
#
# To transmit a message, first the message itself (which should be in
# batched SMTP format) is copied to the directory
# /net/<remote-site>/usr/spool/smail/forpro/<local-site>, then a file
# is created in the <local-site>/done directory to signal that the
# copy is completed.  For both of these files, the basename is the
# message id with the initial letter removed.
#
# On invocation, argument one is expected to be the host that we are
# sending to and 

set -e

HOSTNAME="$1"
MSGID="`echo "$2" | sed 's/.//'`"
HOST_MAIL_DIR="/net/$HOSTNAME/usr/spool/smail/forpro/`cat /etc/sitename`"

# write the mail message
cat > "$HOST_MAIL_DIR/$MSGID"

# signal that we have finished writing the message
touch "$HOST_MAIL_DIR/done/$MSGID"

exit 0
