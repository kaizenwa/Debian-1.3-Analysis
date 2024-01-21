##############################################################################
#									     #
#			CONFIGURABLE VALUES				     #
#									     #
##############################################################################

$MAILGRUNT="";		# To whom to send mail if mail is prefered.

$TMPDIR="/tmp/";		# Place lostmail can do its dirty work.

$LOCAL_LOCK_EXT=".lock";	# Name of file local mailer uses to lock
				# spool file.  This the correct setting for
				# /bin/mail

$SYSTEM_FROM_ADDRESS="Mailer-Daemon";

$MAILER='/usr/lib/sendmail -t';
#$MAILER='./mailer.sh';


##############################################################################
#									     #
#			DEFAULTED CONFIGURATIONS			     #
#									     #
##############################################################################

$LOGFILE="$TMPDIR" . "lostlog";

$MAILDIR="./";
$LOCKEXT=".lostlock";		# our lock file extension. Should not need to
				# modify

$MESSAGE_DELIM="^From[^:]";	# /bin/mail message delimiter. Your milage
				# may differ

$HEADER_BODY_DELIM="\n";	# RFC 822 header-body delimiter.

$RESENT_TO="Resent-To: ";	# 
$RESENT_FROM="Resent-From: ";	# Resent headers (RFC 822).
$RESENT_DATE="Resent-Date: ";	#  You probably don't want to muck with these.
$RESENT_INFO="X-Resent-Info: ";	# (special one to alert folks about mail).


##############################################################################
#									     #
#			LOSTMAIL DEFINITIONS (DON'T TOUCH)		     #
#									     #
##############################################################################

$FALSE=0;
$TRUE=(! $FALSE);

$OK=$TRUE;
$ABORT_RESEND=2;
$LOCK_RETRIES=10;	# The number of seconds/retries lost mail
				#  should wait before requeing or aborting a
				# resend.

TRUE;				# Ansures true return from include file.
