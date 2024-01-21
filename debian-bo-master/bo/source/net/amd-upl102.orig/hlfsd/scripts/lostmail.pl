#!/usr/local/gnu/bin/perl

#$CONFIGDIR="/u/pizza/jtt/Work/Perl/lostmail/"; # Directory where config lives.
$CONFIGDIR="/etc";	# Directory where config lives.
require "$CONFIGDIR/lostmailcfg.pl" || die "Cannot find lostmail configuration file";
require "ctime.pl";

##############################################################################
#									     #
#				LOG_INFO				     #
#									     #
##############################################################################

# Opens appropriate logging file -- STDOUT (cron) or temp file (mail).

sub Log_info {
    local($message,$lastchar) = @_;

    if ( ! defined ($logopened) )  {
	if ( $MAILGRUNT eq "" ) {
	    open (LOGFILE, ">-") || die  "Unable to open stdout";
	}
	else {
	    # Snarf the log into a tmp file for final mailing to MAILGRUNT
	    $logfile = $LOGFILE . ".$$";
	    open (LOGFILE, (">". "$logfile")) || die "Unable to create log file";
	}
    }

    $logopened=1;		# Note that the log is now open

    # Heart of the function.
    printf(LOGFILE "%s", $message);

    if ( index($message,"\n") == -1 ) {
	printf(LOGFILE "\n");
    }
}


##############################################################################
#									     #
#				LOCK_FILE				     #
#									     #
##############################################################################

# Tries to grab a lock on the supplied file name.
# Spins for a bit if it can't on the assumption that the lock will be released
#	quickly.  If it times out and it's allowed to requeue, it will defer
#	until later, other wise write a message to loginfo.
#
# If a recurring error or really unexpected situation arrises, return
# 	ABORT_RESEND

#  PARAMETERS
# mailfile: path to the file to resend.
# should_requeue: BOOLEAN - TRUE if the mailfile should be put on the
				# queue for a later retry if we can't finish
				# now.
sub Lock_file {

    local($mailfile,$should_requeue,$i,$new_lost_file) = @_;

# We need to rename the current mailbox so that mail can loop back into it if
# the resent mail just gets looped right back to us.
    $new_lost_file = $mailfile . "$$";

#  make a tmpfile name based on mailfile;
    $lostlockfile = "$mailfile" . "$LOCKEXT";

    if ( ! open(LOCKFILE, (">" . $lostlockfile)) ) {
	printf(STDERR "Could not create lostlockfile for %s: %s\n", $mailfile,$!);
	return $ABORT_RESEND;
    }
    close(LOCKFILE);

    $maillockfile = "$mailfile" . "$LOCAL_LOCK_EXT";

    for ($i=0; $i < $LOCK_RETRIES && ! link ($lostlockfile, $maillockfile);
	 $i++) {
	sleep(1);
    }

    unlink($lostlockfile);	# No matter what eliminate our cruft

    if ( $i == $LOCK_RETRIES ) {
	&Log_info("Could not grab lock on: " . "$mailfile" . " :timed out");
	if ( $should_requeue ) {
	    &Log_info("Requeing " . "$mailfile" . " for later retry");
	    $retry_list .= " $mailfile";
	}
	else {
	    &Log_info("Giving up on: " . "$mailfile");
	}

	return $ABORT_RESEND;
    }

    # We created the link and therefore have the lock

    if (rename ($mailfile, $new_lost_file) == 0 ){
	# Failed to rename file -- this is serious.
	unlink($maillockfile);
	return $ABORT_RESEND;
    }

    unlink($maillockfile);
    return $new_lost_file;

}

##############################################################################
#									     #
#			PARSE NEXT MAIL MESSAGE				     #
#									     #
##############################################################################

# Parameters:
#  mailfile: handle of mailfile to use.

# Parses the next message in the mail file and inserts it in $current_msg

sub Get_next_msg {
    local($mailfile,$found_body_delimiter) = @_;

    # If this is the first message in the spool file, read the first line
    # otherwise use the MESSAGE_DELIM line from the previous message (which we
    # were forced to overread).

    $done=$FALSE;
    $found_body_delimiter=$FALSE;

    # This if eats the very first "From " line and should never fire again.
    if ( ! defined $current_msg ) {<$mailfile>};
    undef ($current_msg);	# Erase the old message.


    # Read the mailfile and pass through all the lines up until the next message
    # delimiter. Kill any previous resend headers.
    while ( <$mailfile> ) {
	last if (/$MESSAGE_DELIM/);
	next if ( !$found_body_delimiter && /[Rr][Ee][Ss][Ee][Nn][Tt]-.+:/);
	if (  !$found_body_delimiter && /^$HEADER_BODY_DELIM/) {
	    &Splice_in_resent_headers();
	    $found_body_delimiter=$TRUE;
	}

	$current_msg .= $_;
    }

    # Return TRUE when we've hit the end of the file.
    if ( $_ eq "" ) { return $TRUE; }
    else {return $FALSE; }

}

##############################################################################
#									     #
#			SPLICE IN RESENT_HEADERS			     #
#									     #
##############################################################################

# Insert the Resent- headers at the *current location* of the message stream
# (In Engish, print out a few Resent-X: lines and return :-) )
# In addition splice in the X-resent-info: header.
#

# Paremters: None.
# Return: None

sub Splice_in_resent_headers {
    local($date,$utctime,$weekday,$time,$month,$hostname);

    $current_msg .= "$RESENT_TO" . "$currentTO" . "\n";
    $current_msg .= "$RESENT_FROM" . "$SYSTEM_FROM_ADDRESS" . "\n";

    # Calculate date and time.  It's a bit of a shame to do this each time
    # the time needs to be acurate.

    @utctime=gmtime(time);

    $weekday=(Sun,Mon,Tue,Wed,Thu,Fri,Sat)[@utctime[6]];


    # If the minutes or second don't take two columns each, patch em up.
    if ( @utctime[1] < 10 ) {
	if ( @utctime[0] < 10 ) {
	    $time=sprintf("%d:0%d:0%d",@utctime[2],@utctime[1],@utctime[0]);
	}
	else {
	    $time=sprintf("%d:0%d:%d",@utctime[2],@utctime[1],@utctime[0]);
	}
    }
    else {
	if ( @utctime[0] < 10 ) {
	    $time=sprintf("%d:%d:0%d",@utctime[2],@utctime[1],@utctime[0]);
	}
        else {
	    $time=sprintf("%d:%2d:%2d",@utctime[2],@utctime[1],@utctime[0]);
	}
    }

    $month=(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)[@utctime[4]];

    $date=sprintf("%s, %d %s %d %s UTC", $weekday, @utctime[3], $month, ((@utctime[5] < 93 ? 20 : 19).@utctime[5]), $time);

    $current_msg .= "$RESENT_DATE" . $date . "\n";

    if ( defined $RESENT_INFO && $RESENT_INFO ne "") {
	$hostname=`/bin/hostname`;
	$current_msg .= "$RESENT_INFO" . "Lost mail resent from ". $hostname;
    }

    return;
}


##############################################################################
#									     #
#				DO_REMAIL				     #
#									     #
##############################################################################

# Actually resends the mail.   Talks to the process configured as $MAILER
# We need better handling.

sub Do_remail {
    open (MAILER, "| $MAILER $currentTO") || return $ABORT_RESEND;
    print MAILER $current_msg;
    close (MAILER);
}


##############################################################################
#									     #
#				CLEAN_UP				     #
#									     #
##############################################################################

# Clean up my messes.

sub Clean_up {


    # Mail any log info to MAILGRUNT.
    if ( $MAILGRUNT ne "" ) {
	if ( defined $logfile ) {
	    open (MAILER, "| $MAILER $MAILGRUNT");

	    print MAILER "To: $MAILGRUNT\n$HEADER_BODY_DELIM";

	    close (LOGFILE);
	    open (LOGFILE, "< $logfile");

	    while (<LOGFILE>) {
		print MAILER $_;
	    }
	    close (MAILER);
	}

	close (LOGFILE);
	unlink($logfile);
    }

    exit(0);
}


##############################################################################
#									     #
#				COLLECT_WANDERERS			     #
#									     #
##############################################################################

# Collects other files that appear to be mail file for the $currentTO
# but weren't remailed successfully.

# Parameters: none (but uses $currentTO)
# Return:  True if a old mail directory is found. False otherwise.
# Side effects: $wanderers set.

sub Collect_wanderers {
    local (@allfiles,$found);

    $found=$FALSE;


    # Slurp in the directory and close.
    if ( ! opendir(MAILDIR, "$MAILDIR") ) {
	&Log_info("Could not search $MAILDIR. Old lost mail resending suspended");
	return $FALSE;
    }

    if ( !(@allfiles = readdir(MAILDIR)) ) {
	&Log_info("Could not read $MAILDIR. Old lost mail resending suspended");
	return $FALSE;
    }
    closedir (MAILDIR);


    # Check for an files that match $currentTO with a postpended PID.
    foreach $file (@allfiles) {
	if ( $file =~ /$currentTO\d+/ ) {
	    push (@wanderers, $file);
	    $found=$TRUE;
	}
    }
    return ($found);
}


#############################################################################
#									    #
#				REMAIL ALL				    #
#									    #
#############################################################################

# Takes an array of files that all seem to share a common repcipient and
# remails them if possible.

# Parameters: None (uses @wanderers).

sub Remail_all {
    local($file,$i);

    $i=0;
    foreach $file (@wanderers) {
	if ( !open (LOSTFILE, "< $file"))  {
	    &Log_info("Could not open " . "$file" . " for remailing");
	    next;
	}

	do {			# Power loop!
	    $done = &Get_next_msg(LOSTFILE); # Retrieve the next message...
	    &Do_remail;		# and remail it.
	} until $done;
	undef ($current_msg);	# Erase the final remailed message.

	close(LOSTFILE);	# Tidy up.

	unlink ($file);		# Remove the remailed file
	$i++;
    }

}

#############################################################################
#									    #
#				CHECK_USER				    #
#									    #
#############################################################################

# Checks the password tables for the uid of $currentTO. If the user is 
# uid 0 (ie *supposed* to get mail in altmail) or unknown the resend is 
# aborted.

sub Check_user {
    local ($name,$passwd,$uid,$gid,$quota,$real_name,$home,$shell);

    ($name,$passwd,$uid,$gid,$quota,$real_name,$home,$shell) = getpwnam($currentTO);

    if ( ! defined $uid ) {
	&Log_info("Possible non user mail file: $currentTO");
	return $ABORT_RESEND
	}
    return $ABORT_RESEND if ( $uid == 0 );

    return;
}

#############################################################################
#									    #
#				MAIN PROC				    #
#									    #
#############################################################################

$currentTO = @ARGV[0];

exit 1 if ( &Check_user == $ABORT_RESEND);

&Collect_wanderers;		# Pick up stragglers.

$remail_file = &Lock_file($currentTO,$FALSE); # Need to lock the spool.

if ( $remail_file == $ABORT_RESEND) { # Couldn't get that lock
    &Clean_up if ( ! defined @wanderers); # and there isn't any other work,
}
else {
    push (@wanderers, $remail_file); # Try to resend `old' files.
}


&Remail_all;			# Remail the lost list. (@wanderers).

&Clean_up;			# Do a clean exit.
