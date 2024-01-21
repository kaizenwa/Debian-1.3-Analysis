#!/bin/perl 
#
#
# COURTNEY: program to detect SATAN scans.
#
# Copyright (c) 1995
#   The Regents of the University of California.  All rights reserved.
#
# This work was produced at the University of California, Lawrence
# Livermore National Laboratory (UC LLNL) under contract number
# W-7405-ENG-48 (Contract 48) between the U.S. Department of Energy
# (DOE) and the Regents of the University of California (University) for
# the operation of UC LLNL.  Copyright is reserved to the University for
# purposes of controlled dissemination, commercialization through formal
# licensing, or other disposition under Contract 48; DOE policies,
# regulations and orders; and U.S. statutes.  The rights of the Federal
# Government are reserved under Contract 48 subject to the restrictions
# agreed upon by the DOE and University as allowed under DOE Acquisition
# Letter 88-1.
#
# REVISIONS:
#   3/22/95: Marvin J. Christensen  V1.0  Original release 
#   3/29/95: Paul S. Mauvais 	    V1.01 Changed MONITOR to false to
#					  save screen traffic
#   3/29/95: Steve Weeber           V1.02 Added syslog reporting
#   3/30/95: Shawn Instenes         V1.1  Optimized code, fixed repeated
#                                         syslog
#   3/31/95: Paul S. Mauvais        V1.2  Fixed syslog to make it more
#					  portable -- added PATH fixes
#   4/07/95: Marvin J. Christensen  V1.3  Implemented the follow suggestions
#            Dale Drew                    Command line switches
#            Dale Drew                    Interface switch capability
#            Dale Drew                    Email capability
#            Kevin Oberman                OSF/1 compatability 
#            Charley Knox                 OSF/1 compatability 
#            Don Moss                     HEAVY_ATTACK mis-label
#            Dale Drew                    HEAVY_ATTACK mis-label
#            Bob Baldwin                  tcpdump filtering
#
# send flames/comments/suggestions/accolades to "mjchristensen@llnl.gov"
# ----------------------------------------------------------------------
#
# usage: courtney.pl ([-i <interface>]  change default interface 
#                     [-d]              turn debug on
#                     [-l]              turn syslog logging off
#                     [-s]              turn screen output on
#                     [-c]              show connections  
#                     [-m <address>]    mail to user@host
#                     [-h]              print options )
# eg: courtney.pl -h  ; for HELP

require 5; # This won't run on Perl 4.

# Let's make sure we're OK
$ENV{'PATH'} = '/bin:/usr/bin:/usr/ucb:/usr/bsd:/usr/sbin:/usr/etc:/usr/local/bin';
$ENV{'SHELL'} = '/bin/sh' if $ENV{'SHELL'} ne '';
$ENV{'IFS'} = '' if $ENV{'IFS'} ne '';

@assoc_list  = (  'sunrpc',  'icmp',    'ttime',   'telnet',   'smtp', 
                  'ftp',     'whois',   'domain',  'gopher',   'www',
                  'finger',  'exec',    'login',   'shell',    'printer',
                  'uucp',    'tcpmux',  'echo',    'discard',  'systat',
                  'daytime', 'netstat', 'chargen', 'tftp',     'name',
                  'biff',    'syslog',  'talk',    'portscan', 'xwindows' );

$UPDATE_INTERVAL = 5;		# update host information every 
				# X minutes
$OLD_AGE = 7;			# get rid of hosts that are
				# older that X minutes.

$HIGH_THRESHOLD = 15;		# heavy  "SATAN" attack
$LOW_THRESHOLD  = 9;		# normal "SATAN" attack

$TRUE      = 1;
$FALSE     = 0;
$last_time = 0;
$MODULUS   = 1440;		# number of minutes per day


#
# define the command line options
#
&case(*options,<<'ENDCASE');
'-i' $interface=$ARGV[$i+1];
'-d' $debug=$ARGV[$i]; 
'-l' $logger=$ARGV[$i];
'-s' $screen=$ARGV[$i];
'-c' $connect=$ARGV[$i];
'-m' $mail=$ARGV[$i+1];
'-h' $help=$ARGV[$i];
ENDCASE
foreach $i (0..$#ARGV){
    eval $options{$ARGV[$i]};
}

# INTERFACE -- command line option
$tcpdump = ($interface) ? "tcpdump -i $interface -l" : "tcpdump -l";

# DEBUG -- command line option
$DEBUG = ($debug) ? $TRUE : $FALSE; 
print "DEBUG enabled\n" if $DEBUG;

# HELP -- command line option
if ($help) {
 print "usage:  courtney.pl [options] \n";
 print "    OPTIONS ARE: \n";
 print "         [-i <interface_name>]   'default Interface if blank'\n";
 print "         [-d]                    'Debug/verbose mode on'\n";
 print "         [-l]                    'syslog Logging off\n";
 print "         [-s]                    'Screen output on\n";
 print "         [-c]                    'display Connections\n";
 print "         [-m <address>]          'Mail alerts to address\n";
 print "                                 \n";
 print " eg: courtney.pl -h               for HELP \n";
 exit;
}



#
# Starting loop of main program
#

open (TCPDUMP, "$tcpdump '\
    (icmp[0] == 8 ) or \
    (port sunrpc) or \
    ((port (1 or 10 or 100 or 1000 or 5000 or 10000 or 20000 or 30000) or \
      (port (6000 or 6001 or 6002 or 6010 or 6011 or 6012)) ) and \
      (tcp[13] & 18 == 2) )  or \
    (port (tcpmux or \
	   echo or \
	   discard or \
	   systat or \
	   daytime or \
	   netstat or \
	   chargen or \
	   ftp or \
	   telnet or \
	   smtp or \
	   time or \
	   whois or \
	   domain or\
	   70 or \
	   80 or \
	   finger or \
	   tftp or \
	   login or \
	   uucp or \
	   printer or \
	   shell or \
	   exec or \
	   name or \
	   biff or \
	   syslog or \
	   talk) and \
          (tcp[13] & 18 == 2) ) \
    ' |") || die "Unable to execute tcpdump command: $!\n";



while(<TCPDUMP>) {

 chop;
 @line =  split;

#
# parse the input line for time, source, dest, and protocol
#

  if($line[4] =~ "icmp") {
    ($time)        = $line[0] =~ /(\d+:\d+:\d+).\d+/;
    ($src)         = $line[1];
    ($dst)         = $line[3]; 
                     chop($dst);
    ($proto)       = $line[4];
                     chop($proto);
  } else {
    ($time)        = $line[0] =~ /(\d+:\d+:\d+).\d+/;
            @src_a = split(/\./, $line[1]);
    ($src)         = join(".", @src_a[0..($#src_a - 1)] );
            @dst_a = split(/\./, $line[3]);
    ($dst)         = join(".", @dst_a[0..($#dst_a - 1)] );
    ($proto)       = @dst_a[$#dst_a];
                     chop($proto);
  }


#
# calculate time based on input 
#  only worry about minutes
#  modulus 1440 (24 hours * 60 minutes/hour)
#  update_time_interval based on UPDATE_INTERVAL


   ($hours, $minutes, $junk) = split(/:/, $time);
   $current_time = ((60 * $hours) + $minutes);
   
   $delta_time = ($current_time >= $last_time) ? 
                     ($current_time - $last_time) : 
                    (($current_time + $last_time) % $MODULUS);
   if ($delta_time >= $UPDATE_INTERVAL ) {
      $time_to_update = $TRUE;
      $last_time = $current_time;
   } else {
      $time_to_update = $FALSE;
   }


#
# create associative arrays based on protocol
#

  ENTRY: {

    print "PROTO: $proto, \n"  if $DEBUG;

     if($proto eq "70")      { $gopher{$src}   = $current_time; last ENTRY }; 
     if($proto eq "80")      { $www{$src}      = $current_time; last ENTRY }; 
     if( ($proto == 1)     || ($proto ==10 )    || ($proto == 100)    || 
         ($proto == 1000)  || ($proto == 5000 ) || ($proto == 10000)  ||
         ($proto == 20000) || ($proto == 30000) )  
                             { $portscan{$src} = $current_time; last ENTRY }; 
     if( ($proto == 6000)  || ($proto == 6001)  || ($proto == 6002)   ||
         ($proto == 6010)  || ($proto == 6011)  || ($proto == 6012) )  
                             { $xwindows{$src} = $current_time; last ENTRY }; 
     
# This works because the packet has to be in the set defined above or in
# @assoc_list, because tcpdump isn't reporting anything else.

    if ($assoc_list[$proto]) {
        $$proto{$src} = $current_time; last ENTRY;
    } else {
        next;                                 # garbage, go get another entry
    }

  }


#
# clean up associative array list getting rid of entries where the
# time (e.g., value) is older than $OLD_AGE
#

  if ($time_to_update) {
    print "TIME_TO_UPDATE $current_time, $last_time \n" if $DEBUG;

    undef %heavy_attack;                         # clean up lists that
    undef %normal_attack;                        # have identified hosts

    $entry = 0;                                  # used for debug
    foreach $assoc_name (@assoc_list) {

      print "ENTRY $entry  $assoc_list[$entry] \n" if $DEBUG;
      $entry++;

      foreach $key (keys %$assoc_name ) {        
        print "foreach delete: $key $$assoc_name{$key} <-> $current_time \n" 
                                                                    if $DEBUG;

        print "delete: $$assoc_name{$key}\n" 
               if (((($current_time >= $$assoc_name{$key}) ? 
                     ($current_time  - $$assoc_name{$key}) : 
                    (($current_time  + $$assoc_name{$key}) % $MODULUS) )
                                                 >= $OLD_AGE) && $DEBUG);
        delete($$assoc_name{$key}) 
               if (( ($current_time >= $$assoc_name{$key}) ? 
                     ($current_time  - $$assoc_name{$key}) : 
                    (($current_time  + $$assoc_name{$key}) % $MODULUS) )
                                                 >= $OLD_AGE);
      };
    };
  };


#
# count the number of times a host is contained in the associative
# arrays. For each assocative array, extract the hostname and
# increment the count for that hostname.
#
  undef %host_count;
  $entry = 0;                                     # used for debug
  foreach $assoc_name (@assoc_list) {
   print "ENTRY $entry $assoc_list[$entry] \n" if $DEBUG;
   $entry++;

     foreach $key (keys %$assoc_name ) {
       print "hostcount: $key $$assoc_name{$key} \n" if $DEBUG;

       $host_count{ $key }++;

     };
  };


#
# If the host count is above a threshold, print out the hostname,
# using the timestamp from the tcpdump input, and then add the
# host to the array so that you don't keep printing the host
# once it has been identified. There are two lists, one for
# normal scans and one for heavy scans.

  foreach $host (keys %host_count) {
    print "counted: $host_count{$host} for $host \n" if ($DEBUG || $connect);

    if ($host_count{$host} >= $HIGH_THRESHOLD) { 
      if (! $heavy_attack{$host}) {
        $heavy_attack{$host} = 1;
        delete $normal_attack{$host};
        if ($mail) {
          open (SM, "| Mail -s 'HEAVY_ATTACK from $host - target $dst' $mail");
          close(SM);
        };
	if (!$logger) {
	    system ("logger -p alert courtney[$$]: HEAVY_ATTACK from $host - target $dst");
	};
	if ($screen) {
	    print "$time: HEAVY_ATTACK from $host - target $dst\n";
	};
        print "$host added to heavy attack \n" if $DEBUG;
      };
    } 
    elsif ($host_count{$host} >= $LOW_THRESHOLD) {
     if (! $normal_attack{$host}) {
        $normal_attack{$host} = 1;
        if ($mail) {
          open (SM, "| Mail -s 'NORMAL_ATTACK from $host - target $dst' $mail");
          close(SM);
        };
	if (!$logger) {
	    system ("logger -p alert courtney[$$]: NORMAL_ATTACK from $host - target $dst"); 
	};
	if ($screen) {
	    print "$time: NORMAL_ATTACK from $host - target $dst\n";
	}
        print "$host added to normal attack \n" if $DEBUG;
      };
    };
  };
  

}; # endwhile

sub case {
    local(*assoc,$_)=@_;
    for (split(/\n/)){
        /^(\S+)\s+(.*)/;
        for (eval $1) {
            $assoc{$_}=$2;
        }
    }
}

# all done. 
#
# TODO: 
#
# USE ONLY THE FIRST TCP RECORD.
# tcpdump generates two records for each establish connection. since
# courtney is only concerned with the originating machine, the second
# record is not used. However, the current code executes all the way
# through with this second record. courtney should stop right after the 
# switch statement and return back to the top of the while loop.
