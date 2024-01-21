# File: ptime.pl
# Originally was called: pdate.pl
# Created by: Jeff Kellem <composer@chem.bu.edu>
# Created a little while back..
# Last modified: 21 Feb 1991
# Version: 0.8a (first release)
#
# Minor History:
#   Started out as a quick hack for date formatting for bry (a friend).
#   An OS upgrade replaced date with one that didn't support +FORMAT and he
#   wanted something similar.  So, something quick was hacked up.  Now, it's
#   a simple little package for use within other routines.
#
# routines for producing user-specified date formats
#
# Possible things you may want to change before calling these routines:
# $ptime'default_TZ = timezone to use is if no TZ environment var
# ex: $ptime'default_TZ = 'EST5EDT';
#
# Routines available in this package (usage):
# &'zonetime($time);
# &'zonetime;
#   returns what gmtime($time) and localtime($time) return.
#   if $time is not specified, use current time from time function.  :-)
# &'ptime($format, $time);
#   returns date/time in a user-specified $format.
#   if $time is not specified, use current time.
#   n.b.: if $time is null, will be considered the "beginning of time". 
#   if $format is null, use $ptime'default_format (ctime format).
#   $format interprets the following "escape sequences":
#     ESCAPE	REPLACED BY 
#	%n	newline     
#	%t	tab
#	%a	abbreviated weekday name (Sun to Sat)
#	%A	full weekday name
#	%b	abbrev. month name (Jan to Dec)
#	%h	abbrev. month name (Jan to Dec)
#	%B	full month name
#	%d	day of month (01 to 31)
#	%e	day of month ( 1 to 31) [single digits preceded by a space]
#	%m	month (01 to 12)
#	%D	mm/dd/yy
#	%H	hour (00 to 23)
#	%I	hour (01 to 12)
#	%M	minutes (00 to 59)
#	%S	seconds (00 to 59)
#	%p	AM or PM
#	%j	day of year (001 to 366)
#	%w	day of week (Sunday == 0)
#	%r	hh:mm:ss [AP]M
#	%R	hh:mm
#	%T	hh:mm:ss
#	%y	year (last 2 digits)
#	%Y	year (4 digits, 19xx, 20xx)
#	%Z	timezone
#	%%	a single `%'
#
# Example:
#   &'ptime('',$time);  # is just &'ctime($time);
#   print &'ptime("%D %T %Z\n",time); # prints "02/17/91 14:23:54 GMT\n"
#   $date = &'ptime('%d %h %Y',time); # $date = "17 Feb 1991";
#   $filename = &'ptime('%d_%h_%Y');  # $filename = "17_Feb_1991";
#
# Limitations:
#   Doesn't parse left to right (or at all).. ;-}
#	Just does straight subst's. 
#	So, $format='%%y' will return '%91' instead of '%y'.
#	The %% -> % translation is done last.  Maybe I should move it to
#	the top of the subst's.  hmm... 
#   Is biased toward American date formatting.  May add more hooks for
#	easy conversion to other languages.     
#
# TODO:
#   Should get rid of multiple sprintf's when the same occur more than once.
#   Make it easier to modify for other languages
#	things like separators, order, etc.
#

CONFIG: {
    package ptime;

    $default_TZ = 'GMT';
    $default_format = '%a %h %d %T %Z %Y';  # ctime default format
    $am = "AM";
    $pm = "PM";
    @week = ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    @fullweek = ('Sunday','Monday','Tuesday','Wednesday',
		 'Thursday','Friday','Saturday');
    @month = ('Jan','Feb','Mar','Apr','May','Jun',
	      'Jul','Aug','Sep','Oct','Nov','Dec');
    @fullmonth = ('January','February','March','April','May','June','July',
		  'August','September','October','November','December');
}

sub zonetime {	# first arg is time (# of secs)
    package ptime;

    $time = $_[0] || ( ! defined $_[0] && time ) || 0;
    $TZ = $ENV{'TZ'} || $default_TZ;
    $TZ eq 'GMT' ? gmtime($time) : localtime($time);
}

sub ptime {
    package ptime;

    local($_, $time) = @_;	# $_ is format, $time is time (# of secs)

    ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = &'zonetime($time);

    $ampm = ($hour % 12);
    $ampm += ($hour % 12) ? 0 : 12;
    $p = ($hour < 12) ? $am : $pm;

    $_ = $default_format unless $_;
    s/%n/\n/g;	# newline
    s/%t/\t/g;	# tab
    s/%p/$p/g;				# AM or PM
    s/%m/sprintf("%02d",$mon+1)/eg;	# month (01 to 12)
    s/%d/sprintf("%02d",$mday)/eg;	# day of month (01 to 31)
    s/%e/sprintf("%2d",$mday)/eg;	# day of month ( 1 to 31)
    # no special formatting for month & day?  need new %-formats
    #s/%m/$mon+1/eg;	# month (1 to 12)
    #s/%d/$mday/g;	# day (1 to 31)
    s/%y/sprintf("%02d",$year)/eg;	# year (last 2 digits)
    # prints 2069 instead of 1970 for things like EST (oh well.. ;-)
    s/%Y/$year + (($year < 70) ? 2000 : 1900)/eg; # year (19yy or 20yy)
    s:%D:sprintf("%02d/%02d/%02d",$mon+1,$mday,$year):eg;	# mm/dd/yy
    s/%H/sprintf("%02d",$hour)/eg;	# hour (00 to 23)
    s/%I/sprintf("%02d",$ampm)/eg;	# hour (01 to 12)
    s/%M/sprintf("%02d",$min)/eg;	# minutes (00 to 59)
    s/%S/sprintf("%02d",$sec)/eg;	# seconds (00 to 59)
    s/%T/sprintf("%02d:%02d:%02d",$hour,$min,$sec)/eg;  # HH:MM:SS
    s/%j/sprintf("%03d",$yday+1)/eg;# day of year (001 to 366)
    s/%w/$wday/g;# day of week (Sunday == 0)
    s/%a/$week[$wday]/g;	# abbrev. weekday (Sun to Sat)
    s/%A/$fullweek[$wday]/g;	# full weekday name
    s/%[bh]/$month[$mon]/g;	# abbrev. month (Jan to Dec)
    s/%B/$fullmonth[$mon]/g;	# full month name
    s/%r/sprintf("%02d:%02d:%02d $p", $ampm, $min, $sec)/eg; # hh:mm:ss [AP]M
    s/%R/sprintf("%02d:%02d", $hour, $min)/eg;	# hh:mm
    # are we in Daylight Savings Time?
    $TZ =~ s/^(\w{3})-?\d+(\w{3})$/$isdst ? $2 : $1/e;
    s/%Z/$TZ/g;	# timezone as specified by $TZ
    s/%%/%/g;	# single `%'
    $_;
}

# in case someone doesn't want to rely on $ptime'default_format or
# just wants a &'ctime sub.
sub ctime {
    $time = $_[0];
    $format = "%a %h %d %T %Z %Y\n";
    &'ptime($format, $time);
}

1;
