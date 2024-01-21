# ansi.ph
# 
#  Converted from Micro^[['s ANSi.h to perl by
#  Chris Church 
#  ANSi.h (c) 1995 admin@micro.psyberlink.net
#  ansi.ph (c) 1996 psylark@aD.org
#
#  Usage :
#
# require 'ansi.ph';
# do setforecolor(5);
# do setbackcolor(0);
# do clrscr(); (NOOP ?)
# do gotoxy(x, y);
#
#----------------------------------------
 

sub setforecolor {
  $fore = shift(@_);
  $fore = 0 unless($fore);
  if($fore > 15) {
  	return("Invalid color!");
  	}
  if($fore > 7) {
  	$colorstr = "1;3";	
	} else {
		$colorstr = "0;3";
		}
  $colorstr = $colorstr . $fore;
  print("\033[${colorstr}m");
}		


  
sub setbackcolor {

 $back = shift(@_);
 $back = 7 unless(@_);
 undef($colorstr);
 $colorstr = "4";
 if($back > 7) {
 	return("Invalid Color!");
 	}
  $colorstr = $colorstr . $back;
 print("\033[${colorstr}m");
}

sub clrscr {
 print("\033[2J");
}

sub gotoxy {
 $x = shift(@_);
 $y = shift(@_);
 print("\033[$y;${x}H");
}

1;

