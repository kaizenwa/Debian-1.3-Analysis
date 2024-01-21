#!/usr/bin/perl

use FvwmCommand;


if( $ARGV[0] eq 't' ) {
	Desk ( 0,1);
	GotoPage (1, 1); 
	Module (FvwmTalk);
}elsif( $ARGV[0] eq 'td' ) {
	KillModule (FvwmTalk);
}else {
	Move ;
}
