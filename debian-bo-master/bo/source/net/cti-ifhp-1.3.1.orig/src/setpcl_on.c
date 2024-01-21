/*==========================================================================
 =                                                                         =
 =                        Project CTI-Print                                =
 =                                                                         =
 = Author:                                                                 =
 =   Panos Dimakopoulos, Systems Programmer,                               =
 =   Computer Technology Institute,                                        =
 =   Division of Computing Facilities,                                     =
 =   P.O. Box 1122,                                                        =
 =   261 10  Patras,                                                       =
 =   Greece                                                                =
 =   (e-mail: dimakop@cti.gr)                                              =
 =   Tel: +30 61 992061                                                    =
 =   Fax: +30 61 993973                                                    =
 =                                                                         =
 = Created by Patrick Powell  <papowell@sdsu.edu>                          =
 =   for LPRng software Sat Aug 26 06:54:25 PDT 1995                       =
 ==========================================================================*/


/*==========================================================================
  Version CTI-LPRng-1.0
  =========================================================================*/


/*
 * $Id: setpcl_on.c,v 1.3 1996/11/14 19:56:36 papowell Exp papowell $
 */

#include "portable.h"
#include "common.h"
#include "hp4.h"


/*
 * The transition is a bit odd. It needs to be accompanied with some
 * nulls and the driver hold off for some time. After experiments 
 * on HP 4M+ the optimum values have been found and used in the next
 * two functions.
 * The following comments from hp.com try to explain the situation:
>What is happening is the printer
>buffer fills with your switch command and the first parts of your job. The
>printer does not "prescan" the input looking for the <Esc>%-1234... but
>instead gets around to it after some time has expired. During the switch,
>the data that you have already trasmitted is discarded and is picked up
>whereever your driver happens to be. For PostScript that's a messy
>situation.
>
>What you need to do is isolate the switch command. You can do this with
>nulls if you have no control over the timing. 8K of them will fill the
>largest allocated I/O buffer and then hold off the UNIX system. The
>switch will be made and the initial remaining nulls will be discarded. If
>you can control the timing of the data, you'll have to experiment with
>the correct time.
>
 * Although 8K is mentioned my experiments worked for just 240 nulls.
 * I cannot be very sure about this.
 *
 *	Sat Aug 26 18:07:04 PDT 1995 Patrick Powell
 * I spent a little time with a line analyzer looking at the HP/system
 * interaction;  it was very interesting.  Apparently the HP system
 * will send an XOFF when the buffer fills,  and will then not
 * send any more until the mode change has been done.  This may take some
 * time.
 *  Note that the counulls variable counts the number of null_str
 *  that are sent;  this used to be a 8 byte string;
 *
 * I have modified this to use the actual number of nulls;
 *
 * Also, in the above notes the author states that '240' was adequate;
 * This is actually 2400; it turns out that the buffer in the system
 * he was using was 2048 bytes long.  Serendipty, thou toothless hag,
 * once again you gum us in the neck.
 *
 * Another interesting observation is that the 'throwing away the data'
 * is really not done;  I am surprised that this statement is made.
 * There may be a delay,  but this is in processing the data,
 * all transmitted  data is processed.
 */

static void set_backend( command, msg )
	char *command;
	char *msg;
{
    int i, cou, counulls=2400, slptm=10;

	log(4,"set_backend: '%s'", command );
	switch( model ){
		case IV: slptm = 0; counulls = 1000; break;
		case IIID: counulls = 3000; slptm = 20; break;
	}
	write_check(STDOUT, command, msg, DIE);
	log(4,"set_backend: sending %d nulls", counulls );
	for (cou = counulls; cou > 0 ;cou -= i ){
		i = cou;
		if( i > sizeof(null_str) ) i = sizeof(null_str);
		if ( writecn(STDOUT,null_str,i) != i ){
			logerr_die(3,"Cannot send nulls");
		}
	}
	log(4,"set_backend: sleeping %d", slptm);
	if( slptm > 0) sleep(slptm);
	log(4,"set_backend: done");
}

void setpcl_on()
{
	set_backend(PCLON, "Cannot set PCL Mode" );
}
void setps_on()
{
	set_backend(POSTON, "Cannot set PostScript Mode" );
}
