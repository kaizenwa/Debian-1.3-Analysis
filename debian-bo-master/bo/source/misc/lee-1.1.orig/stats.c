/* stats.c
 *                         Copyright (1993)
 *
 *         Jeff Elman.  University of California, San Diego
 *          Rik Belew.  University of California, San Diego
 *      Stefano Nolfi.  Institute of Psychology, Rome.
 *    Filippo Menczer.  University of California, San Diego
 *        Greg Linden.  University of California, San Diego
 *
 *        This software may be redistributed without charge;
 *                 this notice should be preserved.
 */
 
#include "defs.h"


/*
 * called by generati() in populati.c;
 * we save on the results (.dat) file
 * whatever the experiment needs to monitor;
 * format: [tab datum tab datum ... tab datum] 
 */
save_dat(fp)

	FILE	*fp;
{
	return;	/* NOP */
}
