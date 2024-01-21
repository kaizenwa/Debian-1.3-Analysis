/*
 * mode.c
 *
 * Contains the functions voice_mode_on and voice_mode_off.
 *
 */

#include "../include/voice.h"

char *libvoice_mode_c = "$Id: mode.c,v 1.5 1996/10/30 06:19:03 marc Exp $";

int voice_mode_on _P0(void)
     {
     lprintf(L_NOISE, "%s: entering voice mode", program_name);
     voice_install_signal_handler();
     voice_modem->voice_mode_on();
     return(OK);
     }

int voice_mode_off _P0(void)
     {
     lprintf(L_NOISE, "%s: leaving voice mode", program_name);
     voice_modem->voice_mode_off();
     voice_restore_signal_handler();
     return(OK);
     }
