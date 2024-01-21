/*
 * Dolphin.c
 *
 * This file contains the Dolphin specific hardware stuff.
 *
 * This file is based on the dutch documentation mailed to me by
 * Pascal <pern@ramoth.xs4all.nl>. Since I don't have a Dolphin
 * modem, I cannot test nor debug this driver. But I'm quite
 * optimistic that it will work.
 *
 * Marc
 *
 */

#include "../include/voice.h"

char *libvoice_Dolphin_c = "$Id: Dolphin.c,v 1.11 1996/11/25 19:19:35 marc Exp $";

int Dolphin_answer_phone (void)
     {
     watchdog_flag = FALSE;

     if ((voice_command("ATA", "VCON") & VMA_USER) != VMA_USER)
          return(VMA_ERROR);

     return(VMA_OK);
     }

static int Dolphin_init (void)
     {
     char buffer[VOICE_BUF_LEN];

     watchdog_flag = FALSE;
     voice_modem_state = INITIALIZING;
     lprintf(L_MESG, "initializing Dolphin voice modem");

     /*
      * AT+VNH=1 - Disable automatic hangup.
      */

     if (voice_command("AT+VNH=1", "OK") != VMA_USER_1)
          lprintf(L_WARN, "disabling automatic hangup didn't work");

     /*
      * AT+VSD=x,y - Set silence threshold and duration.
      */

     sprintf(buffer, "AT+VSD=%d,%d", cvd.rec_silence_threshold.d.i * 31 / 100,
      cvd.rec_silence_len.d.i);

     if (voice_command(buffer, "OK") != VMA_USER_1)
          lprintf(L_WARN, "setting recording preferences didn't work");

     voice_modem_state = IDLE;
     return(OK);
     }

static int Dolphin_set_compression (int *compression, int *speed, int *bits)
     {
     watchdog_flag = FALSE;

     if (*compression == 0)
          *compression = 2;

     if (*speed == 0)
          *speed = 9600;

     if (*speed != 9600)
          {
          lprintf(L_WARN, "%s: Illeagal sample rate (%d)", voice_modem_name,
           *speed);
          return(FAIL);
          };

     if (*compression != 2)
          {
          lprintf(L_WARN, "%s: Illeagal voice compression method (%d)",
           voice_modem_name, *compression);
          return(FAIL);
          };

     *bits = 2;

     if (voice_command("AT+VSM=2", "OK") != VMA_USER_1)
          return(FAIL);

     IS_101_set_buffer_size((*speed) * (*bits) / 10 / 8);
     return(OK);
     }

static int Dolphin_set_device (int device)
     {
     watchdog_flag = FALSE;

     switch (device)
          {
          case NO_DEVICE:

               if (voice_write("AT+VLS=0") != OK)
                    return(FAIL);

               if (voice_command("", "AT+VLS=0|OK") == VMA_USER_1)
                    voice_command("", "OK");

               return(OK);
          case DIALUP_LINE:
               voice_command("AT+VLS=2", "VCON");
               return(OK);
          case INTERNAL_SPEAKER:
               voice_command("AT+VLS=16", "VCON");
               return(OK);
          };

     lprintf(L_WARN, "%s: Unknown output device (%d)", voice_modem_name,
      device);
     return(FAIL);
     }

voice_modem_struct Dolphin =
     {
     "Dolphin",
     "Dolphin",
     &Dolphin_answer_phone,
     &IS_101_beep,
     &IS_101_dial,
     &IS_101_handle_dle,
     &Dolphin_init,
     &IS_101_message_light_off,
     &IS_101_message_light_on,
     &IS_101_play_file,
     &IS_101_record_file,
     &Dolphin_set_compression,
     &Dolphin_set_device,
     &IS_101_stop_dialing,
     &IS_101_stop_playing,
     &IS_101_stop_recording,
     &IS_101_stop_waiting,
     &IS_101_switch_to_data_fax,
     &IS_101_voice_mode_off,
     &IS_101_voice_mode_on,
     &IS_101_wait
     };
