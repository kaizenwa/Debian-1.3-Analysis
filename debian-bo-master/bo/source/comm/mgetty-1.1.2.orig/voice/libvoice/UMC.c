/*
 * Umc.c
 *
 * This file contains the UMC UM92144EF specific hardware stuff.
 * (e.g. Creatix PhoneMaster 144VFi)
 *
 * This file is written by
 * Ulrich Homann <ulho@uni-paderborn.de>.
 *
 * Creatix Phonemaster 144VFi for sale (cheap&nasty UMC based Modem)
 *   contact me!
 */

#include "../include/voice.h"

char *libvoice_UMC_c = "$Id: UMC.c,v 1.7 1996/11/07 18:38:56 marc Exp $";

#ifdef UPDATE_COMPLETE

#define UMC_RELEASE "0.01"
/* #define UMC_VTS_WORKAROUND yes */
/* #define UMC_EXTENDED_DETECTION yes */
#define UMC_SPEAKER_ON yes
/* workaround: it should set by vgetty
 * ! program_name must be vgetty !
 */

/*
 * Here we save the current mode of operation of the voice modem when
 * switching to voice mode, so that we can restore it afterwards.
 */

static char umc_mode_save[VOICE_BUF_LEN] = "";

/*
 * Internal status variables for aborting some voice modem actions.
 */

static int stop_dialing;
static int stop_playing;
static int stop_recording;
static int stop_waiting;

/*
 * The UMC samples with 7200 samples per second with a maximum of 4 bit
 * per sample. We want to buffer voice data for 0.1 second, so we need a
 * buffer less or equal to 7200 * 0.5 * 0.1 = 360 bytes.
 */

#define UMC_BUFFER_SIZE 360
static int umc_buffer_size = UMC_BUFFER_SIZE;

/*
 * <DLE><ETX> string
 */
static char dletx[2]={ DLE, ETX };

/*
 * This function handles the <DLE> shielded codes.
 */

#define ST_NO_INPUT (0x00)
#define ST_GOT_DLE  (0x01)

static int umc_answer_state = ST_NO_INPUT;

static void handle_umc_answer(char new_byte) {
   switch (umc_answer_state) {

    case ST_NO_INPUT:

      switch (new_byte)  {
       case DLE:
      umc_answer_state = ST_GOT_DLE;
      break;
       case XON:
      lprintf(L_WARN, "%s: Received XON",
           program_name);
      break;
       case XOFF:
      lprintf(L_WARN, "%s: Received XOFF",
           program_name);
      break;
       case NL:
       case CR:
      break;
       default:
      lprintf(L_ERROR, "%s: Received illegal modem answer 0x%2x",
           program_name, new_byte);
      };

      return;

    case ST_GOT_DLE:
      switch (new_byte)  {
      /* 1209      1336      1477      1633     Hz */
       case '1': case '2': case '3': case 'A': /* 697 */
       case '4': case '5': case '6': case 'B': /* 770 */
       case '7': case '8': case '9': case 'C': /* 852 */
       case '*': case '0': case '#': case 'D': /* 941 */
      voice_handle_event(RECEIVED_DTMF, (event_data) new_byte);
      break;

       case 'a':
      lprintf(L_WARN, "%s: Surprise (ignored): Answer Tone ?",
           voice_modem_name);
      break;

       case 'b':
      voice_handle_event(BUSY_TONE, (event_data) 0);
      break;

       case 'c':
      voice_handle_event(FAX_CALLING_TONE, (event_data) 0);
      break;

       case 'd':
      voice_handle_event(DIAL_TONE, (event_data) 0);
      break;

       case 'e':
      voice_handle_event(DATA_CALLING_TONE, (event_data) 0);
      break;

       case 'f':
      lprintf(L_WARN, "%s: Surprise (ignored): Bell Answer Tone ?",
           voice_modem_name);
      break;

       case 'h':
      voice_handle_event(HANDSET_ON_HOOK, (event_data) 0);
      break;

       case 'o':
      lprintf(L_WARN, "%s: Surprise (ignored): Overrun ?",
           voice_modem_name);
      break;

       case 'q':
      voice_handle_event(SILENCE_DETECTED, (event_data) 0);
      break;

       case 's':
      voice_handle_event(NO_VOICE_ENERGY, (event_data) 0);
      break;

       case 't':
      /* while recording from telcoline this modem doesn't recognize it.
       * instead SIGHUP should be sent to vgetty.
       */
      voice_handle_event(HANDSET_OFF_HOOK, (event_data) 0);
      break;

       case 'u':
      /* voice_handle_event(UNDERRUN_ERROR, (event_data) 0); */
      lprintf(L_WARN, "%s: Underrun Error (try increasing PortSpeed)",
           voice_modem_name);
      break;

       case 'T':
      lprintf(L_WARN, "%s: Surprise (ignored): Timing Mark ?",
           voice_modem_name);
      break;

       default:
      lprintf(L_ERROR, "%s: Received illeagal <DLE> shielded code 0x%2x",
           program_name, new_byte);
      }

      umc_answer_state = ST_NO_INPUT;
      return;
   }
}

/*
 * This is the main handle event routine for UMC based modems
 */

int UMC_handle_event(int event, event_data data) {
   char buffer[VOICE_BUF_LEN];

   switch (event) {

    case VOICE_ANSWER_PHONE:
#ifdef UMC_SPEAKER_ON
      if (strcmp(program_name,"vgetty")==0){
      voice_command("AT#VLS=4", "OK");
      }
      /* After an AT#VLS=X command, the modem realease the line for a short
       * period.
       * So it should be done before off-hook.
       * After an AT#VLS=X command, the modem blocks the line.
       * So it shouldn't be done earlier.
       */
#endif
      return(voice_command("ATA", "VCON"));

    case VOICE_BEEP: {
#ifdef UMC_VTS_WORKAROUND
       /* generate a beep with 900Hz
     * sorry: just a near miss.
     */
       TIO tio_save;
       TIO tio;

       char *sinewave="\x37\x8c\xc8\x73";
       int sinelen=4;
       int i;

       tio_get(voice_fd, &tio);
       tio_save = tio;
       tio_set_flow_control(voice_fd, &tio, FLOW_HARD);
       tio_set(voice_fd, &tio);

       voice_command("AT#VBS=4", "OK");
       voice_command("AT#VTX", "CONNECT");

       lprintf(L_JUNK, "%s->%s: sinewave", program_name, voice_modem_name);
       for (i=7200/2/sinelen/10*data.beep.length; i>0; i--){
       /* samplerate 2samples/byte lenght (1/10)seconds */
       if (write(voice_fd,sinewave,sinelen) != sinelen)
         lprintf(L_ERROR, "%s->%s: write error (errno 0x%x)",
              program_name, voice_modem_name, errno);
       }
       lprintf(L_JUNK, "%s->%s: <DLE><ETX>", program_name, voice_modem_name);
       if (write(voice_fd, dletx , 2) != 2)
      lprintf(L_ERROR, "%s->%s: write error (errno 0x%x)",
           program_name, voice_modem_name, errno);

       tio_set(voice_fd, &tio_save);
       voice_command("", "VCON");
#else
       sprintf(buffer, "AT#VTS=[%d,0,%d]", data.beep.frequency,
            data.beep.length);
       /* data.beep.frequency will be ignored: sorry, only an awful dial-tone
     * availiable */
       if (voice_command(buffer, "OK") != VMA_USER_1) return(ERROR);
#endif
       return(OK);
    }

    case VOICE_DIAL: {
       int result = ERROR;

       voice_modem_state = DIALING;
       stop_dialing = FALSE;
       sprintf(buffer, "ATD %s", (char*) data.p);

       voice_write(buffer);

       while (!stop_dialing) {
       voice_read(buffer);
       result = voice_analyze(buffer, NULL);

       switch (result) {
        case VMA_BUSY:
        case VMA_FAIL:
        case VMA_ERROR:
        case VMA_NO_ANSWER:
        case VMA_NO_CARRIER:
        case VMA_NO_DIAL_TONE:
          stop_dialing = TRUE;
          result = ERROR;
          break;
        case VMA_VCON:
          stop_dialing = TRUE;
          result = OK;
          break;
       }
       }
       voice_modem_state = IDLE;
           return(result);
    }
    case VOICE_INIT:
      lprintf(L_MESG,
           "%s: initializing UMC UM92144EF based voice modem (%s)",
           program_name, UMC_RELEASE);
      voice_modem_state = INITIALIZING;

      UMC_handle_event(VOICE_MODE_ON, (event_data) 0);

#ifdef UMC_EXTENDED_DETECTION
      lprintf(L_NOISE, "%s: detect: manufacturer",
           program_name);
      voice_command("AT#MFR?", "");
      voice_flush(1);

      lprintf(L_NOISE, "%s: detect: model",
           program_name);
      voice_command("AT#MDL?", "");
      voice_flush(1);

      lprintf(L_NOISE, "%s: detect: baud rates",
           program_name);
      voice_command("AT#BDR=?", "");
      voice_flush(1);

      lprintf(L_NOISE, "%s: detect: buffer size",
           program_name);
      voice_command("AT#VBQ?", "");
      voice_flush(1);

      lprintf(L_NOISE, "%s: detect: compression method",
           program_name);
      voice_command("AT#VCI?", "");
      voice_flush(1);

      lprintf(L_NOISE, "%s: detect: devices",
           program_name);
      voice_command("AT#VLS=?", "");
      voice_flush(1);

      lprintf(L_NOISE, "%s: detect: silence detection",
           program_name);
      voice_command("AT#VSS=?", "");
      voice_flush(1);

      lprintf(L_NOISE, "%s: detect: bits per sample",
           program_name);
      voice_command("AT#VBS=?", "");
      voice_flush(1);
#endif

      sprintf(buffer, "ATS30=%1u", (cvd.rec_silence_len.d.i+9)/10);
      if (voice_command(buffer, "OK") != VMA_USER_1)
     lprintf(L_WARN, "can't set silence period");

      UMC_handle_event(VOICE_MODE_OFF, (event_data) 0);

      if (voice_command("AT\\Q3", "OK") == VMA_USER_1) {
      TIO tio;

      tio_get(voice_fd, &tio);
      tio_set_flow_control(voice_fd, &tio, FLOW_HARD);
      tio_set(voice_fd, &tio);
      }
      else lprintf(L_WARN, "can't turn on hardware flow control");

      voice_modem_state = IDLE;
      return(OK);

    case VOICE_MESSAGE_LIGHT_OFF:
      lprintf(L_NOISE, "%s: request: message light off");
      return(OK);

    case VOICE_MESSAGE_LIGHT_ON:
      lprintf(L_NOISE, "%s: request: message light on");
      return(OK);

    case VOICE_MODE_OFF:
      sprintf(buffer, "AT#CLS=%s", umc_mode_save);
      voice_command(buffer, "OK");
      return(OK);

    case VOICE_MODE_ON:
      voice_command("AT#CLS?", "");
      voice_read(umc_mode_save);
      voice_flush(1);
      voice_command("AT#CLS=8", "OK");
      return(OK);

    case VOICE_PLAY_FILE: {
       TIO tio_save;
       TIO tio;
       char input_buffer[UMC_BUFFER_SIZE];
       char output_buffer[2 * UMC_BUFFER_SIZE];
       int i;
       int bytes_in;
       int bytes_out;
       int bytes_written;

       stop_playing = FALSE;
       voice_modem_state = PLAYING;
       voice_command("AT#VTX", "CONNECT");
       tio_get(voice_fd, &tio);
       tio_save = tio;
       tio_set_flow_control(voice_fd, &tio, FLOW_HARD);
       tio_set(voice_fd, &tio);

       while (!stop_playing) {

       if ((bytes_in = read(data.i, input_buffer,
                      umc_buffer_size)) <= 0)
         break;

       bytes_out = 0;

       for(i = 0; i < bytes_in; i++) {
          output_buffer[bytes_out] = input_buffer[i];

          if (output_buffer[bytes_out++] == DLE)
            output_buffer[bytes_out++] = DLE;
       }

       lprintf(L_JUNK, "%s: <DATA %d bytes>", program_name,
            bytes_out);

       errno = 0;
       bytes_written = 0;

       while (((bytes_written
             += write(voice_fd,&output_buffer[bytes_written],
                   bytes_out - bytes_written)
             ) != bytes_out)
           && (errno == 0));

       if (bytes_written != bytes_out)
         lprintf(L_ERROR, "%s: could only write %d bytes "
              "of %d bytes (errno 0x%x)", program_name,
              bytes_written, bytes_out, errno);

       while (check_for_input(voice_fd)) {
          char modem_byte;

          if (read(voice_fd, &modem_byte, 1) != 1)
            lprintf(L_ERROR, "%s: could not read byte from"
                 " voice modem", program_name);
          else
            handle_umc_answer(modem_byte);
       }
       }

       /* <DLE><DC4> or <DLE><CAN> (immediate stop)
     * or <DLE>E (Purge Buffer)
     * seem to be unknown !
     */

       lprintf(L_JUNK, "%s->%s: <DLE><ETX>", program_name, voice_modem_name);
       if (write(voice_fd, dletx , 2) != 2)
      lprintf(L_ERROR, "%s->%s: write error (errno 0x%x)",
           program_name, voice_modem_name, errno);

       tio_set(voice_fd, &tio_save);
       voice_command("", "VCON");
       voice_modem_state = IDLE;

       if (stop_playing){
       lprintf(L_NOISE, "%s: playing voice file: interrupted",
            program_name);
       return(INTERRUPTED);
       }

       lprintf(L_NOISE, "%s: playing voice file: done",
            program_name);
       return(OK);
    }

    case VOICE_RECORD_FILE: {
       TIO tio_save;
       TIO tio;
       char input_buffer[UMC_BUFFER_SIZE];
       char output_buffer[UMC_BUFFER_SIZE];
       int i;
       int bytes_in;
       int bytes_out;
       int got_DLE_ETX = FALSE;
       int was_DLE = FALSE;

       stop_recording = FALSE;
       voice_modem_state = RECORDING;

       voice_command("AT#VRX", "CONNECT");

       tio_get(voice_fd, &tio);
       tio_save = tio;
       tio_set_flow_control(voice_fd, &tio, FLOW_HARD);
       tio.c_cc[VMIN] = (umc_buffer_size > 0xff) ? 0xff :
       umc_buffer_size;
       tio.c_cc[VTIME] = 1;
       tio_set(voice_fd, &tio);

       while (!got_DLE_ETX) {

       if ((bytes_in = read(voice_fd, input_buffer,
                      umc_buffer_size)) <= 0) {
          lprintf(L_ERROR, "%s: could not read byte from voice modem",
               program_name);
          return(FAIL);
       }

       bytes_out = 0;

       for (i = 0; (i < bytes_in) && !got_DLE_ETX; i++) {

          if (was_DLE) {
          was_DLE = FALSE;

          switch (input_buffer[i]) {
           case DLE:
             output_buffer[bytes_out++] = DLE;
             break;
           case ETX:
             got_DLE_ETX = TRUE;
             lprintf(L_JUNK, "%s: <DATA %d bytes>",
                  voice_modem_name, bytes_out);
             lprintf(L_JUNK, "%s: <DLE><ETX>",
                  voice_modem_name);
             break;
           default:
             handle_umc_answer(DLE);
             handle_umc_answer(input_buffer[i]);
          }
          } else {

          if (input_buffer[i] == DLE) {
             lprintf(L_JUNK, "%s: DLE", voice_modem_name);
             was_DLE = TRUE;
          } else
            output_buffer[bytes_out++] = input_buffer[i];
          }
       }
       write(data.i, output_buffer, bytes_out);
       }
       tio_set(voice_fd, &tio_save);
       voice_command("","VCON");

       voice_modem_state = IDLE;
       return(OK);
    }

    case VOICE_SET_COMPRESSION:
      /* Set Bits per Sample */
      switch (data.i) {
       case 0:
       case 2:
      umc_buffer_size = UMC_BUFFER_SIZE * 2 / 4;
      voice_command("AT#VBS=2", "OK");
      return(OK);
      /* my modem refuse 3bits adpcm, try out ! */
       case 4:
      umc_buffer_size = UMC_BUFFER_SIZE * 4 / 4;
      voice_command("AT#VBS=4", "OK");
      return(OK);
      }
      lprintf(L_WARN,
           "%s: handle event: Illeagal voice compression method (%d)",
           program_name, data.i);
      return(FAIL);

    case VOICE_SET_DEVICE:
      switch (data.i) {
       case NO_DEVICE:
      voice_command("AT#VLS=0", "OK");
      /* dialup_line without speaker and (surprise) without
       * handset_off_hook detection.
       */
      return(OK);

       case LOCAL_HANDSET:
      voice_command("AT#VLS=1", "VCON");
      /* handset_off/on_hook detection works (but none need it now) */
      return(OK);

       case INTERNAL_SPEAKER:
      voice_command("AT#VLS=2", "VCON");
      return(OK);

       case EXTERNAL_MICROPHONE:
      voice_command("AT#VLS=3", "VCON");
      return(OK);

       case DIALUP_LINE:
      voice_command("AT#VLS=4", "OK");
      return(OK);

      /* case SPEAKER_PHONE_MODE:
       * voice_command("AT#VLS=6", "OK");
       * return(OK);*/
      }

      lprintf(L_WARN, "%s: handle event: Unknown output device (%d)",
           program_name, data.i);
      return(FAIL);

    case VOICE_STOP_DIALING:
      lprintf(L_NOISE, "%s: request: stop dialing");
      stop_dialing = TRUE;
      return(OK);

    case VOICE_STOP_PLAYING:
      lprintf(L_NOISE, "%s: request: stop playing voice file",
           program_name);
      stop_playing = TRUE;
      return(OK);

    case VOICE_STOP_RECORDING:
      lprintf(L_NOISE, "%s: request: stop recording voice file",
           program_name);
      stop_recording = TRUE;
      voice_write("!");
      return(OK);

    case VOICE_STOP_WAITING:
      lprintf(L_NOISE, "%s: request: stop waiting",
           program_name);
      stop_waiting = TRUE;
      return(OK);

    case VOICE_SWITCH_TO_DATA_FAX:
       sprintf(buffer, "AT+FCLASS=%s", (char *) data.p);
       return(voice_command(buffer, "OK"));

    case VOICE_WAIT: {
       stop_waiting = FALSE;
       voice_modem_state = WAITING;
       alarm(data.i);

       while (!stop_waiting) {
       while (check_for_input(voice_fd)) {
          char modem_byte;

          if (read(voice_fd, &modem_byte, 1) != 1)
            lprintf(L_ERROR, "%s: could not read byte from voice modem",
              program_name);
          else
            handle_umc_answer(modem_byte);
       }

       delay(100);
       }

       voice_modem_state = IDLE;
       alarm(0);
       return(OK);
    }
   }

   lprintf(L_WARN, "%s: handle event: Unknown event %04x",
        program_name, event);
   return(FAIL);
}

#endif

voice_modem_struct UMC =
     {
     "Creatix PhoneMaster 144VFi",
     "UMC:UM92144EF",
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL,
     NULL
     };
