/*
 * Programm XBLAST V2.2 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * December 9th 1995
 * started August 1993
 *
 * File: sound.h 
 * Sound generation, header file
 *
 * Author: Norbert Nicolay (e-mail: nicolay@ikp.uni-koeln.de)
 *
 * $Id: sound.h,v 1.8 1996/09/06 22:17:50 norbert Exp $
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Log: sound.h,v $
 * Revision 1.8  1996/09/06 22:17:50  norbert
 * New function unload_sound to free sample memory. The argument of
 * the function stop_sound now is evaluated.
 *
 * Revision 1.7  1996/09/06 21:41:03  norbert
 * Several new sounds added. Shrinking sounds now working.
 *
 * Revision 1.6  1996/08/13 21:06:48  norbert
 * Now server is execed as a seperate process. Thus it is possible to
 * write servers for other machines.
 *
 * Revision 1.5  1996/08/13 20:46:25  norbert
 * New sliding bomb sound introduced. Shrinking wall sounds still not
 * implemented.
 * -
 *
 * Revision 1.5  1996/08/12 21:34:21  norbert
 * Some RCS cosmetics.
 *
 * Revision 1.4  1996/08/12 21:29:43  norbert
 * This version now supports mono samples which can be positioned in the
 * stereo panorama with an additional argument to the client's play_sound()
 * function. Stereo channel flipping now fixed with dsp_syncs where possible.
 *
 * Revision 1.3  1996/08/02 13:36:33  norbert
 * Incorrect termination of sound server on ioctl errors fixed. Now some
 * ioctl errors are simply ignored, on other ones the server now terminates
 * correctly without disturbing the parent.
 *
 * Revision 1.2  1996/08/01 19:14:22  norbert
 * Statistics implemented, repeating of songs ready. TODO: shrinking still
 * incomplete.
 *
 * Revision 1.1  1996/08/01 08:32:21  norbert
 * Initial revision
 *
 *
 */

/*
 * SOUND_DIR: may be redefined by Makefile!
 */
#if !defined(SOUND_DIR)
#define SOUND_DIR "./sounds"
#endif

#if defined(_SOUND_C)
#define EXTERN 
#else
#define EXTERN extern
#endif

/*
 * sample ids
 */
#define SND_LAST      -1

enum sound_id {
  SND_BAD,
  SND_DROP,
  SND_NEWBOMB,
  SND_NEWKICK,
  SND_NEWPUMP,
  SND_NEWRC,
  SND_MOREFIRE,
  SND_DEAD,
  SND_EXPL,
  SND_KICK,
  SND_PUMP,
  SND_OUCH,
  SND_INTRO,
  SND_APPL,
  SND_BUTT,
  SND_SHOOT,
  SND_INVIS,
  SND_INVINC,
  SND_NEWTELE,
  SND_TELE,
  SND_INJ,
  SND_MINIBOMB,
  SND_WON,
  SND_HAUNT,
  SND_SPIRAL,
  SND_SPBOMB,
  SND_SLIDE,
  SND_FINALE,
  SND_WARN,
  SND_STUN,
  SND_WHIRL,
  SND_COMPOUND,
  SND_SNG1,
  SND_SNG2,
  SND_SNG3,
  SND_SNG4,
  SND_SNG5,
  SND_SNG6
};

/* Sound positions in the stereo panorama range from 0 (most left) to 
 * 16 (most right) with 8 as middle position.
 */
#define MAX_SOUND_POSITION     15
#define SOUND_MIDDLE_POSITION  7

/* client/server commands */
#define SND_LOAD_SOUND      0
#define SND_PLAY_SOUND      1
#define SND_STOP_SOUND      2
#define SND_UNLOAD_SOUND    3

/* arguments to the commands */
#define STOP_ALL_SOUNDS     0

/* sound server acknowledge */
#define SND_ACK_ERROR 1
#define SND_ACK_OK    0

/* Sound server name */
#define XBLAST_SOUND_SERVER "xbsndsrv"

/* function protoypes */

#if defined(__STDC__)
EXTERN int  init_sound(char *mode);
EXTERN int  stop_sound(int id);
EXTERN int  play_sound(int id, int position);
EXTERN int  load_sound(int id);
EXTERN int  unload_sound(int id);
EXTERN void flush_audio(void);
EXTERN int  stop_sound_server(void);
#else
EXTERN int  init_sound();
EXTERN int  stop_sound();
EXTERN int  play_sound();
EXTERN int  load_sound();
EXTERN int  unload_sound();
EXTERN void flush_audio();
EXTERN int  stop_sound_server();
#endif











