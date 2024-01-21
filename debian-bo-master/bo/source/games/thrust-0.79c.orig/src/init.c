
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <math.h>

#include "keyboard.h"
#include "thrust_types.h"
#include "init.h"
#include "fast_gr.h"
#include "graphics.h"
#include "things.h"
#include "font5x5.h"
#include "thrust.h"
#include "gr_drv.h"

#ifdef HAVE_SOUND
#include "soundIt.h"
#define NUM_SAMPLES 5
#if defined(HAVE_LINUX_SOUND)
#define SAMPLE_RATE 11000
#elif defined(HAVE_SUN_SOUND) || defined(HAVE_HP_SOUND)
#define SAMPLE_RATE 11025
#endif
#define NUM_CHANNELS 4
#define CHAN_1 0
#define CHAN_2 1
#define CHAN_3 2
#define CHAN_4 3
#define SND_BOOM   0
#define SND_BOOM2  1
#define SND_HARP   2
#define SND_THRUST 3
#define SND_ZERO   4
Sample snd[NUM_SAMPLES];
#endif

void
turnship(void)
{
  word i,j,k;

  for(k=0; k<4; k++)
    for(i=0; i<16; i++)
      for(j=0; j<16; j++)
	*(ship+(5+k)*256 +     i*16+j)=
	  *(ship+(3-k)*256 +(15-j)*16+15-i);
  for(i=0; i<4; i++) {
    memcpy(shipstorage,ship+(i<<8),256);
    memcpy(ship+(i<<8),ship+((8-i)<<8),256);
    memcpy(ship+((8-i)<<8),shipstorage,256);
  }
  for(k=0; k<8; k++)
    for(i=0; i<16; i++)
      for(j=0; j<16; j++)
	*(ship+(9+k)*256 +     i*16+j)=
	  *(ship+(7-k)*256 +     i*16+15-j);
  for(k=0; k<15; k++)
    for(i=0; i<16; i++)
      for(j=0; j<16; j++)
	*(ship+(17+k)*256+     i*16+j)=
	  *(ship+(15-k)*256+(15-i)*16+j);
}

void
makeshieldedship(void)
{
  word i,j,k;

  memcpy(shieldship, ship, 8192);
  for(i=0; i<32; i++)
    for(j=0; j<16; j++)
      for(k=0; k<16; k++)
	if(*(bin_shield+(j<<4)+k))
	  *(shieldship+(i<<8)+(j<<4)+k)=*(bin_shield+(j<<4)+k);
}

void
makefuelmap(byte *fuelmap)
{
  int i;

  memset(fuelmap, 0, 64*4);

  for(i=0; i<32; i++) {
    *(fuelmap+4*(i+32)+(i>6)+(i>16)+(i>26)) = 255;
    *(fuelmap+4*i+(i<27)+(i<17)+(i<7)) = 255;
  }
}

int
initmem(void)
{
  int i;

  printf("Allocating memory...");

  bild=(byte *)malloc((long)PBILDX*PBILDY*2+16);
  bana=(byte *)malloc(maxlenx*maxleny);
  ship=(byte *)malloc(8192);
  shieldship=(byte *)malloc(8192);
  shipstorage=(byte *)malloc(256);
  bulletmap=(byte *)malloc(256);
  bulletstorage=(byte *)malloc(maxbullets*16);
  fragmentstorage=(byte *)malloc(maxfragments*4);
  fuelmap=(byte *)malloc(256);
  fuelstorage=(byte *)malloc(256);
  loadmap=(byte *)malloc(64);
  loadstorage=(byte *)malloc(64);
  wirestorage=(byte *)malloc(64);

  if(!bild || !bana || !ship || !shieldship || !shipstorage
     || !bulletmap || !bulletstorage || !fragmentstorage
     || !fuelmap || !fuelstorage || !loadmap || !loadstorage
     || !wirestorage) {
    printf("failed!.\n");
    return(0);
  }
  printf("done.\n");

  blocks=bin_blocks;
  memcpy(ship, bin_ship, 256*5);
  for(i=0; i<16; i++)
    memcpy(bulletmap+((20-i)&15)*16, bin_bullets+i*16, 16);

  for(i=0; i<title_cols*title_rows; i++)
    *(title_pixels+i) += 192;

  memcpy(bin_colors+192*3, title_colors, title_nr_colors*3);

  for(i=0; i<3*256; i++)
    bin_colors[i]=GAMMA(bin_colors[i]);

  printf("Turning the ship...");
  turnship();
  printf("done.\n");
  printf("Building graphics...");
  makefuelmap(fuelmap);
  memcpy(loadmap, blocks+64*109, 64);
  makeshieldedship();
  printf("done.\n");

  return(1);
}

void
inithardware(int argc, char **argv)
{
#ifdef HAVE_SOUND
  if(play_sound)
    if(initsoundIt())
      play_sound=0;
    else
      play_sound=1;
#endif

  if(graphicsinit(argc, argv))
    exit(-1);

  if(keyinit()) {
    printf("Could not initialize the keyboard.\n");
    exit(-1);
  }
  printf("Keyboard initialized.\n");
}

void
initscreen(int round)
{
  int i,j;

  if(round&2) {
    bin_colors[65*3+0]=0;
    bin_colors[65*3+1]=0;
    bin_colors[65*3+2]=0;
  }
  else {
    bin_colors[65*3+0]=GAMMA(colorr);
    bin_colors[65*3+1]=GAMMA(colorg);
    bin_colors[65*3+2]=GAMMA(colorb);
  }

  for(j=pblocky; j<BBILDY+pblocky; j++)
    for(i=pblockx; i<BBILDX+pblockx; i++)
      writeblock(i%lenx, j, *(bana+i%lenx+j*lenx));
}

void
initgame(int round, int reset, int xblock, int yblock)
{
  int i;

  crash=0;
  shoot=0;
#ifdef DEBUG
  repetetive=1;
#else
  repetetive=0;
#endif
  refueling=0;
  speedx=0;
  speedy=0;
  absspeed=0L;
  oldabs=0L;
  vx=0;
  vy=0;
  if(round&1) {
    kdir=72;
    dir=24;
    gravity=-20;
    alpha=3*M_PI/2;
    deltaalpha=0;
  }
  else {
    kdir=24;
    dir=8;
    gravity=20;
    alpha=M_PI/2;
    deltaalpha=0;
  }
  if(reset) {
    loaded=0;
    loadcontact=0;
    loadpoint=0;
    loadpointshift=0;
    shipdx=0;
    shipdy=0;
  }
  else {
    loadcontact=0;
    if(loaded) {
      loadpoint=126;
      loadpointshift=0;
      shipdx=(int)( cos(alpha)*loadpoint/5.90625);
      shipdy=(int)(-sin(alpha)*loadpoint/5.90625);
    }
    else {
      loadpoint=0;
      loadpointshift=0;
      *(bana+lenx*loadby+loadbx)=109;
      shipdx=0;
      shipdy=0;
    }
  }
  
  pblockx=xblock;
  pblocky=yblock+4*(round&1);
  if(loaded) {
    if(round&1)
      pblocky-=2;
    else
      pblocky+=2;
  }

  pixx=pblockx<<3;
  pixy=pblocky<<3;
  x=pixx<<3;
  y=pixy<<3;
  bildx=(pixx+PBILDX-4)%PBILDX+4;
  bildy=pixy%PBILDY;
  bblockx=bildx>>3;
  bblocky=bildy>>3;

  countdown=0;

  for(i=0; i<maxbullets; i++)
    bullets[i].life=0;
  for(i=0; i<maxfragments; i++)
    fragments[i].life=0;

  chcolor=TEXTCOLOR;
  chpaper=0;
  chflag=0;
}

#ifdef HAVE_SOUND
int
initsoundIt(void)
{
  printf("Initializing soundIt library v" SOUNDIT_VERS "...");
  fflush(stdout);

  snd[0].data = sound_boom;
  snd[0].len  = sound_boom_len;
  snd[0].loop = 0;
  snd[1].data = sound_boom2;
  snd[1].len  = sound_boom2_len;
  snd[1].loop = 0;
  snd[2].data = sound_harp;
  snd[2].len  = sound_harp_len;
  snd[2].loop = 0;
  snd[3].data = sound_thrust;
  snd[3].len  = sound_thrust_len;
  snd[3].loop = 1;
  snd[4].data = sound_zero;
  snd[4].len  = sound_zero_len;
  snd[4].loop = 0;

#if defined(HAVE_LINUX_SOUND)
  if(Snd_init(NUM_SAMPLES, snd, SAMPLE_RATE, NUM_CHANNELS, "/dev/dsp")
#elif defined(HAVE_SUN_SOUND) || defined(HAVE_HP_SOUND)
  if(Snd_init(NUM_SAMPLES, snd, SAMPLE_RATE, NUM_CHANNELS, "/dev/audio")
#endif
     == EXIT_FAILURE) {
    printf("No sound.\n");
    return(-1);
  }

  printf("done.\n");
  return(0);
}
                                                            
void
restoresoundIt(void)
{
  printf("Restoring sound...");
  Snd_restore();
  printf("done.\n");
}
#endif
         
void
restorehardware(void)
{
  printf("Releasing keyboard...");
  keyclose();
  printf("done.\n");

  graphicsclose();

#ifdef HAVE_SOUND
  if(play_sound)
    restoresoundIt();
#endif
}

void
restoremem(void)
{
  printf("Freeing allocated memory...");
  free(bild);
  free(bana);
  free(bulletmap);
  free(ship);
  free(shieldship);
  free(shipstorage);
  free(fragmentstorage);
  free(wirestorage);
  free(fuelmap);
  free(fuelstorage);
  free(loadmap);
  free(loadstorage);
  printf("done.\n");
}
