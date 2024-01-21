/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module X-sound_SUN.c			     */
/*									     */
/*	SUN audio functions play_sound().				     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#ifdef SOUND
#include "X-sok.h"

#ifndef AUDIO_DEVICE
#define AUDIO_DEVICE "/dev/audio"
#endif

void play_sound(const char *filename) {
    static int audio = 1;
    if (audio && checksound()) {
	char fullname[200];
	FILE *fp, *fsnd;
	int c;
	if (!(fsnd = fopen(AUDIO_DEVICE, "wb"))) {
	    audio = 0;
	    return;		/* cannot open /dev/audio */
	}
	XSync(dpy, 0);	/* text first! */
	sprintf(fullname, "%s/audio/%s.au", xsokdir, filename);
	if (!(fp = fopen(fullname, "rb"))) {
	    fclose(fsnd);
	    return;
	}
	/* yeah, copy data */
	while ((c = getc(fp)) != EOF)
	    fputc(c, fsnd);
	fclose(fsnd);
	fclose(fp);
    }
}
#endif
