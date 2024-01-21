/* "commands.c" copyright 1994 thomas insel */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>

#ifdef sun
#  include <sundev/srreg.h>
#elif defined linux
#  include <linux/cdrom.h>
#else
#  error Please fix includes for your system in commands.c
#endif

static char rcsid[] = "$Id: commands.c,v 1.3 1994/06/05 21:40:12 tinsel Exp $";

do_play(int argc, char *argv[], int cdfile) {

    struct cdrom_tochdr tochdr;
    struct cdrom_ti ti;
    struct cdrom_subchnl subchnl;

    subchnl.cdsc_format = CDROM_MSF;

    if ( argc == 1 ) {
        if ( ioctl(cdfile, CDROMSUBCHNL, &subchnl) == -1 ) {
            fprintf(stderr, "%s: ioctl cdromsubchnl\n", argv[0]);
            exit(1);
        }
    }

    if ( argc == 1 && subchnl.cdsc_audiostatus == CDROM_AUDIO_PAUSED ) {

        if ( ioctl(cdfile, CDROMRESUME) == -1 ) {
            fprintf(stderr, "%s: ioctl cdromresume\n", argv[0]);
            exit(1);
        }

    } else {

        if ( ioctl(cdfile, CDROMREADTOCHDR, &tochdr) == -1 ) {
            fprintf(stderr, "%s: ioctl cdromreadtochdr\n", argv[0]);
            exit(1);
        }

        ti.cdti_trk0 = tochdr.cdth_trk0;
        ti.cdti_ind0 = 1;
        ti.cdti_trk1 = tochdr.cdth_trk1;
        ti.cdti_ind1 = 1;

        if ( argc >= 2 ) ti.cdti_trk0 = atoi(argv[1]);
        if ( argc >= 3 ) ti.cdti_trk1 = atoi(argv[1]);
        /* check range */

        if ( ioctl(cdfile, CDROMPLAYTRKIND, &ti) == -1 ) {
            fprintf(stderr, "%s: ioctl cdromplaytrkind\n", argv[0]);
            exit(1);
        }

    }

}

do_pause(int argc, char *argv[], int cdfile) {

    if ( ioctl(cdfile, CDROMPAUSE) == -1 ) {
        fprintf(stderr, "%s: ioctl cdrompause\n", argv[0]);
        exit(1);
    }

}

do_stop(int argc, char *argv[], int cdfile) {

    if ( ioctl(cdfile, CDROMSTOP) == -1 ) {
        fprintf(stderr, "%s: ioctl cdromstop\n", argv[0]);
        exit(1);
    }

}

do_eject(int argc, char *argv[], int cdfile) {

    if ( ioctl(cdfile, CDROMEJECT) == -1 ) {
        fprintf(stderr, "%s: ioctl cdromeject\n", argv[0]);
        exit(1);
    }

}

do_volume(int argc, char *argv[], int cdfile) {
    struct cdrom_volctrl volctrl;
    int volume;

    if (argc == 2) {
    	volume = atoi(argv[1]);
    } else {
	fprintf(stderr, "Usage: %s value  (allowed values 0-255)\n", argv[0]);
	exit(1);
    }
    if ((volume<0) || (volume>255)) {
	fprintf(stderr, "%s: illegal volume value (allowed values 0-255)\n", argv[0]);
	exit(1);
    }
    volctrl.channel0=volume;
    volctrl.channel1=volume;
    volctrl.channel2=volume;
    volctrl.channel3=volume;

    if ( ioctl(cdfile, CDROMVOLCTRL, &volctrl) == -1 ) {
        fprintf(stderr, "%s: ioctl cdromvolctrl\n", argv[0]);
        exit(1);
    }
}

