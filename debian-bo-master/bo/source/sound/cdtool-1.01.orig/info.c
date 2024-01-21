/* "info.c" copyright 1994 thomas insel */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>

#ifdef sun
#  include <sundev/srreg.h>
#elif defined linux
#  include <linux/cdrom.h>
#else
#  error Please fix includes for your system in info.c
#endif

#include "database.h"
#include "hardware.h"

#define P_QUICK 1
#define P_LONG  2
#define P_TEMPL 3

static char rcsid[] = "$Id: info.c,v 1.4 1994/06/05 22:37:22 tinsel Exp $";

char *
tracks_line(cdhw_t *hw) {

    int i;
    char *tempstr = (char *)malloc(20 + 8*hw->tochdr.cdth_trk1);

    sprintf(tempstr, "tracks %u ", hw->tochdr.cdth_trk1);
    for (i=hw->tochdr.cdth_trk0; i<=hw->tochdr.cdth_trk1; i++)
        sprintf(tempstr + strlen(tempstr), "%u ",
                hw->tocentries[i-1].cdte_addr.msf.minute*4500 +
                hw->tocentries[i-1].cdte_addr.msf.second*75 +
                hw->tocentries[i-1].cdte_addr.msf.frame );
    sprintf(tempstr + strlen(tempstr), "%u\n", 
           hw->tocentries[hw->tochdr.cdth_trk1].cdte_addr.msf.minute*60 +
           hw->tocentries[hw->tochdr.cdth_trk1].cdte_addr.msf.second);

   return tempstr;

}

do_info(int argc, char *argv[], int cdfile) {

    cdhw_t *hw;
    char *tracks_buffer;
    cd_t *cd;

    int i;  /* generic index variable */

    int usedb = 1;
    char p_format = P_LONG;
    int theoption;

    while ( (theoption=getopt(argc, argv, "stn")) != -1) {
        switch (theoption) {
            case 's':
                p_format = P_QUICK; break;
            case 't':
                p_format = P_TEMPL; break;
            case 'n':
                usedb = 0; break;
         }
    }

    hw = read_hw(argv[0], cdfile);
    tracks_buffer = tracks_line(hw);
    cd = read_db(tracks_buffer, usedb);

    /* print name & artist info */
    if (p_format == P_TEMPL) {
        for (i=0; i<strlen(tracks_buffer); i++) /* print "tracks" line */
            putchar(tracks_buffer[i]);

        if (cd->cdname[0])
            printf("cdname %s", cd->cdname);
        else
            printf("cdname \n");

        if (cd->artist[0])
            printf("artist %s", cd->artist);
        else
            printf("artist \n");

    } else {
        if (cd->artist[0])
            for (i=0; i<strlen(cd->artist)-1; i++) 
                putchar(cd->artist[i]);
        if (cd->cdname[0] && cd->artist[0])
            printf(" - ");
        if (cd->cdname[0])
            for (i=0; i<strlen(cd->cdname)-1; i++) 
                putchar(cd->cdname[i]);
        if (!cd->artist[0] && !cd->cdname[0])
            printf("unknown cd");
        if (p_format == P_LONG)
            printf(" - %2u:%02u in %u tracks\n",
                    hw->tocentries[hw->tochdr.cdth_trk1].cdte_addr.msf.minute,
                    hw->tocentries[hw->tochdr.cdth_trk1].cdte_addr.msf.second,
                    hw->tochdr.cdth_trk1);
    }

    /* print tracks */
    switch(p_format) {

        case P_LONG:
            for (i = hw->tochdr.cdth_trk0; i<=hw->tochdr.cdth_trk1; i++) {
                int delsecs = hw->tocentries[i].cdte_addr.msf.minute * 60
                      + hw->tocentries[i].cdte_addr.msf.second
                      - hw->tocentries[i-1].cdte_addr.msf.minute * 60
                      - hw->tocentries[i-1].cdte_addr.msf.second;
                printf(" %2u:%02u %2u ", delsecs / 60, delsecs - (delsecs/60)*60, i);
                if (hw->tocentries[i-1].cdte_ctrl == CDROM_DATA_TRACK)
                    printf("[DATA] ");
                if (i == hw->subchnl.cdsc_trk) {
                    if (hw->subchnl.cdsc_audiostatus == CDROM_AUDIO_PLAY)
                        printf("[PLAYING] ");
                    else if (hw->subchnl.cdsc_audiostatus == CDROM_AUDIO_PAUSED)
                        printf("[PAUSED] ");
                }
                if (usedb && cd->track_names[i-1][0] != 0)
                    printf("%s",cd->track_names[i-1]);
                else printf("\n");
            }
            break; /* P_LONG */

        case P_QUICK:
            switch(hw->subchnl.cdsc_audiostatus) {
                case CDROM_AUDIO_PLAY: 
                    printf(" - ");
                    if (cd->track_names[hw->subchnl.cdsc_trk-1][0]) {
                        for (i=0; i<strlen(cd->track_names[hw->subchnl.cdsc_trk - 1])-1; i++)
                            putchar(cd->track_names[hw->subchnl.cdsc_trk-1][i]);
                        printf(" ");
                    }
                    printf("[%u]\n", hw->subchnl.cdsc_trk);
                    break;
                case CDROM_AUDIO_PAUSED:
                    printf(" - paused on ");
                    if (cd->track_names[hw->subchnl.cdsc_trk-1][0]) {
                        for (i=0; i<strlen(cd->track_names[hw->subchnl.cdsc_trk - 1])-1; i++)
                            putchar(cd->track_names[hw->subchnl.cdsc_trk-1][i]);
                        printf(" ");
                    }
                    printf("[%u]\n", hw->subchnl.cdsc_trk);
                    break;
                default:
                    printf("\n");
            }
            break; /* P_QUICK */

        case P_TEMPL:
            for (i = hw->tochdr.cdth_trk0; i<=hw->tochdr.cdth_trk1; i++)
                if (usedb && cd->track_names[i-1][0] != 0)
                    printf("track %s", cd->track_names[i-1]);
                else printf("track \n");
            break; /* P_TEMPL */

    } /* switch */

}
