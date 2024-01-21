/* "hardware.c" copyright 1994 thomas insel */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pwd.h>
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
#  errror Please fix includes for your system in hardware.c
#endif

#include "database.h"
#include "hardware.h"

static char rcsid[] = "$Id: hardware.c,v 1.3 1994/06/05 21:40:12 tinsel Exp $";

cdhw_t *
read_hw(char *progname, int cdfile) {

    int i;
    cdhw_t *hw = malloc(sizeof(cdhw_t));

    /* read header */
    if ( ioctl(cdfile, CDROMREADTOCHDR, &(hw->tochdr)) == -1 ) {
        fprintf(stderr, "%s: ioctl cdromreadtochdr\n", progname);
        exit(1);
    }

    /* read individual tracks */
    for (i=hw->tochdr.cdth_trk0; i<=hw->tochdr.cdth_trk1; i++) {
        hw->tocentries[i-1].cdte_track = i;
        hw->tocentries[i-1].cdte_format = CDROM_MSF;
        if ( ioctl(cdfile, CDROMREADTOCENTRY, &(hw->tocentries[i-1])) == -1 ) {
            fprintf(stderr, "%s: ioctl cdromreadtocentry\n", progname);
            exit(1);
        }
    }

    /* read the lead-out track */
    hw->tocentries[hw->tochdr.cdth_trk1].cdte_track = CDROM_LEADOUT;
    hw->tocentries[hw->tochdr.cdth_trk1].cdte_format = CDROM_MSF;
    if ( ioctl(cdfile, CDROMREADTOCENTRY, &(hw->tocentries[hw->tochdr.cdth_trk1])) == -1 ) {
        fprintf(stderr, "%s: ioctl cdromreadtocentry\n", progname);
        exit(1);
    }

    /* read subchannel info */
    hw->subchnl.cdsc_format = CDROM_MSF;
    if ( ioctl(cdfile, CDROMSUBCHNL, &(hw->subchnl)) == -1 ) {
        fprintf(stderr, "%s: ioctl cdromsubchnl\n", progname);
        exit(1);
    }

    return hw;

}
