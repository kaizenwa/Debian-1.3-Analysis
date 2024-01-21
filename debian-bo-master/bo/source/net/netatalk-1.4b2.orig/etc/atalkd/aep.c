/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved. See COPYRIGHT.
 */

#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netatalk/at.h>
#include <atalk/aep.h>
#include <atalk/ddp.h>

#include "atserv.h"

aep_packet( ap, from, data, len )
    struct atport	*ap;
    struct sockaddr_at	*from;
    char		*data;
    int			len;
{
    char		*end;

    end = data + len;
    if ( data + 2 > end || *data != DDPTYPE_AEP ||
	    *( data + 1 ) != AEPOP_REQUEST ) {
	syslog( LOG_INFO, "aep_packet malformed packet" );
	return;
    }

    *( data + 1 ) = AEPOP_REPLY;
    if ( sendto( ap->ap_fd, data, len, 0, (struct sockaddr *)from,
	    sizeof( struct sockaddr_at )) < 0 ) {
	syslog( LOG_ERR, "aep sendto: %m" );
	return;
    }
    return;
}
