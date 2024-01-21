#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>

#include "snmp.h"
#include "snmp_impl.h"
#include "asn1.h"
#include "snmp_api.h"
#include "snmp_client.h"

int snmp_dump_packet = 0;

/* fwd: */
/* from usec.c: */
extern void v2md5auth_password_to_key();


int
main(argc, argv)
    int	    argc;
    char    *argv[];
{
	u_char key[16];
	u_char agentID[12];
	int i;
	u_char *cp;

	if( argc != 3 ) {
		fprintf( stderr, "Usage: %s <password> <agentID>\n", argv[0] );
		exit (1);
	}

	if( strlen( argv[2] ) != 24 ) {
		fprintf( stderr, "bad agentID, must be 24 hex characters (12 octets)\n" );
		exit (1);
	}

	cp = (u_char *)argv[2];
	for( i = 0; i < 12; i++ ) {
		int x;
		sscanf( cp, "%2x", &x );
		agentID[i] = x & 0xff;
		cp += 2;
	}

	v2md5auth_password_to_key(argv[1], strlen(argv[1]), agentID, key);
	printf( "key for '%s' = ", argv[1] );
	for( i = 0; i < 16; i++ ) {
		printf( "%02x", key[i] );
	}
	printf( "\n" );

	return 0;
}
