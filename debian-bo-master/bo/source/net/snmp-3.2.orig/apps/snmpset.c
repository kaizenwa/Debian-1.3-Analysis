/*
 * snmpset.c - send snmp SET requests to a network entity.
 *
 */
/***********************************************************************
	Copyright 1988, 1989, 1991, 1992 by Carnegie Mellon University

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of CMU not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

CMU DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
CMU BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/
#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/time.h>
#include <netdb.h>
#ifdef linux
# include <stdlib.h>
# include <string.h>
# include <unistd.h>
# include <arpa/inet.h>
#endif

#include "snmp.h"
#include "asn1.h"
#include "mib.h"
#include "snmp_impl.h"
#include "snmp_api.h"
#include "snmp_client.h"

extern void snmp_add_var ();

extern int  errno;
int snmp_dump_packet = 0;

static int ascii_to_binary ();
static int hex_to_binary ();


void
usage()
{
    fprintf(stderr, "Usage: snmpset [options] hostname community [objectID type value] ...\n");
    fprintf(stderr, "\twhere boptions is one of:\n");
    fprintf(stderr, "\t\t -p port      -- set port to sent qury to\n");
    fprintf(stderr, "\t\t -t timeout   -- timeout for every retry to use\n");
    fprintf(stderr, "\t\t -r retries   -- retries to send\n");
    fprintf(stderr, "\t\t -d p         -- dump packet send\n");
    fprintf(stderr, "\twhere type is one of: i, s, x, d, n, o, t, a\n");
    fprintf(stderr, "\t\ti: INTEGER, s: STRING, x: HEX STRING, d: DECIMAL STRING\n");
    fprintf(stderr, "\t\tn: NULLOBJ, o: OBJID, t: TIMETICKS, a: IPADDRESS\n");

    exit(1);
}


int
main(argc, argv)
    int	    argc;
    char    *argv[];
{
    struct snmp_session session, *ss;
    struct snmp_pdu *pdu, *response;
    struct variable_list *vars;
    int	arg;
    char *hostname = NULL;
    char *community = NULL;
    int timeout_flag = 0, timeout = 0, retransmission_flag = 0;
    int retransmission = 0;			/* YYY: check init */
    int	count, current_name = 0, current_type = 0, current_value = 0;
    char *names[128], types[128], *values[128];
    oid name[MAX_NAME_LEN];
    int name_length;
    int status;
    int port_flag = 0;
    int dest_port = 0;
    u_long srcclock = 0, dstclock = 0;		/* YYY: check init */
    int clock_flag = 0;
    int srclen = 0, dstlen = 0, contextlen = 0;
    int trivialSNMPv2 = FALSE;
    struct hostent *hp;
    u_long destAddr;



    init_mib();
    for(arg = 1; arg < argc; arg++){
	if (argv[arg][0] == '-'){
	    switch(argv[arg][1]){
		case 'd':
		    snmp_dump_packet++;
		    break;
                case 'p':
                    port_flag++;
                    dest_port = atoi(argv[++arg]);
                    break;
                case 't':
                    timeout_flag++;
                    timeout = atoi(argv[++arg]) * 1000000L;
                    break;
                case 'r':
                    retransmission_flag++;
                    retransmission = atoi(argv[++arg]);
                    break;
                case 'c':
                    clock_flag++;
                    srcclock = atoi(argv[++arg]);
                    dstclock = atoi(argv[++arg]);
                    break;
		default:
		    printf("invalid option: -%c\n", argv[arg][1]);
		    usage();
		    break;
	    }
	    continue;
	}
	if (hostname == NULL){
	    hostname = argv[arg];
        } else if (community == NULL){
            community = argv[arg];
	} else {
	    names[current_name++] = argv[arg++];
	    if (arg < argc)
		switch(*argv[arg]){
		    case 'i':
	            case 's':
	            case 'x':
	            case 'd':
	            case 'n':
	            case 'o':
	            case 't':
	            case 'a':
		        types[current_type++] = *argv[arg++];
			break;
		    default:
			printf("Bad object type: %c\n", *argv[arg]);
			usage();
			exit(1);
		}
	    if (arg < argc)
	        values[current_value++] = argv[arg];
	}
    }

    if (!hostname || current_name <= 0 
	|| current_name != current_type	|| current_type != current_value
	|| !community) {
	usage();
    }

    if (trivialSNMPv2){
	if ((destAddr = inet_addr(hostname)) == -1){
	    hp = gethostbyname(hostname);
	    if (hp == NULL){
		fprintf(stderr, "unknown host: %s\n", hostname);
		exit(1);
	    } else {
		bcopy((char *)hp->h_addr, (char *)&destAddr,
		      hp->h_length);
	    }
	}
	srclen = dstlen = contextlen = MAX_NAME_LEN;
    }

    bzero((char *)&session, sizeof(struct snmp_session));

    session.peername = hostname;
    if (port_flag)
        session.remote_port = dest_port;

    session.community = (u_char *)community;
    session.community_len = strlen((char *)community);

    if (retransmission_flag)
        session.retries = retransmission;
    else
        session.retries = SNMP_DEFAULT_RETRIES;

    if (timeout_flag)
        session.timeout = timeout;
    else
        session.timeout = SNMP_DEFAULT_TIMEOUT;

    session.authenticator = NULL;
    snmp_synch_setup(&session);

    ss = snmp_open(&session);
    if (ss == NULL){
	printf("Couldn't open snmp\n");
	exit(-1);
    }

    pdu = snmp_pdu_create(SET_REQ_MSG);

    for(count = 0; count < current_name; count++){
	name_length = MAX_NAME_LEN;
	if (!read_objid(names[count], name, &name_length)){
	    printf("Invalid object identifier: %s\n", names[count]);
	}
	
	snmp_add_var(pdu, name, name_length, types[count], values[count]);
    }

retry:
    status = snmp_synch_response(ss, pdu, &response);
    if (status == STAT_SUCCESS){
	if (response->errstat == SNMP_ERR_NOERROR){

	    if( response->command == REPORT_MSG )
		printf( "This is an error report.\n" );
	    for(vars = response->variables; vars; vars = vars->next_variable)
		print_variable(vars->name, vars->name_length, vars);

	    for(vars = response->variables; vars; vars = vars->next_variable)
		print_variable(vars->name, vars->name_length, vars);
	} else {
	    printf("Error in packet.\nReason: %s\n", snmp_errstring(response->errstat));
	    if (response->errstat == SNMP_ERR_NOSUCHNAME){
		printf("This name doesn't exist: ");
		for(count = 1, vars = response->variables; vars && count != response->errindex;
		    vars = vars->next_variable, count++)
			;
		if (vars)
		    print_objid(vars->name, vars->name_length);
		printf("\n");
	    }
	    if ((pdu = snmp_fix_pdu(response, SET_REQ_MSG)) != NULL)
		goto retry;
	}

    } else if (status == STAT_TIMEOUT){
	printf("No Response from %s\n", hostname);
    } else {    /* status == STAT_ERROR */
	printf("An error occurred, Quitting\n");
    }

    if (response)
	snmp_free_pdu(response);
    snmp_close(ss);

    return 0;
}

/*
 * Add a variable with the requested name to the end of the list of
 * variables for this pdu.
 */
void
snmp_add_var(pdu, name, name_length, type, value)
    struct snmp_pdu *pdu;
    oid *name;
    int name_length;
    char type, *value;
{
    struct variable_list *vars;
    char buf[2048];

    if (pdu->variables == NULL){
	pdu->variables = vars =
	    (struct variable_list *)malloc(sizeof(struct variable_list));
    } else {
	for(vars = pdu->variables;
	    vars->next_variable;
	    vars = vars->next_variable)
	    /*EXIT*/;
	vars->next_variable =
	    (struct variable_list *)malloc(sizeof(struct variable_list));
	vars = vars->next_variable;
    }

    vars->next_variable = NULL;
    vars->name = (oid *)malloc(name_length * sizeof(oid));
#ifdef SVR4
    memmove((char *)vars->name, (char *)name, name_length * sizeof(oid));
#else
    bcopy((char *)name, (char *)vars->name, name_length * sizeof(oid));
#endif
    vars->name_length = name_length;

    switch(type){
	case 'i':
	    vars->type = INTEGER;
	    vars->val.integer = (long *)malloc(sizeof(long));
	    *(vars->val.integer) = atoi(value);
	    vars->val_len = sizeof(long);
	    break;
	case 's':
	case 'x':
	case 'd':
	    vars->type = STRING;
	    if (type == 'd'){
		vars->val_len = ascii_to_binary((u_char *)value, buf);
	    } else if (type == 's'){
		strcpy(buf, value);
		vars->val_len = strlen(buf);
	    } else if (type == 'x'){
		vars->val_len = hex_to_binary((u_char *)value, buf);
	    }
	    vars->val.string = (u_char *)malloc(vars->val_len);
#ifdef SVR4
	    memmove((char *)vars->val.string, (char *)buf, vars->val_len);
#else
	    bcopy((char *)buf, (char *)vars->val.string, vars->val_len);
#endif
	    break;
	case 'n':
	    vars->type = NULLOBJ;
	    vars->val_len = 0;
	    vars->val.string = NULL;
	    break;
	case 'o':
	    vars->type = OBJID;
	    vars->val_len = MAX_NAME_LEN;
	    read_objid(value, (oid *)buf, &vars->val_len);
	    vars->val_len *= sizeof(oid);
	    vars->val.objid = (oid *)malloc(vars->val_len);
#ifdef SVR4
	    memmove((char *)vars->val.objid, (char *)buf, vars->val_len);
#else
	    bcopy((char *)buf, (char *)vars->val.objid, vars->val_len);
#endif
	    break;
	case 't':
	    vars->type = TIMETICKS;
	    vars->val.integer = (long *)malloc(sizeof(long));
	    *(vars->val.integer) = atoi(value);
	    vars->val_len = sizeof(long);
	    break;
	case 'a':
	    vars->type = IPADDRESS;
	    vars->val.integer = (long *)malloc(sizeof(long));
	    *(vars->val.integer) = inet_addr(value);
	    vars->val_len = sizeof(long);
	    break;
	default:
	    printf("Internal error in type switching\n");
	    exit(-1);
    }
}

static int
ascii_to_binary(cp, bufp)
    u_char  *cp;
    u_char *bufp;
{
    int	subidentifier;
    u_char *bp = bufp;

    for(; *cp != '\0'; cp++){
	if (isspace(*cp))
	    continue;
	if (!isdigit(*cp)){
	    fprintf(stderr, "Input error\n");
	    return -1;
	}
	subidentifier = atoi(cp);
	if (subidentifier > 255){
	    fprintf(stderr, "subidentifier %d is too large ( > 255)\n",
		    subidentifier);
	    return -1;
	}
	*bp++ = (u_char)subidentifier;
	while(isdigit(*cp))
	    cp++;
	cp--;
    }
    return bp - bufp;
}

static int
hex_to_binary(cp, bufp)
    u_char  *cp;
    u_char *bufp;
{
    int	subidentifier;
    u_char *bp = bufp;

    for(; *cp != '\0'; cp++){
	if (isspace(*cp))
	    continue;
	if (!isxdigit(*cp)){
	    fprintf(stderr, "Input error\n");
	    return -1;
	}
	sscanf((char *)cp, "%x", &subidentifier);
	if (subidentifier > 255){
	    fprintf(stderr, "subidentifier %d is too large ( > 255)\n",
		    subidentifier);
	    return -1;
	}
	*bp++ = (u_char)subidentifier;
	while(isxdigit(*cp))
	    cp++;
	cp--;
    }
    return bp - bufp;
}
