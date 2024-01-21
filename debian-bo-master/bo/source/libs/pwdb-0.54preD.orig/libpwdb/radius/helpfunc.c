
/* Help functions for debugging, reporting, etc. */

#include "../_pwdb_internal.h"
#include "../_pwdb_macros.h"

/* 
 * radstr_ust = get a string of user_service_type
 */
const char * radstr_ust(u_int type) {
	switch (type) {
		case PW_LOGIN_USER:
			return "Login User";
		case PW_FRAMED_USER:
			return "Framed User";
		case PW_DIALBACK_LOGIN_USER:
			return "Dialback Login User";
		case PW_DIALBACK_FRAMED_USER:
			return "Dialback Framed User";
		default:
			return "Unknown type";
	}
	/* Not reached */
	return NULL;
}

/*
 * radstr_fp = get a string of framed_protocol
 */
const char * radstr_fp(u_int type) {
	switch(type) {
		case PW_PPP:
			return "PPP";
		case PW_SLIP:
			return "SLIP";
		default:
			return "Unknown";
	}
	/* Not reached */
	return NULL;
}

/*
 * radstr_fr = get a string of framed_routing
 */
const char * radstr_fr(u_int type) {
	switch(type) {
		case PW_NONE:
			return "None";
		case PW_BROADCAST:
			return "Broadcast";
		case PW_LISTEN:
			return "Listen";
		case PW_BROADCAST_LISTEN:
			return "Broadcast-Listen";
		default:
			return "Unknown";
	}
	/* Not reached */
	return NULL;
}

/*
 * radstr_ls = get a string of login_service
 */
const char * radstr_ls(u_int type) {
	switch(type) {
		case PW_TELNET:
			return "Telnet";
		case PW_RLOGIN:
			return "Rlogin";
		case PW_TCP_CLEAR:
			return "TCP Clear";
		case PW_PORTMASTER:
			return "PortMaster";
		default:
			return "Unknown";
	}
	/* Not reached */
	return NULL;
}

/*
 * radstr_ast = get a string of acct_status_type
 */
const char * radstr_ast(u_int type) {
	switch(type) {
		case PW_STATUS_START:
			return "Start";
		case PW_STATUS_STOP:
			return "Stop";
		default:
			return "Unknown";
	}
	/* Not reached */
	return NULL;
}

/*
 * radstr_aa = get a string of acct_authentic
 */
const char * radstr_aa(u_int type) {
	switch(type) {
		case PW_AUTH_NONE:
			return "None";
		case PW_AUTH_RADIUS:
			return "Radius";
		case PW_AUTH_LOCAL:
			return "Local";
		default:
			return "Unknown";
	}
	/* Not reached */
	return NULL;
}

int get_server_entries (char *hostname, char *secret) {
    char buffer[PATH_MAX];
    FILE *fserver;

    D(("called"));
    memset(buffer, 0, PATH_MAX);
    sprintf (buffer, "%s/%s", RADIUS_DIR, RADIUS_SRV);
    if ((fserver = fopen (buffer, "r")) == (FILE*)NULL) {
	D(("failed looking for %s",buffer));
	return PWDB_RADIUS_CONF_ERR;
    }

    while (fgets (buffer, sizeof(buffer), fserver) != (char*) NULL) {
	char *ptr;

	ptr = buffer;
	while (isspace(*ptr)) { 
	    ptr++;
	};
	if (*ptr == '#')
	    continue;
	/* Just look for one server now */
	if (sscanf (ptr, "%s%s", hostname, secret) != 2)
	    continue; /* invalid line */
	else
	    return PWDB_RADIUS_SUCCESS;
    }
    return PWDB_RADIUS_CONF_ERR;
}

