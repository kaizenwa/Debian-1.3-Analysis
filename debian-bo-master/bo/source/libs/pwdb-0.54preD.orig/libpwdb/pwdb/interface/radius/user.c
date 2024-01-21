/* This file contains the necessary interface functions to
 * the pwdb modules. They are declared as static and this file
 * should be included in the module interface file, because
 * the function addresses are used in the structures from
 * pwdb_module.c file, and I have no idea how to feed a pointer
 * to an external function in that struct...
 * CG
 */

static int _pwdb_radius_return_data(RADIUS_RESULT rad_result,
                                    const struct pwdb **p)
{
    int         length;
    u_char        *buffer;
    int         retval;
    u_char     	*ptr;
    int		attribute;
    int		attrlen;
    DICT_ATTR	*attr;

    D(("called"));
    retval = dict_init();
    if (retval < 0) {
	D(("failed to initialize dictionary entries."));
	return PWDB_ABORT;
    }
    length = rad_result.length;
    buffer = rad_result.result;

    /*
     * Extract attribute-value pairs
     */
    ptr = buffer;
    while(length > 0) {
	attribute = *ptr++;
	attrlen = *ptr++;
	if (attrlen < 2) {
	    length = 0;
	    continue;
	}
	attrlen -= 2;
	if ((attr = dict_attrget(attribute)) == (DICT_ATTR *)NULL) {
	    D(("received unknown attribute %d\n", 
	       attribute));
	} else if ( attrlen >= AUTH_STRING_LEN ) {
	    D(("attribute %d too long, length of %d >= %d\n", 
	       attribute, attrlen, AUTH_STRING_LEN));
	} else {
	    char str_temp[255];
	    UINT4 tmp_int4;
	    
	    /* some sane initialization */
	    memset(str_temp,0,255);
	    tmp_int4 = 0;

	    switch(attr->type) {
		case PW_TYPE_STRING:
		    memcpy(str_temp, ptr, attrlen);
		    retval = pwdb_set_entry(*p, attr->name,        /* entry name       */
					    str_temp, attrlen + 1, /* value and length */
					    NULL,                  /* XXX: compare function */
					    txtcpy,                /* strval function  */
					    attrlen + 1);
		    if (retval != PWDB_SUCCESS)
			return retval;
		    break;			
		case PW_TYPE_INTEGER:
		    tmp_int4 = LONG_VAL_PTR(ptr);
		    retval = pwdb_set_entry(*p, attr->name, /* entry name               */
					    &tmp_int4, 4,   /* this is a 32-bit integer */
					    NULL,           /* XXX: compare function    */
					    str_integer,    /* strval function          */
					    15);            /* max str length for an integer */
		    if (retval != PWDB_SUCCESS)
			return retval;
		    break;
		case PW_TYPE_IPADDR:
		    retval = pwdb_set_entry(*p, attr->name, /* entry name        */
					    ptr, 4,         /* value and size    */
					    NULL,           /* XXX: compare function */
					    str_ipaddr,     /* strval function   */
					    16);            /* xxx.xxx.xxx.xxx\0 */
		    if (retval != PWDB_SUCCESS)
			return retval;
		    break;
		case PW_TYPE_DATE:
		    tmp_int4 = LONG_VAL_PTR(ptr);
		    retval = pwdb_set_entry(*p, attr->name, /* entry name               */
					    &tmp_int4, 4,   /* this is a 32-bit integer */
					    NULL,           /* XXX: compare function    */
					    str_date,       /* strval function          */
					    128);            /* max str length for an integer */
		    if (retval != PWDB_SUCCESS)
			return retval;
		    break;
		default:
		    D(("%s (Unknown Type %d)\n", attr->name,attr->type));
		    break;
	    }
	}
	ptr += attrlen;
	length -= attrlen + 2;
    }

    return PWDB_SUCCESS;
}

/************************************************
 * RADIUS Server interface (USER)
 ************************************************/

static int _pwdb_radius_locate(const char *name,
                   const int id,
                   const struct pwdb **p)
{
    RADIUS_SERVER rad_server;    
    const struct pwdb_entry * ent;
    const struct pwdb_entry * pwe;
    int retval;
    char *clear_password;
    char *pwdb_entry_user=NULL;
    char hostname[BUFFER_SIZE];
    char secret[BUFFER_SIZE];
    RADIUS_RESULT rad_result;

    D(("called"));
    if (p==NULL)
        return PWDB_BAD_REQUEST;

    if (name == PWDB_NAME_UNKNOWN) {
        if (*p == NULL)
            return PWDB_BAD_REQUEST;
        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval == PWDB_SUCCESS) {
            pwdb_entry_user = _pwdb_dup_string((char *) pwe->value);
            if ((pwdb_entry_user == NULL) && (pwe->value != NULL)) {
                pwdb_entry_delete(&pwe);
                return PWDB_MALLOC;
            }
            pwdb_entry_delete(&pwe);
            if (pwdb_entry_user == NULL)
                return PWDB_BAD_REQUEST;
            name = pwdb_entry_user;
        } else {
            DO_DEBUG;
            return PWDB_BAD_REQUEST;
        }
    }

    if (!*p) {
        retval = pwdb_new(p, 0);
        if (retval != PWDB_SUCCESS) {
            DO_DEBUG;
            pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
            return retval;
        }
    }

    if (name == PWDB_NAME_UNKNOWN)
        return PWDB_BAD_REQUEST;

    retval = pwdb_get_entry(*p,"pass_phrase", &ent);
    if (retval != PWDB_SUCCESS) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_PASS_PHRASE_REQD;
    }
    clear_password = _pwdb_dup_string(ent->value);
    pwdb_entry_delete(&ent);
    if (!clear_password) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_MALLOC;
    }    

    /* now is a good time to clear&destroy the pass_phrase entry */
    retval = pwdb_set_entry(*p,"pass_phrase",NULL,-1,NULL,NULL,0); \
    if (retval != PWDB_SUCCESS) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        clear_password = _pwdb_delete_string(clear_password);
        return PWDB_ABORT;
    }    

    D(("trying to get the RADIUS server information"));
    /* start the show - get the server to connect to and his secret */
    retval = get_server_entries(hostname, secret);
    if ((retval != PWDB_RADIUS_SUCCESS) ||
	!strlen(hostname) || !strlen(secret)) {
	clear_password = _pwdb_delete_string(clear_password);
	D(("failed"));
	return PWDB_ABORT;
    }
    D(("success: hostname=%s, secret=%s", hostname, secret));
    rad_server.hostname = hostname;
    rad_server.secret = secret;
    retval = rad_authenticate(rad_server, name, clear_password,&rad_result);
    clear_password = _pwdb_delete_string(clear_password);
    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
    if (retval != PWDB_RADIUS_SUCCESS) {
        switch (retval) {
            case PWDB_RADIUS_CONF_ERR:
            case PWDB_RADIUS_LOCAL_ERR:
                return PWDB_CONF_ERR;
            case PWDB_RADIUS_AUTH_FAIL:
                return PWDB_NOT_FOUND;
	    case PWDB_RADIUS_TIMEOUT:
		return PWDB_TIMEOUT;
            default:
                return PWDB_ABORT;
        }
    }
    retval = _pwdb_radius_return_data(rad_result,p);
    return retval;
}

static int _pwdb_radius_request(const char *entry_name,
                                const struct pwdb **p)
{
    return PWDB_UNSUPPORTED;
}

static int _pwdb_radius_replace(const char *name,
                                const int id,
                                const struct pwdb **p)
{
    RADIUS_SERVER rad_server;
    int retval;
    char *clear_new_password;
    char *clear_old_password;
    const struct pwdb_entry * ent;
    const struct pwdb_entry * pwe;
    char *pwdb_entry_user = NULL;
    char hostname[BUFFER_SIZE];
    char secret[BUFFER_SIZE];
    RADIUS_RESULT rad_result;
    
    /* WARNING: id parameter is ignored */
    if (!p || !*p)
        return PWDB_BAD_REQUEST;
   
    if (name == PWDB_NAME_UNKNOWN) {
        retval = pwdb_get_entry(*p, "user", &pwe);
        if (retval == PWDB_SUCCESS) {
            pwdb_entry_user = _pwdb_dup_string((char *)pwe->value);
            if ((pwdb_entry_user == NULL) && (pwe->value != NULL)) {
                pwdb_entry_delete(&pwe);
                return PWDB_MALLOC;
            }
            pwdb_entry_delete(&pwe);
            if (!strlen(pwdb_entry_user))
                pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        } else {
            DO_DEBUG;
            return retval;
        }
    }

    if ((name == PWDB_NAME_UNKNOWN) && (pwdb_entry_user == NULL))
        return PWDB_BAD_REQUEST;
    else if (name == PWDB_NAME_UNKNOWN)
        name = pwdb_entry_user;
        
    /* Get the old password */
    retval = pwdb_get_entry(*p,"pass_phrase", &ent);
    if (retval != PWDB_SUCCESS) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_PASS_PHRASE_REQD;
    }
    clear_old_password = _pwdb_dup_string(ent->value);
    pwdb_entry_delete(&ent);
    if (!clear_old_password) {
        return PWDB_MALLOC;
    }    
    retval = pwdb_set_entry(*p,"pass_phrase",NULL,-1,NULL,NULL,0); \
    if (retval != PWDB_SUCCESS) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_ABORT;
    }
    /* Get the new password */
    retval = pwdb_get_entry(*p,"passwd", &ent);
    if (retval != PWDB_SUCCESS) {
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_PASS_PHRASE_REQD;
    }
    clear_new_password = _pwdb_dup_string(ent->value);
    pwdb_entry_delete(&ent);
    if (!clear_new_password) {
        clear_old_password = _pwdb_delete_string(clear_old_password);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_MALLOC;
    }    
    retval = pwdb_set_entry(*p,"passwd",NULL,-1,NULL,NULL,0); \
    if (retval != PWDB_SUCCESS) {
        clear_new_password = _pwdb_delete_string(clear_new_password);
        clear_old_password = _pwdb_delete_string(clear_old_password);
        pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
        return PWDB_ABORT;
    }    
    /* start the show - get the server to connect to and his secret */
    retval = get_server_entries (hostname, secret);
    if ((retval != PWDB_RADIUS_SUCCESS) ||
        !strlen(hostname) || !strlen(secret)) {
	clear_new_password = _pwdb_delete_string(clear_new_password);
	clear_old_password = _pwdb_delete_string(clear_old_password);
        return PWDB_CONF_ERR;
    }
    rad_server.hostname = hostname;
    rad_server.secret = secret;
    retval = rad_change_passwd(rad_server, name, clear_old_password,
                               clear_new_password, &rad_result);
    clear_old_password = _pwdb_delete_string(clear_old_password);
    clear_new_password = _pwdb_delete_string(clear_new_password);
    pwdb_entry_user = _pwdb_delete_string(pwdb_entry_user);
    if (retval != PWDB_RADIUS_SUCCESS) {
        switch (retval) {
            case PWDB_RADIUS_CONF_ERR:
            case PWDB_RADIUS_LOCAL_ERR:
                return PWDB_CONF_ERR;
            case PWDB_RADIUS_AUTH_FAIL:
                return PWDB_NOT_FOUND;
	    case PWDB_RADIUS_TIMEOUT:
		return PWDB_TIMEOUT;
            default:
                return PWDB_ABORT;
        }
    }
    retval = _pwdb_radius_return_data(rad_result,p);
    return retval;
}

static int _pwdb_radius_delete(const char *name,
                               const int id,
                               const struct pwdb **p)
{
    return PWDB_UNSUPPORTED;
}

static int _pwdb_radius_support(const char *entry_name)
{
    int i = 0;
    const char *supp_entry[] = {
        "user",                  "passwd",
        "rad_chap_password",     "rad_client_id",
        "rad_client_port_id",    "rad_user_service_type",
        "rad_framed_protocol",   "rad_framed_address",
        "rad_framed_netmask",    "rad_framed_routing",
        "rad_framed_filter_id",  "rad_framed_mtu",
        "rad_framed_compression","rad_login_host",
        "rad_login_service",     "rad_login_tcp_port",
        "rad_old_password",      "rad_port_message",
        "rad_dialback_no",       "rad_dialback_name",
        "rad_expiration",        "rad_framed_route",
        "rad_framed_ipx_network","rad_challenge_state",
        "rad_acct_status_type",  "rad_acct_delay_time",
        "rad_acct_session_id",   "rad_acct_authentic",
        "rad_acct_session_time", NULL
    };

    while (supp_entry[i] != NULL) {
        if (!strcmp(supp_entry[i], entry_name))
            return PWDB_SUCCESS;
        i++;
    }
    return PWDB_UNSUPPORTED;
}

static int _pwdb_radius_flags(pwdb_flag *flags)
{
    return PWDB_UNSUPPORTED;
}
