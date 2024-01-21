/*
 * In this file are defined some helpers functions for handling
 * pwdb entries (convert to string, comparison, etc.
 * Andrew Morgan & Cristian Gafton
 */

#define STRLEN_INT     15  /* length of an integer in ascii -- secure? */

#ifdef DEBUG
#define DO_DEBUG \
    printf("DEBUG: %s:%s(%d):\n\t%s\n", __FILE__, __FUNCTION__, __LINE__-1, \
        pwdb_strerror(retval));
#else
#define DO_DEBUG
#endif                                                            

static int dump_shorts(const void *from, char *to, int len)
{
    const short int *frm = (const short int *)from;
    int i;

    if (frm) {

        /* write an array of integers to 'to' */

        len /= sizeof(short int);
        for (i=0; i<len; ++i) {
            sprintf(to, "%s%d", i?",":"", frm[i]);
            to += strlen(to);
        }
    } else if (to && len > 0) {
        *to = '\0';
    } else {
        return PWDB_BAD_REQUEST;
    }

    return PWDB_SUCCESS;
}

static int txtcpy(const void *from, char *to, int len)
{
    const char *frm = (const char *)from;

    if (frm) {
        do {
            if (--len < 0)
                break;
            *to++ = *frm;
        } while (*frm++);
	*to = '\0';
    } else if (to && len > 0) {
        *to = '\0';
    } else {
        return PWDB_BAD_REQUEST;
    }

    return PWDB_SUCCESS;
}

static int str_ipaddr(const void *data, char * string, int length)
{
    if (length != 4) {
        *string = '\0';
        return 0;
    }
    sprintf(string, "%d.%d.%d.%d",
            (unsigned char)((const char *)data)[0],
            (unsigned char)((const char *)data)[1],
            (unsigned char)((const char *)data)[2],
            (unsigned char)((const char *)data)[3]
           );
    return 1;
}

static int str_integer(const void *data, char * string, int length)
{
    if (length != sizeof (u_int)) {
        *string='\0';
        return 0;
    }
    sprintf(string,"%d",(u_int)*((const u_int *)data));
    return 1;
}

static int str_long(const void *data, char * string, int length)
{
    if (length != sizeof (unsigned long)) {
        *string='\0';
        return 0;
    }
    sprintf(string,"%lu",(unsigned long)*((const unsigned long *)data));
    return 1;
}
    
static int str_date(const void *data, char *string, int length)
{
    const time_t *display_time;

    if (length != sizeof (time_t)) {
	*string = '\0';
	return 0;
    }
    display_time = (const time_t *)data;
    strftime(string, _PWDB_STR_DATE_LENGTH-2, "%b %e %Y",
	     gmtime(display_time));
    return 1;
}
		
