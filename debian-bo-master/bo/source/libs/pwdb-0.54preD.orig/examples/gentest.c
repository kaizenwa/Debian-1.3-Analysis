/* this is a test for generic interface on pwdb */

#include <stdio.h>
#include <pwdb/pwdb_public.h>

#define USER  "morgan"
#define GECOS "GECOS field for testing"

int main(void)
{
    const struct pwdb * _pwdb = NULL;
    pwdb_flag flags;

    int retval;
    
    retval = pwdb_start();
    printf("pwdb_start call: %s, _pwdb=%p\n", pwdb_strerror(retval),_pwdb);

    retval=pwdb_locate("user", PWDB_DEFAULT, USER, PWDB_ID_UNKNOWN, &_pwdb);
    printf("locate returned: %s\n", pwdb_strerror(retval));
    pwdb_print_pwdb_struct(_pwdb);

    retval = pwdb_request("user", PWDB_DEFAULT, "members", &_pwdb);
    printf("members request returned: %s\n", pwdb_strerror(retval));
    pwdb_print_pwdb_struct(_pwdb);

    retval=pwdb_set_entry(_pwdb,"gecos",GECOS,1+strlen(GECOS)
              ,NULL,NULL,0);
    printf("set_entry returned: %s\n", pwdb_strerror(retval));

    retval=pwdb_flags("user", _pwdb->source, &flags);
    printf("flags(%o) returned: %s\n", flags, pwdb_strerror(retval));

    if ( flags & PWDB_F_NOUPDATE ) {
        printf("insufficient privilege to replace db entry\n");
    } else {
        retval=pwdb_replace("user", _pwdb->source, USER
			    , PWDB_ID_UNKNOWN, &_pwdb);
        printf("replace returned: %s\n", pwdb_strerror(retval));
    }

    /* Test for caching */
    _pwdb = NULL;
    retval=pwdb_locate("user", PWDB_DEFAULT, USER
		       , PWDB_ID_UNKNOWN, &_pwdb);
    printf("2nd locate returned: %s\n", pwdb_strerror(retval));
    pwdb_print_pwdb_struct(_pwdb);

    /* request their groups.. */

    retval=pwdb_request("group", PWDB_DEFAULT, "groups", &_pwdb);
    printf("request returned: %s\n", pwdb_strerror(retval));
    pwdb_print_pwdb_struct(_pwdb);

    retval = pwdb_delete(&_pwdb);
    printf("pwdb delete returned: %s\n", pwdb_strerror(retval));

    /* This one should fail */
    retval = pwdb_support("user", PWDB_DEFAULT, "password__1");
    printf("support for password__1: %s\n", pwdb_strerror(retval));

    /* This one should succeed */
    retval = pwdb_support("user", PWDB_DEFAULT, "passwd");
    printf("support for passwd: %s\n", pwdb_strerror(retval));

    pwdb_end();

    exit(retval);
}
