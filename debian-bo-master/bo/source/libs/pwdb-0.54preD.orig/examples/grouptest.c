#include <stdio.h>
#include <pwdb/pwdb_public.h>

#define GROUP  "xxxyyy"
#define NEW_USERS "aaa"

int main(void)
{
    const struct pwdb * _pwdb = NULL;

    int retval;
    
    retval = pwdb_start();
    printf("pwdb_start call: %s, _pwdb=%p\n", pwdb_strerror(retval),_pwdb);

    retval=pwdb_locate("group", PWDB_DEFAULT, GROUP, PWDB_ID_UNKNOWN, &_pwdb);
    printf("locate returned: %s\n", pwdb_strerror(retval));
    pwdb_print_pwdb_struct(_pwdb);
    retval=pwdb_set_entry(_pwdb,"users",NEW_USERS,1+strlen(NEW_USERS),
                          NULL,NULL,0);
    printf("set_entry returned: %s\n", pwdb_strerror(retval));
    pwdb_print_pwdb_struct(_pwdb);
    retval=pwdb_replace("group", _pwdb->source, GROUP, PWDB_ID_UNKNOWN, &_pwdb);
    printf("replace returned: %s\n", pwdb_strerror(retval));
    pwdb_end();

    exit(retval);
}
