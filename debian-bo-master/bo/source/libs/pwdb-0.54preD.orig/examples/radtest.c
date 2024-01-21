
/* RADIUS client test program using the generic interface */

#include <pwdb/pwdb_public.h>
#include <stdio.h>

#define USERNAME    "bob"
#define PASSWORD    "testing"
#define NEWPASSWORD "NEWpassWORD"

int main(void) {
    const struct pwdb * pw = NULL;
    /* struct pwdb_entry * ent = NULL; */
    int retval;
    pwdb_type tlist[2];

    tlist[0] = PWDB_RADIUS;
    tlist[1] = _PWDB_MAX_TYPES;

    pwdb_start();

    /* LOCATE CODE */
    retval = pwdb_new(&pw, 0);
    printf("pwdb new call: %s, pw=%p\n", pwdb_strerror(retval),pw);
    /* try first locate */
    retval=pwdb_locate("user", tlist, USERNAME, PWDB_ID_UNKNOWN, &pw);
    printf("locate returned: %s\n", pwdb_strerror(retval));
    if (retval == PWDB_PASS_PHRASE_REQD) {
        pwdb_set_entry(pw,"pass_phrase",PASSWORD, 1+strlen(PASSWORD)
                   , NULL, NULL, 0);
    } else {
        pwdb_delete(&pw);
        pwdb_end();
        printf("Don't know how to handle this error. Exit.\n");
        exit(-1);
    }
    /* try a second locate */
    pwdb_print_pwdb_struct(pw);    
    retval=pwdb_locate("user", tlist, USERNAME, PWDB_ID_UNKNOWN, &pw);
    printf("locate returned: %s\n", pwdb_strerror(retval));
    if (retval != PWDB_SUCCESS) {
        pwdb_delete(&pw);
        pwdb_end();
        printf("Don't know how to handle this error. Exit.\n");
        exit(-1);
    }
    pwdb_print_pwdb_struct(pw);    
    pwdb_delete(&pw);

    /* UPDATING CODE */

    retval = pwdb_new(&pw, 0);
    printf("pwdb new call: %s, pw=%p\n", pwdb_strerror(retval),pw);
    pwdb_print_pwdb_struct(pw);
    
    /* the old password */
    pwdb_set_entry(pw,"pass_phrase",PASSWORD, 1+strlen(PASSWORD),
                   NULL, NULL, 0);
    pwdb_print_pwdb_struct(pw);
    /* The new password */
    pwdb_set_entry(pw,"passwd",NEWPASSWORD, 1+strlen(PASSWORD),
                   NULL, NULL, 0);
    pwdb_print_pwdb_struct(pw);
    /* tell other modules that RADIUS is taking care of this */
    pwdb_set_entry(pw,"defer_pass","R",2, NULL, NULL, 0);        
    pwdb_print_pwdb_struct(pw);
    /* Now perform an update on passwd */
    retval = pwdb_replace("user", tlist, USERNAME, PWDB_ID_UNKNOWN, &pw);
    printf("replace returned: %s\n",pwdb_strerror(retval));
    if (retval != PWDB_SUCCESS) {
        pwdb_delete(&pw);
        pwdb_end();
        printf("Don't know how to handle this error. Exit.\n");
        exit(-1);
    }
    pwdb_print_pwdb_struct(pw);    

    /* Now change back the passwd */
    pwdb_set_entry(pw,"pass_phrase",NEWPASSWORD, 1+strlen(PASSWORD),
                   NULL, NULL, 0);
    /* The new password */
    pwdb_set_entry(pw,"passwd",PASSWORD, 1+strlen(PASSWORD),
                   NULL, NULL, 0);
    /* tell other modules that RADIUS is taking care of this */
    pwdb_set_entry(pw,"defer_pass","R",2, NULL, NULL, 0);        
    /* Now perform an update on passwd */
    retval = pwdb_replace("user", tlist, USERNAME, PWDB_ID_UNKNOWN, &pw);
    printf("replace returned: %s\n",pwdb_strerror(retval));
    if (retval != PWDB_SUCCESS) {
        pwdb_delete(&pw);
        pwdb_end();
        printf("Don't know how to handle this error. Exit.\n");
        exit(-1);
    }
    pwdb_print_pwdb_struct(pw);    
    
    pwdb_delete(&pw);
        
    pwdb_end();
    return 0;
}
    
