/*
 * $Id$
 *
 * This file is to be used to test the pwdb functions.
 * In order to utilize fully the library, it should be tested by root.
 */

#define SHADOW_FILE "/etc/shadow"

#include <stdio.h>
#include <string.h>
#include <fcntl.h>

#include <pwdb/pwdb_map.h>

#define testlog stdout

int main(void) {
    /* int id; */
    int have_shadow;
    const struct passwd *pw;
    const struct spwd *sp;
    struct passwd pwent;
    struct spwd spent;

    setpwent();
    {
        FILE *test;
        test = fopen(SHADOW_FILE, "r");
        if (test == NULL) {
             have_shadow = 0;
        } else {
             fclose(test);
             have_shadow = 1;
        }
    }
  
    if (have_shadow) {
        setspent();
    } else {
        fprintf(testlog,"[cannot read Shadow file]\n");
    }

/*
    for (id=0; id<1000; ++id) {

        if ((pw = getpwuid(id)) != NULL) {
             char *user = strdup(pw->pw_name);

            if ((pw = getpwnam(user)) == NULL) {
                fprintf(testlog,"failed to pw_lookup user `%s'\n", user);
                continue ;
            } else if (strcmp(user, pw->pw_name))
                fprintf(testlog,"user pw_lookup[%s] differs from uid[%s] one\n", pw->pw_name, user);

            if ((sp = getspnam(user)) == NULL) {
                fprintf(testlog,"failed to sp_lookup user `%s'\n", user);
                continue;
            } else if (strcmp(user, sp->sp_namp))
                fprintf(testlog,"user splookup[%s] differs from uid[%s] one\n", sp->sp_namp, user);
    
            free(user);

            fprintf(testlog,
                "user: %s\n"
                "\tPassword: %s (%s)\n"
                "\tUID=%d, GID=%d\n"
                "\tgecos: %s\n"
                "\tHOME: %s\n"
                "\tSHELL: %s\n"
                , pw->pw_name, pw->pw_passwd, sp->sp_pwdp, pw->pw_uid, pw->pw_gid
                , pw->pw_gecos, pw->pw_dir, pw->pw_shell
               );
        }
  }
*/

  /* Now test the update code */
  /* passwd */

  if (__pwdb_lckpwdf()!=0) {
    printf("Can not lock passwd file !\n");
    goto end;
  };
  
  if (!__pwdb_pw_open(O_RDWR)) {
    printf("can not open passwd file!\n");
    goto end;
  };
  
  pw = __pwdb_pw_locate("gafton1");
  if (!pw) {
    printf("can not pw_locate\n");
    goto end;
  };
  
  pwent = *pw;
  pwent.pw_gid++;
  
  if (! __pwdb_pw_update(&pwent)) {
    printf("update passwd error\n");
    goto end;
  };
  
  if (! __pwdb_pw_close()) {
    printf("error closing.\n");
    goto end;
  };

 if (!__pwdb_spw_open(O_RDWR)) {
    printf("Can not open shadow file !\n");
    goto end;
 };
 
 sp = __pwdb_spw_locate("gafton1");
 if (!sp) {
    printf("can not sp_locate\n");
    goto end;
 };
 
 spent = *sp;
 spent.sp_lstchg++;
 
 if (!__pwdb_spw_update(&spent)) {
    printf("update shadow error\n");
    goto end;
 };
 
 if (!__pwdb_spw_close()) {
    printf("can not close shadow\n");
    goto end;
 };
 
end:

  __pwdb_ulckpwdf();
  
  if (have_shadow) {
        endspent();
  }

  endpwent();
  
  return 0;
}
