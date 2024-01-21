#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "fcgi_stdio.h"

#ifndef FALSE
#define FALSE (0)
#endif

#ifndef TRUE
#define TRUE  (1)
#endif

extern char **environ;
static char **requestEnviron = NULL;

/*
 * For each variable in the array envp, either set or unset it
 * in the global hash %ENV.
 */
static void
DoPerlEnv(envp, set)
char **envp;
int set;
{
    int i;
    char *p, *p1;
    HV   *hv;
    SV   *sv;
    hv = perl_get_hv("ENV", TRUE);
    for(i = 0; ; i++) {
        if((p = envp[i]) == NULL) {
            break;
        }
        p1 = strchr(p, '=');
        assert(p1 != NULL);
        *p1 = '\0';
        if(set) {
            sv = newSVpv(p1 + 1, 0);
            sv_magic(sv, sv, 'e', p, p1 - p);
            hv_store(hv, p, p1 - p, sv, 0);
        } else {
            hv_delete(hv, p, p1 - p, G_DISCARD);
        }
        *p1 = '=';
    }
}


MODULE = FCGI		PACKAGE = FCGI


int
accept()

    CODE:
    {
        char **savedEnviron;
        int acceptStatus;
        /*
         * Unmake Perl variable settings for the request just completed.
         */
        if(requestEnviron != NULL) {
            DoPerlEnv(requestEnviron, FALSE);
            requestEnviron = NULL;
        }
        /*
         * Call FCGI_Accept but preserve environ.
         */
        savedEnviron = environ;
        acceptStatus = FCGI_Accept();
        requestEnviron = environ;
        environ = savedEnviron;
        /*
         * Make Perl variable settings for the new request.
         */
        if(acceptStatus >= 0 && !FCGX_IsCGI()) {
            DoPerlEnv(requestEnviron, TRUE);
        } else {
            requestEnviron = NULL;
        }
        RETVAL = acceptStatus;
    }
    OUTPUT:
    RETVAL


void
finish()

    CODE:
    {
        /*
         * Unmake Perl variable settings for the completed request.
         */
        if(requestEnviron != NULL) {
            DoPerlEnv(requestEnviron, FALSE);
            requestEnviron = NULL;
        }
        /*
         * Finish the request.
         */
        FCGI_Finish();
    }


void
set_exit_status(status)

    int status;

    CODE:
    FCGI_SetExitStatus(status);


int
start_filter_data()

    CODE:
    RETVAL = FCGI_StartFilterData();

    OUTPUT:
    RETVAL
