/*
 * $Id: pwdb_error.c,v 1.1 1996/10/16 22:16:04 morgan Exp morgan $
 *
 * This file contains the pwdb error -> text mapping function
 */

/*
 * $Log: pwdb_error.c,v $
 * Revision 1.1  1996/10/16 22:16:04  morgan
 * Initial revision
 *
 */

#include <pwdb/pwdb_public.h>
#include "pwdb_module.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "_pwdb_macros.h"

/*
 * FUNCTION: pwdb_strerror
 *
 * This function returns a text string to indicate the meaning of a
 * pwdb error
 */

const char *pwdb_strerror(int err)
{
    switch (err) {
        case PWDB_SUCCESS:
            return "pwdb: task completed successfully";
        case PWDB_BAD_REQUEST:
            return "pwdb: request not recognized";
        case PWDB_TOO_WEAK:
            return "pwdb: insufficient privilege for operation";
        case PWDB_ABORT:
            return "pwdb: internal failure - seek help";
        case PWDB_BLOCKED:
            return "pwdb: another process has locked resource";
        case PWDB_MALLOC:
            return "pwdb: insufficient memory for operation";
        case PWDB_NOT_FOUND:
            return "pwdb: requested item was not found";
        case PWDB_PASS_PHRASE_REQD:
            return "pwdb: pass_phrase needed to satisfy request";
        case PWDB_CONF_ERR:
            return "pwdb: file " PWDB_CONF " needs to be fixed";
        case PWDB_EXPIRED:
            return "pwdb: structure is no longer valid";
        case PWDB_UNSUPPORTED:
            return "pwdb: unsupported function call";
	case PWDB_TIMEOUT:
	    return "pwdb: request timed out";
    }
    return "pwdb: return status value unrecognized";
}

void pwdb_print_pwdb_struct(const struct pwdb *p)
{
    struct _pwdb_entry_list *ent_list;
    struct pwdb_entry * entry;

    printf("pwdb *=%p\n", p);
    if (!p)
        return;

    {
        const pwdb_type *tmp;

        printf("pwdb source type:");
        for (tmp=p->source; tmp && *tmp != _PWDB_MAX_TYPES; ++tmp)
            printf(" %s",pwdb_db_name(*tmp));
        printf("\n");
    }

    ent_list = p->data;
    while (ent_list) {
        entry = ent_list->entry;
        if (entry) {
            int i;
            
            printf("\t name=%-25s length=%3d value=",
                   entry->name, entry->length);
            if (entry->max_strval_size && entry->strvalue) {
                char *s;
                s = (char *) malloc(entry->max_strval_size);
                if (!s)
                    for (i=0; i < entry->length; i++) {
                        printf("%d[%c] "
                               , (unsigned char)((char *)(entry->value))[i]
                               , isprint(((char *)(entry->value))[i]) ?
                               ((char *)(entry->value))[i]:'_');
                    }
                else {
                    memset(s, 0, entry->max_strval_size); 
                    (*entry->strvalue)(entry->value,s,entry->length);
                    printf("%s",s);
                    free(s);
                }
            } else {
                for (i=0; i < entry->length; i++) {
                    printf("%d[%c] "
                           , (unsigned char)((char *)(entry->value))[i]
                           , isprint(((char *)(entry->value))[i]) ?
                           ((char *)(entry->value))[i]:'_');
                }
            }    
            printf("\n");
        }
        ent_list = ent_list->next;
    }
    return;
}


void debug_pwdb_struct(const struct pwdb *p)
{
    struct _pwdb_entry_list *ent_list;
    struct pwdb_entry * entry;
    char buffer[BUFSIZ];

    D(("pwdb *=%p", p));
    if (!p)
        return;

    {
        const pwdb_type *tmp;

        sprintf(buffer, "pwdb source type:");
        for (tmp=p->source; tmp && *tmp != _PWDB_MAX_TYPES; ++tmp)
            sprintf(buffer, "%s %s", buffer, pwdb_db_name(*tmp));
	D(("%s",buffer));
    }

    ent_list = p->data;
    while (ent_list) {
        entry = ent_list->entry;
        if (entry) {
            int i;

            sprintf(buffer, "\t name=%-25s length=%3d value=",
		    entry->name, entry->length);
            if (entry->max_strval_size && entry->strvalue) {
                char *s;
                s = (char *) malloc(entry->max_strval_size);
                if (!s)
                    for (i=0; i < entry->length; i++) {
                        sprintf(buffer, "%s%d[%c] "
				, buffer
				, (unsigned char)((char *)(entry->value))[i]
				, isprint(((char *)(entry->value))[i]) ?
				((char *)(entry->value))[i]:'_');
                    }
                else {
                    memset(s, 0, entry->max_strval_size);
                    (*entry->strvalue)(entry->value,s,entry->length);
                    sprintf(buffer,"%s%s",buffer,s);
                    free(s);
                }
            } else {
                for (i=0; i < entry->length; i++) {
                    sprintf(buffer, "%s%d[%c] "
			    , buffer
			    , (unsigned char)((char *)(entry->value))[i]
			    , isprint(((char *)(entry->value))[i]) ?
			    ((char *)(entry->value))[i]:'_');
                }
            }
            D(("%s", buffer));
        }
        ent_list = ent_list->next;
    }
    return;
}

/* -- end of file -- */

