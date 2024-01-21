/* $Id: context.h,v 1.2 1991/06/04 18:16:28 chip Exp $
 *
 * User context, as found in /etc/passwd.
 *
 * $Log: context.h,v $
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  91/05/13  18:36:55  chip
 * Initial revision
 * 
 */

/*----------------------------------------------------------------------
 * The context structure.
 */

#define CONTEXT struct context
CONTEXT
{
    CONTEXT *ct_next;
    char *ct_name;
    char *ct_home;
    int ct_uid;
    int ct_gid;
#ifdef GROUP_VECTOR
    int ct_numgroups;
    GRVEC_T *ct_groups;
#endif
};
