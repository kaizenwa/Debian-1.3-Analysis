/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

struct afp_versions {
    char	*av_name;
    int		av_number;
};

struct afp_uams {
    char	*au_name;
    int		(*au_login)();
    int		(*au_logincont)();
    int		au_flags;
};
#define AU_OFF	(1<<0)

extern int	noauth_login();
#ifdef KRB
extern int	krb4_login();
extern int	krb4_logincont();
#endif KRB
extern int	clrtxt_login();
#ifdef UAM_AFSKRB
extern int	afskrb_login();
extern int	afskrb_logincont();
#endif UAM_AFSKRB

extern uid_t	uuid;
#if defined( __svr4__ ) && !defined( NGROUPS )
#define NGROUPS	NGROUPS_MAX
#endif __svr4__ NGROUPS
#if defined( sun ) && !defined( __svr4__ ) || defined( ultrix )
extern int	groups[ NGROUPS ];
#else sun __svr4__ ultrix
extern gid_t	groups[ NGROUPS ];
#endif sun __svr4__ ultrix
extern int	ngroups;
