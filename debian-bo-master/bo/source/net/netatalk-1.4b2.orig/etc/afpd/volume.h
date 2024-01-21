/*
 * Copyright (c) 1990,1994 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

struct vol {
    struct vol		*v_next;
    char		*v_name;
    char		*v_path;
    struct dir		*v_dir, *v_did;
    int			v_flags;
#ifdef __svr4__
    int			v_qfd;
#else __svr4__
    void		*v_gvs;
#endif __svr4__
    u_long		v_time;
    int			v_lastdid;
    u_short		v_vid;
};

extern struct vol	*getvolbyvid();

#define AFPVOL_OPEN	(1<<0)
#define AFPVOL_DT	(1<<1)

#define AFPVOL_GVSMASK	(7<<2)
#define AFPVOL_NONE	(0<<2)
#define AFPVOL_AFSGVS	(1<<2)
#define AFPVOL_USTATFS	(2<<2)
#define AFPVOL_UQUOTA	(4<<2)

#define AFPVOLSIG_FIX	2

#define VOLPBIT_ATTR	0
#define VOLPBIT_SIG	1
#define VOLPBIT_CDATE	2
#define VOLPBIT_MDATE	3
#define VOLPBIT_BDATE	4
#define VOLPBIT_VID	5
#define VOLPBIT_BFREE	6
#define VOLPBIT_BTOTAL	7
#define VOLPBIT_NAME	8
