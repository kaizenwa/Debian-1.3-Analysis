/*
 * Copyright (c) 1990,1993 Regents of The University of Michigan.
 * All Rights Reserved.  See COPYRIGHT.
 */

struct ofork {
    struct adouble	of_ad;
    struct dir		*of_dir;
    char		*of_name;
};

#define OPENFORK_DATA	(0)
#define OPENFORK_RSCS	(1<<7)

#define OPENACC_RD	(1<<0)
#define OPENACC_WR	(1<<1)
#define OPENACC_DRD	(1<<4)
#define OPENACC_DWR	(1<<5)

#define AFPFORK_OPEN	(1<<0)
#define AFPFORK_RSRC	(1<<1)
#define AFPFORK_DATA	(1<<2)

struct ofork	*of_alloc();
struct ofork	*of_find();
