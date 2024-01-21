/*
 *	(c) Copyright 1992, Luc Rooijakkers.  All rights reserved.
 *
 *	Character set support (rudimentary)
 */

#ifndef _NN_CHSET_H
#define _NN_CHSET_H 1

struct chset {
    char *cs_name;
    int cs_width;
};

extern struct chset *curchset;

extern struct chset *getchset();


#endif /* _NN_CHSET_H */
