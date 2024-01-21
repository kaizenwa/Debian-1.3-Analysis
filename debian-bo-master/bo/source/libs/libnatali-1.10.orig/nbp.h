/*
** natali/nbp.h
** Copyright 1995, 1996, Trinity College Computing Center.
** Written by David Chappell.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software and documentation are provided "as is" without
** express or implied warranty.
**
** This is an attempt the implement the System V ALI for Linux.
**
** Last modified 15 February 1996.
*/

#ifndef _NATALI_NBP
#define _NATALI_NBP 1

#ifdef __cplusplus
extern "C" {
#endif

/* If we are not compiling the library, equate nbp_lookup()
   with the ALI version. */
#ifndef _NATALI
#define nbp_lookup natali_nbp_lookup
#endif

/* The maximum length of a name element. */
#define NBP_NVE_STR_SIZE 32

/* A string which contains a name element. */
typedef struct at_nvestr_t
    {
    char len;
    char str[NBP_NVE_STR_SIZE];
    } at_nvestr_t;

/* This structure describes a name to look up. */
typedef struct at_entity_t
    {
    at_nvestr_t object;
    at_nvestr_t type;
    at_nvestr_t zone;
    } at_entity_t;

/* This structure describes one record returned by nbp_loopup(). */
typedef struct at_nbptuple_t
    {
    at_inet_t enu_addr;		/* AppleTalk internet address */
    u_char enu_enum;
    at_entity_t enu_entity;	/* AppleTalk name */
    } at_nbptuple_t;
    
/* nbp_errno and its possible values */
extern int nbp_errno;
#define NBPSYSERR 1
#define NBPBADNAME 2
#define NBPBADPARM 3
#define NBPNORESOURCE 4
#define NBPTABLEFULL 5
#define NBPDUPNAME 6
#define NBPNONAME 7

/* Function prototypes */
int nbp_parse_entity(at_entity_t *entity, const char *str);
int natali_nbp_lookup (at_entity_t *entity, at_nbptuple_t *buf, int max, at_retry_t *retry, u_char *more);
int nbp_register(at_entity_t *entity, int fd, at_retry_t *retry);
int nbp_remove(at_entity_t *entity, int fd);

#ifdef __cplusplus
} ;
#endif

#endif

/* end of file */
