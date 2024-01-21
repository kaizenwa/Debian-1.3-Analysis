/*
 * Overview access defines and private data
 *
 * @(#)$Id: overview.h,v 1.2 1994/11/01 05:57:31 sob Exp sob $
 */

#define OVER_NAME ".overview"
#define ROVER_NAME ".rover"

/* Undefine this if a new field makes the first letters no longer unique. */
#define OVER_UNIQUE_1ST_LTRS

char *over_field[] = {
	"ARTICLE #"		/* 0 */
	,"subject"		/* 1 */
	,"from"			/* 2 */
	,"date"			/* 3 */
	,"message-id"		/* 4 */
	,"references"		/* 5 */
	,"bytes"		/* 6 */
	,"lines"		/* 7 */
#ifdef OVER_XREFS
	,"xref"			/* 8 */
#endif
};

#define OVER_GROUP_FIELD	8	/* This is the group field */

#define OVER_FIELD_COUNT	(sizeof over_field / sizeof (char*))
