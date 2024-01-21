#include "datetok.h"

/*
 * TODO: XXX indicates things that should be taken out.  A simple rule is to
 * have the long form, and the single letter abbreviation (except for month)
 */
/* This table must be kept sorted by the first member */
datetkn datereltoks[] = {
	"&",		IGNORE,		0,
	"a",		NUMBER,		1,
	"after",	AFTER,		0,
	"ago",		AGO,		1,
	"an",		NUMBER,		1,
	"and",		IGNORE,		0,
	"before",	BEFORE,		1,
	"d",		DAYS,		1,
	"day",		DAYS,		1,
	"days",		DAYS,		1,
	"dy",		DAYS,		1,	/* XXX */
	"dys",		DAYS,		1,	/* XXX */
	"eighth",	ORDINAL,	8,
	"eleven",	ORDINAL,	11,
	"fifth",	ORDINAL,	5,
	"first",	ORDINAL,	1,
	"fortni",	DAYS,		14,	/* fortnight */
	"fourth",	ORDINAL,	4,
	"from",		AFTER,		0,
	"h",		HOURS,		1,
	"hence",	AGO,		0,
	"hour",		HOURS,		1,
	"hours",	HOURS,		1,
	"hr",		HOURS,		1,	/* XXX */
	"hrs",		HOURS,		1,	/* XXX */
	"last",		ORDINAL,	-1,
	"m",		SECONDS,	60,
	"min",		SECONDS,	60,	/* XXX */
	"mins",		SECONDS,	60,	/* XXX */
	"minute",	SECONDS,	60,
	"month",	MONTH,		1,
	"months",	MONTH,		1,
	"next",		NUMBER,		1,
	"next",		ORDINAL,	2,	/* TODO: conflict! */
	"ninth",	ORDINAL,	9,
	"s",		SECONDS,	1,
	"sec",		SECONDS,	1,	/* XXX */
/*	"second",	ORDINAL,	2,	*/
	"second",	SECONDS,	1,
	"secs",		SECONDS,	1,	/* XXX */
	"sevent",	ORDINAL,	7,
	"sixth",	ORDINAL,	6,
	"tenth",	ORDINAL,	10,
	"the",		IGNORE,		0,
	"third",	ORDINAL,	3,
	"this",		ORDINAL,	0,
	"twelft",	ORDINAL,	12,
	"w",		DAYS,		7,
	"week",		DAYS,		7,
	"weeks",	DAYS,		7,
	"wk",		DAYS,		7,	/* XXX */
	"wks",		DAYS,		7,	/* XXX */
	"y",		YEARS,		1,
	"year",		YEARS,		1,
	"years",	YEARS,		1,
	"yr",		YEARS,		1,	/* XXX */
	"yrs",		YEARS,		1,	/* XXX */
};

unsigned szdatereltoks = sizeof datereltoks / sizeof datereltoks[0];
