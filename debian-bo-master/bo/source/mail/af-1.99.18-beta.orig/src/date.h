/* Date.h - Declarations for af date handling.
   Copyright (C) 1992, 1993, 1996, 1997 Malc Arnold.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/****************************************************************************/
/* RCS info */

#ifndef lint
static char *DateId = "$Id: date.h,v 1.7 1997/04/20 10:32:45 malc Exp $";
#endif /* ! lint */

/****************************************************************************/
/* The names of days of the week */

static char *weekdays[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", NULL
};

/****************************************************************************/
/* The names of the months and the number of days in each */

static char *months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec", NULL
};

static char mo_days[] = {
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, -1
};

/****************************************************************************/
/* The struct for the local table of time zone data */

typedef struct tz_tab {
	char *zone;
	long offset;
} TZ_TAB;

/****************************************************************************/
/*
 * The table of time zones we can handle.
 *
 * This table is not comprehensive, but covers as many
 * non-conflicting time zone names as I have been able
 * to determine.  Unknown zones will be treated as UT.
 */

static TZ_TAB tzlist[] = {
	{ "UT",		0L },		/* Universal time */
	{ "UTC",	0L },		/* Universal Time (Coordinated) */
	{ "GMT",	0L },		/* Greenwich Mean Time */
	{ "BST",	3600L },	/* British Summer Time */
	{ "WET",	0L },		/* Western European Time */
	{ "WDT",	3600L },	/* Western Daylight Time */
	{ "CET",	3600L },	/* Central European Time */
	{ "MET",	3600L },	/* Middle European Time */
	{ "MEWT",	3600L },	/* Middle European Winter Time */
	{ "MEST",	7200L },	/* Middle European Summer Time */
	{ "EET",	7200L },	/* Eastern European Time  */
	{ "BT",		10800L },	/* Baghdad Time */
	{ "ZP4",	14400L },	/* USSR Zone 3 */
	{ "ZP5",	18000L },	/* USSR Zone 4 */
	{ "ZP6",	21600L },	/* USSR Zone 5 */
	{ "WAST", 	25200L },	/* Western Australia Standard Time */
	{ "WADT", 	28800L },	/* Western Australia Daylight Time */
	{ "HKT",	28800L },	/* Hong Kong Time */
	{ "JST",	32400L },	/* Japan Standard Time */
	{ "KST",	32400L },	/* Korean Standard Time */
	{ "KDT",	36000L },	/* Korean Daylight Time */
	{ "CAST",	34200L },	/* Central Australian Standard Time */
	{ "CADT",	37800L },	/* Central Australian Daylight Time */
	{ "EAST",	36000L },	/* Eastern Australian Standard Time */
	{ "EADT",	39600L },	/* Eastern Australian Daylight Time */
	{ "GST",	36000L },	/* Guam Standard Time */
	{ "NZT",	43200L },	/* New Zealand Time */
	{ "NZST",	43200L },	/* New Zealand Standard Time */
	{ "NZDT",	46800L },	/* New Zealand Daylight Time */
	{ "IDLE",	43200L },	/* International Date Line East */
	{ "SWT",	-3600L },	/* Swedish Winter Time */
	{ "SST",	0L },		/* Swedish Summer Time */
	{ "FWT",	-3600L },	/* French Winter Time */
	{ "FST",	0L },		/* French Summer Time */
	{ "WAT",	-3600L },	/* West African Time */
	{ "AT",		-7200L },	/* Azores Time */
	{ "EET",	-3600L },	/* Eastern European Time */
	{ "NFT",	-12600L },	/* Newfoundland Time */
	{ "NST",	-12600L },	/* Newfoundland Standard Time */
	{ "NDT",	-9000L },	/* Newfoundland Daylight Time */
	{ "AST",	-14400L },	/* Atlantic Standard Time */
	{ "ADT",	-10800L },	/* Atlantic Daylight Time */
	{ "EST",	-18000L },	/* Eastern Standard Time */
	{ "EDT",	-14400L },	/* Eastern Daylight Time */
	{ "CST",	-21600L },	/* Central Standard Time */
	{ "CDT",	-18000L },	/* Central Daylight Time */
	{ "MST",	-25200L },	/* Mountain Standard Time */
	{ "MDT",	-21600L },	/* Mountain Daylight Time */
	{ "PST",	-28800L },	/* Pacific Standard Time */
	{ "PDT",	-25200L },	/* Pacific Daylight Time */
	{ "YST",	-32400L },	/* Yukon Standard Time */
	{ "YDT",	-28800L },	/* Yukon Daylight Time */
	{ "HST",	-36000L },	/* Hawaii Standard Time */
	{ "HDT",	-32400L },	/* Hawaii Daylight Time */
	{ "CAT",	-36000L },	/* Central Alaskan Time */
	{ "AHST",	-36000L },	/* Alaska-Hawaii Standard Time */
	{ "AHDT",	-32400L },	/* Alaska-Hawaii Daylight Time */

	/* US Military time specifications */

	{ "A",		3600L },
	{ "B",		7200L },
	{ "C",		10800L },
	{ "D",		14400L },
	{ "E",		18000L },
	{ "F",		21600L },
	{ "G",		25200L },
	{ "H",		28800L },
	{ "I",		32400L },
	{ "K",		36000L },
	{ "L",		39600L },
	{ "M",		43800L },
	{ "N",		-3600L },
	{ "O",		-7200L },
	{ "P",		-10800L },
	{ "Q",		-14400L },
	{ "R",		-18000L },
	{ "S",		-21600L },
	{ "T",		-25200L },
	{ "U",		-28800L },
	{ "V",		-32400L },
	{ "W",		-36000L },
	{ "X",		-39600L },
	{ "Y",		-43800L },
	{ "Z",		0L },
	{ NULL,		0L}		/* Terminating value */
};

/****************************************************************************/
/* Values used for calculating seconds from the epoch */

#define BASE_YEAR		1900
#define EPOCH_YEAR		1970
#define NO_MONTHS		12

#define DAYS_PER_WEEK		7
#define DAYS_PER_YEAR		365
#define HOURS_PER_DAY		24
#define MINUTES_PER_HOUR	60
#define SECONDS_PER_MINUTE	60

#define SECONDS_PER_HOUR	(MINUTES_PER_HOUR * SECONDS_PER_MINUTE)
#define SECONDS_PER_DAY		(HOURS_PER_DAY * SECONDS_PER_HOUR)
#define SECONDS_PER_YEAR	(DAYS_PER_YEAR * SECONDS_PER_DAY)

/****************************************************************************/
/* Macros used for calculating dates */

#define IS_LEAP_YEAR(year)	(((year) % 4 == 0) && \
				 ((year) % 100 != 0 || (year % 400) == 0))
#define IS_LEAP_MONTH(mo)	((mo) == 1)

#define IS_DST(a)		(a->type == AT_ATOM && \
				 !strcasecmp(a->text, "DST"))
#define SET_DST(offset)		(offset += SECONDS_PER_HOUR)

/****************************************************************************/
/* The maximum length of a POSIX or short date in string format */

#define MAXDATELEN		25
#define MAXUDATELEN		25
#define MAXQDATELEN		12

/* The threshold for displaying dates in recent format */

#define RECENT_THRESHOLD	(180 * SECONDS_PER_DAY)

/* The base length and format of a message ID */

#define BASEIDLEN		18
#define ID_FORMAT		"<%02d%02d%02d%02d%02d.%04x@%s>"

/* The number of digits in a [+-]dddd time zone */

#define CHARS_IN_ZONE		5

/* The base day of the week (ie. that on Jan 1 1970) */

#define BASE_WEEKDAY		4

/* The message to return for an erroneous date */

#define BAD_MESSAGE		"unknown date"

/****************************************************************************/
