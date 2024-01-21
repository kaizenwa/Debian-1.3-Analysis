/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:
 *
 *	This module defines the preferences system
 */

#ifndef _preferences_h_
#define _preferences_h_

#define PREFSFILENAME	".xtrojka.prefs.bin"
#define DOLLAR_HOME	"HOME"

#define DEF_SPEED		5
#define DEF_IS_SCORE_STATUS	0
#define DEF_IS_AUTO_POSITION	0
#define DEF_IS_WIZARD		0
#define DEF_IS_SLICK		1

typedef struct {
	int speed;
	flag wizard;
	flag slick;
} PREFS;


/*
 *	function prototypes
 */
void init_preferences(void);
void read_prefs(void);
void write_prefs(void);
void create_prefsfile(void);
void set_default_prefs(void);

#endif /* _preferences_h_ */

