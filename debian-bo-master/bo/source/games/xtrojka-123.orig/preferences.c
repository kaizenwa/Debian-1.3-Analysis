/*
 *	xtrojka (c) 1994,1995,1996 Maarten Los
 *
 *	#include "COPYRIGHT"	
 *
 *	created:	26.xi.1995
 *	modified:	04.iv.1996
 *
 *	This module reads and writes the preferences
 */

#include "debug.h"
#include "tr_core.h"

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "xtrojka.h"
#include "preferences.h"

extern flag is_wizard;
extern flag is_slick;

extern int starting_speed;
extern int speed;
extern char *getenv();

PREFS prefs;

char prefsfile[300];



void init_preferences(void)
{
	char *home;

	DEBUG("preferences.c", "init_preferences")

	/* try to read the prefs from the file */
	if((home = getenv(DOLLAR_HOME)) == NULL) {
		/*
		 *	we cannot get $HOME, so use some vague tmp-file
		 */
		sprintf(prefsfile,"/tmp/%s", PREFSFILENAME);
		return;
	}
	sprintf(prefsfile,"%s/%s", home, PREFSFILENAME);

	read_prefs();
}


void read_prefs(void)
{
	int fd;

	DEBUG("preferences.c", "read_prefs")

	/*
	 *	set the default preferences
	 */
	set_default_prefs();

	if((fd = open(prefsfile, O_RDONLY)) < 0) {
		create_prefsfile();
		write_prefs();
		return;
	} else
	if(read(fd, &prefs, sizeof(prefs)) < 0) {
		fprintf(stderr,"%s\n", app_data.wstr_read_prefs);
		return;
	}
	
	is_wizard = prefs.wizard;
	is_slick = prefs.slick;
	starting_speed = speed = prefs.speed;

	if(fd >=0)
		close(fd);
}


void write_prefs(void)
{
	int fd;

	DEBUG("preferences.c", "write_prefs")

	/* try to write the prefs to the prefsfile */
	
	if((fd = open(prefsfile, O_WRONLY)) < 0)
		fprintf(stderr,"%s\n", app_data.wstr_open_prefs);

	prefs.wizard = is_wizard;
	prefs.slick = is_slick;
	prefs.speed = speed;

	if(write(fd, &prefs, sizeof(prefs)) < 0)
		fprintf(stderr,"%s\n", app_data.wstr_write_prefs);
}


void create_prefsfile(void)
{
	int fd;
	int oldumask;

	oldumask = umask(0);

	DEBUG("preferences.c", "create_pefsfile")

	if((fd = creat(prefsfile, 0666)) < 0)
		fprintf(stderr,"%s\n", app_data.wstr_create_prefs);

	if(fd >= 0)
		close(fd);
	
	umask(oldumask);
}


void set_default_prefs(void)
{
	DEBUG("preferences.c", "set_default_prefs")

	is_wizard = DEF_IS_WIZARD;
	is_slick = DEF_IS_SLICK;
	starting_speed = speed = DEF_SPEED;
}

