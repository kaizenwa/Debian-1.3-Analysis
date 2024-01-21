/*
 * USERLIST
 * Show user idletime
 */

#include "userlist.h"
#include "proto.h"

#include <sys/stat.h>
#include <unistd.h>
#include <time.h>

void show_idle(char *tty)
{
    struct stat buf;
    time_t cur_time;
    long diff_time;
    int min, hour, day;
    char idledisp[80];
    char dev_file[80];

    bzero(idledisp, 80);
    bzero(dev_file, 80);

    sprintf(dev_file, "/dev/%s", tty);

    stat((char *) dev_file, &buf);
    cur_time = time(NULL);

    diff_time = (long) cur_time - (long) buf.st_mtime;

    min = hour = day = 0;

    if (diff_time > 86400)
	day = hour = 1;
    else if (diff_time > 3600)
	hour = min = 1;
    else if (diff_time > 59)
	min = 1;

    if (day) {
	day = diff_time / 86400;
	diff_time -= day * 86400;
    }

    if (hour) {
	hour = diff_time / 3600;
	diff_time -= hour * 3600;
    }

    if (min) {
	min = diff_time / 60;
	diff_time -= min * 60;
    }

    idledisp[0] = 0;

    if (day) {
	sprintf(idledisp, "%1dd ", day);
    }

    if (hour || min) {
	if (day)
	    sprintf(idledisp, "%02d:%02d", hour, min);
	else
	    sprintf(idledisp, "%d:%02d", hour, min);
    }

    printf("%8.8s ", idledisp);
}
