/* "database.c" copyright 1994 thomas insel */

#include <stdio.h>
#include <string.h>
#include <pwd.h>
#include <stdlib.h>

#include "database.h"

static char rcsid[] = "$Id: database.c,v 1.1 1994/06/04 19:32:55 tinsel Exp $";

cd_t *
read_db(char *tracks_buffer, char usedb) {

   /* given "tracks" line in tracks_buffer, search databases for a
    * corresponding entry, return cdname/artist/tracks information
    *
    * if the disc is listed more than once, this routine will return
    * the last entry for the disc from the first file that has any
    * entry for it.
    */

    char *path_env = getenv("CDTOOLDBPATH");
    char *path_bit;
    char dbpath_count=1, cur_file=0, found_it=0;
    struct passwd *prec = getpwuid(getuid());
    cd_t *temp_cd = (cd_t *)malloc(sizeof(cd_t));
    char dbpath[10][255];

    if (!usedb) {
        free ((void *)temp_cd);
        return temp_cd;
    }

    /* first, put default file in search path */
    strcpy(dbpath[0], prec->pw_dir);
    strcat(dbpath[0], "/.cdtooldb");

    /* read search path from environment */
    if (path_env != NULL)
        while (dbpath_count < 9 &&
                    (path_bit=strsep(&path_env, ":\0")) != NULL)
            strcpy(dbpath[dbpath_count++], path_bit);

    /* check files in search path until the disc's entry is found */
    while (!found_it && cur_file < dbpath_count) {
        char read_buffer[2048], cur_track=0, printing=0;
        FILE *fred = fopen(dbpath[cur_file], "r");
        cur_file++;

        while (fred != NULL && !feof(fred) ) {
            fgets(read_buffer, 2048, fred);
            if (strcmp(read_buffer, tracks_buffer) == 0) {
                printing = 1;
                found_it = 1;
            } else if (strncmp(read_buffer, "tracks ", 7) == 0) {
                printing = 0;
            } else if (printing) {
                if (strncmp(read_buffer,"track ",6) == 0)
                    strncpy(temp_cd->track_names[cur_track++], 
                                                  &read_buffer[6],99);
                else if (strncmp(read_buffer,"cdname ",7) == 0)
                    strncpy(temp_cd->cdname, &read_buffer[7],99);
                else if (strncmp(read_buffer,"artist ",7) == 0)
                    strncpy(temp_cd->artist, &read_buffer[7],99);
            }
        }

        fclose(fred);
    } /*while*/
    return temp_cd;
}
