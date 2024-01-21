
#ifndef PWDB_CONFIG_H
#define PWDB_CONFIG_H

#define PWDB_CONF                "/etc/pwdb.conf"

/* 
 * how long to try to lock the database for updates/deletions
 */
#define PWDB_LOCK_TIME            10

/*
 * STANDARD PASSWD FILE
 */
#define __PWDB_PASSWD_FILE        "/etc/passwd"
#define __PWDB_GROUP_FILE        "/etc/group"

/*
 * SHADOW PASSWD FILE
 */
#define __PWDB_SHADOW_FILE        "/etc/shadow"
#define __PWDB_SGROUP_FILE        "/etc/gshadow"
#define __PWDB_SHADOW_TOKEN       "x"

/*
 * RADIUS
 */

/* 
 * Where the radius configuration files are kept
 */
#define __PWDB_RADIUS_PATH        "/etc/raddb"    
/*
 * The name of the file containing the RADIUS server name
 */
#define __PWDB_RADIUS_SERVER    "server"

#endif /* PWDB_CONFIG_H */
