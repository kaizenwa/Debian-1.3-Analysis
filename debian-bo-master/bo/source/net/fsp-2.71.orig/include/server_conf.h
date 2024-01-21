#ifndef _FSP_SERVER_CONF_H_
#define _FSP_SERVER_CONF_H_

/****************************************************************************
 * Set this to the location of the fspd.conf file on your system            *
 ****************************************************************************/

#ifdef VMS
#define CONF_FILE "sys$login:fspd.conf"
#else
#define CONF_FILE "/usr/fsp/fspd.conf"
#endif
/****************************************************************************
 * Set this value to the maximum number of open files you wish your system  *
 * to keep around at any given time.  The smaller this number is, the more  *
 * likely the server is to be opening and closing files, but the less file  *
 * descriptors need to be taken up by the server itself                     *
 * Five seems to work reasonably well on my system                          *
 ****************************************************************************/
#define FSP_FILE_CACHE 5

/****************************************************************************
 * If the server machine supports files names longer than 14 characters     *
 * long_file_names will be defined.  If the machine actually has some other *
 * file name length limit, change the value below to reflect that           *
 * For most systems, this will NOT need to be changed.                      *
 ****************************************************************************/
#ifdef HAVE_LONG_FILE_NAMES
#define FILE_NAME_LIMIT 99999
#else
#define FILE_NAME_LIMIT 14
#endif

/****************************************************************************
 * DIR_CACHE_COUNT should be set to contain the max number of listings you  *
 * want held in each file in the cache_dir                                  *
 * For most systems, this will NOT need to be changed.                      *
 ****************************************************************************/
#define MAX_DIR_CACHE_COUNT 32

/****************************************************************************
 * If for some reason, your signal handlers don't reinstall automatically,  *
 * then undefine the following.  SYS_V systems will most likely need this   *
 * to be undefined, while bsd systems should be fine.                       *
 ****************************************************************************/
#define RELIABLE_SIGNALS

#endif /* _FSP_SERVER_CONF_H_ */
