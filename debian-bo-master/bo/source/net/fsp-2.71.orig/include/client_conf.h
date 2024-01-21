#ifndef _FSP_CLIENT_CONF_H_
#define _FSP_CLIENT_CONF_H_

/****************************************************************************
 * Set this to point to the system wide default .fsp_prof file.             *
 * This file is used by fhostcmd to semi-automate the setting of            *
 * environment variable to ease the use of fsp                              *
 * It is only checked if neither ./.fsp_prof nor ~/.fsp_prof exist          *
 ****************************************************************************/
#ifdef VMS
#define FSPRC "sys$common:fsp_prof"
#else
#define FSPRC "/usr/local/fsp/fsp_prof"
#endif
/****************************************************************************
 * Define the following if you want the client programs to time out and     *
 * abort after a certain period (period is settable via an environment      *
 * variable.  See the INFO, client man pages, and ChangeLog files for       *
 * details                                                                  *
 ****************************************************************************/
#define CLIENT_TIMEOUT 1

/****************************************************************************
 * Define the following if you want fhostcmd to attempt to perform name     *
 * lookup on numeric host and numeric lookup on named hosts                 *
 ****************************************************************************/
#define HOST_LOOKUP 1

/****************************************************************************
 * The following code tries to set the file locking mechanism to the one    *
 * best suited for your system.  This should only be changed if the auto    *
 * configuration code fails and it doesn't compile.  That sort of bug       *
 * should also be immediately reported to the maintainers listed in the     *
 * INFO file                                                                *
 ****************************************************************************/
#define KEY_PREFIX "/usr/tmp/.FL"
/* #define USE_SHAREMEM_AND_LOCKF 1*/
/* #define USE_FLOCK 1 */
/* #define USE_LOCKF 1 */
/* #define NOLOCKING 1 */
#ifdef HAVE_SHMEM
#ifdef HAVE_LOCKF
#define USE_SHAREMEM_AND_LOCKF 1
#else
#ifdef HAVE_FLOCK
#define USE_FLOCK 1
#else
#define NOLOCKING 1
#endif /* HAVE_FLOCK */
#endif /* HAVE_LOCKF */
#else
#ifdef HAVE_LOCKF
#define USE_LOCKF 1
#else
#ifdef HAVE_FLOCK
#define USE_FLOCK 1
#else 
#define NOLOCKING 1
#endif /* HAVE_FLOCK */
#endif /* HAVE_LOCKF */
#endif /* HAVE_SHMEM */

#endif /* _FSP_CLIENT_CONF_H_ */
