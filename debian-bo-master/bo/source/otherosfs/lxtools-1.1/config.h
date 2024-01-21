/* --------------------------------------------------------------------
   Project: LX tools
   Module:  config.h
   Author:  A. Garzotto
   Started: 30. Nov. 95
   Last Modified: 11. Dec. 95
   Subject: configuration specific  include file
   -------------------------------------------------------------------- */

/* This file contains constants that are system specific */


/* Set this to reflect the base name of the serial port devices */
/* (on a Sun, this is "/dev/tty") */

#define SERIAL_NAME "/dev/cua"


/* Set this to reflect the name of first serial device */
/* It will be concatenated with SERIAL_NAME to get the full */
/* name of the device. (on a Sun, this is 'a') */

#define SERIAL_FIRST '0'


/* Set this to reflect the stty command on your system */
/* %ld will be replaced by the baud rate and %s by the device name */
/* note that on some systems (e.g. Sun) stty works on the */
/* standard output and not on the standard input. Thus, it */
/* would be "stty sane raw pass8 %ld >%s". */
/* For HP-UX, try "stty cs8 raw min 64 time 1 %ld <%s" */

#define STTY_COMMAND "stty sane raw pass8 %ld <%s"

/* Set this to the desired default baud rate */

#define DEF_BAUD 38400


/* Set this to the number of seconds after which the connection */
/* is assumed to be broken */

#define TIMEOUT_NORMAL 20


/* Set this to the number of seconds after which we can assume */
/* connection did not work while trying to automagically       */
/* find the correct baud rate */

#define TIMEOUT_AUTO   1

/* Define this if files copied from the LX to the Unix system */
/* should have lower case names */

#define LOWERCASE

/* This is the version number */

#define VERSION "1.1"
