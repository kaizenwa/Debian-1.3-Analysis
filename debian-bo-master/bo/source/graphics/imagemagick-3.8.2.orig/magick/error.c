/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                      EEEEE  RRRR   RRRR   OOO   RRRR                        %
%                      E      R   R  R   R O   O  R   R                       %
%                      EEE    RRRR   RRRR  O   O  RRRR                        %
%                      E      R R    R R   O   O  R R                         %
%                      EEEEE  R  R   R  R   OOO   R  R                        %
%                                                                             %
%                                                                             %
%                         ImageMagick Error Routines                          %
%                                                                             %
%                                                                             %
%                                                                             %
%                              Software Design                                %
%                                John Cristy                                  %
%                                 July 1993                                   %
%                                                                             %
%                                                                             %
%  Copyright 1997 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission to use, copy, modify, distribute, and sell this software and    %
%  its documentation for any purpose is hereby granted without fee,           %
%  provided that the above Copyright notice appear in all copies and that     %
%  both that Copyright notice and this permission notice appear in            %
%  supporting documentation, and that the name of E. I. du Pont de Nemours    %
%  and Company not be used in advertising or publicity pertaining to          %
%  distribution of the software without specific, written prior               %
%  permission.  E. I. du Pont de Nemours and Company makes no representations %
%  about the suitability of this software for any purpose.  It is provided    %
%  "as is" without express or implied warranty.                               %
%                                                                             %
%  E. I. du Pont de Nemours and Company disclaims all warranties with regard  %
%  to this software, including all implied warranties of merchantability      %
%  and fitness, in no event shall E. I. du Pont de Nemours and Company be     %
%  liable for any special, indirect or consequential damages or any           %
%  damages whatsoever resulting from loss of use, data or profits, whether    %
%  in an action of contract, negligence or other tortious action, arising     %
%  out of or in connection with the use or performance of this software.      %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include <errno.h>

/*
  Define declarations.
*/
#if defined(sun) && !defined(SVR4)
#ifndef strerror
#define strerror(n) \
  (((n) >= 0 && (n) < sys_nerr) ? sys_errlist[n] : "unknown error")

extern char
  *sys_errlist[];

extern int
  sys_nerr;
#endif
#endif

/*
  Forward declaraations.
*/
static void
  DefaultErrorHandler(const char *,const char *),
  DefaultWarningHandler(const char *,const char *);

/*
  Global declarations.
*/
static ErrorHandler
  error_handler = DefaultErrorHandler,
  warning_handler = DefaultWarningHandler;

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e f a u l t E r r o r H a n d l e r                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function DefaultErrorHandler displays an error message and then terminates
%  the program.
%
%  The format of the DefaultErrorHandler routine is:
%
%      DefaultErrorHandler(message,qualifier)
%
%  A description of each parameter follows:
%
%    o message: Specifies the message to display before terminating the
%      program.
%
%    o qualifier: Specifies any qualifier to the message.
%
%
*/
static void DefaultErrorHandler(const char *message,const char *qualifier)
{
  if (message == (char *) NULL)
    exit(1);
  (void) fprintf(stderr,"%s: %s",client_name,message);
  if (qualifier != (char *) NULL)
    (void) fprintf(stderr," (%s)",qualifier);
  if (errno)
    (void) fprintf(stderr," [%s]",strerror(errno));
  errno=0;
  (void) fprintf(stderr,".\n");
  exit(1);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e f a u l t W a r n i n g H a n d l e r                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function DefaultWarningHandler displays a warning message.
%
%  The format of the DefaultWarningHandler routine is:
%
%      DefaultWarningHandler(message,qualifier)
%
%  A description of each parameter follows:
%
%    o message: Specifies the message to display before terminating the
%      program.
%
%    o qualifier: Specifies any qualifier to the message.
%
%
*/
static void DefaultWarningHandler(const char *message,const char *qualifier)
{
  if (message == (char *) NULL)
    return;
  (void) fprintf(stderr,"%s: %s",client_name,message);
  if (qualifier != (char *) NULL)
    (void) fprintf(stderr," (%s)",qualifier);
  if (errno)
    (void) fprintf(stderr," [%s]",strerror(errno));
  errno=0;
  (void) fprintf(stderr,".\n");
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E r r o r                                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function Error calls the error handler routines with an error message.
%
%  The format of the Error routine is:
%
%      Error(message,qualifier)
%
%  A description of each parameter follows:
%
%    o message: Specifies the message to display before terminating the
%      program.
%
%    o qualifier: Specifies any qualifier to the message.
%
%
*/
Export void Error(const char *message,const char *qualifier)
{
  if (error_handler != (ErrorHandler) NULL)
    (*error_handler)(message,qualifier);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S e t E r r o r H a n d l e r                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function SetErrorHandler sets the error handler to the specified routine
%  and returns the previous error handler.
%
%  The format of the SetErrorHandler routine is:
%
%      previous_handler=SetErrorHandler(handler)
%
%  A description of each parameter follows:
%
%    o handler: Specifies a pointer to a routine to handle errors.
%
%
*/
Export ErrorHandler SetErrorHandler(ErrorHandler handler)
{
  ErrorHandler
    previous_handler;

  previous_handler=error_handler;
  error_handler=handler;
  return(previous_handler);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S e t W a r n i n g H a n d l e r                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function SetWarningHandler sets the warning handler to the specified routine
%  and returns the previous warning handler.
%
%  The format of the SetWarningHandler routine is:
%
%      previous_handler=SetWarningHandler(handler)
%
%  A description of each parameter follows:
%
%    o handler: Specifies a pointer to a routine to handle warnings.
%
%
*/
Export ErrorHandler SetWarningHandler(ErrorHandler handler)
{
  ErrorHandler
    previous_handler;

  previous_handler=error_handler;
  warning_handler=handler;
  return(previous_handler);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W a r n i n g                                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function Warning calls the warning handler routines with an warning message.
%
%  The format of the Warning routine is:
%
%      Warning(message,qualifier)
%
%  A description of each parameter follows:
%
%    o message: Specifies the message to display before terminating the
%      program.
%
%    o qualifier: Specifies any qualifier to the message.
%
%
*/
Export void Warning(const char *message,const char *qualifier)
{
  if (warning_handler != (ErrorHandler) NULL)
    (*warning_handler)(message,qualifier);
}
