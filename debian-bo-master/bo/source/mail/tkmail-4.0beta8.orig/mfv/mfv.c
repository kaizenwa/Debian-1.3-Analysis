static char rcsid[] = "$Header$";
/*
 * mfv - adds commands to Tcl7.4+ interpreter to manage mailx, MMDF, and MH
 * 	mailfolders. The new commands include
 *
 *	mfv_open [<switches>] <folderName> - opens file <folderName> as a BSD 
 *		or MMDF mailfolder. Returns a folder ID to be used in further 
 *		manipulations of the folder. An error will result if <folder> 
 *		is unreadable, not in BSD or MMDF format type or already opened.
 *		If <folderName> doesn't exist, a zero length folder is created
 *		and marked as UNKNOWN type. This will be changed to BSD or
 *		MMDF at the first opportunity the package has to decide (e.g
 *		a message is appended). The type can be set explicitly with
 *		the folder "type" command. If the mfv package is put in a
 *		situation where it is forced to decide without evidence, then
 *		the setting of 'defaulttype' from the mfv_set command is used.
 *
 *		MH folders are supported though currently all MH support files
 *		are ignored (i.e. .mh_profile, .mh_sequencs, etc ) and symbolicly
 *		linked messages are not handled well.
 *
 *		If the "-virtual" switch is given, a virtual folder is created
 *		with the name given as <folderName>. Virtual folders hold
 *		messages from several different real folders (that were
 *		gathered, for example, in a search operation) and can in most
 *		respects be treated like any other folder. Messages are added
 *		to it using the folder 'linkto' command (see below).
 *
 *	mfv_close <folderID> - closes folder associated with <folderID>.
 *		Any pending deletions or "mark (un)read" requests are lost.
 *		Returns an empty list.
 *
 *	mfv_set <options> - manipulates global state of the mfv package.
 *	Options may be:
 *
 *	    mfv_set defaulttype [<formatType>]
 *		Sets the default format type for a folder that will be
 *		used for a folder with UNKNOWN type when a choice is forced
 *		(e.g. a request to append a message to the folder). If
 *		<formatType> is not given, the current default is returned.
 *
 *	    mfv_set lock dotlock [<pathName>]
 *		Sets the pathname to the dotlock executable which is used
 *		to create *.lock files to lock folders. It is necessary that
 *		it be an external program since often it must be setuid or
 *		setgid and such permissions are very insecure for a program
 *		with an embedded Tcl interpeter. With no <pathName> argument,
 *		the current setting is returned.
 *
 *	    mfv_set lock retries [<count>]
 *		Sets the number of times to retry locking a folder if a
 *		previous attempt fails because it is already locked. A
 *		negative value will cause it to retry forever. With no
 *		<count> argument, the current setting is returned.
 *
 *	    mfv_set lock timeout [<seconds>]
 *		Sets the number of seconds between retry attemtps. With
 *		no <seconds> argument, the current setting is returned.
 *
 *	    mfv_set mailspool [<directory>]
 *		Set which directory where the users system mail inbox is.
 *		If no <directory> is specified, returns the current setting.
 *
 *	    mfv_set noempty [<boolean>]
 *		Set whether empty folders are deleted on close.
 *
 *	    mfv_set keyprefix [<prefixString>]
 *		Sets the prefix used for folderIDs. If no <prefixString>
 *		is specified, returns the current prefix.
 *
 *	    mfv_set retain [<fieldList>]
 *		Sets the list of fields that will be retained in results
 *		of the '<folder> message headers' command. All other
 *		fields will be stripped out. With no <fieldList> argument,
 *		returns the current list. If this command is called, any
 *		list set by the 'mfv_set strip' command is emptied.
 *
 * 	   mfv_set strip [<fieldList>]
 *		Sets the list of fields that will be stripped from results of
 *		the '<folder> message headers' command. With no <fieldList>
 *		argument, returns the current list. If this command is
 *		called, any list set by the 'mfv_set retain' command is
 *		emptied.
 *
 *	    mfv_set stupidformat [<boolean>]
 *		Set whether the '>' character is allowed in front of
 *		the BSD SM-from line which signals new messages.
 *
 *	    mfv_set sumformat <formatString>
 *		Sets the format which the '<folderID> message summary'
 *		command uses to report. The sprintf-style placeholders are:
 *
 *		Code	Style	Meaning
 *		----    -----   -----------------------------------------
 *		 D	 s	Contents of Date field
 *		 F	 s	Fullname of sender
 *		 S	 s	Status of message: 'N', 'U' or empty
 *		 c	 d	Number of characters in message
 *		 d	 d	Day of month message received
 *		 f	 s	Sendmail from address
 *		 h	 s	Time of day received in HH:MM:SS
 *		 i	 s	Contents of Message-Id field
 *		 l	 d	Number of lines in message
 *		 m	 s	Month received
 *		 n	 d	message number
 *		 w	 s	Day of week received
 *		 y	 d	Year received
 *
 *		The 'Style' specifies with what sprintf placeholder the
 *		code will be processed: string or decimal. The default
 *		format is "%1S%3n %-15.15F  %3m %2d %5h %4l  %-20.20s"
 *
 *		Currently, there is a maximum of 20 placeholders.
 *
 *	    mfv_set tmpdir [<directory>]
 *		Sets the default directory used for temporary files. If no 
 *		<directory> is specified, returns the current setting.
 *
 *	mfv_util <options> - various mail related utilities
 *	Options may be:
 *
 *	    mfv_util boundary
 *		Returns a string suitable for a MIME multipart boundary.
 *
 *	    mfv_util encode <encodeType> <inFileID> <outFileID>
 *		Given file handles <inFileID> and <outFileID> as returned
 *		from the Tcl open command, encodes the input file into
 *		the output file according to <encodeType>. Only base64 and
 *		quoted-printable supported currently.
 *
 *	    mfv_util folderid <fileName>
 *		Returns folderID of <fileName> if open, empty otherwise.
 *
 *	    mfv_util fullpath <fileName>
 *		Returns full pathname of <fileName>.
 *
 *	    mfv_util home
 *		Returns current user's home directory.
 *
 *	    mfv_util list
 *		Returns list of folderIDs of all open folders.
 *
 *	    mfv_util lockerror
 *		Returns details of last lock error (when folder lock
 *		or unlock command returns -1).
 *
 *	    mfv_util mesgid
 *		Returns a string suitable for use as the contents of
 *		the Message-Id header field.
 *
 *	    mfv_util simplify [-stripq] [-stripw] <fieldStr>
 *		Returns <fieldStr> stripped of header comments. If the
 *		-stripw option is given, extraneous spaces are also
 *		stripped. If the -stripq option is given, quotes are
 *		also stripped.
 *
 *	    mfv_util tmpfile [<prefix> [<directory>]]
 *		Returns a unique filename for use as a temporary file. If
 *		given, the filename will start with <prefix> and be in
 *		<directory>.
 *
 *	    mfv_util user
 *		Returns login name of current user.
 *
 *	    mfv_util version
 *		Returns current version of Mfv package.
 *
 * 	Each open folder's folder ID is also a Tcl command with the
 *	following options:
 *
 *	<folderID> append <filename>
 *		Appends the messages in folder <filename> to <folder>.
 *		Currently only works if both are BSD folders or both
 *		are MMDF folders. The folder is locked during operation.
 *
 *	<folderID> check
 *		Check for appended messages and adds them into the <folderID>
 *		database. If folder corruption is found, <folderID> is
 *		automatically closed. You should always check for the
 *		existance of the <folderID> command after an error. Returns
 *		the number of added messages. NOTE: doing a 'check' will also
 *		imply a 'unlock' if the folder is locked. This is due to some
 *	        problems I am trying to work out with the AFS file system so
 *		might change or be optional in the future.
 *
 *	<folderID> info count
 *		Returns number of messages in folder.
 *
 *	<folderID> info deleted
 *		Returns list of messages in folder that are marked for
 *		deletion.
 *
 *	<folderID> info directory
 *		Returns directory in which folder is located.
 *
 *	<folderID> info max
 *		Returns highest message number in folder. This may
 *		be higher than the count in MH folders and might not
 *		even exist in Virtual folders.
 *
 *	<folderID> info name
 *		Returns filename of folder.
 *
 *	<folderID> info new
 *		Returns list of message numbers of new messages.
 *
 *	<folderID> info readonly
 *		Returns 1 if folder is readonly, 0 otherwise.
 *
 *	<folderID> info type [<formatType>]
 *		Sets the format type of <folderID>. This is allowed only
 *		if the folder is empty. Possible values for <formatType>
 *		are unknown, bsd, and mmdf. If not given, the current
 *		type is returned.
 *
 *	<folderID> lock
 *		Locks the folder according to the 'lock method' setting
 *		above. Returns 0 on success, -1 on error, and -2 if folder
 *		is already locked by another process.
 *
 *	<folderID> message body <mesgnum>
 *		Returns body of message. Will also mark message as 
 *		read if it isn't already.
 *
 *	<folderID> message chars <mesgnum>
 *		Returns number of characters in message <mesgnum>.
 *
 *	<folderID> message contents <mesgnum>
 *		Returns complete contents of message including any special
 *		text, such as the ^A^A^A^A lines for MMDF format mail.  For
 *		display to the user, the 'headers' and 'body' options to
 *		'message' should be used. Will also mark message as read if
 *		it isn't already.
 *
 *	<folderID> message delete <mesgnum>
 *		Marks message <mesgnum> for deletion. Returns empty.
 *		Equivalent to "<folderID> message flag <mesgnum> delete 1".
 *
 *	<folderID> message exists <mesgnum>
 *		Returns 1 if message <mesgnum> exists, 0 otherwise.
 *
 *	<folderID> message field <mesgnum> ?<headerField>?
 *		Returns contents of field <headerField> from headers for
 *		message <mesgnum>. If no <headerField> argument is given
 *		a list of available fields is returned.
 *
 *	<folderID> message flag <mesgnum> ?<flagName>? ?<boolean>? 
 *		With both <flagName> and <boolean> specified, sets
 *		given flag for message <mesgnum> to given boolean
 *		value.  With no <boolean> argument, returns value of
 *		<flagName> for message. With neither, returns list of
 *		flags and their values for <mesgnum>.
 *
 *	<folderID> message headers <mesgnum> [full]
 *		Returns headers from message <mesgnum>.
 *
 *	<folderID> message lines <mesgnum>
 *		Returns number of lines in message <mesgnum>.
 *
 *	<folderID> message links <mesgnum>
 *		Returns list of folderID's of all folders containing message
 *		<mesgnum>. This will be the "real" folder and zero or more
 *		virtual folders.
 *
 *	<folderID> message linkto <mesgnum> <virtualFolderID>
 *		Appends message <mesgnum> to the virtual folder specified
 *		by <virtualFolderId>.
 *
 *	<folderID> message mimelist <mesgnum>
 *		Returns a list that describes the complete MIME structure
 *		of the message. The list has three or four elements. The
 *		first three are the MIME part number, content type, and
 *		subtype respectively. If it is of type "multipart", then
 *		the fourth element will be a sublist of the mime parts it
 *		contains were each element has the same {num type subtype
 *		subparts} structure.  The MIME part number is used in the
 *		'mimepart' command described below.
 *
 *	<folderID> message mimepart <mesgnum> body <partnum> [<decodeType>]
 *		Returns the body of the MIME part <partnum> in
 *		message <mesgnum>. If <decodeType> is given, the text will
 *		be decoded according to the type.  If the type is not
 *		recognized, an error is generated.
 *
 *	<folderID> message mimepart <mesgnum> description <partnum>
 *		Returns the contents of the Content-Description field of
 *		the MIME part <partnum> in message <mesgnum>.
 *
 *	<folderID> message mimepart <mesgnum> disposition <partnum>
 *		Returns the contents of the Content-Disposition field of
 *		the MIME part <partnum> in message <mesgnum>.
 *
 *	<folderID> message mimepart <mesgnum> encoding <partnum>
 *		Returns the encoding scheme used to encode the body of the
 *		MIME part <partnum> in message <mesgnum> as specified by
 *		the Content-Transfer-Encoding field.
 *
 *	<folderID> message mimepart <mesgnum> id <partnum>
 *		Returns the content ID used to identify the body of the
 *		MIME part <partnum> in message <mesgnum> as specified by
 *		the Content-Id field.
 *
 *	<folderID> message mimepart <mesgnum> parameter <partnum> [<param>]
 *		With no <paramKey> argument, returns a list of the content
 *		parameters available for the MIME part <partnum> in message
 *		<mesgnum> as parsed from the Content-Type field. If <param>
 *		is given, the value of the parameter is returned.
 *
 *	<folderID> message mimepart <mesgnum> subtype <partnum>
 *		Returns the content subtype of MIME part <partnum> in message
 *		<mesgnum> as parsed from the Content-Type field.
 *
 *	<folderID> message mimepart <mesgnum> type <partnum>
 *		Returns the content type of MIME part <partnum> in message
 *		<mesgnum> as parsed from the Content-Type field.
 *
 *	<folderID> message mimepart <mesgnum> write <partnum> <fileName> [<decodeType>]
 *		Writes the body of MIME part <partnum> in message <mesgnum>
 *		to the file <fileName>. Returns empty. If <decodeType> is
 *		given, the text will be decoded according to the type.  If
 *		the type is not recognized, an error is generated. If
 *		<fileName> is the empty string, a temporary file is produced
 *		which will be deleted when the folder is closed. The filename
 *		written to is returned. Subsequent calls for the same
 *		mimepart with an empty filename will return the name of the
 *		first temporary file name written so one need not worry about
 *		writing more than once. DO NOT DELETE THE TEMPORARY FILE IN
 *		YOUR CODE.
 *
 *	<folderID> message received <mesgnum>
 *		Returns time message <mesgnum> was received in GMT seconds
 *		as parsed from the sendmail separator line.
 *
 *	<folderID> message replies <mesgnum>
 *		Returns list of messages numbers that are replies to <mesgnum>
 *		according to the Messsage-Id and In-Reply-To fields. Returns
 *		empty list if no replies are found int the folder.
 *
 *	<folderID> message replyto <mesgnum>
 *		Returns message number to which <mesgnum> is a reply to
 *		according to the Messsage-Id and In-Reply-To fields. Returns
 *		empty if there in no In-Reply-To header or the no message has
 *		the specified Message-Id in the folder.
 *
 *	<folderID> message sent <mesgnum>
 *		Returns time message <mesgnum> was sent in GMT seconds
 *		as parsed from the Date header field. If this field can't
 *		be correctly parsed, time received is returned instead.
 *
 *	<folderID> message summary <mesgnum>
 *		Returns a summary string for  message <mesgnum> according
 *		to the sumformat set with 'mfv_set sumformat' command.
 *
 *	<folderID> message undelete <mesgnum>
 *		Mark message as undeleted. Returns empty.
 *		Equivalent to "<folderID> message flag <mesgnum> delete 0".
 *
 *	<folderID> message write <mesgnum> <folderName> 
 *		Append message <mesgnum> to <folderName> handling any folder
 *		format type conversion necessary. If <folderName> does not
 *		exist or is empty, it will be created with the format type
 *		specified with 'mfv_set defaulttype'. If <folderName> is
 *		currently open with 'mfv_open', the copy will work, but a
 *		'<folderID> check' will need to be done on the target folder
 *		for the new message to be recognized. <folderName> is locked
 *		during operation. Returns empty.
 *
 *	<folderID> save [-sorted <sortKeys>] [-type <formatType>] [<fileName>]
 *		Folder is saved. Any messages marked for deletion will be
 *		removed, unread messages which have had the contents
 *		retrieved will be marked read, and new messages will be
 *		marked as unread. If the -sorted switch is given, messages
 *		will be saved in sorted order according to <sortKeys>.  If
 *		the -type switch is given, the folder is saved with the given
 *		format type. If <fileName> is given, the folder is saved to
 *		<fileName>. However, <folderID> will still remains associated
 *		with the original opened folder. An error will result if
 *		<fileName> is an already opened folder (expect the original
 *		folder itself). An error will result if any file system
 *		errors occur. Sometimes this might imply that the original
 *		file has become corrupted or inaccesible. If this occurs,
 *		then the <folderID> will be automatically closed. Therefore,
 *		it is a good idea to check for the existance of the
 *		<folderID> command if an error occurs trying to save.
 *
 *		The save command does a 'check' at the beginning for
 *		any new appended messages. If they exist, they will be
 *		parsed into the <folderID> database, but not included
 *		in the sort or marked with unread status. The save
 *		command returns the number of messages it finds
 *		appended just like the check command. Also, the folder
 *		is locked during the operation.
 *
 *	<folderID> search [-sorted <sortKeys>] [-messages <msglist>] <searchSpec>
 *		Returns message numbers of messages in <folderID> that
 *		satisfy <searchSpec>. If the -sorted switch is given, then
 *		the returned messages numbers are sorted according to
 *		<sortKeys>.  If the -messages switch is given, only the
 *		messages in <msglist> are searched. See below for the format
 *		of the search specification.
 *
 *	<folderID> sort [-messages <msglist>] <sortKeys> 
 *		Returns a list of message numbers sorting according to
 *		<sortKeys> which is a list of headers fields or the special
 *		keys 'normal', 'received', or 'sent'. Currently only the
 *		first two keys are used and any messages that match as
 *		equivalent will be sorted in message number order. If the
 *		-messages switch is given, only the messages in <msglist> are
 *		sorted.
 *
 *	<folderID> threads
 *		Returns a list of all the threads in the folder. Each element
 *		is a list of one or two elements. The first element is a
 *		message number. The optional second element is a list of the
 *		message numbers of replies to the message.
 *
 *	<folderID> unlock
 *		Unlocks the folder according to the 'lock method' setting
 *		above. Returns 0 on success, -1 on error.
 *
 *	<folderID> write <mesgList> <folderName> 
 *		Append messages in <mesgList> to <folderName> handling any folder
 *		format type conversion necessary. If <folderName> does not
 *		exist or is empty, it will be created with the format type
 *		specified with 'mfv_set defaulttype'. If <folderName> is
 *		currently open with 'mfv_open', the copy will work, but a
 *		'<folderID> check' will need to be done on the target folder
 *		for the new message to be recognized. <folderName> is locked
 *		during operation. Returns empty.
 *
 * FOLDER SEARCH FORMAT
 *
 *   A search spec has one of the following forms:
 *
 *      {body <op> <pattern> [<mods>]}
 *      	Searches the message body for <pattern> according to <op>.
 *
 *      {field <fieldName> <op> <pattern> [<mods>]}
 *      	Searches the contents of the header field <fieldName> for <pattern>
 *      	according to <op>.
 *
 *      {field <fieldName> <boolean>}
 *      	Returns <boolean> if message has a header field <fieldName> and
 *      	!<boolean> otherwise.
 *
 *      {flag <flagName> <boolean>}
 *      	Returns <boolean> if message has <flagName> set to true and
 *      	!<boolean> otherwise.
 *
 *      {header <op> <pattern> [<mods>]}
 *      	Searches the message headers for <pattern> according to <op>.
 *
 *      {message <op> <pattern> [<mods>]}
 *      	Searches the entire message for <pattern> according to <op>.
 *
 *   The <op> argument can be one of:
 *
 *      ==     must exactly match <pattern>
 *      !=     must not exactly match <pattern>
 *      =@     must contain a substring that exactly matches <pattern>
 *      !@     must not contain a substring that exactly matches <pattern>
 *      =~     must contain a substring that matches regexp <pattern>
 *      !~     must not contain a substring that matches regexp <pattern>
 *
 *   The optional <mods> argument can be one of:
 *
 *      i      do case-insensitive matching
 *      t      trim starting and ending space from text lines searched
 *
 *   The above specifications can be combined into a larger one in
 *   a single Tcl list with && (logical-AND) and || (logical-OR). For
 *   example: {{spec} && {{spec} || {spec}}}
 *
 *   Logical negation can be done by creating a two element list where
 *   the word "not" is the first element and the search spec the
 *   second. For example: {not {spec}}
 *
 *   Searches in message text (body and header) are done one line at a
 *   time so multiline patterns are not supported.
 *
 *   Examples:
 *
 *      {field mime-version == 1.0 t}
 *	{{field from =@ raines} && {field subject !@ tkmail i}}
 *      {not {{message =@ descent i} || {message =@ doom i}}}
 *      {body =~ {file[0-9]+}}
 *
 * NOTES
 *   Doesn't handle lines longer then 2048 chars. May core dump on them.
 *   Doesn't handle sym links in MH with any intelligence.
 *   Ignores MH special files (i.e. .mh_profile, .mh_sequences)
 *   Need to handle permisions on file opens and mkdir better.
 *   Need to check for NULL returns from Mfv_GetMesg
 *   My use of tmpFile could be dangerous
 *   TODO: go through tmpfile permissions carefully
 *
 * HISTORY
 *   raines - Nov 27, 1995: Created.
 *   See Changelog.
 *
 *-----------------------------------------------------------------------------
 * Copyright 1995-1996 Paul Raines
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies.  PAUL RAINES AND HIS EMPLOYERS MAKE NO REPRESENTATIONS
 * ABOUT THE ACCURACY OR SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE.
 * IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 *-----------------------------------------------------------------------------
 * Some parts for base64 and quotedprintable encoding are: Copyright
 * (c) 1991 Bell Communications Research, Inc. (Bellcore)
 *  
 * Permission to use, copy, modify, and distribute this material 
 * for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice and this permission notice 
 * appear in all copies, and that the name of Bellcore not be 
 * used in advertising or publicity pertaining to this 
 * material without the specific, prior written permission 
 * of an authorized representative of Bellcore.  BELLCORE 
 * MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
 * OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
 * WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
 *----------------------------------------------------------------------------- */

#include <stdio.h>
#include <sys/types.h>

#include <ctype.h>
#ifdef NO_STDLIB_H
# include "compat/stdlib.h"
#else
# include <stdlib.h>
#endif
#ifdef NO_STRING_H
# include "compat/string.h"
#else
# include <string.h>
#endif
#ifdef NO_UNISTD_H
# include "compat/unistd.h"
#else
# include <unistd.h>
#endif

#include <errno.h>
#include <pwd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>

#ifdef HAVE_GETHOSTNAME
# include <sys/param.h>
#endif
#include <sys/utsname.h>

#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#ifdef TM_IN_SYS_TIME
# if TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  include <sys/time.h>
# endif
#else
# include <time.h>
#endif

/* TODO: needed by LINUX but eventually need work around */
#define DIRENT_ILLEGAL_ACCESS 1

#ifdef NO_DIRENT_H
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#else
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#endif

/*  needed by gethostbyname */
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
# include <netdb.h>
#endif

/* Finally, get TCL prototypes and defines */
#include <tcl.h>

/*
 * Macro that behaves like strdup, only uses ckalloc.
 */
#define dbgstrdup(sourceStr) \
  (strcpy (ckalloc (strlen (sourceStr) + 1), sourceStr))
#define dbgfree(ptr) if (ptr) ckfree(ptr)
#define dbgalloc(size) ckalloc(size)

#ifdef MAC_TCL
static char dirsep = ':';
static char dirsepStr[] = ":";
#define CHDIR(d) chdir(strlen(d) ? (d) : ":")
#else
static char dirsep = '/';
static char dirsepStr[] = "/";
#define CHDIR(d) chdir(strlen(d) ? (d) : "/")
#endif

#ifndef UCHAR
#define UCHAR(c) ((unsigned char) (c))
#endif

#ifndef BUFSIZ
#define BUFSIZ 1024
#endif

#ifndef NAME_MAX
#define NAME_MAX 1024
#endif

#ifndef MIN
#define MIN(x,y) ((x)<(y)?(x):(y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y)?(x):(y))
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

/* string to store version of Mfv package */
#ifdef VERSIONSTR
static char version[] = VERSIONSTR;
#else
static char version[] = "Unknown";
#endif

static int debug = FALSE;

/* some useful string constants */
static char emptyString[] = "";
static char mmdfSep[] = "\001\001\001\001\n";
static char wrongNumArgs[] = "wrong # args: should be \"";

#define MAXLINE 2048
#define MAXFIELD 256

static char tmpLine[MAXLINE+1];
static char fmtLine[MAXLINE+1];
static char *keyPrefix = NULL;
static char defaultPrefix[] = "folder";

/* will always have enough room to sprintf largest folder name */
static char *tmpFile = NULL;
static int tmpFileSize = 0;

/* default place for temp files */
static char *tmpDir = NULL;

/* user system inbox directory*/
static char *mailSpool = NULL;
static char *spoolDirs[] = {
#ifdef MAILSPOOLDIR
  MAILSPOOLDIR,
#endif
  "/var/spool/mail",
  "/usr/spool/mail",
  "/var/mail",
  "/usr/mail",
  emptyString
};

/* whether to delete empty folders on close */
static int delEmpty = FALSE;
/* whether the > sign in front of sm-from line is supported */
static int stupidformat = FALSE;

#define MFV_OK		 0
#define MFV_INPUT_ERR	-1
#define MFV_OUTPUT_ERR	-2
#define MFV_PARSE_ERR   -3
#define MFV_ENCODE_ERR  -4

/* flags settable by user (MAX==9) */
#define MFV_OLD	      (1<<0)
#define MFV_READ      (1<<1)
#define MFV_DELETED   (1<<2)
#define MFV_ANSWERED  (1<<3)
#define MFV_FORWARDED (1<<4)
#define MFV_SAVED     (1<<5)

static char *flagNames[] = {
  "old",
  "read",
  "deleted",
  "answered",
  "forwarded",
  "saved",
  emptyString
};

/* flags for internal use (MAX==15) */
#define MFV_THREADED  (1<<10)

#define MFV_STATUS   (MFV_OLD | MFV_READ)
#define MFV_XSTATUS  (MFV_ANSWERED | MFV_FORWARDED)

#define MFV_UNKNOWN	0
#define MFV_VIRTUAL	1
#define MFV_BSD		2
#define MFV_MMDF	3
#define MFV_MH		4

static char *folderTypes[] = {
  "unknown",
  "virtual",
  "bsd",
  "mmdf",
  "mh",
  emptyString
};
static int defaultType = MFV_BSD;

/* locking */
int lock_retries = 3;
int lock_timeout = 5;
char *lock_err = emptyString;
char *dotlockprog = NULL;

/* token keys for parts of SM From line */
static char *smHeaders[] = {
  "sm-from",
  "sm-wday",
  "sm-month",
  "sm-mday",
  "sm-time",
  "sm-year",
  emptyString
};

/* Used for strip/retain filtering of headers */ 
static char **filterHeaders = NULL;
static int filterCount = 0;
static int filterStrip = TRUE;

typedef struct Folder {
  FILE		*fp;
  int		type;
  char		*key;
  char		*name;
  char		*dir;
  dev_t		dev;
  ino_t		ino;
  time_t	ctime;
  long		size;
  int		count;
  int		max;
  short		readonly;
  short		threaded;	/* ALL messages processed for threads */
  int		locked;
  Tcl_HashTable	mesgTable;
} Folder;

typedef struct VFLink {
  Folder	*folder;
  int		num;
  struct VFLink *next;
} VFLink;

typedef struct MimePart {
  char		  *type;
  char		  *subtype;
  Tcl_HashTable	  paramTable;
  char		  *encoding;
  char		  *dispos;
  char		  *id;
  char		  *descr;
  long		  begpart;
  long		  endpart;
  char		  *tmpfile;
  struct MimePart **subparts;
  int		  subcount;  /* count of allocated, not existing */
} MimePart;

#define MIME_MULTI  0
#define MIME_RFC822 1

struct Thread;

typedef struct Mesg {
  Folder	*folder;
  int		num;
  short		flags;
  short		origflags;
  long		begmesg;
  long		beghead;
  long	        begbody;
  long		endmesg;
  long		endhead;
  long	        endbody;
  int		lines;
  int		chars;
  time_t	time_received;
  time_t	time_sent;
  time_t	mtime;
  int		accessnum;
  VFLink	*links;
  MimePart	*mimepart;
  char		*nameBuf;
  char		*smBuf;
  int		hdrLen;
  char		*hdrBuf;
  Tcl_HashTable headerTable;
  char 		*mesgid;
  char		*replyid;
  struct Thread *replies;
  struct Mesg   *replyto;
} Mesg;

typedef struct Thread {
  Mesg		*mesgPtr;
  struct Thread *next;
} Thread;

/* each open folder will have a unique hash key that also
   serves as a Tcl command to manipulate and query it */
static Tcl_HashTable folderTable;
static unsigned int folderId = 1;

/* used for efficiently producing summary lines */
static Tcl_HashTable sumTable;
static unsigned int sumCount = 0;

/* can't be changed without changing Mfv_GetSumLine */
#define MAXSUMSYMB 20
static void *sumSymb[MAXSUMSYMB];

/* used by Mfv_CompareKey */
Folder		*sortFolder;
char   		*sortKey, *sortKey2;
Tcl_Interp	*sortInterp;

/* for Base64 and QPR encoding */
static int ENDIAN = 0;
static char basis_hex[] = "0123456789ABCDEF";
static unsigned char base64_b2c[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static unsigned char base64_c2b[0x80] = {
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0x3e, 0xff, 0xff, 0xff, 0x3f,
    0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b,
    0x3c, 0x3d, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
    0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
    0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
    0x17, 0x18, 0x19, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20,
    0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
    0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30,
    0x31, 0x32, 0x33, 0xff, 0xff, 0xff, 0xff, 0xff
};


/* local offset from GMT */
static long zonediff = 0;
static int now_isdst = 0;

/* some site variables */
static char *localhost = NULL;
static char *username = NULL;
static char *homedir = NULL;

/* PROTOTYPES */
int Tcl_InitSignalHandling (Tcl_Interp *);
int Mfv_FolderCmd(ClientData, Tcl_Interp *, int, char *argv[]);
int Mfv_CloseFolder(Tcl_Interp *, Folder *);
int Mfv_CreateMesgList(Tcl_Interp *, Folder *, char *, char *, Mesg ***);
int Mfv_SortMesgList(Tcl_Interp *, Folder *, char *, Mesg **, int);

#ifdef NO_STRCASECMP
/* Use the strncasecmp that must then be defined in libtcl.a */
#define strcasecmp(s1,s2) strncasecmp((s1),(s2),strlen(s1))
#endif

#ifndef HAVE_TEMPNAM
/* TODO: write compat replacement. Till then, ignore prefix */
char *tempnam(const char *dir, const char *pfx) 
{
  return tmpnam(dbgalloc(L_tmpnam));
}
#endif

#ifndef HAVE_TIMELOCAL
time_t timelocal(struct tm *tm)
{
  tm->tm_isdst = -1;
  return(mktime(tm));
}
#endif

#ifndef HAVE_TIMEGM
time_t timegm(struct tm *tm)
{
  time_t lt;
  lt = timelocal(tm);
  if (tm->tm_isdst == now_isdst)
    return (lt - zonediff);
  else if (tm->tm_isdst && !now_isdst)
    return (lt - zonediff + 3600);
  else
    return (lt - zonediff - 3600);
}
#endif

void Mfv_GetTZInfo()
{
  struct tm *tms;
  time_t now, ut;

  time(&now);

#ifdef HAVE_TIMEZONE
  zonediff = timezone;
#else
  tms = gmtime(&now);
  ut = timelocal(tms);

  zonediff = ((long)(ut-now));
#endif

  tms = localtime(&now);
  now_isdst = tms->tm_isdst;
}

#ifndef HAVE_STRERROR
char *strerror(int errnum)
{
  extern char *sys_errlist[];
  extern int sys_nerr;

  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return (char *) "Unknown error";
}
#endif

/* User is responsible for freeing response */
char *Mfv_GetCWD()
{
  size_t len = 0;
  char *cwd = NULL;

  do {
    dbgfree(cwd);
    len += 255;
    if ((cwd = dbgalloc(sizeof(char)*len)) == NULL) return NULL;
  } while ( getcwd(cwd,len) == NULL && errno == ERANGE );
  return cwd;
}

#if 0
int Mfv_System( Tcl_Interp *interp, char *cmd )
{
  int res;

  if ((res = system(cmd)) != 0) {
    Tcl_AppendResult(interp, "Error running \"", cmd,
	"\". Process ", (char *) NULL);
    if (WIFEXITED(res)) {
      sprintf(tmpLine, "exited with non-zero status %d.", WEXITSTATUS(res));
      Tcl_AppendResult(interp, tmpLine, (char *)NULL);
    } else if (WIFSIGNALED(res)) {
      sprintf(tmpLine, "terminated by signal %d.", WTERMSIG(res));
      Tcl_AppendResult(interp, tmpLine, (char *)NULL);
    } else {
      Tcl_AppendResult(interp, "died for an unknown reason.", (char *)NULL);
    }
    return TCL_ERROR;
  }

  return TCL_OK;
}
#endif

/*  returns pointer to char in <fileName> that is the start of the
 *  basename of the file. If <dir> is non-NULL, memory is allocated to
 *  place the directory and <dir> is set to point to it. A NULL is
 *  returned on error.
 */
char *Mfv_Basename( char *fileName, char **dir )
{
  char *cptr, *cwd;

  if (dir) *dir = NULL;

  cptr = strrchr(fileName, dirsep);
  if (cptr) {
    if (dir) {
      *cptr = 0;
      if ((cwd = Mfv_GetCWD()) == NULL) return NULL;
      if (CHDIR(fileName)!=0) {
	dbgfree(cwd);
	return NULL;
      }
      *dir = Mfv_GetCWD(); 
      if (CHDIR(cwd)!=0 || *dir == NULL) {
	dbgfree(cwd);
	return NULL;
      }
      *cptr = dirsep;
      dbgfree(cwd);
    }
    return(cptr+1);
  } else {
    if (dir && (*dir = Mfv_GetCWD()) == NULL) return NULL;
    return fileName;
  }
}

/*  returns pointer to newly allocated memory containing full path
 *  of <fileName>. Returns NULL on error. If <reqdir> is TRUE, then
 *  <fileName> must be a directory.
 */
char *Mfv_Fullpath( char *fileName, char reqdir )
{
  char *p, *fp = NULL, *dir= NULL, exists = TRUE;
  struct stat statbuf;
  
  if (stat(fileName, &statbuf) == -1) {
    if (errno != ENOENT) return NULL;
    exists = FALSE;
  }
  
  if (exists && S_ISDIR(statbuf.st_mode)) {
    if ((dir = Mfv_GetCWD()) == NULL) return NULL;
    if (CHDIR(fileName)!=0) {
      dbgfree(dir); return NULL;
    }
    fp = Mfv_GetCWD();
    if (CHDIR(dir)!=0 || fp == NULL) {
      dbgfree(dir); return NULL;
    }
  } else {
    if (reqdir) return NULL;
    if ((p = Mfv_Basename(fileName, &dir)) == NULL || dir == NULL) return NULL;
    fp = dbgalloc((strlen(dir) + strlen(dirsepStr) + strlen(p) + 3)*sizeof(char));
    *fp = 0;
    if (strlen(dir) > 1) strcat(fp, dir);
    strcat(fp, dirsepStr);
    strcat(fp, p);
    dbgfree(dir);
  }
  
  return fp;
}

char *Mfv_TrimSpace( char *s )
{
  char *p, *t;

  for (p=s; *p != 0 && isspace(*p); ++p);
  for (t=p+strlen(p)-1; t > p && isspace(*t); --t);
  ++t;
  if (isspace(*t)) *t = 0;
  return p;
}

char *Mfv_ToLower( char *s )
{
  char *p;
  for (p=s; *p != 0; p++)
    if (isupper(UCHAR(*p))) *p = tolower(UCHAR(*p));
  return s;
}

/* check if strings is beginning of BSD mail message */
int Mfv_IsNewMesg( char *s )
{
  char *p;

  if (stupidformat && *s == '>') s++;
  
  if (strncmp(s,"From ",5) != 0) return 0;
  
  p = s+5;
  if (!isspace(*p)) {
    for (; !isspace(*p) && *p; p++);   /* skip email address */
    if (!isspace(*p)) return 0;
  }
  for (p++; isspace(*p) && *p; p++);

  /* check possible tty */
  if (strncmp(p,"tty",3)==0) {
    
    for (; !isspace(*p) && *p; p++);
    if (!isspace(*p)) return 0;
    for (p++; isspace(*p) && *p; p++);
  }
    
  for (; isalpha(*p) && *p; p++);  /* skip day of week */
  if (!isspace(*p)) return 0;
  for (p++; isspace(*p) && *p; p++);

  for (; isalpha(*p) && *p; p++); /* skip month name */
  if (!isspace(*p)) return 0;
  for (p++; isspace(*p) && *p; p++);

  for (; isdigit(*p) && *p; p++); /* skip month day */
  if (!isspace(*p)) return 0;
  for (p++; isspace(*p) && *p; p++);

  for (; isdigit(*p) && *p; p++); /* skip hour */
  if (*p != ':') return 0;
  for (p++; isdigit(*p) && *p; p++); /* skip minute */
  if (*p != ':' && !isspace(*p)) return 0;

  return 1;
}

int Mfv_IsEmpty( char *s )
{
  char *p;

  for (p=s; isspace(*p) && *p; p++);
  if (*p == 0) return 1;
  else return 0;
}

int Mfv_GetMonth(char *mon)
{
  char c = mon[0];

  switch (c) {
  case 'a':
  case 'A':
    if (strncasecmp(mon, "apr",3) == 0) return 3;
    if (strncasecmp(mon, "aug",3) == 0) return 7;
    break;
  case 'j':
  case 'J':
    if (strncasecmp(mon, "jan",3) == 0) return 0;
    if (strncasecmp(mon, "jun",3) == 0) return 5;
    if (strncasecmp(mon, "jul",3) == 0) return 6;
    break;
  case 'm':
  case 'M':
    if (strncasecmp(mon, "mar",3) == 0) return 2;
    if (strncasecmp(mon, "may",3) == 0) return 4;
    break;
  case 'f':
  case 'F':
    if (strncasecmp(mon, "feb",3) == 0) return 1;
    break;
  case 's':
  case 'S':
    if (strncasecmp(mon, "sep",3) == 0) return 8;
    break;
  case 'o':
  case 'O':
    if (strncasecmp(mon, "oct",3) == 0) return 9;
    break;
  case 'n':
  case 'N':
    if (strncasecmp(mon, "nov",3) == 0) return 10;
    break;
  case 'd':
  case 'D':
    if (strncasecmp(mon, "dec",3) == 0) return 11;
    break;
  }
  return -1;
}

char *Mfv_GetTimeFromStr(char *datein, struct tm *tms, int datefield)
{
  int cnt, mday, month, year, hour, min, end, sec;
  char *p, mon[5], wday[5];

  mon[4] = wday[4] = 0; min = sec = 0;

  if (datefield) {
    p = datein;
    for (p = datein; !isdigit(*p) && *p != 0; p++);

    cnt = sscanf(p, "%d %3s %d %d%n:%d:%d",
	&mday, mon, &year, &hour, &end, &min, &sec);

    if (cnt < 5) {
      cnt = sscanf(p, "%d-%3s-%d %d%n:%d:%d",
	  &mday, mon, &year, &hour, &end, &min, &sec);
    }
    if (cnt < 5) return NULL;
  } else {
    cnt = sscanf(p = datein, " %3s %3s %d %d:%d:%d %d",
	wday, mon, &mday, &hour, &min, &sec, &year);
    if (cnt < 7) return NULL;
  }

  month = Mfv_GetMonth(mon);
  if ( year < 100 ) year += 1900;

  if ( month == -1 || year < 1900 || year > 3000 ||
      mday < 1 || mday > 31 || hour < 0 || hour > 25 ) return NULL;

  tms->tm_sec = sec;
  tms->tm_min = min;
  tms->tm_hour = hour;
  tms->tm_mday = mday;
  tms->tm_mon = month;
  tms->tm_year = year - 1900;
  tms->tm_isdst = -1;

  if (datefield)
    for (p += end; *p != ' ' && *p != 0; p++);
  return p;
}

time_t Mfv_TimeParse(char *datein)
{
  struct tm tms;
  char *p, tz[4];
  long offset = -1*zonediff;

  memset((char *)&tms, 0, sizeof(struct tm));

  p = Mfv_GetTimeFromStr( datein, &tms, 1);

  if (p && tms.tm_year > 0) {
    while (*p && (*p == ' ' || *p == '\t')) p++;
    if (*p == '+' || *p == '-') {
      if (isdigit(*(p+1))) {
	offset = atol(p)*36;
      }
    } else if (isalpha(*p)) {
      strncpy(tz,p,3);
      tz[3]=0;
      if (strncmp(tz,"UT",2)==0) offset = 0;
      else if (strncmp(tz,"GMT",3)==0) offset = 0;
      else if (strncmp(tz,"EST",3)==0) offset = -5;
      else if (strncmp(tz,"EDT",3)==0) offset = -4;
      else if (strncmp(tz,"CST",3)==0) offset = -6;
      else if (strncmp(tz,"CDT",3)==0) offset = -5;
      else if (strncmp(tz,"MST",3)==0) offset = -7;
      else if (strncmp(tz,"MDT",3)==0) offset = -6;
      else if (strncmp(tz,"PST",3)==0) offset = -8;
      else if (strncmp(tz,"PDT",3)==0) offset = -7;
      offset *= 3600;
    }
    return timegm(&tms) - offset;
  } else {
    return -1;
  }

}

void Mfv_InformClose(Tcl_Interp *interp, char *key)
{
  Tcl_AppendResult(interp, " The folderID \"", key, 
      "\" has been closed.", (char *) NULL);
}

int Mfv_StringToType(char *s)
{
  int j, len;
  for ( j = MFV_BSD; (len = strlen(folderTypes[j])); j++ ) {
    if (strncasecmp(s, folderTypes[j], len) == 0) break;
  }
  if (len > 0) return j;
  return 0;
}

Folder *Mfv_GetFolder(Tcl_Interp *interp, char *key)
{
  Tcl_HashEntry *entryPtr;
  entryPtr = Tcl_FindHashEntry(&folderTable, key);
  if (entryPtr == NULL) {
    Tcl_AppendResult(interp, "no folderID named \"", key,
	"\"", (char *) NULL);
    return NULL;
  }
  return (Folder *) Tcl_GetHashValue(entryPtr);
}

Mesg *Mfv_GetMesg(Tcl_Interp *interp, Folder *folderPtr, int mesg)
{
  Mesg *mesgPtr;
  Tcl_HashEntry *entryPtr;

  entryPtr = Tcl_FindHashEntry(&(folderPtr->mesgTable), (void *)mesg);
  if (entryPtr == NULL) {
    sprintf(interp->result,"no message %d", mesg);
    return NULL;
  }
  mesgPtr = (Mesg *) Tcl_GetHashValue(entryPtr);
  mesgPtr->accessnum = mesg;
  return mesgPtr;
}

char *Mfv_GetHeader(Tcl_Interp *interp, Mesg *mesgPtr, char *header)
{
  Tcl_HashEntry *entryPtr;
  entryPtr = Tcl_FindHashEntry(&(mesgPtr->headerTable), header);
  if (entryPtr == NULL) {
    return NULL;
  }
  return (char *) Tcl_GetHashValue(entryPtr);
}

FILE *Mfv_GetMesgFilePtr(Mesg *mesgPtr)
{
  Folder *folderPtr = mesgPtr->folder;

  if (folderPtr->type == MFV_MH) {
    sprintf(tmpFile, "%s%c%s%c%d", folderPtr->dir, dirsep, folderPtr->name,
	dirsep, mesgPtr->num );
    return fopen(tmpFile, "r");
  } else {
    return folderPtr->fp;
  }
}

Folder *Mfv_CheckIsOpen(dev_t dev, ino_t ino, char *fileName)
{
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  Folder *folderPtr;
  char *name, *dir = NULL;
  
  if (fileName) name = Mfv_Basename(fileName, &dir );
  else if ( dev == 0 && ino == 0 ) return 0;
  
  for (entryPtr = Tcl_FirstHashEntry(&folderTable,&search);
       entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
    folderPtr = (Folder *) Tcl_GetHashValue(entryPtr);
    if (ino == folderPtr->ino && dev == folderPtr->dev) {
      dbgfree (dir);
      return folderPtr;
    } else if (fileName && folderPtr->fp == NULL) {
      if (strcmp(folderPtr->name, name) == 0 &&
	  strcmp(folderPtr->dir, dir) == 0) {
	dbgfree(dir);
	return folderPtr;

      }
    }
  }
  
  dbgfree(dir);
  return NULL;
}

int Mfv_GetSumLine(Tcl_Interp *interp, Mesg *mesgPtr)
{
  char *p;
  int i;
  Tcl_HashEntry *entryPtr;

  if (sumCount < 1) {
    sprintf(interp->result,"sumformat has not been initialized");
    return TCL_ERROR;
  }

  for (i = 0; i < MAXSUMSYMB; ++i) {
    if (i < sumCount) {
      entryPtr = Tcl_FindHashEntry(&sumTable, (void *)i);
      if (entryPtr == NULL) {
	sprintf(interp->result,"sumformat is corrupted: out of sync");
	return TCL_ERROR;
      }
      p = (char *) Tcl_GetHashValue(entryPtr);
      switch(*p) {
      case '$':
	sumSymb[i] = Mfv_GetHeader(interp,mesgPtr,++p);
	if (!sumSymb[i]) sumSymb[i] = emptyString;
	break;
      case '#':
	sumSymb[i] = (void *)atoi(Mfv_GetHeader(interp,mesgPtr,++p));
	break;
      case 'n':
	sumSymb[i] = (void *)mesgPtr->accessnum;
	break;
      case 'c':
	sumSymb[i] = (void *)mesgPtr->chars;
	break;
      case 'l':
	sumSymb[i] = (void *)mesgPtr->lines;
	break;
      case 'S':
	if (mesgPtr->flags & MFV_SAVED) {
	  sumSymb[i] = "S";
	} else if (mesgPtr->flags & MFV_DELETED) {
	  sumSymb[i] = "D";
	} else if (mesgPtr->flags & MFV_ANSWERED) {
	  sumSymb[i] = "A";
	} else if (mesgPtr->flags & MFV_FORWARDED) {
	  sumSymb[i] = "F";
	} else if (mesgPtr->flags & MFV_READ) {
	  sumSymb[i] = emptyString;
	} else if (mesgPtr->flags & MFV_OLD) {
	  sumSymb[i] = "U";
	} else {
	  sumSymb[i] = "N";
	}
	break;
      default:
	sprintf(interp->result,"sumformat is corrupted: bad key");
	return TCL_ERROR;
      }
    } else {
      sumSymb[i] = NULL;
    }
  }

  /* An ugly but efficient hack */
  sprintf(tmpLine,fmtLine,
      sumSymb[0], sumSymb[1], sumSymb[2], sumSymb[3], sumSymb[4],
      sumSymb[5], sumSymb[6], sumSymb[7], sumSymb[8], sumSymb[9],
      sumSymb[10],sumSymb[11],sumSymb[12],sumSymb[13],sumSymb[14],
      sumSymb[15],sumSymb[16],sumSymb[17],sumSymb[18],sumSymb[19]);

  return TCL_OK;
}

int Mfv_MakeFmtLine(Tcl_Interp *interp, char *sumformat)
{
  char *p;
  int new, looking;
  Tcl_HashEntry *entryPtr;

  sumCount = 0;
  looking = FALSE;
  p = strncpy(fmtLine,sumformat,MAXLINE);
  while (*p) {
    if (looking && isalpha(*p)) {
      entryPtr = Tcl_CreateHashEntry(&sumTable, (void *)sumCount, &new);
      switch(*p) {
      case 'n':
	Tcl_SetHashValue(entryPtr, "n");
	*p = 'd';
	break;
      case 'd':
	Tcl_SetHashValue(entryPtr, "#sm-mday");
	*p = 'd';
	break;
      case 'h':
	Tcl_SetHashValue(entryPtr, "$sm-time");
	*p = 's';
	break;
      case 'm':
	Tcl_SetHashValue(entryPtr, "$sm-month");
	*p = 's';
	break;
      case 'w':
	Tcl_SetHashValue(entryPtr, "$sm-wday");
	*p = 's';
	break;
      case 'y':
	Tcl_SetHashValue(entryPtr, "#sm-year");
	*p = 'd';
	break;
      case 'D':
	Tcl_SetHashValue(entryPtr, "$date");
	*p = 's';
	break;
      case 'f':
	Tcl_SetHashValue(entryPtr, "$sm-from");
	*p = 's';
	break;
      case 'F':
	Tcl_SetHashValue(entryPtr, "$fullname");
	*p = 's';
	break;
      case 'i':
	Tcl_SetHashValue(entryPtr, "$message-id");
	*p = 's';
	break;
      case 's':
	Tcl_SetHashValue(entryPtr, "$subject");
	*p = 's';
	break;
      case 'c':
	Tcl_SetHashValue(entryPtr, "c");
	*p = 'd';
	break;
      case 'l':
	Tcl_SetHashValue(entryPtr, "l");
	*p = 'd';
	break;
      case 'S':
	Tcl_SetHashValue(entryPtr, "S");
	*p = 's';
	break;
      default:
	sprintf(interp->result,"bad placeholder symbol in format string");
	return TCL_ERROR;
      }
      looking = FALSE;
      sumCount++;
      if (sumCount == MAXSUMSYMB) {
	p++; *p = 0; break;
      }
    } else if (*p == '%') {
      if (looking) {
	if (*(p-1) == '%') looking = FALSE;
	else {
	  sprintf(interp->result, "invalid %% symbol in format string");
	  return TCL_ERROR;
	}
      } else {
	looking = TRUE;
      }
    }
    p++;
  }
  return TCL_OK;
}

static int Mfv_CompareByKey(Mesg *mp1, Mesg *mp2, char *key)
{
  char *mh1, *mh2;

  if ((*key == 'n') && (strcmp(key, "normal") == 0)) {
    return(mp1->accessnum - mp2->accessnum);
  } else if ((*key == 's') && (strcmp(key, "sent") == 0)) {
    return(mp1->time_sent - mp2->time_sent);
  } else if ((*key == 'r') && (strcmp(key, "received") == 0)) {
    return(mp1->time_received - mp2->time_received);
  }

  mh1 = Mfv_GetHeader(sortInterp,mp1,key);
  mh2 = Mfv_GetHeader(sortInterp,mp2,key);
  if (mh1 == NULL) mh1 = emptyString;
  if (mh2 == NULL) mh2 = emptyString;

  if ((*key == 's') && (strcmp(key, "subject") == 0)) {
    while (*mh1 == ' ') mh1++;
    while (*mh2 == ' ') mh2++;
    if (strncasecmp(mh1,"re:",3)==0) mh1 += 3;
    if (strncasecmp(mh2,"re:",3)==0) mh2 += 3;
    while (*mh1 == ' ') mh1++;
    while (*mh2 == ' ') mh2++;
  }

  return strcmp(mh1,mh2);
}

static int Mfv_CompareMesg(const void *p1, const void *p2)
{
  int res;
  Mesg *mp1 = (*(Mesg **)p1), *mp2 = (*(Mesg **)p2);

  if (sortKey && (res = Mfv_CompareByKey(mp1,mp2,sortKey))==0) {
    if (sortKey2 && (res = Mfv_CompareByKey(mp1,mp2,sortKey2))==0) {
      /* noop */;
    }
  }
  if (res==0) res = mp1->accessnum - mp2->accessnum;
  return res;
}

int Mfv_SortMesgList(Tcl_Interp *interp, Folder *folderPtr, char *keyList,
    Mesg **mesglist, int count)
{
  char **listArgv;
  int listArgc;

  if (count < 1) return TCL_OK;

  if (Tcl_SplitList(interp, keyList, &listArgc, &listArgv) != TCL_OK) {
    return TCL_ERROR;
  }
  if (!listArgc) {
    Tcl_AppendResult(interp, "empty sort key list", (char *) NULL);
    dbgfree((char *)listArgv);
    return TCL_ERROR;
  }
  sortInterp = interp;
  sortFolder = folderPtr;
  sortKey = listArgv[0];
  if (listArgc>1) sortKey2 = listArgv[1];
  else sortKey2 = NULL;

  qsort((char *)mesglist, count, sizeof(Mesg *), Mfv_CompareMesg);
  dbgfree((char *)listArgv);

  return TCL_OK;
}

int Mfv_CreateMesgList(Tcl_Interp *interp, Folder *folderPtr,
    char *mesgs, char *sortKeys, Mesg ***listPtr)
{
  char *buf, **listArgv;
  int i, mesg, count, listArgc;
  Tcl_HashSearch search;
  Tcl_HashEntry *entryPtr;
  Mesg *mesgPtr, **mesglist;

  if (mesgs) {
    if (Tcl_SplitList(interp, mesgs, &listArgc, &listArgv) != TCL_OK) {
      return -1;
    }
    if (!listArgc) {
      Tcl_AppendResult(interp, "empty message list", (char *) NULL);
      dbgfree((char *)listArgv);
      return -1;
    }
    count = listArgc;
  } else {
    count = folderPtr->count;
  }

  buf = dbgalloc(count*sizeof(Mesg *));
  mesglist = (Mesg **)buf;

  if (mesgs) {
    for (i = 0; i < count; ++i, ++mesglist) {
      if (Tcl_GetInt(interp, listArgv[i], &mesg) != TCL_OK) {
	abort:
	dbgfree(buf);
	dbgfree((char *)listArgv);
	return -1;
      }
      if ((mesgPtr = Mfv_GetMesg(interp, folderPtr, mesg)) == NULL) {
	Tcl_AppendResult(interp, " in folder \"", folderPtr->name,
	    "\"", (char *) NULL);
	goto abort;
      }
      *mesglist = mesgPtr;
    }
  } else {
    for(entryPtr = Tcl_FirstHashEntry(&(folderPtr->mesgTable),&search);
	entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search), ++mesglist) {
      *mesglist = (Mesg *) Tcl_GetHashValue(entryPtr);
      (*mesglist)->accessnum =
	  (int) Tcl_GetHashKey(&(folderPtr->mesgTable), entryPtr);
    }
  }
  mesglist = (Mesg **)buf;

  if (sortKeys && Mfv_SortMesgList(interp, folderPtr,
      sortKeys, mesglist, count) != TCL_OK) goto abort;

  if (mesgs) dbgfree((char *)listArgv);
  *listPtr = mesglist;
  return count;
}

char *Mfv_CompressHeader( char *s, int stripq, int stripw )
{
  char *p1, *p2, esc;
  int incom, quote;
  int guardlen = strlen(s);
  
  if (s == NULL) return NULL;
  
  esc = FALSE;
  incom = quote = 0;

  for (p1 = p2 = s; *p1 != 0; p1++) {
    if (esc) {
      if (!incom && !(quote && stripq)) {
	*p2 = *p1; p2++;
      }
      esc = FALSE;
      continue;
    }

    /* skip spaces outside quotes */
    if (stripw && isspace(*p1) && !quote) continue;

    if (!incom && !(quote && stripq)) {
      *p2 = *p1; p2++;
    }

    switch (*p1) {
    case '"':
      if (!quote) {
	quote = '"';
	if (stripq) p2--; /* take back */
      } else if (quote == '"') quote = 0;
      break;
    case '[':
      if (!quote) {
	quote = ']';
	if (stripq) p2--; /* take back */
      }
      break;
    case ']':
      if (quote == ']') quote = 0;
      break;
    case '(':
      if (!incom && !quote) p2--; /* take back */
      if (!quote) incom++;
      break;
    case ')':
      if (!quote) {
	incom--;
	if (incom < 0) incom = 0;
      }
      break;
    case '\\':
      esc = TRUE;
    }
  }
  *p2 = 0;
  if ( (s+guardlen) < p2 )
    fprintf(stderr, "Memory corruption in Mfv_CompressHeader\n");
  
  return s;
}

int Mfv_Base64DecodePipe( FILE *ifp, FILE *ofp, long stop )
{
  int i, bi;
  char c, got, fin;
  unsigned long bits;
  unsigned char b,
      *bp = (unsigned char *) &bits,
      *bp1 = &bp[ENDIAN > 0 ? 1 : 2],
      *bp2 = &bp[ENDIAN > 0 ? 2 : 1],
      *bp3 = &bp[ENDIAN > 0 ? 3 : 0];

  bits = 0L; bi = 18; got = 0; fin = FALSE;
  while (ftell(ifp) < stop && ((i = fgetc(ifp)) != EOF)) {
    c = (char) i;
    if (isspace(c)) continue;
    if (c & 0x80) {
      return MFV_PARSE_ERR;
    }
    if (c == '=') {
      if (got == 0) return MFV_OK; /* Unofficial */
      if (got < 2) return MFV_PARSE_ERR;
      fin = TRUE;
    } else {
      if (fin) return MFV_PARSE_ERR;
      if ((b = base64_c2b[c & 0x7f]) > 0x3f) {
	return MFV_PARSE_ERR;
      }
      bits |= b << bi;
      bi -= 6;
    }
    if (++got == 4) {
      if (bi < 12) {
	if (fputc((char) *bp1, ofp) == EOF) return MFV_OUTPUT_ERR;
	if (bi < 6) {
	  fputc((char) *bp2, ofp);
	  if (bi < 0) {
	    fputc((char) *bp3, ofp);
	  }
	}
      }
      bits = 0L; bi = 18; got = 0; fin = FALSE;
    }
  }

  fflush(ofp);
  if (got) return MFV_PARSE_ERR;
  return MFV_OK;
}

int Mfv_Base64EncodePipe( FILE *ifp, FILE *ofp, long stop, int port_nl )
{
  int i, c[4], cnt;
  char nl = FALSE;
  
  cnt = 0; c[3] = 99;
  while (1) {
    i = 0;
    while (i < 3 && (stop == 0 || ftell(ifp) < stop || nl)) {
      if (nl) {
	c[i] = '\n';
	nl = FALSE;
      } else {
	if ((c[i] = fgetc(ifp)) == EOF) break;
	if (port_nl && c[i] == '\n' && !nl) {
	  nl = TRUE; c[i] = '\r';
	}
      }
      i++;
    }
    if (i == 0) break;
    if (i < 3) c[i] = 0;
    
    putc(base64_b2c[c[0]>>2], ofp);
    putc(base64_b2c[((c[0] & 0x3)<< 4) | ((c[1] & 0xF0) >> 4)], ofp);
    if (i == 1) {
      putc('=', ofp);
      putc('=', ofp);
    } else if (i == 2) {
      putc(base64_b2c[((c[1] & 0xF) << 2) | ((c[2] & 0xC0) >>6)], ofp);
      putc('=', ofp);
    } else {
      putc(base64_b2c[((c[1] & 0xF) << 2) | ((c[2] & 0xC0) >>6)], ofp);
      putc(base64_b2c[c[2] & 0x3F], ofp);
    }

    cnt += 4;
    if (cnt > 71) {
      putc('\n', ofp);
      cnt = 0;
    }

    if (c[i] == 0) break;
  }
  
  if (cnt) putc('\n', ofp);
  fflush(ofp);
  return MFV_OK;
}

char *Mfv_Base64Decode( char *s )
{
  int bi;
  char *rp, *wp;
  unsigned long bits;
  unsigned char b,
      *bp = (unsigned char *) &bits,
      *bp1 = &bp[ENDIAN > 0 ? 1 : 2],
      *bp2 = &bp[ENDIAN > 0 ? 2 : 1],
      *bp3 = &bp[ENDIAN > 0 ? 3 : 0];

  rp = wp = s;
  while (*rp) {
    bits = 0L;
    for (bi = 18; bi >= 0; bi -= 6) {
      while (isspace(*rp)) ++rp;
      if (!*rp) break;
      if (*rp & 0x80) {
	*wp = 0; return NULL;
      }
      if (*rp == '=') {
	rp++;
	if (*rp == '=') rp++;
	if (*rp == '=') goto done;
	break;
      }
      if ((b = base64_c2b[*rp & 0x7f]) > 0x3f) {
	*wp = 0; return NULL;
      }
      bits |= b << bi;
      rp++;
    }
    if (bi < 12) {
      *(wp++) = (char) *bp1;
      if (bi < 6) {
	*(wp++) = (char) *bp2;
	if (bi < 0) {
	  *(wp++) = (char) *bp3;
	}
      }
    }
  }

  done:
  *wp = 0;
  return s;
}

int Mfv_QuotedPrDecodePipe( FILE *ifp, FILE *ofp, long stop )
{
  int i, j;
  char c, hex[3], looking, spc;
  long num, mark;

  hex[2] = 0; spc = FALSE; looking = FALSE;
  while (ftell(ifp) < stop && ((j = fgetc(ifp)) != EOF)) {
    c = (char) j;
    if (looking) {
      if (isxdigit(c)) {
	if (spc) return MFV_PARSE_ERR;
	hex[i++] = c;
	if (i == 2) {
	  looking = FALSE; spc = FALSE;
	  num = strtol(hex, NULL, 16);
	  fputc((char)num, ofp);
	}
      } else if (isspace(c)) {
	if (i != 0) return MFV_PARSE_ERR;
	if (c == '\n') {
	  looking = FALSE; spc = FALSE;
	} else spc = TRUE;
      } else {
	return MFV_PARSE_ERR;
      }
    } else {
      if (c == '=') {
	looking = TRUE; i = 0; spc = FALSE;
      } else {
	if (c == '\n' || c == '\r') {
	  if (spc)
	    if (fseek(ofp, mark, SEEK_SET) < 0) return MFV_OUTPUT_ERR;
	  spc = FALSE;
	} else if (c == ' ' || c == '\t') {
	  if (!spc) { mark = ftell(ofp); spc = TRUE; }
	}
	else spc = FALSE;

	if (fputc(c, ofp) == EOF) return MFV_OUTPUT_ERR;
      }
    }
  }
  return MFV_OK;
}

int Mfv_QuotedPrEncodePipe( FILE *ifp, FILE *ofp, long stop )
{
  int c, ct=0, prevc=255;
  while ((stop == 0 || ftell(ifp) < stop) && (c = getc(ifp)) != EOF) {
    if ((c < 32 && (c != '\n' && c != '\t'))
	|| (c == '=')
	|| (c >= 127)
	/* Following line is to avoid single periods alone on lines,
	   which messes up some dumb smtp implementations, sigh... */
	|| (ct == 0 && c == '.')) {
      putc('=', ofp);
      putc(basis_hex[c>>4], ofp);
      putc(basis_hex[c&0xF], ofp);
      ct += 3;
      prevc = 'A'; /* close enough */
    } else if (c == '\n') {
      if (prevc == ' ' || prevc == '\t') {
	putc('=', ofp); /* soft & hard lines */
	putc(c, ofp);
      }
      putc(c, ofp);
      ct = 0;
      prevc = c;
    } else {
      if (c == 'F' && prevc == '\n') {
	/* HORRIBLE but clever hack suggested by MTR for sendmail-avoidance */
	c = getc(ifp);
	if (c == 'r') {
	  c = getc(ifp);
	  if (c == 'o') {
	    c = getc(ifp);
	    if (c == 'm') {
	      c = getc(ifp);
	      if (c == ' ') {
		/* This is the case we are looking for */
		fputs("=46rom", ofp);
		ct += 6;
	      } else {
		fputs("From", ofp);
		ct += 4;
	      }
	    } else {
	      fputs("Fro", ofp);
	      ct += 3;
	    }
	  } else {
	    fputs("Fr", ofp);
	    ct += 2;
	  }
	} else {
	  putc('F', ofp);
	  ++ct;
	}
	ungetc(c, ifp);
	prevc = 'x'; /* close enough -- printable */
      } else { /* END horrible hack */
	putc(c, ofp);
	++ct;
	prevc = c;
      }
    }
    if (ct > 72) {
      putc('=', ofp);
      putc('\n', ofp);
      ct = 0;
      prevc = '\n';
    }
  }
  if (ct) {
    putc('=', ofp);
    putc('\n', ofp);
  }

  fflush(ofp);
  return MFV_OK;
}

char *Mfv_QuotedPrDecode( char *s )
{
  char *p, *rp, *wp, hex[3];
  long num;

  hex[2] = 0;
  for (rp = wp = s; *rp; ++rp) {
    if (*rp == '=') {
      p = rp+1;
      if (*p == ' ' || *p == '\t') {
	for (p=p+1; *p && (*p == ' ' || *p == '\t'); ++p);
	if (!*p) { *wp = 0; return NULL; }
      }
      if (*p == '\n' || *p == '\r') {
	if (*p == '\r' && *(p+1) == '\n') ++p;
	rp = p;
	continue;
      }
      hex[0] = *(rp+1); hex[1] = *(rp+2);
      if (isxdigit(hex[0]) && isxdigit(hex[1])) {
	rp += 2;
	num = strtol(hex, NULL, 16);
	*wp = num;
	wp++;
	continue;
      }
    } else if (*rp == ' ' || *rp == '\t') {
      for (p=rp+1; *p && (*p == ' ' || *p == '\t'); ++p);
      if (*p == '\n' || *p == '\r') rp = p;
    }
    *wp = *rp;
    wp++;
  }
  *wp = 0;
  return s;
}

int Mfv_ContentDecodePipe( Tcl_Interp *interp, FILE *ifp,
    FILE *ofp, long stop, char *decode )
{
  char buf[BUFSIZ];
  long bufsize, len;

  if (decode) {
    if (strcasecmp(decode,"base64")==0)
      return Mfv_Base64DecodePipe(ifp, ofp, stop);
    else if (strcasecmp(decode,"quoted-printable")==0)
      return Mfv_QuotedPrDecodePipe(ifp, ofp, stop);
    else if (strcasecmp(decode,"7bit")==0 ||
	     strcasecmp(decode,"8bit")==0 ||
	     strcasecmp(decode,"binary")==0)
      goto nodecode;
    else
      return MFV_ENCODE_ERR;
  } else {
    nodecode:
    while ((bufsize = MIN(BUFSIZ, stop - ftell(ifp))) > 0) {
      if ((len = fread(buf,sizeof(char),bufsize,ifp)) != bufsize)
	return MFV_INPUT_ERR;
      if (fwrite(buf,sizeof(char),len,ofp) != len)
	return MFV_OUTPUT_ERR;
    }
  }

  return MFV_OK;
}

int Mfv_ContentDecode( char *s, char *decode )
{

  if (strcasecmp(decode,"base64")==0) {
    if (Mfv_Base64Decode(s) == NULL) return MFV_PARSE_ERR;
  } else if (strcasecmp(decode,"quoted-printable")==0) {
    if (Mfv_QuotedPrDecode(s) == NULL) return MFV_PARSE_ERR;
  } else if (strcasecmp(decode,"7bit")==0 ||
	   strcasecmp(decode,"8bit")==0 ||
      strcasecmp(decode,"binary")==0) {
    return MFV_OK;
  } else {
    return MFV_ENCODE_ERR;
  }
  return MFV_OK;
}

/* returns -1 on sys error, -2 on fail to lock, 0 on success */
int Mfv_LockInternal(char *dir, char *name)
{
  char *buf;
  int fd, status, retries, tries;
  struct stat statbuf, statsys;

  retries = lock_retries;
  tries = 0;

  if(stat(dir, &statbuf) == -1 ||
      stat(mailSpool, &statsys) == -1) {
    lock_err = strerror(errno);
    return(-1);
  }

  if (statbuf.st_dev == statsys.st_dev &&
      statbuf.st_ino == statsys.st_ino) {
    buf = dbgalloc(sizeof(char)*(strlen(dotlockprog) + 30));
    sprintf(buf, "%s -q -s -r %d -t %d",
	dotlockprog, retries, lock_timeout);
  } else {
    buf = dbgalloc(sizeof(char)*(strlen(dotlockprog) +
	tmpFileSize + 30));
    sprintf(buf, "%s -q -r %d -t %d %s%c%s",
	dotlockprog, retries, lock_timeout, dir, dirsep, name);
  }
  status = system(buf);
  dbgfree(buf);
  if (status != 0) {
    if (WIFEXITED(status) && WEXITSTATUS(status) == 2) {
      return(-2);
    } else {
      lock_err = "The dotlock program failed. Check path and permissions.";
      return(-1);
    }
  }

  return(0);
}

/* returns -1 on sys error, -2 on fail to lock, 0 on success */
int Mfv_LockFolder(Folder *folderPtr)
{
  char *buf, already;
  int result;

  if (folderPtr->fp == NULL) {
    folderPtr->locked = TRUE; /*TODO: how to lock MH folders? */
    return 0;
  }

  already = folderPtr->locked;
  folderPtr->locked = FALSE;
  
  result = Mfv_LockInternal(folderPtr->dir, folderPtr->name);
  
  if (already && result == -2) result = 0;
  if (result == 0) folderPtr->locked = TRUE;
  return result;
}

/* returns -1 on sys error, -2 on fail to lock, 0 on success */
int Mfv_LockFile(char *fileName)
{
  char *name, *dir = NULL;
  int result;
  struct stat statbuf;
  Folder *folderPtr;
  
  statbuf.st_dev = statbuf.st_ino = 0;
  if (stat(fileName, &statbuf) == -1 && errno != ENOENT) {
    lock_err = strerror(errno);
    return(-1);
  } else {
    if (folderPtr = Mfv_CheckIsOpen(statbuf.st_dev, statbuf.st_ino, fileName)) {
      return Mfv_LockFolder(folderPtr);
    }
  }
  
  if ((name = Mfv_Basename(fileName, &dir)) == NULL) {
    lock_err = "Bad filename.";
    return(-1);
  }
  
  result = Mfv_LockInternal(dir, name);
  dbgfree(dir);
  return result;
}

int Mfv_UnlockInternal(char *dir, char *name)
{
  char *buf = NULL;
  int fd, status, result;
  struct stat statbuf, statsys;

  result = 0;

  if(stat(dir, &statbuf) == -1 ||
      stat(mailSpool, &statsys) == -1) {
    lock_err = strerror(errno);
    result = -1;
    
  } else {
    if (statbuf.st_dev == statsys.st_dev &&
	statbuf.st_ino == statsys.st_ino) {
      buf = dbgalloc(sizeof(char)*(strlen(dotlockprog) + 30));
      sprintf(buf, "%s -u -s", dotlockprog);
    } else {
      buf = dbgalloc(sizeof(char)*(strlen(dotlockprog) +
	  strlen(dir) + strlen(name) + 30));
      sprintf(buf, "%s -u %s%c%s", dotlockprog, dir,
	  dirsep, name);
    }
    status = system(buf);
    if (status == -1) {
      sprintf(buf, "%s%c%s.lock", dir, dirsep, name);
      if (access(buf,F_OK)==0) {
	lock_err = "The dotlock program failed. Check path and permissions.";
	result = -1;
      }
    }
    dbgfree(buf);
  }

  return result;
}

int Mfv_UnlockFolder(Folder *folderPtr)
{
  char *lockfile;
  int result;

  result = Mfv_UnlockInternal(folderPtr->dir, folderPtr->name);
  if (result == 0) folderPtr->locked = FALSE;
  return result;
}

int Mfv_UnlockFile(char *fileName)
{
  char *name, *dir = NULL;
  int result;
  struct stat statbuf;
  Folder *folderPtr;
  
  statbuf.st_dev = statbuf.st_ino = 0;
  if (stat(fileName, &statbuf) == -1 && errno != ENOENT) {
    lock_err = strerror(errno);
    return(-1);
  } else {
    if (folderPtr = Mfv_CheckIsOpen(statbuf.st_dev, statbuf.st_ino, fileName)) {
      return Mfv_UnlockFolder(folderPtr);
    }
  }
  
  if ((name = Mfv_Basename(fileName, &dir)) == NULL) {
    lock_err = "Bad filename.";
    return(-1);
  }
  
  result = Mfv_UnlockInternal(dir, name);
  dbgfree(dir);
  return result;
}

char *Mfv_GetFilePart(Mesg *mesgPtr, long start, long stop)
{
  char *cptr, *buf;
  long bufsize, len;
  FILE *ifp;
  Folder *folderPtr;

  if (start > stop) return NULL;

  folderPtr = mesgPtr->folder;
  if ((ifp = Mfv_GetMesgFilePtr(mesgPtr)) == NULL) return NULL;

  bufsize = stop-start;
  cptr = buf = (char *) dbgalloc((bufsize+1)*sizeof(char));
  if (buf == NULL) return NULL;
  buf[bufsize] = 0;

  if(fseek(ifp, start, SEEK_SET) < 0) {
    cptr = NULL;
  } else {
    len = fread(buf,sizeof(char),bufsize,ifp);
    if (len != bufsize) cptr = NULL;
  }
  if (folderPtr->type == MFV_MH ) fclose(ifp);
  if (cptr == NULL) dbgfree(buf);
  return cptr;
}

int Mfv_WriteFilePart(Tcl_Interp *interp, Mesg *mesgPtr,
    long start, long stop, char *decode, char *file)
{
  char *toFile;
  int result, res;
  FILE *ifp, *ofp;
  struct stat statbuf;
  Folder *folderPtr;
  Tcl_DString buffer;

  if (start > stop) return TCL_ERROR;
  ifp = ofp = NULL;
  result = TCL_OK;

  if ((toFile = Tcl_TildeSubst(interp, file, &buffer)) == NULL) {
      Tcl_AppendResult(interp, "cannot resolve as a file \"", file,
	  "\"", (char *) NULL);
      return TCL_ERROR;
  }

  if(stat(toFile, &statbuf) == -1) {
    if (errno != ENOENT) {
      badfile:
      Tcl_AppendResult(interp, "cannot write to file \"", toFile,
	  "\"", (char *) NULL);
      result = TCL_ERROR;
      goto done;
    }
    if (Mfv_CheckIsOpen(0, 0, toFile)) goto alreadyopen;
  } else {
    if (Mfv_CheckIsOpen(statbuf.st_dev, statbuf.st_ino, NULL)) {
      alreadyopen:
      Tcl_AppendResult(interp,
	  "cannot write to open folder", (char *) NULL);
      result = TCL_ERROR;
      goto done;
    } else {
      if (((statbuf.st_mode)&S_IWUSR) != S_IWUSR) goto badfile; 
      if (!S_ISREG(statbuf.st_mode)) goto badfile;
    }
  }
  if ((ofp = fopen(toFile, "w+")) == NULL) goto badfile;

  if ((ifp = Mfv_GetMesgFilePtr(mesgPtr)) == NULL) {
    corrupt:
    folderPtr = mesgPtr->folder;
    Tcl_AppendResult(interp, "internal consistency error in \"",
	folderPtr->name, "\" during save.", (char *) NULL);
    Mfv_InformClose(interp, folderPtr->key);
    Mfv_CloseFolder(interp, folderPtr);
    result = TCL_ERROR;
    goto done;
  }

  if (fseek(ifp, start, SEEK_SET) < 0) goto corrupt;
  if ((res = Mfv_ContentDecodePipe(interp, ifp,
      ofp, stop, decode)) != MFV_OK) {
    if (res == MFV_INPUT_ERR) goto corrupt;
    else if (res == MFV_OUTPUT_ERR)
      Tcl_AppendResult(interp, "Error writing to \"",
	  toFile, "\"", (char *) NULL);
    else if (res == MFV_ENCODE_ERR)
      Tcl_AppendResult(interp, "Unknown encoding \"",
	  decode, "\"", (char *) NULL);
    else if (res == MFV_PARSE_ERR)
      Tcl_AppendResult(interp, "parse error decoding \"",
	  decode, "\" data", (char *) NULL);
    result = TCL_ERROR;
  }

  done:
  Tcl_DStringFree(&buffer);
  if (mesgPtr->folder->type == MFV_MH ) fclose(ifp);
  fclose(ofp);
  return result;
}

void Mfv_FreeMimePart(MimePart *mp)
{
  int i;
  MimePart **smpp;

  if (mp == NULL) return;
  dbgfree(mp->type);
  dbgfree(mp->encoding);
  dbgfree(mp->id);
  dbgfree(mp->descr);
  dbgfree(mp->dispos);
  if (mp->tmpfile) {
    unlink(mp->tmpfile);
    dbgfree(mp->tmpfile);
  }
  Tcl_DeleteHashTable(&(mp->paramTable));
  if (smpp = mp->subparts) {
    for (i = 0; *smpp && i < mp->subcount; ++i, ++smpp)
      Mfv_FreeMimePart(*smpp);
    dbgfree((char *)mp->subparts);
  }
  dbgfree((char *)mp);
}

int Mfv_ParseMimeType(MimePart *mp, char *s)
{
  char *p1, *p2, *buf, c, inquote, esc;
  int new;
  Tcl_HashEntry *entryPtr;
  Tcl_HashTable *paramTblPtr;

  if (s==NULL || *s == 0) return TCL_ERROR;

  for (p1=s; isspace(*p1); p1++);
  buf = dbgstrdup(p1);
  Mfv_CompressHeader(buf, FALSE, TRUE);

  p1 = strchr(buf,'/');
  p2 = strchr(buf,';');
  if (p1 == NULL || (p2 && p1 > p2)) {
    mp->type = buf;
    mp->subtype = "unknown";
    if (p2) *p2 = 0;
  } else {
    mp->type = buf; *p1 = 0;
    Mfv_ToLower(mp->type);
    if (debug) fprintf(stderr, "Type: %s\n", mp->type);

    p1++;
    if (p2) *p2 = 0;
    mp->subtype = p1;
    Mfv_ToLower(mp->subtype);
    if (debug) fprintf(stderr, "Subtype: %s\n", mp->subtype);
  }

  paramTblPtr = &(mp->paramTable);
  while (p2) {
    p1 = ++p2;
    while (*p1 && *p1 != '=' && *p1 != ';') p1++;
    if (*p1 == '=') {
      *p1 = 0; p1++;
      Mfv_ToLower(p2);
      entryPtr = Tcl_CreateHashEntry(paramTblPtr, p2, &new);
      if (debug) fprintf(stderr, "Param: %s = ", p2);
      if (*p1 == '"') { p1++; inquote = TRUE; }
      else inquote = FALSE;
      esc = FALSE;
      p2 = p1;
      while (*p1) {
	p1++;
	if (esc) {
	  esc = FALSE;
	} else {
	  if (inquote && *p1 == '"') break;
	  if (!inquote && *p1 == ';') break;
	}
      }
      c = *p1; *p1 = 0;
      Tcl_SetHashValue(entryPtr, p2);
      if (debug) fprintf(stderr, "%s\n", p2);
      if (c == '"')
	p2 = strchr(p1+1,';');
      else p2 = p1;
    }
    else if (*p1 == ';') p2 = p1;
    else break;
  }

  return TCL_OK;
}

MimePart *Mfv_AddMimeSubPart(MimePart *mp, int cnt, FILE *ifp)
{
  int i;
  MimePart *smp, **tmpList;
  
  if (cnt >= mp->subcount) { /* expand subpart list */
    cnt = mp->subcount;
    mp->subcount += 5;
    tmpList = (MimePart **) dbgalloc(mp->subcount*sizeof(MimePart*));
    for (i = 0; i < cnt; ++i) tmpList[i] = mp->subparts[i];
    for (; i < mp->subcount; ++i) tmpList[i] = NULL;
    dbgfree((char *)mp->subparts);
    mp->subparts = tmpList;
  }
  smp = (MimePart *) dbgalloc(sizeof(MimePart));
  mp->subparts[cnt++] = smp;
  memset(smp,0,sizeof(MimePart));
  smp->begpart = ftell(ifp);  /* temporary */
  smp->endpart = mp->endpart; /*   "   "   */
  Tcl_InitHashTable(&(smp->paramTable), TCL_STRING_KEYS);

  return smp;
}

int Mfv_ParseMultiPart(Tcl_Interp *interp, Mesg *mesgPtr, MimePart *mp, FILE *ifp,
    char *bound, int multitype)
{
  char *p1, *p2, foundsub, foundbody, newfield, *field;
  char begbound[MAXLINE+1], clean, digest = FALSE;
  MimePart *smp;
  Folder *folderPtr;
  int i, cnt, len;
  long filepos;
  Tcl_HashEntry *entryPtr;
  Tcl_HashTable *paramTblPtr;
  Tcl_DString buffer;

  if (mp->subparts) return TCL_OK;

  if (multitype == MIME_MULTI) {
    paramTblPtr = &(mp->paramTable);
    entryPtr = Tcl_FindHashEntry(paramTblPtr, "boundary");
    if (entryPtr == NULL) {
      Tcl_AppendResult(interp, "MIME multipart missing boundary", (char *) NULL);
      return TCL_ERROR;
    }
    bound = (char *) Tcl_GetHashValue(entryPtr);
    if(strcmp(mp->subtype,"digest")==0) digest = TRUE;
  }
  sprintf(begbound,"--%s",bound);
  len = strlen(begbound);

  folderPtr = mesgPtr->folder;
  if (ifp == NULL && (ifp = Mfv_GetMesgFilePtr(mesgPtr)) == NULL) {
    Tcl_AppendResult(interp, "cannot read message from folder \"",
	folderPtr->name, "\"", (char *) NULL);
    return TCL_ERROR;
  }

  Tcl_DStringInit(&buffer);
  fseek(ifp, mp->begpart, SEEK_SET);
  filepos = ftell(ifp);
  foundbody = FALSE;
  cnt = 0;
  if (multitype == MIME_MULTI) {
    foundsub = FALSE; smp = NULL; 
  } else {
    foundsub = TRUE;
    /* eat empty lines */
    while (filepos < mesgPtr->endbody && fgets(tmpLine,MAXLINE,ifp) != NULL) {
      for (p1=tmpLine; *p1 && isspace(*p1); p1++); /* get first non-WSP char */
      if (*p1 != 0) {
	if (strncmp(tmpLine,"--",2)==0 && strncmp(tmpLine,begbound,len)==0)
	  return TCL_OK;
	if (isspace(*tmpLine) || !strchr(tmpLine,':')) foundbody = TRUE;
	fseek(ifp, filepos, SEEK_SET);
	cnt = 1;
	break;
      }
      filepos = ftell(ifp);
    }
    if (cnt != 1) return TCL_ERROR;
    smp = Mfv_AddMimeSubPart(mp, 0, ifp);
  }
  clean = FALSE;
  while (filepos < mesgPtr->endbody && fgets(tmpLine,MAXLINE,ifp) != NULL) {
    if (strncmp(tmpLine,"--",2)==0 && strncmp(tmpLine,begbound,len)==0) {
      if (foundsub && smp) smp->endpart = filepos;
      foundsub = TRUE; foundbody = FALSE;
      if (strncmp(tmpLine+len,"--",2)==0 || multitype == MIME_RFC822) {
	clean = TRUE;
	if (multitype == MIME_RFC822)
	  fseek(ifp, filepos, SEEK_SET); /* put boundary back on stack */
	if (debug) fprintf(stderr, "Multipart ended cleanly\n");
	break;
      } else {
	smp = Mfv_AddMimeSubPart(mp, cnt, ifp); cnt++;
	if (debug) fprintf(stderr, "Processing subpart %d\n", cnt);
      }
    } else if (foundsub && !foundbody) {
      for (p1=tmpLine; *p1 && isspace(*p1); p1++); /* get first non-WSP char */
      if (*p1 == 0) foundbody = TRUE;

      newfield = (!isspace(*tmpLine) && strchr(tmpLine,':'));

      /* if new field or end of headers, process previous field */
      if (foundbody || newfield) { 
	field = Tcl_DStringValue(&buffer);
	if ((p2 = strchr(field,':')) != NULL) {
	  for (p2++; isspace(*p2); p2++);
	  if (*field == 'c' || *field == 'C') {
	    if (strncasecmp(field,"content-type",12)==0) {
	      Mfv_ParseMimeType(smp, p2);
	    } else if (strncasecmp(field,"content-transfer-encoding",25)==0) {
	      smp->encoding = dbgstrdup(p2);
	      Mfv_CompressHeader(smp->encoding, FALSE, TRUE);
	      Mfv_ToLower(smp->encoding);
	    } else if (strncasecmp(field,"content-id",10)==0) {
	      smp->id = dbgstrdup(p2);
	      Mfv_CompressHeader(smp->id, FALSE, TRUE);
	    } else if (strncasecmp(field,"content-disposition",19)==0) {
	      smp->dispos = dbgstrdup(p2);
	      Mfv_CompressHeader(smp->dispos, FALSE, TRUE);
	    } else if (strncasecmp(field,"content-description",19)==0) {
	      smp->descr = dbgstrdup(p2);
	    }
	  }
	}
	Tcl_DStringFree(&buffer);
	Tcl_DStringInit(&buffer);
      }

      if (foundbody) { /* end of headers */
	if (!smp->type) {
	  smp->type = dbgstrdup(digest ? "message/rfc822" : "text/plain");
	  smp->type[digest ? 7 : 4] = 0;
	  smp->subtype = &(smp->type[digest ? 8: 5]);
	  if (debug)
	    fprintf(stderr, "Defaulting subpart %d to %s\n", cnt, smp->type);
	}
	if (!smp->encoding) smp->encoding = dbgstrdup("7bit");
	smp->begpart = ftell(ifp);
	if (smp->type && strcmp(smp->type,"multipart")==0) {
	  if (Mfv_ParseMultiPart(interp, mesgPtr, smp, ifp,
	      NULL, MIME_MULTI) != TCL_OK) return TCL_ERROR;
	} else if (smp->type && strcmp(smp->type,"message")==0 &&
	    strcmp(smp->subtype,"rfc822")==0) {
	  if (Mfv_ParseMultiPart(interp, mesgPtr, smp, ifp,
	      bound, MIME_RFC822) != TCL_OK) return TCL_ERROR;
	}
      } else {
	while (p2 = strrchr(tmpLine, '\n')) *p2 = ' ';
	Tcl_DStringAppend(&buffer, p1, -1);
      }
    }
    filepos = ftell(ifp);
  }

  mp->endpart = ftell(ifp);
  Tcl_DStringFree(&buffer);
  if (!clean) {
    if (multitype == MIME_MULTI) {
      fprintf(stderr, "Multipart ended UNCLEANLY\n");
      return TCL_ERROR;
    } else {
      fprintf(stderr, "WARNING: message/rfc822 ended UNCLEANLY\n");
      if (smp) {
	smp->endpart = filepos;
	if (!smp->type) {
	  smp->type = dbgstrdup("text/plain");
	  smp->type[4] = 0;
	  smp->subtype = &(smp->type[5]);
	}
	if (!smp->encoding) smp->encoding = dbgstrdup("7bit");
      }
    }
  }

  return TCL_OK;
}

int Mfv_ParseMimeMesg(Tcl_Interp *interp, Mesg *mesgPtr)
{
  char *p;
  MimePart *mp;

  if (mesgPtr->mimepart) return TCL_OK;

  /* TODO: check mime-version */

  mesgPtr->mimepart = mp = (MimePart *) dbgalloc(sizeof(MimePart));
  mp->begpart = mesgPtr->begbody;
  mp->endpart = mesgPtr->endbody;
  mp->tmpfile = NULL;
  Tcl_InitHashTable(&(mp->paramTable), TCL_STRING_KEYS);

  p = Mfv_GetHeader(interp, mesgPtr, "content-type");
  if (p == NULL) {
    usedefaulttype:
    mp->type = dbgstrdup("text/plain");
    mp->type[4] = 0;
    mp->subtype = &(mp->type[5]);
  } else {
    if (Mfv_ParseMimeType(mp, p) != TCL_OK) goto usedefaulttype;
  }

  p = Mfv_GetHeader(interp, mesgPtr, "content-transfer-encoding");
  if (p == NULL) {
    mp->encoding = dbgstrdup("7bit");
  } else {
    mp->encoding = dbgstrdup(p);
    Mfv_CompressHeader(mp->encoding, FALSE, TRUE);
    Mfv_ToLower(mp->encoding);
  }

  p = Mfv_GetHeader(interp, mesgPtr, "content-id");
  if (p == NULL) {
    mp->id = NULL;
  } else {
    mp->id = dbgstrdup(p);
    Mfv_CompressHeader(mp->id, FALSE, TRUE);
  }

  p = Mfv_GetHeader(interp, mesgPtr, "content-description");
  if (p == NULL) {
    mp->descr = NULL;
  } else {
    mp->descr = dbgstrdup(p);
    Mfv_CompressHeader(mp->descr, FALSE, TRUE);
  }
  
  p = Mfv_GetHeader(interp, mesgPtr, "content-disposition");
  if (p == NULL) {
    mp->dispos = NULL;
  } else {
    mp->dispos = dbgstrdup(p);
    Mfv_CompressHeader(mp->dispos, FALSE, TRUE);
  }

  mp->subparts = NULL;
  mp->subcount = 0;
  if (strcmp(mp->type,"multipart")==0) {
    if (Mfv_ParseMultiPart(interp, mesgPtr, mp, NULL,
	NULL, MIME_MULTI) != TCL_OK) {
      mimeabort:
      Mfv_FreeMimePart(mp);
      mesgPtr->mimepart = NULL;
      return TCL_ERROR;
    }
  } else if (strcmp(mp->type,"message")==0 &&
      strcmp(mp->subtype,"rfc822")==0) {
    if (Mfv_ParseMultiPart(interp, mesgPtr, mp, NULL,
	"00this_is just_a line_I shoud_never_see##", MIME_RFC822) != TCL_OK)
      goto mimeabort;
  }

  return TCL_OK;
}

MimePart *Mfv_GetMimePart(Tcl_Interp *interp, MimePart *mp, char *label)
{
  int part;
  char *p;
  MimePart **smpp;

  if (mp == NULL) return NULL;

  if ((p = strchr(label, '.')) != NULL) *p = 0;
  if (Tcl_GetInt(interp, label, &part) != TCL_OK) return NULL;
  if (p) *p = '.';

  if (part < 1) return mp;
  if ((smpp = mp->subparts) && (--part < mp->subcount)) {
    if (p) return Mfv_GetMimePart(interp, smpp[part], p+1);
    else return smpp[part];
  } else return NULL; 
}

int Mfv_MimeGetList(Tcl_Interp *interp, MimePart *mp, int lvl, char *label)
{
  int i;
  char newlabel[200], *p;
  MimePart **smpp;

  if (lvl) Tcl_AppendResult(interp, "{", (char *)NULL);
  if (*label)
    Tcl_AppendResult(interp, label, " ", (char *)NULL);
  else
    Tcl_AppendResult(interp, "0 ", (char *)NULL);

  Tcl_AppendResult(interp, mp->type, " ", mp->subtype, (char *)NULL);

  if (smpp = mp->subparts) {
    strcpy(newlabel,label);
    if (*label) strcat(newlabel,".");
    p = newlabel + strlen(newlabel);
    Tcl_AppendResult(interp, " {", (char *)NULL);
    for (i = 0; *smpp && i < mp->subcount; ++i, ++smpp) {
      if (i) Tcl_AppendResult(interp, " ", (char *)NULL);
      sprintf(p,"%d",i+1);
      Mfv_MimeGetList(interp, *smpp, lvl+1, newlabel);
    }
    Tcl_AppendResult(interp, "}", (char *)NULL);
  }
  if (lvl) Tcl_AppendResult(interp, "}", (char *)NULL);

  return TCL_OK;
}

int Mfv_MimeList(Tcl_Interp *interp, Mesg *mesgPtr)
{
  if (Mfv_ParseMimeMesg(interp, mesgPtr) != TCL_OK) return TCL_ERROR;
  return Mfv_MimeGetList(interp, mesgPtr->mimepart, 0, emptyString);
}

int Mfv_MimeGetOutline(Tcl_Interp *interp, MimePart *mp, int lvl, char *label)
{
  int i;
  char newlabel[200], *p;
  MimePart **smpp;

  if (lvl) {
    Tcl_AppendResult(interp, "\n", (char *)NULL);
    for (i = 0; i < lvl; ++i)
      Tcl_AppendResult(interp, "   ", (char *)NULL);
  }
  if (*label)
    Tcl_AppendResult(interp, label, " ", (char *)NULL);
  Tcl_AppendResult(interp, mp->type, "/", mp->subtype, (char *)NULL);

  if (smpp = mp->subparts) {
    strcpy(newlabel,label);
    if (*label) strcat(newlabel,".");
    p = newlabel + strlen(newlabel);
    for (i = 0; *smpp && i < mp->subcount; ++i, ++smpp) {
      sprintf(p,"%d",i+1);
      Mfv_MimeGetOutline(interp, *smpp, lvl+1, newlabel);
    }
  }

  return TCL_OK;
}

int Mfv_MimeOutline(Tcl_Interp *interp, Mesg *mesgPtr)
{
  if (Mfv_ParseMimeMesg(interp, mesgPtr) != TCL_OK) return TCL_ERROR;
  return Mfv_MimeGetOutline(interp, mesgPtr->mimepart, 0, emptyString);
}

char *Mfv_GetFullname( char *s )
{
  char *buf, *p1, *p2, inquote, found;
  size_t len;

  if (!s)
    return NULL;

  buf = NULL;
  inquote = found = FALSE;
  for (p2=s; *p2 && (inquote || *p2 != '<'); p2++)
    if (*p2 == '"') inquote = !inquote;

  if (*p2) {
    for (p2--; ((isspace(*p2) || *p2 == '"') && p2 > s); p2--);
    p2++;
    for (p1=s; ((isspace(*p1) || *p1 == '"') && p2 > p1); p1++);
    found = TRUE;
  } else {
    inquote = FALSE;
    for (p1=s; *p1 && (inquote || *p1 != '('); p1++)
      if (*p1 == '"') inquote = !inquote;
    if (*p1) {
      for (p1++; *p1 && isspace(*p1); p1++);
      for (p2=p1; *p2 && *p2 != ')'; p2++);
      for (p2--; isspace(*p2) && p2 > p1; p2--);
      p2++;
      found = TRUE;
    }
  }

  if (found && p2 > p1) {
    len = p2 - p1;
    buf = dbgalloc(sizeof(char)*(len+2));
    strncpy(buf, p1, len);
    buf[len] = 0;
    return buf;
  }

  return buf;
}

int Mfv_ParseHeader(Tcl_Interp *interp, FILE *ifp,
    Mesg *mesgPtr, int *mesg_lines, int *mesg_chars)
{
  int i, lines, chars, len, new, fieldchars;
  char *p1, *p2, *p3, *cptr, field[MAXFIELD];
  Tcl_HashEntry *entryPtr;
  Tcl_HashTable *hdrTblPtr;
  struct tm tms;

  /* Should already be initialized */
  hdrTblPtr = &(mesgPtr->headerTable);

  /* at this point tmpLine already contains SM line so parse it */
  lines=0;
  chars=strlen(tmpLine);

  if (chars > 0) {
    lines++;

    /* copy parts of SM line we want to store: guarantee a "sm-from" in hash */
    cptr = tmpLine+5;
    if (isspace(*cptr)) {
      cptr--; *cptr = '?';
    }
    p1 = mesgPtr->smBuf = dbgstrdup(cptr);
    entryPtr = Tcl_CreateHashEntry(hdrTblPtr, "sm-from", &new);
    Tcl_SetHashValue(entryPtr, p1);

    cptr = p1;
    if ((p2 = strchr(p1,' ')) != NULL) {
      *p2 = 0;

      /* find start of date string */
      for (p1 = p2+1; *p1 == ' '; p1++);

      /* convert date string to GMT seconds */
      memset((char *)&tms, 0, sizeof(struct tm));
      if (Mfv_GetTimeFromStr( p1, &tms, 0 ))
	mesgPtr->time_received = timelocal(&tms);
      else
	mesgPtr->time_received = 0;
      
      for (i=1; i < 5; ++i) {
	if ((p2 = strchr(p1,' ')) == NULL) break;
	*p2 = 0;
	entryPtr = Tcl_CreateHashEntry(hdrTblPtr, smHeaders[i], &new);
	Tcl_SetHashValue(entryPtr, p1);
	for (p1 = p2+1; *p1 == ' '; p1++); /* find next token */
      }
      if ( i < 5 ) {
	while ( i < 6 ) {
	  entryPtr = Tcl_CreateHashEntry(hdrTblPtr, smHeaders[i], &new);
	  Tcl_SetHashValue(entryPtr, emptyString);
	}
	fprintf(stderr, "SM-from parse failed for mesg %d\n", mesgPtr->num);
      } else {
	/* trim space off end of year */
	p2 = p1 + strlen(p1) - 1;
	while ( isspace(*p2) && p2 > p1 ) --p2;
	*(++p2) = 0;
	entryPtr = Tcl_CreateHashEntry(hdrTblPtr, "sm-year", &new);
	Tcl_SetHashValue(entryPtr, p1);
      }
    }
  }

  /* parse rest of header to determine size to allocate to store header hash */
  fieldchars = 0;
  while (fgets(tmpLine,MAXLINE,ifp) != NULL) {
    lines++;
    chars += strlen(tmpLine);
    /* empty line signals end of headers */
    if (Mfv_IsEmpty(tmpLine)) break;
    if (strncasecmp(tmpLine,"received",8) == 0) continue; /* ignore Received: fields */
    if (!isspace(*tmpLine) &&
	(p1 = strchr(tmpLine,':')) != NULL) fieldchars += p1-tmpLine; 
  }

  mesgPtr->begbody = mesgPtr->endhead = ftell(ifp);
  *mesg_lines = lines;
  *mesg_chars = chars;

  /* create a header hash for message large enough to store them all */
  mesgPtr->hdrLen = chars - fieldchars;
  mesgPtr->hdrBuf = dbgalloc(sizeof(char)*(mesgPtr->hdrLen + 2));

  cptr = mesgPtr->hdrBuf;
  *cptr = *field = 0;
  fseek(ifp, mesgPtr->beghead, SEEK_SET);
  while (fgets(tmpLine,MAXLINE,ifp) != NULL) {
    if (ftell(ifp) >= mesgPtr->endhead) break;

    if (!isspace(*tmpLine) &&
	(p1 = strchr(tmpLine,':')) != NULL) {

      /* make sure it is valid field */
      for (p2=tmpLine; p2 < p1 && *p2 != 0 && !isspace(*p2); ++p2);
      if (p2 < p1) goto notfield;

      if (*field != 0) { /* add last field to hash */
	if (strncmp(field,"received",strlen(field)) != 0) { /* ignore Received: fields */
	  entryPtr = Tcl_CreateHashEntry(hdrTblPtr, field, &new);
	  Tcl_SetHashValue(entryPtr, cptr);
	}
	cptr += strlen(cptr)+1;
	*cptr = *field = 0;
      }
	
      for (p2=p1-1; isspace(*p2); p2--);
      *(++p2) = 0;
      strncpy(field, tmpLine, MAXFIELD);
      Mfv_ToLower(field);
    } else {
      notfield:
      p1 = tmpLine;
    }

    if (*field == 0) continue;

    /* add line to previous field */
    for (p1++; isspace(*p1); p1++);
    p2 = strrchr(p1,'\n');
    if (p2 != NULL) *p2 = ' ';
    strncat(cptr,p1,MAXLINE);
  }

  if (*field != 0) { /* add last field to hash */
    entryPtr = Tcl_CreateHashEntry(hdrTblPtr, field, &new);
    Tcl_SetHashValue(entryPtr, cptr);
  }

  /* determine time sent in GMT seconds */
  p1 = Mfv_GetHeader(interp, mesgPtr, "date");
  if (p1 != NULL) mesgPtr->time_sent = Mfv_TimeParse(p1);
  if (mesgPtr->time_sent < 0)
    mesgPtr->time_sent = mesgPtr->time_received;

  /* determine status flags */
  p1 = Mfv_GetHeader(interp, mesgPtr, "status");
  if (p1!=NULL) {
    if (strchr(p1,'o')!=NULL || strchr(p1,'O')!=NULL)
      mesgPtr->flags |= MFV_OLD;
    if (strchr(p1,'r')!=NULL || strchr(p1,'R')!=NULL)
      mesgPtr->flags |= MFV_READ;
  }
  p1 = Mfv_GetHeader(interp, mesgPtr, "x-status");
  if (p1!=NULL) {
    if (strchr(p1,'a')!=NULL || strchr(p1,'A')!=NULL)
      mesgPtr->flags |= MFV_ANSWERED;
    if (strchr(p1,'f')!=NULL || strchr(p1,'F')!=NULL)
      mesgPtr->flags |= MFV_FORWARDED;
  }
  p1 = Mfv_GetHeader(interp, mesgPtr, "replied");
  if (p1!=NULL) mesgPtr->flags |= MFV_ANSWERED;

  mesgPtr->origflags = mesgPtr->flags;

  /* try to determine a full name for sender */
  entryPtr = Tcl_CreateHashEntry(hdrTblPtr, "fullname", &new);
  p1 = Mfv_GetHeader(interp, mesgPtr, "from");
  cptr = mesgPtr->nameBuf = Mfv_GetFullname(p1);

  if (cptr == NULL) {
    cptr = p1;
    if (cptr == NULL) cptr = Mfv_GetHeader(interp, mesgPtr, "return-path");
    if (cptr == NULL) cptr = Mfv_GetHeader(interp, mesgPtr, "reply-to");
    if (cptr == NULL) cptr = Mfv_GetHeader(interp, mesgPtr, "sm-from");
    if (cptr == NULL) {
      fprintf(stderr, "MFV error : can't get name of sender\n");
      cptr = "Unknown";
    }
  }
  Tcl_SetHashValue(entryPtr, cptr);
  return TCL_OK;
}

/* Fake a sm-from line */
void Mfv_FakeSMLine(Tcl_Interp *interp, Mesg *mesgPtr, time_t *t)
{
  char *cptr, *p, *addr;
  int new, len;
  Tcl_HashEntry *entryPtr;
  Tcl_HashTable *hdrTblPtr;
  struct tm *tm;

  /* find address of sender */
  addr = Mfv_GetHeader(interp, mesgPtr, "return-path");
  if ( addr == NULL ) addr = Mfv_GetHeader(interp, mesgPtr, "reply-to");
  if ( addr == NULL ) addr = "Unknown";
  /* TODO: parse addr for pure email address */

  dbgfree(mesgPtr->smBuf);
  mesgPtr->smBuf = cptr = dbgalloc(sizeof(char)*(strlen(addr)+50));
  strcpy(cptr,"From ");
  strcat(cptr, addr);

  hdrTblPtr = &(mesgPtr->headerTable);
  entryPtr = Tcl_CreateHashEntry(hdrTblPtr, "sm-from", &new);
  Tcl_SetHashValue(entryPtr, cptr+5);

  /* trim the address just written to cptr */
  p = cptr + strlen(cptr) - 1;
  while ( isspace(*p) && p > cptr ) --p;
  *(++p) = 0;

  tm = localtime(t);

  len = strlen(cptr) + 1;
  p = cptr + len;
  strftime( p, MAXLINE - len, "%a", tm);
  entryPtr = Tcl_CreateHashEntry(hdrTblPtr, "sm-wday", &new);
  Tcl_SetHashValue(entryPtr, p);

  len += strlen(p) + 1;
  p = cptr + len;
  strftime( p, MAXLINE - len, "%b", tm);
  entryPtr = Tcl_CreateHashEntry(hdrTblPtr, "sm-month", &new);
  Tcl_SetHashValue(entryPtr, p);

  len += strlen(p) + 1;
  p = cptr + len;
  strftime( p, MAXLINE - len, "%d", tm);
  entryPtr = Tcl_CreateHashEntry(hdrTblPtr, "sm-mday", &new);
  Tcl_SetHashValue(entryPtr, p);

  len += strlen(p) + 1;
  p = cptr + len;
  strftime( p, MAXLINE - len, "%T", tm);
  entryPtr = Tcl_CreateHashEntry(hdrTblPtr, "sm-time", &new);
  Tcl_SetHashValue(entryPtr, p);

  len += strlen(p) + 1;
  p = cptr + len;
  strftime( p, MAXLINE - len, "%Y", tm);
  entryPtr = Tcl_CreateHashEntry(hdrTblPtr, "sm-year", &new);
  Tcl_SetHashValue(entryPtr, p);
}

int Mfv_ParseAppendMH(Tcl_Interp *interp, Folder *folderPtr)
{
  int new, mesg_num, mesg_lines, mesg_chars, real_num;
  char *cwd;
  struct stat statbuf;
  Tcl_HashEntry *entryPtr;
  Tcl_HashTable *mesgTblPtr, *hdrTblPtr;
  Mesg *mesgPtr;
  FILE *ifp;
  struct dirent *dp;
  DIR * dd;

  mesgTblPtr = &(folderPtr->mesgTable);
  entryPtr = NULL;
  mesgPtr = NULL;
  mesg_num=0;
  mesg_lines=0;
  mesg_chars=0;

  sprintf(tmpFile, "%s%c%s", folderPtr->dir, dirsep, folderPtr->name );
  if ((dd = opendir(tmpFile)) == NULL) return -1;
  if (fstat(dd->dd_fd, &statbuf) == -1 ) return -1;

  if ((cwd = Mfv_GetCWD()) == NULL) return -1;
  if (CHDIR(tmpFile)!=0) {
    dbgfree(cwd);
    return -1;
  }

  while( dp = readdir(dd) ) {
    if ( (real_num = atoi(dp->d_name)) > 0 ) {

      if(stat(dp->d_name, &statbuf) == -1) {
	fprintf(stderr, "MFV: could not stat %s\n", dp->d_name);
	continue;
      }
      if(statbuf.st_size == 0) {
	fprintf(stderr, "MFV: %s is empty\n", dp->d_name);
	continue;
      }

      if ((entryPtr =
	  Tcl_FindHashEntry(mesgTblPtr, (void *)real_num)) != NULL) {
	mesgPtr = (Mesg *) Tcl_GetHashValue(entryPtr);
	if (statbuf.st_mtime == mesgPtr->mtime) continue;
	folderPtr->count--; /* reread message, but don't count twice */
      }

      if ((ifp = fopen(dp->d_name, "r")) == NULL) {
	fprintf(stderr, "MFV: could not stat %s\n", dp->d_name);
	continue;
      }

      entryPtr = Tcl_CreateHashEntry(mesgTblPtr, (void *)real_num, &new);
      if (new) {
	mesg_num++;
	mesgPtr = (Mesg *) dbgalloc(sizeof(Mesg));
	Tcl_SetHashValue(entryPtr, mesgPtr);
      } else {
	mesgPtr = (Mesg *) Tcl_GetHashValue(entryPtr);
	dbgfree(mesgPtr->hdrBuf);
	dbgfree(mesgPtr->smBuf);
	dbgfree(mesgPtr->nameBuf);
	Tcl_DeleteHashTable(&(mesgPtr->headerTable));
	fprintf(stderr,
	    "MFV: %d was externally modified. Rereading.\n", real_num);
      }

      mesgPtr->folder = folderPtr;
      mesgPtr->num = mesgPtr->accessnum = real_num;
      mesgPtr->flags = 0;
      mesgPtr->begmesg = mesgPtr->beghead = 0;
      mesgPtr->begbody = mesgPtr->endhead = -1;
      mesgPtr->mtime = statbuf.st_mtime;
      mesgPtr->time_received = statbuf.st_mtime;
      mesgPtr->time_sent = -1;
      mesgPtr->links = NULL;
      mesgPtr->mimepart = NULL;
      mesgPtr->replies = NULL;
      mesgPtr->replyto = NULL;
      mesgPtr->smBuf = mesgPtr->hdrBuf = mesgPtr->nameBuf = NULL;
      hdrTblPtr = &(mesgPtr->headerTable);
      Tcl_InitHashTable(hdrTblPtr, TCL_STRING_KEYS);

      *tmpLine = 0; /* empty tmpLine to prevent sm-from parsing */
      Mfv_ParseHeader(interp,ifp,mesgPtr,&mesg_lines,&mesg_chars);

      /* Fake a sm-from line */
      mesgPtr->time_received = mesgPtr->time_sent;
      Mfv_FakeSMLine(interp, mesgPtr, &mesgPtr->time_received);

      /* ignore Status: in MH messages, TODO: use .mh_sequences */
      mesgPtr->flags |= (MFV_OLD | MFV_READ);

      /* count rest of lines */
      while (fgets(tmpLine,MAXLINE,ifp) != NULL) mesg_lines++;
      mesgPtr->lines = mesg_lines;
      mesgPtr->chars = statbuf.st_size;
      mesgPtr->endmesg = mesgPtr->endbody = statbuf.st_size;

      folderPtr->count++; /* Its official! */
      folderPtr->max = MAX(real_num, folderPtr->max);
      folderPtr->threaded = FALSE;
    }
  }
  closedir(dd);

  if (CHDIR(cwd)!=0) {
    dbgfree(cwd);
    return -1;
  }
  dbgfree(cwd);

  return mesg_num;
}

int Mfv_ParseAppend(Tcl_Interp *interp, FILE *ifp, Folder *folderPtr)
{
  int new, mesg_num, mesg_lines, mesg_chars, real_num, oldmax;
  char newstart, *p, fakesm;
  long filepos, headstart;
  Tcl_HashEntry *entryPtr;
  Tcl_HashTable *mesgTblPtr;
  Mesg *mesgPtr;

  if ( folderPtr->type == MFV_MH )
    return Mfv_ParseAppendMH(interp, folderPtr);

  mesgTblPtr = &(folderPtr->mesgTable);
  entryPtr = NULL;
  mesgPtr = NULL;
  mesg_num=0;
  mesg_lines=0;
  mesg_chars=0;
  filepos=ftell(ifp);
  oldmax = folderPtr->max;
  fakesm = FALSE;

  while (fgets(tmpLine,MAXLINE,ifp) != NULL) {

    /* Determine if current mesg ends & new mesg begins */
    if (folderPtr->type == MFV_BSD) {
      if (newstart =  Mfv_IsNewMesg(tmpLine)) {
	if (mesgPtr && mesg_num) {
	  mesgPtr->endmesg = mesgPtr->endbody = filepos;
	}
	headstart = filepos;
      }
    } else if (folderPtr->type == MFV_MMDF) {
      if (newstart = (strncmp(mmdfSep,tmpLine,4) == 0)) {
	if (mesgPtr && mesg_num) {
	  mesgPtr->endbody = filepos;
	  filepos = mesgPtr->endmesg = ftell(ifp);
	  do { /* find start of next messages */
	    p = fgets(tmpLine,MAXLINE,ifp);
	  } while (p != NULL && strncmp(mmdfSep,tmpLine,4) != 0);
	  if (p == NULL) break;
	}
	headstart = ftell(ifp);
	/* Read sm-from line into tmpLine for Mfv_ParseHeader */
	p = fgets(tmpLine,MAXLINE,ifp);
	if ( p == NULL || !Mfv_IsNewMesg(tmpLine)) {
	  fakesm = TRUE;
	  fseek(ifp, headstart, SEEK_SET);
	  *tmpLine = 0;
	}
      }
    } else {
      return -1;
    }

    /* If a newstart, tmpLine should at this point contain the sm-from line */
    if (newstart) {
      if (mesgPtr && mesg_num) {
	mesgPtr->lines  = mesg_lines;
	mesgPtr->chars  = mesg_chars;
      }
      mesg_num++;
      real_num = mesg_num + oldmax;
      entryPtr = Tcl_CreateHashEntry(mesgTblPtr, (void *)real_num, &new);
      if (!new) fprintf(stderr,
	  "serious error in mfv_open: duplicate mesgs in hash table.\n");
      mesgPtr = (Mesg *) dbgalloc(sizeof(Mesg));
      Tcl_SetHashValue(entryPtr, mesgPtr);

      mesgPtr->folder = folderPtr;
      mesgPtr->num = mesgPtr->accessnum = real_num;
      mesgPtr->flags = 0;
      mesgPtr->begmesg = filepos;
      mesgPtr->beghead = headstart;
      mesgPtr->begbody = mesgPtr->endhead = -1;
      mesgPtr->endbody = -1; /* This is special for last mesg -- see below */
      mesgPtr->time_received = folderPtr->ctime;
      mesgPtr->time_sent = -1;
      mesgPtr->links = NULL;
      mesgPtr->mimepart = NULL;
      mesgPtr->replies = NULL;
      mesgPtr->replyto = NULL;
      mesgPtr->smBuf = mesgPtr->hdrBuf = mesgPtr->nameBuf = NULL;

      Tcl_InitHashTable(&(mesgPtr->headerTable), TCL_STRING_KEYS);
      Mfv_ParseHeader(interp,ifp,mesgPtr,&mesg_lines,&mesg_chars);

      if (fakesm || Mfv_GetHeader(interp, mesgPtr, "sm-from") == NULL) {
	mesgPtr->time_received = mesgPtr->time_sent;
	Mfv_FakeSMLine(interp, mesgPtr, &mesgPtr->time_received);
	fakesm = FALSE;
      }

      folderPtr->count++; /* Its now official! */
      folderPtr->max = real_num;

      filepos=ftell(ifp);
      continue;
    }
    mesg_lines++;
    mesg_chars += strlen(tmpLine);
    filepos=ftell(ifp);
  }

  if (mesg_num) {
    folderPtr->threaded = FALSE;
    
    if (folderPtr->type == MFV_BSD) {
      mesgPtr->endmesg = mesgPtr->endbody = filepos;
    } else if (folderPtr->type == MFV_MMDF) {
      if (mesgPtr->endbody < 0) {
	mesgPtr->endmesg = mesgPtr->endbody = filepos;
      }
    }
    mesgPtr->lines  = mesg_lines;
    mesgPtr->chars  = mesg_chars;
  }

  return mesg_num;
}

int Mfv_AppendVFLink(Tcl_Interp *interp, Folder *folderPtr, Mesg *mesgPtr)
{
  int new;
  VFLink *lastVF, *newVF;
  Tcl_HashEntry *entryPtr;

  if ( folderPtr->type != MFV_VIRTUAL ) return 0;

  folderPtr->count++;
  folderPtr->max++;
  entryPtr = Tcl_CreateHashEntry(&(folderPtr->mesgTable),
      (void *)(folderPtr->max), &new);
  if (!new) fprintf(stderr,
      "serious error in mfv_open: duplicate mesgs in hash table.\n");
  Tcl_SetHashValue(entryPtr, mesgPtr);

  newVF = (VFLink *) dbgalloc(sizeof(VFLink));
  newVF->folder = folderPtr;
  newVF->num = folderPtr->max;
  newVF->next = NULL;

  if (lastVF = mesgPtr->links) {
    while (lastVF->next != NULL) lastVF = lastVF->next;
    lastVF->next = newVF;
  } else {
    mesgPtr->links = newVF;
  }

  return folderPtr->max;
}

int Mfv_SetCmd(ClientData cd, Tcl_Interp *interp, int argc, char *argv[])
{
  char c, *p, *dir, **listArgv;
  int i, j, type, listArgc;
  size_t length;
  Tcl_DString buffer;

  if (argc < 2) {
    Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                " option ?arg ...?\"", (char *) NULL);
    return TCL_ERROR;
  }

  c = argv[1][0];
  length = strlen(argv[1]);

  if ((c == 'd') && (strncmp(argv[1], "defaulttype", length) == 0)) {
    if (argc == 2) {
      Tcl_AppendResult(interp, folderTypes[defaultType], (char *) NULL);
    } else if (argc == 3) {
      if ((type = Mfv_StringToType(argv[2])) == 0) {
	Tcl_AppendResult(interp, "unrecognized folder format type \"", argv[2],
	    "\"", (char *) NULL);
	return TCL_ERROR;
      } else {
	defaultType = type;
      }
    } else {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " defaulttype ?formatType?\"", (char *) NULL);
      return TCL_ERROR;
    }
  } else if ((c == 'k') && (strncmp(argv[1], "keyprefix", length) == 0)) {
    if (argc > 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " ", argv[1], " ?prefixString?\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (argc == 3) {
      for (p = argv[2]; *p != 0; p++) {
	if (!(isalnum(*p) || *p == '_')) {
	  Tcl_AppendResult(interp, "invalid prefix string \"",
	      argv[2], "\"", (char *) NULL);
	  return TCL_ERROR;
	} 
      }
      dbgfree(keyPrefix);
      keyPrefix = dbgalloc(sizeof(char)*(strlen(argv[2])+2));
      strcpy(keyPrefix,argv[2]);
    } else {
      Tcl_AppendResult(interp, keyPrefix, (char *) NULL);
    }
  } else if ((c == 'l') && (strncmp(argv[1], "lock", length) == 0)) {
    if (argc < 3) {
      wronglockargs:
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " lock option ?setting?\"", (char *) NULL);
      return TCL_ERROR;
    }
    c = argv[2][0];
    length = strlen(argv[2]);

    if ((c == 'd') && (strncmp(argv[2], "dotlock", length) == 0)) {
      if (argc == 3) {
	Tcl_AppendResult(interp, dotlockprog, (char *) NULL);
      } else if (argc == 4) {
	dbgfree(dotlockprog);
	dotlockprog = dbgalloc(sizeof(char)*(strlen(argv[3])+2));
	strcpy(dotlockprog,argv[3]);
      } else goto wronglockargs;
    } else if ((c == 'r') && (strncmp(argv[2], "retries", length) == 0)) {
      if (argc == 3) {
	sprintf(interp->result, "%d", lock_retries);
      } else if (argc == 4) {
	if (Tcl_GetInt(interp, argv[3], &i) != TCL_OK) return TCL_ERROR;
	lock_retries = i;
      } else goto wronglockargs;
    } else if ((c == 't') && (strncmp(argv[2], "timeout", length) == 0)) {
      if (argc == 3) {
	sprintf(interp->result, "%d", lock_timeout);
      } else if (argc == 4) {
	if (Tcl_GetInt(interp, argv[3], &i) != TCL_OK) return TCL_ERROR;
	if (i < 1) {
	  Tcl_AppendResult(interp,
	      "timeout cannot be less than one", (char *)NULL);
	  return TCL_ERROR;
	}
	lock_timeout = i;
      } else goto wronglockargs;
    } else {
      Tcl_AppendResult(interp, "bad lock option \"", argv[1],
	"\": should be dotlock, retries, or timeout", (char *) NULL);
      return TCL_ERROR;
    }
  } else if ((c == 'm') && (strncmp(argv[1], "mailspool", length) == 0)) {
    if (argc > 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " ", argv[1], " ?filename?\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (argc == 3) {
      if ((p = Tcl_TildeSubst(interp, argv[2], &buffer)) == NULL) {
	return TCL_ERROR;
      }
      if ((dir = Mfv_Fullpath(p, TRUE)) == NULL) {
	Tcl_AppendResult(interp, "invalid directory ", p, (char *)NULL);
	Tcl_DStringFree(&buffer);
	return TCL_ERROR;
      }
      dbgfree(mailSpool);
      mailSpool = dir;
      Tcl_DStringFree(&buffer);
    } else {
      if (mailSpool) Tcl_AppendResult(interp, mailSpool, (char *) NULL);
    }
  } else if ((c == 'n') && (strncmp(argv[1], "noempty", length) == 0)) {
    if (argc == 2) {
      sprintf(interp->result, ( delEmpty ? "1" : "0" ));
    } else if (argc == 3) {
      if (Tcl_GetBoolean(interp, argv[2], &type) != TCL_OK) {
	return TCL_ERROR;
      } else {
	delEmpty = type;
      }
    } else {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " noempty ?boolean?\"", (char *) NULL);
      return TCL_ERROR;
    }
  } else if ((c == 'r') && (strncmp(argv[1], "retain", length) == 0)) {
    if (argc > 3) {
      badfilterargs:
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " ", argv[1], " ?filterList?\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (argc == 3) {
      filterStrip = FALSE;
      dbgfree((char *)filterHeaders);
      if (Tcl_SplitList(interp, argv[2], &filterCount, &filterHeaders) != TCL_OK) {
	return TCL_ERROR;
      }
    } else if (!filterStrip) {
      Tcl_SetResult(interp, Tcl_Merge(filterCount, filterHeaders), TCL_DYNAMIC);
    }
  } else if ((c == 's') && (strncmp(argv[1], "strip", length) == 0)
	     && length > 2) {
    if (argc > 3) goto badfilterargs;
    if (argc == 3) {
      filterStrip = TRUE;
      dbgfree((char *)filterHeaders);
      if (Tcl_SplitList(interp, argv[2], &filterCount, &filterHeaders) != TCL_OK) {
	return TCL_ERROR;
      }
    } else if (filterStrip) {
      Tcl_SetResult(interp, Tcl_Merge(filterCount, filterHeaders), TCL_DYNAMIC);
    }
  } else if ((c == 's') && (strncmp(argv[1], "stupidformat", length) == 0)
	     && length > 2) {
    if (argc == 2) {
      sprintf(interp->result, ( stupidformat ? "1" : "0" ));
    } else if (argc == 3) {
      if (Tcl_GetBoolean(interp, argv[2], &type) != TCL_OK) {
	return TCL_ERROR;
      } else {
	stupidformat = type;
      }
    } else {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " stupidformat ?boolean?\"", (char *) NULL);
      return TCL_ERROR;
    }
  } else if ((c == 's') && (strncmp(argv[1], "sumformat", length) == 0)
	     && length > 1) {
    if (argc > 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " ", argv[1], " ?formatString?\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (argc ==3 )
      return Mfv_MakeFmtLine(interp,argv[2]);
    else
      Tcl_AppendResult(interp, fmtLine, (char *) NULL);
  } else if ((c == 't') && (strncmp(argv[1], "tmpdir", length) == 0)) {
    if (argc > 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " ", argv[1], " ?directory?\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (argc == 3) {
      if ((dir = Tcl_TildeSubst(interp, argv[2], &buffer)) == NULL) {
	return TCL_ERROR;
      }
      if (!strlen(dir) || access(dir,W_OK) != 0) {
	Tcl_DStringFree(&buffer);
	Tcl_AppendResult(interp, "invalid/unwritable directory", (char *)NULL);
	return TCL_ERROR;
      }
      dbgfree(tmpDir);
      tmpDir = dbgalloc(sizeof(char)*(strlen(dir)+2));
      strcpy(tmpDir,dir);
      Tcl_DStringFree(&buffer);
    } else {
      if (tmpDir) Tcl_AppendResult(interp, tmpDir, (char *) NULL);
    }
  } else {
    Tcl_AppendResult(interp, "bad option \"", argv[1],
	"\": should be defaulttype, inbox, noempty, keyprefix, retain, ",
	"strip, sumformat or tmpdir", (char *) NULL);
    return TCL_ERROR;
  }

  return TCL_OK;
}

int Mfv_CheckAppendMH(Tcl_Interp *interp, Folder *folderPtr)
{
  int cnt;
  struct stat statbuf;

  /* Process appended messages */
  if ((cnt = Mfv_ParseAppendMH(interp, folderPtr)) < 0) {
    return -1;
  }

  sprintf(tmpFile, "%s%c%s", folderPtr->dir, dirsep, folderPtr->name);
  if(stat(tmpFile, &statbuf) == -1) {
    return -3; /* someone deleted file during parsing? */
  } else {
    folderPtr->dev = statbuf.st_dev;
    folderPtr->ino = statbuf.st_ino;
    folderPtr->size = statbuf.st_size;
    folderPtr->ctime = statbuf.st_ctime;
  }

  return cnt;
}

int Mfv_CheckAppend(Tcl_Interp *interp, Folder *folderPtr)
{
  int cnt;
  FILE *ifp;
  char *cptr;
  Mesg *mesgPtr;
  struct stat statbuf;

  if (folderPtr->type == MFV_VIRTUAL) return 0;

  sprintf(tmpFile, "%s%c%s", folderPtr->dir, dirsep, folderPtr->name);

  if (folderPtr->fp) {
    fclose(folderPtr->fp); /* certain filesys require file to be */
    folderPtr->fp = NULL;  /* closed in order to get updated stat. Yuck! */
  }

  if(stat(tmpFile, &statbuf) == -1) {
    if (errno == ENOENT) {
      if (folderPtr->type == MFV_UNKNOWN) return 0;
    }
    /* if stat fails, someone has probably deleted the folder */
    return -1;
  }

  if (S_ISDIR(statbuf.st_mode)) {
    if ( folderPtr->type == MFV_UNKNOWN )
      folderPtr->type = MFV_MH;
    if ( folderPtr->type == MFV_MH )
      return Mfv_CheckAppendMH(interp, folderPtr);
    else
      return -2;
  } else if (!S_ISREG(statbuf.st_mode) || folderPtr->type == MFV_MH ) {
    return -3;
  }

  if ((folderPtr->fp = fopen(tmpFile, (folderPtr->readonly ? "r" : "r+"))) == NULL)
    return -4;

  if ( folderPtr->size != statbuf.st_size || folderPtr->ctime != statbuf.st_ctime ) {
    ifp = folderPtr->fp;
    if (folderPtr->count) { /* check if original messages are corrupted */
      mesgPtr = Mfv_GetMesg(interp, folderPtr, 1);
      fseek(ifp, mesgPtr->begmesg, SEEK_SET);
      cptr = fgets(tmpLine,MAXLINE,ifp);
      if (cptr == NULL) return -5;
      if (folderPtr->type == MFV_BSD && !Mfv_IsNewMesg(tmpLine)) {
	return -6;
      } else if (folderPtr->type == MFV_MMDF &&
	  strncmp(mmdfSep,tmpLine,4) != 0) {
	return -7;
      }
      if (folderPtr->count > 1) {
	mesgPtr = Mfv_GetMesg(interp, folderPtr, folderPtr->count);
	fseek(ifp, mesgPtr->begmesg, SEEK_SET);
	cptr = fgets(tmpLine,MAXLINE,ifp);
	if (cptr == NULL) return -8;
	if (folderPtr->type == MFV_BSD && !Mfv_IsNewMesg(tmpLine)) {
	  return -9;
	} else if (folderPtr->type == MFV_MMDF &&
	    strncmp(mmdfSep,tmpLine,4) != 0) {
	  return -10;
	}
      }
    }
    fseek(ifp, folderPtr->size, SEEK_SET);
    if (ftell(ifp) != folderPtr->size) {
      return -11; /* file has been shortened, original data corrupt */
    }
    
    if (statbuf.st_size > folderPtr->size) {
      cptr = fgets(tmpLine,MAXLINE,ifp);
      if (cptr == NULL) {
	return -12; /* No append, so original data is corrupt */
      }
      if (folderPtr->type == MFV_UNKNOWN) {
	if (strncmp(mmdfSep,tmpLine,4) == 0) {
	  folderPtr->type = MFV_MMDF;
	  cptr = fgets(tmpLine,MAXLINE,ifp);
	} else {
	  folderPtr->type = MFV_BSD;
	}
      } else if (folderPtr->type == MFV_MMDF) {
	if (strncmp(mmdfSep,tmpLine,4) != 0) return -13;
	cptr = fgets(tmpLine,MAXLINE,ifp);
	if (cptr == NULL) return -14;
      }
      if (folderPtr->type == MFV_BSD && !Mfv_IsNewMesg(tmpLine)) {
	return -15; /* Appended data is not a recognized format */
      }
      fseek(ifp, folderPtr->size, SEEK_SET);

      /* Process appended messages */
      if ((cnt = Mfv_ParseAppend(interp, ifp, folderPtr)) < 1) {
	return -16;
      }
    }
    
    fclose(folderPtr->fp); /* certain filesys require file to be */
    folderPtr->fp = NULL;  /* closed in order to get updated stat. Yuck! */

    if(stat(tmpFile, &statbuf) == -1) {
      return -17; /* someone deleted file during parsing? */
    } else {
      folderPtr->dev = statbuf.st_dev;
      folderPtr->ino = statbuf.st_ino;
      folderPtr->size = statbuf.st_size;
      folderPtr->ctime = statbuf.st_ctime;
      if ((folderPtr->fp = fopen(tmpFile, (folderPtr->readonly ? "r" : "r+"))) == NULL)
	return -18;
    }
    return cnt;
  } else {
    return 0;
  }
}

int Mfv_CreateVirtual(Tcl_Interp *interp, char *folderName)
{
  int new, result;
  Tcl_HashEntry *entryPtr;
  Folder *folderPtr;
  Tcl_HashTable *mesgTblPtr;

  result = TCL_OK;
  do {
    sprintf(interp->result, "%s%u", keyPrefix, folderId++);
    entryPtr = Tcl_CreateHashEntry(&folderTable, interp->result, &new);
  } while (!new);
  folderPtr = (Folder *) dbgalloc(sizeof(Folder));
  Tcl_SetHashValue(entryPtr, folderPtr);

  folderPtr->fp = NULL;
  folderPtr->type = MFV_VIRTUAL;
  folderPtr->key = dbgstrdup(interp->result);
  folderPtr->name = dbgstrdup(folderName);
  folderPtr->dir = dbgstrdup(emptyString); 
  folderPtr->dev = 0;
  folderPtr->ino = 0;
  folderPtr->size = 0;
  folderPtr->ctime = -1;
  folderPtr->readonly = TRUE;
  folderPtr->count = folderPtr->max = 0;

  /* expand global scratch variable for filenames if necessary */
  new = strlen(folderName) + 20;
  if ( new > tmpFileSize ) {
    dbgfree(tmpFile);
    tmpFile = dbgalloc(new*sizeof(char));
    *tmpFile = 0;
    tmpFileSize = new;
  }

  mesgTblPtr = &(folderPtr->mesgTable);
  Tcl_InitHashTable(mesgTblPtr, TCL_ONE_WORD_KEYS);

  Tcl_CreateCommand(interp, interp->result, Mfv_FolderCmd, (ClientData) NULL,
      (Tcl_CmdDeleteProc *)NULL);

  return result;
}

int Mfv_OpenFolder(Tcl_Interp *interp, char *folderName, char *hashKey,
    int locked)
{
  FILE *ifp;
  char *cptr, *fileName, *basename;
  int new, result, type;
  short readonly;
  struct stat statbuf;
  Tcl_HashEntry *entryPtr;
  Folder *folderPtr;
  Tcl_HashTable *mesgTblPtr;
  Tcl_DString buffer;
  char *cwd, *mfd;

  result = TCL_OK;
  mfd = NULL;

  fileName = Tcl_TildeSubst(interp, folderName, &buffer);
  if (fileName == NULL) {
      result = TCL_ERROR;
      goto done;
  }

  /* get full directory name and filename. NOTE: mfd gets freed at [done] */
  if ((basename = Mfv_Basename(fileName, &mfd)) == NULL) {
    Tcl_AppendResult(interp, "error getting directory for \"", fileName,
	"\"", (char *) NULL);
    result = TCL_ERROR;
    goto done;
  }

  if(stat(fileName, &statbuf) == -1) {
    if (errno == ENOENT) {
      if (folderPtr = Mfv_CheckIsOpen(0, 0, fileName)) {
	Tcl_AppendResult(interp, folderPtr->key, (char *) NULL);
	result = TCL_OK;
	goto done;
      }
      type = MFV_UNKNOWN;
      readonly = ((access(mfd, W_OK) == 0) ? 0 : 1 );
      ifp = NULL;
      statbuf.st_dev = statbuf.st_ino = 0;
      statbuf.st_size = statbuf.st_ctime = 0;
    } else {
      Tcl_AppendResult(interp, "cannot determine status of file \"", fileName,
	  "\"", (char *) NULL);
      result = TCL_ERROR;
      goto done;
    }
  } else {

    type = MFV_UNKNOWN;
    if (S_ISDIR(statbuf.st_mode)) {
      type = MFV_MH;
      ifp = NULL;
    } else {
      if (!S_ISREG(statbuf.st_mode)) {
	Tcl_AppendResult(interp, "file \"", fileName,
	    "\" is not a regular file", (char *) NULL);
	result = TCL_ERROR;
	goto done;
      }
    }

    /* check if already open and if so, return that folder command */
    if (folderPtr = Mfv_CheckIsOpen(statbuf.st_dev, statbuf.st_ino, NULL)) {
      Tcl_AppendResult(interp, folderPtr->key, (char *) NULL);
      result = TCL_OK;
      goto done;
    }

    readonly = (((statbuf.st_mode)&S_IWUSR) != S_IWUSR);
    if (readonly) fprintf(stderr, "Opening readonly\n");

    if ( type != MFV_MH ) {
      if ((ifp = fopen(fileName, (readonly ? "r" : "r+"))) == NULL) {
	Tcl_AppendResult(interp, "file \"", fileName, "\" cannot be opened",
	    (readonly ? " readonly." : " for writing."), (char *) NULL);
	result = TCL_ERROR;
	goto done;
      }
      cptr = fgets(tmpLine,MAXLINE,ifp);
      if (cptr != NULL) {
	if (strncmp(mmdfSep,tmpLine,4) == 0) {
	  type = MFV_MMDF;
	  cptr = fgets(tmpLine,MAXLINE,ifp);
	  if (cptr == NULL) {
	    badformat:
	    Tcl_AppendResult(interp, "file \"", fileName,
		"\" is not in a recognized format type for a mail folder", (char *) NULL);
	    fclose(ifp);
	    result = TCL_ERROR;
	    goto done;
	  }
	} else {
	  type = MFV_BSD;
	  if (!Mfv_IsNewMesg(tmpLine)) {
	    goto badformat;
	  }
	}
      }
      rewind(ifp);
    }
  }

  /* create new hashkey if this is not a reopen */
  if (hashKey != NULL) {
    entryPtr = Tcl_CreateHashEntry(&folderTable, hashKey, &new);
    if (!new) {
      Tcl_AppendResult(interp, "folder id \"", hashKey,
	"\" already exists", (char *) NULL);
      if (ifp) fclose(ifp);
      result = TCL_ERROR;
      goto done;
    }
    strcpy(interp->result, hashKey);
  } else {
    do {
      sprintf(interp->result, "%s%u", keyPrefix, folderId++);
      entryPtr = Tcl_CreateHashEntry(&folderTable, interp->result, &new);
    } while (!new);
  }
  folderPtr = (Folder *) dbgalloc(sizeof(Folder));
  Tcl_SetHashValue(entryPtr, folderPtr);

  folderPtr->fp = ifp;
  folderPtr->type = type;
  folderPtr->key = dbgstrdup(interp->result);
  folderPtr->name = dbgstrdup(basename);
  folderPtr->dir = mfd; 
  folderPtr->dev = statbuf.st_dev;
  folderPtr->ino = statbuf.st_ino;
  folderPtr->size = statbuf.st_size;
  folderPtr->ctime = statbuf.st_ctime;
  folderPtr->readonly = readonly;
  folderPtr->threaded = FALSE;
  if (locked) Mfv_LockFolder(folderPtr);
  else folderPtr->locked = FALSE;

  /* expand global scratch variable for filenames if necessary */
  new = strlen(folderPtr->name) + strlen(mfd) + 20;
  if ( new > tmpFileSize ) {
    dbgfree(tmpFile);
    tmpFile = dbgalloc(new*sizeof(char));
    *tmpFile = 0;
    tmpFileSize = new;
  }
  mfd = NULL; /* Prevent it from being freed at [done] since used */

  mesgTblPtr = &(folderPtr->mesgTable);
  Tcl_InitHashTable(mesgTblPtr, TCL_ONE_WORD_KEYS);

  folderPtr->count = folderPtr->max = 0;
  if (type != MFV_UNKNOWN)
    new = Mfv_ParseAppend(interp, ifp, folderPtr);
  else new = 0;

  Tcl_CreateCommand(interp, interp->result, Mfv_FolderCmd, (ClientData) NULL,
      (Tcl_CmdDeleteProc *)NULL);

  /* needs to come after Tcl_CreateCommand so Mfv_CloseFolder will work */
  if (new < 0) {
    Mfv_CloseFolder(interp,folderPtr);
    Tcl_ResetResult(interp);
    Tcl_AppendResult(interp, "folder \"", fileName,
      "\" could not be parsed properly", (char *) NULL);
    result = TCL_ERROR;
  }

  if (type != MFV_UNKNOWN && type != MFV_MH && folderPtr->size != ftell(ifp)) {
    fprintf(stderr, "WARNING: folder was modified while being opened\n");
    if(stat(fileName, &statbuf) == -1) {
      Mfv_CloseFolder(interp,folderPtr);
      Tcl_ResetResult(interp);
      Tcl_AppendResult(interp, "folder \"", fileName,
	"\" was corrupted externally during parsing", (char *) NULL);
      result = TCL_ERROR;
    } else {
      folderPtr->dev = statbuf.st_dev;
      folderPtr->ino = statbuf.st_ino;
      folderPtr->size = statbuf.st_size;
      folderPtr->ctime = statbuf.st_ctime;
    }
  }

  done:
  dbgfree(mfd);
  Tcl_DStringFree(&buffer);
  return result;
}

int Mfv_OpenCmd(ClientData cd, Tcl_Interp *interp, int argc, char *argv[])
{
  int i, virtual;
  char *key = NULL;

  if (argc < 2) {
    badopenargs:
    Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                " ?switches? fileName\"", (char *) NULL);
    return TCL_ERROR;
  }
  virtual = FALSE;
  for (i=1; i < argc && argv[i][0] == '-'; ++i ) {
    if (strcmp(argv[i],"--") == 0) {
      ++i; break;
    } else if (strcmp(argv[i],"-virtual") == 0) {
      virtual = TRUE;
    } else if (strcmp(argv[i],"-id") == 0) {
      ++i; key = argv[i];
    }
  }
  if (i != argc-1) goto badopenargs;
  if (virtual) {
    return Mfv_CreateVirtual(interp,argv[i]);
  } else {
    return Mfv_OpenFolder(interp,argv[i],key,0);
  }
}

char *Mfv_TmpName(char *pfx, char *dir)
{
  static unsigned short tmpid = 0;
  char *name;

  if (!pfx || strlen(pfx) == 0) pfx = "mfv";

  if (!dir || !strlen(dir) || access(dir,W_OK) != 0)
    dir = tmpDir;

  if (dir && strlen(dir) && access(dir,W_OK) == 0) {
    name = dbgalloc(sizeof(char)*(strlen(dir) + strlen(pfx) + 25));
    do {
      sprintf(name, "%s%c%s%05d.tmp", dir, dirsep, pfx, tmpid++);
    } while (access(name,F_OK) == 0);
  } else {
    /* Can't use dir so use tempnam */
    name = tempnam( NULL, pfx );
  }

  return(name);
}

int Mfv_CopyFile( char *fromFile, char *toFile)
{
  int ifd, ofd;
  char buf[BUFSIZ];
  int len;
  struct stat statbuf;

  if(stat(fromFile, &statbuf) == -1) return -1;
  if ((ifd = open(fromFile, O_RDONLY)) == -1) return -1;
  if ((ofd = open(toFile, O_CREAT | O_TRUNC | O_WRONLY,
      statbuf.st_mode)) == -1) return -2;

  while((len = read(ifd,buf,BUFSIZ)) > 0) {
    if (write(ofd,buf,len) != len) return -3;
  }
  if (len < 0) return -4;

  return 0;
}

int Mfv_WriteMesg(Tcl_Interp *interp, Mesg *mesgPtr, FILE *ofp,
    int type, int allowmark)
{
  FILE *ifp;
  Folder *folderPtr;
  char *cptr, *p, buf[BUFSIZ];
  long bufsize, len;
  char statstr[10], xstatstr[10], markstat, markxstat;

  folderPtr = mesgPtr->folder;

  if ((ifp = Mfv_GetMesgFilePtr(mesgPtr)) == NULL) return -1;

  if (fseek(ifp, mesgPtr->begmesg, SEEK_SET) < 0) {
    badinput:
    if (folderPtr->type == MFV_MH) fclose(ifp);
    return -1;
  }

  if (!allowmark || type == MFV_MH) {
    markstat = markxstat = FALSE;
  } else {
    if ((mesgPtr->flags & MFV_STATUS) != (mesgPtr->origflags & MFV_STATUS) ||
	!(mesgPtr->flags & MFV_OLD)) {
      markstat = TRUE;
      p = statstr;
      if (mesgPtr->flags & MFV_READ) *(p++) = 'R';
      *(p++) = 'O';
      *p = 0;
    } else {
      markstat = FALSE;
    }
    if ((mesgPtr->flags & MFV_XSTATUS) != (mesgPtr->origflags & MFV_XSTATUS)) {
      markxstat = TRUE;
      p = xstatstr;
      if (mesgPtr->flags & MFV_ANSWERED) *(p++) = 'A';
      if (mesgPtr->flags & MFV_FORWARDED) *(p++) = 'F';
      *p = 0;
    } else {
      markxstat = FALSE;
    }
  }

  if (type != folderPtr->type) {
    if ((cptr = fgets(tmpLine,MAXLINE,ifp)) == NULL) goto badinput;
    if (strncmp(tmpLine,mmdfSep,4)==0)
      if ((cptr = fgets(tmpLine,MAXLINE,ifp)) == NULL) goto badinput;

    if (type == MFV_MMDF) fputs(mmdfSep, ofp);

    if (Mfv_IsNewMesg(tmpLine)) {
      if (type == MFV_MH) {
	cptr = NULL; goto fakeit;
      }
      else fputs(tmpLine, ofp);
    } else if (strncasecmp(tmpLine,"return-path",11) == 0) {
      if (type != MFV_BSD) fputs(tmpLine, ofp);
      else {
	cptr = NULL; goto fakeit;
      }
    } else {
      fakeit:
      if (type == MFV_MH) {
	fprintf(ofp, "Return-Path: %s\n",
	    Mfv_GetHeader(interp, mesgPtr, "sm-from"));
      } else {
	fprintf(ofp, "From %s %s %s %s %s %s\n",
	    Mfv_GetHeader(interp, mesgPtr, "sm-from"),
	    Mfv_GetHeader(interp, mesgPtr, "sm-wday"),
	    Mfv_GetHeader(interp, mesgPtr, "sm-month"),
	    Mfv_GetHeader(interp, mesgPtr, "sm-mday"),
	    Mfv_GetHeader(interp, mesgPtr, "sm-time"),
	    Mfv_GetHeader(interp, mesgPtr, "sm-year"));
      }
      if (cptr) fputs(tmpLine, ofp);
    }
  }

  if (markstat || markxstat) {
    while (fgets(tmpLine,MAXLINE,ifp) != NULL) {
      if (ftell(ifp) >= mesgPtr->endhead) {
	if (markstat) fprintf(ofp,"Status: %s\n",statstr);
	if (markxstat) fprintf(ofp,"X-Status: %s\n",xstatstr);
	fprintf(ofp,"\n");
	break;
      }
      if (markstat && strncasecmp(tmpLine,"status:",7)==0) {
	fprintf(ofp,"Status: %s\n",statstr);
	markstat = FALSE;
      } else if (markxstat && strncasecmp(tmpLine,"x-status:",9)==0) {
	fprintf(ofp,"X-Status: %s\n",xstatstr);
	markxstat = FALSE;
      } else {
	fputs(tmpLine,ofp);
      }
      if (!(markstat || markxstat)) break;
    }
  }

  while ((bufsize = MIN(BUFSIZ, mesgPtr->endbody - ftell(ifp))) > 0) {
    if ((len = fread(buf,sizeof(char),bufsize,ifp)) != bufsize) goto badinput;
    if (fwrite(buf,sizeof(char),len,ofp) != len) {
      if (folderPtr->type == MFV_MH) fclose(ifp);
      return -2;
    }
  }
  if (type == MFV_MMDF) fputs(mmdfSep, ofp);

  return 0;
}

int Mfv_CopyDir( char *fromDir, char *toDir, char move )
{
  int result;
  char *toMesg, sameDev, *cwd;
  struct stat statbuf;
  dev_t fromDev;
  ino_t fromIno;
  struct dirent *dp;
  DIR * dd;

  toMesg = cwd = NULL;
  if ((dd = opendir(fromDir)) == NULL) return -1;
  if (fstat(dd->dd_fd, &statbuf) == -1 ) {
    result = -1; goto done;
  }
  fromDev = statbuf.st_dev;
  fromIno = statbuf.st_ino;
  if(stat(toDir, &statbuf) == -1) {
    if (errno != ENOENT) {
      result = -2; goto done;
    }
    if (mkdir(toDir, S_IRWXU) != 0) {
      result = -2; goto done;
    }
    if(stat(toDir, &statbuf) == -1) {
      result = -2; goto done;
    }
  } else {
    if (!S_ISDIR(statbuf.st_mode)) {
      result = -2; goto done;
    }
  }
  if (fromIno == statbuf.st_ino) {
    result = 0; goto done;
  }
  sameDev = (fromDev == statbuf.st_dev);

  result = 0;
  if ((cwd = Mfv_GetCWD()) == NULL) {
    result = -1; goto done;
  }
  if (CHDIR(fromDir)!=0) {
    result = -1; goto done;
  }

  toMesg = dbgalloc(sizeof(char)*(strlen(toDir)+NAME_MAX+1));

  while( dp = readdir(dd) ) {
    if(stat(dp->d_name, &statbuf) == -1) {
      result = -5; goto done;
    }
    if (!S_ISREG(statbuf.st_mode)) continue;
    sprintf(toMesg,"%s%c%s", toDir, dirsep, dp->d_name);
    if (move && sameDev) {
      if (rename(dp->d_name,toMesg) != 0) {
	result = -3; goto done;
      }
    } else {
      if (Mfv_CopyFile(dp->d_name,toMesg) != 0) {
	result = -4; goto done;
      }
      if (move && unlink(dp->d_name)!=0) {
	result = -6; goto done;
      }
    }
  }

  done:
  if (cwd) {
    if (CHDIR(cwd)!=0)
      fprintf(stderr,"Error changing directory\n");
    dbgfree(cwd);
  }
  dbgfree(toMesg);
  closedir(dd);
  return result;
}

/* TODO: protect against removing CWD */
int Mfv_RemoveDir( char *dir )
{
  int result;
  char *cwd;
  struct stat statbuf;
  struct dirent *dp;
  DIR * dd;

  cwd = NULL;
  if ((dd = opendir(dir)) == NULL) return -1;

  result = 0;
  if ((cwd = Mfv_GetCWD()) == NULL) {
    result = -1; goto done;
  }
  if (CHDIR(dir)!=0) {
    result = -1; goto done;
  }

  while( dp = readdir(dd) ) {
    if (strcmp(dp->d_name,".")==0) continue;
    if (strcmp(dp->d_name,"..")==0) continue;
    if(stat(dp->d_name, &statbuf) == -1) {
      result = -2; goto done;
    }
    if (S_ISDIR(statbuf.st_mode)
#ifndef NO_SYMLINKS
	&& !S_ISLNK(statbuf.st_mode)
#endif
	) {
      if ((result = Mfv_RemoveDir(dp->d_name)) != 0) goto done;
    } else {
      if (unlink(dp->d_name)!=0) {
	result = -3; goto done;
      }
    }
  }

  done:
  if (cwd) {
    if (CHDIR(cwd)!=0)
      fprintf(stderr,"Error changing directory\n");
    dbgfree(cwd);
  }
  closedir(dd);
  if (result > -1)
    if (rmdir(dir) != 0) result = -4;
  return result;
}

int Mfv_SaveFolder(Tcl_Interp *interp, Folder *folderPtr, char *file,
    char *sortKeys, int type)
{
  FILE *ifp, *ofp;
  char *p, usetmp, buf[BUFSIZ], hashKey[100];
  char *mesgbuf, *fileName, sorted;
  int mesg, result, oldcnt, oldmax, addcnt;
  int fd, len, res, ftype, locked;
  long filepos;
  Mesg *mesgPtr, **mesglist, **mesgadd;
  struct stat statbuf;
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  Tcl_DString buffer;

  result = TCL_OK;
  mesgbuf = NULL;
  mesglist = NULL;
  fileName = NULL;

  /* Check for new messages */
  locked = folderPtr->locked;
  if (!locked && Mfv_LockFolder(folderPtr) < 0) {
    Tcl_AppendResult(interp, "Cannot lock folder \"", folderPtr->name,
	"\"", (char *) NULL);
    return TCL_ERROR;
  }

  oldcnt = folderPtr->count;
  oldmax = folderPtr->max;
  if ((addcnt = Mfv_CheckAppend(interp,folderPtr)) < 0 ||
      (addcnt + oldcnt) != folderPtr->count) {
    if (addcnt < 0)
      fprintf(stderr, "CORRUPT: %d\n", addcnt);
    else
      fprintf(stderr, "CORRUPT: sync error\n");
    Tcl_AppendResult(interp, "folder \"", folderPtr->name,
	"\" has been externally corrupted.", (char *) NULL);
    Mfv_InformClose(interp, folderPtr->key);
    Mfv_CloseFolder(interp,folderPtr);
    return TCL_ERROR;
  }

  if (folderPtr->count == 0 && !file) {
    if (!locked) Mfv_UnlockFolder(folderPtr);
    sprintf(interp->result, "0");
    return TCL_OK;
  }

  if ( (ftype = folderPtr->type) == MFV_UNKNOWN ) {
    if (!locked) Mfv_UnlockFolder(folderPtr);
    Tcl_AppendResult(interp, "Sorry. Can't save ",
	folderTypes[ftype], " type folders.", (char *) NULL);
    return TCL_ERROR;
  }

  /* make sure we have a valid save format type */
  if (type < MFV_BSD) {
    type = ftype;
    if (type < MFV_BSD)
      type = defaultType;
  }

  /* check if valid file to save to */
  if (file) {
    usetmp = FALSE;
    if ((fileName = Tcl_TildeSubst(interp, file, &buffer)) == NULL) {
      Tcl_AppendResult(interp, "cannot resolve as a file \"", file,
	    "\"", (char *) NULL);
      if (!locked) Mfv_UnlockFolder(folderPtr);
      return TCL_ERROR;
    }
    if(stat(fileName, &statbuf) == -1) {
      if (errno != ENOENT) {
	Tcl_AppendResult(interp, "cannot stat file \"", file,
	    "\"", (char *) NULL);
	result = TCL_ERROR;
	goto done;
      }
      if (Mfv_CheckIsOpen(0, 0, fileName)) goto alreadyopen;
    } else {
      /* if same file as folder, use tempfile */
      if (statbuf.st_ino == folderPtr->ino &&
	      statbuf.st_dev == folderPtr->dev) {
	Tcl_DStringFree(&buffer);
	file = fileName = NULL;
	usetmp = TRUE;
      } else { /* Check if open elsewhere */
	if (Mfv_CheckIsOpen(statbuf.st_dev, statbuf.st_ino, NULL)) {
	  alreadyopen:
	  Tcl_AppendResult(interp, "illegal to save \"", file,
	      "\" to another open folder", (char *) NULL);
	  result = TCL_ERROR;
	  goto done;
	}
	if (type == MFV_MH && !S_ISDIR(statbuf.st_mode)) {
	  result = TCL_ERROR;
	  goto done;
	} else if (!S_ISREG(statbuf.st_mode)) { 
	  result = TCL_ERROR;
	  goto done;
	}
      }	
    }
  } else { /* we are saving to itself and need to use a tmp file/dir */
    usetmp = TRUE;
    fileName = NULL;
  }

  if (usetmp) {
    if (folderPtr->readonly || ftype == MFV_VIRTUAL) {
      Tcl_AppendResult(interp, "folder \"", folderPtr->name, "\" is ", 
	  (folderPtr->readonly ? "readonly." : "virtual."), (char *) NULL);
      result = TCL_ERROR;
      goto done;
    }
  }

  /* sort messages before save in requested order */
  sorted = (sortKeys != NULL);
  if (folderPtr->count) {
    mesgbuf = dbgalloc(folderPtr->count*sizeof(Mesg *));
    mesglist = (Mesg **)mesgbuf;
    mesgadd = mesglist + oldcnt; /* don't include new messages in sort */
    for(entryPtr = Tcl_FirstHashEntry(&(folderPtr->mesgTable),&search);
	entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
      mesgPtr = (Mesg *) Tcl_GetHashValue(entryPtr);
      mesgPtr->accessnum =
	  (int) Tcl_GetHashKey(&(folderPtr->mesgTable), entryPtr);
      if (mesgPtr->accessnum > oldmax) {
	*mesgadd = mesgPtr; ++mesgadd;
      } else {
	*mesglist = mesgPtr; ++mesglist;
      }
    }

    mesglist = (Mesg **)mesgbuf;
    if (!sorted) sortKeys = "normal";
    if (Mfv_SortMesgList(interp, folderPtr, sortKeys, mesglist, oldcnt)
	!= TCL_OK) {
      result = TCL_ERROR;
      goto done;
    }

    if (addcnt) {
      /* sort new messages in normal order */
      mesgadd = mesglist + oldcnt;
      if (Mfv_SortMesgList(interp, folderPtr, "normal", mesgadd, addcnt)
	!= TCL_OK) {
	result = TCL_ERROR;
	goto done;
      }
    }
  }

  /* get a unique name for a temp file/dir */
  if (usetmp) {
    if (ftype == MFV_MH) {
      sprintf(tmpFile,"%s%c%s", folderPtr->dir, dirsep, folderPtr->name);
      p = tmpFile;
    } else {
      p = folderPtr->dir;
    }
    if ((fileName = Mfv_TmpName("mfv", p)) == NULL) {
      Tcl_AppendResult(interp, "cannot obtain a temp file for writing",
	  (char *) NULL);
      result = TCL_ERROR;
      goto done;
    }
  }

  /* expand global scratch variable for filenames if necessary */
  len = strlen(fileName) + 20;
  if ( len > tmpFileSize ) {
    dbgfree(tmpFile);
    tmpFile = dbgalloc(len*sizeof(char));
    *tmpFile = 0;
    tmpFileSize = len;
  }

  if (type == MFV_MH) {
    /* make sure destination dir is valid and exists */
    ofp = NULL;
    if(stat(fileName, &statbuf) == -1) {
      if (errno != ENOENT) {
	Tcl_AppendResult(interp, "cannot stat \"", fileName,
	    "\"", (char *) NULL);
	result = TCL_ERROR;
	goto done;
      }
      if (mkdir(fileName, S_IRWXU) != 0) {
	Tcl_AppendResult(interp, "cannot make directory \"", fileName,
	    "\"", (char *) NULL);
	result = TCL_ERROR;
	goto done;
      }
    } else {
      if (!S_ISDIR(statbuf.st_mode) ||
	  (((statbuf.st_mode)&S_IWUSR) != S_IWUSR)) {
	Tcl_AppendResult(interp, " \"", fileName,
	    "\" is not a directory or writable", (char *) NULL);
	result = TCL_ERROR;
	goto done;
      }
    }
  } else {
    /* open destination folder file */
    if ((ofp = fopen(fileName, "w+")) == NULL) {
      Tcl_AppendResult(interp, "cannot open file \"", fileName,
	  "\" for writing", (char *) NULL);
      result = TCL_ERROR;
      goto done;
    }
  }

  /* loop through messages and write them if not deleted */
  mesglist = (Mesg **)mesgbuf;
  for (mesg = 1; mesg <= folderPtr->count; ++mesg) {
    mesgPtr = *mesglist; ++mesglist;

    if (!(mesgPtr->flags & MFV_DELETED)) {
      if (type == MFV_MH) {
	fclose(ofp);
	sprintf(tmpFile, "%s%c%d", fileName, dirsep,
	    (sorted ? mesg : mesgPtr->num));
	if ((ofp = fopen(tmpFile, "w+")) == NULL) {
	  Tcl_AppendResult(interp, "Cannot open file \"", tmpFile,
	      "\" for writing.", (char *) NULL);
	  goto abort;
	}
      }

      if ((res = Mfv_WriteMesg(interp, mesgPtr, ofp, type,
	  (mesgPtr->num <= oldmax))) < 0) {
	if (res == -1) {
	  /* TODO: What to do here? */
	  Tcl_AppendResult(interp, "internal consistency error in \"",
	      folderPtr->name, "\" during save.", (char *) NULL);
	  Mfv_InformClose(interp, folderPtr->key);
	  Mfv_CloseFolder(interp, folderPtr);
	  folderPtr = NULL;
	} else {
	  Tcl_AppendResult(interp, "failed to write data to \"",
	      fileName, "\" during save", (char *) NULL);
	}
	abort:
	result = TCL_ERROR;
	fclose(ofp);
	if (usetmp) {
	  if (type == MFV_MH) {
	    if (Mfv_RemoveDir(fileName) != 0)
	      fprintf(stderr, "Could not remove tempdir %s\n", fileName);
	  }
	  else unlink(fileName);
	}
	goto done;
      }
    }

    /* if a pure save on MH folder, need to delete originals */
    if (usetmp && type == MFV_MH && ftype == MFV_MH) {
      sprintf(tmpFile, "%s%c%s%c%d", folderPtr->dir, dirsep,
	  folderPtr->name, dirsep, mesgPtr->num);
      if (unlink(tmpFile) != 0) {
	Tcl_AppendResult(interp, "file \"", tmpFile,
	    "\" could not be deleted.", (char *) NULL);
	Mfv_InformClose(interp, folderPtr->key);
	Mfv_CloseFolder(interp, folderPtr);
	folderPtr = NULL;
	goto abort;
      }
    }
  }

  /* TODO: check for new appends that snuck in btw check and here */

  /* TODO: check atomic-ness below */
  /* if a save to original, process accordingly */
  if (usetmp) {
    strncpy(hashKey,folderPtr->key,100);
    sprintf(tmpFile,"%s%c%s", folderPtr->dir, dirsep, folderPtr->name);
    if (type == MFV_MH) {
      fclose(ofp); ofp = NULL;
      Mfv_CloseFolder(interp,folderPtr);
      folderPtr = NULL;
      if (ftype != MFV_MH) {
	if (unlink(tmpFile) != 0) {
	  Tcl_AppendResult(interp, "cannot remove old folder \"", tmpFile,
	      "\" for overwriting.", (char *) NULL);
	  aftercloseerr:
	  fclose(ofp);
	  Mfv_InformClose(interp, hashKey);
	  result = TCL_ERROR;
	  goto done;
	}
	if (mkdir(tmpFile, S_IRWXU) != 0) {
	  Tcl_AppendResult(interp, "cannot create directory \"", tmpFile,
	      "\" for writing.", (char *) NULL);
	  goto aftercloseerr;
	}	
      }
      if (Mfv_CopyDir(fileName, tmpFile, TRUE) != 0) {
	Tcl_AppendResult(interp, "cannot copy messages from \"", fileName,
	    "\" to \"", tmpFile, "\"", (char *) NULL);
	goto aftercloseerr;
      }
      if (Mfv_RemoveDir(fileName) != 0)
	fprintf(stderr, "Could not remove tempdir %s\n", fileName);
    } else {
      if (ftype == MFV_MH) {
	Mfv_CloseFolder(interp,folderPtr);
	folderPtr = NULL;
	if (Mfv_RemoveDir(tmpFile) != 0) {
	  Tcl_AppendResult(interp, "cannot remove directory \"", tmpFile,
	      "\" for overwriting.", (char *) NULL);
	  goto aftercloseerr;
	}
	if ((ifp = fopen(tmpFile, "w+")) == NULL) {
	  Tcl_AppendResult(interp, "unrecoverable error : cannot open \"",
	      tmpFile, "\" for writing.", (char *) NULL);
	  goto aftercloseerr;
	}
      } else {
	ifp = folderPtr->fp;
	rewind(ifp);
      }

      rewind(ofp);
      while ((len = fread(buf,sizeof(char),BUFSIZ,ofp)) > 0) {
	if (fwrite(buf,sizeof(char),len,ifp) != len) {
	  Tcl_AppendResult(interp, "unrecoverable error : short write to \"",
	      tmpFile, "\" during save.", (char *) NULL);
	  if (ftype != MFV_MH) {
	    Mfv_CloseFolder(interp,folderPtr);
	    folderPtr = NULL;
	  } else fclose(ifp);
	  goto aftercloseerr;
	}
      }
      filepos=ftell(ifp);
      rewind(ifp);
      if ((fd = fileno(ifp)) == -1 || ftruncate(fd, filepos) != 0) {
	fprintf(stderr, "%s\n", strerror(errno));
	Tcl_AppendResult(interp, "unrecoverable error : cannot truncate \"",
	    tmpFile, "\" during save.", (char *) NULL);
	if (ftype != MFV_MH) {
	  Mfv_CloseFolder(interp,folderPtr);
	  folderPtr = NULL;
	} else fclose(ifp);
	goto aftercloseerr;
      }
      fclose(ofp); /* close and delete tmpfile */
      unlink(fileName);
      if (ftype != MFV_MH) {
	Mfv_CloseFolder(interp,folderPtr);
	folderPtr = NULL;
      }
      else fclose(ifp);
    }
    result = Mfv_OpenFolder(interp,tmpFile,hashKey,locked);
  } else {
    fclose(ofp);
  }

  done:
  if (folderPtr && !locked) Mfv_UnlockFolder(folderPtr);
  dbgfree(mesgbuf);
  if (usetmp) dbgfree(fileName);
  else if (fileName) Tcl_DStringFree(&buffer);

  if (result == TCL_OK) {
    Tcl_ResetResult(interp);
    sprintf(interp->result, "%d", addcnt);
  }

  return result;
}

int Mfv_DeleteMesgTable(Tcl_Interp *interp, Folder *folderPtr)
{
  Mesg *mesgPtr;
  VFLink *curVF, *nextVF, *lastVF;
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  Thread *thrd, *nextthrd;
  struct stat statbuf;

  for(entryPtr = Tcl_FirstHashEntry(&(folderPtr->mesgTable),&search);
      entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
    mesgPtr = (Mesg *) Tcl_GetHashValue(entryPtr);

    if (mesgPtr->num < 0) {
      fprintf(stderr, "\007SERIOUS BUG! This message was already deleted (%d)!\n",
	  mesgPtr->num + 10);
      continue;
    }
    mesgPtr->num = -10 - mesgPtr->num;
    
    nextVF = mesgPtr->links;
    if (folderPtr->type == MFV_VIRTUAL) {
      lastVF = NULL;
      while (nextVF) {
	curVF = nextVF;
	nextVF = curVF->next;
	if (curVF->folder == folderPtr) {
	  if (lastVF) lastVF->next = nextVF;
	  else mesgPtr->links = nextVF;
	  dbgfree((char *)curVF);
	} else {
	  lastVF = curVF;
	}
      }
    } else {
      while (nextVF) {
	curVF = nextVF;
	nextVF = curVF->next;
	entryPtr = Tcl_FindHashEntry(&(curVF->folder->mesgTable),
	    (char *)curVF->num);
	Tcl_DeleteHashEntry(entryPtr);
	curVF->folder->count--;
	dbgfree((char *)curVF);
      }

      nextthrd = mesgPtr->replies;
      while(nextthrd != NULL) {
	thrd = nextthrd;
	nextthrd = thrd->next;
	dbgfree((char *)thrd);
      }
      
      Mfv_FreeMimePart(mesgPtr->mimepart);
      dbgfree(mesgPtr->hdrBuf);
      dbgfree(mesgPtr->smBuf);
      dbgfree(mesgPtr->nameBuf);
      Tcl_DeleteHashTable(&(mesgPtr->headerTable));
      dbgfree((char *)mesgPtr);
    }
  }

  folderPtr->count = folderPtr->max = 0;
  Tcl_DeleteHashTable(&(folderPtr->mesgTable));
  return TCL_OK;
}

int Mfv_CloseFolder(Tcl_Interp *interp, Folder *folderPtr)
{
  Mesg *mesgPtr;
  VFLink *curVF, *nextVF, *lastVF;
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  struct stat statbuf;

  if (folderPtr->locked) Mfv_UnlockFolder(folderPtr);

  Mfv_DeleteMesgTable(interp, folderPtr);

  entryPtr = Tcl_FindHashEntry(&folderTable, folderPtr->key);
  if (folderPtr->fp) fclose(folderPtr->fp);
  /* don't delete mailspool file, leave as zero length for security */
  if (delEmpty && strcmp(folderPtr->dir,mailSpool) != 0) {
    sprintf(tmpFile,"%s%c%s", folderPtr->dir, dirsep, folderPtr->name);
    if(stat(tmpFile, &statbuf) != -1) {
      if (statbuf.st_size == 0) unlink(tmpFile);
    }
  }

  Tcl_DeleteCommand(interp, folderPtr->key);
  dbgfree(folderPtr->key);
  dbgfree(folderPtr->name);
  dbgfree(folderPtr->dir);
  dbgfree((char *)folderPtr);
  Tcl_DeleteHashEntry(entryPtr);

  return TCL_OK;
}

int Mfv_CloseCmd(ClientData cd, Tcl_Interp *interp, int argc, char *argv[])
{
  Folder *folderPtr;

  if (argc != 2) {
    Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                " folderId\"", (char *) NULL);
    return TCL_ERROR;
  }

  folderPtr = Mfv_GetFolder(interp, argv[1]);
  if (folderPtr == NULL) return TCL_ERROR;

  return Mfv_CloseFolder(interp, folderPtr);
}

int Mfv_FilterHeaders(Tcl_Interp *interp, Mesg *mesgPtr)
{
  char *p1, *p2, strip;
  int i;
  size_t len;
  FILE *ifp;
  Folder *folderPtr;

  folderPtr = mesgPtr->folder;

  if ((ifp = Mfv_GetMesgFilePtr(mesgPtr)) == NULL) return TCL_ERROR;

  strip = !filterStrip;
  fseek(ifp, mesgPtr->beghead, SEEK_SET);
  while (fgets(tmpLine,MAXLINE,ifp) != NULL) {
    if (ftell(ifp) >= mesgPtr->endhead) break;

    if (!isspace(*tmpLine) &&
	(p1 = strchr(tmpLine,':')) != NULL) {
      /* make sure it is valid field and check filter list */
      for (p2=tmpLine; p2 < p1 && *p2 != 0 && !isspace(*p2); ++p2);
      if (p2 >= p1) {
	for (p2=p1-1; isspace(*p2); p2--);
	len = (++p2)-tmpLine;
	strip = !filterStrip;
	for (i = 0; i < filterCount; ++i) {
	  if (strncasecmp(filterHeaders[i],tmpLine,len)==0) {
	    strip = filterStrip;
	    break;
	  }
	}
      }
    }
    if (!strip) Tcl_AppendResult(interp, tmpLine, (char *)NULL);
  }

  if (folderPtr->type == MFV_MH ) fclose(ifp);
  return TCL_OK;
}

/* What a NIGHTMARE! */
int Mfv_AppendMesg(Tcl_Interp *interp, Mesg **mesgList, int mesgCnt,
    char *toFolder, int plain)
{
  char *cptr;
  int mesg, max, type, res, result, i;
  long initlen;
  FILE *ifp, *ofp;
  struct stat statbuf;
  Folder *folderPtr, *toFolderPtr;
  Mesg *mesgPtr;
  Tcl_DString buffer;
  struct dirent *dp;
  DIR * dd;
  
  toFolderPtr = NULL;
  result = TCL_OK;
  
  if (toFolder == NULL) {
    Tcl_AppendResult(interp, "no target folder given", (char *) NULL);
    return TCL_ERROR;
  } else {
    if ((cptr = Tcl_TildeSubst(interp, toFolder, &buffer)) == NULL) {
	Tcl_AppendResult(interp, "cannot resolve as a file \"", toFolder,
	    "\"", (char *) NULL);
	return TCL_ERROR;
    }
    toFolder = cptr;

    if(stat(toFolder, &statbuf) == -1) {
      if (errno == ENOENT) {
	if (toFolderPtr = Mfv_CheckIsOpen(0, 0, toFolder)) goto alreadyopen;
	/* create with default type */
	type = ( plain ? defaultType : MFV_BSD );
	if (type == MFV_MH) {
	  if (mkdir(toFolder, S_IRWXU) != 0) {
	    Tcl_AppendResult(interp, "cannot create MH directory \"",
		toFolder, "\".", (char *) NULL);
	    Tcl_DStringFree(&buffer);
	    return TCL_ERROR;
	  }
	}
	ofp = NULL;
      } else {
	badfolder:
	Tcl_AppendResult(interp, "invalid/unwritable destination folder \"",
	    ( toFolder ? toFolder : emptyString), "\"", (char *) NULL);
	Tcl_DStringFree(&buffer);
	return TCL_ERROR;
      }
    } else {
      /* check if already open and if writable */
      if (toFolderPtr = Mfv_CheckIsOpen(statbuf.st_dev, statbuf.st_ino, NULL)) {
	alreadyopen:
	if (toFolderPtr->readonly) goto badfolder;
	/* copy to same folder not allowed */
	for (i = 0; i < mesgCnt; i++) {
	  folderPtr = mesgList[i]->folder;
	  if (toFolderPtr == folderPtr) {
	    Tcl_AppendResult(interp, "Cannot append to same folder", (char *) NULL);
	    Tcl_DStringFree(&buffer);
	    return TCL_ERROR;
	  }
	}
	if (toFolderPtr->locked) {
	  Tcl_AppendResult(interp, "Target folder is locked", (char *) NULL);
	  Tcl_DStringFree(&buffer);
	  return TCL_ERROR;
	}
	type = toFolderPtr->type;
	ofp = toFolderPtr->fp;
      } else {
	/* determine type dynamically */
	if (((statbuf.st_mode)&S_IWUSR) != S_IWUSR) goto badfolder; 
	ofp = NULL;
	if (S_ISDIR(statbuf.st_mode)) {
	  type = MFV_MH;
	} else {
	  if (!S_ISREG(statbuf.st_mode)) goto badfolder;
	  if ((ifp = fopen(toFolder, "r")) == NULL) goto badfolder;
	  cptr = fgets(tmpLine,MAXLINE,ifp);
	  if (cptr == NULL) {
	    /* give empty folder default type */
	    type = ( defaultType == MFV_MH || plain ? MFV_BSD : defaultType );
	  } else {
	    if (strncmp(mmdfSep,tmpLine,4) == 0) {
	      type = MFV_MMDF;
	      cptr = fgets(tmpLine,MAXLINE,ifp);
	      if (cptr == NULL) {
		badformat:
		Tcl_AppendResult(interp, "file \"", toFolder,
		    "\" is not in a recognized format type for a mail folder", (char *) NULL);
		fclose(ifp);
		Tcl_DStringFree(&buffer);
		return TCL_ERROR;
	      }
	    } else {
	      type = MFV_BSD;
	      if (!Mfv_IsNewMesg(tmpLine)) {
		fclose(ifp);
		goto badformat;
	      }
	    }
	  }
	  fclose(ifp);
	}
      }
    }
  }

  if (ofp == NULL) {
    if ( type == MFV_MH ) {
      /* find next message number and open that file */
      if ((dd = opendir(toFolder)) == NULL) {
	Tcl_AppendResult(interp, "Error reading directory \"",
	    toFolder, "\"", (char *) NULL);
	result = TCL_ERROR;
	goto done;
      }
      max = 0;
      while( dp = readdir(dd) ) {
	if ( (mesg = atoi(dp->d_name)) > max ) max = mesg;
      }
      closedir(dd);
      while (1) { /* need to use open for atomicness */
	sprintf(tmpFile, "%s%c%d", toFolder, dirsep, max+1);
	if ((res = open(tmpFile, O_RDWR | O_CREAT | O_EXCL, S_IRWXU)) < 0) {
	  if (errno != EEXIST) {
	    badopenappend:
	    Tcl_AppendResult(interp, "Cannot open file \"", tmpFile,
		"\" for append.", (char *) NULL);
	    result = TCL_ERROR;
	    goto done;
	  }
	  max++;
	}
	else break;
      }
      if ((ofp = fdopen(res, "a+")) == NULL) {
	close(res);
	goto badopenappend;
      }
    } else {
      if ((ofp = fopen(toFolder, "a+")) == NULL) {
	Tcl_AppendResult(interp, "Cannot open file \"", toFolder,
	    "\" for append.", (char *) NULL);
	result = TCL_ERROR;
	goto done;
      }
    }
  }

  if ((res = (toFolderPtr ?
      Mfv_LockFolder(toFolderPtr) : Mfv_LockFile(toFolder))) < 0) {
    if (res == -2)
      Tcl_AppendResult(interp, "\"", toFolder, "\" is locked", (char *) NULL);
    else
      Tcl_AppendResult(interp, "Lock failed: ", lock_err, (char *) NULL);
    result = TCL_ERROR;
    goto done;
  }
  fseek(ofp, 0, SEEK_END);
  initlen = ftell(ofp);
  
  for (res = i = 0; i < mesgCnt; i++) {
    mesgPtr = mesgList[i]; /* needed for error handling below */
    if ((res = Mfv_WriteMesg(interp, mesgPtr, ofp, type, TRUE)) < 0) break;
  }
  
  if ((toFolderPtr ? Mfv_UnlockFolder(toFolderPtr) : Mfv_UnlockFile(toFolder)) < 0)
    fprintf(stderr, "Can't unlock %s.\n", toFolder);
  
  if (res < 0) {
    result = TCL_ERROR;
    if (res == -1) {
      folderPtr = mesgPtr->folder;
      Tcl_AppendResult(interp, "Folder \"", folderPtr->name,
	  "\" is corrupt.", (char *) NULL);
      Mfv_InformClose(interp, folderPtr->key);
      Mfv_CloseFolder(interp, folderPtr);
    }
    if ( type == MFV_MH ) {
      fclose(ofp); ofp = NULL;
      sprintf(tmpFile, "%s%c%d", toFolder, dirsep, max+1);
      if (unlink(tmpFile) != 0) {
	fprintf(stderr, "Remove error on %s ignored\n", tmpFile);
      }
    } else if (initlen != ftell(ofp)) {
      fclose(ofp); ofp = NULL;
      max = truncate(toFolder,initlen);
      if (toFolderPtr) {
	toFolderPtr->fp = fopen(tmpFile, (toFolderPtr->readonly ? "r" : "r+"));
	if (max != 0 || toFolderPtr->fp == NULL) {
	  if (strlen(interp->result))
	      Tcl_AppendResult(interp, "  ", (char *) NULL);
	  Tcl_AppendResult(interp, "Target folder \"", toFolderPtr->name,
	      "\" is corrupt.", (char *) NULL);
	  Mfv_InformClose(interp, toFolderPtr->key);
	  Mfv_CloseFolder(interp, toFolderPtr);
	}
      } else if (max != 0) {
	fprintf(stderr, "Truncate error on %s ignored\n", toFolder);
      }
    }
  }

  done:
  if (!toFolderPtr || ofp != toFolderPtr->fp) fclose(ofp);
  Tcl_DStringFree(&buffer);
  return(result);
}

int Mfv_AppendFolder(Tcl_Interp *interp, Folder *folderPtr, char *srcFolder)
{
  char *cptr, buf[BUFSIZ], exists;
  int res, mesg, fd, len, result, type;
  long initlen;
  FILE *ifp, *ofp;
  struct stat statbuf;
  Folder *srcFolderPtr;
  Tcl_DString buffer;

  srcFolderPtr = NULL;
  result = TCL_OK;
  
  if (folderPtr->type != MFV_BSD && folderPtr->type != MFV_MMDF &&
      folderPtr->type != MFV_UNKNOWN ) {
    Tcl_AppendResult(interp, "Sorry, can only append MMDF/BSD folders currently.",
	(char *) NULL);
    return TCL_ERROR;
  }
  if (folderPtr->readonly || folderPtr->locked) {
    Tcl_AppendResult(interp,
	"Folder \"", folderPtr->name, "\" is ",
	(folderPtr->readonly ? "readonly." : "locked."), (char *) NULL);
    return TCL_ERROR;
  }
  
  ifp = NULL;
  if (folderPtr->fp == NULL) {
    sprintf(tmpFile, "%s%c%s", folderPtr->dir, dirsep, folderPtr->name);
    exists = (access(tmpFile,F_OK) == 0);
    if ((folderPtr->fp = fopen(tmpFile, exists ? "r+" : "w+")) == NULL) {
      Tcl_AppendResult(interp, "Cannot open folder \"", folderPtr->name,
	  "\": ", strerror(errno), (char *) NULL);
      return TCL_ERROR;
    }
  }
  ofp = folderPtr->fp;

  if (srcFolder == NULL) {
    Tcl_AppendResult(interp, "no source folder given", (char *) NULL);
    return TCL_ERROR;
  } else {
    if ((cptr = Tcl_TildeSubst(interp, srcFolder, &buffer)) == NULL) {
	Tcl_AppendResult(interp, "cannot resolve as a file \"", srcFolder,
	    "\"", (char *) NULL);
	return TCL_ERROR;
    }
    srcFolder = cptr;

    if(stat(srcFolder, &statbuf) == -1) {
      Tcl_AppendResult(interp, "can't open folder \"",
	  srcFolder, "\" -- ", strerror(errno), ".", (char *) NULL);
      result = TCL_ERROR;
      goto done;
    } else {
      /* check if already open */
      if (srcFolderPtr = Mfv_CheckIsOpen(statbuf.st_dev, statbuf.st_ino, NULL)) {
	/* copy to same folder not allowed */
	if (srcFolderPtr == folderPtr) {
	  Tcl_AppendResult(interp, "Cannot append to same folder", (char *) NULL);
	  result = TCL_ERROR;
	  goto done;
	}
	type = srcFolderPtr->type;
	ifp = srcFolderPtr->fp;
      } else {
	/* determine type dynamically */
	if (((statbuf.st_mode)&S_IWUSR) != S_IWUSR) {
	  badfolder:
	  Tcl_AppendResult(interp, "invalid source folder \"",
	      ( srcFolder ? srcFolder : emptyString), "\"", (char *) NULL);
	  result = TCL_ERROR;
	  goto done;
	}
	if (S_ISDIR(statbuf.st_mode)) {
	  type = MFV_MH;
	} else {
	  if (!S_ISREG(statbuf.st_mode)) goto badfolder;
	  if ((ifp = fopen(srcFolder, "r")) == NULL) goto badfolder;
	  cptr = fgets(tmpLine,MAXLINE,ifp);
	  if (cptr == NULL) {
	    /* No messages to append */
	    goto done;
	  } else {
	    if (strncmp(mmdfSep,tmpLine,4) == 0) {
	      type = MFV_MMDF;
	      cptr = fgets(tmpLine,MAXLINE,ifp);
	      if (cptr == NULL) {
		badformat:
		Tcl_AppendResult(interp, "file \"", srcFolder,
		    "\" is not in a recognized format type for a mail folder", (char *) NULL);
		result = TCL_ERROR;
		goto done;
	      }
	    } else {
	      type = MFV_BSD;
	      if (!Mfv_IsNewMesg(tmpLine)) {
		goto badformat;
	      }
	    }
	  }
	  fseek(ifp, 0, SEEK_SET);
	}
      }
      if (folderPtr->type != MFV_UNKNOWN && type != folderPtr->type) {
	Tcl_AppendResult(interp, "Sorry, can only folders of same type currently.",
	    (char *) NULL);
	result = TCL_ERROR;
	goto done;
      }
    }    
  }

  if (Mfv_LockFolder(folderPtr) < 0) {
    if (res == -2)
      Tcl_AppendResult(interp, "Folder is locked", (char *) NULL);
    else
      Tcl_AppendResult(interp, "Lock failed: ", lock_err, (char *) NULL);
    result = TCL_ERROR;
    goto done;
  }
  fseek(ofp, 0, SEEK_END);
  initlen = ftell(ofp);

  while ((len = fread(buf,sizeof(char),BUFSIZ,ifp)) != EOF && len > 0) {
    if (fwrite(buf,sizeof(char),len,ofp) != len) {
      Tcl_AppendResult(interp, "Short write to ", folderPtr->name, " -- ",
	  strerror(errno), ".", (char *) NULL);    
      goto corrupt;
    }
  }
  
  if (ferror(ifp)) {
    Tcl_AppendResult(interp, "Read error on ", srcFolder, " -- ",
	strerror(errno), ".", (char *) NULL);    
    corrupt:
    result = TCL_ERROR;
    if ((fd = fileno(ofp)) == -1 || ftruncate(fd, initlen) != 0) {
      Tcl_AppendResult(interp, " Folder \"", folderPtr->name,
	  "\" had been corrupted.", (char *) NULL);
      Mfv_InformClose(interp, folderPtr->key);
      Mfv_CloseFolder(interp, folderPtr);
    } else {
      rewind(ofp);
    }
  }

  done:
  if (Mfv_UnlockFolder(folderPtr) < 0)
    fprintf(stderr, "Cannot unlock %s.\n", folderPtr->name);
  if (srcFolderPtr == NULL) fclose(ifp);
  Tcl_DStringFree(&buffer);
  return(result);
}

int Mfv_SearchTest(Tcl_Interp *interp, char *buf, char *op,
    char *pat, char *mods, int *match)
{
  char *lbuf, *p1, *p2;
  int res, result;

  *match = 0;
  result = TCL_OK;
  lbuf = buf;

  if (strlen(op) != 2) goto badop;

  if (mods) {
    if (strchr(mods,'i')) {
      lbuf = dbgalloc(sizeof(char)*(strlen(buf)+1));
      for (p1=buf, p2=lbuf; *p1 != 0; p1++, p2++)
	*p2 = tolower(UCHAR(*p1));
      Mfv_ToLower(pat);
    }
    if (strchr(mods,'t')) {
      if (lbuf == buf) 
	lbuf = dbgalloc(sizeof(char)*(strlen(buf)+1));
      for (p1=buf; isspace(*p1) && *p1 != 0; p1++);
      strcpy(lbuf,p1);
      Mfv_TrimSpace(lbuf);
    }
  }

  if (op[1] == '=') {
    res = (strcmp(lbuf, pat) == 0);
  } else if (op[1] == '~') {
    if ((res = Tcl_RegExpMatch(interp, lbuf, pat)) == -1) {
      Tcl_AppendResult(interp, "bad regexp pattern \"", pat,
	  "\" or internal error", (char *) NULL);
      result = TCL_ERROR;
      goto done;
    }
  } else if (op[1] == '@') {
    res = (strstr(lbuf, pat) != NULL);
  } else {
    badop:
    Tcl_AppendResult(interp, "bad search operator \"", op,
	"\"", (char *) NULL);
    result = TCL_ERROR;
    goto done;
  }

  if (op[0] == '!') res = !res;
  *match = res;

  done:
  if (lbuf != buf) dbgfree(lbuf);
  return result;
}

int Mfv_FileSearchTest(Tcl_Interp *interp, FILE *ifp, long start, long stop,
    char *op, char *pat, char *mods, int *match)
{
  int neg, res;
  char *p, tmpop[3], lower, trim;

  *match = 0;
  lower = trim = FALSE;
  neg = (op[0] == '!');
  tmpop[0] = '='; tmpop[1] = op[1]; tmpop[2] = 0;

  if (stop < start || fseek(ifp, start, SEEK_SET) < 0) {
    Tcl_AppendResult(interp, "bad file seek", (char *) NULL);
    return TCL_ERROR;
  }

  /* small optimization handling of ignore case modification */
  if (mods && (p = strchr(mods,'i'))) {
    lower = TRUE;
    Mfv_ToLower(pat);
  }
  if (mods && (p = strchr(mods,'t'))) {
    trim = TRUE;
  }

  while (fgets(tmpLine,MAXLINE,ifp) != NULL) {
    if (ftell(ifp) >= stop) break;
    if ((p = strrchr(tmpLine,'\n')) != NULL) *p = 0;
    if (lower) Mfv_ToLower(tmpLine);
    if (trim)
      p = Mfv_TrimSpace(tmpLine);
    else p = tmpLine;
    if (Mfv_SearchTest(interp, p, tmpop, pat, NULL, &res) != TCL_OK) {
      return TCL_ERROR;
    }
    if (res) {
      *match = !neg;
      return TCL_OK;
    }
  }

  *match = neg;
  return TCL_OK;
}

int Mfv_MesgMatch(Tcl_Interp *interp, Mesg *mesgPtr, char *spec, int *match)
{
  char c, *p, *buf, **listArgv;
  int i, j, len, listArgc, result, sofar, neg;
  size_t length;
  Folder *folderPtr;
  FILE *ifp;

  *match = sofar = 0;
  result = TCL_OK;

  /* check for negation */
  while (isspace(*spec)) spec++;
  if (*spec == '!') {
    neg = TRUE; spec++;
  } else if (strncmp(spec,"not",3) == 0) {
    neg = TRUE; spec += 3;
  } else neg = FALSE;
  while (isspace(*spec)) spec++;

  if (Tcl_SplitList(interp, spec, &listArgc, &listArgv) != TCL_OK) {
    return TCL_ERROR;
  }
  if (!listArgc) {
    Tcl_AppendResult(interp, "empty message search spec", (char *) NULL);
    result = TCL_ERROR;
    goto done;
  }

  folderPtr = mesgPtr->folder;
  if ((ifp = Mfv_GetMesgFilePtr(mesgPtr)) == NULL) {
    Tcl_AppendResult(interp, "cannot read message from folder \"",
	folderPtr->name, "\"", (char *) NULL);
    result = TCL_ERROR;
    goto done;
  }

  p = Mfv_TrimSpace(listArgv[0]);
  c = p[0];
  length = strlen(p);
  if (c == '{' || c == '"' || strchr(p,' ')) {
    for (i = 0; i < listArgc; ++i) {
      if (i%2) {
	if (strncmp(listArgv[i],"&&",2) == 0) {
	  if (!sofar) goto done;
	} else if (strncmp(listArgv[i],"||",2) == 0) {
	  if (sofar) { *match = 1; goto done; }
	} else {
	  Tcl_AppendResult(interp, "invalid search logical operator \"",
	      listArgv[i], "\": must be && or ||", (char *) NULL);
	  result = TCL_ERROR;
	  goto done;
	}
      } else {
	if ((result = Mfv_MesgMatch(interp, mesgPtr,
	    listArgv[i], &sofar)) != TCL_OK) goto done;
      }
    }
  } else if (c == 'b' && strncmp(p, "body", length) == 0) {
    if (listArgc < 3 || listArgc > 4) goto invalidspec;
    if ((result = Mfv_FileSearchTest(interp, ifp, mesgPtr->begbody, 
	mesgPtr->endbody, listArgv[1], listArgv[2],
	(listArgc == 4 ? listArgv[3] : NULL), &sofar)) != TCL_OK)
      goto done;
  } else if (c == 'f' && strncmp(p, "field", length) == 0) {
    if (listArgc < 3 || listArgc > 5) goto invalidspec;
    Mfv_ToLower(listArgv[1]);
    buf = Mfv_GetHeader(interp, mesgPtr, listArgv[1]);
    if (listArgc == 3) {
      if ((result = Tcl_GetBoolean(interp, listArgv[2], &i)) != TCL_OK)
	goto done;
      if (buf != NULL) sofar = i;
      else sofar = (!i);
    } else {
      if ((result = Mfv_SearchTest(interp, buf, listArgv[2],
	  listArgv[3], (listArgc == 5 ? listArgv[4] : NULL),
	  &sofar)) != TCL_OK) goto done;
    }
  } else if (c == 'f' && strncmp(p, "flag", length) == 0) {
    if (listArgc != 3) goto invalidspec;
    for ( i = 0; (len = strlen(flagNames[i])); i++ ) {
      if (strncasecmp(listArgv[1], flagNames[i], len) == 0) break;
    }
    if (len == 0) {
      Tcl_AppendResult(interp, "Invalid flag \"", listArgv[1], "\"", (char *) NULL);
      result = TCL_ERROR;
      goto done;
    }
    if ((result = Tcl_GetBoolean(interp, listArgv[2], &j)) != TCL_OK)
      goto done;
    sofar = (mesgPtr->flags & (1<<i));
    if (!j) sofar = (!sofar);
  } else if (c == 'h' && strncmp(p, "header", length) == 0) {
    if (listArgc < 3 || listArgc > 4) goto invalidspec;
    if ((result = Mfv_FileSearchTest(interp, ifp, mesgPtr->beghead, 
	mesgPtr->endhead, listArgv[1], listArgv[2],
	(listArgc == 4 ? listArgv[3] : NULL), &sofar)) != TCL_OK)
      goto done;
  } else if (c == 'm' && strncmp(p, "message", length) == 0) {
    ismessage:
    if (listArgc < 3 || listArgc > 4) goto invalidspec;
    if ((result = Mfv_FileSearchTest(interp, ifp, mesgPtr->begmesg, 
	mesgPtr->endmesg, listArgv[1], listArgv[2],
	(listArgc == 4 ? listArgv[3] : NULL), &sofar)) != TCL_OK)
      goto done;
  } else if (c == 'm' && strncmp(p, "mesg", length) == 0) {
    goto ismessage;
  } else {
    invalidspec:
    Tcl_AppendResult(interp, "invalid message search spec \"", p,
	"\"", (char *) NULL);
    result = TCL_ERROR;
    goto done;
  }

  *match = sofar;
  if (neg) *match = !(*match);

  done:
  dbgfree((char *)listArgv);
  return result;
}


int Mfv_FolderSearch(Tcl_Interp *interp, Folder *folderPtr,
    char *spec, char *sortKeys, char *mesgs)
{
  int mesg, match, count;
  char *buf;
  Mesg **mesglist;

  if (folderPtr->count) {
    if ((count = Mfv_CreateMesgList(interp, folderPtr, mesgs,
	sortKeys, &mesglist)) < 0) return TCL_ERROR;
    buf = (char *)mesglist;

    for (mesg = 0; mesg < count; ++mesg, ++mesglist) {
      if (Mfv_MesgMatch(interp, *mesglist, spec, &match) != TCL_OK) {
	dbgfree(buf);
	return TCL_ERROR;
      }
      if (!match) *mesglist = NULL;
    }

    mesglist = (Mesg **)buf;
    for (mesg = 0; mesg < count; ++mesg, ++mesglist) {
      if (*mesglist) {
	sprintf(tmpLine,"%d",(*mesglist)->accessnum);
	Tcl_AppendElement(interp, tmpLine);
      }
    }
    dbgfree(buf);
  }

  return TCL_OK;
}

void Mfv_AddReplyThread(Mesg *mesgPtr, Mesg *replyPtr)
{
  Thread *thrd, *reply;
  
  thrd = (Thread *)dbgalloc(sizeof(Thread));
  thrd->mesgPtr = replyPtr;
  thrd->next = NULL;

  if ((reply = mesgPtr->replies) == NULL)
    mesgPtr->replies = thrd;
  else {
    while(reply->next != NULL) reply = reply->next;
    reply->next = thrd;
  }
}

int Mfv_FolderThread(Tcl_Interp *interp, Folder *folderPtr, Mesg ***retlist)
{
  int mesg, tmesg, count;
  char *buf, *mesgid, *replyid, *tmesgid, *treplyid;
  Mesg **mesglist, **tmesglist;

  if (retlist) *retlist = NULL;
  
  if (folderPtr->count && !folderPtr->threaded) {
    if ((count = Mfv_CreateMesgList(interp, folderPtr, NULL,
	"sent", &mesglist)) < 0) return TCL_ERROR;
    buf = (char *)mesglist;

    for (mesg = 0; mesg < count; ++mesg, ++mesglist) {
      (*mesglist)->mesgid = Mfv_GetHeader(interp, *mesglist, "message-id");
      (*mesglist)->replyid = Mfv_GetHeader(interp, *mesglist, "in-reply-to");
    }
    
    mesglist = (Mesg **)buf;
    for (mesg = 0; mesg < count; ++mesg, ++mesglist) {
      for (tmesg = mesg+1, tmesglist = mesglist+1;
	   tmesg < count; ++tmesg, ++tmesglist) {
	if ((*mesglist)->flags & MFV_THREADED &&
	    (*tmesglist)->flags & MFV_THREADED) continue;
	if ((*mesglist)->replyid && (*tmesglist)->mesgid &&
	    strcmp((*mesglist)->replyid, (*tmesglist)->mesgid) == 0) {
	  (*mesglist)->replyto = *tmesglist;
	  Mfv_AddReplyThread(*tmesglist, *mesglist);
	}
	if ((*tmesglist)->replyid && (*mesglist)->mesgid &&
	    strcmp((*tmesglist)->replyid, (*mesglist)->mesgid) == 0){
	  (*tmesglist)->replyto = *mesglist;
	  Mfv_AddReplyThread(*mesglist, *tmesglist);
	}
      }
      (*mesglist)->flags |= MFV_THREADED;
    }

    if (retlist) *retlist = (Mesg **) buf;
    else dbgfree(buf);
  }

  folderPtr->threaded = TRUE;
  return TCL_OK;
}

void Mfv_FolderThreadItem(Tcl_Interp *interp, Mesg *mesgPtr)
{
  char num[25];
  Thread *thrd;
  
  sprintf(num, "%d", mesgPtr->num);
  Tcl_AppendResult(interp, "{", num, (char *)NULL);

  if ((thrd = mesgPtr->replies) != NULL) {
    Tcl_AppendResult(interp, " {", (char *)NULL);
    while(thrd != NULL) {
      Mfv_FolderThreadItem(interp, thrd->mesgPtr);
      thrd = thrd->next;
    }
    Tcl_AppendResult(interp, "}", (char *)NULL);
  }
  Tcl_AppendResult(interp, "} ", (char *)NULL);
}

int Mfv_FolderThreadList(Tcl_Interp *interp, Folder *folderPtr)
{
  int mesg, count;
  Mesg **mesglist, **savelist;

  if (count = folderPtr->count) {
    if (!folderPtr->threaded) {
      if (Mfv_FolderThread(interp, folderPtr, &mesglist) != TCL_OK)
	return TCL_ERROR;
    }
    else
      if ((count = Mfv_CreateMesgList(interp, folderPtr, NULL,
	  "sent", &mesglist)) < 0) return TCL_ERROR;
      
    savelist = mesglist;
    for (mesg = 0; mesg < count; ++mesg, ++mesglist) {
      if ((*mesglist)->replyto == NULL)
	Mfv_FolderThreadItem(interp, *mesglist);
    }

    dbgfree((char *)savelist);
  }

  return TCL_OK;
}

int Mfv_FolderCmd(ClientData cd, Tcl_Interp *interp, int argc, char *argv[])
{
  char c, *p, *p2, *buf, num[25];
  int i, mesg, len, type, result;
  size_t length;
  Folder *folderPtr;
  Tcl_HashSearch search;
  Tcl_HashEntry *entryPtr;
  Mesg *mesgPtr, **mesglist;
  Thread *thrd;
  VFLink *nextVF;
  MimePart *mp;

  if (argc < 2) {
    Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                " command ?arg ...?\"", (char *) NULL);
    return TCL_ERROR;
  }

  folderPtr = Mfv_GetFolder(interp, argv[0]);
  if (folderPtr == NULL) return TCL_ERROR;

  c = argv[1][0];
  length = strlen(argv[1]);

  if ((c == 'a') && (strncmp(argv[1], "append", length) == 0)) {
    if (argc != 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " append folderName\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (Mfv_AppendFolder(interp, folderPtr, argv[2]) != TCL_OK)
      return TCL_ERROR;
    goto CHECKAPPEND;
  } else if ((c == 'c') && (strncmp(argv[1], "check", length) == 0)) {
    if (argc != 2) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " check\"", (char *) NULL);
      return TCL_ERROR;
    }
    CHECKAPPEND:
    if ((mesg = Mfv_CheckAppend(interp, folderPtr)) < 0) {
      corrupt:
      fprintf(stderr, "CORRUPT: %d\n", mesg);
      Tcl_AppendResult(interp, "folder \"", folderPtr->name,
	  "\" has been externally corrupted.", (char *) NULL);
      Mfv_InformClose(interp, folderPtr->key);
      Mfv_CloseFolder(interp, folderPtr);
      return TCL_ERROR;
    }
    sprintf(interp->result, "%d", mesg);

  } else if ((c == 'i') && (strncmp(argv[1], "info", length) == 0)) {
    if (argc < 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " info infoItem ?options?\"", (char *) NULL);
      return TCL_ERROR;
    }
    c = argv[2][0];
    length = strlen(argv[2]);

    if ((c == 'c') && (strncmp(argv[2], "count", length) == 0)) {
      if (argc != 3) {
	not3infoargs:
	Tcl_AppendResult(interp, wrongNumArgs, argv[0],
		      " info ", argv[2], "\"", (char *) NULL);
	return TCL_ERROR;
      }
      sprintf(interp->result, "%d", folderPtr->count);
    } else if ((c == 'd') && (strncmp(argv[2], "deleted", length) == 0)
	       && (length >= 2)) {
      if (argc != 3) goto not3infoargs;
      for(entryPtr = Tcl_FirstHashEntry(&(folderPtr->mesgTable),&search);
	  entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
	mesgPtr = (Mesg *) Tcl_GetHashValue(entryPtr);
	mesgPtr->accessnum =
	    (int) Tcl_GetHashKey(&(folderPtr->mesgTable), entryPtr);
	if (mesgPtr && (mesgPtr->flags & MFV_DELETED)) {
	  sprintf(tmpLine,"%d",mesgPtr->accessnum);
	  Tcl_AppendElement(interp, tmpLine);
	}
      }
    } else if ((c == 'd') && (strncmp(argv[2], "directory", length) == 0)
	       && (length >= 2)) {
      if (argc != 3) goto not3infoargs;
      Tcl_AppendResult(interp, folderPtr->dir, (char *) NULL);
    } else if ((c == 'm') && (strncmp(argv[2], "modified", length) == 0) &&
	       length > 1) {
      if (argc != 3) goto not3infoargs;
      for(entryPtr = Tcl_FirstHashEntry(&(folderPtr->mesgTable),&search);
	  entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
	mesgPtr = (Mesg *) Tcl_GetHashValue(entryPtr);
	mesgPtr->accessnum =
	    (int) Tcl_GetHashKey(&(folderPtr->mesgTable), entryPtr);
	if ((mesgPtr->flags & MFV_DELETED) ||
	    (mesgPtr->flags & MFV_STATUS) != (mesgPtr->origflags & MFV_STATUS) ||
	    !(mesgPtr->flags & MFV_OLD) ||
	    (mesgPtr->flags & MFV_XSTATUS) != (mesgPtr->origflags & MFV_XSTATUS)) {
	  sprintf(tmpLine,"%d",mesgPtr->accessnum);
	  Tcl_AppendElement(interp, tmpLine);
	}
      }
    } else if ((c == 'm') && (strncmp(argv[2], "max", length) == 0) &&
	       length > 1) {
      if (argc != 3) goto not3infoargs;
      sprintf(interp->result, "%d", folderPtr->max);
    } else if ((c == 'n') && (strncmp(argv[2], "name", length) == 0)
	       && length > 1) {
      if (argc != 3) goto not3infoargs;
      Tcl_AppendResult(interp, folderPtr->name, (char *) NULL);
    } else if ((c == 'n') && (strncmp(argv[2], "new", length) == 0)
	       && length > 1) {
      if (argc != 3) goto not3infoargs;
      for(entryPtr = Tcl_FirstHashEntry(&(folderPtr->mesgTable),&search);
	  entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
	mesgPtr = (Mesg *) Tcl_GetHashValue(entryPtr);
	mesgPtr->accessnum =
	    (int) Tcl_GetHashKey(&(folderPtr->mesgTable), entryPtr);
	if (mesgPtr && !(mesgPtr->flags & (MFV_READ|MFV_OLD))) {
	  sprintf(tmpLine,"%d",mesgPtr->accessnum);
	  Tcl_AppendElement(interp, tmpLine);
	}
      }
    } else if ((c == 'r') && (strncmp(argv[2], "readonly", length) == 0)) {
      if (argc != 3) goto not3infoargs;
      sprintf(interp->result, "%d", folderPtr->readonly);
    } else if ((c == 't') && (strncmp(argv[2], "type", length) == 0)) {
      if (argc == 3) {
	Tcl_AppendResult(interp, folderTypes[folderPtr->type], (char *) NULL);
      } else if (argc == 4) {
	if (folderPtr->count > 0 && folderPtr->type != MFV_UNKNOWN) {
	  Tcl_AppendResult(interp, "cannot change format type of folder",
	      " that already has parsed messages", (char *) NULL);
	  return TCL_ERROR;
	} else {
	  if ((i = Mfv_StringToType(argv[3])) == 0) {
	    Tcl_AppendResult(interp, "unrecognized/illegal folder format type \"",
		argv[3], "\"", (char *) NULL);
	    return TCL_ERROR;
	  } else {
	    folderPtr->type = i;
	  }
	}
      } else {
	Tcl_AppendResult(interp, wrongNumArgs, argv[0],
		      " info type ?formatType?\"", (char *) NULL);
	return TCL_ERROR;
      }
    } else {
      Tcl_AppendResult(interp, "bad info item \"", argv[2],
	  "\": should be count, deleted, directory, max, modified, name, ",
	  "readonly or type", (char *) NULL);
      return TCL_ERROR;
    }

  } else if ((c == 'l') && (strncmp(argv[1], "lock", length) == 0)) {
    if (argc != 2) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0], " lock\"", (char *) NULL);
      return TCL_ERROR;
    }
    sprintf(interp->result, "%d", i = Mfv_LockFolder(folderPtr));

  } else if ((c == 'm') && (strncmp(argv[1], "message", length) == 0)) {
    if (argc < 4) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " ", argv[1], " option mesgnum ?arg ...?\"", (char *) NULL);
      return TCL_ERROR;
    }

    c = argv[2][0];
    length = strlen(argv[2]);

    if (Tcl_GetInt(interp, argv[3], &mesg) != TCL_OK) return TCL_ERROR;
    mesgPtr = Mfv_GetMesg(interp, folderPtr, mesg);

    if (c == 'e' && strncmp(argv[2], "exists", length) == 0) {
      Tcl_ResetResult(interp);
      if (mesgPtr) sprintf(interp->result, "1");
      else sprintf(interp->result, "0");
      return TCL_OK;
    }

    if (mesgPtr == NULL) {
      Tcl_AppendResult(interp, " in folder \"", argv[0],
	  "\"", (char *) NULL);
      return TCL_ERROR;
    }

    if ((c == 'b') && (strncmp(argv[2], "body", length) == 0)) {
      if (argc != 4) {
	argv[2] = "contents";
	goto not4args;
      }
      if ((buf = Mfv_GetFilePart(mesgPtr, mesgPtr->begbody,
	  mesgPtr->endbody)) == NULL) goto corrupt;
      mesgPtr->flags |= MFV_READ;
      Tcl_SetResult(interp, buf, TCL_DYNAMIC);
    } else if ((c == 'c') && (strncmp(argv[2], "chars", length) == 0)
            && (length >= 2)) {
      if (argc != 4) {
	argv[2] = "chars";
	not4args:
	Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	    " message ", argv[2], " mesgNum", (char *) NULL);
	return TCL_ERROR;
      }
      sprintf(interp->result, "%d", mesgPtr->chars);
    } else if ((c == 'c') && (strncmp(argv[2], "contents", length) == 0)
            && (length >= 2)) {
      if (argc != 4) {
	argv[2] = "contents";
	goto not4args;
      }
      if ((buf = Mfv_GetFilePart(mesgPtr, mesgPtr->begmesg,
	  mesgPtr->endmesg)) == NULL) goto corrupt;
      mesgPtr->flags |= MFV_READ;
      Tcl_SetResult(interp, buf, TCL_DYNAMIC);
    } else if ((c == 'd') && (strncmp(argv[2], "delete", length) == 0)) {
      if (argc != 4) {
	argv[2] = "delete";
	goto not4args;
      }
      mesgPtr->flags |= MFV_DELETED;
    } else if ((c == 'f') && (strncmp(argv[2], "field", length) == 0)
	&& (length > 1)) {
      if (argc < 4 || argc > 5) {
	Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	    " message field mesgnum ?fieldName?", (char *) NULL);
	return TCL_ERROR;
      }
      if (argc==5) {
	Mfv_ToLower(argv[4]);
	buf = Mfv_GetHeader(interp, mesgPtr, argv[4]);
	if (buf != NULL)
	  Tcl_AppendResult(interp, buf, (char *) NULL);
      } else {
	for (entryPtr = Tcl_FirstHashEntry(&(mesgPtr->headerTable),&search);
	     entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
	  Tcl_AppendElement(interp,
	      Tcl_GetHashKey(&(mesgPtr->headerTable),entryPtr));
	}
      }
    } else if ((c == 'f') && (strncmp(argv[2], "flag", length) == 0)
	&& (length > 1)) {
      if (argc < 4 || argc > 6 ) {
	Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	    " message flag mesgnum ?flagName? ?boolean?", (char *) NULL);
	return TCL_ERROR;
      }
      if (argc > 4) {
	for ( i = 0; (len = strlen(flagNames[i])); i++ ) {
	  if (strncasecmp(argv[4], flagNames[i], len) == 0) break;
	}
	if (len == 0) {
	  Tcl_AppendResult(interp, "Invalid flag \"", argv[4], "\"", (char *) NULL);
	  return TCL_ERROR;
	}
	if (argc == 6) {
	  if (Tcl_GetBoolean(interp, argv[5], &type) != TCL_OK) return TCL_ERROR;
	  if (type) mesgPtr->flags |= (1<<i);
	  else mesgPtr->flags &= ~(1<<i);
	} else {
	  strcpy(interp->result, (mesgPtr->flags & (1<<i)) ? "1" : "0");	
	}
      } else {
	for ( i = 0; (len = strlen(flagNames[i])); i++ ) {
	  Tcl_AppendResult(interp, "{", flagNames[i], " ",
	      ((mesgPtr->flags & (1<<i)) ? "1" : "0"), "} ", (char *) NULL);
	}
      }
    } else if ((c == 'h') && (strncmp(argv[2], "headers", length) == 0)) {
      if (argc < 4 || argc > 5) {
	Tcl_AppendResult(interp, wrongNumArgs, argv[0],
		      " message headers mesgNum full\"", (char *) NULL);
	return TCL_ERROR;
      }
      if (filterCount == 0 || (
	argc > 4 && strncmp(argv[4], "full", strlen(argv[4])) == 0)) {
	if ((buf = Mfv_GetFilePart(mesgPtr, mesgPtr->beghead,
	    mesgPtr->endhead)) == NULL) goto corrupt;
	Tcl_SetResult(interp, buf, TCL_DYNAMIC);
      } else {
	return Mfv_FilterHeaders(interp, mesgPtr);
      }
    } else if ((c == 'l') && (strncmp(argv[2], "lines", length) == 0)
	       && (length >= 4)) {
      if (argc != 4) {
	argv[2] = "contents";
	goto not4args;
      }
      sprintf(interp->result, "%d", mesgPtr->lines);
    } else if ((c == 'l') && (strncmp(argv[2], "links", length) == 0)
	       && (length >= 5)) {
      if (argc != 4) {
	argv[2] = "links";
	goto not4args;
      }
      Tcl_AppendElement(interp, folderPtr->key);
      nextVF = mesgPtr->links;
      while (nextVF) {
	Tcl_AppendElement(interp, nextVF->folder->key);
	nextVF = nextVF->next;
      }
    } else if ((c == 'l') && (strncmp(argv[2], "linkto", length) == 0)
	       && (length >= 5)) {
      if (argc != 5) {
	Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	    " message linkto mesgNum virtualFolderID", (char *) NULL);
	return TCL_ERROR;
      }
      folderPtr = Mfv_GetFolder(interp, argv[4]);
      if (folderPtr == NULL) return TCL_ERROR;
      if (folderPtr->type != MFV_VIRTUAL) {
	Tcl_AppendResult(interp, "folder \"", folderPtr->name,
	    "\" is not a virtual folder", (char *)NULL);
	return TCL_ERROR;
      }
      Mfv_AppendVFLink(interp, folderPtr, mesgPtr);
    } else if ((c == 'm') && (strncmp(argv[2], "mimelist", length) == 0)
	       && (length >= 5)) {
      if (argc != 4) {
	argv[2] = "mimelist";
	goto not4args;
      }
      return Mfv_MimeList(interp, mesgPtr);
    } else if ((c == 'm') && (strncmp(argv[2], "mimepart", length) == 0)
	       && (length >= 5)) {
      if (argc < 6) {
	Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	    " message mimepart mesgnum request partnum ?requestOptions?",
	    (char *) NULL);
	return TCL_ERROR;
      }
      if (Mfv_ParseMimeMesg(interp, mesgPtr) != TCL_OK) return TCL_ERROR;
      mp = Mfv_GetMimePart(interp, mesgPtr->mimepart, argv[5]);
      if (mp == NULL) {
	if (!strlen(interp->result)) {
	  Tcl_AppendResult(interp, "no mimepart with number \"",
	      argv[5], "\"", (char *) NULL);
	}
	return TCL_ERROR;
      }

      c = argv[4][0];
      length = strlen(argv[4]);
      if ((c == 'b') && (strncmp(argv[4], "body", length) == 0)) {
	if ((buf = Mfv_GetFilePart(mesgPtr,
	    mp->begpart, mp->endpart)) == NULL) goto corrupt;
	if (argc == 7 &&
	    (i = Mfv_ContentDecode(buf, argv[6])) != MFV_OK) {
	  dbgfree(buf);
	  if (i == MFV_ENCODE_ERR)
	    Tcl_AppendResult(interp, "Unknown encoding \"",
		argv[6], "\"", (char *) NULL);
	  else if (i == MFV_PARSE_ERR)
	    Tcl_AppendResult(interp, "parse error decoding \"",
		argv[6], "\" data", (char *) NULL);
	  return TCL_ERROR;
	}
	mesgPtr->flags |= MFV_READ;
	Tcl_SetResult(interp, buf, TCL_DYNAMIC);
      } else if ((c == 'd') && (strncmp(argv[4], "description", length) == 0)
		 && length > 1) {
	if (mp->descr) Tcl_AppendResult(interp, mp->descr, (char *) NULL);	
      } else if ((c == 'd') && (strncmp(argv[4], "disposition", length) == 0)
		 && length > 1) {
	if (mp->dispos) Tcl_AppendResult(interp, mp->dispos, (char *) NULL);	
      } else if ((c == 'e') && (strncmp(argv[4], "encoding", length) == 0)) {
	Tcl_AppendResult(interp, mp->encoding, (char *) NULL);	
      } else if ((c == 'i') && (strncmp(argv[4], "id", length) == 0)) {
	if (mp->id) Tcl_AppendResult(interp, mp->id, (char *) NULL);	
      } else if ((c == 'p') && (strncmp(argv[4], "parameter", length) == 0)) {
	if (argc>6) {
	  Mfv_ToLower(argv[6]);
	  entryPtr = Tcl_FindHashEntry(&(mp->paramTable), argv[6]);
	  if (entryPtr != NULL) {
	    Tcl_AppendResult(interp, Tcl_GetHashValue(entryPtr), (char *) NULL);
	  }
	} else {
	  for (entryPtr = Tcl_FirstHashEntry(&(mp->paramTable),&search);
	       entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
	    Tcl_AppendElement(interp,
		Tcl_GetHashKey(&(mp->paramTable),entryPtr));
	  }
	}	
      } else if ((c == 's') && (strncmp(argv[4], "subtype", length) == 0)) {
	Tcl_AppendResult(interp, mp->subtype, (char *) NULL);	
      } else if ((c == 't') && (strncmp(argv[4], "type", length) == 0)) {
	Tcl_AppendResult(interp, mp->type, (char *) NULL);	
      } else if ((c == 'w') && (strncmp(argv[4], "write", length) == 0)) {
	if (argc < 7 || argc > 8) {
	  Tcl_AppendResult(interp, "wrong # args", (char *)NULL);
	  return TCL_ERROR;
	}
	p = argv[6];
	if (strlen(p) == 0) {
	  if (mp->tmpfile != NULL) {
	    if (access(mp->tmpfile,F_OK) == 0) {
	      Tcl_AppendResult(interp, mp->tmpfile, (char *)NULL);
	      return TCL_OK;
	    } else {
	      dbgfree(mp->tmpfile);
	    }
	  }
	  p = mp->tmpfile = Mfv_TmpName("mfv", NULL);
	}
	if (Mfv_WriteFilePart(interp, mesgPtr, mp->begpart,
	    mp->endpart, (argc==8 ? argv[7] : NULL), p) != TCL_OK) {
	  if (mp->tmpfile) {
	    unlink(mp->tmpfile);
	    dbgfree(mp->tmpfile);
	  }
	  goto corrupt;
	}
	Tcl_AppendResult(interp, p, (char *)NULL);
      } else {
	Tcl_AppendResult(interp, "bad mimepart option \"", argv[4],
	    "\": should be body, description, encoding, id, ",
	    "parameter, subtype, type or write", (char *)NULL);
	return TCL_ERROR;
      }
    } else if ((c == 'o') && (strncmp(argv[2], "outline", length) == 0)) {
      if (argc != 4) {
	argv[2] = "outline";
	goto not4args;
      }
      return Mfv_MimeOutline(interp, mesgPtr);
    } else if ((c == 'r') && (strncmp(argv[2], "received", length) == 0)
	       && (length > 2)) {
      if (argc != 4) {
	argv[2] = "received";
	goto not4args;
      }
      sprintf(interp->result, "%ld", (long) mesgPtr->time_received);
    } else if ((c == 'r') && (strncmp(argv[2], "replies", length) == 0)
	       && (length > 3)) {
      if (argc != 4) {
	argv[2] = "replies";
	goto not4args;
      }
      if (!folderPtr->threaded) {
	if (Mfv_FolderThread(interp, folderPtr, NULL) != TCL_OK)
	  return TCL_ERROR;
      }
      for (thrd = mesgPtr->replies; thrd != NULL; thrd = thrd->next) {
	sprintf(num, "%d", thrd->mesgPtr->num);
	Tcl_AppendElement(interp, num);
      }
    } else if ((c == 'r') && (strncmp(argv[2], "replyto", length) == 0)
	       && (length > 3)) {
      if (argc != 4) {
	argv[2] = "replyto";
	goto not4args;
      }
      if (!folderPtr->threaded) {
	if (Mfv_FolderThread(interp, folderPtr, NULL) != TCL_OK)
	  return TCL_ERROR;
      }
      if (mesgPtr->replyto)
	sprintf(interp->result, "%d", mesgPtr->replyto->num);
    } else if ((c == 's') && (strncmp(argv[2], "sent", length) == 0)
	       && (length >= 2)) {
      if (argc != 4) {
	argv[2] = "sent";
	goto not4args;
      }
      sprintf(interp->result, "%ld", (long) mesgPtr->time_sent);
    } else if ((c == 's') && (strncmp(argv[2], "summary", length) == 0)
	       && (length >= 2)) {
      if (argc != 4) {
	argv[2] = "sent";
	goto not4args;
      }
      if (Mfv_GetSumLine(interp,mesgPtr) != TCL_OK)
	return TCL_ERROR;
      Tcl_SetResult(interp, tmpLine, TCL_VOLATILE);
    } else if ((c == 'u') && (strncmp(argv[2], "undelete", length) == 0)
	       && (length >= 3)) {
      if (argc != 4) {
	argv[2] = "undelete";
	goto not4args;
      }
      mesgPtr->flags &= ~MFV_DELETED;
    } else if ((c == 'w') && (strncmp(argv[2], "write", length) == 0)) {
      if (argc != 5) {
	Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	    " message write mesgnum folderName", (char *) NULL);
	return TCL_ERROR;
      }
      return Mfv_AppendMesg(interp, &mesgPtr, 1, argv[4], 0);
    } else {
      Tcl_AppendResult(interp, "bad message option \"", argv[2],
	  "\": should be body, chars, contents, delete, field, flag,",
	  " headers, lines, links, linkto,",
	  " mimelist, mimepart, received, sent, summary,"
	  " undelete or write", (char *) NULL);
      return TCL_ERROR;
    }

  } else if ((c == 's') && (strncmp(argv[1], "save", length) == 0)
	     && (length >= 2)) {
    if (argc < 2) {
      wrongsaveargs:
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	  " ", argv[1], " ?switches? ?fileName?\"", (char *) NULL);
      return TCL_ERROR;
    }
    type = MFV_UNKNOWN; p = "normal";
    for (i=2; i < argc && argv[i][0] == '-'; ++i ) {
      if (strcmp(argv[i],"--") == 0) {
	++i; break;
      } else if (strcmp(argv[i],"-sorted") == 0) {
	++i;
	if (i >= argc) goto wrongsaveargs;
	p = argv[i];
      } else if (strcmp(argv[i],"-type") == 0) {
	++i;
	if ((type = Mfv_StringToType(argv[i]))==0) {
	  Tcl_AppendResult(interp, "unknown folder format type \"",
	      argv[i], "\"", (char *) NULL);
	  return TCL_ERROR;
	}
      } else {
	Tcl_AppendResult(interp, "invalid switch: should be ",
	    "-sorted or -type", (char *) NULL);
	return TCL_ERROR;
      }
    }
    if (argc > i+1) goto wrongsaveargs;
    result = Mfv_SaveFolder(interp, folderPtr,
	(i < argc ? argv[i] : NULL), p, type);
    return result;
  } else if ((c == 's') && (strncmp(argv[1], "search", length) == 0)
	     && (length >= 2)) {
    if (argc < 3) {
      wrongsearchargs:
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " search ?switches? searchSpec\"", (char *) NULL);
      return TCL_ERROR;
    }
    p = "normal"; p2 = NULL;
    for (i=2; i < argc && argv[i][0] == '-'; ++i ) {
      if (strcmp(argv[i],"--") == 0) {
	++i; break;
      } else if (strcmp(argv[i],"-sorted") == 0) {
	++i;
	if (i >= argc) goto wrongsearchargs;
	p = argv[i];
      } else if (strcmp(argv[i],"-messages") == 0 ||
	  strcmp(argv[i],"-mesgs") == 0) {
	++i;
	if (i >= argc) goto wrongsortargs;
	p2 = argv[i];
      } else {
	Tcl_AppendResult(interp, "invalid switch: should be ",
	    "-sorted", (char *) NULL);
	return TCL_ERROR;
      }
    }
    if (i != argc-1) goto wrongsearchargs;
    return Mfv_FolderSearch(interp, folderPtr, argv[i], p, p2);
  } else if ((c == 's') && (strncmp(argv[1], "sort", length) == 0)
	     && (length >= 2)) {
    if (argc < 3) {
      wrongsortargs:
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " sort ?switches? sortKeys\"", (char *) NULL);
      return TCL_ERROR;
    }
    p = NULL;
    for (i=2; i < argc && argv[i][0] == '-'; ++i ) {
      if (strcmp(argv[i],"--") == 0) {
	++i; break;
      } else if (strcmp(argv[i],"-messages") == 0 ||
	  strcmp(argv[i],"-mesgs") == 0) {
	++i;
	if (i >= argc) goto wrongsortargs;
	p = argv[i];
      } else {
	Tcl_AppendResult(interp, "invalid switch: should be ",
	    "-sorted", (char *) NULL);
	return TCL_ERROR;
      }
    }
    if (i != argc-1) goto wrongsortargs;
    if (folderPtr->count) {
      if ((len = Mfv_CreateMesgList(interp, folderPtr, p,
	  argv[i], &mesglist)) < 0) return TCL_ERROR;
      buf = (char *)mesglist;
      for (mesg = 0; mesg < len; ++mesg, ++mesglist) {
	sprintf(tmpLine,"%d",(*mesglist)->accessnum);
	Tcl_AppendElement(interp, tmpLine);
      }
      dbgfree(buf);
    }
  } else if ((c == 't') && (strncmp(argv[1], "threads", length) == 0)) {
    if (argc != 2) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	  " threads\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (folderPtr->type == MFV_VIRTUAL) {
      Tcl_AppendResult(interp, "Virtual folders do not support threads",
	  (char *) NULL);
      return TCL_ERROR;
    }
    return Mfv_FolderThreadList(interp, folderPtr);

  } else if ((c == 'u') && (strncmp(argv[1], "unlock", length) == 0)) {
    if (argc != 2) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	  " unlock\"", (char *) NULL);
      return TCL_ERROR;
    }
    sprintf(interp->result, "%d", Mfv_UnlockFolder(folderPtr));

  } else if ((c == 'w') && (strncmp(argv[1], "write", length) == 0)) {
    if (argc != 4) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	  " write mesgList folderName", (char *) NULL);
      return TCL_ERROR;
    }
    if ((len = Mfv_CreateMesgList(interp, folderPtr, argv[2],
	NULL, &mesglist)) < 1) return TCL_ERROR;
    return Mfv_AppendMesg(interp, mesglist, len, argv[3], 0);
    
  } else {
    Tcl_AppendResult(interp, "bad option \"", argv[1], "\": should be "
	"check, info, lock, message, save, search, sort or unlock",
	(char *) NULL);
    return TCL_ERROR;
  }

  return TCL_OK;
}

int Mfv_UtilCmd(ClientData cd, Tcl_Interp *interp, int argc, char *argv[])
{
  int i, stripq, stripw;
  char c, *p, *dir;
  size_t length;
  time_t now;
  struct tm *tm;
  static int uniq_id = 0xA00;
  Tcl_HashSearch search;
  Tcl_HashEntry *entryPtr;
  struct stat statbuf;
  Folder *folderPtr;
  Tcl_DString buffer;
  FILE *ifp, *ofp;
  
  if (argc < 2) {
    Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                " option ?arg ...?\"", (char *) NULL);
    return TCL_ERROR;
  }

  c = argv[1][0];
  length = strlen(argv[1]);

  if ((c == 'b') && (strncmp(argv[1], "boundary", length) == 0)) {
    if (argc != 2) {
      argv[1] = "boundary";
      not2args:
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " ", argv[1], "\"", (char *) NULL);
      return TCL_ERROR;
    }
    time(&now);
    tm = localtime(&now);
    p = dbgalloc(60*sizeof(char)+sizeof(localhost));
    sprintf(p, "%02d%02d%02d%02d%02d%02d-%s-%05d",
	tm->tm_year, tm->tm_mon+1, tm->tm_mday, tm->tm_hour,
	tm->tm_min, tm->tm_sec, localhost, ++uniq_id);
    if (uniq_id > 0xFFC) uniq_id = 0xA00;
    for (dir = p; *dir != '-' && *dir; dir++) *dir += 50;
    Tcl_SetResult(interp, p, TCL_DYNAMIC);
  } else if ((c == 'e') && (strncmp(argv[1], "encode", length) == 0)) {
    if (argc != 5) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " encode encodeType inFileID outFileID\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (Tcl_GetOpenFile(interp, argv[i=3], FALSE, TRUE, &ifp) != TCL_OK) {
      badfileid:
      Tcl_AppendResult(interp, "bad file id \"", argv[i], "\"", (char *) NULL);
      return TCL_ERROR;
    }
    if (Tcl_GetOpenFile(interp, argv[i=4], TRUE, TRUE, &ofp) != TCL_OK) {
      goto badfileid;
    }
    
    c = argv[2][0];
    length = strlen(argv[2]);
    if (c == 'q' && strncasecmp(argv[2], "quoted-printable", length) == 0) {
      Mfv_QuotedPrEncodePipe(ifp, ofp, 0);
    } else if (c == 'b' && strncasecmp(argv[2], "base64", length) == 0) {
      Mfv_Base64EncodePipe(ifp, ofp, 0, 0);      
    } else {
      Tcl_AppendResult(interp, "unknown encoding \"", argv[2], "\"", (char *) NULL);
      return TCL_ERROR;
    }
  } else if ((c == 'f') && (strncmp(argv[1], "folderid", length) == 0)
	     && length > 1) {
    /* TODO: doesn't work when file is empty */
    if (argc != 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " folderid fileName\"", (char *) NULL);
      return TCL_ERROR;
    }
    if ((p = Tcl_TildeSubst(interp, argv[2], &buffer)) == NULL) {
      Tcl_ResetResult(interp);
      return TCL_OK;
    }
    statbuf.st_dev = statbuf.st_ino = 0;
    if(stat(p, &statbuf) != -1 || errno == ENOENT) {
      if (folderPtr = Mfv_CheckIsOpen(statbuf.st_dev, statbuf.st_ino, p)) {
	Tcl_AppendResult(interp, folderPtr->key, (char *)NULL);
      }
    }
    Tcl_DStringFree(&buffer);
  } else if ((c == 'f') && (strncmp(argv[1], "fullpath", length) == 0)
	     && length > 1) {
    if (argc != 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
                    " fullpath fileName\"", (char *) NULL);
      return TCL_ERROR;
    }
    if ((p = Tcl_TildeSubst(interp, argv[2], &buffer)) == NULL) {
      return TCL_ERROR;
    }
    if ((dir = Mfv_Fullpath(p, FALSE)) == NULL) {
      Tcl_AppendResult(interp, "invalid file path", (char *)NULL);
      Tcl_DStringFree(&buffer);
      return TCL_ERROR;
    }
    Tcl_AppendResult(interp, dir, (char *)NULL);
    dbgfree(dir);
    Tcl_DStringFree(&buffer);
  } else if ((c == 'h') && (strncmp(argv[1], "home", length) == 0)
	     && length > 2) {
    if (argc != 2) {
      argv[1] = "home";
      goto not2args;
    }
    Tcl_SetResult(interp, homedir, TCL_STATIC);
  } else if ((c == 'h') && (strncmp(argv[1], "host", length) == 0)
	     && length > 2) {
    if (argc != 2) {
      argv[1] = "host";
      goto not2args;
    }
    Tcl_SetResult(interp, localhost, TCL_STATIC);
  } else if ((c == 'l') && (strncmp(argv[1], "list", length) == 0)
	     && length > 1) {
    if (argc != 2) {
      argv[1] = "list";
      goto not2args;
    }
    for (entryPtr = Tcl_FirstHashEntry(&folderTable,&search);
	 entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&search)) {
      Tcl_AppendElement(interp,
	  Tcl_GetHashKey(&folderTable,entryPtr));
    }
  } else if ((c == 'l') && (strncmp(argv[1], "lock", length) == 0) &&
	     length > 3) {
    if (argc != 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	  " lock fileName\"", (char *) NULL);
      return TCL_ERROR;
    }
    sprintf(interp->result, "%d", Mfv_LockFile(argv[2]));

  } else if ((c == 'l') && (strncmp(argv[1], "lockerror", length) == 0)
	     && length > 3) {
    if (argc != 2) {
      argv[1] = "lockerror";
      goto not2args;
    }
    Tcl_AppendResult(interp, lock_err, (char *)NULL);
  } else if ((c == 'm') && (strncmp(argv[1], "mesgid", length) == 0)) {
    if (argc != 2) {
      argv[1] = "mesgid";
      goto not2args;
    }
    time(&now);
    tm = localtime(&now);
    p = dbgalloc((60+sizeof(localhost))*sizeof(char));
    sprintf(p, "<%.8s-%02d%02d%02d%02d%02d%02d.%3X%d@%s>",
	username, tm->tm_year, tm->tm_mon+1, tm->tm_mday, tm->tm_hour,
	tm->tm_min, tm->tm_sec, ++uniq_id, getpid(), localhost);
    if (uniq_id > 0xFFC) uniq_id = 0xA00;
    Tcl_SetResult(interp, p, TCL_DYNAMIC);
  } else if ((c == 's') && (strncmp(argv[1], "simplify", length) == 0)) {
    stripq = FALSE;
    stripw = FALSE;
    for (i = 2; i < argc && argv[i][0] == '-'; ++i) {
      if (strcmp(argv[i],"--") == 0) { i++; break; }
      if (strcmp(argv[i],"-stripq") == 0) stripq = TRUE;
      else if (strcmp(argv[i],"-stripw") == 0) stripw = TRUE;
      else {
	Tcl_AppendResult(interp, "invalid simplify option: should be "
	    "-stripq or -stripw", (char *)NULL);
	return TCL_ERROR;
      }
    }
    if (argc == i+1)
      Tcl_SetResult(interp,
	  Mfv_CompressHeader(argv[i], stripq, stripw), TCL_VOLATILE);
    else {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	  " simplify ?-stripq? fieldText\"", (char *) NULL);
      return TCL_ERROR;
    }
  } else if ((c == 't') && (strncmp(argv[1], "tmpfile", length) == 0)) {
    if (argc < 2 || argc > 4) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	  " tmpfile ?prefix? ?directory? \"", (char *) NULL);
      return TCL_ERROR;
    }
    dir = (argc>3 ? argv[3] : NULL);
    if (dir && (dir = Tcl_TildeSubst(interp, dir, &buffer)) == NULL) {
      return TCL_ERROR;
    }
    p = Mfv_TmpName( argc>2 ? argv[2] : NULL, dir );
    if (dir) Tcl_DStringFree(&buffer);
    if (p) {
      Tcl_SetResult(interp, p, TCL_DYNAMIC);
    } else {
      Tcl_AppendResult(interp, "error getting tempfile name", (char *) NULL);
      return TCL_ERROR;
    }
  } else if ((c == 'u') && (strncmp(argv[1], "unlock", length) == 0) &&
	     length > 1) {
    if (argc != 3) {
      Tcl_AppendResult(interp, wrongNumArgs, argv[0],
	  " unlock fileName\"", (char *) NULL);
      return TCL_ERROR;
    }
    sprintf(interp->result, "%d", Mfv_UnlockFile(argv[2]));

  } else if ((c == 'u') && (strncmp(argv[1], "user", length) == 0) &&
	     length > 1) {
    if (argc != 2) {
      argv[1] = "user";
      goto not2args;
    }
    Tcl_SetResult(interp, username, TCL_STATIC);
  } else if ((c == 'v') && (strncmp(argv[1], "version", length) == 0)) {
    if (argc != 2) {
      argv[1] = "version";
      goto not2args;
    }
    Tcl_SetResult(interp, version, TCL_STATIC);
  } else {
    Tcl_AppendResult(interp, "bad option \"", argv[1],
	"\": should be boundary, encode, folderid, fullpath, home, list, mesgid, ",
	"simplify, tmpfile, user or version", (char *) NULL);
    return TCL_ERROR;
  }

  return TCL_OK;
}

static void Mfv_GetEndian()
{
    union {
        long    l;
        char    c[sizeof(long)];
    } u;

    u.l = 1;
    ENDIAN = u.c[0] ? -1 : 1;
    if (debug) fprintf (stderr, "this machine is %s endian\n",
	ENDIAN > 0 ? "big" : "little");
}

int Mfv_Init(Tcl_Interp *interp)
{
  int i;
  char *p;
  struct passwd *pw;
#ifdef HAVE_GETHOSTNAME
  char hostname[MAXHOSTNAMELEN];
#endif
  struct utsname unamebuf;

  for (i=0; mailSpool == NULL && strlen(spoolDirs[i]); i++)
    if (access(spoolDirs[i], F_OK) == 0)
      mailSpool = Mfv_Fullpath(spoolDirs[i], TRUE);
  if (mailSpool == NULL) {
    mailSpool = dbgstrdup(spoolDirs[0]);
    fprintf(stderr, "WARNING: cannot find mail spool! Using %s anyway.\n",
	mailSpool);
    /* Tcl_AppendResult(interp,
	"Cannot determine system mail spool", (char *)NULL);
    return TCL_ERROR; */
  }

#ifndef HAVE_TCLX
  if (Tcl_InitSignalHandling (interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
#endif

  /* if (Mfv_CompileRegExps(interp) != TCL_OK) return TCL_ERROR; */

  Mfv_GetEndian();
  Mfv_GetTZInfo();

  Tcl_CreateCommand(interp, "mfv_open", Mfv_OpenCmd, (ClientData) NULL,
      (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateCommand(interp, "mfv_set", Mfv_SetCmd, (ClientData) NULL,
      (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateCommand(interp, "mfv_util", Mfv_UtilCmd, (ClientData) NULL,
      (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateCommand(interp, "mfv_close", Mfv_CloseCmd, (ClientData) NULL,
      (Tcl_CmdDeleteProc *)NULL);

  Tcl_InitHashTable(&folderTable, TCL_STRING_KEYS);
  Tcl_InitHashTable(&sumTable, TCL_ONE_WORD_KEYS);

  Mfv_MakeFmtLine(interp, "%1S%3n %-15.15F  %3m %2d %-5.5h %4l  %-20.20s");

  keyPrefix = dbgstrdup(defaultPrefix);

#ifdef DOTLOCK
  dotlockprog = dbgstrdup(DOTLOCK);
#else
  dotlockprog = dbgstrdup("dotlock");
#endif

#ifdef TMPDIRPATH
  tmpDir = dbgstrdup(TMPDIRPATH);
#else
  tmpDir = dbgstrdup("/usr/tmp");
#endif

#ifdef HAVE_GETHOSTNAME
  if (gethostname(hostname, MAXHOSTNAMELEN) != -1 && strlen(hostname)) {
#ifdef HAVE_GETHOSTBYNAME
    struct hostent *he;
    he = gethostbyname(hostname);
    if (he != NULL) {
      localhost = dbgstrdup(he->h_name);
    }
    else
#endif
      localhost = dbgstrdup(hostname);
  }
  else
#endif
    if (uname(&unamebuf) != -1) {
      localhost = dbgstrdup(unamebuf.nodename);
    }
    else {
      if ((p = getenv("HOST")) != NULL) {
	localhost = dbgstrdup(p);
      }
      else
	localhost = dbgstrdup("unknown");
    }

  pw = getpwuid(getuid());
  username = dbgstrdup(pw->pw_name);
  homedir = dbgstrdup(pw->pw_dir);

  /* initialize scratch area for file names */
  tmpFileSize = 100;
  tmpFile = dbgalloc(tmpFileSize*sizeof(char));
  *tmpFile = 0;
  
  return TCL_OK;
}
