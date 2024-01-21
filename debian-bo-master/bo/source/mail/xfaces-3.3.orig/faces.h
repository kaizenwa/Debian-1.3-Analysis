/*                             -*- Mode: C++-C -*- 
 * 
 *		 Copyright 1994 Christopher B. Liebman
 *
 *     Permission to use, copy, modify, distribute, and sell this software
 *     and its documentation for any purpose is hereby granted without fee,
 *     provided that the above copyright notice appear in all copies and that
 *     both that copyright notice and this permission notice appear in
 *     supporting documentation, and that the name Christopher B. Liebman not
 *     be used in advertising or publicity pertaining to distribution of this
 *     software without specific, written prior permission.
 *
 *    THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 *    ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 *    LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 *    PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 *    B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 *    INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 *    PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 *    WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 *    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author          : Chris Liebman
 * Created On      : Tue Jan 11 14:11:30 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Mon Mar  7 16:55:46 1994
 * Update Count    : 164
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sun Feb 13 00:38:34 1994 #150 (Chris Liebman)
 *    Added mail annotations. Also added new command stuff.
 *
 * 2-Feb-1994		Chris Liebman	
 *    Last Modified: Tue Feb  1 14:38:13 1994 #120 (Chris Liebman)
 *    Added annotation support.
 *
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Mon Jan 31 22:35:28 1994 #116 (Chris Liebman)
 *    New search support and resources.
 *
 * 24-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 23 18:54:23 1994 #47 (Chris Liebman)
 *    Added lots of new resources!
 *
 * 18-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 18 16:57:29 1994 #31 (Chris Liebman)
 *    Handle the cases where index, rindex, bcopy, bzero are
 *    already defined as macros on SYSV.  Added new resource:
 *    closeness. Added new header structs.
 *
 * 14-Jan-1994		Chris Liebman	
 *    Last Modified: Fri Jan 14 10:22:29 1994 #5 (Chris Liebman)
 *    Added defines for index(), rindex(), bcopy() and bzero() for SYSV.
 *    Removed include of xpm.h, its now in face_image_xpm.c.
 *    Added new ignore message binding resource element to TheXFacesResources.
 *    Added the status header to the Face structure.
 * 
 * PURPOSE
 * 	Definitions structures and extern declarations for xfaces.
 *
 * $Id: faces.h,v 1.29 1994/03/12 20:05:52 liebman Exp $
*/

#ifndef FACES_H_
#define FACES_H_

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Label.h>
#include "Tiled.h"

#ifndef SYSV
#include <strings.h>
#else

/*
 * Handle these functions if macros are not already defined.
*/

#ifndef index
#define index(a, b)     strchr(a, b)
#endif
#ifndef rindex
#define rindex(a, b)    strrchr(a, b)
#endif
#ifndef bcopy
#define bcopy(a, b, c)  memcpy(b, a, c)
#endif
#ifndef bzero
#define bzero(a, b)     memset(a, 0, b)
#endif

#endif /* SYSV */

#include "regexp.h"
#include "patchlevel.h"

/*
 *    Compare a string.
*/

#define	StringEqVal(s1, val)	(strncmp((s1), (val), strlen(val)) == 0)

/*
 *   Application class.
*/

#ifndef XFACES_CLASS
#define	XFACES_CLASS	"XFaces"
#endif

/*
 * Max size for mail seperator string.
*/

#define	MAX_MAILSEP_SIZE	31
#define	MAIL_SEP1		"From "
#define	MAIL_SEP1_SKIP		False
#define	MAIL_SEP2		"\001\001\001\001\n"
#define	MAIL_SEP2_SKIP		True

/*
 * The face type.
*/

typedef struct face		Face;
typedef struct face_item	FaceItem;

/*
 *Image and sound types.
*/

typedef struct face_image 	FaceImage;
typedef	struct face_image_type	FaceImageType;
typedef struct face_sound 	FaceSound;
typedef struct face_sound_type	FaceSoundType;
typedef struct face_command	FaceCommand;
typedef struct face_search_type FaceSearchType;
typedef struct face_search_data	FaceSearchData;

/*
 * Here is a mail item structures.
*/

typedef struct mail_header	MailHeader;
typedef struct mail_item	MailItem;

/*
 *    Here is the binding structure.
*/

typedef struct face_binding
{
    String		name;		/* Field to search. */
    String		patsrc;
    regexp		*pattern;	/* Pattern to search for. */
    String		file;		/* file for image/sound. */
    String		label;		/* Label for face. */
    int			anno;		/* Annotation number for label. */
    int			casesensitive;	/* Zero means case insensitive. */
    struct face_binding	*next;
} FaceBinding;

/*
 * The resource structure.
*/

typedef struct _faces_resources
{
    String		spool_dir;   /* The directory for default spool  */
				     /* files.                           */
    String		spool_file;  /* This is the file to monitor      */
				     /* (full path!).                    */
    String		list_command;
    String		pop_host;	/* pop mailbox host. */
    int			pop_port;
    String		pop_auth_file;	/* login/passwd  for pop server */
    					/* (full path!) */
    String		image_path;     /* Where to find the images.        */
    char*		image_path_str;
    char**		image_paths;
    String		sound_path;     /* Where to find the sounds.        */
    char*		sound_path_str;
    char**		sound_paths;
    String		facedb_path;	/* Where to find the facedb's.*/
    char*		facedb_path_str;
    char**		facedb_paths;
    char*		machine;	/* name for machine file. */
    char*		people;		/* name for people file. */
    int			update;      /* How often to check the mail.     */
    int			volume;      /* How loud to play sounds.         */
    String		from_field;  /* String to use as the field name
				      * for getting the from name.	 */
    String		no_mail_image; /* No mail picture. 		 */
    String		no_mail_sound; /* No mail sound.		 */
    Boolean		keep_order;
    Boolean		compress_images; /* Show faces compressed.	 */
    Boolean		use_sound;   	/* Play sounds.			 */
    Boolean		use_shape;   	/* Shape windows.		 */
    Boolean		use_commands;   /* Shape windows.		 */
    Boolean		use_content_length; /* Use Content-Length header (if
					     * available) to skip mail body. */
    Boolean		shape_borders;
    Boolean		shape_internal;
    int			closeness;   /* Allow similar colors.		 */
    String		image_types_str;
    FaceImageType**	image_types;
    String		image_search_str;
    FaceSearchData*	image_search;
    String		sound_search_str;
    FaceSearchData*	sound_search;
    String		command_search_str;
    FaceSearchData*	command_search;
    String		ignore_message_bindings_str;
    FaceBinding*	ignore_message_bindings;
    String		before_image_bindings_str;
    FaceBinding*	before_image_bindings;
    String		after_image_bindings_str;
    FaceBinding*	after_image_bindings;
    String		before_sound_bindings_str;
    FaceBinding*	before_sound_bindings;
    String		after_sound_bindings_str;
    FaceBinding*	after_sound_bindings;
    String		before_command_bindings_str;
    FaceBinding*	before_command_bindings;
    String		after_command_bindings_str;
    FaceBinding*	after_command_bindings;
    int			annotation_count;
    int			unknown_annotation_count;
    Boolean		annotation_above;
    Pixel		background;
    Boolean		shape_extra;
    Boolean		path_by_chdir;
    Boolean		lookup_hostname;
}   FacesResourcesRec, *FacesResources;

/*
 * Headers are on a double linked list.
*/

struct mail_header
{
    char*	name;	/* Header name. */
    char*	value;	/* Header value. */
    char*	line;	/* If we could not parse out name/value. */
    MailHeader*	next;	/* Next header. */
    MailHeader*	prev;	/* Previous header. */
};

struct mail_item
{
    String		user;		/* User from From line. */
    String		host;		/* Host from From line. */
    String		realhost;	/* Unaliased hostname. */
    String		label;		/* Item label. */
    int			use_label;	/* Use label for annotation. */
    int			unknown;	/* if non zero use unknown annos. */
    int			in_use;		/* Message in use. */
    MailHeader		*headers;	/* List of mail headers. */
    char*		body;		/* Mail body. */
    FaceImage*		image;		/* Image for user. */
    FaceSound*		sound;		/* Sound to play. */
    FaceCommand*	command;		/* Command to run. */
    Face*		face;
    char**		annotations;	/* Annotation data. */
    MailItem*		next;		/* Pointer to next item. */
    MailItem*		prev;		/* Pointer to prev item. */
};

struct face_item
{
    MailItem*	item;
    FaceItem*	next;
};


/*
 * Here is a face structure.
*/

struct face
{
    FaceImage	*image;		/* Image */
    char*	label;		/* Face label. */
    int		count;		/* Count of items using this face. */
    int		last_count;	/* Previous count. */
    FaceItem*	items;		/* List of mail items using face. */
    Widget	widget;		/* Widget used to display user. */
    Pixmap	pixmap;		/* Annotated pixmap. */
    Pixmap	shape;		/* Annotated shape pixmap. */
    char**	annotations;	/* Annotation data. */
    struct	face *next;	/* Pointer to next face. */
    struct	face *prev;	/* Pointer to prev face. */
};

typedef enum face_format
{
    FormatImage = 1,
    FormatAudio,
    FormatCommand
} FaceFormat;

#ifdef __STDC__
#define	P_(xxx)	xxx
#else
#define	P_(xxx)	()
#endif

/*
 * Global variable definitions.
*/

/* face_display.c */

extern Face     *TheFaceList;
extern Widget	NoMailWidget;

/* mail_box.c */
extern char*	MailSeperator;
extern int	MailSeperatorLength;
extern Boolean	MailSeperatorSkip;

/* mail_items.c */

extern MailItem*	TheMailItems;

/* main.c */
extern FacesResourcesRec TheFacesResources;
extern Widget TheTopLevel;
extern Widget TheFrame;
extern FaceImage	*NoMailImage;
extern FaceSound	*NoMailSound;


extern char *getlogin();


/*
 * External functions.
*/

/* cmd_check.c */

extern void CmdCheck P_((void));

/* face_actions.c */
extern void FaceActionsInit P_((void));

/* face_annotate.c */

extern int  FaceAnnotate P_((Face* face));
extern void FaceAnnotateFree P_((Face* face));
extern void FaceAnnotateInit P_((void));

/* face_binding.c */

extern FaceBinding* FaceBindingParse P_((String	str,
					 int has_value,
					 int has_label));
extern FaceBinding* FaceBindingCheck P_((MailHeader* headers,
					 FaceBinding*bindings));

/* face_command.c */

extern void FaceCommandFree P_((FaceCommand* fc));
extern int FaceCommandLoad P_((char* name, MailItem* item, FaceSearchData* data));
extern void FaceCommandRun P_((FaceCommand* fc));
extern void FaceCommandFind P_((MailItem* item));

/* face_display.c */
extern Face* FaceDisplay P_((MailItem* item));
extern void FaceClear P_((void));
extern void FaceClean P_((void));
extern int FaceCount P_((void));
extern void FaceDisplayInit P_((void));

/* face_image.c */
extern FaceImageType* FaceImageTypeByName P_((char* name));
extern void FaceImageTypeRegister P_((FaceImageType* type));
extern FaceImageType**  FaceImageTypeListParse P_((char* str));
extern int FaceImageLoad P_((char* file, MailItem* item, FaceSearchData* data));
extern FaceImage* FaceImageCreate P_((char*		file,
				      FaceImageType*	type,
				      void*		data));
extern void FaceImageLabelCreate P_((MailItem* item));
extern void FaceImageFree P_((FaceImage* fi));
extern Pixmap FaceImagePixmap P_((FaceImage* fi));
extern Pixmap FaceImageShape P_((FaceImage* fi));
extern String StringConcat P_((String	s1,
			       String	s2));
extern void FaceImageRef P_((FaceImage* fi));
extern void FaceImageCount P_((FaceImage* fi));
extern void FaceImageDecount P_((FaceImage* fi));
extern String FaceImageLabelGet P_((FaceImage* fi));

extern void FaceImageFind P_((MailItem* item));

/* face_image_xbm.c */

extern void FaceImageColorize P_((Pixmap* pixmap, int width, int height));
extern void* FaceImageXbmRead P_((String file, void* type_data));
#ifdef SHAPE
extern void* FaceImageShapedXbmRead P_((String file, void* type_data));
#endif
extern void FaceImageXbmFree P_((void* data, void* type_data));
extern Pixmap FaceImageXbmPixmap P_((void *data, void* type_data));
extern Pixmap FaceImageXbmShape P_((void *data, void* type_data));
extern void FaceImageXbmInit P_((void));

/* face_image_xface.c */
#ifdef XFACE
extern FaceImage* FaceImageXFaceCreate P_((String str));
extern void FaceImageXFaceFree P_((void* data, void* type_data));
extern Pixmap FaceImageXFacePixmap P_((void *data, void* type_data));
extern Pixmap FaceImageXFaceShape P_((void *data, void* type_data));
#endif

/* face_image_xpm.c */

extern void* FaceImageXpmRead P_((String file, void* type_data));
#ifdef SHAPE
extern void* FaceImageShapedXpmRead P_((String file, void* type_data));
#endif
extern void FaceImageXpmFree P_((void* data, void* type_data));
extern Pixmap FaceImageXpmPixmap P_((void *data, void* type_data));
extern Pixmap FaceImageXpmShape P_((void *data, void* type_data));
extern void FaceImageXpmInit P_((void));

/* face_search.c */

extern FaceSearchType*	FaceSearchTypeByName P_((char *name));
extern void FaceSearchTypeRegister P_((FaceSearchType* type));
extern int FaceSearch P_((MailItem* item, FaceSearchData* search_list));
extern int FaceSearchLoad P_((char *name, MailItem* item, FaceSearchData* data));
extern FaceSearchData* FaceSearchParse P_((char* str, FaceFormat format));

/* face_search_binding.c */

extern void FaceSearchBindingInit P_((void));

/* face_search_facedb.c */

extern void FaceSearchFacedbInit P_((void));

/* face_search_resource.c */

extern void FaceSearchResourceInit P_((void));

/* face_search_uh.c */

extern void FaceSearchUserHostInit P_((void));

/* face_search_xface.c */
#ifdef XFACE
extern void FaceSearchXFaceInit P_((void));
#endif

/* face_shape.c */

extern void FaceShapeCreate P_((void));

/* face_sound.c */

#ifdef SOUND
extern FaceSound* FaceSoundCreate P_((char* file));
extern void FaceSoundFree P_((FaceSound* fs));
extern int FaceSoundLoad P_((char* name, MailItem* item, FaceSearchData* data));
extern void FaceSoundPlay P_((FaceSound* fs));
extern void FaceSoundInit P_((void));
extern void FaceSoundFind P_((MailItem* item));
#endif

/* mail_body.c */

extern char* MailBodyRead P_((int content_length));
extern void MailBodySkip P_((int content_length));

/* mail_box.c */

extern void MailBoxParse P_((void));

/* mail_check.c */

extern void MailCheck P_((void));

/* mail_file.c */

extern int MailFileOpen P_((char* name));
extern void MailFileClose P_((void));
extern int MailFileReadChar P_((void));
extern void MailFileUnReadChar P_((int ch));
extern void MailFileUnReadString P_((char* str));
extern void MailFileClearUnRead P_((void));
extern void MailFilePeekString P_((char* buf, int len));
extern int MailFileReadString P_((char* buf, int len));
extern int MailFileSkipString P_((int len));
extern int MailFilePeekChar P_(());

/* mail_header.c */

extern char* MailHeaderLineRead P_((void));
extern MailHeader* MailHeaderRead P_((void));
extern MailHeader* MailHeaderListRead P_((void));
extern void MailHeaderFree P_((MailHeader* header));
extern void MailHeaderListFree P_((MailHeader* list));
extern MailHeader* MailHeaderFind P_((char* name, MailHeader* list));
extern int MailHeaderListCompare P_((MailHeader* list1, MailHeader* list2));

/* mail_items.c */

extern void MailBoxClear P_((void));
extern void MailBoxUnClear P_((void));
extern void MailBoxClean P_((void));
extern void MailItemCreate P_((MailHeader* headers));
extern void MailItemCreateNoHeaders P_((char*	user,
					char*	host,
					char**	annotations));
extern void MailBoxEmpty P_((void));
extern void MailItemInit P_((void));

/* mail_parse.c */

extern void MailParseAddress P_((String  from,
				 String* user,
				 String* host));

/* main.c */

extern void CheckMailNow P_((void));
extern int main P_((int argc, char** argv));
extern void regerror P_((String s));

/* path.c */

extern char** PathParse P_((char* path));
extern int PathEnumerate P_((char* file,
			     char** paths,
			     int (*func)(char* file, char* path, void* data),
			     void* data));

/* pop_check.c */

extern void PopCheck P_((void));

/* regexp.c */

extern regexp* regcomp P_((char* exp));
extern int regexec P_((regexp *prog, char *string));

/* regsub.c */

extern void regsub P_((regexp *prog, char *source, char *dest));

/* string.c */

extern char* SkipChars P_((char* str, char* delim));
extern char* ParseToken P_((char** str, char* delim));
extern char** StringParse P_((char* str, char* delims));

#endif /* FACES_H_ */
