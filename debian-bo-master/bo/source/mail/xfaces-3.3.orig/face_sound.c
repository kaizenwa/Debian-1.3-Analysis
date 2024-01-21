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
 * Last Modified On: Sat Feb 12 23:11:34 1994
 * Update Count    : 40
 * Status          : Released
 * 
 * HISTORY
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Sat Jan 29 23:56:11 1994 #35 (Chris Liebman)
 *    Major changes for new search stuff.
 *
 * 24-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 23 16:03:49 1994 #19 (Chris Liebman)
 *    Added path support and moved all sound checking here.
 *
 * 14-Jan-1994		Chris Liebman	
 *    Last Modified: Wed Jan 12 11:58:51 1994 #4 (Chris Liebman)
 *    added #if to use XtDatabase() for versions of Xt less than 5
 *    
 * 
 * PURPOSE
 * 	Sound support using NetAudio.
*/

#ifndef lint
static char *RCSid = "$Id: face_sound.c,v 1.17 1994/03/07 20:27:46 liebman Exp $";
#endif

#include "faces.h"
#include <audio/audiolib.h>
#include <audio/Xtutil.h>
#include <audio/soundlib.h>
#include "face_sound.h"
#include "face_search.h"
#include <sys/stat.h>

static AuServer*	audio = NULL;

/*
 *  Here is the play list.
*/

typedef struct play_list
{
    FaceSound*	sound;
    struct play_list* next;
} PlayList;

static PlayList* PlayListHead;
static PlayList* PlayListTail;

/*
 *   Here we store a complete list of sounds.
*/

static FaceSound	*TheSounds = NULL;

/*
 *    Create face sound data for face.
*/

FaceSound *
FaceSoundCreate(file)
char	*file;
{
    struct stat buf;
    FaceSound	*fs;
    
    /*
     *    First see if we already have this sound.
    */
    
    for (fs = TheSounds; fs != NULL; fs = fs->next)
    {
	if (strcmp(fs->file, file) == 0)
	{
	    /*
	     * Yep!
	    */
	    
	    fs->refs += 1;
	    
	    return(fs);
	}
    }
    
    if (!audio)
    {
	return NULL;
    }
    
    /*
     * Fail if file is a directory or does not exist.
    */
    
    if ((stat(file, &buf) == -1) || (buf.st_mode & S_IFDIR))
    {
	return NULL;
    }
    
    /*
     *   Ok, create a face sound struct.
    */
    
    fs = (FaceSound *)XtCalloc(1, sizeof(FaceSound));
    fs->file  = XtNewString(file);
#ifdef USE_BUCKETS
    fs->bucket = AuSoundCreateBucketFromFile(audio, file, (AuAccessImportMask |
							   AuAccessExportMask |
							   AuAccessListMask),
					     NULL, NULL);
    
    if (fs->bucket == AuNone)
    {
	XtFree((char *)fs);
	return NULL;
    }
#else
    fs->sound = SoundOpenFileForReading(file);
    if (!fs->sound)
    {
	XtFree((void*)fs);
	return NULL;
    }
#endif /* USE_BUCKETS */
    
    fs->refs  = 1;
    
    /*
     *  Put the new sound on the list.
    */
    
    fs->next = TheSounds;
    fs->prev = NULL;
    
    if (fs->next != NULL)
    {
	fs->next->prev = fs;
    }
    
    TheSounds = fs;
    
    /*
     * and return it.
    */
    
    return(fs);
}

/*
 *    Free an sound.
*/

void
FaceSoundFree(fs)
FaceSound	*fs;
{
    if (!fs)
    {
	return;
    }
    
    /*
     *   First remove one reference.  If there are still more refs just
     * return.
    */
    
    fs->refs -= 1;
    if (fs->refs != 0)
    {
	return;
    }

    /*
     *   Destroy the attacked sound.
    */
    
    if (audio)
    {
#ifdef USE_BUCKETS
	AuDestroyBucket(audio, fs->bucket, NULL);
#else
	SoundCloseFile(fs->sound);
#endif /* USE_BUCKETS */
    }
    
    /*
     * The previous sound is now previous to the next sound.
    */
    
    if (fs->next != NULL)
    {
	fs->next->prev = fs->prev;
    }
    
    /*
     * The next face is now next from the previous face.
    */
    
    if (fs->prev != NULL)
    {
	fs->prev->next = fs->next;
    }
    
    /*
     * If this was the first sound then the next sound is
     * first.
    */
    
    if (fs == TheSounds)
    {
	TheSounds = fs->next;
    }
    
    /*
     *    Ok, free the name.
    */
    
    XtFree(fs->file);
    
    /*
     *    Free the struct.
    */
    
    XtFree((void *)fs);
}

static int
FaceSoundLoadWork(file, path, item)
char*		file;
char*		path;
MailItem*	item;
{
    FaceSound*	fs;
    char**	p;
    int		length;
    static char *filename = NULL;
    static int  filename_length;
    static char *extensions[] = 
    {
	".au", ".snd", ".voc", ".wav", ".wave", 0
    };
    
    /*
     * First try the file as given.
    */
    
    if ((fs = FaceSoundCreate(file)) != NULL)
    {
	item->sound = fs;
	return 1;
    }
    
    /*
     * Make a buffer to try various names in.
    */
    
    length = strlen(file) + 20;
    
    if (filename_length < length)
    {
	filename_length = length;
	
	if (filename)
	{
	    XtFree(filename);
	}
	
	filename = XtMalloc(filename_length);
    }
    
    for(p = extensions; *p; ++p)
    {
	sprintf(filename, "%s%s", file, *p);
	if ((fs = FaceSoundCreate(filename)) != NULL)
	{
	    item->sound = fs;
	    return 1;
	}
    }
    
    return 0;
}

int
FaceSoundLoad(file, item, data)
char*		file;
MailItem*	item;
FaceSearchData*	data;
{
    int			found = 0;
    char**		paths = TheFacesResources.sound_paths;
    
    if (data != NULL)
    {
	/*
	 * Fixup paths.
	*/
	
	if (data->paths != NULL)
	{
	    paths = data->paths;
	}
    }
    
    /*
     *   enumerate thru paths if needed.
     */
    
    if ((*file != '/') &&
	strncmp(file, "./", 2) &&
	strncmp(file, "../", 3))
    {
	found = PathEnumerate(file, paths, FaceSoundLoadWork, item);
    }
    else
    {
	found = FaceSoundLoadWork(file, ".", item);
    }
    
    return found;
}

/*
 * Shamlessly coppied from net audio and modified to play from bucket
 * instead of file.
*/

#define	VOL(volume)		((1 << 16) * (volume) / 100)

/* ARGSUSED */
static void
play_cb(aud, handler, ev, data)
AuServer       *aud;
AuEventHandlerRec *handler;
AuEvent        *ev;
AuPointer       data;
{
    AuStatus	ret;
    PlayList	*this;
    
    /*
     * Ok, one item finished, remove it from the list and start the next.
    */

    
    this = PlayListHead;
    PlayListHead = this->next;
    XtFree((char *)this);
    
    /*
     *   If there is another sound to play then start it, else that was the
     * last sound so NULL the tail pointer.
    */
    
    if (PlayListHead != NULL)
    {
#ifdef USE_BUCKETS
	AuSoundPlayFromBucket(audio, PlayListHead->sound->bucket, AuNone,
			    VOL(TheFacesResources.volume),
			    play_cb, NULL, 1,
			    NULL, NULL, NULL,
			    &ret);
#else
	AuSoundPlayFromFile(audio, PlayListHead->sound->file, AuNone,
			    VOL(TheFacesResources.volume),
			    play_cb, NULL,
			    NULL, NULL, NULL,
			    &ret);
#endif
    }
    else
    {
	PlayListTail = NULL;
    }
}

/*
 *  Play a sound.
*/

void
FaceSoundPlay(fs)
FaceSound	*fs;
{
    PlayList *this;
    AuStatus ret;

    /*
     *  No audio device!
    */
    
    if (!audio)
    {
	return;
    }

    /*
     *  No sound!
    */
    
    if (!fs)
    {
	return;
    }
    
    /*
     *  make a play list for this guy.
    */
    
    this = (PlayList*)XtMalloc(sizeof(PlayList));
    this->sound = fs;
    this->next = NULL;
    
    /*
     * First item on play list starts the playing.
    */
    
    if (!PlayListHead)
    {
	PlayListHead = PlayListTail = this;
#ifdef USE_BUCKETS
	AuSoundPlayFromBucket(audio, PlayListHead->sound->bucket, AuNone,
			    VOL(TheFacesResources.volume),
			    play_cb, NULL, 1,
			    NULL, NULL, NULL,
			    &ret);
#else
	AuSoundPlayFromFile(audio, PlayListHead->sound->file, AuNone,
			    VOL(TheFacesResources.volume),
			    play_cb, NULL,
			    NULL, NULL, NULL,
			    &ret);
#endif
	return;
    }
    
    /*
     *  Just add this item to the tail of the list.
    */
    
    PlayListTail->next = this;
    PlayListTail = this;
}

void
FaceSoundFind(item)
MailItem*	item;
{
    if (TheFacesResources.use_sound)
    {
	FaceSearch(item, TheFacesResources.sound_search);
    }
}


void
FaceSoundInit()
{
    static int init = 0;
    
    if (!init)
    {
	String	dpy_name = DisplayString(XtDisplay(TheTopLevel));
	audio = AuOpenServer(dpy_name, 0, NULL, 0, NULL, NULL);
	
	if (audio)
	{
	    AuXtAppAddAudioHandler(XtWidgetToApplicationContext(TheTopLevel),
				   audio);
	}
	
	init = 1;
    }
}
