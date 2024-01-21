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
 * Last Modified On: Sun Feb 13 17:03:59 1994
 * Update Count    : 139
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb 12 22:51:24 1994 #138 (Chris Liebman)
 *    Removed the labelBindings. (the general bindings can now specify the
 *    labels.
 *
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Sat Jan 29 23:44:35 1994 #133 (Chris Liebman)
 *    *Major* changes to support new searching configuration.
 *
 * 24-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 23 18:55:48 1994 #64 (Chris Liebman)
 *    Added path support and added X-Face header support.
 *
 * 14-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 11 14:19:02 1994 #1 (Chris Liebman)
 *    added #if to use XtDatabase() for versions of Xt less than 5
 * 
 * PURPOSE
 * 	Handle images in an image independent way.
*/

#ifndef lint
static char *RCSid = "$Id: face_image.c,v 1.12 1994/02/23 13:17:02 liebman Exp $";
#endif

#include "faces.h"
#include "face_image.h"
#include "face_search.h"
#include <sys/stat.h>

/*
 *  This is the master list of supported image types.
*/

static FaceImageType*	AllImageTypes = NULL;

/*
 *   Look up an image type by name.
*/

FaceImageType*
FaceImageTypeByName(name)
char* name;
{
    FaceImageType* type;
    
    for (type = AllImageTypes; type != NULL; type = type->next)
    {
	if (strcmp(name, type->name) == 0)
	{
	    return type;
	}
    }
    
    return NULL;
}

/*
 * Register a new image type.
*/

void
FaceImageTypeRegister(type)
FaceImageType* type;
{
    type->next = AllImageTypes;
    AllImageTypes = type;
}

/*
 * Parse a string of ':' seperated image type names into an array of
 * image type pointers.
*/

FaceImageType**
FaceImageTypeListParse(str)
char*		str;
{
    FaceImageType*	type;
    char*		name;
    int			type_count = 0;
    int			array_size = 0;
    FaceImageType**	array = NULL;
    
    if (str == NULL)
    {
	return NULL;
    }
    
    array_size = 10;
    array = (FaceImageType**)XtMalloc(array_size * sizeof(FaceImageType*));
    
    while((name = ParseToken(&str, ":")) != NULL)
    {
	type = FaceImageTypeByName(name);
	
	if (type == NULL)
	{
	    fprintf(stderr, "FaceImageTypeListParse: bad type name: <%s>\n", name);
	    continue;
	}
	
	if (type_count >= array_size)
	{
	    array_size += 10;
	    array = (FaceImageType**)XtRealloc((char*)array,
				       array_size * sizeof(FaceImageType*));
	}
	
	array[type_count++] = type;
    }
    
    array[type_count] = NULL;

    return array;
}

/*
 *   Here we store a complete list of images.
*/

static FaceImage	*TheImages = NULL;

/*
 *    Create face image data for face.
*/

FaceImage*
FaceImageCreate(file, type, data)
char*		file;
FaceImageType*	type;
void*		data;
{
    FaceImage*	fi;
    
    fi = (FaceImage *)XtCalloc(1, sizeof(FaceImage));
    fi->file  = XtNewString(file);
    fi->refs  = 1;
    fi->type  = type;
    fi->data  = data;
    
    /*
     *  Put the new image on the list.
    */
    
    fi->next = TheImages;
    fi->prev = NULL;
    
    if (fi->next != NULL)
    {
	fi->next->prev = fi;
    }
    
    TheImages = fi;
    
    return fi;
}

typedef struct face_image_load_info
{
    MailItem		*item;
    FaceImageType*	type;
} FaceImageLoadInfo;

static int
FaceImageLoadWork(file, path, info)
char*			file;
char*			path;
FaceImageLoadInfo*	info;
{
    struct stat		buf;
    void*		image_data = NULL;
    FaceImage*		fi;
    int			length;
    static char*	filename = NULL;
    static int		filename_length = 0;
    
    /*
     *    First see if we already have this image.
    */
    
    for (fi = TheImages; fi != NULL; fi = fi->next)
    {
	if (strcmp(fi->file, file) == 0)
	{
	    /*
	     * Yep!
	    */
	    
	    fi->refs += 1;
	    
	    info->item->image = fi;
	    return 1;
	}
    }

    /*
     * Only read if file exists and is not a directory.
    */
    
    if ((stat(file, &buf) != -1) && !(buf.st_mode & S_IFDIR))
    {
	/*
	 * Attempt to read the file.
	*/
	
	image_data = info->type->read(file, info->type->data);
    }
    
    
    /*
     *    Try the common extension if we failed.
    */
    
    if (image_data == NULL)
    {
	/*
	 *   Now try common extension.
	*/

	length = strlen(file) + strlen(info->type->extension) + 1;

	if (filename_length < length)
	{
	    filename_length = length;

	    if (filename)
	    {
		XtFree(filename);
	    }
	    
	    filename = XtMalloc(filename_length);
	}
	
	sprintf(filename, "%s%s", file, info->type->extension);
	
	/*
	 * Only read if file exists and is not a directory.
	*/
	
	if ((stat(filename, &buf) != -1) && !(buf.st_mode & S_IFDIR))
	{
	    /*
	     * Attempt to read the file.
	    */
	    
	    image_data = info->type->read(filename, info->type->data);
	}
    }
    
    /*
     *    If we still do not have it then fail.
    */
    
    if (image_data == NULL)
    {
	return 0;
    }
    
    /*
     *   Ok, create a face image struct.
    */
    
    fi = FaceImageCreate(file, info->type, image_data);
    
    info->item->image = fi;
    
    return 1;
}

int
FaceImageLoad(file, item, data)
char*		file;
MailItem*	item;
FaceSearchData*	data;
{
    int			type;
    FaceImageType**	types = TheFacesResources.image_types;
    int			found = 0;
    FaceImageLoadInfo	info;
    char**		paths = TheFacesResources.image_paths;
    
    if (data != NULL)
    {
	/*
	 * Use the image types in teh search data.
	*/
	
	if (data->itypes != NULL)
	{
	    types = data->itypes;
	}
	
	/*
	 * Fixup paths.
	*/
	
	if (data->paths != NULL)
	{
	    paths = data->paths;
	}
    }
    
    /*
     *   No image types?
    */
    
    if (types == NULL)
    {
	fprintf(stderr, "FaceImageLoad: no image types!\n");
	return 0;
    }
    
    info.item = item;
    
    for(type = 0; types[type] != NULL; ++type)
    {
	info.type = types[type];
	
	/*
	 *   enumerate thru paths if needed.
	*/
	
	if ((*file != '/') &&
	    strncmp(file, "./", 2) &&
	    strncmp(file, "../", 3))
	{
	    found = PathEnumerate(file, paths, FaceImageLoadWork, &info);
	}
	else
	{
	    found = FaceImageLoadWork(file, ".", &info);
	}
	
	if (found)
	{
	    break;
	}
    }
    
    return found;
}

/*
 *    Free an image.
*/

void
FaceImageFree(fi)
FaceImage	*fi;
{
    if (!fi)
    {
	return;
    }
    
    /*
     *   First remove one reference.  If there are still more refs just
     * return.
    */
    
    fi->refs -= 1;
    if (fi->refs != 0)
    {
	return;
    }
    
    /*
     * The previous image is now previous to the next image.
    */
    
    if (fi->next != NULL)
    {
	fi->next->prev = fi->prev;
    }
    
    /*
     * The next face is now next from the previous face.
    */
    
    if (fi->prev != NULL)
    {
	fi->prev->next = fi->next;
    }
    
    /*
     * If this was the first image then the next image is
     * first.
    */
    
    if (fi == TheImages)
    {
	TheImages = fi->next;
    }
    
    /*
     *    Ok, free the name.
    */
    
    XtFree(fi->file);
    
    /*
     *    Free any label.
    */
    
    XtFree(fi->label);
    
    /*
     *    Free the image data.
    */
    
    if (fi->type != NULL && fi->type->free != NULL)
    {
	fi->type->free(fi->data, fi->type->data);
    }
    
    /*
     *    Free the struct.
    */
    
    XtFree((void *)fi);
}

/*
 *    Retrieve the image pixmap.
*/

Pixmap
FaceImagePixmap(fi)
FaceImage	*fi;
{
    Pixmap	pixmap = None;

    if (fi != NULL && fi->type != NULL && fi->type->pixmap != NULL)
    {
	pixmap = fi->type->pixmap(fi->data, fi->type->data);
    }
    
    return(pixmap);
}

/*
 *    Retrieve the image shape mask.
*/

Pixmap
FaceImageShape(fi)
FaceImage	*fi;
{
    Pixmap	shape = None;
    
    if (fi != NULL && fi->type != NULL && fi->type->shape != NULL)
    {
	shape = fi->type->shape(fi->data, fi->type->data);
    }
    
    return(shape);
}


/*
 * Add a label to an image, if there is no image then create one.
*/

void
FaceImageLabelCreate(item)
MailItem*	item;
{
    FaceImage*	fi;
    char	*label;
    
    label = XtMalloc(strlen(item->user) + strlen(item->host) + 2);
    sprintf(label, "%s@%s", item->user, item->host);
    
    /*
     *   Ok, create a face image struct.
    */
    
    if (item->image == NULL)
    {
	/*
	 *    First see if we already have this image/label.
	*/
	
	for (fi = TheImages; fi != NULL; fi = fi->next)
	{
	    if (strcmp(fi->file, label) == 0)
	    {
		/*
		 * Yep!
		*/
		
		XtFree(label);
		fi->refs += 1;
		
		item->image = fi;
		return;
	    }
	}
	
	item->image = FaceImageCreate(label, NULL, NULL);
    }
    
    /* 
     * Add the label.
    */
    
    item->image->label = label;
}

String
StringConcat(s1, s2)
String	s1;
String	s2;
{
    String	s;
    
    s = XtMalloc(strlen(s1) + strlen(s2) + 1);
    sprintf(s, "%s%s", s1, s2);
    
    return(s);
}

void
FaceImageRef(fi)
FaceImage* fi;
{
    if (fi)
    {
	fi->refs += 1;
    }
}

void
FaceImageCount(fi)
FaceImage* fi;
{
    if (fi)
    {
	fi->list_count += 1;
    }
}

void
FaceImageDecount(fi)
FaceImage* fi;
{
    if (fi)
    {
	fi->list_count -= 1;
    }
}

String
FaceImageLabelGet(fi)
FaceImage* fi;
{
    if (!fi)
    {
	return "NoLabel";
    }
    
    return fi->label;
}

/*
 *  Find an image for the given mail item.
*/

void
FaceImageFind(item)
MailItem* item;
{
    FaceSearch(item, TheFacesResources.image_search);
    FaceImageLabelCreate(item);
}
