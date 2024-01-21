/*                             -*- Mode: C++-C -*- 
 * 
 *  
 * 	     Copyright 1994 Christopher B. Liebman
 * 
 *  Permission to use, copy, modify, distribute, and sell this software
 *  and its documentation for any purpose is hereby granted without fee,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name Christopher B. Liebman not
 *  be used in advertising or publicity pertaining to distribution of this
 *  software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 * B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 * INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 * PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * 
 * 
 * Author          : Chris Liebman
 * Created On      : Sat Jan 29 15:17:46 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sun Mar  6 22:32:09 1994
 * Update Count    : 34
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb 12 23:08:32 1994 #31 (Chris Liebman)
 *    Use label from bindings it available.  Also added command before and
 *    after bindings.
 * 
 * PURPOSE
 * 	Binding search.
*/

#ifndef lint
static char *RCSid = "$Id: face_search_binding.c,v 1.3 1994/03/07 04:46:30 liebman Exp $";
#endif

#include "faces.h"
#include "face_search.h"

typedef enum face_binding_type { BeforeBinding, AfterBinding } FaceBindingType;

static int
FaceSearchBinding(item, data)
MailItem*	item;
FaceSearchData*	data;
{
    FaceBinding*	binding;
    int			ret = 0;
    
    binding = *(FaceBinding**)(data->search->data);
    
    /*
     *  Check mail headers agains bindings.
    */
    
    binding = FaceBindingCheck(item->headers, binding);
    
    /*
     * If no binding matched then fail.
    */
    
    if (binding == NULL)
    {
	return 0;
    }
    
    /*
     *   Now try to get an image/sound.
    */
    
    ret = FaceSearchLoad(binding->file, item, data);

    /*
     *  If we found something and it was an image and the binding specified
     * a label then set the item label. And force annotation to use the
     * label in the named position.
    */
    
    if (ret && (data->format == FormatImage) && (binding->label != NULL))
    {
	item->label = XtNewString(binding->label);
	item->use_label = binding->anno;
    }
    
    return ret;
}

static FaceSearchType beforeImage =
{
    "beforeImage",
    FaceSearchBinding,
    &TheFacesResources.before_image_bindings,
    NULL,
};

static FaceSearchType afterImage =
{
    "afterImage",
    FaceSearchBinding,
    &TheFacesResources.after_image_bindings,
    NULL,
};

static FaceSearchType beforeSound =
{
    "beforeSound",
    FaceSearchBinding,
    &TheFacesResources.before_sound_bindings,
    NULL,
};

static FaceSearchType afterSound =
{
    "afterSound",
    FaceSearchBinding,
    &TheFacesResources.after_sound_bindings,
    NULL,
};

static FaceSearchType beforeCommand =
{
    "beforeCommand",
    FaceSearchBinding,
    &TheFacesResources.before_command_bindings,
    NULL,
};

static FaceSearchType afterCommand =
{
    "afterCommand",
    FaceSearchBinding,
    &TheFacesResources.after_command_bindings,
    NULL,
};

void
FaceSearchBindingInit()
{
    FaceSearchTypeRegister(&beforeImage);
    FaceSearchTypeRegister(&afterImage);
    FaceSearchTypeRegister(&beforeSound);
    FaceSearchTypeRegister(&afterSound);
    FaceSearchTypeRegister(&beforeCommand);
    FaceSearchTypeRegister(&afterCommand);
}
