/********************************************************************\
**                             _________________________________    **
**   A n t h o n y            |________    __    __    _________|   **
**                                     |  |o_|  |o_|  |             **
**           T h y s s e n           __|   __    __   |__           **
**                                __|   __|  |  |  |__   |__        **
**  `` Dragon Computing! ''    __|   __|     |  |     |__   |__     **
**                            |_____|        |__|        |_____|    **
**                                                                  **
\********************************************************************/
/* 
** images.c
**
**  This module controls the data structure of `items' used to hold all the
** directory elements. The icon images represented by these elements are read
** in and assigned to widgets to be displayed.
** 
** The goal here is to create a data structure which can merge in new icons
** or changes to the currently displayed icons with the minimum of reads and
** delays. The rescan in particular could and should be improved.
** 
**                         Anthony Thyssen <anthony@dragon.cit.gu.edu.au>
*/
#include "xbmbrowser.h"

static Item   *file_list = NULL;     /* current list of all files */
static Item   *new_file_list = NULL; /* new file list just scanned (to merge) */
static Item   *old_file_list = NULL; /* old file list already loaded (merge) */
static Item  **last_link = NULL;     /* point to the last next ptr in merge */

static Widget *widget_array = NULL;  /* array of widgets available to use */
static int     allocated = 0;        /* number of widgets in above array */
static int     managed = 0;          /* number of widgets being managed */

static int items_allocated = 0;      /* current number of allocated items */
static int num_files;                /* number files in file_list */
static int num_visible;              /* number items which are visible */
static int file_counts[(int)NumFileTypes]; /* count of the various file types */

#define DIR_LIMIT  256
static char *dir_list[DIR_LIMIT];    /* directories in directory menu */
static int   num_dirs;               /* number of directories in this list */


#ifdef DO_XPMS

#ifndef NO_TRANS_COLOR
static XpmColorSymbol attr_colorsymbols[] = {
    {NULL, "none",  0},  /* transparent pixel to assign filled in later */
 };
#endif

/* do we have to invert pixmap on monocrome displays so label gets it right */
static GC invert_pixmaps_gc = None;

#endif

/* ------------------------------------------------------------------------ */
/* --------------------- Item Allocation / Dellocation -------------------- */

Item *
alloc_item()
/* allocate and default a file item */
{
  Item *item = (Item *) XtMalloc( sizeof(Item) );

  item->next     = NULL;       /* set defaults */
  item->fname[0] = '\0';
  item->info[0]  = '\0';   
  item->type     = Unknown;        /* type is unknown */
  item->visible  = False;          /* not visible - yet */
  item->index    = -1;             /* no widget assigned */
  item->pixmap   = (Pixmap)None;   /* no image loaded */
  item->mask     = (Pixmap)None;   /* no mask either (of course) */

  items_allocated++;
  return item;
}


static void
free_image(item)
/* Free any image that may be currently accociated with this item.
** This is usally done either as a prelude to freeing the items
** memory, or hiding the item from the user (why bother?).
*/
  Item *item;
{
  /* remove images from widget if one is allocated */
  if( item->index != -1 ) {
    XtVaSetValues(widget_array[item->index],
                  XtNpixmap, (XtArgVal)None,
                  XtNmask,   (XtArgVal)None,
                  XtNlabel,  (XtArgVal)NULL,  NULL);
  }

  /* Free Pixmap (Bitmap) of the image */
  if( item->pixmap != (Pixmap)None &&
      item->pixmap != sym_bmaps[item->type] ) {
    XFreePixmap(display, item->pixmap );
#ifdef DO_XPMS
    if( item->type == Xpm ) {
      /* free colors and attributes that were used by this pixmap only */
      XFreeColors(display, colormap, item->attr.pixels, item->attr.npixels, 0);
      XpmFreeAttributes((XpmAttributes *)&item->attr);
    }
#endif
  }

  /* Free Bitmap Mask of the image */
  if( item->mask != (Pixmap)None &&
      item->mask != sym_masks[item->type]) {
    XFreePixmap(display, item->mask );
  }
  item->pixmap = (Pixmap)None;
  item->mask = (Pixmap)None;
}


Item *
free_item(item)
/* Free the item and any aspect about that item. This is the ONLY place
** where items are to be freed.  Returns the next item in the current list!
*/
  Item *item;
{
  Item *next = item->next;

  free_image(item);
  /* item->index = -1; */

  XtFree( (char *) item );

  items_allocated--;
  return next;
}


void
free_list(list)
/* completely free all items in the given item list */
  Item **list;   /* pointer to a list pointer */
{
  while( *list != NULL )
    *list = free_item( *list );
}

/* ------------------------------------------------------------------------ */
/* --------------------------- Widget Control ----------------------------- */

static void
allocate_widgets( n )
/* create enough widgets to display n icon images */
  int n;
{
  char name[8];
  Widget w;

  if( allocated == n ) return;   /* just right -- no need to do anthing */

  if( allocated < n ) {   /* ok allocate more widgets */

    if(widget_array == NULL) {
      widget_array = (Widget *)XtMalloc( n * sizeof(Widget) );
      if( widget_array == NULL ) {
        perror("xbmbrowser: XtMalloc widget_array");
        abort();
      }
    } else {
      widget_array = (Widget *)realloc(widget_array, n * sizeof(Widget) );
      if( widget_array == NULL ) {
        perror("xbmbrowser: realloc larger widget_array");
        abort();
      }
    }

    for( ; allocated < n; allocated++ ) {
      sprintf(name, "%d", allocated);
      w = XtVaCreateWidget(name, iconLabelWidgetClass, iconbox, NULL);
      XtOverrideTranslations(w, XtParseTranslationTable(icon_trans));
      widget_array[allocated] = w;
    }
  }

#if 0
/* This code will eventually cause a core dump when lots of directory
** changes are performed by the user. The problem seems to occur when
** widgets are really destroyed (they are just flaged for destruction).
** I can't seem to find any problem with the code below however.
** It just doesn't seem to work.       Anthony
*/
  else {  /* ok we have too many widgets -- remove about a 1/3 of them */
    n = ( allocated + allocated + n ) / 3; /* the final level we want */
    
    for( allocated--; allocated >= n ; allocated-- )
      XtDestroyWidget( widget_array[allocated] );
    allocated++;

    widget_array = (Widget *)realloc(widget_array, n * sizeof(Widget) );
    if( widget_array == NULL ) {
      perror("xbmbrowser: realloc smaller widget_array");
      abort();
    }
  }

  /* insure that allocated is now equal to n (which may be calculated above */
  assert( allocated == n );
#endif
}


static void
assign_widget(item)
/* Set up the widget to display this item (index already assigned)
** and insure it is using the appropriate resource settings for
** correct display occording to the current user options.
*/
  Item *item;
{
  /* do we label the widget? */
  Boolean label = app_data.label_all || (
         item->type == Xbm ? app_data.label_icons :
         item->type == Xpm ? app_data.label_icons :
         ( app_data.label_syms || IsDirItem(item) && app_data.label_dirs ) );

  XtVaSetValues( widget_array[item->index],
       XtNinfoPtr,  (XtArgVal)item,                  /* pointer to more info */
       XtNpixmap,   (XtArgVal)item->pixmap,          /* the icon image */
       XtNmask,     (XtArgVal)item->mask,            /* its mask if present */
       XtNisBitmap, (XtArgVal)(item->type != Xpm),   /* draw as a bitmap? */

       XtNlabel,    (XtArgVal)(                      /* display the filename? */
	    label ? item->fname : NULL ),
       XtNshape,    (XtArgVal)(                      /* shape window on? */
	    item->type == Xbm ? app_data.solid_bgnd :
	    item->type == Xpm ? app_data.solid_bgnd :
				app_data.shape_syms ),
       XtNforeground, (XtArgVal)(                    /* bitmap/label color */
	    item->type == Xbm ? app_data.icon_fg :
	    item->type == Xpm ? app_data.icon_fg :
				app_data.sym_fg  ),
       XtNbackground, (XtArgVal)(                    /* background color */
	    item->type == Xbm ? app_data.icon_bg :
	    item->type == Xpm ? app_data.icon_tr :
				app_data.sym_bg  ),
       NULL); 
}


static void
assign_widgets()
/* Assign all items to the widgets as required.
** This allows us to adjust the current display after user option
** changes without doing a full rescan of the current directory.
** This should only be done when either 
**    1/ widgets are unmanaged during new directory scan/merge
**    2/ none of the items have actually changed only the
**       assignments to the widgets themselves have changed.
*/
{
  Item *item;

  managed = 0;   /* re-calculating this now */

  for( item = file_list;  item != NULL;  item = item->next ) {
    if( item->visible ) {
      assert( item->pixmap != (Pixmap)None ); /* a pixmap must be present */
      item->index = managed++;
      assign_widget(item);  /* set up this widget to hold the item */
    }
  }

}

/* ------------------------------------------------------------------------ */
/* --------------------------- Item Image Loading ------------------------- */

static void
load_image(item)
/* Attempt to determine and set the type of the file given and
** load the image (if present) which is stored in that file.
*/
  Item *item;
{ 
  int i, status;        /* junk integer, size of icon, return status */
  int c;                /* junk char -- integer for EOF test */
  unsigned int x, y;    /* size of bitmap/pixmap */
  FILE *fp;             /* file pointer */

  /* insure the bitmap is not allocated already */
  assert( item->pixmap == (Pixmap)None ||
          item->pixmap == sym_bmaps[item->type]);

  /* if the type is a X pixmap -- skip direct to load pixmap */
  if( item->type == Xpm || item->type == XpmBad )
    goto load_xpm;

  /* Test if the file is not a binary file
   * This should not be necessary, however the XReadBitmapFile and
   * XpmReadPixmapFile functions have a nasty habit of crashing with
   * some binary files.  --  Ashley
   */
   /* open the file and look to see if the first few chars are binary */
   if((fp = fopen(item->fname,"r")) == (FILE *) NULL) {
     /* can't open file -- declare it as such */
     item->type = Unknown;
     item->pixmap = (Pixmap)None;
     (void) sprintf(item->info, "\"%s\", unreadable", item->fname );
     return;
   } else {
     for(i = 0; i < 100; i++) {  /* check 100 chars */
       c = getc(fp);
       if(c == EOF)
	 break;
       if( c > 127 || c < 7 ) {  /* a char with non ascii values */
	 fclose(fp);
	 item->type = Binary;
         item->pixmap = (Pixmap)None;
         /* (void) sprintf(item->info, "\"%s\", binary (0x%02x at %d)", 
         **            item->fname, c, i ); /**/
	 return;
       }
     }
     fclose(fp);
     if( i == 0 ) {  /* hey empty file -- declare it as such */
       item->type = File;   /* leave it as a plain file */
       item->pixmap = (Pixmap)None;
       (void) sprintf(item->info, "\"%s\", empty file", item->fname );
       return;
     }
       
   } /* probably a non binary file so continue */

  /* presumably a text file -- so try an load it as a bitmap */
  item->pixmap = None; /* Precaution */
  status = XReadBitmapFile(display, DefaultRootWindow(display),
                            item->fname, &x, &y, &item->pixmap, &i, &i);

  if( status == BitmapSuccess && item->pixmap != None ) {
    item->type = Xbm;
    (void) sprintf(item->info, "\"%s\", %dx%d bitmap", item->fname, x, y );
    return;
  }

load_xpm:
  /* Not a bitmap -- Ok try it as a Pixmap file */
#ifdef DO_XPMS
  item->pixmap = (Pixmap)None;   /* Precaution */
  item->mask = (Pixmap)None;

   /* Set the attributes */
#ifdef NO_TRANS_COLOR
  item->attr.valuemask = XpmExactColors | XpmCloseness | XpmReturnPixels;
#else
  item->attr.valuemask =
          /* IN */    XpmColorSymbols | XpmExactColors | XpmCloseness 
          /* OUT */ | XpmReturnPixels;
  item->attr.colorsymbols = attr_colorsymbols;  /* setup in merge_init() */
  item->attr.numsymbols = XtNumber(attr_colorsymbols);
#endif /* NO_TRANS_COLOR */
  item->attr.exactColors = 0;
  item->attr.closeness = 40000;

  status = XpmReadPixmapFile(display, DefaultRootWindow(display),
                    item->fname, &item->pixmap, &item->mask, &item->attr);


  /* save a copy of the number of pixels (the kludge below may change this) */
  c = item->attr.npixels;

#ifndef NO_TRANS_COLOR
/*
** WARNING: The Transparent color given to the Xpixmap library for the `None'
** Color is returned in the  item->attr.pixels array.  This means I now have
** to remove this color from the array so that it is not free'd later when we
** are finished with the pixmap. This is only a problem when we use Color
** Symbols to set the transparent color.  For more information see the
** Changes file.
** 
** Note before release 3.4d if a None color was in the pixmap but was NOT the
** first element in the colortable, the first element in the pixels array was
** missing, and of course with the Arnaud's `philosophy' the None color pixel
** is present in the appropriate place. As such the first color is not free'd
** and can remain allocated for the life of the programs execution.
** IE: A Color Allocation Leak existed before version 3.4d!
**
** NOTE: this also has to be done on Monocrome Displays, as well as color
** displays.
*/
  if ( item->mask != None ) { /* None color present in xpimap? */
    int i,j;

    for( i = j = 0;  i < item->attr.npixels;  i++ ) {
      if( item->attr.pixels[i] == app_data.icon_tr )
        continue;  /* don't include the none pixel color */
      item->attr.pixels[j++] = item->attr.pixels[i];
    }
    item->attr.npixels = j;  /* the number of pixels now */

#if XpmFormat == 3 && XpmVersion == 4 && XpmRevision <= 6
#else
    /* Assuming that the None color was only in the X pixmap file once only
    ** (rarely otherwise) reduce the number of pixels returned by one.  
    ** NOTE:  a bug in the Xpm library before version 3.4d resulted in the
    ** number of pixels returned being correct but results in a color
    ** allocation leak, due to the first pixel value being missing.  */
    c--;
#endif
  } /* if mask present */
#endif /* NO_TRANS_COLOR */

  /* Check the return status of the Read Pixmap */
  switch( status ) {
    case XpmColorError:
    case XpmSuccess:
      item->type = Xpm;
      if ( item->attr.width == 0 || item->attr.height == 0 ) {
        /* this pixmap has NO PIXELS, and is a color table only!
        ** remove the pixmap and let the default symbol be used
        ** Suggestion by Thomas Cooke    <cooke@newice.stortek.com> */
        free_image(item);
      }
      (void) sprintf(item->info, "\"%s\", %dx%d pixmap (%d colors%s)",
               item->fname, item->attr.width, item->attr.height, c,
               item->mask != None ? " + mask" : "" );

      if ( item->pixmap != None ) return; /* Return if success */
      /* fall through if we failed after all! */

    case XpmColorFailed:
    case XpmNoMemory:
      item->type = XpmBad;
      /* if a pixmap was assigned -- remove it */
      /* this actually should never be needed but just in case */
      free_image(item);   
      return;

    case XpmOpenFailed:
    case XpmFileInvalid:
      /* OK so it isn't a pixmap */
      break;
   }
#endif /* DO_XPMS */

  /* Non Bitmap/Pixmap file -- it must be a plain text file */
  item->type = Text;
  item->pixmap = None;
  return;
}


static void
load_item(item)
/* Given an item, fill out all the other information required
** by that item, including the images needed.
*/
  Item *item;
{
  item->visible = True;
  if ( !app_data.show_hidden        /* don't show hidden 'dot' files */
       && item->fname[0] == '.'     /* and is a hidden file? */
       && item->type != DirUp ) {   /* and is not the up directory */
    item->visible = False;   
  }

  /* Try to load a image from this file and determine file type
  ** more closly. This may also set the info line for the file if a load
  ** was successful.  BadXpm's are NOT attempted again otherwise rescan's
  ** become very very slow on large directories of X pixmaps.
  */
  if( item->type == File && item->visible )
    load_image( item );

  /* Determine item visiblity as appropiate for current options */
  switch( item->type ) {
  case Dir:
  case DirUp:
  case DirLink:
  case DirBad: /* Directory Items */
    item->visible = item->visible &&
                    !app_data.icons_only && app_data.show_dir;
    break;
  case Unknown: /* Special File -- pipe, device */
  case Text:    /* Plain Text file */
  case Binary:  /* Binary file */
  case File:    /* New or Empty file */
    item->visible = item->visible &&
                    !app_data.icons_only && app_data.show_other;
    break;
  case XpmBad:  /* x pixmap which failed this load attempt */
    item->visible = item->visible &&
                    !app_data.icons_only && app_data.show_xpmbad;
    break;
  case Xbm:     /* X bitmap file */
  case Xpm:     /* X pixmap file */
    item->visible = True;  /* pixmaps always visible */
    break;
  default:      /* Unknown file type -- error should not happen */
    item->visible = False;  /* just not visible */
    assert( False ); /* assertion failure */
  }

  /* if it is not visible -- finished */
  if( !item->visible ) {
    free_image(item);  /* ???? insure no images is set */
    return;
  }

  /* At this point the item must be visible (widget assigned latter) */

  /* assign default bitmap is none present */
  if( item->pixmap == (Pixmap)None ) {
    item->pixmap = sym_bmaps[item->type];
    item->mask   = sym_masks[item->type];
  }

  /* assign a default info line if none present */
  if( item->info[0] != '"' ) {
    char *desc = "Description Error"; /* default description -- just in case */

    switch ( item->type ) {
      case Unknown: desc = "unknown";                    break;
      case Dir:     desc = "directory";                  break;
      case DirUp:   desc = "parent directory";           break;
      case DirLink: desc = "directory sym-link";         break;
      case DirBad:  desc = "directory (no access)";      break;
      case File:    desc = "file (unknown)";             break;
      case Text:    desc = "text";                       break;
      case Binary:  desc = "binary";                     break;
      case XpmBad:  desc = "pixmap (unable to display)"; break;
    }
    (void) sprintf(item->info, "\"%s\", %s", item->fname, desc);
  }

}

/*------------------------------------------------------------------------*/
/* -------------------- List Merging and Updating ----------------------- */

static void
merge_init()
/* initialize the appropiate variables for to merge `new_file_list'
** into the current `file_list'.
*/
{
  int i;  /* looping variable */

  /* first un-manage all the widgets and free the current file_list
  ** This saves on multiple geometry requests while we do the work
  */
  if( managed > 0 ) {
    XtUnmanageChildren(widget_array, managed);
    managed = 0;
  }

  old_file_list = file_list;  /* make the current list old */
  file_list = NULL;           /* empty destination of merged file_lists */
  last_link = &file_list;     /* last next ptr is the list ptr itself */

  /* zero all the counts required */
  num_files   = 0;            /* reset number of files in current list */
  num_dirs    = 0;            /* number of directoires in new dirmenu list */
  num_visible = 0;            /* number of visible items requiring widgets */
  for( i = 0 ; i < NumFileTypes ; i++ )
     file_counts[i] = 0;

  /* initialize the directory list */
  /* FUTURE: initialise this from user preferences? */
  dir_list[num_dirs++] = "/";
  dir_list[num_dirs++] = "~/";

#ifdef DO_XPMS
#ifndef NO_TRANS_COLOR
  /* set the transparent pixel color for X Pixmaps */
  attr_colorsymbols[0].pixel = app_data.icon_tr;
#endif /* ! NO_TRANS_COLOR */
#endif /* DO_XPMS */

}


static void
merge_item(item)
/* Add this item to the current (merging) `file_list'.
** Assigning the appropiate information line, bitmap.
** And adjusting all file counts appropiately.
*/
  Item *item;
{
  /* add item to the file_list */
  *last_link = item;            /* append new item to merged list */
  last_link = &(item->next);    /* move last_link to new last next ptr */
  /* *last_ptr = NULL;          /* terminate current list (not needed?) */
  num_files++;                  /* increment number of file in list */

  /* if this item is a directory add it to the directory menu list */
  /* NOTE: What should be done if num_dirs found > DIR_LIMIT */
  if( IsDirItem(item) ) {
    if( num_dirs < DIR_LIMIT-1 ) 
      dir_list[num_dirs++] = item->fname;
  }

  /* Figure out the rest of the items requirments like 
  ** file type, load image, info line, if visible etc... */
  load_item(item);

  /* count up the items of each type */
  file_counts[item->type]++;

  /* This item is visible give it a widget */
  if ( item->visible )
    num_visible++;
}


static void
merge_finish()
/* Finialise the merger of the new and old file_lists, assign the
** widgets, and set the default information line.
*/
{
  Item      *item;          /* looping variable */
  int        i;             /* junk integer */

  /* insure the last next pointer in the newly merged list is NULL */
  *last_link = NULL;

  /* finialise the directory menu list and install */
  dir_list[num_dirs] = NULL;
  XawListChange(dirlist, dir_list, 0, 0, True);
  XtVaSetValues(dirlist, XtNdefaultColumns, (XtArgVal)(num_dirs/25+1), NULL);

  /* allocate/deallocate widgets as needed */
  allocate_widgets( num_visible );

  /* Assign all items to the widgets appropriately */
  assign_widgets();

  /* manage the widgets */
  if( managed > 0 )
    XtManageChildren(widget_array, managed);

  /* Set the default information label */
# define strend  label_info+strlen(label_info)
  label_info[0] = '\0';  /* empty the string */
  if( num_files <= 1 ) /* only the ".." directory? */
    strcpy(label_info, "Empty Directory" );
  if( (i = file_counts[Xbm]) > 0 )
    sprintf(strend, " %d Bitmaps ", i );
  if( (i = file_counts[Xpm]) > 0 )
    sprintf(strend, " %d Pixmaps ", i );
  if( (i = file_counts[XpmBad]) > 0 )
    sprintf(strend, " %d Unloadable ", i );
  if( (i = file_counts[File] + file_counts[Text] + file_counts[Binary]) > 0 )
    sprintf(strend, " %d Others ", i );
  if( (i = file_counts[Dir] + file_counts[DirLink] + file_counts[DirBad]) > 0 )
    sprintf(strend, " %d Dirs ", i );
  if( (i = file_counts[Unknown]) > 0 )
    sprintf(strend, " %d Unknown ",    file_counts[Unknown] );

  XtVaSetValues(label, XtNlabel, (XtArgVal)label_info, NULL);
# undef strend

  /* check the results of the merger */
  assert( managed == num_visible );         /* number of widgets */
  assert( new_file_list == NULL );          /* both lists now empty */
  assert( old_file_list == NULL );
  assert( items_allocated == num_files );   /* allocation test */

}


static void
merge_file_lists()
/* Merge the old_file_list and the new_file_list. */
{
  int cmp;     /* comparision of top two elements of the lists */

  merge_init();

  while( old_file_list != NULL && new_file_list != NULL ) {
    /* both lists have items in them */

    cmp = strcmp( old_file_list->fname, new_file_list->fname );
    
    if( cmp == 0 ) {  /* the file names are the same */
      if ( !IsFileItem(new_file_list) ||
           old_file_list->mtime != new_file_list->mtime ) {

        /* NOT A FILE  -- mtime is non-sense so just add the new item */
        /* MODIFIED    -- same name but it has been modified or replaced */
        old_file_list = free_item( old_file_list );  /* junk the old item */
        cmp = 1;    /* ok now pretend that this is a new item -- ADDITION */
        /* FALL THROUGH -- to ADDITION */

      } else {

        /* NO CHANGE -- same file in both lists without any modifications */
        new_file_list = free_item( new_file_list ); /* junk the new item */
        merge_item(old_file_list);         /* merge the old item */
	old_file_list = *last_link;        /* remove item from old list */
	continue;
      }
    }

    /* ADDITION -- a new file (or newly modified) to be merged */
    if( cmp > 0 ) {  /* name of new item is smaller than in old */
      merge_item(new_file_list);         /* merge the new item */
      new_file_list = *last_link;        /* remove item from new list */
      continue;
    }

    /* DELETION -- the old file has been deleted */
    if( cmp < 0 ) {  /* Hey old item name is smaller then new item name */
      old_file_list = free_item( old_file_list ); /* delete old item */
      continue;
    }

    /* this point should never be reached */
    /*NOTREACHED*/
    assert( FALSE );
  }

  /*                   --------------
  ** At this point either one or both of the merging file lists
  ** is empty. As such we can just finish of the merged list quickly 
  */
  assert( new_file_list == NULL || old_file_list == NULL );

  /* ADD any more new items in the new_file_list */

  while( new_file_list != NULL ) {
    merge_item(new_file_list);         /* merge the new item */
    new_file_list = *last_link;        /* remove item from new list */
  }
  new_file_list = NULL;    /* new_file_list is now empty */
     
  /* DELETE any old items left in the old_file_list */
  free_list(&old_file_list);

  /*                   --------------           */
  /* Merger of file lists complete -- finialize */
  merge_finish(); 
}


/*========================================================================*/
/* ------------------- Public Routines and Functions -------------------- */

void
rescan_item(item)
/* If the given item is a visible file, and is newer 
** then rescan it loading any new image it may have now
*/
  Item  *item;
{
  /* the files current modification time -- 0 if not a file or deleted */
  time_t  time;

  if ( !item->visible  )
    return;   /* not visible -- why were we called? */

  if ( IsFileItem(item) ) {
    if ( (time = check_file_time(item->fname)) == 0 )
      return;   /* it is deleted -- don't mess with things */

    if ( time > item->mtime ) { /* Update this items image */
      free_image(item);         /* remove images from widget and free them */
      load_image(item);         /* load the file image */
      load_item(item);          /* set up the rest of the stuff */
      assign_widget(item);      /* setup the widget again */
    }
  }
}


void
redisplay_images(unmap)
/* Just redisplay the widgets after a slight option change */
  Boolean unmap;  /* unmap the widget before reseting them? */
{
  if( unmap && managed > 0 )
    XtUnmanageChildren(widget_array, managed);

  assign_widgets();   /* Reset all the widget settings -- correctly */

  if( unmap && managed > 0 )
    XtManageChildren(widget_array, managed);
}


void
reassign_images()
/* don't re-read the directory -- just re-assign the list */
{
  new_file_list = file_list;  /* pretend that this is the new list */
  file_list = NULL;

  /* totally reassign all items to the widgets */
  merge_file_lists();         /* go through the list again */
}


void
rescan_images()
/* Do a fast rescan and merger of the images in the current directory.
** The goal is to avoid re-reading files which were never changed.
*/
{
  new_file_list = get_files();  /* get items in the current directory */
  merge_file_lists();           /* merge any changes into file_list */
}


void
scan_images()
/* scan and display all the icon images in the current directory.
** This is to be used when changing to new directories.
*/
{
  /* just free all the icon images and do a rescan which handles
  ** the reading of new images perfectly fine on its own. We need
  ** to unmanage them here, to avoid problems.
  */
  if( managed > 0 ) {
    XtUnmanageChildren(widget_array, managed);
    managed = 0;
  }

  free_list(&file_list);
  assert( items_allocated == 0 );  /* their should be no items left */

  rescan_images();
}

/*========================================================================*/
