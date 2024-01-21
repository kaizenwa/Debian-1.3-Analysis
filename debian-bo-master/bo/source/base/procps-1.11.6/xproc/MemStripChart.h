#ifndef _XawMemStripChart_h
#define _XawMemStripChart_h

/***********************************************************************
 *
 * MemStripChart Widget
 * from StripChart Widget derived 
 *
 * Author:  Hans-Helmut B"uhmann 20. Jan. 1996
 *
 ***********************************************************************/

/* StripChart resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 accelerators	     Accelerators	AcceleratorTable NULL
 ancestorSensitive   AncestorSensitive	Boolean		True 
 background	     Background		Pixel		XtDefaultBackground
 backgroundPixmap    Pixmap		Pixmap		XtUnspecifiedPixmap
 borderColor	     BorderColor	Pixel		XtDefaultForeground
 borderPixmap	     Pixmap		Pixmap		XtUnspecifiedPixmap
 borderWidth	     BorderWidth	Dimension	1
 colormap	     Colormap		Colormap	parent's colormap
 cursor		     Cursor		Cursor		None
 cursorName	     Cursor		String		NULL
 depth		     Depth		int		parent's depth
 destroyCallback     Callback		XtCallbackList	NULL
 foreground	     Foreground		Pixel		XtDefaultForeground
 getValue	     Callback		XtCallbackList	NULL
 height		     Height		Dimension	120
 highlight	     Foreground		Pixel		XtDefaultForeground
 insensitiveBorder   Insensitive	Pixmap		GreyPixmap
 jumpScroll	     JumpScroll		int		1/2 width
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 minScale	     Scale		int		1
 pointerColor	     Foreground		Pixel		XtDefaultForeground
 pointerColorBackground Background	Pixel		XtDefaultBackground
 screen		     Screen		Screen		parent's screen
 sensitive	     Sensitive		Boolean		True
 translations	     Translations	TranslationTable NULL
 update		     Interval		int		10 (seconds)
 width		     Width		Dimension	120
 x		     Position		Position	0
 y		     Position		Position	0

 codecolor           Foreground		Pixel           "red"
 cachedcolor         Foreground         Pixel           "yellow"
 buffercolor         Foreground         Pixel           "blue"
 freecolor           Foreground         Pixel           "green"
 swapcolor           Foreground         Pixel           "red"

*/

#define DEFAULT_JUMP -1

#ifndef _XtStringDefs_h_
#define XtNhighlight "highlight"
#define XtNupdate "update"
#endif
#define XtNcodecolor "codecolor"
#define XtNcachedcolor "cachedcolor"
#define XtNbuffercolor "buffercolor"
#define XtNfreecolor "freecolor"
#define XtNswapcolor "swapcolor"

#define XtCJumpScroll "JumpScroll"
#define XtCScale "Scale"

#define XtNgetValue "getValue"
#define XtNjumpScroll "jumpScroll"
#define XtNminScale "minScale"
#define XtNscale "scale"
#define XtNvmunix "vmunix"

/* struct for callback */
/* code + shared + buffer + free == 1.0 */ 
typedef struct {
    double code;
    double cached;
    double buffer;
    double free;
    double swap;
} MemStripChartCallbackData;

typedef struct _MemStripChartRec *MemStripChartWidget;
typedef struct _MemStripChartClassRec *MemStripChartWidgetClass;

extern WidgetClass memStripChartWidgetClass;

#endif /* _XawMemStripChart_h */
/* DON'T ADD STUFF AFTER THIS #endif */
