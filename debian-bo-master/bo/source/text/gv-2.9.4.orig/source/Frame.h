
#ifndef _XawFrame_h
#define _XawFrame_h

#define XtNshadowWidth "shadowWidth"
#define XtCShadowWidth "ShadowWidth"
#define XtNtopShadowPixel "topShadowPixel"
#define XtCTopShadowPixel "TopShadowPixel"
#define XtNbottomShadowPixel "bottomShadowPixel"
#define XtCBottomShadowPixel "BottomShadowPixel"
#define XtNuserData "userData"
#define XtCUserData "UserData"

typedef enum {
  XawRAISED,
  XawSUNKEN,
  XawCHISELED,
  XawLEDGED,
  XawFrameMassiveRaised
} XawFrameType;
 
#define XtNframeType "frameType"
#define XtCFrameType "FrameType"
#define XtRFrameType "FrameType"

#ifndef XtNhSpace             
#define XtNhSpace "hSpace"
#endif
#ifndef XtNvSpace             
#define XtNvSpace "vSpace"
#endif
#ifndef XtCHSpace             
#define XtCHSpace "HSpace"
#endif
#ifndef XtCVSpace             
#define XtCVSpace "VSpace"
#endif

/* Class record constants */

extern WidgetClass frameWidgetClass;

typedef struct _FrameClassRec *FrameWidgetClass;
typedef struct _FrameRec      *FrameWidget;

#endif /* _XawFrame_h */
