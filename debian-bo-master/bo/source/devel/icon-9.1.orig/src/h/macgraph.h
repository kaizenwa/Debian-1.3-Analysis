/*
 * Constants
 */

#define kEmptyString    "\p"
#define kMoveToFront    (WindowPtr)-1L
#define kVisible        true
#define kInvisible      false
#define kHasGoAway      true
#define kNilRefCon      (long)nil
#define kPi             3.14159265358979323846264338327950288419716939937511
#define kSleep          400

#define kMenuBar        128

#define kAppleMenu      128
#define kAboutMItem     1

#define kFileMenu       129
#define kCompileMItem   1
#define kRunMItem       1
#define kQuitMItem      3

#define kOptionsMenu    130
#define k_cMItem        1
#define kRInMItem       1
#define kROutMItem      2
#define kArgsMItem      3
#define kEchoOut2Con    5

#define kCLArgsDialog   128
#define kArgStringField 3

#define kDisableButton  255
#define kEnableButton   0

#define kStringID       128
#define kNARGS          25

#define kMinDocSize     64
#define kMaxDocSize     500
#define kZeroTolerance  0x0000

#define DMAXCOLORS           256
#define FS_SOLID             1
#define FS_STIPPLE           2
#define FS_OPAQUESTIPPLE     4


/*
 * Macros
 */

#define STDLOCALS(wb) wcp wc=(wb)->context;\
                      wsp ws=(wb)->window;\
                      WindowPtr stdwin=ws->theWindow
   
#define ICONFILENAME(wb) ((wb)->window->iconimage)
#define ICONLABEL(wb) ((wb)->window->iconlabel)
#define WINDOWLABEL(wb) ((wb)->window->windowlabel)

#define ANGLE(ang) (ang)
#define EXTENT(ang) (ang)
#define RECX(recs) ((recs).x)
#define RECY(recs) ((recs).y)
#define RECWIDTH(recs) ((recs).width)
#define RECHEIGHT(recs) ((recs).height)

#define FULLARC 360<<6
#define ARCWIDTH(arc) ((arc).width)
#define ARCHEIGHT(arc) ((arc).height)

#define RED(x) ((x).red)
#define GREEN(x) ((x).green)
#define BLUE(x) ((x).blue)

#define ASCENT(wb) (wb->context->font->fInfo.ascent)
#define DESCENT(wb) (wb->context->font->fInfo.descent)
#define LEADING(wb) (wb->context->font->fInfo.leading)
#define FWIDTH(wb) (wb->context->font->fInfo.widMax)
#define FHEIGHT(wb) ( (ASCENT(wb))+(DESCENT(wb))+(LEADING(wb)) )
#define MAXDESCENDER(wb) (wb->context->font->fInfo.descent)

#define DISPLAYWIDTH(wb) (screenBits.bounds.right-screenBits.bounds.left)
#define DISPLAYHEIGHT(wb) (screenBits.bounds.bottom-screenBits.bounds.top)

#define LINEWIDTH(wb) (wb->context->contextPtr->pnSize.h)
#define TEXTWIDTH(wb,s,n) ( TextWidth(s,(int)(s),n) )

#define SCREENDEPTH(wb) 8

#define COPYCONTEXT(c) BackPat((c)->bkPat);\
                       MoveTo((c)->pnLoc.h,(c)->pnLoc.v);\
                       PenSize((c)->pnSize.h,(c)->pnSize.v);\
                       PenMode((c)->pnMode);\
                       PenPat((c)->pnPat);\
                       TextFont((c)->txFont);\
                       TextFace((c)->txFace);\
                       TextMode((c)->txMode);\
                       TextSize((c)->txSize);\
                       SpaceExtra((c)->spExtra)
                         
#define SETCONTEXTDEFAULT(c) memcpy(&((c)->bkPat),&white,sizeof(Pattern));\
                             memcpy(&((c)->fillPat),&black,sizeof(Pattern));\
                             (c)->pnLoc.h=0;(c)->pnLoc.v=0;\
                             (c)->pnSize.h=1;(c)->pnSize.v=1;\
                             (c)->pnMode=patCopy;\
                             memcpy(&((c)->pnPat),&black,sizeof(Pattern));\
                             (c)->txFont=0;\
                             (c)->txFace=normal;\
                             (c)->txMode=srcOr;\
                             (c)->txSize=0;\
                             (c)->spExtra=0;\
                             (c)->fgColor.red=0;\
                             (c)->fgColor.green=0;\
                             (c)->fgColor.blue=0;\
                             (c)->bgColor.red=65535;\
                             (c)->bgColor.green=65535;\
                             (c)->bgColor.blue=65535

#define PREPAREGWORLD(ws)  GetGWorld(&((ws)->origPort),&((ws)->origDev));\
                           SetGWorld((ws)->offScreenGWorld,nil);\
                           (ws)->offScreenPMHandle=GetGWorldPixMap((ws)->offScreenGWorld);\
                           (ws)->lockOK=LockPixels((ws)->offScreenPMHandle)
        
#define GWORLD2WINDOW(ws)  (ws)->sourceRect=(ws)->theWindow->portRect;\
                           (ws)->sourceRect.bottom=(ws)->theWindow->portRect.bottom;\
                           (ws)->sourceRect.right=(ws)->theWindow->portRect.right;\
                           (ws)->destRect=(ws)->theWindow->portRect;\
                           (ws)->destRect.bottom=(ws)->theWindow->portRect.bottom;\
                           (ws)->destRect.right=(ws)->theWindow->portRect.right;\
                           CopyBits(&(((GrafPtr)((ws)->offScreenGWorld))->portBits),\
                                    &(((GrafPtr)((ws)->theWindow))->portBits),\
                                    &((ws)->sourceRect),&((ws)->destRect),srcCopy,\
                                    nil);\
                           UnlockPixels((ws)->offScreenPMHandle);\
                           SetGWorld((ws)->origPort,(ws)->origDev)

#define EVQUEGET(ws,d) {\
                       int i;\
                       if (!c_get((struct b_list *)BlkLoc((ws)->listp),&d)) fatalerr(0,NULL);\
                       if (Qual(d)) {\
                          ws->eventQueue[(ws)->eQfront++] = *StrLoc(d);\
                          if ((ws)->eQfront >= EQUEUELEN) (ws)->eQfront = 0;\
                             (ws)->eQback = (ws)->eQfront;\
                          }\
                       }
                       
#define EVQUEEMPTY(ws) (BlkLoc((ws)->listp)->list.size == 0)

/*
 * the bitmasks for the modifier keys
 */
 
#define ControlMask          (1 << 16)
#define Mod1Mask           (2 << 16)
#define ShiftMask            (4 << 16)

/*
 * typedef & structs
 */
 
typedef struct
  {
  long x, y;
  long width, height;
  long angle1, angle2;
  } XArc;
  
typedef struct
  {
  long x,y;
  } XPoint;
  
typedef struct 
   {
   long x1, y1;
   long x2, y2;
   } XSegment;
   
typedef struct
   {
   long x, y;
   long width, height;
   } XRectangle;
