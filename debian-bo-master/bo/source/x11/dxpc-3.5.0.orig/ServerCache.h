#ifndef ServerCache_H
#define ServerCache_H

#include "IntCache.h"
#include "CharCache.h"
#include "TextCompressor.h"
#include "BlockCache.h"
#include "BlockCacheSet.h"


class ServerCache {
 public:
  ServerCache();
  ~ServerCache();

  // General-purpose caches:
  CharCache textCache[8192];
  unsigned int lastSequenceNum;
  IntCache replySequenceNumCache;
  IntCache eventSequenceNumCache;
  unsigned int lastTimestamp;
  CharCache depthCache;
  IntCache visualCache;
  IntCache colormapCache;

  // Opcode prediction caches (predict next opcode based on previous one)
  CharCache opcodeCache[256];
  unsigned char lastOpcode;

  // X connection startup
  static BlockCache lastInitReply;

  // X errors
  CharCache errorCodeCache;
  IntCache errorMinorCache;
  CharCache errorMajorCache;

  // Colormap
  IntCache colormapNotifyWindowCache;
  IntCache colormapNotifyColormapCache;

  // ConfigureNotify event
  IntCache* configureNotifyWindowCache[3];
  IntCache* configureNotifyGeomCache[5];

  // Expose event
  IntCache exposeWindowCache;
  IntCache* exposeGeomCache[5];

  // FocusIn event
  // (also used for FocusOut)
  IntCache focusInWindowCache;

  // KeymapNotify event
  static BlockCache lastKeymap;

  // MapNotify event
  // (also used for UnmapNotify)
  IntCache mapNotifyEventCache;
  IntCache mapNotifyWindowCache;

  // MotionNotify event
  // (also used for KeyPress, KeyRelease, ButtonPress, ButtonRelease,
  //  EnterNotify, and LeaveNotify events and QueryPointer reply)
  IntCache motionNotifyTimestampCache;
  unsigned int motionNotifyLastRootX;
  unsigned int motionNotifyLastRootY;
  IntCache motionNotifyRootXCache;
  IntCache motionNotifyRootYCache;
  IntCache motionNotifyEventXCache;
  IntCache motionNotifyEventYCache;
  IntCache motionNotifyStateCache;
  IntCache* motionNotifyWindowCache[3];

  // NoExpose event
  IntCache noExposeDrawableCache;
  IntCache noExposeMinorCache;
  CharCache noExposeMajorCache;

  // PropertyNotify event
  IntCache propertyNotifyWindowCache;
  IntCache propertyNotifyAtomCache;

  // VisibilityNotify event
  IntCache visibilityNotifyWindowCache;


  // GetGeometry reply
  IntCache getGeometryRootCache;
  IntCache* getGeometryGeomCache[5];

  // GetInputFocus reply
  IntCache getInputFocusWindowCache;

  // GetKeyboardMapping reply
  static unsigned char getKeyboardMappingLastKeysymsPerKeycode;
  static BlockCache getKeyboardMappingLastMap;
  IntCache getKeyboardMappingKeysymCache;
  CharCache getKeyboardMappingLastByteCache;

  // GetModifierMapping reply
  static BlockCache getModifierMappingLastMap;

  // GetProperty reply
  CharCache getPropertyFormatCache;
  IntCache getPropertyTypeCache;
  TextCompressor getPropertyTextCompressor;
  static BlockCache xResources;

  // GetSelection reply
  IntCache getSelectionOwnerCache;

  // GetWindowAttributes reply
  IntCache getWindowAttributesClassCache;
  CharCache getWindowAttributesBitGravityCache;
  CharCache getWindowAttributesWinGravityCache;
  IntCache getWindowAttributesPlanesCache;
  IntCache getWindowAttributesPixelCache;
  IntCache getWindowAttributesAllEventsCache;
  IntCache getWindowAttributesYourEventsCache;
  IntCache getWindowAttributesDontPropagateCache;

  // QueryColors reply
  BlockCache queryColorsLastReply;

  // QueryFont reply
  static BlockCacheSet queryFontFontCache;
  IntCache* queryFontCharInfoCache[6];
  unsigned int queryFontLastCharInfo[6];

  // QueryPointer reply
  IntCache queryPointerRootCache;
  IntCache queryPointerChildCache;

  // TranslateCoords reply
  IntCache translateCoordsChildCache;
  IntCache translateCoordsXCache;
  IntCache translateCoordsYCache;
};

#endif /* ServerCache_H */
