#ifndef MAC_GLOB
#define MAC_GLOB 1
/* Types. */

typedef char CStr255[256];	/* like Str255, except for C-format strings. */

extern Boolean		gDone;	/* flag set true upon program termination */
extern Boolean 		gCancel;/* flag set when user cancels an action */

extern Handle		gLifeBoat;/* lifeboat memory -- de-allocated when
				   * memory gets low */

extern Boolean		gSinking; /* flag set after lifeboat has been jettisoned */

extern Boolean 		gOutOfMemory; /* flag set when out of memory - and luck */

#endif /* MAC_GLOB */
