#include "Types.r"
#include "SysTypes.r"

// Resources shared by Think C and MPW C versions.

include "IMFAppProj.rsrc" not 'ckid';

// Need this for MPW only, since Think C synthesizes.

resource 'SIZE' (-1, purgeable)
{
	reserved,
	acceptSuspendResumeEvents,	// ignoreSuspendResumeEvents
	reserved,
	canBackground,				// cannotBackground
	doesActivateOnFGSwitch,		// needsActivateOnFGSwitch
	backgroundAndForeground,	// onlyBackground
	dontGetFrontClicks,			// getFrontClicks
	ignoreAppDiedEvents,		// acceptAppDiedEvents
	is32BitCompatible,			// not32BitCompatible
	isHighLevelEventAware,		// notHighLevelEventAware
	localAndRemoteHLEvents,		// onlyLocalHLEvents
	notStationeryAware,			// isStationeryAware
	dontUseTextEditServices,	// useTextEditServices
	reserved,
	reserved,
	reserved,
	1000*1024,					// preferred mem size (Bytes)
	500*1024					// minimum mem size (Bytes)
};

