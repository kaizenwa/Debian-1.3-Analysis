//=======================================================================
//	vgdefs.h:	Defs for vg app
//=======================================================================
#ifndef VGDEFS_H
#define VGDEFS_H
// Define where V lives on your system

#ifdef LINUX
#define DEF_HOMEV "/usr" // to find libV.a
#else
#define DEF_HOMEV "/usr/local/v" // to find libV.a
#endif
#define DEF_VPATH "/include"	// to find v_defs.h, etc. for compile, link
#define VGEN_VERS "1.02"

    typedef struct vgOptions
      {
	    char appName[100];	// base name of app classes
	    char fileName[100]; // base name of app files
	    char title[100];    // title of app
	    char home[100];     // V home
	    int	addDialog,	// if add a dialog
		addModal, 	// if add modal dialog
		addMake, 	// if add makefile
		extraDialog,	// if making an extra dialog
		addToolBar,	// if add tool bar
		addStatus, 	// if add status bar
		addDate,	// if add date/time
		vScroll,	// if include v Scroll bar
		hScroll,	// if include h scroll bar
		canvasType,	// kind of canvas to generate
		tinyApp,	// for a menuless/canvasless app
		winSDI,		// if a Windows SDI model
		fullMenu;	// if a full menu or short menu
      } vgOptions;
#endif
