#define PATCHLEVEL 0
/*
**    ScEd: A Constraint Based Scene Editor.
**    Copyright (C) 1994-1995  Stephen Chenney (stephen@cs.su.oz.au)
**
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 2 of the License, or
**    (at your option) any later version.
**
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
**    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
**	sced: A Constraint Based Object Scene Editor
**
**	export.c : Functions needed to export a file.  Just the callback really.
**
**	External Function:
**
**	Export_Callback(Widget, XtPointer, XtPointer)
**	The callback function for the export menu selection.
**	Pops up a dialog (with others if necessary) and then calls the appropriate
**	raytracer specific function.
*/

#include <sced.h>
#include <instance_list.h>
#include <unistd.h>
#include <sys/param.h>
#include <SelFile.h>
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Text.h>

#if ( ! defined(MAXPATHLEN) )
#define MAXPATHLEN 1024
#endif


extern void Target_Callback(Widget, XtPointer, XtPointer);

#if RAYSHADE_SUPPORT
extern int	Export_Rayshade(FILE*, ScenePtr);
#endif
#if POVRAY_SUPPORT
extern int	Export_POVray(FILE*, ScenePtr);
#endif
#if RENDERMAN_SUPPORT
extern int	Export_Renderman(FILE*, char*, ScenePtr, Boolean);
#endif
#if RADIANCE_SUPPORT
extern int	Export_Radiance(FILE*, char*, ScenePtr, Boolean);
#endif
#if VRML_SUPPORT
extern int	Export_VRML(FILE*, ScenePtr);
#endif
#if GENRAY_SUPPORT
extern int	Export_Genray(FILE*, ScenePtr);
#endif
#if GENSCAN_SUPPORT
extern int	Export_Genscan(FILE*, ScenePtr);
#endif
#if BERKELEY_SUPPORT
extern int	Export_Berkeley(FILE*, ScenePtr);
#endif

static char		*export_file_name = NULL;


/*	void
**	Export_Callback(Widget w, XtPointer cl_data, XtPointer ca_data)
**	The callback invoked for the export menu function.
**	Checks for a target, then pops up the export dialog.
*/
void
Export_Callback(Widget w, XtPointer cl_data, XtPointer ca_data)
{
	SceneStruct	export_scene;
	FILE		*outfile;
	char		path_name[MAXPATHLEN];
	char		warning[128];
	Boolean		displayed = ( cl_data ? TRUE : FALSE );

	/* Need to have a target. */
	if ( target_renderer == NoTarget )
		Target_Callback(NULL, NULL, NULL);

	if ( displayed )
		export_scene.instances =
			Instances_Build_Visible_List(main_window.all_instances);
	else
		export_scene.instances = main_window.all_instances;

	/* Check for camera and lights. */
	warning[0] = '\0';
	if ( ! camera.defined )
		strcat(warning, "Warning: No camera defined.\n");
	if ( ! Light_In_Instances(export_scene.instances) )
		strcat(warning, "Warning: No lights defined.\n");
	if ( ambient_light.red == 0 &&
		 ambient_light.green == 0 &&
		 ambient_light.blue == 0 )
		strcat(warning, "Warning: No ambient light.\n");

	if ( warning[0] != '\0' )
		Popup_Error(warning, main_window.shell, "Warning");

	if ( ! export_file_name )
	{
		strcat(path_name, "/");
		if ( io_file_name )
		{
			char	*dot;

			strcpy(path_name, io_file_name);
			dot = strrchr(path_name, '/');
			if ( ! dot )
				dot = path_name;
			dot = strchr(dot, '.');
			if ( dot )
				*(dot + 1) = '\0';
			else
				strcat(path_name, ".");
		}
		else
		{
			if ( sced_preferences.scene_path )
				strcpy(path_name, sced_preferences.scene_path);
			else
#if HAVE_GETWD
				getwd(path_name);
#else
				getcwd(path_name, MAXPATHLEN);
#endif
			strcat(path_name, "/*.");
		}
		switch ( target_renderer )
		{
			case Genray:
				strcat(path_name, "gen");
				break;
			case Genscan:
#if BERKELEY_SUPPORT
				strcat(path_name, "world");
#else
				strcat(path_name, "gen");
#endif
				break;
			case POVray:
				strcat(path_name, "pov");
				break;
			case Rayshade:
				strcat(path_name, "ray");
				break;
			case Radiance:
				strcat(path_name, "rif");
				break;
			case Renderman:
				strcat(path_name, "rib");
				break;
			case VRML:
				strcat(path_name, "wrl");
				break;
			default:;
		}
	}
	else
		strcpy(path_name, export_file_name);

	outfile = XsraSelFile(main_window.shell, "Export to:", "Export", "Cancel",
							NULL, path_name, "w", NULL, &export_file_name);

	export_scene.target = target_renderer;
	export_scene.camera = camera;
	export_scene.light = NULL;
	export_scene.ambient = ambient_light;
	if ( outfile )
		Export_File(outfile, export_file_name, &export_scene, FALSE);

	if ( displayed )
		Free_Selection_List(export_scene.instances);
}


int
Export_File(FILE *outfile, char *name, ScenePtr scene, Boolean preview)
{
	int	res = 0;

	switch ( scene->target )
	{
		case Rayshade:
#if RAYSHADE_SUPPORT
			res = Export_Rayshade(outfile, scene);
#else
			Popup_Error("Rayshade is not supported", main_window.shell,"Error");
			res = FALSE;
#endif
			break;
		case POVray:
#if POVRAY_SUPPORT
			res = Export_POVray(outfile, scene);
#else
			Popup_Error("POVray is not supported", main_window.shell, "Error");
			res = FALSE;
#endif
			break;
		case Renderman:
#if RENDERMAN_SUPPORT
			res = Export_Renderman(outfile, name, scene, preview);
#else
			Popup_Error("Renderman is not supported",main_window.shell,"Error");
			res = FALSE;
#endif
			break;
		case Radiance:
#if RADIANCE_SUPPORT
			res = Export_Radiance(outfile, name, scene, preview);
#else
			Popup_Error("Radiance is not supported", main_window.shell,"Error");
			res = FALSE;
#endif
			break;
		case VRML:
#if VRML_SUPPORT
			res = Export_VRML(outfile, scene);
#else
			Popup_Error("VRML is not supported", main_window.shell,"Error");
			res = FALSE;
#endif
			break;
		case Genray:
#if GENRAY_SUPPORT
			res = Export_Genray(outfile, scene);
#else
			Popup_Error("Genray is not supported", main_window.shell, "Error");
			res = FALSE;
#endif
			break;
		case Genscan:
#if GENSCAN_SUPPORT
			res = Export_Genscan(outfile, scene);
#elif BERKELEY_SUPPORT
			res = Export_Berkeley(outfile, scene);
#else
			Popup_Error("Genscan is not supported", main_window.shell, "Error");
			res = FALSE;
#endif
			break;
		case NoTarget:
		case LastTarget:
			res = TRUE;
			break;
	}

	fclose(outfile);

	return res;
}


