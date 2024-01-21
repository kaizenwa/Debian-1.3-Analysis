//$Id: dxfsave.cpp 4.60 1996/09/21 23:00:20 RICK Released RICK $

/*
DXFSAVE.CPP - DXF Module for Geodesic Class

	Copyright (C) 1995, 1996 Richard J. Bono

	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

	 Please direct inquiries, comments and modifications to:
	 Richard J. Bono
	 44 Augusta Rd.
	 Brownsville, TX 78521

	 email: rjbono@hiline.net

Revision history:

$Log: dxfsave.cpp $
'Revision 4.60  1996/09/21  23:00:20  RICK
'Released Version
'-Added Buckyball VRML support
'-Added wire-frame for DXF & VRML
'-Added face & axial Angle Calcs to DAT
'-Added full sphere Class II support
'-Enhanced POV-ray output
'
'Revision 4.50  1996/08/19  00:10:15  RICK
'Production Release
'-Split POV Output into a scene & geometry file
'-Added texture to POV output
'-Added Wire-frame output to VRML Output
'-Added Full support for Class II Spheres
'
'Revision 4.20  1996/01/27  23:23:35  RICK
'Production Release
'Added elliptical support
'Added enhanced buckyball constructs
'Streamlined Code
'
'Revision 4.0  1995/12/31  18:59:31  RICK
'-Changed data structures to array based linked-lists
'-Many function changes to accomodate linked lists
'-Added Buckball support
'-Added VRML output functions
'-Added face functions
'-Added faces to POV data.
'-DXF data saves face data instead of chord data
'-Source code split into modules
'-Added time passage display while calculating
'-File name is displayed when execution is complete.
'-deleted make_sphere function
*/

#include "geodesic.hpp"

//----------------------------------------DXF File memeber Functions
//-------------------save DXF file
void Geodesic::save_dxf(char *filename)
{
	double Ax, Ay, Az, Bx, By, Bz, Cx, Cy, Cz;
	long end_face;

	//save Face data for symmetry triangle to DXF file
	//filename should include .DXF extension on MS-DOS systems
	//For full spheres each face is saved on a different level...

	ofstream DXF(filename);
	//output DXF data
	DXF << "0" << '\n';
	DXF << "SECTION" << '\n';
	DXF << "2" << '\n';
	DXF << "ENTITIES" << '\n';
	//Set field widths
	DXF << setiosflags(ios::fixed) << setw(8) << setprecision(6);

	//determine the number of faces required (first face = 0)
	if(sphere_flg)
		end_face = face_quantity(classtype, polytype);
	else
		end_face = 0;


	if(!show_status){
		cout << "Saving Data to File... ";
		status_count = 0;
	}

	for(j=0; j<=end_face; j++){

		//Get the vertex data for the face in question
		if(polytype == 1)
			icosa_sphere(j);
		else if(polytype == 2)
			octa_sphere(j);
		else if(polytype == 3)
			tetra_sphere(j);

		DXF << "999" << '\n';
		DXF << "Face #" << (j+1) << '\n';
		for(i=1; i<=face_calc; i++){
				if(!show_status){
					time_passage(status_count);
					status_count++;
					if(status_count > 3)
						status_count = 0;
				}

				//convert spherical to cartesian.
				//Face point A
				Ax = sphere_pnt[polyface[i].cornerA].radius *
						clean_float(cos(sphere_pnt[polyface[i].cornerA].phi * DEG_TO_RAD) *
										sin(sphere_pnt[polyface[i].cornerA].theta * DEG_TO_RAD));
				Ay = sphere_pnt[polyface[i].cornerA].radius *
						clean_float(sin(sphere_pnt[polyface[i].cornerA].phi * DEG_TO_RAD) *
										sin(sphere_pnt[polyface[i].cornerA].theta * DEG_TO_RAD));
				Az = sphere_pnt[polyface[i].cornerA].radius *
						clean_float(cos(sphere_pnt[polyface[i].cornerA].theta * DEG_TO_RAD));

				//Face point B
				Bx = sphere_pnt[polyface[i].cornerB].radius *
						clean_float(cos(sphere_pnt[polyface[i].cornerB].phi * DEG_TO_RAD) *
										sin(sphere_pnt[polyface[i].cornerB].theta * DEG_TO_RAD));
				By = sphere_pnt[polyface[i].cornerB].radius *
						clean_float(sin(sphere_pnt[polyface[i].cornerB].phi * DEG_TO_RAD) *
										sin(sphere_pnt[polyface[i].cornerB].theta * DEG_TO_RAD));
				Bz = sphere_pnt[polyface[i].cornerB].radius *
						clean_float(cos(sphere_pnt[polyface[i].cornerB].theta * DEG_TO_RAD));

				//Face point C
				Cx = sphere_pnt[polyface[i].cornerC].radius *
						clean_float(cos(sphere_pnt[polyface[i].cornerC].phi * DEG_TO_RAD) *
										sin(sphere_pnt[polyface[i].cornerC].theta * DEG_TO_RAD));
				Cy = sphere_pnt[polyface[i].cornerC].radius *
						clean_float(sin(sphere_pnt[polyface[i].cornerC].phi * DEG_TO_RAD) *
										sin(sphere_pnt[polyface[i].cornerC].theta * DEG_TO_RAD));
				Cz = sphere_pnt[polyface[i].cornerC].radius *
						clean_float(cos(sphere_pnt[polyface[i].cornerC].theta * DEG_TO_RAD));

				//save data
				DXF << "0" << '\n';
				DXF << "3DFACE" << '\n';	//Save as a 3D Polyface entity
				DXF << "8" << '\n';
				DXF << (j + 1) << '\n';		//Each face is saved on a different level.
				DXF << "62" << '\n';
				DXF << "3" << '\n';
				DXF << "10" << '\n';
				DXF << Ax << '\n';
				DXF << "20" << '\n';
				DXF << Ay << '\n';
				DXF << "30" << '\n';
				DXF << Az << '\n';
				DXF << "11" << '\n';
				DXF << Bx << '\n';
				DXF << "21" << '\n';
				DXF << By << '\n';
				DXF << "31" << '\n';
				DXF << Bz << '\n';
				DXF << "12" << '\n';
				DXF << Cx << '\n';
				DXF << "22" << '\n';
				DXF << Cy << '\n';
				DXF << "32" << '\n';
				DXF << Cz << '\n';
				DXF << "13" << '\n';
				DXF << Cx << '\n';
				DXF << "23" << '\n';
				DXF << Cy << '\n';
				DXF << "33" << '\n';
				DXF << Cz << '\n';
		}

	}

	cout << '\r' << "                     " << '\r'; //Clear status signal

	DXF << "0" << '\n';
	DXF << "ENDSEC" << '\n';
	DXF << "0" << '\n';
	DXF << "EOF" << '\n';

	DXF.close();
}
//-------------------save DXF file in Wire-frame mode
void Geodesic::save_dxf_wire(char *filename)
{
	double Ax, Ay, Az, Bx, By, Bz;
	long end_face;

	//save chord data for symmetry triangle to DXF file
	//filename should include .DXF extension on MS-DOS systems
	//For full spheres each face is saved on a different level...

	ofstream DXF(filename);
	//output DXF data
	DXF << "0" << '\n';
	DXF << "SECTION" << '\n';
	DXF << "2" << '\n';
	DXF << "ENTITIES" << '\n';
	//Set field widths
	DXF << setiosflags(ios::fixed) << setw(8) << setprecision(6);

	//determine the number of faces required (first face = 0)
	if(sphere_flg)
		end_face = face_quantity(classtype, polytype);
	else
		end_face = 0;


	if(!show_status){
		cout << "Saving Data to File... ";
		status_count = 0;
	}

	for(j=0; j<=end_face; j++){

		//Get the vertex data for the face in question
		if(polytype == 1)
			icosa_sphere(j);
		else if(polytype == 2)
			octa_sphere(j);
		else if(polytype == 3)
			tetra_sphere(j);

		DXF << "999" << '\n';
		DXF << "Face #" << (j+1) << '\n';

		for(i=1; i<=edges_calc; i++){
				if(!show_status){
					time_passage(status_count);
					status_count++;
					if(status_count > 3)
						status_count = 0;
				}

				//convert spherical to cartesian.
				//Start of chord
				Ax = sphere_pnt[edgepts[i].start].radius *
						clean_float(cos(sphere_pnt[edgepts[i].start].phi * DEG_TO_RAD) *
										sin(sphere_pnt[edgepts[i].start].theta * DEG_TO_RAD));
				Ay = sphere_pnt[edgepts[i].start].radius *
						clean_float(sin(sphere_pnt[edgepts[i].start].phi * DEG_TO_RAD) *
										sin(sphere_pnt[edgepts[i].start].theta * DEG_TO_RAD));
				Az = sphere_pnt[edgepts[i].start].radius *
						clean_float(cos(sphere_pnt[edgepts[i].start].theta * DEG_TO_RAD));

				//End of chord
				Bx = sphere_pnt[edgepts[i].end].radius *
						clean_float(cos(sphere_pnt[edgepts[i].end].phi * DEG_TO_RAD) *
										sin(sphere_pnt[edgepts[i].end].theta * DEG_TO_RAD));
				By = sphere_pnt[edgepts[i].end].radius *
						clean_float(sin(sphere_pnt[edgepts[i].end].phi * DEG_TO_RAD) *
										sin(sphere_pnt[edgepts[i].end].theta * DEG_TO_RAD));
				Bz = sphere_pnt[edgepts[i].end].radius *
						clean_float(cos(sphere_pnt[edgepts[i].end].theta * DEG_TO_RAD));

				//save data
				DXF << "0" << '\n';
				DXF << "LINE" << '\n';	//Save as a 3D Polyface entity
				DXF << "8" << '\n';
				DXF << (j + 1) << '\n';		//Each face is saved on a different level.
				DXF << "62" << '\n';
				DXF << "3" << '\n';
				DXF << "10" << '\n';
				DXF << Ax << '\n';
				DXF << "20" << '\n';
				DXF << Ay << '\n';
				DXF << "30" << '\n';
				DXF << Az << '\n';
				DXF << "11" << '\n';
				DXF << Bx << '\n';
				DXF << "21" << '\n';
				DXF << By << '\n';
				DXF << "31" << '\n';
				DXF << Bz << '\n';
		}
	}
	cout << '\r' << "                     " << '\r'; //Clear status signal

	DXF << "0" << '\n';
	DXF << "ENDSEC" << '\n';
	DXF << "0" << '\n';
	DXF << "EOF" << '\n';

	DXF.close();
}

//-------------------save Buckyball data in DXF format
void Geodesic::save_buckydxf(char *filename)
{
	double Ax, Ay, Az, Bx, By, Bz;
	long end_face;

	//save chords data for buckyball chord data to DXF file
	//filename should include .DXF extension on MS-DOS systems

	ofstream DXF(filename);
	//output DXF data
	DXF << "0" << '\n';
	DXF << "SECTION" << '\n';
	DXF << "2" << '\n';
	DXF << "ENTITIES" << '\n';
	//Set field widths
	DXF << setiosflags(ios::fixed) << setw(8) << setprecision(6);



	//determine the number of faces required (first face = 0)
	if(sphere_flg)
		end_face = face_quantity(classtype, polytype);
	else
		end_face = 0;

	edges_calc = bucky_edges;

	if(!show_status){
		cout << "Saving Data to File... ";
		status_count = 0;
	}

	for(j=0; j<=end_face; j++){
		//Get the vertex data for the face in question
		if(polytype == 1)
			icosa_sphere(j);
		else if(polytype == 2)
			octa_sphere(j);
		else if(polytype == 3)
			tetra_sphere(j);

		DXF << "999" << '\n';
		DXF << "Face #" << (j+1) << '\n';
		for(i=1; i<=edges_calc; i++){
				if(!show_status){
					time_passage(status_count);
					status_count++;
					if(status_count > 3)
						status_count = 0;
				}

				//convert spherical to cartesian
				//start point of chord
				Ax = sphere_pnt[edgepts[i].start].radius *
						clean_float(cos(sphere_pnt[edgepts[i].start].phi * DEG_TO_RAD) *
										sin(sphere_pnt[edgepts[i].start].theta * DEG_TO_RAD));
				Ay = sphere_pnt[edgepts[i].start].radius *
						clean_float(sin(sphere_pnt[edgepts[i].start].phi * DEG_TO_RAD) *
										sin(sphere_pnt[edgepts[i].start].theta * DEG_TO_RAD));
				Az = sphere_pnt[edgepts[i].start].radius *
						clean_float(cos(sphere_pnt[edgepts[i].start].theta * DEG_TO_RAD));

				//end point of chord
				Bx = sphere_pnt[edgepts[i].end].radius *
						clean_float(cos(sphere_pnt[edgepts[i].end].phi * DEG_TO_RAD) *
										sin(sphere_pnt[edgepts[i].end].theta * DEG_TO_RAD));
				By = sphere_pnt[edgepts[i].end].radius *
						clean_float(sin(sphere_pnt[edgepts[i].end].phi * DEG_TO_RAD) *
										sin(sphere_pnt[edgepts[i].end].theta * DEG_TO_RAD));
				Bz = sphere_pnt[edgepts[i].end].radius *
						clean_float(cos(sphere_pnt[edgepts[i].end].theta * DEG_TO_RAD));

				//save data
				DXF << "0" << '\n';
				DXF << "LINE" << '\n';
				DXF << "8" << '\n';
				DXF << (j + 1) << '\n';
				DXF << "62" << '\n';
				DXF << "3" << '\n';
				DXF << "10" << '\n';
				DXF << Ax << '\n';
				DXF << "20" << '\n';
				DXF << Ay << '\n';
				DXF << "30" << '\n';
				DXF << Az << '\n';
				DXF << "11" << '\n';
				DXF << Bx << '\n';
				DXF << "21" << '\n';
				DXF << By << '\n';
				DXF << "31" << '\n';
				DXF << Bz << '\n';
		}
	}
	cout << '\r' << "                     " << '\r'; //Clear status signal

	DXF << "0" << '\n';
	DXF << "ENDSEC" << '\n';
	DXF << "0" << '\n';
	DXF << "EOF" << '\n';

	DXF.close();
}

//End of DXFSAVE