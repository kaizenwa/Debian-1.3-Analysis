//$Id: geodesic.hpp 4.60 1996/09/21 23:00:26 RICK Released RICK $

/*
GEODESIC.HPP - C++ Header file for geodesic.cpp geodesic dome class

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

	 email: rjbono@cris.com

Revision history:

$Log: geodesic.hpp $
'Revision 4.60  1996/09/21  23:00:26  RICK
'Released Version
'-Added Buckyball VRML support
'-Added wire-frame for DXF & VRML
'-Added face & axial Angle Calcs to DAT
'-Added full sphere Class II support
'-Enhanced POV-ray output
'
'Revision 4.50  1996/08/19  00:10:40  RICK
'Production Release
'-Split POV Output into a scene & geometry file
'-Added texture to POV output
'-Added Wire-frame output to VRML Output
'-Added Full support for Class II Spheres
'
'Revision 4.20  1996/01/27  23:23:45  RICK
'Production Release
'Added elliptical support
'Added enhanced buckyball constructs
'Streamlined Code
'
'Revision 4.0  1995/12/31  18:59:38  RICK
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

#include<iostream.h>
#include<iomanip.h>
#include<fstream.h>
#include<stdlib.h>
#include<math.h>
#include<ctype.h>
#include<string.h>

#ifdef __BORLANDC__
#include<conio.h>
#endif

#ifdef __MSDOS__
#define HUGEPREFIX huge
#else
#define HUGEPREFIX
#endif

const double RAD_TO_DEG = 57.2957795130824;

const double DEG_TO_RAD = .0174532925199433;

#define BYTE unsigned char
#define WORD unsigned int

//Structure containing input parameters
struct parameters{
	long freq;				//Frequency of subdivison
	long classt;			//Class Type
	long polyt;				//Polyhedron Type
	long filet;				//File Output Type
	char filename[80];	//Output filename
	int verbose_flag;		//Suppress Topology display
	int sphere_flag;		//Generate sphere
	int buckyball;			//Generate buckyball
	int faceflag;			//Generate data files based on lines or polyfaces
	int suppress_status;	//Suppress display of calculation status
	double E;				//Elliptical eccentricity (0 - 1)
};

//--------------geodesic points class
class Geodesic	{
private:
	//Various indices, and temporary variables
	long i,	j, k;
	long index;
	double sphi, stheta, ephi, etheta, X, Y, Z, E;
	//actual values used in calculations set dependent on class type
	long freq_calc, vertex_calc, face_calc, edges_calc;
	int status_count;

public:
	long frequency, vertex, faces, edges, show_status;
	long sphere_flg, face_flag, ellipse_flag;
	long vertexII, facesII, edgesII;		//class II topology
	long bucky_face, bucky_vertex, bucky_edges, bucky_ball;
	long classtype, polytype;

	//structure containing intermediate coordinates
	struct sphere{
		//vertex coordinates: 0,0,0 equals zenith
		long xprime;
		long yprime;
		long zprime;
	};

	//structure containing vertex labels
	struct label{
		//vertex labels: 0,0 equals zenith
		long A;
		long B;
	};

	//structure defines geodesic coordinate points
	struct coordinates{
		//vertex spherical coordinates - This is the main data structure
		double phi;
		double theta;
		double radius;
	};

	//structure defines chord data
	struct chords{
		//Chords are defined by the point array index
		//This structure stores the vertex array index for the chord start
		//and end points.
		long	start;		//chord start point
		long 	end;			//chord end point
	};

	//structure defines face data
	struct polygon_face{
		//chords are defined by the point array index
		//This structure stores the vertex array index for the three
		//points required to define a triangular face.
		long cornerA;
		long cornerB;
		long cornerC;
	};


	//portability note: huge keyword was needed in order to allocate memory for

	//arrays from the far heap in a MSDOS far memory model.
	sphere HUGEPREFIX *pnt_calc;			//pointer to spherical point variable array
	label HUGEPREFIX *pnt_label;			//pointer to label array
	coordinates HUGEPREFIX *pntcrd;  	//pointer to coordinate array
	chords HUGEPREFIX *edgepts;      	//pointer to chord array
	polygon_face HUGEPREFIX *polyface;	//pointer to face array
	coordinates HUGEPREFIX *sphere_pnt; //pointer to sphere array

	Geodesic(struct parameters *command);	//constructor
	~Geodesic();		   						//destructor
	void topology(void);	   					//topological abundance function
	void init_crd(void);	   					//Set up coordinate values
	void spherical(void);	   				//calculate spherical coordinates
	double chord_length(double, double, double, double); //chord length function
	void chord_factor(void);   				//calculates chord factors
	void face_factor(void);						//calculates face factors
	void icosa_sphere(long);					//Make an icosa face
	void octa_sphere(long);						//Make an octa face
	void tetra_sphere(long);			  		//Make a Tetra face
	long tetra_angle(void);						//Get "A" coordinate to begin correction of bottom tetra face
	void save_dxf(char *);     				//save face data in DXF format
	void save_dxf_wire(char *);				//save DXF wireframe
	void save_buckydxf(char *);				//save bucky chords in DXF format
	void save_ascii(char *);   				//save all coordinate data in ASCII
	void save_POV(char *);						//save data in POV format
	void save_buckypov(char *);				//save buckyball in DXF format
	void save_PRN(char *);						//Save raw data in ASCII
	void save_WRL(char *);						//Save data in VRML 1.0 WRL format - Indexed-face sets
	void save_WRL_wire(char *);				//Save data as indexed-line sets
	void save_buckywrl(char *);				//Save buckyball as wire-frame data
	void display_data(void);					//Display data during program execution
	double clean_float(double);				//function cleans up triag zeros
	double rotate_phi(double, double, double, double);	//phi rotation function
	double rotate_theta(double, double, double, double); //theta rotation function
	void time_passage(int);						//Show a time passage signal
	void bucky_factor(void);					//Generate Buckyball chord factors
	double ellipse_radius(double, double);	//calculate elliptical radius
	double root_E(double, double);			//root E theta correction for ellipses
	long face_quantity(long, long);			//determine the number of faces required for sphere
	double axial_angle_A(long);				//Determine Axial Angle A given chord index
	double axial_angle_B(long, double);		//Determine Axial Angle B given chord index & Angle A
	double face_angle_A(long);					//Determine Face Angle A given face index
	double face_angle_B(long, double);		//Determine Face Angle B Given face index & Face angle A
	double face_angle_C(double, double);	//Determine Face Angle C given Angle A & B
};
