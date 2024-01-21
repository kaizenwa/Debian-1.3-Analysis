//$Id: dome.cpp 4.60 1996/09/21 23:00:17 RICK Released RICK $

/*
DOME.CPP - A program for the calculation of geodesic dome properties
			  using geodesic class

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

$Log: dome.cpp $
'Revision 4.60  1996/09/21  23:00:17  RICK
'Released Version
'-Added Buckyball VRML support
'-Added wire-frame for DXF & VRML
'-Added face & axial Angle Calcs to DAT
'-Added full sphere Class II support
'-Enhanced POV-ray output
'
'Revision 4.50  1996/08/19  00:10:01  RICK
'Production Release
'-Split POV Output into a scene & geometry file
'-Added texture to POV output
'-Added Wire-frame output to VRML Output
'-Added Full support for Class II Spheres
'
'Revision 4.20  1996/01/27  23:23:29  RICK
'Production Release
'Added elliptical support
'Added enhanced buckyball constructs
'Streamlined Code
'
'Revision 4.0  1995/12/31  18:59:27  RICK
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
'

Acknowledgements & References:
The main reference used in the creation of this code was "Geodesic Math & How
to Use It" by Hugh Kenner, 1976, University of California Press.
ISBN 0-520-02924-0; Library of Congress Catalog Card Number: 74-27292. Many
thanks to Hugh for putting this data in an accessible format.

Also, many thanks to:
	J. F. Nystrom
	My wife Sonia
	My daughter Kathy
	Chris Fearnley
	Kirby Urner
		&
	R. Buckminster Fuller for changing the way I view Universe.

Compliation:
This program was complied and tested using Borland C++ 4.52 using the large
memory model on a Gateway 2000 4DX2-50V.

Support is included for non-Borland compilation. Unix patches provided thanks
to Chris Fearnley & John Kirk.
*/

#include<geodesic.hpp>

static char rcsid[]="$Id: dome.cpp 4.60 1996/09/21 23:00:17 RICK Released RICK $";

parameters cmd_parm;				//input parameters structure

//Function prototypes
void logo_display(void);
void help_display(void);
void get_cmd(struct parameters *command, int param_count, char *param[]);
int main(int argc, char *argv[]);

void help_display(void)
{
	//Display usage and help then exit
	logo_display();
	cout << "Usage: dome [-fnnn] [-cn] [-px] [-s] [-sb] [-en] [-v] [-w] [-h] [filename.xxx]" << '\n';
	cout << "Where: -fnnn is geodesic frequency (default nnn=3)" << '\n';
	cout << "       -cn is class type (n=1 or 2; default n=1)" << '\n';
	cout << "       -en enables ellipse and specifies eccentricity (default = 1.0)" << '\n';
   cout << "           n > 0.0 and n < 2.0" << '\n';
	cout << "       -px sets the polyhedron type" << '\n';
	cout << "           where x is: i for icosahedron (default)" << '\n';
	cout << "                       o for octahedron" << '\n';
	cout << "                       t for tetrahedron" << '\n';
	cout << "       -s  generate full sphere data (default: symmetry triangle)" << '\n';
	cout << "       -sb generate buckyball. Sets Class I" << '\n';
	cout << "           Frequency must be a multiple of three" << '\n';
	cout << "       -w  Enable wire-frame DXF or VRML output (default: Face data)" << '\n';
	cout << "       -v  verbose data display at run-time" << '\n';
	cout << "       -h  displays this help screen" << '\n';
	cout << "           filename.xxx is a standard DOS filename" << '\n';
	cout << "           where xxx is: DXF, WRL, DAT, POV or PRN" << '\n';
	exit(2);
}
void logo_display(void)
{
	char rev[]="$Revision: 4.60 $";
	int i, j;
	char level[10];

	//Get the revision level. I have been using GNU RCS to do revision control
	//on dome. This revision level will be used to automatically display the
	//correct rev level.
	j = 11;
	i = 0;
	while(rev[j] != ' '){
		level[i] = rev[j];
		j++;
		i++;
	}
	level[i] = '\0';

	#ifdef __BORLANDC__
	clrscr();
	#endif

	cout << "Dome " << level << ", Copyright (C) 1995, 1996 - Richard J. Bono" << '\n';
	cout << "Dome comes with ABSOLUTELY NO WARRANTY. This is free software," << '\n';
	cout << "and you are welcome to redistribute it under certain conditions." << '\n';
	cout << "See GNU General Public License for more details." << '\n' << '\n';
}

void get_cmd(struct parameters *command, int param_count, char *param[])
{
	//Get and parse command line
	char cmd_parm[6];

	//Set defaults
	command->freq = 3;				//frequency = 3
	command->classt = 1;				//Class I
	command->polyt = 1;				//Icosahedron
	command->verbose_flag = 0;  	//disable verbose
	command->filet = 6;				//No file output
	command->sphere_flag = 0;		//Symmetry triangle only
	command->suppress_status = 0; //Status display enabled
	command->faceflag = 1;			//default face entities
	command->buckyball = 0;			//default buckyball disabled
	command->E = 1.0;					//default circle

	int t, j, k;

	for(t=1; t<param_count; ++t){
		if(param[t][0] == '-'){
			switch (tolower(param[t][1])){
				case ('p'):
					//Set polyhedron type
					if(tolower(param[t][2]) == 'i')
						//Set to Icosahedron
						command->polyt = 1;
					else if(tolower(param[t][2]) == 'o')
						//Set to octahedron
						command->polyt = 2;
					else if(tolower(param[t][2]) == 't')
						//Set to tetrahedron
						command->polyt = 3;
					else{
						cout << "Invalid Polyhedron Type --- Execution Terminating" << '\n';
						exit(1);
					}
					break;
				case ('h'):
					//Display help and exit. This overrides other parameters
					help_display();
					break;
				case ('v'):
					//Enable Parameter Display during execution
					command->verbose_flag = 1;
					break;
				case ('w'):
					//Enable wire frame output
					command->faceflag = 0;
					break;
				case ('s'):
					//generate buckyball or sphere
					if(tolower(param[t][2]) == 'b'){
						command->buckyball = 1;
						command->sphere_flag = 1;
					}
					else if(param[t][2] == '\0'){
						command->sphere_flag = 1;
						command->buckyball = 0;
					}
					else{
						cout << "Invalid command-line --- Execution Terminating" << '\n';
						exit(1);
					}
					break;
				case ('f'):
					//get frequency
					j=2;
					k=0;
					while(param[t][j]){
						cmd_parm[k] = param[t][j];
						j++;
						k++;
					}
					cmd_parm[k] = '\0';
					command->freq = atol(cmd_parm);
					if (command->freq <= 0){
						cout << "Invalid Frequency --- Execution Terminating" << '\n';
						exit(1);
					}
					break;
				case ('e'):
					//get ellipse flag and eccentricity
					j=2;
					k=0;
					while(param[t][j]){
						cmd_parm[k] = param[t][j];
						j++;
						k++;
					}
					cmd_parm[k] = '\0';
					command->E = atof(cmd_parm);
					if (command->E <= 0.0 || command->E >= 2.0){
						cout << "Invalid Eccentricity --- Execution Terminating" << '\n';
						exit(1);
					}
					break;
				case ('c'):
					//get class type
					j=2;
					k=0;
					while(param[t][j]){
						cmd_parm[k] = param[t][j];
						j++;
						k++;
					}
					cmd_parm[k] = '\0';
					command->classt = atol(cmd_parm);
					if (command->classt < 1 || command->classt > 2){
						cout << "Invalid Class Type --- Execution Terminating" << '\n';
						exit(1);
					}
					break;
				default:
					cout << "Invalid command-line --- Execution Terminating" << '\n';
					exit(1);
					break;
			}
		}
		else{
			//Check to see if parameter is a valid filename
			j = 0;
			while(param[t][j]){
				if(param[t][j] != '.')
					j++;
				else{
					//Check for valid extension
					j++;
					k = 0;
					while(param[t][j]){
						cmd_parm[k] = tolower(param[t][j]);
						j++;
						k++;
					}
					cmd_parm[k] = '\0';
					if(!strcmp(cmd_parm,"dxf"))
						command->filet = 1;
					else if(!strcmp(cmd_parm, "dat"))
						command->filet = 2;
					else if(!strcmp(cmd_parm, "pov"))
						command->filet = 3;
					else if(!strcmp(cmd_parm, "prn"))
						command->filet = 4;
					else if(!strcmp(cmd_parm, "wrl"))
						command->filet = 5;
					else{
						cout << "Invalid command-line --- Execution terminating" << '\n';
						exit(1);
					}
					j = 0;
					k = 0;
					while(param[t][j]){
						command->filename[k] = tolower(param[t][j]);
						j++;
						k++;
					}
					command->filename[k] = '\0';
				}
			}
		}
	}

	//Check input for Buckyballs
	//Force class I. Stop if frequency is not multiple of three
	if(command->buckyball){
		if(fmod(command->freq, 3) != 0){
			cout << "Buckyball Frequency must be a Multiple of Three --- Execution Terminating" << '\n';
			exit(1);
		}
		//Override polyhedron and class values if needed...
		if(command->classt != 1){
			command->classt = 1;
			cout << "Class I set for Buckyball" << '\n';
		}
	}

	if(fmod(command->freq, 2) != 0 && command->classt == 2){
		cout << "Class II requires Even Frequency --- Execution Terminating" << '\n';
		exit(1);
	}
}

int main(int argc, char *argv[])
{
	//This main routines shows what can be done with this class. It implements
	//a simple program which displays dome parameters and optionally saves the
	//data in either DXF or ASCII file formats.

	//Some things that can be tried involve creating several instances of the
	//geodesic object. Geodesic shells can be created in this way given enough
	//memory.

	//Command line parameters are changing this routine into something more
	//permanent. Remember the class can be incorporated into your own programs.
	//This shell is just one possible implementation.

	//Get dome parameters
	if(argc == 1)
		//display usage and exit
		help_display();
	else
		//Get command line
		get_cmd(&cmd_parm, argc, argv);

	logo_display();
	//class instance
	Geodesic geosys(&cmd_parm);

	if(cmd_parm.verbose_flag)
		geosys.display_data();

	if(cmd_parm.filet == 1){
		//output file in DXF format
		if(cmd_parm.buckyball){
			geosys.save_buckydxf(cmd_parm.filename);
		}
		else if(cmd_parm.faceflag)
			geosys.save_dxf(cmd_parm.filename);
		else
			geosys.save_dxf_wire(cmd_parm.filename);
	}
	else if(cmd_parm.filet == 2){
		//output all data in ASCII report format
		if(cmd_parm.buckyball){
			cout << "ASCII Report not Valid for Buckyball Formation" << '\n';
			exit(1);
		}
		else if(cmd_parm.E != 1.0){
			cout << "ASCII Report not Valid for Elliptical Formation" << '\n';
			exit(1);
		}
		else
			geosys.save_ascii(cmd_parm.filename);
	}
	else if(cmd_parm.filet == 3){
		//output data in POV-Ray script format
		if(cmd_parm.buckyball)
			geosys.save_buckypov(cmd_parm.filename);
		else
			geosys.save_POV(cmd_parm.filename);
	}
	else if(cmd_parm.filet == 4){
		//output data in PRN Text format
		if(cmd_parm.buckyball){
			cout << "PRN Data File not Valid for Buckyball Formation" << '\n';
			exit(1);
		}
		else if(cmd_parm.E != 1.0){
			cout << "PRN Data File not Valid for Elliptical Formation" << '\n';
			exit(1);
		}
		else
			geosys.save_PRN(cmd_parm.filename);
	}
	else if(cmd_parm.filet == 5){
		//output data in VRML format
		if(cmd_parm.buckyball){
			geosys.save_buckywrl(cmd_parm.filename);
		}
		else if(cmd_parm.faceflag)
			geosys.save_WRL(cmd_parm.filename);
		else
			geosys.save_WRL_wire(cmd_parm.filename);
	}

	if(cmd_parm.filet == 6)
		cout << "Execution Complete" << '\n';
	else{
		cout << '\r' << "                      " << '\r';
		cout << "Execution Complete -- Output File: " << cmd_parm.filename << '\n';
	}

	return(0);
}
