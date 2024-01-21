//$Id: dxftopov.cpp 1.4 1996/01/21 06:32:28 RICK Released RICK $

/*
DXFTOPOV.CPP - A program utility to convert DXF 3D line data to POV-ray script
					format. This program only converts DXF line entities.

	 Copyright (C) 1995, 1996  Richard J. Bono

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

$Log: dxftopov.cpp $
'Revision 1.4  1996/01/21  06:32:28  RICK
'Corrected level error during scan for "10"
'Removed clrscr and conio.h
'Added sphere and cylinders to output
'
'Revision 1.3  1996/01/21  01:34:14  RICK
'Added face functions
'
'Revision 1.2  1995/02/06  13:53:42  PONDERMATIC
'Added a few carriage returns and updated revision
'levels
'
'Revision 1.1  1995/02/06  13:47:55  PONDERMATIC
'Initial.
'

Acknowledgements & References:
Kirby Urner for subtly guiding my thought processes over a three day-weekend
resulting in this code. Chris Fearnley for providing GNU POV scripts for several
polyhedrons. Most of the POV header sections came from Chris' scripts. POV-ray
is available on most on-line services and through the internet. DXF file format
data is available on Compuserve. Search for file R12DXF.TXT in the Acad forum.

Compliation:
This program was complied and tested using Borland C++ 4.52 using the large
memory model on a Gateway 2000 4DX2-50V.

Comments:
This program is slowly evolving into perhaps a useful utility. DOME produces
DXF file output in either DXF line (for buckyball) or 3D polyface for all
other DXF constructs. DXFtoPOV converts DXF formats to POV-ray scripts. This program
does not currently eliminate redundant connections so that the script files
end up a bit larger than they should. Also it may not be able to handle some
non-DOME produced DXF files. I will be addressing these issues in a future
release.
*/

#include<iostream.h>
#include<iomanip.h>
#include<fstream.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include<io.h>

static char rcsid[]="$Id: dxftopov.cpp 1.4 1996/01/21 06:32:28 RICK Released RICK $";

int main(int argc, char *argv[]);
void logo_display(void);

void logo_display(void)
{
	char rev[]="$Revision: 1.4 $";
	int i, j;
	char level[10];

	//Get the revision level. I have been using GNU RCS to do revision control.
	//This revision level will be used to automatically display the correct rev level.

	j = 11;
	i = 0;
	while(rev[j] != ' '){
		level[i] = rev[j];
		j++;
		i++;
	}
	level[i] = '\0';

	cout << "DXFtoPOV " << level << ", Copyright (C) 1995, 1996 - Richard J. Bono" << '\n';
	cout << "DXFtoPOV comes with ABSOLUTELY NO WARRANTY. This is free software," << '\n';
	cout << "and you are welcome to redistribute it under certain conditions." << '\n';
	cout << "See GNU General Public License for more details." << '\n' << '\n';
}

int main(int argc, char *argv[])
{
	char DXFfile[80];
	char POVfile[80];
	char DXF_line[80];
	char last_line[80];
	double sX, sY, sZ, eX, eY, eZ, cX, cY, cZ;
	int temp, temp1, line_ok, line_value, face_value;

	logo_display();

	if(argc != 3){
		cout << "Usage - DXFTOPOV filename.DXF filename.POV";
		exit(0);
	}
	else if(argc == 3){
		strcpy(DXFfile, argv[1]);
		strcpy(POVfile, argv[2]);
	}
	else{
		cout << "Command line error --- Terminating Execution" << '\n';
		cout << "Type DXFTOPOV for usage" << '\n';
	}

	//Open file streams DXF for input POV for output
	ifstream DXF(DXFfile);
	ofstream POV(POVfile);

	//set up format for POV file data
	POV << setiosflags(ios::fixed) << setw(8) << setprecision(6);

	//set up file header
	POV << "//POV-Ray script" << '\n';
	POV << "#include" << '"' << "colors.inc" << '"' << '\n' << '\n';
	POV << "#declare Cam_factor = 6" << '\n';
	POV << "#declare Camera_X = 1 * Cam_factor" << '\n';
	POV << "#declare Camera_Y = 0.5 * Cam_factor" << '\n';
	POV << "#declare Camera_Z = -0.3 * Cam_factor" << '\n';

	POV << "camera { location  <Camera_X, Camera_Y, Camera_Z>" << '\n';
	POV << "		up        <0, 1.0,  0>    right     <-1.33, 0,  0>" << '\n';
	POV << "		direction <0, 0,  3>      look_at   <0, 0, 0> }" << '\n' << '\n';

	POV << "light_source { <Camera_X - 2, Camera_Y + 5 , Camera_Z + 5> color White }" << '\n';
	POV << "light_source { <Camera_X - 2, Camera_Y + 5 , Camera_Z - 3> color White }" << '\n';

	POV << '\n' << "// Background:" << '\n';
	POV << "background {color Gray75}" << '\n' << '\n';

	POV << "// POV script created by DXFTOPOV: " << '\n' << '\n';

	POV << "union {" << '\n';

	//Get the chord data
	do{
		//get a line and see if says LINE or 3DFACE
		DXF >> DXF_line;
		face_value = strcmp(DXF_line, "3DFACE");
		line_value = strcmp(DXF_line, "LINE");

		if(!face_value){
			//found a face...now get 1st point
			do{
				line_ok = 1;
				//now look for starting X coordinate
				strcpy(last_line, DXF_line);
				DXF >> DXF_line;
				temp = strcmp(DXF_line, "10");
				temp1 = strcmp(last_line, "8");
				if(!temp && temp1){
					//found coordinates, get and store
					DXF >> sX;
					DXF >> DXF_line;
					DXF >> sY;
					DXF >> DXF_line;
					DXF >> sZ;
					DXF >> DXF_line;
					DXF >> eX;
					DXF >> DXF_line;
					DXF >> eY;
					DXF >> DXF_line;
					DXF >> eZ;
					DXF >> DXF_line;
					DXF >> cX;
					DXF >> DXF_line;
					DXF >> cY;
					DXF >> DXF_line;
					DXF >> cZ;
					line_ok = 0;
					//save data to POV script
					POV << "sphere{<" << sX << ", " << sY << ", " << sZ;
					POV << ">,0.04 pigment {Blue} no_shadow}" << '\n';

					POV << "sphere{<" << eX << ", " << eY << ", " << eZ;
					POV << ">,0.04 pigment {Blue} no_shadow}" << '\n';

					POV << "sphere{<" << cX << ", " << cY << ", " << cZ;
					POV << ">,0.04 pigment {Blue} no_shadow}" << '\n';

					POV << "cylinder{<" << sX << ", " << sY << ", " << sZ << ">, ";
					POV << "<" << eX << ", " << eY << ", " << eZ << ">,0.03 pigment {Red} no_shadow}" << '\n';

					POV << "cylinder{<" << sX << ", " << sY << ", " << sZ << ">, ";
					POV << "<" << cX << ", " << cY << ", " << cZ << ">,0.03 pigment {Red} no_shadow}" << '\n';

					POV << "cylinder{<" << cX << ", " << cY << ", " << cZ << ">, ";
					POV << "<" << eX << ", " << eY << ", " << eZ << ">,0.03 pigment {Red} no_shadow}" << '\n';

					POV << "triangle{<" << sX << ", " << sY << ", " << sZ << ">, " << '\n';
					POV << "         <" << eX << ", " << eY << ", " << eZ << ">, " << '\n';
					POV << "         <" << cX << ", " << cY << ", " << cZ << "> " << '\n';
					POV << "pigment {Blue} no_shadow}" << '\n';
				}
			}while(line_ok == 1);
		}
		else if(!line_value){
			//found a line...get the first point
			do{
				line_ok = 1;
				//now look for starting X coordinate
				strcpy(last_line, DXF_line);
				DXF >> DXF_line;
				temp = strcmp(DXF_line, "10");
				temp1 = strcmp(last_line, "8");
				if(!temp && temp1){
					//found coordinates, get and store
					DXF >> sX;
					DXF >> DXF_line;
					DXF >> sY;
					DXF >> DXF_line;
					DXF >> sZ;
					DXF >> DXF_line;
					DXF >> eX;
					DXF >> DXF_line;
					DXF >> eY;
					DXF >> DXF_line;
					DXF >> eZ;
					line_ok = 0;
					//save data to POV script
					POV << "sphere{<" << sX << ", " << sY << ", " << sZ;
					POV << ">,0.04 pigment {Blue} no_shadow}" << '\n';

					POV << "sphere{<" << eX << ", " << eY << ", " << eZ;
					POV << ">,0.04 pigment {Blue} no_shadow}" << '\n';

					POV << "cylinder{<" << sX << ", " << sY << ", " << sZ << ">, ";
					POV << "<" << eX << ", " << eY << ", " << eZ << ">,0.03 pigment {Red} no_shadow}" << '\n';
				}
			}while(line_ok == 1);
		}
	}while(!DXF.eof());	//keep going until there is no more data
	//add a rotation
	POV << "rotate <0, 0, 0> }" << '\n';

	//close the data files
	POV.close();
	DXF.close();

	cout << "Conversion complete!" << '\n';

	return(0);
}

