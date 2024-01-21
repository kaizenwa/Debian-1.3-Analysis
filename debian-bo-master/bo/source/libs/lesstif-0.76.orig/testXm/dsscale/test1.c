

/************************************************************************* 
 * Version 1.3  on  20.2.1996
 * (c) 1996 Pralay Kanti Dakua
 *     pralay@teil.soft.net      
 *     
 * Credits
 * -------
 * The keyboard translations are added by Andreas Zeller (zeller@comsoft.de).
 * -------
 *
 * This program is free software; permission hereby granted, free of
 * charge, to any person obtaining a copy of this software and 
 * associated documentations to copy, modify and  distribute it under 
 * the terms of the GNU General Public License as published by 
 * the Free Software Foundation, subject to the following conditions: 

 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.

 * AS THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
 * FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
 * OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
 * PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
 * OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
 * TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU,  SHOULD THE
 * PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
 * REPAIR OR CORRECTION.
 *
 **************************************************************************/


#include "DSScale.h"


int main(int argc,char *argv[])
{
Widget topLevel,newid;


topLevel=XtInitialize(argv[0],"DS",NULL,0,&argc,argv);


newid=XtVaCreateManagedWidget("new",xmDoubleSliderScaleWidgetClass,topLevel,
XmNwidth,200,
XmNheight,50,
NULL);


XtRealizeWidget(topLevel);
XtMainLoop();
exit(0);
}

