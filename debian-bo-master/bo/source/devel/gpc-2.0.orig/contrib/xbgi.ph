
{ BGI graphics interface module.

  This interface module defines the BGI interfaces to be used with the
  XBGI library written by Peter King <king@physics.su.oz.au>

  You should put

#include "xbgi.ph"

  or something like that in front of your pascal source code.

  Author: Juki <jtv@hut.fi>

  This has been used only to test two tiny programs, so it is not
  likely to be correct.....

  Please send me your improvements.
}

MODULE bgi Interface;

EXPORT BgiGraphics =
 	       (Arc,Bar,Bar3D,Circle,ClearDevice,ClearViewport,CloseGraph,
 		DetectGraph,DrawPoly,FillEllipse,FillPoly,FloodFill,
 		GetAspectRatio,GetbkColor,GetColor,GetFillPattern,GetImage,
 		GetMaxColor,GetMaxMode,GetMaxX,GetMaxY,GetModeName,
 		GetModeRange,GetPixel,GetX,GetY,GraphDefaults,
		GraphResult,ImageSize,InitGraph,Line,LineTo,MoveTo,OutText,
 		OutTextXY,PieSlice,PutImage,PutPixel,Rectangle,Sector,
 		SetActivePage,SetAspectRatio,SetbkColor,SetColor,
 		SetFillPattern,SetFillStyle,SetLineStyle,SetPalette,
 		SetTextJustify,SetTextStyle,SetUserCharSize,SetViewPort,
 		SetVisualPage,SetWriteMode,TextHeight,TextWidth,
 		GetArcCoords, GetFillSettings, GetLineSettings,
 		GetTextSettings, GetViewSettings, SetAllPalette,
  		GetCH,KbHit,KeyPressed,ReadKey);

TYPE
   str_param = PACKED ARRAY [ 1..255 ] OF Char;
   charptr = ^str_param;

   ptr_char = ^Char;
   ptr_int  = ^Integer;

#ifdef __GPC__
   u_int    =  __unsigned__ integer;
   longint  =  __long__     integer;
   u_long   =  __unsigned__ longint;
#else
   u_int    =  Integer; (* @@@ UNSIGNED INT *)
   u_long   =  Integer; (* @@@ UNSIGNED LONG *)
#endif { __GPC__ }

   Byte     =  0 .. 255;
   SignedByte = -128 .. 127;

   colours  = (Black,    Blue,  Green,  Cyan, Red,  Magenta,    Brown,
    	       LightGray, DarkGray,     LightBlue,  LightGreen, LightCyan,
	       LightRed,  LightMagenta, Yellow,     White);

TYPE
   Fill_Types = (Empty_Fill,     Solid_Fill,      Line_Fill,      Ltslash_Fill,
    		 Slash_Fill,     Bkslash_Fill,    Ltbkslash_Fill, Hatch_Fill,
    		 Xhatch_Fill,    Interleave_Fill, Wide_Dot_Fill,
    		 Close_Dot_Fill, User_Fill);

   Line_Types = (Solid_Line, Dotted_Line, Center_Line,
	         Dashed_Line, Userbit_Line);

   Widths     = (uw0,Norm_Width,uw2,Thick_Width);

  Lengths    = (ul0,ul1,DottedLine_Length,uw3,CentreLine_Length);

CONST
   Dashedline_Length = DottedLine_Length;

TYPE
   Fonts = (Default_Font, Triplex_Font, Small_Font,
 	    Sansserif_Font, Gothic_Font);

   Directions = (Horiz_Dir, Vert_Dir);

   Text_Justify = (Left_Text, Center_Text, Right_Text);

CONST
   Bottom_Text = Left_Text;
   Top_Text    = Right_Text;

TYPE
   puts	  =  (Copy_Put, Xor_Put, Or_Put, And_Put, Not_Put);

CONST
   Max_Pages = 4;

TYPE
  ArcCoordsType =  RECORD
		     x,
   		     y,
   		     xStart,
		     yStart,
		     xEnd,
		     yEnd   :  Integer;
		   END;

{ Date is reserved in Extended Pascal.
  Use GetTimeStamp to get the date & time }

   rdate = RECORD
	     da_year : Integer;
	     da_day  ,
	     da_mon : Byte;
	   END;	     

  FillSettingsType = RECORD
		       pattern,
		       color   : Integer;
		     END;

  LineSettingsType = RECORD
		       linestyle : integer;
		       upattern  : u_int;
		       thickness : integer;
		     END;

  PaletteType = RECORD
		  size    : Byte;
		  colours : PACKED ARRAY [ Colours ] OF SignedByte;
		END;

  TextSettingsType = RECORD
		       font,
		       direction,
		       charsize,
		       horiz,
		       vert :  Integer;
		     END;

  ViewPortType = RECORD
		   left,
		   top,
		   right,
		   bottom,
		   clip :	 Integer;
		 END;

  Borland_Info = RECORD
		   colour_index : Integer;
		   colour_name  : ptr_char;
		   pixel_values : u_long;
		 END;


  PROCEDURE arc (x,y,stangle,endangle,radius : integer); C;
  PROCEDURE bar (left,top,right,bottom: integer); C;
  PROCEDURE bar3d (left,top,right,bottom,depth,topflag: integer); C;
  PROCEDURE circle (x,y,radius: integer); C;
  PROCEDURE cleardevice; C;
  PROCEDURE clearviewport; C;
  PROCEDURE closegraph; C;
  PROCEDURE detectgraph (VAR driver, mode: integer); C;
  PROCEDURE drawpoly (numpoints: integer; polypoints: ptr_int); C;
  PROCEDURE fillellipse (a,b,c,d: integer); C;
  PROCEDURE fillpoly (numpoints: integer; polypoints: ptr_int); C;
  PROCEDURE floodfill (x,y,border: integer); C;
  PROCEDURE getaspectratio (VAR xasp, yasp: integer); C;
  FUNCTION  getbkcolor : integer; C;
  FUNCTION  getcolor : integer; C;
  PROCEDURE getfillsettings (VAR a : fillsettingstype); C;
  PROCEDURE getfillpattern (VAR a: str_param); C;
  PROCEDURE getimage (a,b,c,d : integer; e : ptr_char); C;
  FUNCTION  getmaxcolor : integer; C;
  FUNCTION  getmaxmode : integer; C;
  FUNCTION  getmaxx : integer; C;
  FUNCTION  getmaxy : integer; C;
  FUNCTION  getmodename (a: integer) : ptr_char; C;
  PROCEDURE getmoderange (a: integer; VAR b,c: integer); C;
  FUNCTION  getpixel (a, b: integer) : u_int; C;
  FUNCTION  getx : integer; C;
  FUNCTION  gety : integer; C;
  PROCEDURE graphdefaults; C;
  FUNCTION  graphresult : integer; C;
  FUNCTION  imagesize (a,b,c,d: integer) : u_int; C;
  PROCEDURE initgraph (VAR a,b: integer; VAR c: str_param); C;
  PROCEDURE line (a,b,c,d: integer); C;
  PROCEDURE lineto (a, b: integer); C;
  PROCEDURE moveto (a, b: integer); C;
  PROCEDURE outtext (VAR c: str_param); C;
  PROCEDURE outtextxy (x,y: integer; VAR c: str_param); C;
  PROCEDURE pieslice (a,b,c,d,e: integer); C;
  PROCEDURE putimage (a,b: integer; c: ptr_char; d: integer); C;
  PROCEDURE putpixel (a,b,c: integer); C;
  PROCEDURE rectangle (a,b,c,d: integer); C;
  PROCEDURE sector (a,b,c,d,e,f	: integer); C;
  PROCEDURE setactivepage (a: integer); C;
  PROCEDURE setaspectratio (a, b: integer); C;
  PROCEDURE setbkcolor (a: integer); C;
  PROCEDURE setcolor (a: integer); C;
  PROCEDURE setfillpattern (VAR	a: str_param; b: integer); C;
  PROCEDURE setfillstyle (a, b: integer); C;
  PROCEDURE setlinestyle (a: integer; b: u_int; c: integer); C;
  PROCEDURE setpalette (a, b: integer); C;
  PROCEDURE settextjustify (a, b: integer); C;
  PROCEDURE settextstyle (a,b,c: integer); C;
  PROCEDURE setusercharsize (a,b,c,d: integer); C;
  PROCEDURE setviewport (a,b,c,d,e: integer); C;
  PROCEDURE setvisualpage (a: integer); C;
  PROCEDURE setwritemode (a: integer); C;
  FUNCTION  textheight (VAR a: str_param) : integer; C;
  FUNCTION  textwidth (VAR a: str_param) : integer; C;

  PROCEDURE getarccoords (VAR a	: arccoordstype); C;
  PROCEDURE getlinesettings (VAR a : linesettingstype); C;
  PROCEDURE gettextsettings (VAR a : textsettingstype); C;
  PROCEDURE getviewsettings (VAR a : viewporttype); C;
  PROCEDURE setallpalette (VAR a : palettetype); C;

{-------------------------------------------------------------------------}

  FUNCTION  getch : integer; C;
  FUNCTION  kbhit : integer; C;

  { This should be done in a standard way in extended pascal.

    Anyway, the getdate routine is renamed to bgi_getdate(VAR a: rdate);
    because it clashes with time.h
  }
   PROCEDURE bgi_getdate (VAR a : rdate); C;

   { the following simple routines added to xbgi library by Juki }
   FUNCTION keypressed : integer; C;
   FUNCTION readkey : char; C;

   FUNCTION param_argv (i: integer); charptr; C;
   FUNCTION paramcount: integer; C;

{ Here is ParamStr function:
   
       Type
	     mystring = string(100);

	Function ParamStr(a : Integer) : mystring;
	begin
	   ParamStr := trim(param_argv (a)^);
	end;
}

end.
