% This is a simple vhdl mode.  It does not defined any form of indentation
% style.  Rather, it simply implements a highlighting scheme.

$1 = "VHDL";

create_syntax_table ($1);
define_syntax ("--","",'%', $1);
define_syntax ("([{", ")]}", '(', $1);
define_syntax ('"', '"', $1);
%define_syntax ('\'', '\'', $1);
define_syntax ('\'', '\\', $1);
define_syntax ("0-9a-zA-Z_", 'w', $1);        % words
define_syntax ("-+0-9a-FA-F.xXL", '0', $1);   % Numbers
define_syntax (",;.?:=<>", ',', $1);
define_syntax ('#', '#', $1);
define_syntax ("%-+/&*<>|!~^", '+', $1);
set_syntax_flags ($1, 8);

() = define_keywords ($1, "IFINISOFORTOifinisoforto", 2);
() = define_keywords ($1, "ANDENDFORMAXMINOUTRUNUSEandendformaxminoutrunuse", 3);
() = define_keywords ($1, "CASEELSELOOPPORTTHENTYPEWHENcaseelseloopportthentypewhen", 4);
() = define_keywords ($1, "BEGINELSIFRANGETRACEbeginelsifrangetrace", 5);
() = define_keywords ($1, "ASSIGNBUFFERDOWNTOENTITYOTHERSRETURNSIGNALassignbufferdowntoentityothersreturnsignal", 6);
() = define_keywords ($1, "GENERICINTEGERLIBRARYPACKAGEPROCESSSUBTYPEgenericintegerlibrarypackageprocesssubtype", 7);
() = define_keywords ($1, "CONSTANTFUNCTIONVARIABLEconstantfunctionvariable", 8);
() = define_keywords ($1, "COMPONENTcomponent", 9);
() = define_keywords ($1, "ARCHITECTUREarchitecture", 12);
() = define_keywords ($1, "CONFIGURATIONconfiguration", 13);

define vhdl_mode ()
{
   variable s = "VHDL";
   set_mode(s, 0x28);
   use_syntax_table (s);
   runhooks("vhdl_mode_hook");
}
