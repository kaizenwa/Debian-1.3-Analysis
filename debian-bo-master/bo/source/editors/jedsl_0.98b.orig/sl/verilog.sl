$1 = "VERILOG";

create_syntax_table ($1);
define_syntax ("//","",'%', $1);
define_syntax ("([{", ")]}", '(', $1);
define_syntax ('"', '"', $1);
%define_syntax ('\'', '\'', $1);
define_syntax ('\$', '\\', $1);
define_syntax ('\`', '\\', $1);
define_syntax ("0-9a-zA-Z_", 'w', $1);        % words
define_syntax ("-+0-9a-FA-F.xXL", '0', $1);   % Numbers
define_syntax (",;.?:=<>", ',', $1);
define_syntax ('#', '#', $1);
define_syntax ("%-+/&*<>|!~^", '+', $1);
set_syntax_flags ($1, 8);

() = define_keywords ($1, "IFINISOFTOifinisofto", 2);
() = define_keywords ($1, "ANDENDFORMAXMINOUTUSEandendformaxminoutreguse", 3);
() = define_keywords ($1, "ELSELOOPPORTTHENelseloopportthenwire", 4);
() = define_keywords ($1, "BEGINELSIFbeginelsifinput", 5);
() = define_keywords ($1, "BUFFERDOWNTOENTITYMODULERETURNSIGNALbufferdowntoentitymoduleoutputreturnsignal", 6);
() = define_keywords ($1, "GENERICLIBRARYPACKAGEPROCESSgenericinitiallibrarypackageprocessspecify", 7);
() = define_keywords ($1, "CONSTANTFUNCTION", 8);
() = define_keywords ($1, "COMPONENTcomponentendmoduleparameterspecparam", 9);
() = define_keywords ($1, "endspecify", 10);
() = define_keywords ($1, "ARCHITECTUREarchitecture", 12);
() = define_keywords ($1, "CONFIGURATIONconfiguration", 13);

define verilog_mode ()
{
   variable kmap = "VERILOG";

   set_mode(kmap, 0x28);
   use_syntax_table (kmap);
   runhooks("verilog_mode_hook");
}
