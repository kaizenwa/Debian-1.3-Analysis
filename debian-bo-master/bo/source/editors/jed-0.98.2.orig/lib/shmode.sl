% This is a simple shell mode.  It does not defined any form of indentation
% style.  Rather, it simply implements a highlighting scheme.

$1 = "SH";

create_syntax_table ($1);
define_syntax ("#", "", '%', $1);
define_syntax ("([{", ")]}", '(', $1);
define_syntax ('"', '"', $1);
define_syntax ('\'', '\'', $1);
define_syntax ('\\', '\\', $1);
define_syntax ("-0-9a-zA-Z_", 'w', $1);        % words
define_syntax ("-+0-9", '0', $1);   % Numbers
define_syntax (",;:", ',', $1);
define_syntax ("%-+/&*=<>|!~^", '+', $1);

#ifdef HAS_DFA_SYNTAX
enable_highlight_cache ("shmode.dfa", $1);
define_highlight_rule ("\\\\.", "normal", $1);
define_highlight_rule ("#.*$", "comment", $1);
define_highlight_rule ("\"([^\\\\\"]|\\\\.)*\"", "string", $1);
define_highlight_rule ("\"([^\\\\\"]|\\\\.)*$", "string", $1);
define_highlight_rule ("'[^']*'", "string", $1);
define_highlight_rule ("'[^']*$", "string", $1);
define_highlight_rule ("[\\|&;\\(\\)<>]", "Qdelimiter", $1);
define_highlight_rule ("[\\[\\]\\*\\?]", "Qoperator", $1);
define_highlight_rule ("[^ \t\"'\\\\\\|&;\\(\\)<>\\[\\]\\*\\?]+",
                       "Knormal", $1);
define_highlight_rule (".", "normal", $1);
build_highlight_table ($1);
#endif

() = define_keywords ($1, "dofiifin", 2);
() = define_keywords ($1, "forset", 3);
() = define_keywords ($1, "casedoneechoelseesacexitifeqtestthen", 4);
() = define_keywords ($1, "aliasendifendswifdefifneqshiftunsetwhile", 5);
() = define_keywords ($1, "ifndefsetenvsourceswitch", 6);
() = define_keywords ($1, "breaksw", 7);

define sh_mode ()
{
   set_mode("SH", 0);
   use_syntax_table ("SH");
   runhooks("sh_mode_hook");
}

