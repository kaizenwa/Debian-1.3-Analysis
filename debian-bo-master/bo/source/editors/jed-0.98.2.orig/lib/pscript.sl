% This is a simple PostScript mode. It just implements a highlighting
% scheme.

% Note that PostScript considers any set of matched parentheses to be a
% valid string. This cannot easily be matched by DFA-based methods, so
% the highlighting scheme here simply deals with *one* set of inner
% parentheses. Hence (asd(df)df) is a valid string under these rules,
% but (sdf(df(sdf)sdf)sdf) contains more nested parentheses than we can
% cope with here.

$1 = "PostScript";

create_syntax_table ($1);

#ifdef HAS_DFA_SYNTAX
enable_highlight_cache ("pscript.dfa", $1);
define_highlight_rule("%.*$", "comment", $1);
define_highlight_rule("[\\-\\+]?[0-9]*\\.?[0-9]+([Ee][\\-\\+]?[0-9]*)?",
		      "number", $1);
define_highlight_rule("[0-9]+#[0-9A-Za-z]*", "number", $1);
define_highlight_rule(strcat("\\((\\\\.|[^\\(\\)\\\\]|",
			     "\\(([^\\\\\\(\\)]|\\\\.)*\\))*\\)"),
		      "string", $1);
define_highlight_rule(strcat("\\((\\\\.|[^\\(\\)\\\\]|\\(([^\\\\\\(\\)]|\\\\",
			     ".)*\\))*(\\(([^\\\\\\(\\)]|\\\\.)*)?\\\\?$"),
		      "string", $1);
define_highlight_rule(strcat("^(([^\\\\\\(\\)]|\\\\.)*\\))?(\\\\.|[^\\(\\)",
			     "\\\\]|\\(([^\\\\\\(\\)]|\\\\.)*\\))*\\)"),
		      "string", $1);
define_highlight_rule("<[ \t0-9a-fA-F]*>", "string", $1);
define_highlight_rule("<[ \t0-9a-fA-F]*$", "string", $1);
define_highlight_rule("^[ \t0-9a-fA-F]*>", "Qstring", $1);
define_highlight_rule("<~[ \t!-u]*~>", "string", $1);
define_highlight_rule("<~[ \t!-u]*~?$", "string", $1);
define_highlight_rule("^[ \t!-u]*~>", "Qstring", $1);
define_highlight_rule("[!-\\$&'\\*-\\.0-;=\\?-Z\\\\\\^-z\\|~]+",
		      "Knormal", $1);
define_highlight_rule("//?[!-\\$&'\\*-\\.0-;=\\?-Z\\\\\\^-z\\|~]+",
		      "normal", $1);
define_highlight_rule("<<|>>|\\[|\\]|{|}", "Qdelimiter", $1);
define_highlight_rule(".", "normal", $1);
build_highlight_table ($1);
#endif

% All words in PostScript are of course redefinable. So we will simply
% define as keywords the *really* basic ones. These are basic language
% operators.
define_keywords ($1, "=", 1);
define_keywords ($1, "==eqgegtiflelnltneor", 2);
define_keywords ($1, strcat("absaddandcoscvicvncvrcvscvxdefdivdupendexpfor",
			    "getlogmodmulnegnotpopputrunsinsrtsubxor"), 3);
define_keywords ($1, strcat("atancopycopycvrsdictexchexecexitfileidivload",
			    "loopmarkquitrandreadrollsavestoptruetype"), 4);
define_keywords ($1, strcat("aloadarraybeginclearcountcvlitfalsefloorindex",
			    "knownroundrrandsrandstackstoreundefwherewrite"),
		 5);
define_keywords ($1, "astoreforallgstateifelselengthpstackrepeatstring", 6);
define_keywords ($1, "ceilingrestore", 7);
define_keywords ($1, "truncate", 8);
define_keywords ($1, strcat("counttomarkcurrentfilegetintervalpackedarray",
			    "putinterval"), 11);

% And these are basic graphical operators.
define_keywords_n ($1, "arc", 3, 1);
define_keywords_n ($1, "arcnarctclipfillshow", 4, 1);
define_keywords_n ($1, "arctogsaveimagescale", 5, 1);
define_keywords_n ($1, "concateoclipeofilllinetomatrixmovetorotatestroke",
		   6, 1);
define_keywords_n ($1, "curvetonewpathrlinetormovetosetdashsetfontsetgray",
		   7, 1);
define_keywords_n ($1, "findfontgrestoremakefontrcurvetosetcolorshowpage",
		   8, 1);
define_keywords_n ($1, "closepathscalefontsetmatrixtransformtranslate", 9, 1);
define_keywords_n ($1, "definefontsetlinecapsetpattern", 10, 1);
define_keywords_n ($1, strcat("currentdashcurrentfontcurrentgraysethsbcolor",
			    "setlinejoinsetrgbcolor"), 11, 1);
define_keywords_n ($1, "currentcolorsetcmykcolorsetlinewidth", 12, 1);
define_keywords_n ($1, "currentmatrix", 13, 1);
define_keywords_n ($1, "currentlinecapcurrentpattern", 14, 1);
define_keywords_n ($1, "currenthsbcolorcurrentlinejoincurrentrgbcolor",
		   15, 1);
define_keywords_n ($1, "currentcmykcolorcurrentlinewidth", 16, 1);

define ps_mode ()
{
   variable ps = "PostScript";
   set_mode(ps, 0);
   use_syntax_table (ps);
   runhooks("ps_mode_hook");
}
