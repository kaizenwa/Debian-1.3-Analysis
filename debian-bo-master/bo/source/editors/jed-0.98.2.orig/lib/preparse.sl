variable Preprocess_Only = 1;
evalfile ("bytecomp.sl"); pop ();

#ifdef HAS_DFA_SYNTAX

flush ("creating DFA syntax tables...");
% List of modes for which dfa cache tables should be constructed

() = evalfile ("cmode.sl");
() = evalfile ("slmode.sl");
() = evalfile ("javamode.sl");
() = evalfile ("html.sl");
() = evalfile ("perl.sl");
() = evalfile ("pscript.sl");
() = evalfile ("shmode.sl");
() = evalfile ("texcom.sl");
() = evalfile ("tpascal.sl");

#endif

exit_jed();
