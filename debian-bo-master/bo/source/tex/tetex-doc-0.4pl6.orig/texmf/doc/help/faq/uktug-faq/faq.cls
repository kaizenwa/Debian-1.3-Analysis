% simple class to format the UKTUG FAQ in two columns

\NeedsTeXFormat{LaTeX2e}[1994/06/01]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions

\LoadClass{article}

%  ****************************************
%  *            PAGE LAYOUT               *
%  ****************************************
%
% (This stuff is hacked from SPQR (et al) in baskerv.cls)
%
% SIDE MARGINS:
\oddsidemargin -2.5pc \evensidemargin -2.5pc
\marginparwidth      4pc       % don't use marginal notes...
\marginparsep      0.5pc       % ...in the UK TUG newsletter

% VERTICAL SPACING:
\topmargin  -0.5in             % allow half an inch border
\headheight 0\p@               % we don't bother with headers here ...
\headsep    0\p@               % ... this ain't a publication
\topskip    10\p@
\footskip   15\p@

% DIMENSION OF TEXT:

\textheight        \paperheight
\advance\textheight -1.5in
%\textheight       250mm       % height of text on a page (A4 paper)

\textwidth         \paperwidth
\advance\textwidth -1in
%\textwidth        180mm       % total width of a page    (A4 paper)

\columnseprule     0.5\p@      % width of line in the inter-column gutter
\columnsep         10mm        % space between columns
\tolerance         9999        % make those columns justify

% FOOTNOTES:
\footnotesep 6\p@
\skip\footins 19.5\p@ plus 12\p@ \@minus 1\p@

\RequirePackage{faq,multicol}
