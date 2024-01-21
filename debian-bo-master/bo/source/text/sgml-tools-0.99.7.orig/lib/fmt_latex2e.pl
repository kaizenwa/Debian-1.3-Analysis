#
#  fmt_latex2e.pl
#
#  $Id: fmt_latex2e.pl,v 1.3 1997/03/21 09:18:21 cg Exp $
#
#  LaTeX-specific driver stuff
#
#  © Copyright 1996, Cees de Groot
#

$latex2e = {};
$latex2e->{NAME} = "latex2e";
$latex2e->{HELP} = <<EOF;
  Note that this output format requires LaTeX 2e.

EOF
$latex2e->{OPTIONS} = [
   { option => "output", type => "l", 
     'values' => [ "dvi", "tex", "ps" ], short => "o" },
   { option => "bibtex", type => "f",  short => "b" },
   { option => "quick",  type => "f",  short => "q" }
];
$latex2e->{output} = "dvi";
$latex2e->{quick}  = 0;
$latex2e->{bibtex}  = 0;
$latex2e->{preNSGMLS} = << 'EOF';
  $NsgmlsOpts .= " -ifmttex ";
EOF
$latex2e->{postASP} = latex2e_postASP;

$Formats{$latex2e->{NAME}} = $latex2e;

#
#  Take the sgmlsasp output, and make something
#  useful from it.
#
sub latex2e_postASP
{
  $ENV{TEXINPUTS} .= ":$LibDir";

  #
  #  Set the correct \documentclass options. The if statement is just
  #  a small optimization.
  #
  if ($global->{language} ne "en" ||
      $global->{papersize} ne "a4")
    {
      $replace = '\documentclass[' . $global->{papersize} . 'paper,' . 
	Lang::ISO2English ($global->{language}) . ']';
      open LTXFILE, "$TmpBase.3";
      open OUTFILE, "$FileName.tex";
      while (<LTXFILE>)
        {
	  s/\\documentclass\[a4paper,english\]/$replace/;
	  print OUTFILE;
	  print
	}
      close LTXFILE;
      close OUTFILE;
    }
  else
    {
      `mv $TmpBase.3 $FileName.tex`;
    }

  #
  #  LaTeX, dvips, and assorted cleanups.
  #
  system "latex $FileName.tex" || die "LaTeX problem";
  $latex2e->{bibtex} && system "bibtex $FileName";
  $latex2e->{quick} || system "latex $FileName.tex";
  $latex2e->{quick} || system "latex $FileName.tex";
  if ($global->{debug} == 0)
    {
      @suffixes = qw(log blg aux toc lof lot dlog bbl);
      for $suf (@suffixes)
        {
          unlink "$FileName.$suf";
        }
    }
  if ($latex2e->{output} eq "tex")
    { 
      unlink "$FileName.dvi"; 
      return 0; 
    }
  if ($latex2e->{output} eq "dvi")
    {
      $global->{debug} || unlink "$FileName.tex";
      return 0;
    }
  `dvips -q -t $global->{papersize} -o $FileName.ps $FileName.dvi`;
  $global->{debug} || `rm $FileName.dvi`;

  return 0;
}

1;
