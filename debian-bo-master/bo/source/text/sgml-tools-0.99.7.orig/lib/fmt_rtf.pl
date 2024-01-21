#
#  fmt_rtf.pl
#
#  $Id: fmt_rtf.pl,v 1.1 1997/02/04 22:48:32 cg Exp $
#
#  RTF-specific driver stuff
#
#  © Copyright 1996, Cees de Groot
#

$rtf = {};
$rtf->{NAME} = "rtf";
$rtf->{HELP} = "";
$rtf->{OPTIONS} = [
   { option => "twosplit", type => "f", short => "2" }
];
$rtf->{twosplit}  = 0;
$rtf->{preASP} = rtf_preASP;
$rtf->{postASP} = rtf_postASP;

$Formats{$rtf->{NAME}} = $rtf;

#
#  RTF does not treat newline as whitespace, so we need to
#  turn "\n" into " \n". Without the extra space, two words
#  separated only by a newline will get jammed together in
#  the RTF output.
#
sub rtf_preASP
{
  system "sed -f $LibDir/prertf.sed <$TmpBase.1 >$TmpBase.2";
}

#
#  Take the sgmlsasp output, and make something
#  useful from it.
#
sub rtf_postASP
{
  #
  #  Set various stuff as a result of option processing.
  #
  $split = "-2" if $rtf->{twosplit};

  `$BinDir/rtf2rtf $split $FileName <$TmpBase.3 >$FileName.rtf`;

  return 0;
}

1;
