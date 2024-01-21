#
#  fmt_lyx.pl
#
#  $Id: fmt_lyx.pl,v 1.2 1997/03/21 09:40:57 cg Exp $
#
#  Lyx-specific driver stuff
#
#  © Copyright 1996, Cees de Groot
#

$lyx = {};
$lyx->{NAME} = "lyx";
$lyx->{HELP} = "";
$lyx->{OPTIONS} = [
];
$lyx->{preNSGMLS} = << 'EOF';
  $NsgmlsOpts .= " -ifmtlyx ";
  $NsgmlsPrePipe = "awk '{print \$0 \" \"}' $file ";
EOF
$lyx->{preASP} = lyx_preASP;
$lyx->{postASP} = lyx_postASP;

$Formats{$lyx->{NAME}} = $lyx;


#
#  Take the nsgmls output, and prepare it a bit
#
sub lyx_preASP
{
  open BASE1, "$TmpBase.1";
  open BASE2, ">$TmpBase.2";
  while (<BASE1>)
    {
      s/\\\\/\\\\backslash /g;
      print BASE2;
    }
  close BASE1;
  close BASE2;
}


#
#  Take the sgmlsasp output, and make something
#  useful from it.
#
sub lyx_postASP
{
  system <<"EOF";
cat $TmpBase.3 |
  gawk -f $LibDir/prelyx.awk |
  sed -f $LibDir/prelyx.sed |
  gawk -f $LibDir/purifylyx.awk |
  gawk -f $LibDir/postlyx.awk |
  gawk -f $LibDir/purifylyx.awk >$FileName.lyx
EOF
}

1;
