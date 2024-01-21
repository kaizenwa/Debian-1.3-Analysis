#
#  fmt_info.pl
#
#  $Id: fmt_info.pl,v 1.1 1997/02/04 22:48:30 cg Exp $
#
#  GNU Info-specific driver stuff
#
#  © Copyright 1996, Cees de Groot
#

$info = {};
$info->{NAME} = "info";
$info->{HELP} = "";
$info->{OPTIONS} = [
];
$info->{preNSGMLS} = << 'EOF';
  $NsgmlsOpts .= " -ifmtinfo ";
  $NsgmlsPrePipe = "sed 's/\@/\@\@/g' $file";
EOF
$info->{postASP} = info_postASP;

$Formats{$info->{NAME}} = $info;

#
#  Take the sgmlsasp output, and make something
#  useful from it.
#
sub info_postASP
{
  system <<"EOF";
gawk -v INFO=$FileName.info -f $LibDir/info.awk -- $TmpBase.3 >$TmpBase.4
makeinfo $TmpBase.4 -o $FileName.info
EOF

}

1;
