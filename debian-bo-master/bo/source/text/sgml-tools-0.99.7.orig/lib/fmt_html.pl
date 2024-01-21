#
#  fmt_html.pl
#
#  $Id: fmt_html.pl,v 1.6 1997/03/21 09:18:21 cg Exp $
#
#  HTML-specific driver stuff
#
#  © Copyright 1996, Cees de Groot
#

require "$LibDir/fixref.pl";
$fixref = $fixref::fixref;

require "$LibDir/html2html.pl";
$html2html = $html2html::html2html;

$html = {};
$html->{NAME} = "html";
$html->{HELP} = "";
$html->{OPTIONS} = [
   { option => "split", type => "l", 
     'values' => [ "0", "1", "2" ], short => "s" },
   { option => "dosnames", type => "f", short => "D" },
   { option => "imagebuttons", type => "f", short => "i"}
];
$html->{'split'}  = 1;
$html->{dosnames}  = 0;
$html->{imagebuttons}  = 0;
$html->{preNSGMLS} = << 'EOF';
  $NsgmlsOpts .= " -ifmthtml ";
EOF
$html->{postASP} = html_postASP;

$Formats{$html->{NAME}} = $html;

#
#  Take the sgmlsasp output, and make something
#  useful from it.
#
sub html_postASP
{
  #
  #  Set various stuff as a result of option processing.
  #
  $ext   = "html";
  $ext   = "htm"  if $html->{dosnames};
  $lang  = "$LibDir/rep/html/$global->{language}";
  $img   = 0;
  $img   = 1 if $html->{imagebuttons};

  #
  # Bring in file
  #
  open FILE, "<$TmpBase.3" or die "Cannot open $TmpBase.3";
  @file = <FILE>;
  close FILE;

  #
  #  Find references
  #
  &{$fixref->{init}}($html->{'split'});
  LINE: foreach (@file) {
      foreach $pat (keys %{$fixref->{rules}}) {
          if (/$pat/) {
              # Call rule function then skip to next line
              &{$fixref->{rules}->{$pat}}; next LINE;
          }
      }
      &{$fixref->{defaultrule}};
  }
  &{$fixref->{finish}};

  #  
  #  Run through html2html, preserving stdout
  #  Also, handle prehtml.sed's tasks
  #
  open SAVEOUT, ">&STDOUT";
  open STDOUT, ">$FileName.$ext" or die qq(Cannot open "$FileName.$ext");

  &{$html2html->{init}}($html->{'split'}, $lang, $ext, $img, $FileName,
                        $fixref->{filenum}, $fixref->{lrec});
  LINE: foreach (@file) {
      if (/<PRE>/ .. /<\/PRE>/) {
          s/"/\&quot;/g;		# change " to &quot;
          s/</\&lt;/g;			# change < to &lt;
          s,\&lt;([/]*)PRE>,<$1PRE>,g;	#  except in "</*PRE>"
          s/>/\&gt;/g;			# change > to &gt;
          s,<([/]*)PRE\&gt;,<$1PRE>,g;	#  except in "</*PRE>"
      }
      s,<P></P>,,g; 			# remove empty <P></P> containers
      foreach $pat (keys %{$html2html->{rules}}) {
          if (/$pat/) {
              # Call rule function then skip to next line
              &{$html2html->{rules}->{$pat}}; next LINE;
          }
      }
      &{$html2html->{defaultrule}};
  }
  &{$html2html->{finish}};

  close STDOUT;
  open STDOUT, ">&SAVEOUT";

  return 0;
}

1;

