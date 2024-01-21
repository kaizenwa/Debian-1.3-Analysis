#
#  fmt_txt.pl
#
#  $Id: fmt_txt.pl,v 1.8 1997/03/21 09:18:22 cg Exp $
#
#  TXT-specific driver stuff
#
#  © Copyright 1996, Cees de Groot
#

$txt = {};
$txt->{NAME} = "txt";
$txt->{HELP} = "";
$txt->{OPTIONS} = [
   { option => "manpage", type => "f", short => "m" },
   { option => "filter",  type => "f", short => "f" }
];
$txt->{manpage} = 0;
$txt->{filter}  = 0;
$txt->{preNSGMLS} = txt_preNSGMLS;
$txt->{preASP} = txt_preASP;
$txt->{postASP} = txt_postASP;

$Formats{$txt->{NAME}} = $txt;

#
#  Set correct NsgmlsOpts
#
sub txt_preNSGMLS
{
  if ($txt->{manpage})
    {
      $NsgmlsOpts .= " -iman ";
      $global->{charset} = "man";
    }
  else
    {
      $NsgmlsOpts .= " -ifmttxt ";
      $global->{charset} = "latin1" if $global->{charset} eq "latin";
    }

  #
  #  Is there a cleaner solution than this? Can't do it earlier,
  #  would show up in the help messages...
  #
  $theFormat = $global->{charset};
  $theFormat = "groff" if $theFormat eq "ascii";
  $ENV{SGML_SEARCH_PATH} =~ s/txt/$theFormat/;

  $Formats{"groff"} = $txt;
  $Formats{"latin1"} = $txt;
  $Formats{"man"} = $txt;
}


#
#  Run the file through the genertoc utility before sgmlsasp. Not necessary
#  when producing a manpage. A lot of code from FJM, untested by me.
#
sub txt_preASP
{
  $txt->{manpage} && return;
  
  open INFILE, "$TmpBase.1";
  open OUTFILE, "|$BinDir/genertoc >$TmpBase.2";
  while (<INFILE>)
    {
      print OUTFILE $_;
      if (/^\(HEADING/) 
        {
	  my $tipo = '';
          while(1) 
            {
	      $_ = <INFILE>;
	      next if (/^\)TT/ || /^\(TT/ || /^\)IT/ || /^\(IT/ ||
                       /^\)EM/ || /^\(EM/ || /^\)BF/ || /^\(BF/);
	      if (/^-/) 
                {
		  chop;
		  s/^-// if $tipo;
		  $tipo .=  $_ ;
		  $tipo .= " " unless $tipo =~ / $/;
		}
	      else 
                {
	          $tipo =~ s/ $//;
		  print OUTFILE "$tipo\n" if $tipo ;
		  print OUTFILE $_;
		  $tipo = '';
	        }
	      last if (/^\)HEADING/);
            }
        } 
    }
  close INFILE;
  close OUTFILE;
}


#
#  Take the sgmlsasp output, and make something
#  useful from it.
#
sub txt_postASP
{
  if ($txt->{manpage})
    {
      `sed -f $LibDir/preroff.sed <$TmpBase.3 >$FileName.man`;
    }
  else
    {
      `sed -f $LibDir/preroff.sed <$TmpBase.3 | 
        $progs->{GROFF} -T $global->{charset} -t -ms | cat -s >$TmpBase.4`;
      if ($txt->{filter})
        {
	  #
	  #  Strip ^H stuff. It seems that col junks ISO-8859-1 stuff,
	  #  so a manual solution for that one.
	  #
	  if ($global->{charset} eq "ascii")
	    {
	      `cat $TmpBase.4 | col -bx >$FileName.txt`;
	    }
	  else
	    {
	      `cat $TmpBase.4 | 
	         sed -e 's/\(.\)\1/\1/g' -e 's/_\(.\)/\1/g' >$FileName.txt`; 
	    }
	}
      else
        {
	  `cp $TmpBase.4 $FileName.txt`;
	}
    }

  return 0;
}

1;
