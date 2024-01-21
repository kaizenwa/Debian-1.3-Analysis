#!/hgdv/bin/perl
# ice-idx.pl - create an index file // June 20 1994
# 
# ICE Version 1.05a
# (C) Christian Neuss, Fraunhofer IGD (neuss@igd.fhg.de) 

# Have this script called up on a regular base via 'cron'.
# Make sure that if for security reasons, it is being executed
# with a user id other then root, this user has
# both read access to the html files and write access to
# the index file.
# Here's an example of a crontab entry (crontab syntax varies
# between different platforms):
# 1 20 * * *  neuss /usr/httpd/ice-idx.pl >/dev/console 2>&1

#--- start of configuration --- put your changes here ---
# The physical directory/directories to scan for html-files.
# It's better to supply a tailing "/" for each directory,
# since otherwise automounting may not work.
# Example:
#  @SEARCHDIRS=("/usr/www/dir/","/tmp/html/","/usr/foo/html-dir/"); 
@SEARCHDIRS=( 
  "/hgdv/www",
);

# Name of the index file.
# Example:
#  $INDEXFILE="/usr/local/httpd/index.idx"
$INDEXFILE="/hgdv/www/.index/index.idx";

# Location of the "compress (1)" command:
#   Example: $compcmd="/usr/ucb/compress";
$compcmd="/usr/ucb/compress";

#--- end of configuration --- don't change anything below ---

open(INDEX,"| $compcmd >$INDEXFILE");

foreach $SEARCHDIR (@SEARCHDIRS){
print $SEARCHDIR;
  # chdir to work around find's problem with automount
  open(FILES,"cd $SEARCHDIR; find . -type f -name '*.html' -print |");
  while(<FILES>){
    chop;
		s/^./$SEARCHDIR/o;
    ### print "indexing $_\n";
    do &indexfile($_);
  }
  close(FILES);
}

sub indexfile{
  local($file)=@_;
  local(%freqlist,$title,$intitle);
	unless (-r $file && open(fpin,"$file")){ # file readable?
    print STDERR "cannot read file \"$file\"\n";
		return;
	}
	local($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
	      $atime,$mtime,%dontcare) = stat($file);
  while(<fpin>){
    s/([^\n])</\1\n</g; # add CR before every '<'
    s/>([^\n])/>\n\1/g; # add CR after every '>'
    s/<([^>]*)\n/<\1/g;
    #if($line++>40) {exit;}
    foreach (split(/\n/,$_)){
       if(m:<title>:i){
        $intitle="y";
        $title="";
      }
      if(m:</title>:i){
        $intitle="";
      }
      # if outside a tag
      if(!/</){
        # convert iso 8bit to html
        ###print "1. [$_]\n";
        $_ = &iso2html($_);
        ###print "2. [$_]\n";

        # dispose of unwanted chars
        tr/A-Za-z&;0-9/ /cs;
        if ($intitle){
          $title.="$_";
        } else {
          # dispose of unwanted chars
          tr/A-Za-z&;/ /cs;
          foreach (split(/ /,$_)){
            next unless ($_); # if empty skip
            if(/^[A-Z][^A-Z]*$/){  # "If" to "if"
              tr/A-Z/a-z/;
            }
            ###print "3. [$_]\n";
            $freqlist{$_}++;
          }
        }
      }
    }
  }
  $file =~ tr/\n/ /s;
  $title =~ tr/\n/ /s;
  print INDEX "@f $file\n";
  print INDEX "@t $title\n";
	print INDEX "@m $mtime\n";
  foreach $w (sort keys(%freqlist)){
    print INDEX "$freqlist{$w} $w\n";
    ###print "4. $freqlist{$w} $w\n";
  }
  %freqlist=();
  close(fpin);
}

# iso2html - translate iso 8 bit characters to HTML
#
# Thanks to
# Pierre Cormier (cormier.pierre@uqam.ca)
# Universite du Quebec Montreal
sub iso2html {
  local($input)=@_;
  unless($isohtml[0]){
    foreach (0..191) { $isohtml[$_] = pack("C",$_);}
    $isohtml[hex('c0')] = '&Agrave;';
    $isohtml[hex('c1')] = '&Aacute;';
    $isohtml[hex('c2')] = '&Acirc;';
    $isohtml[hex('c3')] = '&Atilde;';
    $isohtml[hex('c4')] = '&Auml;';
    $isohtml[hex('c5')] = '&Aring;';
    $isohtml[hex('c6')] = '&AElig;';
    $isohtml[hex('c7')] = '&Ccedil;';
    $isohtml[hex('c8')] = '&Egrave;';
    $isohtml[hex('c9')] = '&Eacute;';
    $isohtml[hex('ca')] = '&Ecirc;';
    $isohtml[hex('cb')] = '&Euml;';
    $isohtml[hex('cc')] = '&Igrave;';
    $isohtml[hex('cd')] = '&Iacute;';
    $isohtml[hex('ce')] = '&Icirc;';
    $isohtml[hex('cf')] = '&Iuml;';
    $isohtml[hex('d0')] = '&ETH;';
    $isohtml[hex('d1')] = '&Ntilde;';
    $isohtml[hex('d2')] = '&Ograve;';
    $isohtml[hex('d3')] = '&Oacute;';
    $isohtml[hex('d4')] = '&Ocirc;';
    $isohtml[hex('d5')] = '&Otilde;';
    $isohtml[hex('d6')] = '&Ouml;';
    $isohtml[hex('d7')] = '&times;';
    $isohtml[hex('d8')] = '&Ostroke;';
    $isohtml[hex('d9')] = '&Ugrave;';
    $isohtml[hex('da')] = '&Uacute;';
    $isohtml[hex('db')] = '&Ucirc;';
    $isohtml[hex('dc')] = '&Uuml;';
    $isohtml[hex('dd')] = '&Yacute;';
    $isohtml[hex('de')] = '&THORN;';
    $isohtml[hex('df')] = '&szlig;';
    $isohtml[hex('e0')] = '&agrave;';
    $isohtml[hex('e1')] = '&aacute;';
    $isohtml[hex('e2')] = '&acirc;';
    $isohtml[hex('e3')] = '&atilde;';
    $isohtml[hex('e4')] = '&auml;';
    $isohtml[hex('e5')] = '&aring;';
    $isohtml[hex('e6')] = '&aelig;';
    $isohtml[hex('e7')] = '&ccedil;';
    $isohtml[hex('e8')] = '&egrave;';
    $isohtml[hex('e9')] = '&eacute;';
    $isohtml[hex('ea')] = '&ecirc;';
    $isohtml[hex('eb')] = '&euml;';
    $isohtml[hex('ec')] = '&igrave;';
    $isohtml[hex('ed')] = '&iacute;';
    $isohtml[hex('ee')] = '&icirc;';
    $isohtml[hex('ef')] = '&iuml;';
    $isohtml[hex('f0')] = '&eth;';
    $isohtml[hex('f1')] = '&ntilde;';
    $isohtml[hex('f2')] = '&ograve;';
    $isohtml[hex('f3')] = '&oacute;';
    $isohtml[hex('f4')] = '&ocirc;';
    $isohtml[hex('f5')] = '&otilde;';
    $isohtml[hex('f6')] = '&ouml;';
    $isohtml[hex('f7')] = '&DIVIS;';
    $isohtml[hex('f8')] = '&ostroke;';
    $isohtml[hex('f9')] = '&ugrave;';
    $isohtml[hex('fa')] = '&uacute;';
    $isohtml[hex('fb')] = '&ucirc;';
    $isohtml[hex('fc')] = '&uuml;';
    $isohtml[hex('fd')] = '&yacute;';
    $isohtml[hex('fe')] = '&thorn;';
    $isohtml[hex('ff')] = '&yuml;';
	}
	
	local(@car) = split(//,$input);
	local($output);
	foreach (@car) {
	  $output .= $isohtml[ord($_)];
	}
	$output;
}