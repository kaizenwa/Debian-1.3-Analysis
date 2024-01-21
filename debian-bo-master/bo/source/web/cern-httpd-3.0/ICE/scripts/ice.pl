#!/hgdv/bin/perl
#
# ice.pl -- ICE search basic routines // Jul 3 1994
#
# ICE Version 1.05a
# (C) Christian Neuss, Fraunhofer IGD (neuss@igd.fhg.de) 

#--- start of configuration --- put your changes here ---
# Location of the indexfile:
#   Example: $indexfile="/usr/local/etc/httpd/index/index.idx";
#$indexfile="/usr/local/etc/httpd/index/index.idx";
###$indexfile="/hgdv/www/.index/index.idx.Z";
$indexfile="/hgdv/www/.index/index.idx";

# Location of the "egrep (1)" command:
#   Example: $grepcmd="/usr/bin/egrep";
$grepcmd="/usr/bin/egrep";

# Location of the "zcat (1)" command:
#   Example: $zcatcmd="/usr/ucb/zcat";
$zcatcmd="/usr/ucb/zcat";

# Location of the thesaurus data file:
#   Example: $thesfile="/igd/a3/home1/neuss/Perl/thes.dat";
$thesfile="/igd/a3/home1/neuss/Perl/thes.dat";

# URL Mappings (a.k.a Aliases) that your server does
# map "/" to some path to reflect a "document root"
#   Example
#   %urltopath = (
#   '/mms',   '/usr/stud/glaser/mms', 
#   '/',      '/usr/www',
#   );
#
%urltopath = (
  '/',         '/hgdv/www',
);
#--- end of configuration --- don't change anything below ---


#$orig="pic[rut]*e or {foobar} or image -L -T @ /icib";
#$orig="fridelzupf or test @ /";
#$foo=&getquery($orig);
#print $foo;
#exit;
#print &getquery("the -D 7 -S @ /");
#exit;

sub getquery{
  local($query)=@_;
  local($page)="<UL>\n";
  local($hitcount);
$query = &iso2html($query);
  local($err,$context,$thes,$substr,$bool,$days,@ret)=&parsequery($query);
  if($err){
    return $err; #XXX hm..
  }

  local(@tmplist)= &getindex($thes,$bool,$days,$substr,@querystring);
  
  if($context){
    # translate virtual<->physical path
    local($v,$p)=($context,&translate($context));
    @tmplist=grep(/^[^\n]*\n$p/,@tmplist);
    ###print "translate context from \n$v to \n$p\n";
  }
  
  for $w (@tmplist){
    local($freq,$file,$title,@hits)=split(/\n/,$w);
    local($virt)=&translateback($file);
    ###print "translate docpath from \n$file to \n$virt\n";
    $w=join("\n",$freq,$virt,$title,@hits);
  }
  
  sub downbynumber {$b <=> $a;}
  @tmplist = sort downbynumber @tmplist;
  
  foreach $w (@tmplist){
    local($freq,$file,$title,@hits)=split(/\n/,$w);
    $hitcount++;
    ###print "$freq,$title\n";
    unless($title) { $title="(NO TITLE)"; } 
    $page .= "<LI> <A HREF=\"$file\"><I>$title</I></A><BR>\n";
    $page .= "$file<BR>\n";
    foreach $line (@hits){
      $page .= "$line<BR>\n";
    }
  }
  if($hitcount>0){
    $page .= "</UL>";
  }else{
    $page = "";
  }
  ("",$page); # return;
}

# parse query
sub parsequery{
  local($query)=@_;
  local($context,$thesaurus,$substr);
  # preprocess whitespace and discard spaces after @ and -D
  $query =~ tr/ \t/ /s;
  $query =~ s/@ /@/g;
  $query =~ s/-D /-D/g;
  $query =~ s/^-D/ -D/g;
  $_=$query;
  # "optional URL context as @-sign"
  if(m:^([^@]*)\s+@(.*)$:){
    $context=$2;
    $_=$1;
  } ### to be added: "IN"
  while(m:\s+-[SDT]\d*$:){
    # "turn on "global" thesaurus" by adding -T"
    if(m:^(.*)\s+-T$:){
      $thesaurus="y";
      # print "turn on thesaurus\n";
      $_=$1;
    } 
    # "turn on matching substrings by adding -S"
    if(m:^(.*)\s+-S$:){
      $substr="y";
      # print "turn on substring matching\n";
      $_=$1;
    }
    # "turn on modified since n days" by adding -D"
    if(m:^(.*)\s+-D(\d+)$:){
      $days=$2;
      # print "turn on modified since $days\n";
      $_=$1;
    }
  } 
  # print "remaining query $_\n";
  @list=split(/ /,$_);
  $expectword="y" unless($days && $#list==-1);
  foreach $w (@list){
    $_ = $w;
    tr/A-Z/a-z/;
    if(/^and$/) {
      if($expectword) {$err="$w"; last;}
      $expectword="y";
      $bool .= "&";
    }elsif(/^or$/){
      if($expectword) {$err="$w"; last;}
      $expectword="y";
      $bool .= "+";
    }else{
      ### unless($expectword) {$err="$w"; last;}
      # new: and is optional
			unless($expectword) {
        $bool .= "&";
			}
      $expectword="";
      push(@querystring,$w); 
    }
  }
  if($expectword){
    return ("syntax error in query: must end with keyword!");
  }
  if($err){
    return ("syntax error in query near '$err'!");
  }
  #print"c=$context\nt=$thesaurus\nl=$levenshtein\n";
  #print "b=$bool\nd=$days\nq=@querystring\n";
  return("",$context,$thesaurus,$substr,$bool,$days,@querystring);
}

# get index entries matching query
sub getindex{
  local($thes,$bool,$days,$substr,@query)=@_;
  local(@list,$count,$item,$w,@wordnum,$grepexpr,$ret);
  local($limit);
  if($days){
    $limit=time()-(60*60*24*$days) unless($days==0);
  }
  foreach $item (@query){
    ++$count;
    local($w);
    $_=$item;
    local($flag)=$thes;
    if (/{(.*)}/) {
      $_ = $1;
      $flag="y";
    }
    # convert e.g. "Picture" to "picture"
    if(/^[A-Z][^A-Z]*$/){
      tr/A-Z/a-z/;
    }
    # evaluate thesaurus
    if ($flag) {
      push (@keywords,$_);
      $wordnum{$_}=$count;
      local(@synonyms)=split(/\n/,&thesread($thesfile,$_));
      foreach $w (@synonyms){
        push (@keywords,$w);
        $wordnum{$w}=$count;
      }
    } else {
      $w=$_;
      push (@keywords,$w);
      $wordnum{$w}=$count;  
    }
  }

  $grepexpr="^@";
  foreach (@keywords) { $grepexpr.="|$_"; }

  # egrep is a lot faster then anything we could do in Perl..
  ###print "grepexpr is $grepexpr\n";
	local($timstr,$pat);
  open(fpin,"$zcatcmd <$indexfile | $grepcmd '$grepexpr' |");
  while(<fpin>){
    if(!/^@/){
      if($limit==0 || $mtime>=$limit){
        foreach $w (@keywords){
				  $pat = $substr ? ".*$w.*" : $w;
          if(/(\d+)\s+($pat)$/){
            $freq=$1; 
            $word=$2;
            if(length($word)>0){
              $token=$wordnum{$w};
              $entry=join("\n",$path,$token,$word,$freq,$title);
              push(@list,$entry);
            }
          }
        }
      }
    } else {
      if(/@f\s(.*)$/) { $path  =$1; next; }
      if(/@t\s(.*)$/) { $title =$1; next; }
      if(/@m\s(.*)$/) { 
 			  $mtime=$1;
        $timstr = &timetostr($mtime);
				$title = "$title (last change $timstr)"; 
        if(($limit==0 || $mtime>=$limit) && ($#keywords==-1)){
			    $entry=join("\n",$path,"","","",$title);
          push(@list,$entry);
        }
        next; 
      }
    }
  }

  ### print "list has $#list elements\n";
  if($#keywords>=0){
    # if keywords given evaluate expression and stuff
    @list=sort(@list);
    &evaluateexpr($bool,@list); #return
  }else{
    # else just reorder
    foreach $w (@list){
      ($path,$token,$word,$freq,$title)=split(/\n/,$w);
      $w=join("\n","1",$path,$title);
    }
		@list;
  }
}

sub evaluateexpr {
  local($bool,@list)=@_;
  local($lastpath,$lasttitle,$retval);
  local($relevance,%wordlist);
  local(@reslist);
  
  # let's do some initialisation
  ($path,$token,$word,$freq,$title)=split(/\n/,$list[0]);
  $lastpath=$path;
  $lasttitle=$title;
  $relevance=$freq;
  %wordlist=();
  local($fw);
  if($word ne $query[$token-1]){
    $fw="$freq ($query[$token-1])";
  }else{
    $fw="$freq";
  }
  $wordlist{$word}=$fw;
  foreach $i (0 .. $count-1){
    $exprarray[$i]=0;
  }
  
  # loop over all entries in list
  foreach $i (0 .. $#list+1){ # sic!
    if($i <= $#list){
      ($path,$token,$word,$freq,$title)=split(/\n/,$list[$i]);
    } 
    # if path has changed -> compute expression
    if(($lastpath ne $path) || ($i==$#list+1)) {
      $expr=join('',@exprarray);
      ###print "path changed, call $exprarray ($expr) and ($bool)\n";
      local($ret)=&booleval($bool,$expr);
      if($ret==1){
        local($w);
        $retval .= "$relevance\n"; #here's a hit
        $retval .= "$lastpath\n"; # here's a hit
        $retval .= "$lasttitle\n"; # here's a hit
        foreach $w (sort keys(%wordlist)){
          $retval .= "\"$w\" $wordlist{$w}\n";
        }
        push(@reslist,$retval);
        $retval="";
      }
      if($i==$#list+1){
        last; # leave loop
      }
      $lastpath=$path;
      $lasttitle=$title;
      $relevance=$freq;
      %wordlist=();
      if($word ne $query[$token-1]){
        $fw="$freq ($query[$token-1])";
      }else{
        $fw="$freq";
      }
      $wordlist{$word}=$fw;
      foreach $i (0 .. $count-1){
        $exprarray[$i]=0;
      }
      $exprarray[$token-1]=1;
    }else{
      $exprarray[$token-1]=1;
      $relevance += $freq;
      if($word ne $query[$token-1]){
        $fw="$freq ($query[$token-1])";
      }else{
        $fw="$freq";
      }
      $wordlist{$word}=$fw;
    }
  }
  @reslist; # return;
}

# compute boolean expressions
# e.g. to compute "1 or 0 and 1" use booleval("101","+&")
sub booleval {
  local($arg1,$arg2)=@_;
  local($t1,$t2,$i,$op,$opers,$terms);
  # print "bool($arg1,$arg2)\n"; #XXX
  @opers=split(//,$arg1);
  @terms=split(//,$arg2);

  #test output:  
  #local(@fump)=();
  #for $i (0..($#terms-1)){
  #  push(@fump,$terms[$i]);
  #  push(@fump,$opers[$i]);
  #}
  #push(@fump,$terms[$#terms]);
  #print "calling booleval @fump -> "; 
  
  $t1=$terms[0];
  if($#terms==0){ # only one term given
    return $t1;
  }
  for $i (0..($#terms-1)){
    $t2=$terms[$i+1];
    $op=$opers[$i];
    if($op eq "+"){
      if($t1!=0){ 
        return 1;
      } else {
        $t1=$t2;
      }
    }else{
      $t1*=$t2;
    }
  }
  return $t1;
}

# evaulate a thesaurus file for a given term
sub thesread {
  local($thesfile,$word)=@_;
  local($last,$result,$line)="";
  local($allowed)="EQ|AB|UF";
  unless (open(fpin,$thesfile)) {
    print STDERR "Cannot open thesaurus file $thesfile\n";
    return undef;
  }
  while(<fpin>){
    $line++;
    if (m:^(\S+)\s+$:) {
      $last=$1;
    }elsif((m:^\s+($allowed)\s+(\S+)$:)&&($last eq $word)) {
      $result .= "$2\n";
    }
  }
  close(fpin);
  $result;
}

# translate URL to physical
sub translate {
  local($url)=@_;
  local($docroot,$aliasdone);
  $_=$url;
  s/(.*)\/$/\1/; # strip off a trailing "/"
  #print "Was $_\n";
  foreach $key (keys(%urltopath)){
   if($key eq "/"){
      $docroot=$urltopath{$key};
	 }
   if( ($key ne "/") && (/^$key/) ){
      s/^$key/$urltopath{$key}/;
			$aliasdone="y";
      #print "replacing $key with $urltopath{$key}\n";
    }
  }
  if(!$aliasdone && $docroot){
    $_ = $docroot.$_;
  }
  #print "Now is $_\n";
  $_;
}

# translate physical to URL
sub translateback {
  local($url)=@_;
  local($docroot,$aliasdone);
  $_=$url;
  s/(.*)\/$/\1/; # strip off a trailing "/"
  #print "Was $_\n";
  foreach $key (keys(%urltopath)){
    if($key eq "/"){
      $docroot=$urltopath{$key};
    }else{ 
      if(/^$urltopath{$key}/){
        s/^$urltopath{$key}/$key/;
        $aliasdone="y";
        #print "replacing $urltopath{$key} with $key\n";
      }
    }
  }
  if(!$aliasdone && $docroot){
    s/$docroot//;
  }
  #print "Now is $_\n";
  $_;
}

# convert time to string
sub timetostr{
  local($time)=@_;
  local($sec,$min,$hour,$mday,$mon,$year,$wday,@dontcare)=localtime($time);
  local($weekday)=(Sun,Mon,Tue,Wed,Thu,Fri,Sat)[$wday];
  local($month)=(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)[$mon];
  local($result)="$weekday $mday $month $year";
	$result;
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