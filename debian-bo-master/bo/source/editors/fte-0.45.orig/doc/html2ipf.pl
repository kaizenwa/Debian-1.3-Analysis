#!perl5 -w

# html2ipf - version 0.4
# by Marko Macek, mark@hermes.si | marko.macek@snet.fer.uni-lj.si
# needs some work, but is much faster than 0.2.

# version 0.4: handle some internal A HREF and A NAME.

$h2i = 'html2ipf';
$h2i_version = '0.4';

print ".*! $h2i $h2i_version\n\n";

$dl_param = 'compact tsize=10 break=all';
$ol_param = 'compact';
$ul_param = 'compact';
$lmargin  = ':lm margin=1.';
$fontspec = ''; #:font facename=Helv size=16x8.';

undef $/; # slurp whole file as one line
$line = <>; # read input file

$wasws = 0;

for ($pass = 1; $pass <= 2; $pass++) {
    $nheads = 0;
    $list_level = 0;
    $head_level = 0;
    $pre = 0;
    $in_head = 0;
    $ahref = 0;
    $naname = 0;

    $cpos = 0;
    
    TAG: 
    while ($opos = $cpos,
           ($cpos = index($line, "<", $cpos) + 1) != 0) # skip to next tag
    {
        pos($line) = $cpos;  # match regexp there
        
        &out(substr($line, $opos, pos($line) - $opos - 1)); # output text
        
        $TAG = undef;
        
        if ($line =~ /\G!(.*?)(--.*?--\s*)*(.*?)>/sgo) { # comment
            $cpos = pos $line;
            #&comment($2);
            next TAG;
        }
        
        pos ($line) = $cpos;
        if ($line =~ /\G\s*([\/]?[A-Za-z0-9]*)/go) {
            $cpos = pos($line);
            $TAG = uc $1;
            
            #print "<|", $TAG, "\n";
        }
        
        undef %ARGS;
        
        ARG:
        while (1) {
            pos($line) = $cpos;
            
            if ($line =~ /\G\s*/go) { $cpos = pos ($line); } # skip whitespace
            
            last ARG unless $line =~ /\G([A-Za-z0-9]+)\s*/go; # param name
            $cpos = pos $line;
            
            $pname = uc $1;
            if ($line =~ /\G=\s*/go) {
                $cpos = pos $line;
                
                if ($line =~ /\G"([^"]*)"\s*/go) {
                    $cpos = pos $line;
                    #print "+|$pname=\"$1\"\n";
                    $ARGS{$pname} = $1;
                    next ARG;
                };
                pos($line) = $cpos;
                if ($line =~ /\G'([^']*)'\s*/go) {
                    $cpos = pos $line;
                    #print "+|$pname='$1'\n";
                    $ARGS{$pname} = $1;
                    next ARG;
                };
                pos($line) = $cpos;
                if ($line =~ /\G([^ <>"']+)\s*/go) {
                    $cpos = pos $line;
                    #print "+|$pname=$1\n";
                    $ARGS{$pname} = $1;
                    next ARG;
                };
                $ARGS{$pname} = "";
                die "no value for tag";
            }
            #print "+|$pname\n";
        }
        pos($line) = $cpos;
        ($cpos = index($line, ">", $cpos) + 1) != 0 or die "tag without end";
        
        &tag($TAG, \%ARGS);
    }
    
    &out(substr($line, $opos, length($line) - $opos));
    print STDERR "\n";
}

sub put {
    my $lin = $_[0];

    print $lin;

    $wasws = ($lin =~ /[\n\t ]$/os);
}

sub tag {
    my $TAG = $_[0];
    my %ARGS = %{$_[1]};

    if ($pass == 1) { # during first pass, check for: A NAME=...
        $TAG =~ /^\/H[1-6]$/o  && do {
            $nheads++;
            print STDERR ".";
        }
        or $TAG eq 'A' && do {
            $naname++;
            print STDERR %ARGS if $naname > 1;
            if (defined $ARGS{"NAME"}) {
                $aname = '#' . $ARGS{"NAME"};
            } 
        }
        or $TAG eq '/A' && do {
            $naname--;
            if ($naname == 0) {
                $rnames{$aname} = $nheads - 1;
            }
        }
    } elsif ($pass == 2) {
        $list_level = ($list_level < 0) ? 0 : $list_level;
        
        $TAG eq 'B'       && do { put(':hp2.') unless $in_head; }
        or $TAG eq '/B'      && do { put(':ehp2.') unless $in_head; }
        or $TAG eq 'STRONG'  && do { put(':hp7.')  unless $in_head; }
        or $TAG eq '/STRONG' && do { put(':ehp7.') unless $in_head; }
        or $TAG eq 'I'       && do { put(':hp1.')  unless $in_head; }
        or $TAG eq '/I'      && do { put(':ehp1.') unless $in_head; }
        or $TAG eq 'TT'      && do { put(':hp2.')  unless $in_head; }
        or $TAG eq '/TT'     && do { put(':ehp2.') unless $in_head; }
        or $TAG eq 'BR'      && do { put("\n.br\n"); $wasws = 1; }
        or $TAG eq 'HR'      && do { put("\n.br\n"); $wasws = 1; }
        or $TAG eq 'P'       && do { put("\n:p."); $wasws = 1; }
        or $TAG eq 'LI'      && do { put("\n:li."); $wasws = 1;} 
        or $TAG eq 'CENTER'  && do { put(':lines align=center.'); }
        or $TAG eq '/CENTER' && do { put(':elines.'); $wasws = 1; }
        or $TAG eq 'DL'      && do { put(":dl " . $dl_param . '.'); $list_level++; } 
        or $TAG eq '/DL'     && do { put(':edl.'); $list_level--; $wasws = 1; }
        or $TAG eq 'DD'      && do { put("\n:dd."); $wasws = 1; }
        or $TAG eq 'DT'      && do { put("\n:dt."); $wasws = 1; }
        or $TAG eq 'PRE'     && do { put(':xmp.'); $pre++; }
        or $TAG eq '/PRE'    && do { put(':exmp.'); $pre--; $wasws = 1; }
        or $TAG eq 'XMP'     && do { put(':xmp.'); $pre++; }
        or $TAG eq '/XMP'    && do { put(':exmp.'); $pre--; $wasws = 1; }
        or $TAG eq 'OL>'     && do { put(":ol " . $ol_param . '.'); $list_level++; }
        or $TAG eq '/OL'     && do { put(":eol."); $list_level--; $wasws = 1; }
        or $TAG eq 'UL'      && do { put(":ul " . $ul_param . '.'); $list_level++; }
        or $TAG eq '/UL'     && do { put(":eul."); $list_level--; $wasws = 1; }
        or $TAG eq 'IMG'     && do {
            $pic = $ARGS{"SRC"};
            $pic =~ s/gif$/bmp/i;
            put(":artwork runin name='$pic'.") unless $in_head;
        }
        or $TAG eq 'HTML'    && do { put("\n:userdoc.\n") }
        or $TAG eq '/HTML'   && do { put("\n:euserdoc.\n") }
        or $TAG eq 'TITLE'   && do { put("\n:title.") }
        or $TAG eq '/TITLE'  && do { put("\n") }
        or $TAG eq '/A' && do { 
            if ($ahref > 0) {
                put(":elink.");
                --$ahref;
            }
        }
        or $TAG eq 'A' && do {
            if (defined $ARGS{"HREF"}) {
                if ($ARGS{"HREF"} =~ /^#/) {
                    if (defined $rnames{$ARGS{"HREF"}}) {
                        $id = $rnames{$ARGS{"HREF"}};
                        put(":link reftype=hd refid=$id.");
                        ++$ahref;
                    } else {
                        print STDERR "no link for " . $ARGS{"HREF"} . "\n";
                    }
                } else {
                    print STDERR "external ref not handled: " . $ARGS{"HREF"} . "\n";
                }
            }
        }
        or $TAG =~ /^\/H[1-6]$/o  && do {
            $nheads++;
            put("\n" . $fontspec . $lmargin . ":i1." . $curhead . "\n:p.");
            $in_head = 0;
            $wasws = 1;
            print STDERR ".";
        }
        or $TAG =~ /^H([1-6])/o   && do {
            $hl = $1;
            if ($hl > $head_level + 1) { # hack for bad headings
                $hl = $head_level + 1;
            }
            $head_level = $hl;
            put("\n:h$hl id=$nheads.");
            $in_head = 1;
            $curhead = "";
        }
    }
}

sub out {
    my $lin = $_[0];
    my $first = 1;
    my $i;

    return if ($pass == 1);

    #$lin =~ s/\&lt\;/\</og;
    #$lin =~ s/\&gt\;/\>/og;
    #$lin =~ s/\&amp\;/\&/og;
    ##$lin =~ s/\n/ /og;
    #print $lin;

    #    $lin =~ s/\./\&per\./og;             # .
    $lin =~ s/\&lt\;/\</og;            # <
    $lin =~ s/\&gt\;/\>/og;            # >
    $lin =~ s/\:/\&colon\./og;         # :
    $lin =~ s/\&amp\;/\&amp\./og;      # &

    if ($pre > 0) {
        print $lin; 
    } else {
#        $lin =~ s/\n / /osg;
        $lin =~ s/\n/ /osg;
        $lin =~ s/ +/ /og;
        if ($wasws) {
            $lin =~ s/^ +//o;
        }
        if ($in_head) {
            $curhead .= $lin;
        }
        
        while ($lin ne "") {
            put("\n") unless ($first);
            put(" ") if $line =~ /^\./;
            if (length($lin) <= 70) {
                put($lin);
                $lin = "";
            } else {
                $i = 70;
                if ($i > length $lin) { $i = length $lin; }
                while ($i > 0 && substr($lin, $i, 1) ne ' ') { $i--; }
                if ($i == 0) { $i = 70 };
                if ($i > length $lin) { $i = length $lin; }
                put(substr($lin, 0, $i));
		$lin = substr($lin, $i + 1);
                $lin =~ s/^ +//o;
                $first = 0;
            }
        }
    }
}

sub comment {
#my $comm = $_[0];

#print $comm;
}

sub badtag {
#    my $badtag = $_[0];
#    print "<$badtag>"; # ?
}
__END__
sub tag {
    my $TAG = $_[0];
    my %PARM = %{$_[1]};
    my @ARGS = @{$_[2]};
    
    print "<$TAG";
    foreach $n (@ARGS) {
        $key = $ARGS[$n][2];
        print ' ';
        print $key                             if $ARGS[$n][0] == -1;
        print $key, '=', $ARGS[$n][1]          if $ARGS[$n][0] == 0;
        print $key, '=\'', $ARGS[$n][1], '\''  if $ARGS[$n][0] == 1;
        print $key, '="', $ARGS[$n][1], '"'    if $ARGS[$n][0] == 2;
    }
    print ">\n";
}

sub out {
    my $lin = $_[0];
    
    $lin =~ s/\n/ /;
    
    print " |", $lin, "\n";
}

sub comment {
    my $comm = $_[0];

    print "#|", $comm;
}

__END__
