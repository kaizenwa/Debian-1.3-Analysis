# MAKEINDEX.PERL by Nikos Drakos <nikos@cbl.leeds.ac.uk> 30-11-93
# Computer Based Learning Unit, University of Leeds.
#
# Extension to LaTeX2HTML to translate makeindex 
# commands to equivalent HTML commands.
#
# The Perl code was written by Axel Belinfante 
# <Axel.Belinfante@cs.utwente.nl> 
#
# 25-JAN 1994 Modified by Nikos Drakos to use ;tex2html_html_special_mark_quot;
# instead of &quot;
#
# 13-APR 1996  Modified by Ross Moore <ross@mpce.mq.edu.au>
#  a.  made sub-items work properly;
#  b.  recognise \char-combinations for a backslash;
#  c.  shorten the text used with hyperlinks...
# ( 30-APR 1996 ) 
#      ...optionally according to whether  $SHORT_INDEX  is set;
#  d.  changed the separator from `,' to  `|' 
#  e.  implemented section-ranges, corresp. to |( and |) 
#  f.  partially implemented  |see 
# ( 2-MAY 1996 ) 
#  g.  allow the printable index-key to be a hyper-link
#  h.  ... even with `$' and `_' characters in the name.
# ( 17-MAY 1996 ) 
#  i.  allow index entry to be the target of hyper-links.
#  j.  include a Legend with (c.) above.
# ( 22-MAY 1996 ) 
#  k.  reimplemented quoting with  " using  ;SPMquot;
# ( 24-MAY 1996 ) 
#  l.  allow anchors also in the page-ref section
# ( 15-JUNE 1996 ) 
#  m.  fixed printable_keys with sub-items, etc.


sub add_idx {
    print "\nDoing the index ...";
    local($key, @keys, $next, $index);
# RRM, 15.6.96:  index constructed from %printable_key, not %index
    @keys = keys %printable_key;
#    @keys = keys %index;
    @keys = sort @keys;
    @keys = grep(!/\001/, @keys);
    foreach $key (@keys) {
	$index .= &add_idx_key($key);
    }
    if ($SHORT_INDEX) { 
	print "(compact version with Legend)";
	local($num) = ( $index =~ s/\<D/<D/g ); 
	if ($num > 50 ) {
	    s/$idx_mark/$preindex<HR><DL>\n$index\n<\/DL>$preindex/o;
	} else { 
	    s/$idx_mark/$preindex<HR><DL>\n$index\n<\/DL>/o;
	}
    } else { s/$idx_mark/<DL COMPACT>\n$index\n<\/DL>/o; }
}
 
sub add_idx_key {
    local($key) = @_;
    local($index, $next);
    if ( $index{$key} eq '@' ) { 
	if ($SHORT_INDEX) { $index .= "<DD><BR><DT>".&print_key."\n<DD>"; }
	else { $index .= "<DT><DD><BR>\n"; }
    } elsif ($index{$key}) {
	if ($SHORT_INDEX) { 
	    $next = "<DD>".&print_key." : ". $index{$key};
	} else { $next = "<DT>".&print_key."\n<DD>".$index{$key}; }
#        $next =~ s/[,] $/\n/;   # Get rid of the last comma-space
	$next =~ s/[\|] $//;
	$index .= $next."\n";
	$index_printed{$key} = 1;
    }

    if ($sub_index{$key}) {
	local($subkey, @subkeys, $subnext, $subindex);
	@subkeys = sort(split("\004", $sub_index{$key}));
	if ($SHORT_INDEX) { 
	    $index .= "<DD>".&print_key  unless $index_printed{$key};
	    $index .= "<DL>\n"; }
	else { 
	    $index .= "<DT>".&print_key."\n<DD>"  unless $index_printed{$key};
	    $index .= "<DL COMPACT>\n"; }
	foreach $subkey (@subkeys) {
	    if (!($index_printed{$subkey} == 1)) {
		$index .= &add_sub_idx_key($subkey); }
	}
	$index .= "</DL>\n";
    }
    return $index;
}

sub print_key {
   "<STRONG>$printable_key{$key}</STRONG>"
}

sub add_sub_idx_key {
    local($key) = @_;
    local($index, $next);
    if ($sub_index{$key}) {
	local($subkey, @subkeys, $subnext, $subindex);
	@subkeys = sort(split("\004", $sub_index{$key}));
	if ($SHORT_INDEX) { 
	    if ($index{$key}) {
		$next = "<DD>".&print_key." : ".$index{$key};
#		$next =~ s/[\|] $/\n/;
#        	$index .= $next;
		$next =~ s/[\|] $//;
        	$index .= $next."\n";
        	$index_printed{$key} = 1;
	    } else { $index .= "<DD>".&print_key  unless $index_printed{$key} }
	} else { 
	    if ($index{$key}) {
		$next = "<DT>".&print_key."\n<DD>".$index{$key};
		$next =~ s/[\|] $//;
		$index .= $next."\n";
		$index_printed{$key} = 1;
	    } else { $index .= "<DT>".&print_key  unless $index_printed{$key} }
	} 
	if ($SHORT_INDEX) { $index .= "<DD><DL>\n"; }
	else { $index .= "<DD><DL COMPACT>\n"; }
	foreach $subkey (@subkeys) {
	    if (!($index_printed{$subkey} == 1)) {
		$index .= &add_sub_idx_key($subkey); }
	}
	$index .= "</DL>\n";
    } elsif ($index{$key}) {
	if ($SHORT_INDEX) { 
	    $next = "<DD>".&print_key." : " . $index{$key};
	} else { $next = "<DT>".&print_key."\n<DD>".$index{$key}; }
	$next =~ s/[\|] $//;
	$index .= $next."\n";
	$index_printed{$key} = 1;
    }
    return $index;
}

sub do_cmd_index {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
#    local($br_id, $str) = ($1, $2);
    join('',&named_index_entry($1, $2),$_);
}

sub named_index_entry {
    local($br_id, $str) = @_;
    # escape the quoting etc characters
    # ! -> \001
    # @ -> \002
    # | -> \003
    $str =~ s/\\\\/\011/g;
    $str =~ s/\\;SPMquot;/\012/g;
    $str =~ s/;SPMquot;!/\013/g;
    $str =~ s/!/\001/g;
    $str =~ s/\013/!/g;
    $str =~ s/;SPMquot;@/\015/g;
    $str =~ s/@/\002/g;
    $str =~ s/\015/@/g;
    $str =~ s/;SPMquot;\|/\017/g;
    $str =~ s/\|/\003/g;
    $str =~ s/\017/|/g;
    $str =~ s/;SPMquot;(.)/\1/g;
    $str =~ s/\012/;SPMquot;/g;
    $str =~ s/\011/\\\\/g;

    local($key_part, $pageref) = split("\003", $str, 2);
    local(@keys) = split("\001", $key_part);
#print STDERR "\nINDEX ($str)\n($key_part, $pageref)(@keys)\n";
    # If TITLE is not yet available (i.e the \index command is in the title of the
    # current section), use $before.
    $TITLE = $before unless $TITLE;
    # Save the reference
    local($words) = '';
    if ($SHORT_INDEX) { $words = &make_shortidxname; }
    else { $words = &make_idxname; }
    local($super_key) = '';
    local($sort_key, $printable_key, $cur_key);
    foreach $key (@keys) {
        ($sort_key, $printable_key) = split("\002", $key);
#
# RRM:  16 May 1996
# any \label in the printable-key will have already
# created a label where the \index occurred.
# This has to be removed, so that the desired label 
# will be found on the Index page instead. 
#
	if ($printable_key =~ /tex2html_anchor_mark/ ) {
	    $printable_key =~ s/><tex2html_anchor_mark><\/A><A//g;
	    local($tmpA,$tmpB) = split("NAME=\"", $printable_key);
	    ($tmpA,$tmpB) = split("\"", $tmpB);
	    $ref_files{$tmpA}='';
	    $index_labels{$tmpA} = 1;
	}
#
# resolve and clean-up the hyperlink index-entries 
# so they can be saved in an  index.pl  file
#
	if ($printable_key =~ /$cross_ref_mark/ ) {
	    local($label,$id,$ref_label);
	    $printable_key =~ s/$cross_ref_mark#(\w+)#(\w+)>$cross_ref_mark/
	        do { ($label,$id) = ($1,$2);
		    $ref_label = $external_labels{$label} unless
			($ref_label = $ref_files{$label});
		    '"' . "$ref_label#$label" . '">' .
		    &get_ref_mark($label,$id)}/geo;
	}
	$printable_key =~ s/<\#[^\#>]*\#>//go;
#RRM
# recognise \char combinations, for a \backslash
#
	$printable_key =~ s/\&\#;\'134/\\/g;		# restore \\s
	$printable_key =~ s/\&\#;\`<BR> /\\/g;		#  ditto
	$printable_key =~ s/\&\#;*SPMquot;92/\\/g;	#  ditto
#
#        $sort_key .= "@$printable_key" if !($printable_key);	# RRM
        $sort_key .= "@$printable_key" if !($sort_key);		# RRM
        $sort_key =~ tr/A-Z/a-z/;
        if ($super_key) {
            $cur_key = $super_key . "\001" . $sort_key;
            $sub_index{$super_key} .= $cur_key . "\004";
        } else {
            $cur_key = $sort_key;
        }
        $index{$cur_key} .= ""; 
#
# RRM,  15 June 1996
# if there is no printable key, but one is known from
# a previous index-entry, then use it.
#
	if (!($printable_key) && ($printable_key{$cur_key}))
	    { $printable_key = $printable_key{$cur_key}; } 
#
# do not overwrite the printable_key if it contains an anchor
#
	if (!($printable_key{$cur_key} =~ /tex2html_anchor_mark/ ))
            { $printable_key{$cur_key} = $printable_key || $key; }
#
        $super_key = $cur_key;
    }
#
# RRM
# page-ranges, from  |(  and  |)  and  |see
#
    if ($pageref) {
	if ($pageref eq "\(" ) { 
	    $pageref = ''; 
	    $next .= " from ";
	} elsif ($pageref eq "\)" ) { 
	    $pageref = ''; 
	    local($next) = $index{$cur_key};
	    $next =~ s/[\|] *$//;
            $index{$cur_key} = "$next to ";
	}
    }
    if ($pageref) {
	$pageref =~ s/see/<i>see <\/i> /g;
	$pageref =~ s/emph//g;
#
# RRM:  25 May 1996
# any \label in the pageref section will have already
# created a label where the \index occurred.
# This has to be removed, so that the desired label 
# will be found on the Index page instead. 
#
	if ($pageref =~ /tex2html_anchor_mark/ ) {
	    $pageref =~ s/><tex2html_anchor_mark><\/A><A//g;
	    local($tmpA,$tmpB) = split("NAME=\"", $pageref);
	    ($tmpA,$tmpB) = split("\"", $tmpB);
	    $ref_files{$tmpA}='';
	    $index_labels{$tmpA} = 1;
	}
#
# resolve and clean-up any hyperlinks in the page-ref, 
# so they can be saved in an  index.pl  file
#
	if ($pageref =~ /$cross_ref_mark/ ) {
	    local($label,$id,$ref_label);
	    $pageref =~ s/$cross_ref_mark#(\w+)#(\w+)>$cross_ref_mark/
	      do { ($label,$id) = ($1,$2);
		if ($index_labels{$label}) { $ref_label = ''; } 
		else { $ref_label = $external_labels{$label} 
		    unless ($ref_label = $ref_files{$label}); }
		'"' . "$ref_label#$label" . '">' .
		  &get_ref_mark($label,$id)}/geo;
	}
	$pageref =~ s/<\#[^\#>]*\#>//go;
#
	if ($pageref eq ' ') { $index{$cur_key}='@'; }
        else { $index{$cur_key} .= $pageref . " | "; }
    } else {
#        $index{$cur_key} .= &make_href("$CURRENT_FILE#$br_id",$words) . ", ";
#        $index{$cur_key} .= &make_href("$CURRENT_FILE#$br_id",$words) . " | ";
        $index{$cur_key} .= &make_named_href('',"$CURRENT_FILE#$br_id",$words)." | ";
    } 
#    join('',"<A NAME=$br_id>$anchor_invisible_mark<\/A>",$_);
    "<A NAME=\"$br_id\">$anchor_invisible_mark<\/A>";
}

$WORDS_IN_INDEX = 4 unless ($WORDS_IN_INDEX);

#RRM:
# alternative strings for short-names or section-names
#
sub make_idxname {
#    (&get_first_words($TITLE, 4) || 'no title')
    (&get_first_words($TITLE, $WORDS_IN_INDEX) || 'no title')
}


sub make_shortidxname {
    local($sstring, $key );
    foreach $key (@curr_sec_id) {
	if ("$key" eq "0") {} else { 
		if ($sstring) { $sstring .= "."."$key";
		} else { if ($PREFIX) 
			{ $sstring = "$PREFIX";
			} else { $sstring = "$key"; }
		}
	 }
    }
    if ($sstring) {} else { 
	if ($PREFIX) {$sstring = "$PREFIX"; } else { $sstring = "^"; }
    }
    $sstring;
}

1;				# This must be the last line
