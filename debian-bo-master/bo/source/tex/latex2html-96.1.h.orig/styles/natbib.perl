# natbib.perl - LaTeX2HTML support for the LaTeX2e natbib package
#  (flexible author-year citations)
# Martin Wilck, 20.5.1996 (martin@tropos.de)
#
# modified for document segmentation by
# Ross Moore, 28.5.1996 <ross@mpce.mq.edu.au>
#

package main;

# CUSTOMIZATION: Delimiters for citations in text 
#   (in natbib.sty, per default round parentheses)
#   POSSIBLE IMPROVEMENT: It should be possible to alter these
#       variables with options to the natbib package
#       (requires change in texexpand)
# The LaTeX \bibpunct command changes the punctuation
# variables
$CITE_OPEN_DELIM = '(' unless $CITE_OPEN_DELIM;
$CITE_CLOSE_DELIM = ')' unless $CITE_CLOSE_DELIM;

# CUSTOMIZATION: Delimiters for seperation of multiple citations
$CITE_ENUM = '; ' unless $CITE_ENUM;

# CUSTOMIZATION: 1 for numeric citations
$NUMERIC=0 unless defined ($NUMERIC);

# CUSTOMIZATION: Delimiter between author and year in parentheses
#  i.e. comma in "(Jones et al., 1990)"
$BEFORE_PAR_YEAR=', ' unless $BEFORE_PAR_YEAR;

# CUSTOMIZATION: Delimiter between multiple citations if authors are common
#  i.e. comma in "Jones et al. (1990,1991)" or "Jones (1990a,b)"
$COMMON_AUTHOR_SEP=',' unless $COMMON_AUTHOR_SEP;

# CUSTOMIZATION: Delimiter before a note in a citation
#  i.e. 2nd comma in "(Jones et al., 1990, page 267)"
$POST_NOTE=',' unless $POST_NOTE;

# CUSTOMIZATION: 
# Boolean value that determines if citations are put in the index
# Can be modified in the text by the \citeindextrue and
# \citeindexfalse commands
$CITEINDEX=0 unless defined ($CITEINDEX);

# The variable $HARVARD makes natbib.perl emulate harvard.perl
# It is usually set to one in the "fake harvard.perl" before
# calling natbib.perl. 
# Users normally shouldn't have to set it "by hand".
$HARVARD=0 unless defined ($HARVARD);

# Instead of $cite_mark, different markers for different manners
# of citation 
# Jones et al. (1990)
$cite_mark = '<tex2html_cite_mark>';
# Jones, Baker, and Williams (1990)
$cite_full_mark = '<tex2html_cite_full_mark>';
# (Jones et al., 1990)
$cite_par_mark = '<tex2html_cite_par_mark>';
# (Jones, Baker, and Williams, 1990)
$cite_par_full_mark = '<tex2html_cite_par_full_mark>';
# Jones et al.
$cite_author_mark = '<tex2html_cite_author_mark>';
# Jones, Baker, and Williams
$cite_author_full_mark = '<tex2html_cite_author_full_mark>';
# 1990
$cite_year_mark = '<tex2html_cite_year_mark>';
# Jones et al. [21]
$citet_mark = '<tex2html_citet_mark>';
# Jones, Baker, and Williams [21]
$citet_full_mark = '<tex2html_citet_full_mark>';
# Jones et al. 1990
$citealt_mark = '<tex2html_citealt_mark>';
# Jones, Baker, and Williams 1990
$citealt_full_mark = '<tex2html_citealt_full_mark>';
# marker for multiple citations
$cite_multiple_mark = '<tex2html_cite_multiple_mark>';

$HARVARDAND="&amp;";

# bibpunct arrays for citestyle command
@citestyle_chicago  =('(',  ')',  '; ',  'a',  ', ',  ',' );
@citestyle_named    =('[',  ']',  '; ',  'a',  ', ',  ',' );
@citestyle_agu      =('[',  ']',  '; ',  'a',  ', ',  ', ');
@citestyle_egs      =('(',  ')',  '; ',  'a',  ', ',  ',' );
@citestyle_agsm     =('(',  ')',  ', ',  'a',  ''  ,  ',' );
@citestyle_kluwer   =('(',  ')',  ', ',  'a',  ''  ,  ',' );
@citestyle_dcu      =('(',  ')',  '; ',  'a',  '; ',  ',' );
@citestyle_aa       =('(',  ')',  '; ',  'a',  ''  ,  ',' );
@citestyle_pass     =('(',  ')',  '; ',  'a',  ', ',  ',' );
@citestyle_anngeo   =('(',  ')',  '; ',  'a',  ', ',  ',' );
@citestyle_nlinproc =('(',  ')',  '; ',  'a',  ', ',  ',' );

$HARVARDAND_dcu = 'and';

sub do_cmd_cite {
    local($_) = @_;
    local($cite_key, @cite_keys);
# Look for options of the command in a seperate subroutine
    local($has_optional,$optional1,$optional2)=&cite_check_options;
# Select the correct marker 
    local ($c_mark) = ($has_optional ? $cite_par_mark : $cite_mark);
# In numeric mode, all citations except those by \citet and \citet*
# are marked by $cite_par_mark
    $c_mark = $cite_par_mark if ($NUMERIC);
# The following is standard from the original latex2html routine
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
# Second argument of &do_cite_keys set to TRUE in numeric mode 
# -> surround citation with parentheses
	    &do_cite_keys($br_id,($has_optional || $NUMERIC),
		$optional1,$optional2,$c_mark,$cite_key), $_); 
    } else {print "Cannot find citation argument\n";}
    $_;
}

# The following are Harvard-specific, but generally defined,
# since they don't conflict with natbib syntax
# They are therefore available in L2H inside a 
# htmlonly environment
sub do_cmd_citeaffixed {
# second argument for additional text inside the parentheses 
# before the citation
    local($_) = @_;
    local($cite_key, @cite_keys);
    local ($optional1,$dummy)=&get_next_optional_argument;
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    $cite_key=$2;
# read 2nd argument
    s/$next_pair_pr_rx//o;
    local($optional2)=$2;
    if ($cite_key) {
	local ($br_id)=$1;
	$_ = join('',
	    &do_cite_keys($br_id,1,
		$optional1,$optional2,$cite_par_mark,$cite_key), $_); 
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citeaffixedstar {
    local($_) = @_;
    local($cite_key, @cite_keys);
    local ($optional1,$dummy)=&get_next_optional_argument;
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    $cite_key=$2;
    s/$next_pair_pr_rx//o;
    local($optional2)=$2;
    if ($cite_key) {
	local ($br_id)=$1;
	$_ = join('',
	    &do_cite_keys($br_id,1,
		$optional1,$optional2,
		  ($NUMERIC ? $cite_par_mark: $cite_par_full_mark),
		  $cite_key), $_); 
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citeasnoun {
# Harvard:
# Jones et al. (1990)
    local($_) = @_;
    local($cite_key, @cite_keys);
# All harvard citation commands take one optional argument:
#   Text to be inserted *after* the citation
    local($optional1,$dummy)=&get_next_optional_argument;
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
	    &do_cite_keys($br_id,$NUMERIC,
		$optional1,'',($NUMERIC? $cite_par_mark : $cite_mark)
			  ,$cite_key), $_); 
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citeasnounstar {
# Harvard:
# Jones, Baker and Williams (1990) 
    local($_) = @_;
    local($cite_key, @cite_keys);
    local($optional1,$dummy)=&get_next_optional_argument;
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
	    &do_cite_keys($br_id,$NUMERIC,
		$optional1,'',($NUMERIC? $cite_par_mark : $cite_full_mark)
			  ,$cite_key), $_); 
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_possessivecite {
# Harvard:
# Jones et al.'s (1990)
# Uses the $citealt_mark marker (only in HARVARD mode)
    local($_) = @_;
    local($cite_key, @cite_keys);
# All harvard citation commands take one optional argument:
#   Text to be inserted *after* the citation
    local($optional1,$dummy)=&get_next_optional_argument;
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
	    &do_cite_keys($br_id,$NUMERIC,
		$optional1,'',($NUMERIC? $cite_par_mark : $citealt_mark)
			  ,$cite_key), $_); 
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_possessivecitestar {
# Harvard:
# Jones, Baker, and Williams's (1990)
    local($_) = @_;
    local($cite_key, @cite_keys);
    local($optional1,$dummy)=&get_next_optional_argument;
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
	    &do_cite_keys($br_id,$NUMERIC,
		$optional1,'',($NUMERIC? $cite_par_mark : $citealt_full_mark)
			  ,$cite_key), $_); 
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citename {
# "Jones et al."
    local($_) = @_;
    local($cite_key, @cite_keys);
    local($optional1,$dummy)=&get_next_optional_argument;
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('', 
	    &do_cite_keys($br_id,$NUMERIC,$optional1,'',
		($NUMERIC ? $cite_par_mark : $cite_author_mark)
		,$cite_key),$_);
    }
    else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citenamestar {
# "Jones, Baker, and Williams"
    local($_) = @_;
    local($optional1,$dummy)=&get_next_optional_argument;
    local($cite_key, @cite_keys);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('', 
	    &do_cite_keys($br_id,$NUMERIC,$optional1,'',
		($NUMERIC ? $cite_par_mark :$cite_author_full_mark)
		,$cite_key),$_);
    }
    else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_harvardparenthesis {
# Harvard command for customizing parentheses.
# \harvardyearparenthesis is ignored, since natbib
# doesn't distinguish the parentheses for citations
# and parentheses for years
    local ($_)=@_;
    s/$next_pair_pr_rx//o;
    my($arg)=$2;
  SWITCH: {
      $arg =~ /round/ && do {
	  $CITE_OPEN_DELIM='(';
	  $CITE_CLOSE_DELIM=')';
	  last SWITCH};
      $arg =~ /curly/ && do {
	  $CITE_OPEN_DELIM='{';
	  $CITE_CLOSE_DELIM='}';
	  last SWITCH};
      $arg =~ /square/ && do {
	  $CITE_OPEN_DELIM='[';
	  $CITE_CLOSE_DELIM=']';
	  last SWITCH};
      $arg =~ /angle/ && do {
	  $CITE_OPEN_DELIM='&lt';
	  $CITE_CLOSE_DELIM='&gt';
	  last SWITCH};
      $arg =~ /none/ && do {
	  $CITE_OPEN_DELIM='';
	  $CITE_CLOSE_DELIM='';
	  last SWITCH};
      print "\nInvalid argument to \\harvardparenthesis: $arg!\n"
      }
    $_;
}

# special subroutine definition for Harvard emulation
if ($HARVARD) {

print "\nnatbib.perl: Operating in Harvard emulation mode.\n";

sub do_cmd_citeyear {
# "(1990a)"
    local($_) = @_;
    local($cite_key, @cite_keys);
    $HARVARD && do { local($optional1,$dummy)=&get_next_optional_argument };
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('', 
	    &do_cite_keys($br_id,($HARVARD || $NUMERIC),$optional1,'',				
		($NUMERIC ? $cite_par_mark : $cite_year_mark)
		,$cite_key),$_);
    }
    else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citeyearstar {
# "1990a"
    local($_) = @_;
    local($cite_key, @cite_keys);
    local($optional1,$dummy)=&get_next_optional_argument;
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('', 
	    &do_cite_keys($br_id,$NUMERIC,$optional1,'',				
		($NUMERIC ? $cite_par_mark : $cite_year_mark)
		,$cite_key),$_);
    }
    else {print "Cannot find citation argument\n";}
    $_;
}

#End of special HARVARD definitions
} else { 
# citeyear syntax differs between natbib and harvard
print "\nnatbib.perl: Operating in natbib mode.\n";

};

# Citation commands specific for natbib
sub do_cmd_citet {
# Special citation style in natbib 6.x: Jones et al [21]
# no optional arguments
# Only makes sense with a numerical bibliography style.
# Otherwise, acts like \cite (-> same marker)
# In numeric mode, uses $cite_mark
    local($_) = @_;
    local($cite_key, @cite_keys);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
# First argument to &do_cite_keys always empty ->
# no parens, not even in numeric mode
	    &do_cite_keys($br_id,'','','',$cite_mark,$cite_key), $_);
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citetstar {
# Special citation style in natbib 6.x: Jones, Baker, and Williams [21]
# no optional arguments
    local($_) = @_;
    local($cite_key, @cite_keys);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
	    &do_cite_keys($br_id,'','','',$cite_full_mark,$cite_key), $_);
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citep {
# Shortcut for parenthetical citation
# no optional arguments
    local($_) = @_;
    local($cite_key, @cite_keys);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
# First argument of &do_cite_keys set to 1 for parenthetical citation
	    &do_cite_keys ($br_id,1,'','',
		($cite_par_mark),
		$cite_key), $_);
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citestar {
# Same as do_cmd_cite, but uses full author information
    local($_) = @_;
    local($cite_key, @cite_keys);
    local($has_optional,$optional1,$optional2)=&cite_check_options;
    local ($c_mark) = ($has_optional ? $cite_par_full_mark : $cite_full_mark);
    $c_mark = $cite_par_mark if ($NUMERIC);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
	    &do_cite_keys($br_id,($has_optional || $NUMERIC),
		$optional1,$optional2,$c_mark,$cite_key), $_); 
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citepstar {
# Shortcut for full parenthetical citation
# no optional arguments
    local($_) = @_;
    local($cite_key, @cite_keys);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('',
	    &do_cite_keys($br_id,1,'','',
		($NUMERIC ? $cite_par_mark :$cite_par_full_mark),
		$cite_key), $_);
    } else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citealt {
# Alternative form of citation: No punctuation between author an year
#    i.e. "Jones et al. 1990"
    local($_) = @_;
    local($cite_key, @cite_keys);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('', 
# First argument of &do_cite_keys = $NUMERIC
# (In numeric mode, always use parentheses)
# Same in the next subroutines
	    &do_cite_keys($br_id,$NUMERIC,'','',
		($NUMERIC ? $cite_par_mark : $citealt_mark)
		,$cite_key),$_);
    }
    else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citealtstar {
# Full alternative citation, i.e. "Jones, Baker, and Williams 1990"
    local($_) = @_;
    local($cite_key, @cite_keys);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('', 
	    &do_cite_keys($br_id,$NUMERIC,'','',				
		($NUMERIC ? $cite_par_mark : $citealt_full_mark)
		,$cite_key),$_);
    }
    else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citeauthor {
# "Jones et al."
    local($_) = @_;
    local($cite_key, @cite_keys);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('', 
	    &do_cite_keys($br_id,$NUMERIC,'','',
		($NUMERIC ? $cite_par_mark : $cite_author_mark)
		,$cite_key),$_);
    }
    else {print "Cannot find citation argument\n";}
    $_;
}

sub do_cmd_citefullauthor {
# "Jones, Baker, and Williams"
    local($_) = @_;
    local($cite_key, @cite_keys);
    s/^\s*\\space//o;		# Hack - \space is inserted in .aux
    s/$next_pair_pr_rx//o;
    if ($cite_key = $2) {
	local ($br_id)=$1;
	$_ = join('', 
	    &do_cite_keys($br_id,$NUMERIC,'','',
		($NUMERIC ? $cite_par_mark :$cite_author_full_mark)
		,$cite_key),$_);
    }
    else {print "Cannot find citation argument\n";}
    $_;
}

sub cite_check_options {
# Check if there's an optional argument (even if it's empty)
# In this case, citation in parentheses is desired.
# If Harvard syntax is selected, just look for one nonempty optional
    if ($HARVARD) {
	my ($opt1,$dummy)=&get_next_optional_argument;	
# Always pretend there was an optional, since harvard \cite means
# parenthetical citation
	(1,$opt1,'')
    } else {
	my($hasopt) = (/^\s*\[([^]]*)\]/ && (! $`));
# Look for two possible optional arguments
        my($opt1,$dummy)= &get_next_optional_argument;
        my($opt2,$dummy)= &get_next_optional_argument;
# If optional Nr. 2 is present, exchange 1 and 2
        if ($opt2) {
            my($hopt)=$opt1;
	    $opt1=$opt2;
	    $opt2=$hopt;
        };
        ($hasopt,$opt1,$opt2)
   }
}

sub do_cite_keys{
# $hasopt indicates that citations should be enclosed in parentheses
    local($br_id,$hasopt,$first,$second,$c_mark,$cite_key) = @_;
    local(@cite_keys) = (split(/,/,$cite_key));
    local ($multiple,$cite_anchor,$key);
# Create index entries if desired
    if (($CITEINDEX) && (! $NUMERIC)) {
	foreach $key (@cite_keys) {$cite_anchor=&make_cite_index("$br_id",$key);};};
# Is there more than 1 citation ?
# If yes, the multiple citations are enclosed by $cite_multiple_mark's
    if ($#cite_keys > 0){ $multiple = $cite_multiple_mark;}
    else { $multiple = '';};
    my ($citauth)=($c_mark =~ /($cite_author_mark|$cite_full_author_mark)/);
    $first = "$POST_NOTE $first" if ($first && !($HARVARD && $citauth));
    grep ( do { &cite_check_segmentation($_);
# MW: change 25.6.: throw out the reference to $bbl_mark.
# The second pair of # remains empty unless we are in HARVARD mode
# and have a single citation with optional text
	$_ = "#$_#$c_mark#".(($HARVARD && (!$hasopt) && (!$multiple))? $first: "")."#";}, @cite_keys);
    # Add parentheses and delimiters as appropriate 
    #
    $second .= ' ' if ($second); 
    if ($hasopt) { 
	local($_)=join('', $CITE_OPEN_DELIM, $second,$multiple,
	    join($CITE_ENUM,@cite_keys),$multiple, $first, $CITE_CLOSE_DELIM );
    } else { 
	local($_)=join ('',$multiple,join($CITE_ENUM,@cite_keys),$multiple); 
    }
    join ('',$cite_anchor,$_);
}


sub make_cite_index {
    local ($br_id,$cite_key) =@_;
    local ($index_key)="$cite_short{$cite_key} ($cite_year{$cite_key})";
    local ($sort_key)="$cite_short{$cite_key}$cite_year{$cite_key}$cite_key";
#    local ($bib_label)="<A NAME=\"III${cite_key}\"<\/A>";
    if (defined  &named_index_entry ) {
	&named_index_entry($br_id,"$sort_key\@$index_key") }
    elsif ($br_id > 0) {
	&do_cmd_index("<\#$br_id\#>$index_key<\#$br_id\#>") }
    else { $name++; &do_cmd_index("<\#$name\#>$index_key<\#$name\#>") }
}


sub do_cmd_bibitem {
# Process the \bibitem command.
    local($_) = @_;
    local ($tmp,$label);
    $bbl_cnt++;
    local($label, $dummy) = &get_next_optional_argument;
# Check if label is of the natbib form ...(1994abc)...
    $tmp = ($label =~ /(.*)(\([^)]*?\))(.*)/s);
    my ($supported)= ($tmp && !($label =~ /\\protect/));
# Short name: before year, long name: after year
    local($short, $year, $long) = ($1,$2,($3 ? $3 : $1));
# If numeric citation is chosen -> standard procedure
    if (! $NUMERIC) { $year =~ s/[\(\)]//g; }
    else { $label=++$bibitem_counter; };
# Throw out brackets that may stem from 1990{\em a} or similar
    $year =~ s/<#\d+#>//g;
# The compulsory argument is the LaTeX label
    s/$next_pair_pr_rx//o;
    $cite_key = &translate_commands($2);
    if ($cite_key) {
# remove tags resulting from excess braces
	$tmp = $_;
	$_ = $short;
	s/$next_pair_pr_rx//o;
	if (!($2 eq $cite_key)) 
	    {$short =$2; $short =~ s/<\#[^\#>]*\#>//go; };
	$_ = $long;
	s/$next_pair_pr_rx//o;
	if (!($2 eq $cite_key))
	    {$long = $2; $long =~ s/<\#[^\#>]*\#>//go; };
	$_ = "$tmp";
# Three hashes are used to store the information for text citations
	if ($supported) {
	    $cite_short{$cite_key} = &translate_commands($short);
	    $cite_year{$cite_key} = &translate_commands($year);
	    $cite_long{$cite_key} = &translate_commands($long)}
	else {
	    print "\\bibitem label format not supported - using \\bibcite information!\n"};
# Update the $ref_file entry, if necessary, making sure changes are saved.
	if (!($ref_files{'cite_'."$cite_key"} eq $CURRENT_FILE)) {
	    $ref_files{'cite_'."$cite_key"} = $CURRENT_FILE;
	    $changed = 1; }
	$citefile{$cite_key} = $CURRENT_FILE;
# Create an anchor around the citation
	$_=&make_cite_reference ($cite_key,$_);
    } else {
	print "Cannot find bibitem labels: $label\n";
	$_=join('',"\n<DT><STRONG>$label</STRONG>\n<DD>", $_);
    }
    $_;
}

sub make_cite_reference {
# Make the anchor
    local ($cite_key,$_)=@_;
    local($label)=$cite_info{$cite_key};
    local($next_lines, $after_lines);
    local($sort_key, $indexdata);
    if (defined  &named_index_entry ) { #  makeidx.perl  is loaded
	$sort_key = "$cite_short{$cite_key}$cite_year{$cite_key}$cite_key"; 
        $sort_key =~ tr/A-Z/a-z/;
    } else {$sort_key = "$cite_short{$cite_key} ($cite_year{$cite_key})";}
    if ($index{$sort_key}) { 
# append the index entries as a list of citations
	$indexdata = $index{$sort_key};
	$indexdata =~ s/[\|] $//;
	$indexdata = join('',"\n<DD>cited: ", "$indexdata");
# Create index entry to the Bibliography entry only, if desired
	$index{$sort_key} = '';
	if ($CITEINDEX) { &make_cite_index("$cite_key",$cite_key);} 
	elsif (defined  &named_index_entry ) {$printable_key{$sort_key} = '';}
    } else { $indexdata = '';}
    $indexdata .= "\n<P>";

    local ($found) = /(\\bibitem|\\harvarditem)/o;
    if ($found) { $after_lines = $&.$'; $next_lines = $`;}
    else { $after_lines = ''; $next_lines = $_;}
    $next_lines .= $indexdata;
    $indexdata = '';
    $_ = $next_lines.$after_lines;

    if ($NUMERIC) {
	join('',"\n<DT><A NAME=\"$cite_key\"><STRONG>$label</STRONG></A>\n<DD>",$_);
    } else {
# For Author-year citation: Don't print the label to the bibliography
# Use the first line of the bib entry as description title instead
# First line ends with \newblock or with the next \bibitem  command
#	$found = /\\newblock/o;	# these have been converted to  <BR>s
	$found = /\<BR\>/o;
	if ($found) {
	    join('',"\n<DT><A NAME=\"$cite_key\"><STRONG>",
		 &translate_commands($`),"</STRONG></A>\n<DD>", 
# No call to &translate_commands on $': Avoid recursion
		    $');
	} else {
	    $found= /(\\bibitem|\\harvarditem)/o;
	    if ($found) {
		join('',"\n<DT><A NAME=\"$cite_key\"><STRONG>",
		    &translate_commands($`),"</STRONG></A>\n<DD>",
# No call to &translate_commands on $': Avoid recursion
		    $');
	    } else {
		join('',"\n<DT><A NAME=\"$cite_key\"><STRONG>",
		     &translate_commands($_),"</STRONG></A>\n<DD>",' ');
	    };
	};
    }
}

sub do_cmd_harvarditem {
# natbib.sty also reads files created by harvard bibstyles 
# (harvard, kluwer, ...)
    local ($_)=@_;
    local ($dum,$short)=&get_next_optional_argument;
    $short =~ s/[\[\]]//g;
    $bbl_cnt++;
# Get full citation text
    s/$next_pair_pr_rx//o; local ($long)=$2; 
# Get year
    s/$next_pair_pr_rx//o; local ($year)=$2;
    $year =~ s/<#\d+#>//g;
# Get the key
    s/$next_pair_pr_rx//o; local ($cite_key)=$2;
    if ($cite_key) {
	if (!($short)) {$short=$long};
# remove tags resulting from excess braces
	local($tmp) = $_;
	$_ = $short;
	s/$next_pair_pr_rx//o;
	if (!($2 eq $cite_key)) 
	    {$short =$2; $short =~ s/<\#[^\#>]*\#>//go; };
	$_ = $long;
	s/$next_pair_pr_rx//o;
	if (!($2 eq $cite_key))
	    {$long = $2; $long =~ s/<\#[^\#>]*\#>//go; };
	$_ = "$tmp";
# Three hashes are used to store the information for text citations
        $cite_short{$cite_key} = &translate_commands($short);
        $cite_year{$cite_key} = &translate_commands($year);
        $cite_long{$cite_key} = &translate_commands($long);
# Update the $ref_file entry, if necessary, making sure changes are saved.
# $citefile is set by  do_env_thebibliography
#	$citefile{$cite_key} = $citefile;
	if (!($ref_files{'cite_'."$cite_key"} eq $CURRENT_FILE)) {
	    $ref_files{'cite_'."$cite_key"} = $CURRENT_FILE;
	    $changed = 1; }
	$citefile{$cite_key} = $CURRENT_FILE;
	&make_harvard_reference ($cite_key,$year,$_);
    } else {
	print "Cannot find bibitem labels: $label\n";
	join('',"\n<DT><STRONG>$label</STRONG>\n<DD>", $_);
    }
}

sub make_harvard_reference {
# Almost the same as &make_cite_reference.
    local ($cite_key,$year,$_)=@_;
    local($label)=$cite_info{$cite_key};
    local($next_lines, $after_lines);
    local($sort_key, $indexdata);
    if (defined  &named_index_entry ) { #  makeidx.perl  is loaded
	$sort_key = "$cite_short{$cite_key}$cite_year{$cite_key}$cite_key"; 
        $sort_key =~ tr/A-Z/a-z/;
    } else {$sort_key = "$cite_short{$cite_key} ($cite_year{$cite_key})";}
    if ($index{$sort_key}) { 
# append the index entries as a list of citations
	$indexdata = $index{$sort_key};
	$indexdata =~ s/[\|] $//;
	$indexdata = join('',"\n<DD>cited: ", "$indexdata");
# Create index entry to the Bibliography entry only, if desired
	$index{$sort_key} = '';
	if ($CITEINDEX) { &make_cite_index("$cite_key",$cite_key);} 
	elsif (defined  &named_index_entry ) {$printable_key{$sort_key} = '';}
    } else { $indexdata = '';}
    $indexdata .= "\n<P>";
    local ($found) = /(\\bibitem|\\harvarditem)/o;
    if ($found) { $after_lines = $&.$'; $next_lines = $`;}
    else { $after_lines = ''; $next_lines = $_;}
    $next_lines .= $indexdata;
    $indexdata = '';
    $_ = $next_lines.$after_lines;
    if ($NUMERIC) {
	join('',"\n<DT><A NAME=\"$cite_key\"><STRONG>$label</STRONG></A>\n<DD>",$_);
    } else {
# For Author-year citation: Don't print the label to the bibliography
# Difference to &make_cite_reference:
# \newblocks are not be used, so use the year stored in $year as delimiter
# for the first line
#	local ($found)= /$year([.:;,\s\)\]\!\?\}]|\\harvardyearright)*/s;
# Extract the numeric part of the year, to avoid confusion by 1991{\em b} or similar
	$year =~ /\d+/;
	my ($numyear) = $&;
# Look for the year followed by anything and a punctuation character or newline
	local ($found)= /$numyear(.*?)[.,:;\n]/s;
	if ($found) {
	    join('',"\n<DT><A NAME=\"$cite_key\"><STRONG>",
		 &translate_commands($`.$&),"</STRONG></A>\n<DD>", 
# No call to &translate_commands on $': Avoid recursion
		 $')
	} else {
	    $found= /(\\bibitem|\\harvarditem)/o;
	    if ($found) {
		join('',"\n<DT><A NAME=\"$cite_key\"><STRONG>",
		     &translate_commands($`),"</STRONG></A>\n<DD>",
# No call to &translate_commands on $': Avoid recursion
		     $');
	    } else {
		join('',"\n<DT><A NAME=\"$cite_key\"><STRONG>",
		     &translate_commands($_),"</STRONG></A>\n<DD>",' ');
	    };
	};
    }
}

sub do_cmd_harvardand {
    &translate_commands("$HARVARDAND".$_[0]);
}
sub do_cmd_harvardleft {
    &translate_commands("$CITE_OPEN_DELIM".$_[0]);
}
sub do_cmd_harvardright {
    &translate_commands("$CITE_CLOSE_DELIM".$_[0]);
}
sub do_cmd_harvardyearleft {
    &translate_commands("$CITE_OPEN_DELIM".$_[0]);
}
sub do_cmd_harvardyearright {
    &translate_commands("$CITE_CLOSE_DELIM".$_[0]);
}
sub do_cmd_harvardurl{ 
    if (defined &do_cmd_htmladdnormallink) { 
	&translate_commands("\\htmladdnormallink".$_[0]);}
    else {
	print STDERR "\n\nload the html.perl package for  \\harvardurl\n\n";
	&translate_commands("\\texttt".$_[0]);
    }
}

sub do_cmd_bibcite {
# !! This routine reads bibcite commands produced by natbib 6.0 or later !!
# It is used to build the citation information
# (hash tables %cite_info, %cite_short, %cite_long, %cite_year)
    local($_) = @_;
	# extract the key
    s/$next_pair_pr_rx//o;
    local($br_id, $cite_key) = ($1, $2);
	# next group is the information
#    $cite_key =~ s/\W//g;
    s/$next_pair_pr_rx//o;
    local($br_id, $print_key) = ($1, $2);
    local($rest) = "$_";
    $_ = $print_key;
	# first is the numeric value...
    s/$next_pair_pr_rx//o;
    ($br_id, $print_key) = ($1, $2);
    $print_key =~ s/<\#[^\#>]*\#>//go;
# Complain if no label is found: This is not a proper natbib \bibcite command
    print ("\nWARNING: natbib.perl: no valid citation key found in \bibitem.",
	   "\n    Perhaps you are running a natbib.sty version earlier than 6.x?",
	   "\n    Unable to generate citation references correctly.\n")
	if (! $print_key);
    $cite_info{$cite_key} = &translate_commands($print_key);
	# then comes the year
    s/$next_pair_pr_rx//o;
    ($br_id, $print_key) = ($1, $2);
    $print_key =~ s/<\#[^\#>]*\#>//go;
    $cite_year{$cite_key} = &translate_commands($print_key);
# then the short citation
    s/$next_pair_pr_rx//o;
    ($br_id, $print_key) = ($1, $2);
    $print_key =~ s/<\#[^\#>]*\#>//go;
    $cite_short{$cite_key} = &translate_commands($print_key);
	# then the long citation
    s/$next_pair_pr_rx//o;
    ($br_id, $print_key) = ($1, $2);
    $print_key =~ s/<\#[^\#>]*\#>//go;
    if ($print_key) {
	$cite_long{$cite_key} = &translate_commands($print_key);}
    else {$cite_long{$cite_key}=$cite_short{$cite_key}};
# Switch to numeric mode if author or year is undefined
# (this happens if natbib.sty is used with a numerical bibstyle like 
# "plain.bst")
    $NUMERIC=($NUMERIC || 
	      (! $cite_short{$cite_key}) || 
	      (! $cite_year{$cite_key}));
    # just in case anything is left over...
    $rest;
}

sub do_cmd_harvardcite {
# This is used to build the citation information
# (hash tables %cite_info, %cite_short, %cite_long, %cite_year)
# from \harvardcite commands produced by the harvard package.
    local($_) = @_;
	# extract the key
    s/$next_pair_pr_rx//o;
    local($br_id, $cite_key) = ($1, $2);
	# next group is the long citation
    s/$next_pair_pr_rx//o;
    $cite_long{$cite_key}=&translate_commands($2);
	# next group is the short citation
    s/$next_pair_pr_rx//o;
    $cite_short{$cite_key}=&translate_commands($2);
	# next group is the year
    s/$next_pair_pr_rx//o;
    $cite_year{$cite_key}=&translate_commands($2);
    $cite_year{$cite_key} =~ s/<#\d+#>//g;
    $_;
}

# Now come to the correct replacements for all citation styles.
# Text is assembled from the 
# $cite_short, $cite_year, and $cite_long hash tables
sub replace_cite_references_hook { 
# Handle multiple citations first!
    if (/$cite_multiple_mark/) {&replace_multiple_cite_references };
    &replace_nat_cite_references if 
/$cite_mark|$cite_full_mark|$cite_year_mark|$cite_par_mark|$cite_par_full_mark|$cite_author_mark|$cite_author_full_mark|$citealt_mark|$citealt_full_mark/;
}

sub replace_multiple_cite_references {
# Look for $cite_multiple_mark pairs
    while 
	(s/$cite_multiple_mark(.*?)$cite_multiple_mark/&do_multiple_citation($1)/se) {;};
}

sub do_multiple_citation {
    local ($cit)=@_;
    my ($before_year,$after_year);
    my ($author,$thisyear,$lastyear,$lastauth,$theauth,$year);
    my ($thetext,$lasttext,$thekey,$lastkey);
    my ($mark,%second,@sorted);
# Clear arrays & hash tables
    undef %second;
    undef @sorted;
# Construct hash table with the labels of the multiple citation as keys
# (Values of hash %second are actually unimportant)
    while 
#	($cit =~ s/#(\w+)#($cite_mark|$cite_par_mark|$cite_full_mark|$cite_par_full_mark|$citealt_mark|$citealt_full_mark)#([^#]*)#($CITE_ENUM)?//) {
	($cit =~ s/#([^#]+)#($cite_mark|$cite_par_mark|$cite_full_mark|$cite_par_full_mark|$citealt_mark|$citealt_full_mark)#([^#]*)#($CITE_ENUM)?//) {
	 $mark=$2;
	 %second=(%second,$1,$3);
     };
	
    if ($NUMERIC) {
# Numerical Citation: normal procedure
# sort the entries in ascending bibliographic order
# DO WE REALLY WANT THIS ??
	@sorted=sort {$cite_info{$a} cmp $cite_info{$b}} (keys (%second));
	$_=join($CITE_ENUM,
# make_href is used for anchor creation!
		map { &make_href("$citefile{$_}#$_","$cite_info{$_}");}
		@sorted);
    } else {
# Author-year citation
# Different punctuation for parenthetical, normal, and alternative
# Author-year citation
# citations (\cite[] or \citep, \cite, and \citealt resp. starred versions)
      SWITCH:	{
# Parenthetical type (\cite[],\citep)
 	  $mark =~ /^$cite_par_mark|$cite_par_full_mark/ && do {
	      ($before_year,$after_year)=($BEFORE_PAR_YEAR,'');
	      last SWITCH;};
# normal type (\cite)
	  $mark =~ /^$cite_mark|$cite_full_mark/ && do {
	      ($before_year,$after_year)=
		  (" $CITE_OPEN_DELIM","$CITE_CLOSE_DELIM");
	      last SWITCH;};
# alternative type (\citealt)
	  ($before_year,$after_year)=(' ','');}
# Reference $author is set to %cite_long if full author name citation is
# requested, to  %cite_short otherwise
	if ($mark =~ /^$cite_par_full_mark|$cite_full_mark|$citealt_full_mark/) 
	{$author=\%cite_long;} else {$author=\%cite_short;};
# Sort the citation list according to author and year fields
#   => only subsequent entries must be compared afterwards.
# The citations are always sorted in ascending alphabetic order!
# DO WE REALLY WANT THIS ??
	@sorted = sort 
	{$$author{$a}.$cite_year{$a} cmp $$author{$b}.$cite_year{$b}} 
	(keys (%second));
# First entry
	$lastkey=shift(@sorted);
	($lastauth,$lastyear)=($$author{$lastkey},$cite_year{$lastkey});
	$lasttext=join('',$$author{$lastkey},
		       $before_year,
		       $cite_year{$lastkey});
	$_='';
# The text for the entry can only be written to $_ AFTER the next entry
# was analyzed (different punctuation whether next entry has the same authors
# or not!)
#
# iterate through the other entries
	while ($thekey=shift(@sorted)) {
	    ($theauth,$theyear)=($$author{$thekey},$cite_year{$thekey});
	    if ($lastauth eq $theauth) {
# If authors are the same as last entry: suppress printing
# author information
# Truncate last year field to numeric part ("1994", usually)
		$lastyear =~ /^\d+/;
		$year=$&;
# If year is equal to that of last entry, keep only additional info
# I.e. 1994a, 1994b -> 1994a,b 		    
		$thetext=($theyear =~ /^$year/ ? $' : ' '.$theyear);
# This line is for bibstyles that don't distinguish articles with
# common author list & year by appending letters (1990a, 1990b)
# In this case, $thetext might be empty after execution of the last line
		$thetext=' '.$theyear unless ($thetext);
# At this point, the PRECEDING entry may be written to $_
# Note use of &make_href.
		$_=join('',
			$_,
			&make_href("$citefile{$lastkey}#$lastkey",
				   $lasttext),
			$COMMON_AUTHOR_SEP);
	    } else {
# New author(s): new list entry
# The last entry needs an $after year character (e.g., ")"), since it's
# the last one in a series of articles by common authors
# This character should go into the anchor text.
		$lasttext=$lasttext.$after_year;
# The new entry will be printed out completely
		$thetext=join('',
			      $theauth,$before_year,$theyear);
# Write the preceding entry
		$_=join('',
			$_,
			&make_href("$citefile{$lastkey}#$lastkey",
				   $lasttext),
			$CITE_ENUM);
	    };
# Shift last entry
	    ($lastkey,$lastauth,$lastyear,$lasttext)=
		($thekey,$theauth,$theyear,$thetext);
	};
# write the last entry of the list
	$_=join('',$_,
		&make_href("$citefile{$lastkey}#$lastkey",
			   $lasttext.$after_year));
    };
    $_;
}

sub replace_nat_cite_references { 
# Modifies $_
# Uses $citefile set by the thebibliography environment
# Creates hyperrefs EXCLUSIVELY by calling &make_href
# Note that %citefile is indexed by the latex label ($1) rather than $bbl_nr ($2)
# MW 25.6.96: second pair of #'s may now be empty!
    if ($NUMERIC) {
	s/#([^#]+)#$cite_par_mark#([^#]*)#/&make_named_href("","$citefile{$1}#$1",$cite_info{$1})/ge;
	s/#([^#]+)#$cite_mark#([^#]*)#/&make_named_href("",
		"$citefile{$1}#$1","$cite_short{$1} $CITE_OPEN_DELIM"."$cite_info{$key}$CITE_CLOSE_DELIM")/ge;
	s/#([^#]+)#$cite_full_mark#([^#]*)#/&make_named_href("",
		"$citefile{$1}#$1","$cite_long{$1} $CITE_OPEN_DELIM"."$cite_info{$1}$CITE_CLOSE_DELIM")/ge;
    } else {
# MW 25.6.96: use $2 eventually as optional text for harvard citations
	s/#([^#]+)#$cite_mark#([^#]*)#/&make_named_href("","$citefile{$1}#$1","$cite_short{$1} ".
		"$CITE_OPEN_DELIM$cite_year{$1}$2$CITE_CLOSE_DELIM")/ge;
	s/#([^#]+)#$cite_full_mark#([^#]*)#/&make_named_href("",
		"$citefile{$1}#$1","$cite_long{$1} "."$CITE_OPEN_DELIM$cite_year{$1}$2$CITE_CLOSE_DELIM")/ge;
        if ($HARVARD) {
# in HARVARD mode, $citealt_mark stands for \possessivecite commands
           s/#([^#]+)#$citealt_mark#([^#]*)#/&make_named_href("",
                "$citefile{$1}#$1","$cite_short{$1}\'s "."$CITE_OPEN_DELIM$cite_year{$1}$2$CITE_CLOSE_DELIM")/ge;
	   s/#([^#]+)#$citealt_full_mark#([^#]*)#/&make_named_href("",
                "$citefile{$1}#$1","$cite_long{$1}\'s "."$CITE_OPEN_DELIM$cite_year{$1}$2$CITE_CLOSE_DELIM")/ge
        } else {
# in usual natbib mode, it stands for \citealt commands
           s/#([^#]+)#$citealt_mark#([^#]*)#/&make_named_href("",
                "$citefile{$1}#$1","$cite_short{$1} $cite_year{$1}")/ge;
	   s/#([^#]+)#$citealt_full_mark#([^#]*)#/&make_named_href("",
                "$citefile{$1}#$1","$cite_long{$1} $cite_year{$1}")/ge
        }
	s/#([^#]+)#$cite_par_mark#([^#]*)#/&make_named_href("",
		"$citefile{$1}#$1","$cite_short{$1}$BEFORE_PAR_YEAR$cite_year{$1}")/ge;
	s/#([^#]+)#$cite_par_full_mark#([^#]*)#/&make_named_href("",
		"$citefile{$1}#$1","$cite_long{$1}$BEFORE_PAR_YEAR$cite_year{$1}")/ge;
	s/#([^#]+)#$cite_year_mark#([^#]*)#/&make_named_href("","$citefile{$1}#$1","$cite_year{$1}$2")/ge;
	s/#([^#]+)#$cite_author_mark#([^#]*)#/&make_named_href("","$citefile{$1}#$1","$cite_short{$1}".($2? " $CITE_OPEN_DELIM$2$CITE_CLOSE_DELIM": ""))/ge;
	s/#([^#]+)#$cite_author_full_mark#([^#]*)#/&make_named_href("","$citefile{$1}#$1","$cite_long{$1}".($2? " $CITE_OPEN_DELIM$2$CITE_CLOSE_DELIM": ""))/ge;
    }
}

# This subroutine must be extended such that pages containing non-standard 
# citations are also cleaned
sub remove_general_markers {
    s/$lof_mark/<UL>$figure_captions<\/UL>/o;
    s/$lot_mark/<UL>$table_captions<\/UL>/o;
    if (defined &replace_citations_hook) {&replace_citations_hook;}
    else {&replace_citations if /$bbl_mark/;}
    if (defined &add_toc_hook) {&add_toc_hook;}
    else {&add_toc if (/$toc_mark/);}
    if (defined &add_idx_hook) {&add_idx_hook;}
    else {&add_idx if (/$idx_mark/);}
    if (defined &replace_cross_references_hook) {&replace_cross_references_hook;}
    else {&replace_cross_references if /$cross_ref_mark/;}
    if (defined &replace_external_references_hook) {&replace_external_references_hook;}
    else {&replace_external_references if /$external_ref_mark/;}
    if (defined &replace_cite_references_hook) {&replace_cite_references_hook;}
    else { &replace_cite_references if /$cite_mark/; }
    if (defined &replace_user_references) {
 	&replace_user_references if /$user_ref_mark/;
    }
}  

sub cite_check_segmentation {
    local($c_key)=@_;
# This is based on code by Ross Moore
# Important for file segments
# It sets $citefile from the hash $ref_files:
    if  ($ref_files{"cite_$c_key"})  {
	$citefile{$c_key} = $ref_files{"cite_$c_key" };
    };
## or $external_labels:   not needed (RRM)
# elsif  ($external_labels{"cite_$c_key"}) {
#	$citefile{$c_key} = $external_labels{"cite_$c_key" };};
    $citefile{$c_key};
}    

sub do_env_thebibliography {
    # Sets $citefile and $citations defined in translate
    # gives a nicely formatted .html file --- RRM
    local($_) = @_;
    $bibitem_counter = 0;
    $citefile = $CURRENT_FILE;
    $citefile{$bbl_nr} = $citefile;
    s/$next_pair_rx//o;
    $* = 1;			# Multiline matching ON
    s/^\s*$//g;	# Remove empty lines (otherwise will have paragraphs!)
    s/\n//g;	# Remove all \n s --- we format the HTML file ourselves.
    s/\\newblock/\n\<BR\>/g;	# break at each \newblock
    $* = 0;			# Multiline matching OFF
    s/(\\bibitem|\\harvarditem)//o;	# skip to the first bibliography entry
    $_ = $&.$';
    $citations = join('',"\n<DL COMPACT>",
		      &translate_commands(&translate_environments($_)),"\n</DL>");
    $citations{$bbl_nr} = $citations;
    $_ = join('','<P>' , "\n<A NAME=\"SECTIONREF\"><H2>$bib_title</H2></A><P>\n$bbl_mark#$bbl_nr#");
    $bbl_nr++ if $bbl_cnt > 1;
    $_;
}

sub do_cmd_bibpunct {
    local($_) = @_;
# six arguments 
    local($post, $dummy) = &get_next_optional_argument;
    $POST_NOTE=$post." " if ($post);
    s/$next_pair_pr_rx//o;
    $CITE_OPEN_DELIM=$2;
    s/$next_pair_pr_rx//o;
    $CITE_CLOSE_DELIM=$2;
    s/$next_pair_pr_rx//o;
    $CITE_ENUM=$2." " if ($2);
    s/$next_pair_pr_rx//o;
    my ($style)=$2;
    $NUMERIC=($style =~ /[ns]/);
    s/$next_pair_pr_rx//o;
    $BEFORE_PAR_YEAR=$2." " if ($2);
    s/$next_pair_pr_rx//o;
    $COMMON_AUTHOR_SEP=$2;
    $_;
}

sub do_cmd_citeindexfalse {
    $CITEINDEX=0; $_[0];
}

sub do_cmd_citeindextrue {
    $CITEINDEX=1; $_[0];
}

sub do_cmd_citestyle {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    my ($style)="citestyle_$2";
    if (defined @$style) {
	($CITE_OPEN_DELIM,
	 $CITE_CLOSE_DELIM,
	 $CITE_ENUM,
	 $NUMERIC,
	 $BEFORE_PAR_YEAR,
	 $COMMON_AUTHOR_SEP)=@$style;
	$NUMERIC=($NUMERIC =~ /[sn]/);
	my ($and)="HARVARDAND_$2";
	defined $$and && do { $HARVARDAND=$$and }
    } else { print "\nnatbib.perl: invalid argument to \\citestyle!" };
    $_;
}

sub do_cmd_citationstyle {
    &do_cmd_citestyle 
}

&ignore_commands ( <<_IGNORED_CMDS_);
bibsection # {}
bibfont # {}
bibhang # &ignore_numeric_argument
bibsep # &ignore_numeric_argument
citeindextype # {}
harvardyearparenthesis # {}
_IGNORED_CMDS_

1;                              # This must be the last line
