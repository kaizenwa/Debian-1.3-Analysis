# verbatim.perl by Jens Lippmann <lippmann@cs.uni-sb.de> 6-2-96
#
# Extension to LaTeX2HTML V 96.1 to supply support for the
# "verbatim" LaTeX package.
#
# Change Log:
# ===========
#  jcl = Jens Lippmann <http://www-jb.cs.uni-sb.de/~www/people/lippmann>
#
# jcl  2-JUN-96 - Created


package main;

sub do_cmd_verbatimfile {
    local($outer) = @_;
    local($_);

    $outer =~ s/$next_pair_pr_rx//o;
    local($file) = $2;
    $file .= ".tex" unless $file =~ /\.tex$/;

    foreach $dir ("$texfilepath", split(/:/,$LATEX2HTMLSTYLES)) { 
	if (-f ($_ = "$dir/$file")) {
	    #overread $_ with file contents
	    &slurp_input($_);
	    last;
	}
    }
    # pre_process file contents
    &replace_html_special_chars;

    $verbatim{++$global{'verbatim_counter'}} = $_;
    join('',"<BR>\n",$verbatim_mark,'verbatim',$global{'verbatim_counter'},$outer);
}

sub do_cmd_verbatimlisting {
    local($outer) = @_;
    local($_);
    local($counter) = 0;

    $outer =~ s/$next_pair_pr_rx//o;
    local($file) = $2;
    $file .= ".tex" unless $file =~ /\.tex$/;

    foreach $dir ("$texfilepath", split(/:/,$LATEX2HTMLSTYLES)) { 
	if (-f ($_ = "$dir/$file")) {
	    #overread $_ with file contents
	    &slurp_input($_);
	    last;
	}
    }
    # pre_process file contents
    &replace_html_special_chars;

    #insert line numbers
    s/(^|\n)(([^\n])+)/$1.sprintf("%4d ",++$counter).$2/ge;

    $verbatim{++$global{'verbatim_counter'}} = $_;
    join('',"<BR>\n",$verbatim_mark,'verbatim',$global{'verbatim_counter'},$outer);
}

1; 		# Must be last line
