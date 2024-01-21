# CHANGEBAR.PERL by Nikos Drakos <nikos@cbl.leeds.ac.uk> 4-AUG-94
# Computer Based Learning Unit, University of Leeds.
#
# Extension to LaTeX2HTML to translate commands defined in 
# the changebar.sty file (c) 1990 by David B. Johnson 
# (dbj@titan.rice.edu). It also supports some commands from
# changebars.sty by Michael Fine and 
# Johannes Braams <J.L.Braams@research.ptt.nl>
#
#
# Modifications:
#
# nd = Nikos Drakos <nikos@cbl.leeds.ac.uk>
# hs = Herb Swan    <dprhws@edp.Arco.com>
# mg = Michel Goossens <goossens@cern.ch>
# rm = Ross Moore <ross@mpce.mq.edu.au>
#
# nd  4-AUG-94 - Created
# hs 20-DEC-95 - Removed arguments from chgbarwidth and chgbarsep
# mg 14-Jan-96 - added more commands for Braams's changebar package
# rm 10-Apr-96 - added version-control, using  \cbversion{..}

package main; 

sub do_cmd_chgbarbegin {
    &do_cmd_cbstart;
}

sub do_cmd_chgbarend {
    &do_cmd_cbend;
}

&ignore_commands( <<_IGNORED_CMDS_);
chgbarwidth
chgbarsep
driver
changebarwidth
changebarsep
changebargrey
deletebarwidth
outerbars
nochangebars
cb_at_barpoint # {} # {} # {}
_IGNORED_CMDS_

$cb_version = '';	# RRM  string for version control, initially empty.

# RRM
# This routine is currently redundant, as the environments get processed
# before individual commands. However this may change with V96.3.
#
sub do_cmd_cbversion{
    local($_) = @_;
    s/$next_pair_pr_rx/$cb_version=$2;''/eo;
    $_;
}


sub do_cmd_cbstart{
    if ($cb_version) {
	join('',"<BR>$change_begin_visible_mark<I>$cb_version</I><BR>\n",@_);
    } else {
	join('',"<BR>$change_begin_visible_mark<BR>\n",@_);
    }
}

sub do_cmd_cbend{
    if ($cb_version) {
	join('',"<BR>$change_end_visible_mark<I> $cb_version</I><BR>\n",@_);
    } else {
	join('',"<BR>$change_end_visible_mark<BR>\n",@_);
    }
}

sub do_cmd_cbdelete{
    if ($cb_version) {
	join('',"<BR>$change_delete_visible_mark<I>$cb_version</I><BR>\n",@_);
    } else {
	join('',"<BR>$change_delete_visible_mark<BR>\n",@_);
    }
}


# RRM
# Look for a \cbversion{..} command as first thing in the environment.
# If found, use its argument for  $cb_version .
#
sub do_env_changebar {
    &set_chgbar_preamble;
    local($_) = @_;
    local($next,$pat) = ('','');
    ($next,$pat) = &get_next_tex_cmd; 
    if ($next eq "cbversion") { s/$next_pair_rx/$cb_version=$2;''/eo; };
    if ($cb_version) {
	join('',"<BR>$change_begin_visible_mark<I>$cb_version</I><BR>\n@_",
	    "\n<BR>$change_end_visible_mark<I> $cb_version</I><BR>\n");
    } else {
	join('',"<BR>$change_begin_visible_mark<BR>\n@_",
	    "\n<BR>$change_end_visible_mark<BR>\n");
    }
}

sub set_chgbar_preamble {
    $preamble .= "\\def\\cb\@barpoint#1#2#3{}\n"
    unless $preamble =~ /cb\@barpoint/;
}

1;				# This must be the last line



