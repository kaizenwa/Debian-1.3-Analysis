# justify.perl  by Ross Moore <ross@mpce.mq.edu.au> 29-MAY-96
# Mathematics Department, Macquarie University, Sydney.
#
# Extension to LaTeX2HTML to translate LaTeX commands
# for justified paragraphs and environments.
# The HTML markup is not part of HTML 2.0 but it part
# of the recommendations for HTML 3.2.
# Currently it is recognised by popular browsers;
# e.g. Netscape Navigator.
#
#

package main;

print "\n*********************\n";
print "The justify.perl package allows HTML markup to be produced \nwhich is not conforming to the HTML 2.0 standard.\n";
print "Not all Web-browsers can be expected to correctly display this markup.";
print "\n*********************\n\n";


# implements the LaTeX environments 
#  {center}, {flushleft}, {flushright}

sub do_env_center {
    join('',"<P ALIGN=CENTER>","@_","</P>");}

sub do_env_flushleft {
    join('',"<P ALIGN=LEFT>","@_","</P>");}

sub do_env_flushright {
    join('',"<P ALIGN=RIGHT>","@_","</P>");}



# implements the TeX commands 
#  \centerline, \leftline, \rightline

sub do_cmd_centerline {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    join('',"<P ALIGN=CENTER>$2</P>\n",$_);
}

sub do_cmd_leftline {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    join('',"<P ALIGN=LEFT>$2</P>\n",$_);
}

sub do_cmd_rightline {
    local($_) = @_;
    s/$next_pair_pr_rx//o;
    join('',"<P ALIGN=RIGHT>$2</P>\n",$_);
}



1;                              # This must be the last line
