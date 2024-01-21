#!/usr/local/bin/perl -w

#
# AltaVista.pm
# by John Heidemann
# Copyright (C) 1996 by USC/ISI
# $Id: AltaVista.pm,v 1.17 1996/11/21 23:02:14 johnh Exp $
#
# Complete copyright notice follows below.
# 


package WWW::Search::AltaVista;

=head1 NAME

WWW::Search::AltaVista - class for searching Alta Vista 

=head1 DESCRIPTION

This class is an AltaVista specialization of WWW::Search.
It handles making and interpreting AltaVista searches
F<http://www.altavista.digital.com>.

This class exports no public interface; all interaction should
be done through WWW::Search objects.


=head1 OPTIONS

The default is for simple web queries.
Specialized back-ends for simple and advanced web and news searches
are available (see
L<WWW::Search::AltaVista::Web>,
L<WWW::Search::AltaVista::AdvancedWeb>,
L<WWW::Search::AltaVista::News>,
L<WWW::Search::AltaVista::AdvancedNews>).
These back-ends set different combinations following options.

=over 8

=item pg=aq

Do advanced queries.
(It defaults to simple queries.)

=item what=news

Search Usenet instead of the web.
(It defaults to search the web.)

=back


=head1 SEE ALSO

To make new back-ends, see L<WWW::Search>,
or the specialized AltaVista searches described in options.


=head1 HOW DOES IT WORK?

C<native_setup_search> is called before we do anything.
It initializes our private variables (which all begin with underscores)
and sets up a URL to the first results page in C<{_next_url}>.

C<native_retrieve_some> is called (from C<WWW::Search::retrieve_some>)
whenever more hits are needed.  It calls the LWP library
to fetch the page specified by C<{_next_url}>.
It parses this page, appending any search hits it finds to 
C<{cache}>.  If it finds a ``next'' button in the text,
it sets C<{_next_url}> to point to the page for the next
set of results, otherwise it sets it to undef to indicate we're done.


=head1 AUTHOR

C<WWW::Search::AltaVista> is written by John Heidemann, <johnh@isi.edu>.


=head1 COPYRIGHT

Copyright (c) 1996 University of Southern California.
All rights reserved.                                            
                                                               
Redistribution and use in source and binary forms are permitted
provided that the above copyright notice and this paragraph are
duplicated in all such forms and that any documentation, advertising
materials, and other materials related to such distribution and use
acknowledge that the software was developed by the University of
Southern California, Information Sciences Institute.  The name of the
University may not be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.


=cut
#'

#
#  Test cases:
# ./altavista.pl xxxasdf                        --- no hits
# ./altavista.pl '"lsam replication"'           --- single page return
# ./altavista.pl '+"john heidemann" +work'      --- 9 page return
#



#####################################################################

require Exporter;
@EXPORT = qw();
@EXPORT_OK = qw();
@ISA = qw(WWW::Search Exporter);

use Carp ();
require WWW::SearchResult;



# private
sub native_setup_search
{
    my($self, $native_query, $native_options_ref) = @_;
    $self->user_agent('user');
    $self->{_next_to_retrieve} = 0;
    if (!defined($self->{_default_options})) {
	$self->{_default_options} = {
	    pg => 'q',
	    what => 'web',
	    fmt => 'd',
        };
    };
    my($options_ref) = $self->{_default_options};
    if (defined($native_options_ref)) {
	# copy options
	foreach (keys %$native_options_ref) {
	    $options_ref->{$_} = $native_options_ref->{$_};
	};
    };
    # process the options
    my($options) = '';
    foreach (keys %$options_ref) {
	$options .= $_ . '=' . $options_ref->{$_} . '&';
    };
    
    $self->{_base_url} = 
	$self->{_next_url} =
	"http://www.altavista.digital.com/cgi-bin/query?" . $options .
	"q=" . $native_query;
}


# private
sub native_retrieve_some
{
    my ($self) = @_;

    # fast exit if already done
    return undef if (!defined($self->{_next_url}));

    # get some
    my($request) = new HTTP::Request('GET', $self->{_next_url});
    my($response) = $self->{user_agent}->request($request);
    $self->{response} = $response;
    if (!$response->is_success) {
	return undef;
    };

    # parse the output
    my($HEADER, $HITS, $TRAILER) = (1..10);
    my($hits_found) = 0;
    my($state) = ($HEADER);
    my($hit) = ();
    foreach (split(/\n/, $response->content())) {
         next if m@^$@; # short circuit for blank lines
	 if ($state == $HEADER && /Documents?.*of\s*(about)?\s*(\d+)\s+matching/) {
	    $self->approximate_result_count($2);
	    $state = $HITS;
	} elsif ($state == $HITS && m@^(<[Pp]><dt>|<dt>)<a href=\"([^"]+)"><strong>(.*)</strong></a><dd>(.*)(\.)?<br>@) {
	    if (defined($hit)) {
	        push(@{$self->{cache}}, $hit);
	    };
	    $hit = new WWW::SearchResult;
	    $hit->add_url($2);
	    $hits_found++;
	    $hit->title($3);
	    $hit->description($4.$5);
	} elsif ($state == $HITS && m@^(<[Pp]><dt>|<dt>)<a href=\"([^"]+)"><strong>(.*)</strong></a><dd>(.*)<br><a href=\"([^"]+)">@) {
	    # news is slightly different
	    if (defined($hit)) {
	        push(@{$self->{cache}}, $hit);
	    };
	    $hit = new WWW::SearchResult;
	    $hit->add_url($2);   # AltaVista's news gateway URL
	    $hits_found++;
	    $hit->title($3);
	    $hit->description($4);
	    $hit->add_url($5);   # news: URL
	    $hits_found++;
	} elsif ($state == $HITS && /^<cite><a href="([^"]+)">/) { #"
	    if (defined($hit)) {
	        $hit->add_url($1);
	        $hits_found++;   # altavista counts URL==hit
	    };
	} elsif ($state == $HITS && /^<CENTER>.*\s+p\./) {
	    # end, with a list of other pages to go to
	    if (defined($hit)) {
	        push(@{$self->{cache}}, $hit);
	    };
	    if (/Next\]/) {
		# set up next page
		my($relative_url) = m@<a\s+href="([^"]+)">\s*\[\s*[Nn]ext\s*\]\s*</a>@; #"
		$self->{_next_url} = new URI::URL($relative_url, $self->{_base_url});
	    } else {
		$self->{_next_url} = undef;
	    };
	    $state = $TRAILER;
	};
    };
    if ($state != $TRAILER) {
	# end, no other pages (missed ``next'' tag)
	if (defined($hit)) {
	    push(@{$self->{cache}}, $hit);
	};
	$self->{_next_url} = undef;
    };

    # sleep so as to not overload altavista
    $self->user_agent_delay if (defined($self->{_next_url}));

    return $hits_found;
}

1;
