$| = 1; # autoflush

require IO::Socket;  # make sure this work before we try to make a HTTP::Daemon

# First we make ourself a daemon in another process

unless (open(DAEMON, "-|")) {

    require HTTP::Daemon;

    my $d = new HTTP::Daemon Timeout => 10;

    print "Please to meet you at: <URL:", $d->url, ">\n";
    open(STDOUT, ">/dev/null");

    while ($c = $d->accept) {
	$r = $c->get_request;
	if ($r) {
	    my $p = ($r->url->path_components)[1];
	    $p =~ s/\W//g;
	    my $func = lc("httpd_" . $r->method . "_$p");
	    #print STDERR "Calling $func...\n";
            if (defined &$func) {
		&$func($c, $r);
	    } else {
		$c->send_error(404);
	    }
	}
	$c = undef;  # close connection
    }
    print STDERR "HTTP Server terminated\n";
    exit;
}

print "1..7\n";


$greating = <DAEMON>;
$greating =~ /(<[^>]+>)/;

require URI::URL;
URI::URL->import;
my $base = new URI::URL $1;

print "Will access HTTP server at $base\n";

require LWP::RobotUA;
require HTTP::Request;
$ua = new LWP::RobotUA 'lwp-spider/0.1', 'gisle@aas.no';
$ua->delay(0.05);  # rather quick robot

#----------------------------------------------------------------
sub httpd_get_robotstxt
{
   my($c,$r) = @_;
   $c->send_basic_header;
   $c->print("Content-Type: text/plain");
   $c->send_crlf;
   $c->send_crlf;
   $c->print("User-Agent: *
Disallow: /private

");
}

sub httpd_get_someplace
{
   my($c,$r) = @_;
   $c->send_basic_header;
   $c->print("Content-Type: text/plain");
   $c->send_crlf;
   $c->send_crlf;
   $c->print("Okidok\n");
}

$req = new HTTP::Request GET => url("/someplace", $base);
$res = $ua->request($req);
#print $res->as_string;
print "not " unless $res->is_success;
print "ok 1\n";

$req = new HTTP::Request GET => url("/private/place", $base);
$res = $ua->request($req);
#print $res->as_string;
print "not " unless $res->code == 403
                and $res->message =~ /robots.txt/;
print "ok 2\n";

$req = new HTTP::Request GET => url("/foo", $base);
$res = $ua->request($req);
#print $res->as_string;
print "not " unless $res->code == 404;  # not found
print "ok 3\n";

# Let the robotua generate "Service unavailable/Retry After response";
$ua->delay(1);
$ua->use_alarm(0);
$req = new HTTP::Request GET => url("/foo", $base);
$res = $ua->request($req);
#print $res->as_string;
print "not " unless $res->code == 503   # Unavailable
                and $res->header("Retry-After");
print "ok 4\n";

#----------------------------------------------------------------
print "Terminating server...\n";
sub httpd_get_quit
{
    my($c) = @_;
    $c->send_error(503, "Bye, bye");
    exit;  # terminate HTTP server
}

$ua->delay(0);
$req = new HTTP::Request GET => url("/quit", $base);
$res = $ua->request($req);

print "not " unless $res->code == 503 and $res->content =~ /Bye, bye/;
print "ok 5\n";

#---------------------------------------------------------------
$ua->delay(1);

# host_wait() should be around 60s now
print "not " unless abs($ua->host_wait($base->netloc) - 60) < 5;
print "ok 6\n";

# Number of visits to this place should be 
print "not " unless $ua->no_visits($base->netloc) == 4;
print "ok 7\n";

