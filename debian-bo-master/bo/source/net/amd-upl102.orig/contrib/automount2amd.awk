#!/usr/bin/awk -f

#
# automount2amd --
#
# A simple script (to be used in a Makefile) that takes a NIS map (or
# a file) auto.xxx and generates output suitable for makedbm and
# the corresponding amd.xxx file.
#
# ATTENTION:  Some assembly required.  Read the comments.
#
# This works with the AWK provided with HP-UX.  I believe it also
# works with the AWK on SunOS 4.1.3.  Untested with other versions.
#
# Written by Philippe-Andre Prindeville <philipp@res.enst.fr> on or
# about the 24 February 1995.
#
# Use it at your own risk.  We do, and haven't had many problems...
#

BEGIN			{
			  # chose your defaults carefully.  the next 3 lines
			  # must be configured per-site...
			  gopts = "rw,nosuid,intr,soft,timeo=20,retrans=8";
			  mydomain = "enst.fr";
			  filters = "(local|bsd|share|X11|openwin)";

			  # the "remopts:= ... " field is optional.  delete it
			  # if your gateways rarely drop packets (yeah, right!)
			  printf "/defaults opts:=%s;remopts:=${opts},rsize=1024,wsize=1024\n", gopts;

			  # this is the list of options we understand.  Used
			  # in the function newopts()...
			  n = 0;
			  names[++n] = "(hard|soft)";
			  names[++n] = "(intr|nointr)";
			  names[++n] = "(suid|nosuid)";
			  names[++n] = "(fg|bg)";
			  names[++n] = "(rw|ro)";
			  names[++n] = "(devs|nodevs)";
			  names[++n] = "nocto";
			  names[++n] = "noac";
			  names[++n] = "acregmin=[0-9]+";
			  names[++n] = "acregmax=[0-9]+";
			  names[++n] = "acdirmin=[0-9]+";
			  names[++n] = "acdirmax=[0-9]+";
			  names[++n] = "actimeo=[0-9]+";
			  names[++n] = "port=[0-9]+";
			  names[++n] = "retrans=[0-9]+";
			  names[++n] = "retry=[0-9]+";
			  names[++n] = "rsize=[0-9]+";
			  names[++n] = "timeo=[0-9]+";
			  names[++n] = "wsize=[0-9]+";

			  remydomain = "." mydomain "$";
			  gsub("\.", "\.", remydomain);
			  needwildcard = 0;
			}

# you probably don't need this line at your site, so comment it out.
# here we have a directory called /cal that contains symlinks to /.cal/host;
# under AMD we ignore this (and hide out mount-points).
$1 ~ /^\/[a-z]*$/	{ next; }

# comment out this line if filters is empty or undefined ...
$1 ~ filters		{ next; }

#
# here we extract the mount-point name...  in our case, our entries look
# like /.cal/machine ... don't ask me why.  under AMD, we remove the
# "/.cal/" prefix...  you might be able to comment out the "sub" command
# at your site if the mount-points are relative..
#
			{ key = $1;
			  sub("^\/.?[a-z_]*\/", "", key);
			}

			{ start = 2; }

#
# if there are options, remove the "-" and note that the first mount-point
# is the $3 and not $2...  also, cross the global options (gopts) with the
# per mount-point options (the per m.p. options override the global ones)
#
$2 ~ /^-/		{ opts = substr($2, 2);
			  opts = newopts(opts);
			  ++start;
			}

#
# for every mount-point, split off the hostname and the remote directory
# name.  if multiple mount-points exist, generate one selector for each.
#
# if the name is qualified and local, remove the redundant portion of the
# domain name.  if the selector matches the "wildcard" template, ie.
#
# machine		machine:/home/machine
#
# don't output anything, but note that we need to generate a wildcard
# entry at the end.
#
			{ str = key " ";

			  nt = (NF - start) + 1;
			  if (nt > 1) { str = str "-"; sep = " "; }
			  else sep = ";";
			  str = str "type:=nfs";

			  if (opts != gopts) str = str ";opts:=" opts;

			  for (n = start; n <= NF; ++n) {
			      mntpnt = $n;
			      split(mntpnt, parts, /:/);
			      rhost = parts[1];
			      rfs = parts[2];
			      sub(remydomain, "", rhost);

			      # case of "*	&:/home/&"
			      if (nt == 1 && key == "*" && rhost == "&" && rfs == "/home/&" && opts == gopts) {
				  ++needwildcard;
				  next;
			      }

			      # case of "rfs:=/home/mount-point"
			      if (rfs == "/home/" key) rfs = "/home/${key}";

			      # case of "machine machine:/home/machine"
			      if (nt == 1 && key == rhost && rfs == "/home/${key}" && opts == gopts) {
				  ++needwildcard;
				  next;
			      }

			      str = str sep "rfs:=" rfs ";rhost:=" rhost;
			  }
			  printf "%s\n", str;
			}

#
# remember if we needed to generate wildcards or not...
#
END			{ if (needwildcard)
			      printf "* type:=nfs;rfs:=/home/${key};rhost:=${key}\n";
			}

#
# given the global options (gopts), and mount-point specific options, merge
# the two...  the return value is the union of the two (with m.p. options
# prefered in the case of collisions).
#
function newopts(s)	{
			  copts = gopts;
			  no = split(s, tokens, /,/);
			  for (k = 1; k <= no; ++k)
			      for (l in names) {
				if (tokens[k] ~ names[l])
				  sub(names[l], tokens[k], copts);
			      }
			  return copts;
			}
