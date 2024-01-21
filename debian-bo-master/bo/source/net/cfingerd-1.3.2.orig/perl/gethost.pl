## Get Fully Qualified Domain Name
## by Ken Hollis <khollis@bitgate.com>
##
## This program is GPLed.  Please read the LICENSE file for more information.
## Copyright (C) 1996, Bitgate Software.

sub get_fqdn {
	local($question, $config) = @_;

	&disp_file($config);

	local($hostname) = `hostname`;
	chop($hostname);

	local($domainname) = `domainname`;
	chop($domainname);

	if (($hostname ne "(none)") && ($domainname ne "(none)")) {
		local($fqdn) = $hostname . "." . $domainname;
	} elsif ($hostname eq "(none)") {
		local($fqdn) = $domainname;
	} elsif ($domainname eq "(none)") {
		local($fqdn) = $hostname;
	}

	$fqdn = &ask_question($question, $fqdn, STRING_QUESTION);
}

1;
