## File Magic Parser
## by Ken Hollis <khollis@bitgate.com>
##
## This program is GPLed.  Please read the LICENSE file for more information.
## Copyright (C) 1996, Bitgate Software.

sub check_exist {
	local($type, $filename, $checkfor, $cfile) = @_;
	if ($type eq "2") {
		if (-e $filename) {
			return 1;
		} else {
			return 0;
		}
	}

	if ($type eq "1") {
		if (-e $filename) {
			$clean = 0;
			open(IF, $filename);

			while(<IF>) {
				chop;
				if (/$checkfor/) {
					$clean = 1;
					last;
				}
			}
			close(IF);

			if (!$clean) {
				&disp_file($cfile);
				return &ask_question("Go ahead?", "N", BOOLEAN_QUESTION, $cfile);
			} else {
				return "Y";
			}
		} else {
			return "0";
		}
	}
}

1;

