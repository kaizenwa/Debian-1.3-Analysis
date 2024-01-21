const char *default_timer_phrases[] = { 
	"Elapsed Time = %v1% mins %v2% seconds\n",		/* 0 */
	"%v1% Bytes received in %v2% mins %v3% secs, BPS = %v4%\n",
	"%v1% BPS"
};

const char *default_chkh_phrases[] = {
	"Can not open %v1%, Skipping\n",		/* 0 */
	"Processing History File ",
	"Processing History File - Old version ",
	"Bogus line, ignoring: %v1%",
	"Processed history, %v1% dupes removed\n",
	"Out of Memory, skipping history check",	/* 5 */
	"Processing History Database "
};

const char *default_dedupe_phrases[] = {
	"Deduping ",				/* 0 */
	"Out of Memory, skipping Dedupe\n",
	"Deduped, %v1% items remaining, %v2% dupes removed.\n",
};

const char *default_killf_reasons[] = {
	"No Reason - major screwup",		/* 0 */
	"Over Hi-Line parameter",
	"Under Low-Line parameter",
	"Host in Path rejected",
	"Word in Subject rejected",
	"Name in From rejected",		/* 5 */
	"Name in NNTP_Host rejected",
	"Too many Newsgroups",
	"Not matched in Group Keep file",
	"Multiple Matches/Tiebreaker Used"
};

const char *default_killf_phrases[] = {
	"Out of memory\n",				/* 0 */
	"Out of memory, can't use killfiles\n",
	"Invalid parameter line %v1%\n",
	"Unable to log killed article",
	"ARTICLE KILLED: %v1% - %v2%\n%v3%\n",
	"Out of memory, skipping killfile regex processing",		/* 5 */
	"No paths will be processed for %v1%\n",
	"No From lines will be processed for %v1%\n",
	"No Subjects will be processed for %v1%\n",
	"No NNTP Hosts will be processed for %v1%\n",
	"No character to process for quote, ignoring\n",	/* 10 */
	"No character to process for pathhost separator, ignoring\n",
	"No character to process for subject separator, ignoring\n",
	"No character to process for from separator, ignoring\n",
	"No character to process for NNTP Host separator, ignoring\n"
};

const char *default_killp_phrases[] = {
	"Out of memory processing killprg args",		/* 0 */
	"Setting up pipes",
	"Out of memory processing path",
	"%v1%: Unable to find, sorry",	
	"Unable to write to child process",
	"Unable to read from child process",			/* 5 */
	"Process Error",
	"Child Process died",
	"Child process closed pipe, waiting for child to end\n",
	"Child exited with a status of %v1%\n",
	"Child exited due to signal %v1%\n",				/* 10 */
	"Child died, cause of death unknown\n"
};

const char *default_sucku_phrases[] = { 
	"%v1%: Not a directory\n",		/* 0 */
	"%v1%: Write permission denied\n",
	"Lock File %v1%, Invalid PID, aborting\n",
	"Lock File %v1%, stale PID, removed\n",
	"Lock File %v1%, stale PID, Unable to remove, aborting\n",
	"Lock file %v1%, PID exists, aborting\n",	/* 5 */
	"Unable to create Lock File %v1%\n"
};

const char *default_suck_phrases[] = {
	"suck: status log",		/* 0 */
	"Attempting to connect to %v1%\n",
	"Initiating restart at article %v1% of %v2%\n",
	"No articles\n",
	"Closed connection to %v1%\n",
	"Building Inn Batch File\n",	/* 5*/
	"Building Rnews Batch File(s)\n",
	"Cleaning up after myself\n",
	"Skipping Line: %v1%",
	"Invalid Line: %v1%",	
	"Invalid number for maxread field: %v1% : ignoring\n",	/* 10 */
	"GROUP <%v1%> not found on host\n",
	"GROUP <%v1%>, unexpected response, %v2%\n",
	"%v1% - %v2%...High Article Nr is low, did host reset its counter?\n",
	"%v1% - limiting # of articles to %v2%\n",
	"%v1% - articles %v2%-%v3%\n",			/* 15 */
	"%v1% Articles to download\n",
	"Processing Supplemental List\n",
	"Supplemental Invalid Line: %v1%, ignoring\n",
	"Supplemental List Processed, %v1% articles added, %v2% articles to download\n",
	"Total articles to download: %v1%\n",		/* 20 */
	"Invalid Article Nr, skipping: %v1%\n",
	"Out of Memory, aborting\n",
	"Whoops, %v1% exists, %v2% doesn't, aborting\n",
	"Signal received, will finish downloading article.\n",
	"Notify programmer, problem with pause_signal()\n",	/* 25 */
	"\nGot Pause Signal, swapping pause values\n",	
	"Weird Response to Authorization: %v1%",
	"Authorization Denied",
	"***Unexpected response to command, %v1%\n%v2%\n", 
	"Moving newsrc to backup",				/* 30 */
	"Moving newrc to newsrc",
	"Removing Sorted File",
	"Removing Supplemental Message file",
	"Invalid argument: %v1%\n",
	"No rnews batch file size provided\n",			/* 35 */
	"No postfix provided\n",
	"No directory name provided\n",
	"Invalid directory\n",
	"Invalid Batch arg: %v1%\n",
	"No Batch file name provided\n",			/* 40 */
	"No Error Log name provided\n",
	"No Status Log name provided\n",
	"No Userid provided\n",
	"No Passwd provided\n",
	"No Port Number provided\n",				/* 45 */
	"Invalid pause arguments\n",
	"No phrase file provided\n",
	"GROUP command not recognized, try the -M option\n" 
};

int nr_suck_phrases= sizeof(default_suck_phrases)/sizeof(default_suck_phrases[0]);
int nr_timer_phrases= sizeof(default_timer_phrases)/sizeof(default_timer_phrases[0]);
int nr_chkh_phrases= sizeof(default_chkh_phrases)/sizeof(default_chkh_phrases[0]);
int nr_dedupe_phrases= sizeof(default_dedupe_phrases)/sizeof(default_dedupe_phrases[0]);
int nr_killf_reasons= sizeof(default_killf_reasons)/sizeof(default_killf_reasons[0]);
int nr_killf_phrases= sizeof(default_killf_phrases)/sizeof(default_killf_phrases[0]);
int nr_killp_phrases= sizeof(default_killp_phrases)/sizeof(default_killp_phrases[0]);
int nr_sucku_phrases= sizeof(default_sucku_phrases)/sizeof(default_sucku_phrases[0]);

