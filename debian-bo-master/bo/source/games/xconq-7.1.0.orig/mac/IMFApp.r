resource 'MENU' (128, "Apple") {
	128,
	textMenuProc,
	0x7FFFFFFD,
	enabled,
	apple,
	{	/* array: 2 elements */
		/* [1] */
		"About IMFApp...", noIcon, noKey, noMark, plain,
		/* [2] */
		"-", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (129, "File") {
	129,
	textMenuProc,
	0x7FFFFDEF,
	enabled,
	"File",
	{	/* array: 11 elements */
		/* [1] */
		"New", noIcon, noKey, noMark, plain,
		/* [2] */
		"Open...", noIcon, noKey, noMark, plain,
		/* [3] */
		"Add Imf...", noIcon, noKey, noMark, plain,
		/* [4] */
		"Add Resources...", noIcon, noKey, noMark, plain,
		/* [5] */
		"-", noIcon, noKey, noMark, plain,
		/* [6] */
		"Save...", noIcon, noKey, noMark, plain,
		/* [7] */
		"Save Imf...", noIcon, noKey, noMark, plain,
		/* [8] */
		"Save Resources...", noIcon, noKey, noMark, plain,
		/* [9] */
		"Selected Only", noIcon, noKey, noMark, plain,
		/* [10] */
		"-", noIcon, noKey, noMark, plain,
		/* [11] */
		"Quit", noIcon, "Q", noMark, plain
	}
};

resource 'MENU' (130, "Edit") {
	130,
	textMenuProc,
	0x7FFFFFF8,
	enabled,
	"Edit",
	{	/* array: 4 elements */
		/* [1] */
		"Cut", noIcon, "X", noMark, plain,
		/* [2] */
		"Copy", noIcon, "C", noMark, plain,
		/* [3] */
		"Paste", noIcon, "V", noMark, plain,
		/* [4] */
		"Clear", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (131, "View") {
	131,
	textMenuProc,
	0x7FFEFDDF,
	enabled,
	"View",
	{	/* array: 20 elements */
		/* [1] */
		"4 x 4", noIcon, noKey, noMark, plain,
		/* [2] */
		"8 x 8", noIcon, noKey, noMark, plain,
		/* [3] */
		"16 x 16", noIcon, noKey, noMark, plain,
		/* [4] */
		"32 x 32", noIcon, noKey, noMark, plain,
		/* [5] */
		"64 x 64", noIcon, noKey, noMark, plain,
		/* [6] */
		"-", noIcon, noKey, noMark, plain,
		/* [7] */
		"Color", noIcon, noKey, noMark, plain,
		/* [8] */
		"Names", noIcon, noKey, noMark, plain,
		/* [9] */
		"Mask", noIcon, noKey, noMark, plain,
		/* [10] */
		"-", noIcon, noKey, noMark, plain,
		/* [11] */
		"As Unit", noIcon, noKey, noMark, plain,
		/* [12] */
		"As Terrain", noIcon, noKey, noMark, plain,
		/* [13] */
		"As Emblem", noIcon, noKey, noMark, plain,
		/* [14] */
		"With Unit", noIcon, noKey, noMark, plain,
		/* [15] */
		"With Terrain", noIcon, noKey, noMark, plain,
		/* [16] */
		"With Emblem", noIcon, noKey, noMark, plain,
		/* [17] */
		"-", noIcon, noKey, noMark, plain,
		/* [18] */
		"Icons", noIcon, noKey, noMark, plain,
		/* [19] */
		"Tiles", noIcon, noKey, noMark, plain,
		/* [20] */
		"Use CQD", noIcon, noKey, noMark, plain
	}
};

resource 'WIND' (128, "Images") {
	{50, 10, 332, 502},
	zoomDocProc,
	visible,
	goAway,
	0x0,
	"Images"
};

resource 'MBAR' (128) {
	{	/* array MenuArray: 4 elements */
		/* [1] */
		128,
		/* [2] */
		129,
		/* [3] */
		130,
		/* [4] */
		131
	}
};

resource 'ALRT' (141, "Error") {
	{60, 50, 220, 450},
	141,
	{	/* array: 4 elements */
		/* [1] */
		OK, visible, sound1,
		/* [2] */
		OK, visible, sound1,
		/* [3] */
		OK, visible, sound1,
		/* [4] */
		OK, visible, sound1
	}
};

resource 'ALRT' (128, "About", purgeable) {
	{60, 50, 220, 370},
	128,
	{	/* array: 4 elements */
		/* [1] */
		OK, visible, silent,
		/* [2] */
		OK, visible, silent,
		/* [3] */
		OK, visible, silent,
		/* [4] */
		OK, visible, silent
	}
};

resource 'ALRT' (140, "Warning") {
	{60, 50, 220, 450},
	140,
	{	/* array: 4 elements */
		/* [1] */
		OK, visible, sound1,
		/* [2] */
		OK, visible, sound1,
		/* [3] */
		OK, visible, sound1,
		/* [4] */
		OK, visible, sound1
	}
};

resource 'DITL' (140, "Warning Items") {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{130, 80, 150, 160},
		Button {
			enabled,
			"Continue"
		},
		/* [2] */
		{125, 250, 145, 330},
		Button {
			enabled,
			"Quit"
		},
		/* [3] */
		{5, 90, 115, 390},
		StaticText {
			disabled,
			"Warning: \n\"^0\"\n\nDo you want to continue "
			"loading?"
		}
	}
};

resource 'DITL' (128, "About Items", purgeable) {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{130, 115, 150, 195},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{10, 10, 123, 313},
		StaticText {
			disabled,
			"Image Family Viewer/Translator for Xconq"
		}
	}
};

resource 'DITL' (141, "Error Items") {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{135, 155, 155, 235},
		Button {
			enabled,
			"Quit"
		},
		/* [2] */
		{5, 90, 115, 390},
		StaticText {
			disabled,
			"Fatal Error: \n\"^0\"\n\n"
		}
	}
};

data 'cicn' (12345) {
	$"0000 0000 8004 0000 0000 0020 0020 0000"            /* ....€...... . .. */
	$"0000 0000 0000 0048 0000 0048 0000 0000"            /* .......H...H.... */
	$"0001 0001 0001 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0004 0000 0000 0020 0020"            /* ............. .  */
	$"0000 0000 0004 0000 0000 0020 0020 0000"            /* ........... . .. */
	$"0000 FFFF FFFF FFFF FFFF FFFF FFFF FFFF"            /* ..ÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"            /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"            /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"            /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"            /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"            /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"            /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"            /* ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ */
	$"FFFF 0000 0000 0000 0000 0000 0000 0000"            /* ÿÿ.............. */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 FFFF FFFF"            /* ............ÿÿÿÿ */
	$"FFFF 0000 0000 0000 0000 0000 0000 0000"            /* ÿÿ.............. */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000 0000 0000 0000 0000 0000 0000 0000"            /* ................ */
	$"0000"                                               /* .. */
};

data 'vers' (1, purgeable) {
	$"0700 8000 0000 0637 2E30 2E38 382B 666F"            /* ..€....7.0.88+fo */
	$"7220 5863 6F6E 7120 372E 302C 20A9 2053"            /* r Xconq 7.0, © S */
	$"7461 6E6C 6579 2054 2E20 5368 6562 7320"            /* tanley T. Shebs  */
	$"3139 3836 2D31 3939 35"                             /* 1986-1995 */
};

data 'vers' (2, purgeable) {
	$"0701 8000 0000 0637 2E30 2E38 360D 666F"            /* ..€....7.0.86Âfo */
	$"7220 5863 6F6E 7120 372E 30"                        /* r Xconq 7.0 */
};

