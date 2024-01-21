resource 'MENU' (128, "Apple") {
	128,
	textMenuProc,
	0x7FFFFFF7,
	enabled,
	apple,
	{	/* array: 4 elements */
		/* [1] */
		"About Xconq...", noIcon, noKey, noMark, plain,
		/* [2] */
		"Help...", noIcon, "?", noMark, plain,
		/* [3] */
		"Instructions...", noIcon, "/", noMark, plain,
		/* [4] */
		"-", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (129, "File") {
	129,
	textMenuProc,
	0x7FFFED6B,
	enabled,
	"File",
	{	/* array: 15 elements */
		/* [1] */
		"New Game", noIcon, "N", noMark, plain,
		/* [2] */
		"Open Game...", noIcon, "O", noMark, plain,
		/* [3] */
		"-", noIcon, noKey, noMark, plain,
		/* [4] */
		"Connect...", noIcon, noKey, noMark, plain,
		/* [5] */
		"-", noIcon, noKey, noMark, plain,
		/* [6] */
		"Save", noIcon, "S", noMark, plain,
		/* [7] */
		"Save As...", noIcon, noKey, noMark, plain,
		/* [8] */
		"-", noIcon, noKey, noMark, plain,
		/* [9] */
		"Preferences...", noIcon, noKey, noMark, plain,
		/* [10] */
		"-", noIcon, noKey, noMark, plain,
		/* [11] */
		"Page Setup...", noIcon, noKey, noMark, plain,
		/* [12] */
		"Print Window...", noIcon, noKey, noMark, plain,
		/* [13] */
		"-", noIcon, noKey, noMark, plain,
		/* [14] */
		"Resign", noIcon, noKey, noMark, plain,
		/* [15] */
		"Quit", noIcon, "Q", noMark, plain
	}
};

resource 'MENU' (130, "Edit") {
	130,
	textMenuProc,
	0x7FFFFE80,
	enabled,
	"Edit",
	{	/* array: 10 elements */
		/* [1] */
		"Can't Undo", noIcon, "Z", noMark, plain,
		/* [2] */
		"-", noIcon, noKey, noMark, plain,
		/* [3] */
		"Cut", noIcon, "X", noMark, plain,
		/* [4] */
		"Copy", noIcon, "C", noMark, plain,
		/* [5] */
		"Paste", noIcon, "V", noMark, plain,
		/* [6] */
		"Clear", noIcon, noKey, noMark, plain,
		/* [7] */
		"-", noIcon, noKey, noMark, plain,
		/* [8] */
		"Select All", noIcon, "A", noMark, plain,
		/* [9] */
		"-", noIcon, noKey, noMark, plain,
		/* [10] */
		"Design...", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (200, "Sides") {
	200,
	textMenuProc,
	allEnabled,
	enabled,
	"Sides",
	{	/* array: 0 elements */
	}
};

resource 'MENU' (134, "Windows") {
	134,
	textMenuProc,
	0x7FFFFACF,
	enabled,
	"Windows",
	{	/* array: 11 elements */
		/* [1] */
		"Game", noIcon, "1", noMark, plain,
		/* [2] */
		"Notices", noIcon, "2", noMark, plain,
		/* [3] */
		"History", noIcon, "3", noMark, plain,
		/* [4] */
		"Construction", noIcon, "4", noMark, plain,
		/* [5] */
		"Agreements", noIcon, hierarchicalMenu, noMark, plain,
		/* [6] */
		"-", noIcon, noKey, noMark, plain,
		/* [7] */
		"New Map", noIcon, noKey, noMark, plain,
		/* [8] */
		"New List", noIcon, noKey, noMark, plain,
		/* [9] */
		"-", noIcon, noKey, noMark, plain,
		/* [10] */
		"World Map", noIcon, "W", noMark, plain,
		/* [11] */
		"-", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (201, "Weather") {
	201,
	textMenuProc,
	allEnabled,
	enabled,
	"Weather",
	{	/* array: 4 elements */
		/* [1] */
		"Temperature", noIcon, noKey, noMark, plain,
		/* [2] */
		"Winds", noIcon, noKey, noMark, plain,
		/* [3] */
		"Clouds", noIcon, noKey, noMark, plain,
		/* [4] */
		"Storms", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (202, "Material Types") {
	202,
	textMenuProc,
	allEnabled,
	enabled,
	"Material Types",
	{	/* array: 0 elements */
	}
};

resource 'MENU' (203, "Font Sizes") {
	203,
	textMenuProc,
	allEnabled,
	enabled,
	"Font Sizes",
	{	/* array: 0 elements */
	}
};

resource 'MENU' (235, "List View") {
	235,
	textMenuProc,
	0x7FFFFE1F,
	enabled,
	"View",
	{	/* array: 10 elements */
		/* [1] */
		"by Type", noIcon, noKey, noMark, plain,
		/* [2] */
		"by Name", noIcon, noKey, noMark, plain,
		/* [3] */
		"by Side", noIcon, noKey, noMark, plain,
		/* [4] */
		"by Acting Order", noIcon, noKey, noMark, plain,
		/* [5] */
		"by Location", noIcon, noKey, noMark, plain,
		/* [6] */
		"-", noIcon, noKey, noMark, plain,
		/* [7] */
		"with Transport", noIcon, noKey, noMark, plain,
		/* [8] */
		"with Commanders", noIcon, noKey, noMark, plain,
		/* [9] */
		"-", noIcon, noKey, noMark, plain,
		/* [10] */
		"Larger Icons", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (205, "Unit Types") {
	205,
	textMenuProc,
	allEnabled,
	enabled,
	"Unit Types",
	{	/* array: 0 elements */
	}
};

resource 'MENU' (204, "Terrain Types") {
	204,
	textMenuProc,
	allEnabled,
	enabled,
	"Terrain Types",
	{	/* array: 0 elements */
	}
};

resource 'MENU' (131, "Find") {
	131,
	textMenuProc,
	0x7FFFFFDB,
	enabled,
	"Find",
	{	/* array: 7 elements */
		/* [1] */
		"Previous Actor", noIcon, noKey, noMark, plain,
		/* [2] */
		"Next Actor", noIcon, noKey, noMark, plain,
		/* [3] */
		"-", noIcon, noKey, noMark, plain,
		/* [4] */
		"Location...", noIcon, noKey, noMark, plain,
		/* [5] */
		"Unit by Name...", noIcon, noKey, noMark, plain,
		/* [6] */
		"-", noIcon, noKey, noMark, plain,
		/* [7] */
		"Selected", noIcon, "F", noMark, plain
	}
};

resource 'MENU' (133, "Side") {
	133,
	textMenuProc,
	0x7FFFE2BA,
	enabled,
	"Side",
	{	/* array: 14 elements */
		/* [1] */
		"Closeup", noIcon, noKey, noMark, plain,
		/* [2] */
		"End This Turn", noIcon, "E", noMark, plain,
		/* [3] */
		"-", noIcon, noKey, noMark, plain,
		/* [4] */
		"Move On Click", noIcon, "M", noMark, plain,
		/* [5] */
		"Auto Jump Next", noIcon, "J", noMark, plain,
		/* [6] */
		"Auto End Turn", noIcon, noKey, noMark, plain,
		/* [7] */
		"-", noIcon, noKey, noMark, plain,
		/* [8] */
		"Sound", noIcon, noKey, noMark, plain,
		/* [9] */
		"-", noIcon, noKey, noMark, plain,
		/* [10] */
		"AI...", noIcon, hierarchicalMenu, "Œ", plain,
		/* [11] */
		"-", noIcon, noKey, noMark, plain,
		/* [12] */
		"Doctrines", noIcon, hierarchicalMenu, noMark, plain,
		/* [13] */
		"-", noIcon, noKey, noMark, plain,
		/* [14] */
		"Rename Side...", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (335, "Unit Closeup View") {
	335,
	textMenuProc,
	allEnabled,
	enabled,
	"View",
	{	/* array: 0 elements */
	}
};

resource 'MENU' (206, "AI Types") {
	206,
	textMenuProc,
	allEnabled,
	enabled,
	"AI Types",
	{	/* array: 2 elements */
		/* [1] */
		"None", noIcon, noKey, noMark, plain,
		/* [2] */
		"Mplayer", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (207, "Strengths") {
	207,
	textMenuProc,
	0x7FFFF801,
	enabled,
	"Strengths",
	{	/* array: 11 elements */
		/* [1] */
		"1", noIcon, noKey, noMark, plain,
		/* [2] */
		"2", noIcon, noKey, noMark, plain,
		/* [3] */
		"3", noIcon, noKey, noMark, plain,
		/* [4] */
		"4", noIcon, noKey, noMark, plain,
		/* [5] */
		"5", noIcon, noKey, noMark, plain,
		/* [6] */
		"6", noIcon, noKey, noMark, plain,
		/* [7] */
		"7", noIcon, noKey, noMark, plain,
		/* [8] */
		"8", noIcon, noKey, noMark, plain,
		/* [9] */
		"9", noIcon, noKey, noMark, plain,
		/* [10] */
		"-", noIcon, noKey, noMark, plain,
		/* [11] */
		"Other...", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (135, "Map View") {
	135,
	textMenuProc,
	0x7FFDEFF7,
	enabled,
	"View",
	{	/* array: 18 elements */
		/* [1] */
		"Closer", noIcon, noKey, noMark, plain,
		/* [2] */
		"Farther", noIcon, noKey, noMark, plain,
		/* [3] */
		"Set Mag", noIcon, hierarchicalMenu, "—", plain,
		/* [4] */
		"-", noIcon, noKey, noMark, plain,
		/* [5] */
		"Names", noIcon, noKey, noMark, plain,
		/* [6] */
		"Elevations", noIcon, noKey, noMark, plain,
		/* [7] */
		"People", noIcon, noKey, noMark, plain,
		/* [8] */
		"Weather", noIcon, hierarchicalMenu, "…", plain,
		/* [9] */
		"Materials", noIcon, hierarchicalMenu, " ", plain,
		/* [10] */
		"Terrain", noIcon, hierarchicalMenu, "’", plain,
		/* [11] */
		"Daylight", noIcon, noKey, noMark, plain,
		/* [12] */
		"Coverage", noIcon, noKey, noMark, plain,
		/* [13] */
		"-", noIcon, noKey, noMark, plain,
		/* [14] */
		"Grid", noIcon, noKey, noMark, plain,
		/* [15] */
		"Top Line", noIcon, noKey, noMark, plain,
		/* [16] */
		"Unit Info", noIcon, noKey, noMark, plain,
		/* [17] */
		"Other Maps", noIcon, noKey, noMark, plain,
		/* [18] */
		"Font Size", noIcon, hierarchicalMenu, "À", plain
	}
};

resource 'MENU' (208, "Agreements") {
	208,
	textMenuProc,
	allEnabled,
	enabled,
	"Agreements",
	{	/* array: 0 elements */
	}
};

resource 'MENU' (209, "Magnifications") {
	209,
	textMenuProc,
	allEnabled,
	enabled,
	"Magnifications",
	{	/* array: 8 elements */
		/* [1] */
		"1 x 1", noIcon, noKey, noMark, plain,
		/* [2] */
		"2 x 2", noIcon, noKey, noMark, plain,
		/* [3] */
		"4 x 4", noIcon, noKey, noMark, plain,
		/* [4] */
		"8 x 8", noIcon, noKey, noMark, plain,
		/* [5] */
		"16 x 16", noIcon, noKey, noMark, plain,
		/* [6] */
		"32 x 32", noIcon, noKey, noMark, plain,
		/* [7] */
		"64 x 64", noIcon, noKey, noMark, plain,
		/* [8] */
		"128 x 128", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (132, "Play") {
	132,
	textMenuProc,
	0x6DF7DBDD,
	enabled,
	"Play",
	{	/* array: 30 elements */
		/* [1] */
		"Closeup", noIcon, "I", noMark, plain,
		/* [2] */
		"-", noIcon, noKey, noMark, plain,
		/* [3] */
		"Move To...", noIcon, noKey, noMark, plain,
		/* [4] */
		"Return", noIcon, "R", noMark, plain,
		/* [5] */
		"Set Formation...", noIcon, noKey, noMark, plain,
		/* [6] */
		"-", noIcon, noKey, noMark, plain,
		/* [7] */
		"Wake", noIcon, noKey, noMark, plain,
		/* [8] */
		"Sleep", noIcon, noKey, noMark, plain,
		/* [9] */
		"Reserve", noIcon, noKey, noMark, plain,
		/* [10] */
		"Delay", noIcon, noKey, noMark, plain,
		/* [11] */
		"-", noIcon, noKey, noMark, plain,
		/* [12] */
		"Build", noIcon, "P", noMark, plain,
		/* [13] */
		"Repair...", noIcon, noKey, noMark, plain,
		/* [14] */
		"-", noIcon, noKey, noMark, plain,
		/* [15] */
		"Attack", noIcon, "-", noMark, plain,
		/* [16] */
		"Overrun", noIcon, noKey, noMark, plain,
		/* [17] */
		"Fire", noIcon, noKey, noMark, plain,
		/* [18] */
		"Fire Into", noIcon, noKey, noMark, plain,
		/* [19] */
		"Detonate", noIcon, "=", noMark, plain,
		/* [20] */
		"-", noIcon, noKey, noMark, plain,
		/* [21] */
		"Take...", noIcon, noKey, noMark, plain,
		/* [22] */
		"Drop...", noIcon, noKey, noMark, plain,
		/* [23] */
		"Give", noIcon, hierarchicalMenu, "»", plain,
		/* [24] */
		"Detach", noIcon, noKey, noMark, plain,
		/* [25] */
		"Disband", noIcon, "K", noMark, plain,
		/* [26] */
		"-", noIcon, noKey, noMark, plain,
		/* [27] */
		"Add Terrain...", noIcon, noKey, noMark, plain,
		/* [28] */
		"Remove Terrain...", noIcon, noKey, noMark, plain,
		/* [29] */
		"-", noIcon, noKey, noMark, plain,
		/* [30] */
		"Rename...", noIcon, "9", noMark, plain
	}
};

resource 'MENU' (210, "Run Length") {
	210,
	textMenuProc,
	0x7FFFF9FF,
	enabled,
	"Run Length",
	{	/* array: 11 elements */
		/* [1] */
		"1", noIcon, noKey, noMark, plain,
		/* [2] */
		"2", noIcon, noKey, noMark, plain,
		/* [3] */
		"3", noIcon, noKey, noMark, plain,
		/* [4] */
		"4", noIcon, noKey, noMark, plain,
		/* [5] */
		"5", noIcon, noKey, noMark, plain,
		/* [6] */
		"10", noIcon, noKey, noMark, plain,
		/* [7] */
		"20", noIcon, noKey, noMark, plain,
		/* [8] */
		"50", noIcon, noKey, noMark, plain,
		/* [9] */
		"99", noIcon, noKey, noMark, plain,
		/* [10] */
		"-", noIcon, noKey, noMark, plain,
		/* [11] */
		"Other...", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (211, "Elevations") {
	211,
	textMenuProc,
	0x7FFFF9FF,
	enabled,
	"Elevations",
	{	/* array: 11 elements */
		/* [1] */
		"1", noIcon, noKey, noMark, plain,
		/* [2] */
		"2", noIcon, noKey, noMark, plain,
		/* [3] */
		"3", noIcon, noKey, noMark, plain,
		/* [4] */
		"4", noIcon, noKey, noMark, plain,
		/* [5] */
		"5", noIcon, noKey, noMark, plain,
		/* [6] */
		"10", noIcon, noKey, noMark, plain,
		/* [7] */
		"20", noIcon, noKey, noMark, plain,
		/* [8] */
		"50", noIcon, noKey, noMark, plain,
		/* [9] */
		"99", noIcon, noKey, noMark, plain,
		/* [10] */
		"-", noIcon, noKey, noMark, plain,
		/* [11] */
		"Other...", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (212, "Features") {
	212,
	textMenuProc,
	allEnabled,
	enabled,
	"Features",
	{	/* array: 0 elements */
	}
};

resource 'MENU' (213) {
	213,
	textMenuProc,
	allEnabled,
	enabled,
	"Opt Terrain Types",
	{	/* array: 0 elements */
	}
};

resource 'DITL' (133, "About Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{270, 180, 290, 250},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{220, 10, 235, 334},
		StaticText {
			disabled,
			"(version)"
		},
		/* [3] */
		{240, 10, 256, 334},
		StaticText {
			disabled,
			"(copyright)"
		},
		/* [4] */
		{5, 5, 205, 405},
		Picture {
			disabled,
			2000
		}
	}
};

resource 'DITL' (129, "New Game Items") {
	{	/* array DITLarray: 6 elements */
		/* [1] */
		{270, 65, 290, 145},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{270, 255, 290, 335},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{5, 5, 260, 195},
		UserItem {
			disabled
		},
		/* [4] */
		{155, 200, 260, 410},
		UserItem {
			disabled
		},
		/* [5] */
		{50, 200, 150, 410},
		UserItem {
			disabled
		},
		/* [6] */
		{5, 200, 46, 411},
		Picture {
			enabled,
			2002
		}
	}
};

resource 'DITL' (200, "Side Closeup Items") {
	{	/* array DITLarray: 9 elements */
		/* [1] */
		{110, 20, 130, 78},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{110, 140, 130, 198},
		Button {
			enabled,
			"Apply"
		},
		/* [3] */
		{110, 260, 130, 318},
		Button {
			enabled,
			"Cancel"
		},
		/* [4] */
		{5, 5, 37, 37},
		Icon {
			disabled,
			1431
		},
		/* [5] */
		{13, 50, 29, 96},
		StaticText {
			disabled,
			"Name:"
		},
		/* [6] */
		{13, 100, 29, 250},
		EditText {
			enabled,
			"<name>"
		},
		/* [7] */
		{11, 260, 31, 334},
		Button {
			enabled,
			"Player..."
		},
		/* [8] */
		{50, 10, 68, 151},
		CheckBox {
			enabled,
			"Machine Strategy"
		},
		/* [9] */
		{80, 10, 98, 144},
		CheckBox {
			enabled,
			"Auto-Finish Turn"
		}
	}
};

resource 'DITL' (501, "Designer Save Items") {
	{	/* array DITLarray: 30 elements */
		/* [1] */
		{275, 110, 295, 190},
		Button {
			enabled,
			"Save"
		},
		/* [2] */
		{275, 250, 295, 330},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{10, 125, 26, 302},
		EditText {
			enabled,
			"game-data"
		},
		/* [4] */
		{10, 350, 30, 430},
		Button {
			enabled,
			"Module..."
		},
		/* [5] */
		{55, 10, 71, 90},
		CheckBox {
			enabled,
			"Types"
		},
		/* [6] */
		{75, 10, 91, 90},
		CheckBox {
			enabled,
			"Tables"
		},
		/* [7] */
		{95, 10, 111, 90},
		CheckBox {
			enabled,
			"Globals"
		},
		/* [8] */
		{120, 10, 136, 90},
		CheckBox {
			enabled,
			"World"
		},
		/* [9] */
		{140, 10, 156, 90},
		CheckBox {
			enabled,
			"Area"
		},
		/* [10] */
		{170, 10, 186, 90},
		CheckBox {
			enabled,
			"Sides"
		},
		/* [11] */
		{190, 10, 206, 90},
		CheckBox {
			enabled,
			"Players"
		},
		/* [12] */
		{215, 10, 231, 90},
		CheckBox {
			enabled,
			"Units"
		},
		/* [13] */
		{240, 10, 256, 90},
		CheckBox {
			enabled,
			"Scoring"
		},
		/* [14] */
		{260, 10, 276, 90},
		CheckBox {
			enabled,
			"History"
		},
		/* [15] */
		{105, 350, 125, 430},
		Button {
			enabled,
			"Reshape..."
		},
		/* [16] */
		{250, 350, 266, 430},
		CheckBox {
			enabled,
			"Compress"
		},
		/* [17] */
		{170, 95, 186, 175},
		CheckBox {
			enabled,
			"Names"
		},
		/* [18] */
		{170, 180, 186, 260},
		CheckBox {
			enabled,
			"Props"
		},
		/* [19] */
		{170, 265, 186, 345},
		CheckBox {
			enabled,
			"Views"
		},
		/* [20] */
		{215, 180, 231, 260},
		CheckBox {
			enabled,
			"Props"
		},
		/* [21] */
		{215, 265, 231, 345},
		CheckBox {
			enabled,
			"Action"
		},
		/* [22] */
		{215, 350, 231, 430},
		CheckBox {
			enabled,
			"Plans"
		},
		/* [23] */
		{10, 10, 28, 117},
		StaticText {
			disabled,
			"Module Name:"
		},
		/* [24] */
		{35, 205, 53, 257},
		StaticText {
			disabled,
			""
		},
		/* [25] */
		{170, 350, 186, 430},
		CheckBox {
			enabled,
			"Doctrine"
		},
		/* [26] */
		{140, 95, 156, 175},
		CheckBox {
			enabled,
			"Terrain"
		},
		/* [27] */
		{140, 180, 156, 260},
		CheckBox {
			enabled,
			"Misc"
		},
		/* [28] */
		{140, 265, 156, 345},
		CheckBox {
			enabled,
			"Weather"
		},
		/* [29] */
		{140, 350, 156, 430},
		CheckBox {
			enabled,
			"Material"
		},
		/* [30] */
		{215, 95, 231, 175},
		CheckBox {
			enabled,
			"IDs"
		}
	}
};

resource 'DITL' (150, "Preferences Items") {
	{	/* array DITLarray: 9 elements */
		/* [1] */
		{160, 30, 180, 88},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{160, 175, 180, 233},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{10, 10, 28, 222},
		CheckBox {
			enabled,
			"Show Hex Grid on New Maps"
		},
		/* [4] */
		{35, 10, 53, 222},
		CheckBox {
			enabled,
			"Display Names on New Maps"
		},
		/* [5] */
		{60, 10, 78, 145},
		CheckBox {
			enabled,
			"Checkpoint Game"
		},
		/* [6] */
		{60, 145, 78, 186},
		StaticText {
			disabled,
			"Every"
		},
		/* [7] */
		{60, 190, 77, 220},
		EditText {
			enabled,
			"5"
		},
		/* [8] */
		{60, 225, 78, 268},
		StaticText {
			disabled,
			"Turns"
		},
		/* [9] */
		{85, 10, 103, 222},
		CheckBox {
			enabled,
			"Write Game Statistics File"
		}
	}
};

resource 'DITL' (132, "Progress Items") {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{10, 10, 30, 310},
		StaticText {
			disabled,
			""
		},
		/* [2] */
		{39, 10, 51, 210},
		UserItem {
			disabled
		},
		/* [3] */
		{35, 229, 55, 309},
		Button {
			disabled,
			"Stop"
		}
	}
};

resource 'DITL' (130, "Player Setup Items") {
	{	/* array DITLarray: 13 elements */
		/* [1] */
		{255, 345, 275, 425},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{220, 345, 240, 425},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{35, 10, 275, 331},
		UserItem {
			enabled
		},
		/* [4] */
		{35, 345, 55, 425},
		Button {
			enabled,
			"Add"
		},
		/* [5] */
		{60, 345, 80, 425},
		Button {
			enabled,
			"Remove"
		},
		/* [6] */
		{9, 315, 27, 326},
		UserItem {
			enabled
		},
		/* [7] */
		{85, 345, 105, 425},
		Button {
			enabled,
			"Rename"
		},
		/* [8] */
		{110, 345, 130, 425},
		Button {
			enabled,
			"Computer"
		},
		/* [9] */
		{135, 345, 155, 425},
		Button {
			enabled,
			"Remote"
		},
		/* [10] */
		{160, 345, 180, 425},
		Button {
			enabled,
			"Exchange"
		},
		/* [11] */
		{10, 25, 26, 125},
		StaticText {
			disabled,
			"Side"
		},
		/* [12] */
		{10, 140, 26, 240},
		StaticText {
			disabled,
			"Player"
		},
		/* [13] */
		{10, 240, 26, 315},
		StaticText {
			disabled,
			"Advantage"
		}
	}
};

resource 'DITL' (1000, "Init Warning Items") {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{140, 90, 160, 170},
		Button {
			enabled,
			"Continue"
		},
		/* [2] */
		{140, 300, 160, 380},
		Button {
			enabled,
			"Quit"
		},
		/* [3] */
		{10, 80, 130, 390},
		StaticText {
			disabled,
			"Warning: \"^0\"\n\nXconq may not be able to "
			"give you exactly the game that you asked"
			" for.  Do you want to continue setting u"
			"p this game?"
		}
	}
};

resource 'DITL' (134, "World Shape Items") {
	{	/* array DITLarray: 17 elements */
		/* [1] */
		{235, 100, 255, 180},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{235, 265, 255, 345},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{5, 5, 225, 225},
		UserItem {
			disabled
		},
		/* [4] */
		{55, 350, 72, 390},
		EditText {
			enabled,
			"360"
		},
		/* [5] */
		{125, 280, 142, 309},
		EditText {
			enabled,
			"60"
		},
		/* [6] */
		{125, 380, 142, 409},
		EditText {
			enabled,
			"30"
		},
		/* [7] */
		{160, 280, 177, 309},
		EditText {
			enabled,
			"0"
		},
		/* [8] */
		{160, 380, 177, 409},
		EditText {
			enabled,
			"0"
		},
		/* [9] */
		{125, 235, 142, 277},
		StaticText {
			disabled,
			"Width"
		},
		/* [10] */
		{125, 315, 141, 327},
		StaticText {
			disabled,
			"x"
		},
		/* [11] */
		{125, 330, 143, 377},
		StaticText {
			disabled,
			"Height"
		},
		/* [12] */
		{20, 230, 36, 278},
		StaticText {
			disabled,
			"World:"
		},
		/* [13] */
		{100, 230, 116, 278},
		StaticText {
			disabled,
			"Area:"
		},
		/* [14] */
		{55, 235, 72, 347},
		StaticText {
			disabled,
			"Circumference:"
		},
		/* [15] */
		{160, 235, 177, 277},
		StaticText {
			disabled,
			"Lat"
		},
		/* [16] */
		{160, 330, 177, 372},
		StaticText {
			disabled,
			"Long"
		},
		/* [17] */
		{10, 355, 42, 387},
		Icon {
			enabled,
			128
		}
	}
};

resource 'DITL' (1003, "Run Error Items") {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{140, 80, 160, 200},
		Button {
			enabled,
			"Quit"
		},
		/* [2] */
		{140, 270, 160, 390},
		Button {
			enabled,
			"Attempt to Save"
		},
		/* [3] */
		{10, 80, 130, 390},
		StaticText {
			disabled,
			"Fatal Error: \"^0\"\n\nXconq cannot possibly"
			" continue, but you may be able to save t"
			"he game to a file."
		}
	}
};

resource 'DITL' (1001, "Init Error Items") {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{140, 160, 160, 240},
		Button {
			enabled,
			"Bummer"
		},
		/* [2] */
		{10, 80, 130, 390},
		StaticText {
			disabled,
			"Fatal Setup Error: \"^0\"\n\nSomething is se"
			"riously wrong, either with Xconq or with"
			" the game that you chose."
		}
	}
};

resource 'DITL' (1002, "Run Warning Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{140, 190, 160, 270},
		Button {
			enabled,
			"Continue"
		},
		/* [2] */
		{140, 280, 160, 390},
		Button {
			enabled,
			"Save and Quit"
		},
		/* [3] */
		{140, 15, 160, 125},
		Button {
			enabled,
			"Quit, Don't Save"
		},
		/* [4] */
		{5, 80, 125, 390},
		StaticText {
			disabled,
			"Warning: \"^0\"\n\nThis is not fatal, but ma"
			"y cause more serious problems later on. "
			" Do you want to continue playing this ga"
			"me?"
		}
	}
};

resource 'DITL' (2001, "Resign Game Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{95, 70, 115, 150},
		Button {
			enabled,
			"Resign"
		},
		/* [2] */
		{95, 280, 115, 360},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{95, 160, 115, 270},
		Button {
			enabled,
			"Willing to Draw"
		},
		/* [4] */
		{10, 70, 87, 361},
		StaticText {
			disabled,
			"^0 not willing to declare a draw; you mu"
			"st resign to quit now.  Do you want to r"
			"esign, or are you willing to draw if eve"
			"ryone else does?"
		}
	}
};

resource 'DITL' (901, "Lose Game Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{115, 90, 135, 170},
		Button {
			enabled,
			"Quit Now"
		},
		/* [2] */
		{190, 90, 210, 170},
		Button {
			enabled,
			"Continue"
		},
		/* [3] */
		{35, 26, 75, 234},
		Picture {
			disabled,
			3002
		},
		/* [4] */
		{150, 5, 180, 255},
		StaticText {
			disabled,
			"If you continue, you can look around and"
			" see how the game ended."
		}
	}
};

resource 'DITL' (2000, "Quit Game Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{87, 284, 107, 344},
		Button {
			enabled,
			"Save"
		},
		/* [2] */
		{87, 211, 107, 271},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{87, 70, 107, 155},
		Button {
			enabled,
			"Don’t Save"
		},
		/* [4] */
		{10, 70, 76, 342},
		StaticText {
			disabled,
			"Save this game before quitting?"
		}
	}
};

resource 'DITL' (160, "Mplayer Items") {
	{	/* array DITLarray: 1 elements */
		/* [1] */
		{65, 5, 261, 367},
		UserItem {
			disabled
		}
	}
};

resource 'DITL' (502, "Designer Reshape Items") {
	{	/* array DITLarray: 32 elements */
		/* [1] */
		{180, 80, 200, 160},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{180, 245, 200, 325},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{10, 90, 26, 140},
		StaticText {
			disabled,
			""
		},
		/* [4] */
		{10, 165, 26, 215},
		StaticText {
			disabled,
			""
		},
		/* [5] */
		{10, 300, 26, 350},
		StaticText {
			disabled,
			""
		},
		/* [6] */
		{35, 90, 51, 140},
		EditText {
			disabled,
			""
		},
		/* [7] */
		{35, 165, 51, 215},
		EditText {
			disabled,
			""
		},
		/* [8] */
		{35, 255, 51, 305},
		EditText {
			disabled,
			""
		},
		/* [9] */
		{35, 325, 51, 375},
		EditText {
			disabled,
			""
		},
		/* [10] */
		{85, 90, 101, 140},
		EditText {
			disabled,
			""
		},
		/* [11] */
		{85, 165, 101, 215},
		EditText {
			disabled,
			""
		},
		/* [12] */
		{85, 255, 101, 305},
		EditText {
			disabled,
			""
		},
		/* [13] */
		{85, 325, 101, 375},
		EditText {
			disabled,
			""
		},
		/* [14] */
		{115, 90, 131, 140},
		EditText {
			disabled,
			""
		},
		/* [15] */
		{115, 165, 131, 215},
		EditText {
			disabled,
			""
		},
		/* [16] */
		{115, 300, 131, 350},
		EditText {
			disabled,
			""
		},
		/* [17] */
		{145, 150, 161, 200},
		EditText {
			disabled,
			"0"
		},
		/* [18] */
		{10, 10, 26, 85},
		StaticText {
			disabled,
			"Original:"
		},
		/* [19] */
		{35, 10, 51, 85},
		StaticText {
			disabled,
			"Subarea:"
		},
		/* [20] */
		{35, 225, 53, 247},
		StaticText {
			disabled,
			"At:"
		},
		/* [21] */
		{115, 10, 131, 85},
		StaticText {
			disabled,
			"Final:"
		},
		/* [22] */
		{10, 145, 26, 161},
		StaticText {
			disabled,
			"x"
		},
		/* [23] */
		{35, 145, 51, 161},
		StaticText {
			disabled,
			"x"
		},
		/* [24] */
		{35, 310, 51, 321},
		StaticText {
			disabled,
			","
		},
		/* [25] */
		{115, 145, 131, 161},
		StaticText {
			disabled,
			"x"
		},
		/* [26] */
		{10, 235, 26, 295},
		StaticText {
			disabled,
			"Circumf:"
		},
		/* [27] */
		{115, 235, 131, 295},
		StaticText {
			disabled,
			"Circumf:"
		},
		/* [28] */
		{85, 10, 101, 85},
		StaticText {
			disabled,
			"Final Suba:"
		},
		/* [29] */
		{85, 225, 103, 247},
		StaticText {
			disabled,
			"At:"
		},
		/* [30] */
		{85, 145, 101, 161},
		StaticText {
			disabled,
			"x"
		},
		/* [31] */
		{85, 310, 101, 321},
		StaticText {
			disabled,
			","
		},
		/* [32] */
		{145, 10, 161, 142},
		StaticText {
			disabled,
			"Fill New Cells with:"
		}
	}
};

resource 'DITL' (131, "Variants Items") {
	{	/* array DITLarray: 20 elements */
		/* [1] */
		{260, 105, 280, 185},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{260, 305, 280, 385},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{5, 10, 29, 480},
		StaticText {
			disabled,
			"Variants for this Game"
		},
		/* [4] */
		{45, 10, 63, 160},
		CheckBox {
			enabled,
			"World Seen"
		},
		/* [5] */
		{65, 10, 83, 160},
		CheckBox {
			enabled,
			"See All"
		},
		/* [6] */
		{85, 10, 103, 160},
		CheckBox {
			enabled,
			"Sequential"
		},
		/* [7] */
		{105, 10, 123, 160},
		CheckBox {
			enabled,
			""
		},
		/* [8] */
		{125, 10, 143, 160},
		CheckBox {
			enabled,
			""
		},
		/* [9] */
		{45, 170, 63, 320},
		CheckBox {
			enabled,
			""
		},
		/* [10] */
		{65, 170, 83, 320},
		CheckBox {
			enabled,
			""
		},
		/* [11] */
		{85, 170, 103, 320},
		CheckBox {
			enabled,
			""
		},
		/* [12] */
		{105, 170, 123, 320},
		CheckBox {
			enabled,
			""
		},
		/* [13] */
		{125, 170, 143, 320},
		CheckBox {
			enabled,
			""
		},
		/* [14] */
		{150, 10, 182, 160},
		UserItem {
			disabled
		},
		/* [15] */
		{150, 170, 182, 320},
		UserItem {
			disabled
		},
		/* [16] */
		{150, 330, 182, 480},
		UserItem {
			disabled
		},
		/* [17] */
		{50, 355, 70, 455},
		Button {
			enabled,
			"World Size..."
		},
		/* [18] */
		{80, 355, 100, 455},
		Button {
			enabled,
			"Real Time..."
		},
		/* [19] */
		{110, 355, 130, 455},
		Button {
			enabled,
			"More..."
		},
		/* [20] */
		{190, 10, 241, 480},
		UserItem {
			disabled
		}
	}
};

resource 'DITL' (201, "Side Rename Items") {
	{	/* array DITLarray: 20 elements */
		/* [1] */
		{235, 60, 255, 140},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{235, 205, 255, 285},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{5, 190, 25, 320},
		Button {
			enabled,
			"Choose Randomly"
		},
		/* [4] */
		{35, 120, 51, 320},
		EditText {
			enabled,
			""
		},
		/* [5] */
		{60, 120, 76, 320},
		EditText {
			enabled,
			""
		},
		/* [6] */
		{85, 120, 101, 320},
		EditText {
			enabled,
			""
		},
		/* [7] */
		{110, 120, 126, 320},
		EditText {
			enabled,
			""
		},
		/* [8] */
		{135, 120, 151, 320},
		EditText {
			enabled,
			""
		},
		/* [9] */
		{160, 120, 176, 320},
		EditText {
			enabled,
			""
		},
		/* [10] */
		{185, 120, 201, 320},
		EditText {
			enabled,
			""
		},
		/* [11] */
		{210, 120, 226, 320},
		EditText {
			enabled,
			""
		},
		/* [12] */
		{10, 10, 26, 122},
		StaticText {
			disabled,
			"Names for Side:"
		},
		/* [13] */
		{35, 10, 51, 110},
		StaticText {
			disabled,
			"Name:"
		},
		/* [14] */
		{60, 10, 76, 110},
		StaticText {
			disabled,
			"Long Name:"
		},
		/* [15] */
		{85, 10, 101, 110},
		StaticText {
			disabled,
			"Short Name"
		},
		/* [16] */
		{110, 10, 126, 110},
		StaticText {
			disabled,
			"Noun:"
		},
		/* [17] */
		{135, 10, 151, 110},
		StaticText {
			disabled,
			"Plural Noun:"
		},
		/* [18] */
		{160, 10, 176, 110},
		StaticText {
			disabled,
			"Adjective:"
		},
		/* [19] */
		{185, 10, 201, 110},
		StaticText {
			disabled,
			"Emblem:"
		},
		/* [20] */
		{210, 10, 226, 110},
		StaticText {
			disabled,
			"Color Scheme:"
		}
	}
};

resource 'DITL' (202, "Rename Items") {
	{	/* array DITLarray: 5 elements */
		/* [1] */
		{70, 25, 90, 105},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{70, 155, 90, 235},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{15, 185, 35, 255},
		Button {
			enabled,
			"Random"
		},
		/* [4] */
		{45, 10, 61, 210},
		EditText {
			enabled,
			""
		},
		/* [5] */
		{10, 10, 39, 180},
		StaticText {
			disabled,
			"Name:"
		}
	}
};

resource 'DITL' (605, "Help Items") {
	{	/* array DITLarray: 8 elements */
		/* [1] */
		{40, 5, 72, 37},
		UserItem {
			disabled
		},
		/* [2] */
		{45, 45, 72, 305},
		StaticText {
			disabled,
			""
		},
		/* [3] */
		{75, 5, 275, 305},
		UserItem {
			disabled
		},
		/* [4] */
		{5, 130, 37, 162},
		Icon {
			enabled,
			304
		},
		/* [5] */
		{5, 165, 37, 197},
		Icon {
			enabled,
			303
		},
		/* [6] */
		{5, 200, 37, 232},
		Icon {
			enabled,
			300
		},
		/* [7] */
		{5, 235, 37, 267},
		Icon {
			enabled,
			301
		},
		/* [8] */
		{5, 270, 37, 302},
		Icon {
			enabled,
			302
		}
	}
};

resource 'DITL' (600, "Instructions Items") {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{5, 5, 33, 230},
		StaticText {
			disabled,
			"Title"
		},
		/* [2] */
		{9, 236, 29, 295},
		Button {
			enabled,
			"Help"
		},
		/* [3] */
		{40, 5, 290, 305},
		UserItem {
			disabled
		}
	}
};

resource 'DITL' (601, "Unit Type Desc Items") {
	{	/* array DITLarray: 9 elements */
		/* [1] */
		{10, 10, 42, 42},
		UserItem {
			disabled
		},
		/* [2] */
		{10, 50, 38, 290},
		StaticText {
			disabled,
			"name"
		},
		/* [3] */
		{50, 10, 66, 400},
		StaticText {
			disabled,
			"help"
		},
		/* [4] */
		{75, 10, 91, 60},
		StaticText {
			disabled,
			"acp"
		},
		/* [5] */
		{75, 65, 91, 140},
		StaticText {
			disabled,
			"ACP/Turn"
		},
		/* [6] */
		{95, 10, 111, 60},
		StaticText {
			disabled,
			"mp"
		},
		/* [7] */
		{95, 65, 111, 140},
		StaticText {
			disabled,
			"MP/Turn"
		},
		/* [8] */
		{15, 295, 33, 401},
		CheckBox {
			enabled,
			"Available"
		},
		/* [9] */
		{140, 10, 240, 400},
		StaticText {
			disabled,
			"notes"
		}
	}
};

resource 'DITL' (602, "Material Type Desc Items") {
	{	/* array DITLarray: 8 elements */
		/* [1] */
		{10, 10, 42, 42},
		UserItem {
			disabled
		},
		/* [2] */
		{10, 50, 38, 290},
		StaticText {
			disabled,
			"name"
		},
		/* [3] */
		{50, 10, 66, 400},
		StaticText {
			disabled,
			"help"
		},
		/* [4] */
		{75, 150, 91, 225},
		StaticText {
			disabled,
			"individuals"
		},
		/* [5] */
		{75, 95, 91, 145},
		StaticText {
			disabled,
			"n"
		},
		/* [6] */
		{75, 10, 91, 90},
		StaticText {
			disabled,
			"Represents"
		},
		/* [7] */
		{15, 295, 33, 401},
		CheckBox {
			enabled,
			"Available"
		},
		/* [8] */
		{100, 10, 200, 400},
		StaticText {
			disabled,
			"notes"
		}
	}
};

resource 'DITL' (603, "Terrain Type Desc Items") {
	{	/* array DITLarray: 15 elements */
		/* [1] */
		{10, 10, 42, 42},
		UserItem {
			disabled
		},
		/* [2] */
		{10, 50, 38, 290},
		StaticText {
			disabled,
			"name"
		},
		/* [3] */
		{50, 10, 66, 400},
		StaticText {
			disabled,
			"help"
		},
		/* [4] */
		{75, 10, 91, 90},
		StaticText {
			disabled,
			"Elevations:"
		},
		/* [5] */
		{75, 95, 91, 145},
		StaticText {
			disabled,
			"min"
		},
		/* [6] */
		{75, 150, 91, 170},
		StaticText {
			disabled,
			"to"
		},
		/* [7] */
		{75, 175, 91, 225},
		StaticText {
			disabled,
			"max"
		},
		/* [8] */
		{145, 10, 245, 400},
		StaticText {
			disabled,
			"notes"
		},
		/* [9] */
		{95, 10, 111, 90},
		StaticText {
			disabled,
			"Temperatures:"
		},
		/* [10] */
		{95, 95, 111, 145},
		StaticText {
			disabled,
			"min"
		},
		/* [11] */
		{95, 150, 111, 170},
		StaticText {
			disabled,
			"to"
		},
		/* [12] */
		{95, 175, 111, 225},
		StaticText {
			disabled,
			"max"
		},
		/* [13] */
		{120, 10, 136, 85},
		StaticText {
			disabled,
			"Capacity"
		},
		/* [14] */
		{120, 95, 136, 145},
		StaticText {
			disabled,
			"n"
		},
		/* [15] */
		{15, 295, 33, 401},
		CheckBox {
			enabled,
			"Available"
		}
	}
};

resource 'DITL' (1004, "Images Missing Items") {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{160, 105, 180, 185},
		Button {
			enabled,
			"Continue"
		},
		/* [2] */
		{160, 265, 180, 345},
		Button {
			enabled,
			"Quit"
		},
		/* [3] */
		{10, 80, 150, 430},
		StaticText {
			disabled,
			"Could not find some of the icons or pict"
			"ures needed in this game, and made up su"
			"bstitutes for them. You can still play, "
			"but some displays may be hard to interpr"
			"et.\n(missing ^0^1)\n\nDo you want to conti"
			"nue anyway?"
		}
	}
};

resource 'DITL' (604, "Game Module Desc Items") {
	{	/* array DITLarray: 6 elements */
		/* [1] */
		{5, 5, 33, 305},
		StaticText {
			disabled,
			"Title"
		},
		/* [2] */
		{40, 5, 121, 305},
		StaticText {
			disabled,
			"Blurb"
		},
		/* [3] */
		{5, 310, 33, 410},
		StaticText {
			disabled,
			"Version"
		},
		/* [4] */
		{40, 310, 76, 410},
		StaticText {
			disabled,
			"for Xconq version"
		},
		/* [5] */
		{80, 310, 121, 410},
		StaticText {
			disabled,
			"program version"
		},
		/* [6] */
		{125, 5, 296, 410},
		StaticText {
			disabled,
			"Notes"
		}
	}
};

resource 'DITL' (3001, "Confirm Design Items") {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{100, 125, 120, 205},
		Button {
			enabled,
			"Design"
		},
		/* [2] */
		{100, 280, 120, 360},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{5, 95, 85, 395},
		StaticText {
			disabled,
			"Once you start designing, you cannot go "
			"back to playing the game.  Are you sure "
			"you want to do this?"
		}
	}
};

resource 'DITL' (2003, "Win Game Items") {
	{	/* array DITLarray: 5 elements */
		/* [1] */
		{85, 275, 105, 335},
		Button {
			enabled,
			"Save"
		},
		/* [2] */
		{85, 200, 105, 260},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{85, 75, 105, 160},
		Button {
			enabled,
			"Don’t Save"
		},
		/* [4] */
		{10, 20, 42, 52},
		Icon {
			disabled,
			2
		},
		/* [5] */
		{10, 72, 76, 344},
		StaticText {
			disabled,
			"Save this game before quitting?"
		}
	}
};

resource 'DITL' (902, "Game Over Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{115, 90, 135, 170},
		Button {
			enabled,
			"Quit Now"
		},
		/* [2] */
		{190, 90, 210, 170},
		Button {
			enabled,
			"Continue"
		},
		/* [3] */
		{40, 65, 57, 189},
		StaticText {
			disabled,
			"The Game is Over!"
		},
		/* [4] */
		{150, 5, 180, 255},
		StaticText {
			disabled,
			"If you continue, you can look around and"
			" see how the game ended."
		}
	}
};

resource 'DITL' (203, "Feature Rename Items") {
	{	/* array DITLarray: 6 elements */
		/* [1] */
		{77, 22, 97, 102},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{77, 160, 97, 240},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{10, 64, 30, 264},
		EditText {
			enabled,
			""
		},
		/* [4] */
		{40, 64, 60, 264},
		EditText {
			enabled,
			""
		},
		/* [5] */
		{10, 9, 30, 59},
		StaticText {
			disabled,
			"Type:"
		},
		/* [6] */
		{40, 9, 60, 59},
		StaticText {
			disabled,
			"Name:"
		}
	}
};

resource 'DITL' (128, "Splash Items") {
	{	/* array DITLarray: 8 elements */
		/* [1] */
		{210, 5, 260, 135},
		Picture {
			enabled,
			2010
		},
		/* [2] */
		{210, 140, 259, 270},
		Picture {
			enabled,
			2011
		},
		/* [3] */
		{210, 275, 259, 405},
		Picture {
			enabled,
			2012
		},
		/* [4] */
		{275, 335, 295, 405},
		Button {
			enabled,
			"Quit"
		},
		/* [5] */
		{265, 60, 281, 320},
		StaticText {
			disabled,
			"(version)"
		},
		/* [6] */
		{285, 5, 301, 320},
		StaticText {
			disabled,
			"(copyright)"
		},
		/* [7] */
		{5, 5, 205, 405},
		Picture {
			disabled,
			2000
		},
		/* [8] */
		{265, 5, 281, 60},
		StaticText {
			disabled,
			"Version"
		}
	}
};

resource 'DITL' (204, "Command Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{85, 55, 105, 135},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{85, 175, 105, 255},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{30, 10, 70, 290},
		EditText {
			enabled,
			""
		},
		/* [4] */
		{5, 10, 21, 290},
		StaticText {
			disabled,
			"Command:"
		}
	}
};

resource 'DITL' (900, "Win Game Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{110, 90, 130, 170},
		Button {
			enabled,
			"Quit Now"
		},
		/* [2] */
		{190, 90, 210, 170},
		Button {
			enabled,
			"Continue"
		},
		/* [3] */
		{30, 25, 70, 240},
		Picture {
			disabled,
			3001
		},
		/* [4] */
		{145, 5, 180, 255},
		StaticText {
			disabled,
			"If you continue, you can look around and"
			" see how the game ended."
		}
	}
};

resource 'DITL' (205, "Message Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{120, 45, 140, 125},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{120, 180, 140, 260},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{30, 10, 110, 290},
		EditText {
			enabled,
			""
		},
		/* [4] */
		{5, 10, 25, 290},
		StaticText {
			disabled,
			"Message to ^0:"
		}
	}
};

resource 'DITL' (135, "Real Time Items") {
	{	/* array DITLarray: 8 elements */
		/* [1] */
		{235, 100, 255, 180},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{235, 265, 255, 345},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{25, 230, 42, 290},
		EditText {
			enabled,
			"60"
		},
		/* [4] */
		{65, 230, 82, 290},
		EditText {
			enabled,
			"30"
		},
		/* [5] */
		{110, 230, 127, 290},
		EditText {
			enabled,
			"2"
		},
		/* [6] */
		{25, 100, 42, 220},
		StaticText {
			disabled,
			"Total Time (secs)"
		},
		/* [7] */
		{65, 100, 82, 220},
		StaticText {
			disabled,
			"Per Side (secs)"
		},
		/* [8] */
		{110, 100, 127, 220},
		StaticText {
			disabled,
			"Per Turn (secs)"
		}
	}
};

resource 'DITL' (136, "More Variants Items") {
	{	/* array DITLarray: 2 elements */
		/* [1] */
		{235, 100, 255, 180},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{235, 265, 255, 345},
		Button {
			enabled,
			"Cancel"
		}
	}
};

resource 'DITL' (206, "Message Receive Items") {
	{	/* array DITLarray: 3 elements */
		/* [1] */
		{120, 110, 140, 190},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{30, 10, 110, 290},
		EditText {
			enabled,
			""
		},
		/* [3] */
		{5, 10, 25, 290},
		StaticText {
			disabled,
			"Message from <xxx>:"
		}
	}
};

resource 'DITL' (3002, "Designer Quit Game Items") {
	{	/* array DITLarray: 4 elements */
		/* [1] */
		{87, 284, 107, 344},
		Button {
			enabled,
			"Save"
		},
		/* [2] */
		{87, 211, 107, 271},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{87, 70, 107, 155},
		Button {
			enabled,
			"Don’t Save"
		},
		/* [4] */
		{10, 70, 76, 342},
		StaticText {
			disabled,
			"Save any of this game data before quitti"
			"ng?"
		}
	}
};

resource 'DITL' (170, "Connection Method Items") {
	{	/* array DITLarray: 6 elements */
		/* [1] */
		{155, 20, 175, 100},
		Button {
			enabled,
			"OK"
		},
		/* [2] */
		{155, 130, 175, 210},
		Button {
			enabled,
			"Cancel"
		},
		/* [3] */
		{45, 35, 63, 141},
		RadioButton {
			enabled,
			"Serial Port"
		},
		/* [4] */
		{75, 35, 93, 141},
		RadioButton {
			enabled,
			"AppleTalk"
		},
		/* [5] */
		{105, 35, 123, 141},
		RadioButton {
			enabled,
			"TCP"
		},
		/* [6] */
		{10, 15, 26, 217},
		StaticText {
			disabled,
			"Connection Method:"
		}
	}
};

resource 'DLOG' (129, "New Game") {
	{30, 10, 325, 425},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	129,
	""
	/****** Extra bytes follow... ******/
	/* $"A328 0A"                                            /* £(. */
};

resource 'DLOG' (200, "Side Closeup") {
	{162, 130, 300, 474},
	noGrowDocProc,
	invisible,
	noGoAway,
	0x0,
	200,
	""
};

resource 'DLOG' (130, "Player Setup") {
	{45, 30, 335, 480},
	noGrowDocProc,
	visible,
	noGoAway,
	0x0,
	130,
	"Player Setup"
	/****** Extra bytes follow... ******/
	/* $"CB28 0A"                                            /* À(. */
};

resource 'DLOG' (128, "Splash") {
	{30, 49, 335, 459},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	128,
	""
	/****** Extra bytes follow... ******/
	/* $"6E28 0A"                                            /* n(. */
};

resource 'DLOG' (501, "Designer Save") {
	{30, 10, 332, 450},
	dBoxProc,
	visible,
	goAway,
	0x0,
	501,
	""
	/****** Extra bytes follow... ******/
	/* $"7128 0A"                                            /* q(. */
};

resource 'DLOG' (150, "Preferences") {
	{40, 40, 240, 314},
	dBoxProc,
	visible,
	noGoAway,
	0x0,
	150,
	""
};

resource 'DLOG' (132, "Progress") {
	{270, 10, 330, 330},
	dBoxProc,
	visible,
	noGoAway,
	0x0,
	132,
	""
	/****** Extra bytes follow... ******/
	/* $"1928 0A"                                            /* .(. */
};

resource 'DLOG' (605, "Help (unused)") {
	{43, 154, 283, 495},
	documentProc,
	invisible,
	goAway,
	0x0,
	605,
	"Help"
};

resource 'DLOG' (131, "Variants") {
	{30, 10, 330, 500},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	131,
	""
	/****** Extra bytes follow... ******/
	/* $"9828 0A"                                            /* ò(. */
};

resource 'DLOG' (134, "World Shape") {
	{40, 40, 300, 460},
	dBoxProc,
	visible,
	goAway,
	0x0,
	134,
	""
	/****** Extra bytes follow... ******/
	/* $"FF28 0A"                                            /* ˇ(. */
};

resource 'DLOG' (900, "Win Game") {
	{71, 129, 291, 389},
	dBoxProc,
	visible,
	noGoAway,
	0x0,
	900,
	""
	/****** Extra bytes follow... ******/
	/* $"0A28 0A"                                            /* .(. */
};

resource 'DLOG' (901, "Lose Game") {
	{70, 130, 290, 390},
	dBoxProc,
	visible,
	goAway,
	0x0,
	901,
	""
	/****** Extra bytes follow... ******/
	/* $"0028 0A"                                            /* .(. */
};

resource 'DLOG' (160, "Mplayer") {
	{60, 130, 328, 502},
	noGrowDocProc,
	visible,
	goAway,
	0x0,
	160,
	""
};

resource 'DLOG' (502, "Designer Reshape") {
	{40, 30, 250, 430},
	dBoxProc,
	visible,
	goAway,
	0x0,
	502,
	""
};

resource 'DLOG' (201, "Side Rename") {
	{40, 40, 300, 370},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	201,
	""
};

resource 'DLOG' (202, "Rename") {
	{40, 40, 142, 306},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	202,
	""
};

resource 'DLOG' (600, "Instructions") {
	{180, 328, 478, 638},
	noGrowDocProc,
	invisible,
	goAway,
	0x0,
	600,
	""
};

resource 'DLOG' (601, "Unit Type Desc") {
	{40, 100, 340, 510},
	noGrowDocProc,
	invisible,
	goAway,
	0x0,
	601,
	""
};

resource 'DLOG' (602, "Material Type Desc") {
	{40, 100, 340, 510},
	noGrowDocProc,
	invisible,
	goAway,
	0x0,
	602,
	""
};

resource 'DLOG' (603, "Terrain Type Desc") {
	{40, 100, 340, 510},
	noGrowDocProc,
	invisible,
	goAway,
	0x0,
	603,
	""
};

resource 'DLOG' (604, "Game Module Desc") {
	{40, 90, 340, 510},
	noGrowDocProc,
	invisible,
	goAway,
	0x0,
	604,
	""
};

resource 'DLOG' (902, "Game Over") {
	{70, 130, 290, 390},
	dBoxProc,
	visible,
	noGoAway,
	0x0,
	902,
	""
	/****** Extra bytes follow... ******/
	/* $"0028 0A"                                            /* .(. */
};

resource 'DLOG' (203, "Feature Rename") {
	{51, 34, 160, 314},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	203,
	""
};

resource 'DLOG' (133, "About") {
	{30, 49, 332, 459},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	133,
	""
	/****** Extra bytes follow... ******/
	/* $"6E28 0A"                                            /* n(. */
};

resource 'DLOG' (204, "Command") {
	{40, 40, 155, 340},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	204,
	""
};

resource 'DLOG' (205, "Message") {
	{40, 40, 190, 340},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	205,
	""
	/****** Extra bytes follow... ******/
	/* $"0028 0A"                                            /* .(. */
};

resource 'DLOG' (135, "Real Time") {
	{40, 40, 300, 460},
	dBoxProc,
	visible,
	goAway,
	0x0,
	135,
	""
	/****** Extra bytes follow... ******/
	/* $"FF28 0A"                                            /* ˇ(. */
};

resource 'DLOG' (136, "More Variants") {
	{40, 40, 300, 460},
	dBoxProc,
	visible,
	goAway,
	0x0,
	136,
	""
	/****** Extra bytes follow... ******/
	/* $"FF28 0A"                                            /* ˇ(. */
};

resource 'DLOG' (206, "Message Receive") {
	{40, 40, 190, 340},
	dBoxProc,
	invisible,
	noGoAway,
	0x0,
	206,
	""
	/****** Extra bytes follow... ******/
	/* $"0028 0A"                                            /* .(. */
};

resource 'DLOG' (170, "Connection Method") {
	{57, 121, 267, 371},
	dBoxProc,
	visible,
	noGoAway,
	0x0,
	170,
	""
	/****** Extra bytes follow... ******/
	/* $"2728 0A"                                            /* '(. */
};

resource 'WIND' (133, "History") {
	{197, 157, 337, 507},
	zoomDocProc,
	invisible,
	goAway,
	0x0,
	"History"
};

resource 'WIND' (130, "Map") {
	{40, 2, 314, 434},
	zoomDocProc,
	invisible,
	goAway,
	0x0,
	"Map"
};

resource 'WIND' (131, "List") {
	{80, 270, 380, 620},
	zoomDocProc,
	invisible,
	goAway,
	0x0,
	"List"
};

resource 'WIND' (128, "Game") {
	{40, 438, 140, 638},
	noGrowDocProc,
	invisible,
	goAway,
	0x0,
	"Game"
};

resource 'WIND' (129, "Design") {
	{0, 0, 256, 120},
	2048,
	invisible,
	goAway,
	0x0,
	"Design"
};

resource 'WIND' (132, "Unit Closeup") {
	{42, 296, 327, 506},
	noGrowDocProc,
	invisible,
	goAway,
	0x0,
	"Closeup"
};

resource 'WIND' (134, "Construction") {
	{318, 4, 478, 504},
	zoomDocProc,
	invisible,
	goAway,
	0x0,
	"Construction Plan"
};

resource 'WIND' (135, "Help") {
	{42, 164, 338, 506},
	zoomDocProc,
	invisible,
	goAway,
	0x0,
	"Help"
};

resource 'WIND' (136, "Notices") {
	{204, 4, 337, 404},
	zoomDocProc,
	invisible,
	goAway,
	0x0,
	"Notices"
};

resource 'FREF' (128, purgeable) {
	'APPL',
	0,
	""
};

resource 'FREF' (129, purgeable) {
	'TEXT',
	1,
	""
};

resource 'FREF' (130, purgeable) {
	'GAME',
	2,
	""
};

resource 'FREF' (131, purgeable) {
	'rsrc',
	3,
	""
};

resource 'FREF' (132) {
	'rsrc',
	3,
	""
};

resource 'ICN#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"0030 0003 00CC 000C 0303 0030 0C00 C0C0"
		$"3000 3300 C0E0 0C00 80A0 0800 80E7 0800"
		$"80A5 0800 9CFF 0800 94B5 0800 9FFF C800"
		$"96B5 4800 9FFF C800 96B5 4800 9FFF E800"
		$"8000 0800 C000 0C00 3000 3300 0C00 C0C0"
		$"0303 0030 00CC 000C 0030 0003 0020 0001"
		$"0020 0001 0020 0801 0022 1A11 0021 DF61"
		$"0027 FFFD 0023 FFF9 0020 0001 0020 0001",
		/* [2] */
		$"0030 0003 00FC 000F 03FF 003F 0FFF C0FF"
		$"3FFF F3FF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF 3FFF FFFF 0FFF FFFF"
		$"03FF FFFF 00FF FFFF 003F FFFF 003F FFFF"
		$"003F FFFF 003F FFFF 003F FFFF 003F FFFF"
		$"003F FFFF 003F FFFF 003F FFFF 003F FFFF"
	}
};

resource 'ICN#' (129) {
	{	/* array: 2 elements */
		/* [1] */
		$"0FFF FE00 0802 0300 0802 0280 0802 0240"
		$"0803 0220 080C C210 0830 33F8 08C0 0C08"
		$"0B00 0338 0C0E 00C8 080A 0088 080E 7088"
		$"080A 5088 09CF F088 094B 5088 09FF FC88"
		$"096B 5488 09FF FC88 096B 5488 09FF FE88"
		$"0800 0088 0C00 00C8 0B00 0338 08C0 0C08"
		$"0830 3008 080C C008 0803 0048 0802 10D8"
		$"0802 0EF8 0802 3FF8 0802 1FF8 0FFF FFF8",
		/* [2] */
		$"0FFF FE00 0FFF FF00 0FFF FF80 0FFF FFC0"
		$"0FFF FFE0 0FFF FFF0 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
	}
};

resource 'ICN#' (130) {
	{	/* array: 2 elements */
		/* [1] */
		$"0FFF FFF8 0802 0008 0802 0008 0802 0008"
		$"0803 0008 080C C008 0830 3008 08C0 0C08"
		$"0B00 0338 0C07 00C8 0807 0088 0805 3088"
		$"0807 3088 08E5 F088 08A7 B088 08FF F088"
		$"08B5 B688 08FF F688 08B5 BE88 09FF FE88"
		$"0800 0088 0C00 00C8 0B00 0338 08C0 0C08"
		$"0830 3008 0FEC C008 0423 0048 0222 10D8"
		$"0122 0EF8 00A2 3FF8 0062 1FF8 003F FFF8",
		/* [2] */
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 0FFF FFF8 0FFF FFF8"
		$"0FFF FFF8 0FFF FFF8 07FF FFF8 03FF FFF8"
		$"01FF FFF8 00FF FFF8 007F FFF8 003F FFF8"
	}
};

resource 'ICN#' (131) {
	{	/* array: 2 elements */
		/* [1] */
		$"AAAA AAAA 0001 0001 8000 8000 01C1 0001"
		$"8140 8000 01CD 1FF1 814C 9C00 39FD 1FF1"
		$"A96C 8000 3FFD 1FF1 AB6C 8000 3FFD 1FF1"
		$"AB6C 8000 3FFD 0001 8000 8000 5555 5555"
		$"AAAA AAAA 7FFF 0001 F3F2 8100 67E7 0321"
		$"D7D6 8510 3839 3949 FFFE A128 7FFF 2129"
		$"FFFE A128 7FFF 2129 F3F2 B948 67E7 0511"
		$"D7D6 8320 3839 0101 FFFE 8000 5555 5555",
		/* [2] */
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	}
};

resource 'ICN#' (132) {
	{	/* array: 2 elements */
		/* [1] */
		$"0060 0000 0198 0000 0606 0000 1801 8000"
		$"6000 6000 8000 1000 8000 1000 8000 1000"
		$"8000 1000 8000 1000 8000 1000 8000 1000"
		$"8000 1000 8000 1000 8000 1000 8000 1000"
		$"8000 1000 6000 6000 1801 8000 0606 0000"
		$"0198 0000 0060",
		/* [2] */
		$"0060 0000 01F8 0000 07FE 0000 1FFF 8000"
		$"7FFF E000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 7FFF E000 1FFF 8000 07FE 0000"
		$"01F8 0000 0060"
	}
};

resource 'ICN#' (133) {
	{	/* array: 2 elements */
		/* [1] */
		$"0060 0000 0078 0000 001E 0000 0007 8000"
		$"0001 E000 0000 60",
		/* [2] */
		$"0030 0003 00FC 000F 03FF 003F 0FFF C0FF"
		$"3FFF F3FF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF 3FFF FFFF 0FFF FFFF"
		$"03FF FFFF 00FF FFFF 003F FFFF 003F FFFF"
		$"003F FFFF 003F FFFF 003F FFFF 003F FFFF"
		$"003F FFFF 003F FFFF 003F FFFF 003F FFFF"
	}
};

resource 'ICN#' (134) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"0000 2000 0000 3000 0000 3000 0000 3000"
		$"0000 3000 0000 3000 0000 3000 0000 3000"
		$"0000 3000 0000 3000 0000 3000 0000 3000"
		$"0000 3000 0000 20",
		/* [2] */
		$"0060 0000 01F8 0000 07FE 0000 1FFF 8000"
		$"7FFF E000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 7FFF E000 1FFF 8000 07FE 0000"
		$"01F8 0000 0060"
	}
};

resource 'ICN#' (135) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"0000 7000 0001 E000 0007 8000 001E 0000"
		$"0078 0000 0060",
		/* [2] */
		$"0060 0000 01F8 0000 07FE 0000 1FFF 8000"
		$"7FFF E000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 7FFF E000 1FFF 8000 07FE 0000"
		$"01F8 0000 0060"
	}
};

resource 'ICN#' (136) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"E000 0000 7800 0000 1E00 0000 0780 0000"
		$"01E0 0000 0060",
		/* [2] */
		$"0060 0000 01F8 0000 07FE 0000 1FFF 8000"
		$"7FFF E000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 7FFF E000 1FFF 8000 07FE 0000"
		$"01F8 0000 0060"
	}
};

resource 'ICN#' (137) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0000 0000 0000 0000 0000 0000 0000"
		$"4000 0000 C000 0000 C000 0000 C000 0000"
		$"C000 0000 C000 0000 C000 0000 C000 0000"
		$"C000 0000 C000 0000 C000 0000 C000 0000"
		$"C000 0000 40",
		/* [2] */
		$"0060 0000 01F8 0000 07FE 0000 1FFF 8000"
		$"7FFF E000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 7FFF E000 1FFF 8000 07FE 0000"
		$"01F8 0000 0060"
	}
};

resource 'ICN#' (138) {
	{	/* array: 2 elements */
		/* [1] */
		$"0060 0000 01E0 0000 0780 0000 1E00 0000"
		$"7800 0000 E0",
		/* [2] */
		$"0060 0000 01F8 0000 07FE 0000 1FFF 8000"
		$"7FFF E000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 7FFF E000 1FFF 8000 07FE 0000"
		$"01F8 0000 0060"
	}
};

resource 'ICN#' (141) {
	{	/* array: 2 elements */
		/* [1] */
		$"0000 0000 0000 0000 0C02 0000 0607 0000"
		$"0606 0000 030C 0000 030C 0000 0198 0000"
		$"0198 0000 00F0 0000 FFFF F800 FFFF F800"
		$"00E0 0000 01B0 0000 01B0 0000 0318 0000"
		$"0318 0000 060C 0000 060C 0000 0C06",
		/* [2] */
		$"0060 0000 01F8 0000 07FE 0000 1FFF 8000"
		$"7FFF E000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 7FFF E000 1FFF 8000 07FE 0000"
		$"01F8 0000 0060"
	}
};

resource 'ICN#' (140) {
	{	/* array: 2 elements */
		/* [1] */
		$"0060 0000 01F8 0000 079E 0000 1E07 8000"
		$"7801 E000 E000 7000 C000 3000 C000 3000"
		$"C000 3000 C000 3000 C000 3000 C000 3000"
		$"C000 3000 C000 3000 C000 3000 C000 3000"
		$"E000 7000 7801 E000 1E07 8000 079E 0000"
		$"01F8 0000 0060",
		/* [2] */
		$"0060 0000 01F8 0000 07FE 0000 1FFF 8000"
		$"7FFF E000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 FFFF F000 FFFF F000 FFFF F000"
		$"FFFF F000 7FFF E000 1FFF 8000 07FE 0000"
		$"01F8 0000 0060"
	}
};

resource 'BNDL' (128) {
	'XCNQ',
	0,
	{	/* array TypeArray: 2 elements */
		/* [1] */
		'FREF',
		{	/* array IDArray: 4 elements */
			/* [1] */
			0, 128,
			/* [2] */
			1, 129,
			/* [3] */
			2, 130,
			/* [4] */
			3, 132
		},
		/* [2] */
		'ICN#',
		{	/* array IDArray: 4 elements */
			/* [1] */
			0, 128,
			/* [2] */
			1, 129,
			/* [3] */
			2, 130,
			/* [4] */
			3, 131
		}
	}
};

resource 'icl8' (128) {
	$"0000 0000 0000 0000 0000 FFFF 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 FFFF"
	$"0000 0000 0000 0000 FFFF E3E3 FFFF 0000"
	$"0000 0000 0000 0000 0000 0000 FFFF 0505"
	$"0000 0000 0000 FFFF E3E3 E3E3 E3E3 FFFF"
	$"0000 0000 0000 0000 0000 FFFF 0505 0505"
	$"0000 0000 FFFF E3E3 E3E3 E3E3 E3E3 E3E3"
	$"FFFF 0000 0000 0000 FFFF 0505 0505 0505"
	$"0000 FFFF E3E3 E3E3 E3E3 E3E3 E3E3 E3E3"
	$"E3E3 FFFF 0000 FFFF 0505 0505 0505 0505"
	$"FFFF E3E3 E3E3 E3E3 FFFF FFE3 E3E3 E3E3"
	$"E3E3 E3E3 FFFF 0505 0505 0505 0505 0505"
	$"FFE3 E3E3 E3E3 E3E3 FFE3 FFE3 E3E3 E3E3"
	$"E3E3 E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3E3 E3E3 E3E3 FFFF FFE3 E3FF FFFF"
	$"E3E3 E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3E3 E3E3 E3E3 FFE3 FFE3 E3FF E3FF"
	$"E3E3 E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3FF FFFF E3E3 FFFF FFFF FFFF FFFF"
	$"E3E3 E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3FF E3FF E3E3 FFE3 FFFF E3FF E3FF"
	$"E3E3 E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3FF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3FF E3FF FFE3 FFE3 FFFF E3FF E3FF"
	$"E3FF E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3FF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3FF E3FF FFE3 FFE3 FFFF E3FF E3FF"
	$"E3FF E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3FF FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFE3 FF05 0505 0505 0505 0505 0505"
	$"FFE3 E3E3 E3E3 E3E3 E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 FF05 0505 0505 0505 0505 0505"
	$"FFFF E3E3 E3E3 E3E3 E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 FFFF 0505 0505 0505 0505 0505"
	$"0000 FFFF E3E3 E3E3 E3E3 E3E3 E3E3 E3E3"
	$"E3E3 FFFF C0C0 FFFF 0505 0505 0505 0505"
	$"0000 0000 FFFF E3E3 E3E3 E3E3 E3E3 E3E3"
	$"FFFF C0C0 C0C0 C0C0 FFFF 0505 0505 0505"
	$"0000 0000 0000 FFFF E3E3 E3E3 E3E3 FFFF"
	$"C0C0 C0C0 C0C0 C0C0 C0C0 FFFF 0505 0505"
	$"0000 0000 0000 0000 FFFF E3E3 FFFF C0C0"
	$"C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 FFFF 0505"
	$"0000 0000 0000 0000 0000 FFFF C0C0 C0C0"
	$"C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 FFFF"
	$"0000 0000 0000 0000 0000 FFC0 C0C0 C0C0"
	$"C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C0FF"
	$"0000 0000 0000 0000 0000 FFC0 C0C0 C0C0"
	$"C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C0FF"
	$"0000 0000 0000 0000 0000 FFC0 C0C0 C0C0"
	$"C0C0 C0C0 FFC0 C0C0 C0C0 C0C0 C0C0 C0FF"
	$"0000 0000 0000 0000 0000 FFC0 C0C0 FFC0"
	$"C0C0 C0FF FFC0 FFC0 C0C0 C0FF C0C0 C0FF"
	$"0000 0000 0000 0000 0000 FFC0 C0C0 C0FF"
	$"FFFF C0FF FFFF FFFF C0FF FFC0 C0C0 C0FF"
	$"0000 0000 0000 0000 0000 FFC0 C0FF FFFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF C0FF"
	$"0000 0000 0000 0000 0000 FFC0 C0C0 FFFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FFC0 C0FF"
	$"0000 0000 0000 0000 0000 FFC0 C0C0 C0C0"
	$"C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C0FF"
	$"0000 0000 0000 0000 0000 FFC0 C0C0 C0C0"
	$"C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C0FF"
};

resource 'icl8' (129) {
	$"0000 0000 FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FF00 0000 0000 0000 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFE3"
	$"E3E3 E3E3 E3E3 FFFF 0000 0000 0000 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFE3"
	$"E3E3 E3E3 E3E3 FF00 FF00 0000 0000 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFE3"
	$"E3E3 E3E3 E3E3 FF00 00FF 0000 0000 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFFF"
	$"E3E3 E3E3 E3E3 FF00 0000 FF00 0000 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 FFFF E3E3"
	$"FFFF E3E3 E3E3 FF00 0000 00FF 0000 0000"
	$"0000 0000 FFE3 E3E3 E3E3 FFFF E3E3 E3E3"
	$"E3E3 FFFF E3E3 FFFF FFFF FFFF FF00 0000"
	$"0000 0000 FFE3 E3E3 FFFF E3E3 E3E3 E3E3"
	$"E3E3 E3E3 FFFF E3E3 E3E3 E3E3 FF00 0000"
	$"0000 0000 FFE3 FFFF E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 E3E3 FFFF E3E3 FFFF FF00 0000"
	$"0000 0000 FFFF E3E3 E3E3 E3E3 FFFF FFE3"
	$"E3E3 E3E3 E3E3 E3E3 FFFF 0505 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 FFE3 FFE3"
	$"E3E3 E3E3 E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 FFFF FFE3"
	$"E3FF FFFF E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 FFE3 FFE3"
	$"E3FF E3FF E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF FFFF E3E3 FFFF FFFF"
	$"FFFF FFFF E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF E3FF E3E3 FFE3 FFFF"
	$"E3FF E3FF E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF E3FF FFE3 FFE3 FFFF"
	$"E3FF E3FF E3FF E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF E3FF FFE3 FFE3 FFFF"
	$"E3FF E3FF E3FF E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FFE3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFFF E3E3 E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 E3E3 E3E3 FFFF 0505 FF00 0000"
	$"0000 0000 FFE3 FFFF E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 E3E3 FFFF C0C0 FFFF FF00 0000"
	$"0000 0000 FFE3 E3E3 FFFF E3E3 E3E3 E3E3"
	$"E3E3 E3E3 FFFF C0C0 C0C0 C0C0 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 FFFF E3E3 E3E3"
	$"E3E3 FFFF C0C0 C0C0 C0C0 C0C0 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 FFFF E3E3"
	$"FFFF C0C0 C0C0 C0C0 C0C0 C0C0 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFFF"
	$"C0C0 C0C0 C0C0 C0C0 C0FF C0C0 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFC0"
	$"C0C0 C0FF C0C0 C0C0 FFFF C0FF FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFC0"
	$"C0C0 C0C0 FFFF FFC0 FFFF FFFF FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFC0"
	$"C0C0 FFFF FFFF FFFF FFFF FFFF FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFC0"
	$"C0C0 C0FF FFFF FFFF FFFF FFFF FF00 0000"
	$"0000 0000 FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FF"
};

resource 'icl8' (131) {
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 0000 0000 00FF FFFF 0000 0000 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 0000 0000 00FF 00FF 0000 0000 002B"
	$"2B00 00EC ECEC D8D8 D8D8 D8D8 0000 002B"
	$"2B00 0000 0000 00FF FFFF 0000 FFFF 002B"
	$"2B00 00EC ECEC 0000 0000 0000 0000 002B"
	$"2B00 0000 0000 00FF 00FF 0000 FFFF 002B"
	$"2B00 00EC ECEC D8D8 D8D8 D8D8 0000 002B"
	$"2B00 FFFF FF00 00FF FFFF FFFF FFFF 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 FF00 FF00 00FF 00FF FF00 FFFF 002B"
	$"2B00 00D8 D8D8 D8D8 D8D8 D8D8 0000 002B"
	$"2B00 FFFF FFFF FFFF FFFF FFFF FFFF 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 FF00 FFFF 00FF 00FF FF00 FFFF 002B"
	$"2B00 00D8 D8D8 D8D8 D8D8 D8D8 0000 002B"
	$"2B00 FFFF FFFF FFFF FFFF FFFF FFFF 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 FF00 FFFF 00FF 00FF FF00 FFFF 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 FFFF FFFF FFFF FFFF FFFF FFFF 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
	$"2BC0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C02B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2BC0 C0C0 0000 C0C0 C0C0 C0C0 0000 C02B"
	$"2B00 0000 0000 00FC 0000 0000 0000 002B"
	$"2BC0 C000 00C0 C0C0 C0C0 C000 00C0 C02B"
	$"2B00 0000 0000 FCFC 0000 FC00 0000 002B"
	$"2BC0 00C0 00C0 C0C0 C0C0 00C0 00C0 C02B"
	$"2B00 0000 00FC 00FC 0000 00FC 0000 002B"
	$"2B00 C0C0 C000 0000 0000 C0C0 C000 002B"
	$"2B00 FCFC FC00 00FC 00FC 0000 FC00 002B"
	$"2BC0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C02B"
	$"2B00 FC00 0000 00FC 0000 FC00 FC00 002B"
	$"2BC0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C02B"
	$"2B00 FC00 0000 00FC 0000 FC00 FC00 002B"
	$"2BC0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C02B"
	$"2B00 FC00 0000 00FC 0000 FC00 FC00 002B"
	$"2BC0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C02B"
	$"2B00 FC00 0000 00FC 0000 FC00 FC00 002B"
	$"2BC0 C0C0 0000 C0C0 C0C0 C0C0 0000 C02B"
	$"2B00 FCFC FC00 00FC 00FC 0000 FC00 002B"
	$"2BC0 C000 00C0 C0C0 C0C0 C000 00C0 C02B"
	$"2B00 0000 00FC 00FC 0000 00FC 0000 002B"
	$"2BC0 00C0 00C0 C0C0 C0C0 00C0 00C0 C02B"
	$"2B00 0000 0000 FCFC 0000 FC00 0000 002B"
	$"2B00 C0C0 C000 0000 0000 C0C0 C000 002B"
	$"2B00 0000 0000 00FC 0000 0000 0000 002B"
	$"2BC0 C0C0 C0C0 C0C0 C0C0 C0C0 C0C0 C02B"
	$"2B00 0000 0000 0000 0000 0000 0000 002B"
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
};

resource 'icl8' (130) {
	$"0000 0000 FFFF FFFF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFE3"
	$"E3E3 E3E3 E3E3 E3E3 E3E3 E3E3 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFE3"
	$"E3E3 E3E3 E3E3 E3E3 E3E3 E3E3 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFE3"
	$"E3E3 E3E3 E3E3 E3E3 E3E3 E3E3 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 FFFF"
	$"E3E3 E3E3 E3E3 E3E3 E3E3 E3E3 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 FFFF E3E3"
	$"FFFF E3E3 E3E3 E3E3 E3E3 E3E3 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 FFFF E3E3 E3E3"
	$"E3E3 FFFF E3E3 E3E3 E3E3 E3E3 FF00 0000"
	$"0000 0000 FFE3 E3E3 FFFF E3E3 E3E3 E3E3"
	$"E3E3 E3E3 FFFF E3E3 E3E3 E3E3 FF00 0000"
	$"0000 0000 FFE3 FFFF E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 E3E3 FFFF E3E3 FFFF FF00 0000"
	$"0000 0000 FFFF E3E3 E3E3 E3E3 FFFF FFE3"
	$"E3E3 E3E3 E3E3 E3E3 FFFF 0505 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 FFE3 FFE3"
	$"E3E3 E3E3 E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 FFFF FFE3"
	$"E3FF FFFF E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 FFE3 FFE3"
	$"E3FF E3FF E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF FFFF E3E3 FFFF FFFF"
	$"FFFF FFFF E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF E3FF E3E3 FFE3 FFFF"
	$"E3FF E3FF E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF E3FF FFE3 FFE3 FFFF"
	$"E3FF E3FF E3FF E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF E3FF FFE3 FFE3 FFFF"
	$"E3FF E3FF E3FF E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3FF FFFF FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FFE3 FF05 0505 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 E3E3 E3E3 FF05 0505 FF00 0000"
	$"0000 0000 FFFF E3E3 E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 E3E3 E3E3 FFFF 0505 FF00 0000"
	$"0000 0000 FFE3 FFFF E3E3 E3E3 E3E3 E3E3"
	$"E3E3 E3E3 E3E3 FFFF C0C0 FFFF FF00 0000"
	$"0000 0000 FFE3 E3E3 FFFF E3E3 E3E3 E3E3"
	$"E3E3 E3E3 FFFF C0C0 C0C0 C0C0 FF00 0000"
	$"0000 0000 FFE3 E3E3 E3E3 FFFF E3E3 E3E3"
	$"E3E3 FFFF C0C0 C0C0 C0C0 C0C0 FF00 0000"
	$"0000 0000 FFFF FFFF FFFF FFE3 FFFF E3E3"
	$"FFFF C0C0 C0C0 C0C0 C0C0 C0C0 FF00 0000"
	$"0000 0000 00FF 0000 0000 FFE3 E3E3 FFFF"
	$"C0C0 C0C0 C0C0 C0C0 C0FF C0C0 FF00 0000"
	$"0000 0000 0000 FF00 0000 FFE3 E3E3 FFC0"
	$"C0C0 C0FF C0C0 C0C0 FFFF C0FF FF00 0000"
	$"0000 0000 0000 00FF 0000 FFE3 E3E3 FFC0"
	$"C0C0 C0C0 FFFF FFC0 FFFF FFFF FF00 0000"
	$"0000 0000 0000 0000 FF00 FFE3 E3E3 FFC0"
	$"C0C0 FFFF FFFF FFFF FFFF FFFF FF00 0000"
	$"0000 0000 0000 0000 00FF FFE3 E3E3 FFC0"
	$"C0C0 C0FF FFFF FFFF FFFF FFFF FF00 0000"
	$"0000 0000 0000 0000 0000 FFFF FFFF FFFF"
	$"FFFF FFFF FFFF FFFF FFFF FFFF FF"
};

resource 'icl4' (128) {
	$"0000 0000 00FF 0000 0000 0000 0000 00FF"
	$"0000 0000 FF88 FF00 0000 0000 0000 FF11"
	$"0000 00FF 8888 88FF 0000 0000 00FF 1111"
	$"0000 FF88 8888 8888 FF00 0000 FF11 1111"
	$"00FF 8888 8888 8888 88FF 00FF 1111 1111"
	$"FF88 8888 FFF8 8888 8888 FF11 1111 1111"
	$"F888 8888 F8F8 8888 8888 F111 1111 1111"
	$"F888 8888 FFF8 8FFF 8888 F111 1111 1111"
	$"F888 8888 F8F8 8F8F 8888 F111 1111 1111"
	$"F88F FF88 FFFF FFFF 8888 F111 1111 1111"
	$"F88F 8F88 F8FF 8F8F 8888 F111 1111 1111"
	$"F88F FFFF FFFF FFFF FF88 F111 1111 1111"
	$"F88F 8FF8 F8FF 8F8F 8F88 F111 1111 1111"
	$"F88F FFFF FFFF FFFF FF88 F111 1111 1111"
	$"F88F 8FF8 F8FF 8F8F 8F88 F111 1111 1111"
	$"F88F FFFF FFFF FFFF FFF8 F111 1111 1111"
	$"F888 8888 8888 8888 8888 F111 1111 1111"
	$"FF88 8888 8888 8888 8888 FF11 1111 1111"
	$"00FF 8888 8888 8888 88FF 77FF 1111 1111"
	$"0000 FF88 8888 8888 FF77 7777 FF11 1111"
	$"0000 00FF 8888 88FF 7777 7777 77FF 1111"
	$"0000 0000 FF88 FF77 7777 7777 7777 FF11"
	$"0000 0000 00FF 7777 7777 7777 7777 77FF"
	$"0000 0000 00F7 7777 7777 7777 7777 777F"
	$"0000 0000 00F7 7777 7777 7777 7777 777F"
	$"0000 0000 00F7 7777 7777 F777 7777 777F"
	$"0000 0000 00F7 77F7 777F F7F7 777F 777F"
	$"0000 0000 00F7 777F FF7F FFFF 7FF7 777F"
	$"0000 0000 00F7 7FFF FFFF FFFF FFFF FF7F"
	$"0000 0000 00F7 77FF FFFF FFFF FFFF F77F"
	$"0000 0000 00F7 7777 7777 7777 7777 777F"
	$"0000 0000 00F7 7777 7777 7777 7777 777F"
};

resource 'icl4' (129) {
	$"0000 FFFF FFFF FFFF FFFF FFF0 0000 0000"
	$"0000 F888 8888 88F8 8888 88FF 0000 0000"
	$"0000 F888 8888 88F8 8888 88F0 F000 0000"
	$"0000 F888 8888 88F8 8888 88F0 0F00 0000"
	$"0000 F888 8888 88FF 8888 88F0 00F0 0000"
	$"0000 F888 8888 FF88 FF88 88F0 000F 0000"
	$"0000 F888 88FF 8888 88FF 88FF FFFF F000"
	$"0000 F888 FF88 8888 8888 FF88 8888 F000"
	$"0000 F8FF 8888 8888 8888 88FF 88FF F000"
	$"0000 FF88 8888 FFF8 8888 8888 FF11 F000"
	$"0000 F888 8888 F8F8 8888 8888 F111 F000"
	$"0000 F888 8888 FFF8 8FFF 8888 F111 F000"
	$"0000 F888 8888 F8F8 8F8F 8888 F111 F000"
	$"0000 F88F FF88 FFFF FFFF 8888 F111 F000"
	$"0000 F88F 8F88 F8FF 8F8F 8888 F111 F000"
	$"0000 F88F FFFF FFFF FFFF FF88 F111 F000"
	$"0000 F88F 8FF8 F8FF 8F8F 8F88 F111 F000"
	$"0000 F88F FFFF FFFF FFFF FF88 F111 F000"
	$"0000 F88F 8FF8 F8FF 8F8F 8F88 F111 F000"
	$"0000 F88F FFFF FFFF FFFF FFF8 F111 F000"
	$"0000 F888 8888 8888 8888 8888 F111 F000"
	$"0000 FF88 8888 8888 8888 8888 FF11 F000"
	$"0000 F8FF 8888 8888 8888 88FF 77FF F000"
	$"0000 F888 FF88 8888 8888 FF77 7777 F000"
	$"0000 F888 88FF 8888 88FF 7777 7777 F000"
	$"0000 F888 8888 FF88 FF77 7777 7777 F000"
	$"0000 F888 8888 88FF 7777 7777 7F77 F000"
	$"0000 F888 8888 88F7 777F 7777 FF7F F000"
	$"0000 F888 8888 88F7 7777 FFF7 FFFF F000"
	$"0000 F888 8888 88F7 77FF FFFF FFFF F000"
	$"0000 F888 8888 88F7 777F FFFF FFFF F000"
	$"0000 FFFF FFFF FFFF FFFF FFFF FFFF F0"
};

resource 'icl4' (131) {
	$"CCCC CCCC CCCC CCCC CCCC CCCC CCCC CCCC"
	$"C000 0000 0000 000C C000 0000 0000 000C"
	$"C000 0000 0000 000C C000 0000 0000 000C"
	$"C000 000F FF00 000C C000 0000 0000 000C"
	$"C000 000F 0F00 000C C006 6633 3333 000C"
	$"C000 000F FF00 FF0C C006 6600 0000 000C"
	$"C000 000F 0F00 FF0C C006 6633 3333 000C"
	$"C0FF F00F FFFF FF0C C000 0000 0000 000C"
	$"C0F0 F00F 0FF0 FF0C C003 3333 3333 000C"
	$"C0FF FFFF FFFF FF0C C000 0000 0000 000C"
	$"C0F0 FF0F 0FF0 FF0C C003 3333 3333 000C"
	$"C0FF FFFF FFFF FF0C C000 0000 0000 000C"
	$"C0F0 FF0F 0FF0 FF0C C000 0000 0000 000C"
	$"C0FF FFFF FFFF FF0C C000 0000 0000 000C"
	$"C000 0000 0000 000C C000 0000 0000 000C"
	$"CCCC CCCC CCCC CCCC CCCC CCCC CCCC CCCC"
	$"CCCC CCCC CCCC CCCC CCCC CCCC CCCC CCCC"
	$"C777 7777 7777 777C C000 0000 0000 000C"
	$"C777 0077 7777 007C C000 000E 0000 000C"
	$"C770 0777 7770 077C C000 00EE 00E0 000C"
	$"C707 0777 7707 077C C000 0E0E 000E 000C"
	$"C077 7000 0077 700C C0EE E00E 0E00 E00C"
	$"C777 7777 7777 777C C0E0 000E 00E0 E00C"
	$"C777 7777 7777 777C C0E0 000E 00E0 E00C"
	$"C777 7777 7777 777C C0E0 000E 00E0 E00C"
	$"C777 7777 7777 777C C0E0 000E 00E0 E00C"
	$"C777 0077 7777 007C C0EE E00E 0E00 E00C"
	$"C770 0777 7770 077C C000 0E0E 000E 000C"
	$"C707 0777 7707 077C C000 00EE 00E0 000C"
	$"C077 7000 0077 700C C000 000E 0000 000C"
	$"C777 7777 7777 777C C000 0000 0000 000C"
	$"CCCC CCCC CCCC CCCC CCCC CCCC CCCC CCCC"
};

resource 'icl4' (130) {
	$"0000 FFFF FFFF FFFF FFFF FFFF FFFF F000"
	$"0000 F888 8888 88F8 8888 8888 8888 F000"
	$"0000 F888 8888 88F8 8888 8888 8888 F000"
	$"0000 F888 8888 88F8 8888 8888 8888 F000"
	$"0000 F888 8888 88FF 8888 8888 8888 F000"
	$"0000 F888 8888 FF88 FF88 8888 8888 F000"
	$"0000 F888 88FF 8888 88FF 8888 8888 F000"
	$"0000 F888 FF88 8888 8888 FF88 8888 F000"
	$"0000 F8FF 8888 8888 8888 88FF 88FF F000"
	$"0000 FF88 8888 FFF8 8888 8888 FF11 F000"
	$"0000 F888 8888 F8F8 8888 8888 F111 F000"
	$"0000 F888 8888 FFF8 8FFF 8888 F111 F000"
	$"0000 F888 8888 F8F8 8F8F 8888 F111 F000"
	$"0000 F88F FF88 FFFF FFFF 8888 F111 F000"
	$"0000 F88F 8F88 F8FF 8F8F 8888 F111 F000"
	$"0000 F88F FFFF FFFF FFFF FF88 F111 F000"
	$"0000 F88F 8FF8 F8FF 8F8F 8F88 F111 F000"
	$"0000 F88F FFFF FFFF FFFF FF88 F111 F000"
	$"0000 F88F 8FF8 F8FF 8F8F 8F88 F111 F000"
	$"0000 F88F FFFF FFFF FFFF FFF8 F111 F000"
	$"0000 F888 8888 8888 8888 8888 F111 F000"
	$"0000 FF88 8888 8888 8888 8888 FF11 F000"
	$"0000 F8FF 8888 8888 8888 88FF 77FF F000"
	$"0000 F888 FF88 8888 8888 FF77 7777 F000"
	$"0000 F888 88FF 8888 88FF 7777 7777 F000"
	$"0000 FFFF FFF8 FF88 FF77 7777 7777 F000"
	$"0000 0F00 00F8 88FF 7777 7777 7F77 F000"
	$"0000 00F0 00F8 88F7 777F 7777 FF7F F000"
	$"0000 000F 00F8 88F7 7777 FFF7 FFFF F000"
	$"0000 0000 F0F8 88F7 77FF FFFF FFFF F000"
	$"0000 0000 0FF8 88F7 777F FFFF FFFF F000"
	$"0000 0000 00FF FFFF FFFF FFFF FFFF F0"
};

data 'XCNQ' (0) {
	$"0A58 636F 6E71 2037 2E30 00"                        /* .Xconq 7.0. */
};

resource 'ics8' (128) {
	$"0000 0000 FFFF FF00 0000 0000 0000 FFFF"
	$"0000 FFFF E3E3 E3FF FF00 0000 FFFF 0505"
	$"FFFF E3E3 FFFF E3E3 E3FF FFFF 0505 0505"
	$"FFE3 E3E3 FFFF E3FF E3E3 FF05 0505 0505"
	$"FFE3 FFFF FFFF FFFF E3E3 FF05 0505 0505"
	$"FFE3 FFFF FFFF FFFF E3E3 FF05 0505 0505"
	$"FFE3 FFFF FFFF FFFF FFE3 FF05 0505 0505"
	$"FFE3 FFFF FFFF FFFF FFE3 FF05 0505 0505"
	$"FFE3 E3E3 E3E3 E3E3 E3E3 FF05 0505 0505"
	$"00FF FFE3 E3E3 E3E3 FFFF C0FF FF05 0505"
	$"0000 00FF FFE3 FFFF C0C0 C0C0 C0FF FF05"
	$"0000 0000 00FF C0C0 C0C0 C0C0 C0C0 C0FF"
	$"0000 0000 00FF C0C0 C0C0 FFC0 C0C0 C0FF"
	$"0000 0000 00FF C0C0 FFFF FFFF FFC0 C0FF"
	$"0000 0000 00FF C0FF FFFF FFFF FFFF C0FF"
	$"0000 0000 00FF C0C0 C0C0 C0C0 C0C0 C0FF"
};

resource 'ics8' (129) {
	$"0000 FFFF FFFF FFFF FFFF FFFF 0000 0000"
	$"0000 FFE3 E3E3 E3FF E3E3 E3FF FF00 0000"
	$"0000 FFE3 E3E3 FFFF FFE3 E3FF 00FF 0000"
	$"0000 FFE3 FFFF E3E3 E3FF FFFF FFFF FF00"
	$"0000 FFFF E3E3 FFFF E3E3 E3FF FF05 FF00"
	$"0000 FFE3 E3E3 FFFF E3FF E3E3 FF05 FF00"
	$"0000 FFE3 FFFF FFFF FFFF E3E3 FF05 FF00"
	$"0000 FFE3 FFFF FFFF FFFF E3E3 FF05 FF00"
	$"0000 FFE3 FFFF FFFF FFFF FFE3 FF05 FF00"
	$"0000 FFE3 FFFF FFFF FFFF FFE3 FF05 FF00"
	$"0000 FFE3 E3E3 E3E3 E3E3 E3E3 FF05 FF00"
	$"0000 FFFF FFE3 E3E3 E3E3 FFFF C0FF FF00"
	$"0000 FFE3 E3FF FFE3 FFFF C0C0 C0C0 FF00"
	$"0000 FFE3 E3E3 E3FF C0C0 C0C0 FFC0 FF00"
	$"0000 FFE3 E3E3 E3FF C0C0 FFFF FFFF FF00"
	$"0000 FFFF FFFF FFFF FFFF FFFF FFFF FF"
};

resource 'ics8' (130) {
	$"0000 FFFF FFFF FFFF FFFF FFFF FFFF FF00"
	$"0000 FFE3 E3E3 E3FF E3E3 E3E3 E3E3 FF00"
	$"0000 FFE3 E3E3 FFFF FFE3 E3E3 E3E3 FF00"
	$"0000 FFE3 FFFF E3E3 E3FF FFE3 E3FF FF00"
	$"0000 FFFF E3E3 FFFF E3E3 E3FF FF05 FF00"
	$"0000 FFE3 E3E3 FFFF E3FF E3E3 FF05 FF00"
	$"0000 FFE3 FFFF FFFF FFFF E3E3 FF05 FF00"
	$"0000 FFE3 FFFF FFFF FFFF E3E3 FF05 FF00"
	$"0000 FFE3 FFFF FFFF FFFF FFE3 FF05 FF00"
	$"0000 FFE3 FFFF FFFF FFFF FFE3 FF05 FF00"
	$"0000 FFE3 E3E3 E3E3 E3E3 E3E3 FF05 FF00"
	$"0000 FFFF FFE3 E3E3 E3E3 FFFF C0FF FF00"
	$"0000 FFFF FFFF FFE3 FFFF C0C0 C0C0 FF00"
	$"0000 00FF 00FF E3FF C0C0 C0C0 FFC0 FF00"
	$"0000 0000 FFFF E3FF C0C0 FFFF FFFF FF00"
	$"0000 0000 00FF FFFF FFFF FFFF FFFF FF"
};

resource 'ics8' (131) {
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
	$"2B00 0000 0000 002B 0000 0000 0000 002B"
	$"2B00 0000 FF00 002B 00EC ECD8 D8D8 002B"
	$"2B00 FF00 FFFF 002B 00EC EC00 0000 002B"
	$"2B00 FFFF FFFF 002B 00D8 D8D8 D8D8 002B"
	$"2B00 FF00 FFFF 002B 0000 0000 0000 002B"
	$"2B00 FFFF FFFF 002B 00D8 D8D8 D8D8 002B"
	$"2B00 0000 0000 002B 0000 0000 0000 002B"
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
	$"2BC0 C0C0 C0C0 C02B 0000 0000 0000 002B"
	$"2BC0 C0C0 00C0 C02B 0000 FFFF 0000 002B"
	$"2BC0 C000 C0C0 C02B 00FF 00FF 00FF 002B"
	$"2B00 00C0 0000 002B 00FF 00FF 00FF 002B"
	$"2BC0 C0C0 C0C0 C02B 0000 FFFF 0000 002B"
	$"2BC0 C0C0 C0C0 C02B 0000 0000 0000 002B"
	$"2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B 2B2B"
};

resource 'ics4' (128) {
	$"0000 FFF0 0000 00FF 00FF 888F F000 FF11"
	$"FF88 FF88 8FFF 1111 F888 FF8F 88F1 1111"
	$"F8FF FFFF 88F1 1111 F8FF FFFF 88F1 1111"
	$"F8FF FFFF F8F1 1111 F8FF FFFF F8F1 1111"
	$"F888 8888 88F1 1111 0FF8 8888 FF7F F111"
	$"000F F8FF 7777 7FF1 0000 0F77 7777 777F"
	$"0000 0F77 77F7 777F 0000 0F77 FFFF F77F"
	$"0000 0F7F FFFF FF7F 0000 0F77 7777 777F"
};

resource 'ics4' (129) {
	$"00FF FFFF FFFF 0000 00F8 888F 888F F000"
	$"00F8 88FF F88F 0F00 00F8 FF88 8FFF FFF0"
	$"00FF 88FF 888F F1F0 00F8 88FF 8F88 F1F0"
	$"00F8 FFFF FF88 F1F0 00F8 FFFF FF88 F1F0"
	$"00F8 FFFF FFF8 F1F0 00F8 FFFF FFF8 F1F0"
	$"00F8 8888 8888 F1F0 00FF F888 88FF 7FF0"
	$"00F8 8FF8 FF77 77F0 00F8 888F 7777 F7F0"
	$"00F8 888F 77FF FFF0 00FF FFFF FFFF FFF0"
};

resource 'ics4' (130) {
	$"00FF FFFF FFFF FFF0 00F8 888F 8888 88F0"
	$"00F8 88FF F888 88F0 00F8 FF88 8FF8 8FF0"
	$"00FF 88FF 888F F1F0 00F8 88FF 8F88 F1F0"
	$"00F8 FFFF FF88 F1F0 00F8 FFFF FF88 F1F0"
	$"00F8 FFFF FFF8 F1F0 00F8 FFFF FFF8 F1F0"
	$"00F8 8888 8888 F1F0 00FF F888 88FF 7FF0"
	$"00FF FFF8 FF77 77F0 000F 0F8F 7777 F7F0"
	$"0000 FF8F 77FF FFF0 0000 0FFF FFFF FFF0"
};

resource 'ics4' (131) {
	$"CCCC CCCC CCCC CCCC C000 000C 0000 000C"
	$"C000 F00C 0663 330C C0F0 FF0C 0660 000C"
	$"C0FF FF0C 0333 330C C0F0 FF0C 0000 000C"
	$"C0FF FF0C 0333 330C C000 000C 0000 000C"
	$"CCCC CCCC CCCC CCCC C777 777C 0000 000C"
	$"C777 077C 00FF 000C C770 777C 0F0F 0F0C"
	$"C007 000C 0F0F 0F0C C777 777C 00FF 000C"
	$"C777 777C 0000 000C CCCC CCCC CCCC CCCC"
};

resource 'ics#' (128) {
	{	/* array: 2 elements */
		/* [1] */
		$"0E03 318C CC70 8D20 BF20 BF20 BFA0 BFA0"
		$"8020 60D8 1B06 0401 0421 04F9 05FD 0401",
		/* [2] */
		$"0E03 3F8F FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF 7FFF 1FFF 07FF 07FF 07FF 07FF 07FF"
	}
};

resource 'ics#' (129) {
	{	/* array: 2 elements */
		/* [1] */
		$"3FF0 2118 2394 2C7E 331A 234A 2FCA 2FCA"
		$"2FEA 2FEA 200A 3836 26C2 210A 213E 3FFE",
		/* [2] */
		$"3FF0 3FF8 3FFC 3FFE 3FFE 3FFE 3FFE 3FFE"
		$"3FFE 3FFE 3FFE 3FFE 3FFE 3FFE 3FFE 3FFE"
	}
};

resource 'ics#' (130) {
	{	/* array: 2 elements */
		/* [1] */
		$"3FFE 2102 2382 2C66 331A 234A 2FCA 2FCA"
		$"2FEA 2FEA 200A 3836 3EC2 150A 0D3E 07FE",
		/* [2] */
		$"3FFE 3FFE 3FFE 3FFE 3FFE 3FFE 3FFE 3FFE"
		$"3FFE 3FFE 3FFE 3FFE 3FFE 1FFE 0FFE 07FE"
	}
};

resource 'ics#' (131) {
	{	/* array: 2 elements */
		/* [1] */
		$"AAAA 0101 887C 2D61 BC7C 2D01 BC7C 0101"
		$"AAAA 7F01 F630 6F55 9054 7F31 FE00 5555",
		/* [2] */
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
		$"FFFF FFFF FFFF FFFF FFFF FFFF FFFF FFFF"
	}
};

resource 'PICT' (129, "Map Controls BL") {
	123,
	{283, 273, 299, 304},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9000 0401 1B01 1001 2B01 3001 1B01 1101"
	$"2B01 3001 1B01 1101 2B01 3000 007F FFFF"
	$"FF00 0080 0100 0080 0100 0080 1100 0080"
	$"3900 0080 7D00 0080 FF00 0081 FF00 0083"
	$"FF00 4087 FF00 E0CF FF01 F0FF FF03 F8FF"
	$"FF17 FCFF FF3F FEFF FF00 0000 00A0 0083"
	$"FF"
};

resource 'PICT' (128, "Empty Pict") {
	0,
	{0, 0, 0, 0},
	$""
};

resource 'PICT' (2002, "New Game Decor") {
	1062,
	{114, 231, 155, 442},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9800 1C00 7200 E000 9B01 C000 7200 E700"
	$"9B01 BA00 7200 E700 9B01 BA00 0002 E500"
	$"10F3 000A 0C00 00C0 000C 0000 C000 0CFE"
	$"0011 F300 0D33 0003 3000 3300 0330 0033"
	$"0003 0016 0300 00FE 7FF7 000D C0C0 0C0C"
	$"00C0 C00C 0C00 C0C0 0C00 1703 0000 381C"
	$"F800 0E03 0030 3003 0300 3030 0303 0030"
	$"3000 1703 0000 3830 F800 0E0C 000C C000"
	$"CC00 0CC0 00CC 000C C000 1703 0000 3860"
	$"F800 0C30 1C03 0000 3000 0300 0030 0003"
	$"FF00 1703 0000 18C0 F800 0C20 1C02 0000"
	$"2000 0200 0020 0002 FF00 1D19 0000 1980"
	$"1F40 7C0B 700F 6000 0020 14C2 0000 2000"
	$"0200 0020 0002 FF00 1D19 0000 1F00 7BC1"
	$"DE0F F83B C000 0020 1CC2 0000 2000 0200"
	$"0020 0002 FF00 1D19 0000 1E00 F1C3 8E0E"
	$"3871 C000 0023 97C2 0000 2000 0200 0020"
	$"0002 FF00 1D19 0000 3C01 C006 0E1C 70C3"
	$"8000 0022 9EC2 0000 2000 0201 E020 0002"
	$"FF00 1D19 0000 7C01 C00E 0E1C 71C3 8000"
	$"0023 FFC2 0000 2000 027F F820 0002 FF00"
	$"1D09 0000 CC03 801C 1C38 E387 FE00 0C22"
	$"D6DA 0000 2000 0203 FF20 0002 FF00 1D09"
	$"0001 8E03 801C 1C38 E387 FE00 0C23 FFDA"
	$"0000 2000 023F FE20 0002 FF00 1D09 0003"
	$"0E07 0638 3871 C70E FE00 0C22 D6FA 0000"
	$"2000 021F FC20 0002 FF00 1D09 0006 0E07"
	$"8E3C 7071 C79E FE00 0C27 FFFA 0000 2000"
	$"0200 0020 0002 FF00 1D09 001C 0E03 F81D"
	$"E0E3 83FC FE00 0C20 0002 0000 2000 0200"
	$"0020 0002 FF00 1D09 007F 3F81 E00F 83F7"
	$"E1FC FE00 0C30 0003 0000 3000 0300 0030"
	$"0003 FF00 16F8 0000 38FE 000E 0C00 0CC0"
	$"00CC 000C C000 CC00 0CC0 0016 F800 0038"
	$"FE00 0E03 0030 3003 0300 3030 0303 0030"
	$"3000 15F8 0000 70FD 000D C0C0 0C0C 00C0"
	$"C00C 0C00 C0C0 0C00 15F8 0000 70FD 000D"
	$"3300 0330 0033 0003 3000 3300 0300 15F8"
	$"0000 E0FD 000D 0C00 00C0 000C 0000 C000"
	$"0C00 0080 16F9 0001 03F8 FD00 0D08 0000"
	$"8000 0800 0080 0008 0000 8011 F300 0D08"
	$"0000 8000 0800 0080 0008 0000 8011 F300"
	$"0D08 0000 8000 0800 0080 0008 0400 8011"
	$"F300 0D08 0000 80F0 0800 0080 0009 0D08"
	$"801C FE00 0308 0000 79FE 0011 F200 0400"
	$"0800 0083 FFC8 0000 8000 08EF B080 1CFE"
	$"0018 1800 0089 0000 0116 000C 0008 0000"
	$"9FF8 0800 0080 000B FFFC 801C FE00 180B"
	$"1DC0 83CE 1601 02C1 8587 0800 008F FF88"
	$"0000 8000 09FF FC80 1CFE 0018 0C88 8041"
	$"1139 0083 2246 4888 0000 87FF 0800 0080"
	$"0008 0000 801C FE00 1808 4880 2103 1100"
	$"4224 2428 0800 0080 0008 0000 8000 0800"
	$"0080 1CFE 0018 0845 0011 0D11 0022 27E4"
	$"2708 0000 8000 0800 0080 0008 0000 801C"
	$"FE00 1808 4500 0911 1100 1224 0420 8C00"
	$"00C0 000C 0000 C000 0C00 0080 1CFE 0018"
	$"0C82 0089 3111 0112 2246 4883 0003 3000"
	$"3300 0330 0033 0003 001C FE00 180B 0200"
	$"F0CE BB81 E771 8587 00C0 0C0C 00C0 C00C"
	$"0C00 C0C0 0C00 14FD 0000 04F7 000C 3030"
	$"0303 0030 3003 0300 3030 0014 FD00 000C"
	$"F700 0C0C C000 CC00 0CC0 00CC 000C C000"
	$"14FD 0000 08F7 000A 0300 0030 0003 0000"
	$"3000 03FF 0002 E500 A000 83FF"
};

resource 'PICT' (2000, "Splash B/W") {
	5971,
	{188, 130, 388, 530},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9800 3400 BC00 8001 8402 1800 BC00 8201"
	$"8402 1200 BC00 8201 8402 1200 0002 CD00"
	$"02CD 0002 CD00 0AEF 0004 03FF E1FF 80E4"
	$"0008 EE00 02FF 003E E300 08EE 0002 7E00"
	$"38E3 0017 EE00 023E 0030 F200 0340 0000"
	$"7AFE 0004 03D2 0000 80FE 0017 EE00 023F"
	$"0060 F200 0BC0 0000 8640 0000 0436 0001"
	$"80FE 0017 EE00 021F 00C0 F200 0B40 0001"
	$"0240 0000 0812 0000 80FE 0018 EE00 021F"
	$"00C0 F200 0C5C 71C1 00E0 E170 0802 E070"
	$"B878 FF00 18EE 0002 1F81 80F2 000C 6220"
	$"80C0 4113 8806 0310 88C4 84FF 0017 EE00"
	$"010F 83F1 000C 4111 0030 4231 0401 8209"
	$"0482 80FF 0017 EE00 010F 86F1 000C 4111"
	$"000C 40D1 0400 6209 FC82 60FF 0024 EE00"
	$"0E07 CC00 003F 8000 FC00 0E07 8000 F3E0"
	$"FE00 0C41 0A01 0241 1104 0812 0900 8218"
	$"FF00 24EE 000E 07D8 0000 E1C0 078F 01FE"
	$"1F80 038B C0FE 000C 410A 0102 4211 0408"
	$"1209 0482 04FF 0024 EE00 0E07 F800 0181"
	$"E00E 0780 3E27 C00E 0BC0 FE00 0C62 0401"
	$"844A 3104 0C22 0888 C484 FF00 24EE 000E"
	$"03F0 0007 01E0 1C03 803C 47C0 1C07 C0FE"
	$"000C 5C04 0178 31CB 8E0B C71C 70B8 78FF"
	$"0018 EE00 0E03 E000 0E01 E038 03C0 3C87"
	$"C038 07C0 FD00 0008 F400 18EE 000E 01F0"
	$"001E 00C0 7803 C03D 0780 7807 80FD 0000"
	$"18F4 0018 EE00 0E01 F000 1C00 00F0 03C0"
	$"7907 8070 0780 FD00 0010 F400 14EE 000E"
	$"03F8 003C 0001 E003 C07A 0780 F007 80EF"
	$"0014 EE00 0E07 F800 7800 01E0 03C0 7A0F"
	$"01E0 0F80 EF00 13EE 000D 06F8 0078 0003"
	$"C003 C07C 0F01 E00F EE00 13EE 000D 0C7C"
	$"0078 0003 C007 80FC 0F03 C00F EE00 13EE"
	$"000D 187C 00F0 0007 C007 80F8 0F03 C01F"
	$"EE00 13EE 000D 307E 00F0 0007 C007 80F8"
	$"1E03 C01E EE00 13EE 000D 603E 00F0 0007"
	$"800F 01F0 1E07 803E EE00 13EE 000D E03E"
	$"00F0 0007 800F 01F0 1E07 803E EE00 13EE"
	$"000D C03F 00F0 0007 801E 01E0 3E07 805E"
	$"EE00 14EF 000E 0180 1F00 F001 0780 1C01"
	$"E03C 2780 FCEE 0014 EF00 0E03 001F 80F0"
	$"0107 8038 03C0 3C47 80BC EE00 14EF 000E"
	$"0700 0F80 7802 0380 7003 C03C 8781 3CEE"
	$"0014 EF00 0E0E 000F C038 0C03 C0E0 03C0"
	$"7D03 C63C EE00 14EF 000E 3F00 1FE0 1C10"
	$"01E1 8007 C03E 03FC 78EE 0014 EF00 0E7F"
	$"E0FF FC0F E000 7E00 0780 3C00 F078 EE00"
	$"06E1 0000 78EE 0006 E100 00F8 EE00 06E1"
	$"0000 F0EE 0006 E100 00F0 EE00 06E1 0000"
	$"F0EE 0007 E200 0101 E0EE 0007 E200 0101"
	$"E0EE 0007 E200 0101 E0EE 0007 E200 0107"
	$"E0EE 0007 E200 011F FCEE 0002 CD00 02CD"
	$"0002 CD00 02CD 0002 CD00 02CD 0021 F500"
	$"1B60 0006 0000 6000 0600 0060 0006 0000"
	$"6000 0600 0060 0006 0000 6000 06F5 0023"
	$"F600 1D01 9800 1980 0198 0019 8001 9800"
	$"1980 0198 0019 8001 9800 1980 0198 0019"
	$"80F6 0023 F600 1D06 0600 6060 0606 0060"
	$"6006 0600 6060 0606 0060 6006 0600 6060"
	$"0606 0060 60F6 0023 F600 1D18 0181 8018"
	$"1801 8180 1818 0181 8018 1801 8180 1818"
	$"0181 8018 1801 8180 18F6 0023 F600 1D60"
	$"0066 0006 6000 6600 0660 0066 0006 6000"
	$"6600 0660 0066 0006 6000 6600 06F6 0024"
	$"F700 1E01 8000 1800 0180 0018 0001 8000"
	$"1800 0180 0018 0001 8000 1800 0180 0018"
	$"0001 F600 24F7 001E 0100 0010 0001 0000"
	$"1000 0100 0010 0001 0000 1000 0100 0010"
	$"0001 0000 1000 01F6 0024 F700 1E01 0000"
	$"1000 0100 0010 0001 0000 1000 0100 0010"
	$"0001 0000 1000 0100 0010 0001 F600 24F7"
	$"001E 0100 0010 0001 0000 1000 0100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"01F6 0024 F700 1E01 0000 1000 0100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 F600 24F7 001E 0100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 1000 01F6 0024 F700"
	$"1E01 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"F600 24F7 001E 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 01F6 0024 F700 1E01 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 F600 24F7 001E"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 1000 01F6"
	$"0024 F700 1E01 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 1000 0100"
	$"0010 0001 F600 24F7 001E 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 1000 0100"
	$"0010 0001 0000 1000 01F6 0025 F700 1F01"
	$"8000 1800 0180 0018 0001 8000 1800 0180"
	$"0018 0001 8000 1800 0180 0018 0001 80F7"
	$"0025 F700 1F06 6000 6600 0660 0066 0006"
	$"6000 6600 0660 0066 0006 6000 6600 0660"
	$"0066 0006 60F7 0025 F700 1F18 1801 8180"
	$"1818 0181 8018 1801 8180 1818 0181 8018"
	$"1801 8180 1818 0181 8018 18F7 0025 F700"
	$"1F60 0606 0060 6006 0600 6060 0606 0060"
	$"6006 0600 6060 0606 0060 6006 0600 6060"
	$"06F7 0027 F800 2101 8001 9800 1980 0198"
	$"0019 8001 9800 1980 0198 0019 8001 9800"
	$"1980 0198 0019 8001 80F8 0027 F800 2106"
	$"0000 6000 0603 8060 0006 0000 6000 0600"
	$"0060 0006 0000 6000 0603 8060 0006 0000"
	$"40F8 0027 F800 2104 0000 4000 0403 8040"
	$"0004 0000 4000 0400 0040 0004 0000 4000"
	$"0403 8040 0004 0000 40F8 0027 F800 2104"
	$"0000 4000 0402 9840 0004 0000 4000 0400"
	$"0040 0004 0000 4000 0402 9840 0004 0000"
	$"40F8 0027 F800 2104 0000 4000 0403 9840"
	$"0004 0000 4000 0400 0040 0004 0000 4000"
	$"0403 9840 0004 0000 40F8 0027 F800 2104"
	$"0000 4000 0472 F840 0004 0000 4000 0400"
	$"0040 0004 0000 4000 0472 F840 0004 0000"
	$"40F8 0027 F800 2104 0000 4000 0453 D840"
	$"0004 0000 4000 0403 C040 0004 0000 4000"
	$"0453 D840 0004 0000 40F8 0027 F800 2104"
	$"0000 4000 047F F840 0004 0000 4000 04FF"
	$"F040 0004 0000 4000 047F F840 0004 0000"
	$"40F8 0027 F800 2104 0000 4000 045A DB40"
	$"0004 0000 4000 0407 FE40 0004 0000 4000"
	$"045A DB40 0004 0000 40F8 0027 F800 2104"
	$"0000 4000 047F FB40 0004 0000 4000 047F"
	$"FC40 0004 0000 4000 047F FB40 0004 0000"
	$"40F8 0027 F800 2104 0000 4000 045A DF40"
	$"0004 0000 4000 043F F840 0004 0000 4000"
	$"045A DF40 0004 0000 40F8 0027 F800 2104"
	$"0000 4000 04FF FF40 0004 0000 4000 0400"
	$"0040 0004 0000 4000 04FF FF40 0004 0000"
	$"40F8 0027 F800 2104 0000 4000 0400 0040"
	$"0004 0000 4000 0400 0040 0004 0000 4000"
	$"0400 0040 0004 0000 40F8 0027 F800 2106"
	$"0000 6000 0600 0060 0006 0000 6000 0600"
	$"0060 0006 0000 6000 0600 0060 0006 0000"
	$"60F8 0027 F800 2119 8001 9800 1980 0198"
	$"0019 8001 9800 1980 0198 0019 8001 9800"
	$"1980 0198 0019 8001 98F8 0027 F800 2160"
	$"6006 0600 6060 0606 0060 6006 0600 6060"
	$"0606 0060 6006 0600 6060 0606 0060 6006"
	$"06F8 0029 F900 2301 8018 1801 8180 1818"
	$"0181 8018 1801 8180 1818 0181 8018 1801"
	$"8180 1818 0181 8018 1801 80F9 0029 F900"
	$"2306 0006 6000 6600 0660 0066 0006 6000"
	$"6600 0660 0066 0006 6000 6600 0660 0066"
	$"0006 6000 60F9 0029 F900 2318 0001 8000"
	$"1800 0180 0018 0001 8000 1800 0180 0018"
	$"0001 8000 1800 0180 0018 0001 8000 10F9"
	$"0029 F900 2310 0001 0000 1000 0100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 10F9 0029 F900 2310"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 10F9 0029 F900 2310 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 10F9 0029"
	$"F900 2310 0001 0000 1000 0100 0010 0001"
	$"0000 101E 0100 0010 0001 0000 1000 0100"
	$"0010 0001 0000 10F9 0029 F900 2310 0001"
	$"0000 1000 0100 0010 0001 0000 107F F900"
	$"0010 0001 0000 1000 0100 0010 0001 0000"
	$"10F9 0029 F900 2310 0001 0000 1000 0100"
	$"0010 0001 0000 13FF 0100 0010 0001 0000"
	$"1000 0100 0010 0001 0000 10F9 0029 F900"
	$"2310 0001 0000 1000 0100 0010 0001 0000"
	$"11FF F100 0010 0001 0000 1000 0100 0010"
	$"0001 0000 10F9 0029 F900 2310 0001 0000"
	$"1000 0100 0010 0001 0000 10FF E100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 10F9"
	$"0029 F900 2310 0001 0000 1000 0100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 10F9 0029 F900 2310"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 10F9 0029 F900 2310 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 10F9 0029"
	$"F900 2318 0001 8000 1800 0180 0018 0001"
	$"8000 1800 0180 0018 0001 8000 1800 0180"
	$"0018 0001 8000 18F9 0029 F900 2366 0006"
	$"6000 6600 0660 0066 0006 6000 6600 0660"
	$"0066 0006 6000 6600 0660 0066 0006 6000"
	$"66F9 002B FA00 2501 8180 1818 0181 8018"
	$"1801 8180 1818 0181 8018 1801 8180 1818"
	$"0181 8018 1801 8180 1818 0181 80FA 002B"
	$"FA00 2506 0060 6006 0600 6060 0606 0060"
	$"6006 0600 6060 0606 0060 6006 0600 6060"
	$"0606 0060 6006 0600 60FA 002B FA00 2518"
	$"0019 8001 9800 1980 0198 0019 8001 9800"
	$"1980 0198 0019 8001 9800 1980 0198 0019"
	$"8001 9800 18FA 002B FA00 2560 0006 0000"
	$"6000 0603 8060 0006 0000 6000 0600 0060"
	$"0006 0000 6000 0603 8060 0006 0000 6000"
	$"04FA 002B FA00 2540 0004 0000 4000 0403"
	$"8040 0004 0000 4000 0400 0040 0004 0000"
	$"4000 0403 8040 0004 0000 4000 04FA 002B"
	$"FA00 2540 0004 0000 4000 0402 9840 0004"
	$"0000 4000 0400 0040 0004 0000 4000 0402"
	$"9840 0004 0000 4000 04FA 002B FA00 2540"
	$"0004 0000 4000 0403 9840 0004 0000 4000"
	$"0400 0040 0004 0000 4000 0403 9840 0004"
	$"0000 4000 04FA 002B FA00 2540 0004 0000"
	$"4000 0472 F840 0004 0000 4000 0400 0040"
	$"0004 0000 4000 0472 F840 0004 0000 4000"
	$"04FA 002B FA00 2540 0004 0000 4000 0453"
	$"D840 0004 0000 4000 0403 C040 0004 0000"
	$"4000 0453 D840 0004 0000 4000 04FA 002B"
	$"FA00 2540 0004 0000 4000 047F F840 0004"
	$"0000 4000 04FF F040 0004 0000 4000 047F"
	$"F840 0004 0000 4000 04FA 002B FA00 2540"
	$"0004 0000 4000 045A DB40 0004 0000 4000"
	$"0407 FE40 0004 0000 4000 045A DB40 0004"
	$"0000 4000 04FA 002B FA00 2540 0004 0000"
	$"4000 047F FB40 0004 0000 4000 047F FC40"
	$"0004 0000 4000 047F FB40 0004 0000 4000"
	$"04FA 002B FA00 2540 0004 0000 4000 045A"
	$"DF40 0004 0000 4000 043F F840 0004 0000"
	$"4000 045A DF40 0004 0000 4000 04FA 002B"
	$"FA00 2540 0004 0000 4000 04FF FF40 0004"
	$"0000 4000 0400 0040 0004 0000 4000 04FF"
	$"FF40 0004 0000 4000 04FA 002B FA00 2540"
	$"0004 0000 4000 0400 0040 0004 0000 4000"
	$"0400 0040 0004 0000 4000 0400 0040 0004"
	$"0000 4000 04FA 002B FA00 2560 0006 0000"
	$"6000 0600 0060 0006 0000 6000 0600 0060"
	$"0006 0000 6000 0600 0060 0006 0000 6000"
	$"04FA 002B FA00 2518 0019 8001 9800 1980"
	$"0198 0019 8001 9800 1980 0198 0019 8001"
	$"9800 1980 0198 0019 8001 9800 18FA 002B"
	$"FA00 2506 0060 6006 0600 6060 0606 0060"
	$"6006 0600 6060 0606 0060 6006 0600 6060"
	$"0606 0060 6006 0600 60FA 002B FA00 2501"
	$"8180 1818 0181 8018 1801 8180 1818 0181"
	$"8018 1801 8180 1818 0181 8018 1801 8180"
	$"1818 0181 80FA 0029 F900 2366 0006 6000"
	$"6600 0660 0066 0006 6000 6600 0660 0066"
	$"0006 6000 6600 0660 0066 0006 6000 66F9"
	$"0029 F900 2318 0001 8000 1800 0180 0018"
	$"0001 8000 1800 0180 0018 0001 8000 1800"
	$"0180 0018 0001 8000 18F9 0029 F900 2310"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 10F9 0029 F900 2310 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 10F9 0029"
	$"F900 2310 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 1000 0100"
	$"0010 0001 0000 10F9 0029 F900 2310 0001"
	$"0000 1000 0100 0010 0001 0000 1000 0100"
	$"0010 0001 0000 1000 0100 0010 0001 0000"
	$"10F9 0029 F900 2310 0001 0000 1000 0100"
	$"0010 0001 0000 101E 0100 0010 0001 0000"
	$"1000 0100 0010 0001 0000 10F9 0029 F900"
	$"2310 0001 0000 1000 0100 0010 0001 0000"
	$"107F F900 0010 0001 0000 1000 0100 0010"
	$"0001 0000 10F9 0029 F900 2310 0001 0000"
	$"1000 0100 0010 0001 0000 13FF 0100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 10F9"
	$"0029 F900 2310 0001 0000 1000 0100 0010"
	$"0001 0000 11FF F100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 10F9 0029 F900 2310"
	$"0001 0000 1000 0100 0010 0001 0000 10FF"
	$"E100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 10F9 0029 F900 2310 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 10F9 0029"
	$"F900 2310 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 1000 0100"
	$"0010 0001 0000 10F9 0029 F900 2318 0001"
	$"8000 1800 0180 0018 0001 8000 1800 0180"
	$"0018 0001 8000 1800 0180 0018 0001 8000"
	$"10F9 0029 F900 2306 0006 6000 6600 0660"
	$"0066 0006 6000 6600 0660 0066 0006 6000"
	$"6600 0660 0066 0006 6000 60F9 0029 F900"
	$"2301 8018 1801 8180 1818 0181 8018 1801"
	$"8180 1818 0181 8018 1801 8180 1818 0181"
	$"8018 1801 80F9 0027 F800 2160 6006 0600"
	$"6060 0606 0060 6006 0600 6060 0606 0060"
	$"6006 0600 6060 0606 0060 6006 06F8 0027"
	$"F800 2119 8001 9800 1980 0198 0019 8001"
	$"9800 1980 0198 0019 8001 9800 1980 0198"
	$"0019 8001 98F8 0027 F800 2106 0000 6000"
	$"0603 8060 0006 0000 6000 0600 0060 0006"
	$"0000 6000 0603 8060 0006 0000 60F8 0027"
	$"F800 2104 0000 4000 0403 8040 0004 0000"
	$"4000 0400 0040 0004 0000 4000 0403 8040"
	$"0004 0000 40F8 0027 F800 2104 0000 4000"
	$"0402 9840 0004 0000 4000 0400 0040 0004"
	$"0000 4000 0402 9840 0004 0000 40F8 0027"
	$"F800 2104 0000 4000 0403 9840 0004 0000"
	$"4000 0400 0040 0004 0000 4000 0403 9840"
	$"0004 0000 40F8 0027 F800 2104 0000 4000"
	$"0472 F840 0004 0000 4000 0400 0040 0004"
	$"0000 4000 0472 F840 0004 0000 40F8 0027"
	$"F800 2104 0000 4000 0453 D840 0004 0000"
	$"4000 0403 C040 0004 0000 4000 0453 D840"
	$"0004 0000 40F8 0027 F800 2104 0000 4000"
	$"047F F840 0004 0000 4000 04FF F040 0004"
	$"0000 4000 047F F840 0004 0000 40F8 0027"
	$"F800 2104 0000 4000 045A DB40 0004 0000"
	$"4000 0407 FE40 0004 0000 4000 045A DB40"
	$"0004 0000 40F8 0027 F800 2104 0000 4000"
	$"047F FB40 0004 0000 4000 047F FC40 0004"
	$"0000 4000 047F FB40 0004 0000 40F8 0027"
	$"F800 2104 0000 4000 045A DF40 0004 0000"
	$"4000 043F F840 0004 0000 4000 045A DF40"
	$"0004 0000 40F8 0027 F800 2104 0000 4000"
	$"04FF FF40 0004 0000 4000 0400 0040 0004"
	$"0000 4000 04FF FF40 0004 0000 40F8 0027"
	$"F800 2104 0000 4000 0400 0040 0004 0000"
	$"4000 0400 0040 0004 0000 4000 0400 0040"
	$"0004 0000 40F8 0027 F800 2106 0000 6000"
	$"0600 0060 0006 0000 6000 0600 0060 0006"
	$"0000 6000 0600 0060 0006 0000 40F8 0027"
	$"F800 2101 8001 9800 1980 0198 0019 8001"
	$"9800 1980 0198 0019 8001 9800 1980 0198"
	$"0019 8001 80F8 0025 F700 1F60 0606 0060"
	$"6006 0600 6060 0606 0060 6006 0600 6060"
	$"0606 0060 6006 0600 6060 06F7 0025 F700"
	$"1F18 1801 8180 1818 0181 8018 1801 8180"
	$"1818 0181 8018 1801 8180 1818 0181 8018"
	$"18F7 0025 F700 1F06 6000 6600 0660 0066"
	$"0006 6000 6600 0660 0066 0006 6000 6600"
	$"0660 0066 0006 60F7 0025 F700 1F01 8000"
	$"1800 0180 0018 0001 8000 1800 0180 0018"
	$"0001 8000 1800 0180 0018 0001 80F7 0024"
	$"F700 1E01 0000 1000 0100 0010 0001 0000"
	$"1000 0100 0010 0001 0000 1000 0100 0010"
	$"0001 F600 24F7 001E 0100 0010 0001 0000"
	$"1000 0100 0010 0001 0000 1000 0100 0010"
	$"0001 0000 1000 01F6 0024 F700 1E01 0000"
	$"1000 0100 0010 0001 0000 1000 0100 0010"
	$"0001 0000 1000 0100 0010 0001 F600 24F7"
	$"001E 0100 0010 0001 0000 1000 0100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"01F6 0024 F700 1E01 0000 1000 0100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 F600 24F7 001E 0100 0010"
	$"0001 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 1000 01F6 0024 F700"
	$"1E01 0000 1000 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"F600 24F7 001E 0100 0010 0001 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 01F6 0024 F700 1E01 0000 1000"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 F600 24F7 001E"
	$"0100 0010 0001 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 1000 01F6"
	$"0024 F700 1E01 0000 1000 0100 0010 0001"
	$"0000 1000 0100 0010 0001 0000 1000 0100"
	$"0010 0001 F600 24F7 001E 0180 0018 0001"
	$"8000 1800 0180 0018 0001 8000 1800 0180"
	$"0018 0001 8000 1800 01F6 0023 F600 1D60"
	$"0066 0006 6000 6600 0660 0066 0006 6000"
	$"6600 0660 0066 0006 6000 6600 06F6 0023"
	$"F600 1D18 0181 8018 1801 8180 1818 0181"
	$"8018 1801 8180 1818 0181 8018 1801 8180"
	$"18F6 0023 F600 1D06 0600 6060 0606 0060"
	$"6006 0600 6060 0606 0060 6006 0600 6060"
	$"0606 0060 60F6 0023 F600 1D01 9800 1980"
	$"0198 0019 8001 9800 1980 0198 0019 8001"
	$"9800 1980 0198 0019 80F6 0021 F500 1B60"
	$"0006 0000 6000 0600 0060 0006 0000 6000"
	$"0600 0060 0006 0000 6000 06F5 0002 CD00"
	$"02CD 0002 CD00 02CD 0002 CD00 02CD 0002"
	$"CD00 02CD 0002 CD00 02CD 0002 CD00 02CD"
	$"0002 CD00 02CD 0002 CD00 02CD 0002 CD00"
	$"02CD 0002 CD00 02CD 0002 CD00 02CD 0002"
	$"CD00 02CD 00A0 0083 FF"
};

resource 'PICT' (2001, "Splash Color") {
	15300,
	{0, 0, 200, 400},
	$"0011 02FF 0C00 FFFF FFFF 0000 0000 0000"
	$"0000 0190 0000 00C8 0000 0000 0000 001F"
	$"7FFF 7FFF 7FFF 001E 0001 000A 0000 0000"
	$"0280 0280 0098 8198 0000 0000 00C8 0197"
	$"0000 0000 0000 0000 0048 0000 0048 0000"
	$"0000 0008 0001 0008 0000 0000 001E 4388"
	$"0000 0000 0000 0008 0000 00FF 0000 FFFF"
	$"FFFF FFFF 0001 FFFF FFFF CCCC 0002 FFFF"
	$"FFFF 9999 0003 FFFF FFFF 6666 0004 FFFF"
	$"FFFF 3333 0005 FFFF FFFF 0000 0006 FFFF"
	$"CCCC FFFF 0007 FFFF CCCC CCCC 0008 FFFF"
	$"CCCC 9999 0009 FFFF CCCC 6666 000A FFFF"
	$"CCCC 3333 000B FFFF CCCC 0000 000C FFFF"
	$"9999 FFFF 000D FFFF 9999 CCCC 000E FFFF"
	$"9999 9999 000F FFFF 9999 6666 0010 FFFF"
	$"9999 3333 0011 FFFF 9999 0000 0012 FFFF"
	$"6666 FFFF 0013 FFFF 6666 CCCC 0014 FFFF"
	$"6666 9999 0015 FFFF 6666 6666 0016 FFFF"
	$"6666 3333 0017 FFFF 6666 0000 0018 FFFF"
	$"3333 FFFF 0019 FFFF 3333 CCCC 001A FFFF"
	$"3333 9999 001B FFFF 3333 6666 001C FFFF"
	$"3333 3333 001D FFFF 3333 0000 001E FFFF"
	$"0000 FFFF 001F FFFF 0000 CCCC 0020 FFFF"
	$"0000 9999 0021 FFFF 0000 6666 0022 FFFF"
	$"0000 3333 0023 FFFF 0000 0000 0024 CCCC"
	$"FFFF FFFF 0025 CCCC FFFF CCCC 0026 CCCC"
	$"FFFF 9999 0027 CCCC FFFF 6666 0028 CCCC"
	$"FFFF 3333 0029 CCCC FFFF 0000 002A CCCC"
	$"CCCC FFFF 002B CCCC CCCC CCCC 002C CCCC"
	$"CCCC 9999 002D CCCC CCCC 6666 002E CCCC"
	$"CCCC 3333 002F CCCC CCCC 0000 0030 CCCC"
	$"9999 FFFF 0031 CCCC 9999 CCCC 0032 CCCC"
	$"9999 9999 0033 CCCC 9999 6666 0034 CCCC"
	$"9999 3333 0035 CCCC 9999 0000 0036 CCCC"
	$"6666 FFFF 0037 CCCC 6666 CCCC 0038 CCCC"
	$"6666 9999 0039 CCCC 6666 6666 003A CCCC"
	$"6666 3333 003B CCCC 6666 0000 003C CCCC"
	$"3333 FFFF 003D CCCC 3333 CCCC 003E CCCC"
	$"3333 9999 003F CCCC 3333 6666 0040 CCCC"
	$"3333 3333 0041 CCCC 3333 0000 0042 CCCC"
	$"0000 FFFF 0043 CCCC 0000 CCCC 0044 CCCC"
	$"0000 9999 0045 CCCC 0000 6666 0046 CCCC"
	$"0000 3333 0047 CCCC 0000 0000 0048 9999"
	$"FFFF FFFF 0049 9999 FFFF CCCC 004A 9999"
	$"FFFF 9999 004B 9999 FFFF 6666 004C 9999"
	$"FFFF 3333 004D 9999 FFFF 0000 004E 9999"
	$"CCCC FFFF 004F 9999 CCCC CCCC 0050 9999"
	$"CCCC 9999 0051 9999 CCCC 6666 0052 9999"
	$"CCCC 3333 0053 9999 CCCC 0000 0054 9999"
	$"9999 FFFF 0055 9999 9999 CCCC 0056 9999"
	$"9999 9999 0057 9999 9999 6666 0058 9999"
	$"9999 3333 0059 9999 9999 0000 005A 9999"
	$"6666 FFFF 005B 9999 6666 CCCC 005C 9999"
	$"6666 9999 005D 9999 6666 6666 005E 9999"
	$"6666 3333 005F 9999 6666 0000 0060 9999"
	$"3333 FFFF 0061 9999 3333 CCCC 0062 9999"
	$"3333 9999 0063 9999 3333 6666 0064 9999"
	$"3333 3333 0065 9999 3333 0000 0066 9999"
	$"0000 FFFF 0067 9999 0000 CCCC 0068 9999"
	$"0000 9999 0069 9999 0000 6666 006A 9999"
	$"0000 3333 006B 9999 0000 0000 006C 6666"
	$"FFFF FFFF 006D 6666 FFFF CCCC 006E 6666"
	$"FFFF 9999 006F 6666 FFFF 6666 0070 6666"
	$"FFFF 3333 0071 6666 FFFF 0000 0072 6666"
	$"CCCC FFFF 0073 6666 CCCC CCCC 0074 6666"
	$"CCCC 9999 0075 6666 CCCC 6666 0076 6666"
	$"CCCC 3333 0077 6666 CCCC 0000 0078 6666"
	$"9999 FFFF 0079 6666 9999 CCCC 007A 6666"
	$"9999 9999 007B 6666 9999 6666 007C 6666"
	$"9999 3333 007D 6666 9999 0000 007E 6666"
	$"6666 FFFF 007F 6666 6666 CCCC 0080 6666"
	$"6666 9999 0081 6666 6666 6666 0082 6666"
	$"6666 3333 0083 6666 6666 0000 0084 6666"
	$"3333 FFFF 0085 6666 3333 CCCC 0086 6666"
	$"3333 9999 0087 6666 3333 6666 0088 6666"
	$"3333 3333 0089 6666 3333 0000 008A 6666"
	$"0000 FFFF 008B 6666 0000 CCCC 008C 6666"
	$"0000 9999 008D 6666 0000 6666 008E 6666"
	$"0000 3333 008F 6666 0000 0000 0090 3333"
	$"FFFF FFFF 0091 3333 FFFF CCCC 0092 3333"
	$"FFFF 9999 0093 3333 FFFF 6666 0094 3333"
	$"FFFF 3333 0095 3333 FFFF 0000 0096 3333"
	$"CCCC FFFF 0097 3333 CCCC CCCC 0098 3333"
	$"CCCC 9999 0099 3333 CCCC 6666 009A 3333"
	$"CCCC 3333 009B 3333 CCCC 0000 009C 3333"
	$"9999 FFFF 009D 3333 9999 CCCC 009E 3333"
	$"9999 9999 009F 3333 9999 6666 00A0 3333"
	$"9999 3333 00A1 3333 9999 0000 00A2 3333"
	$"6666 FFFF 00A3 3333 6666 CCCC 00A4 3333"
	$"6666 9999 00A5 3333 6666 6666 00A6 3333"
	$"6666 3333 00A7 3333 6666 0000 00A8 3333"
	$"3333 FFFF 00A9 3333 3333 CCCC 00AA 3333"
	$"3333 9999 00AB 3333 3333 6666 00AC 3333"
	$"3333 3333 00AD 3333 3333 0000 00AE 3333"
	$"0000 FFFF 00AF 3333 0000 CCCC 00B0 3333"
	$"0000 9999 00B1 3333 0000 6666 00B2 3333"
	$"0000 3333 00B3 3333 0000 0000 00B4 0000"
	$"FFFF FFFF 00B5 0000 FFFF CCCC 00B6 0000"
	$"FFFF 9999 00B7 0000 FFFF 6666 00B8 0000"
	$"FFFF 3333 00B9 0000 FFFF 0000 00BA 0000"
	$"CCCC FFFF 00BB 0000 CCCC CCCC 00BC 0000"
	$"CCCC 9999 00BD 0000 CCCC 6666 00BE 0000"
	$"CCCC 3333 00BF 0000 CCCC 0000 00C0 0000"
	$"9999 FFFF 00C1 0000 9999 CCCC 00C2 0000"
	$"9999 9999 00C3 0000 9999 6666 00C4 0000"
	$"9999 3333 00C5 0000 9999 0000 00C6 0000"
	$"6666 FFFF 00C7 0000 6666 CCCC 00C8 0000"
	$"6666 9999 00C9 0000 6666 6666 00CA 0000"
	$"6666 3333 00CB 0000 6666 0000 00CC 0000"
	$"3333 FFFF 00CD 0000 3333 CCCC 00CE 0000"
	$"3333 9999 00CF 0000 3333 6666 00D0 0000"
	$"3333 3333 00D1 0000 3333 0000 00D2 0000"
	$"0000 FFFF 00D3 0000 0000 CCCC 00D4 0000"
	$"0000 9999 00D5 0000 0000 6666 00D6 0000"
	$"0000 3333 00D7 EEEE 0000 0000 00D8 DDDD"
	$"0000 0000 00D9 BBBB 0000 0000 00DA AAAA"
	$"0000 0000 00DB 8888 0000 0000 00DC 7777"
	$"0000 0000 00DD 5555 0000 0000 00DE 4444"
	$"0000 0000 00DF 2222 0000 0000 00E0 1111"
	$"0000 0000 00E1 0000 EEEE 0000 00E2 0000"
	$"DDDD 0000 00E3 0000 BBBB 0000 00E4 0000"
	$"AAAA 0000 00E5 0000 8888 0000 00E6 0000"
	$"7777 0000 00E7 0000 5555 0000 00E8 0000"
	$"4444 0000 00E9 0000 2222 0000 00EA 0000"
	$"1111 0000 00EB 0000 0000 EEEE 00EC 0000"
	$"0000 DDDD 00ED 0000 0000 BBBB 00EE 0000"
	$"0000 AAAA 00EF 0000 0000 8888 00F0 0000"
	$"0000 7777 00F1 0000 0000 5555 00F2 0000"
	$"0000 4444 00F3 0000 0000 2222 00F4 0000"
	$"0000 1111 00F5 EEEE EEEE EEEE 00F6 DDDD"
	$"DDDD DDDD 00F7 BBBB BBBB BBBB 00F8 AAAA"
	$"AAAA AAAA 00F9 8888 8888 8888 00FA 7777"
	$"7777 7777 00FB 5555 5555 5555 00FC 4444"
	$"4444 4444 00FD 2222 2222 2222 00FE 1111"
	$"1111 1111 00FF 0000 0000 0000 0000 0000"
	$"00C8 0190 0000 0000 00C8 0190 0000 0008"
	$"8100 8100 8100 E900 0008 8100 8100 8100"
	$"E900 0008 8100 8100 8100 E900 000E 8100"
	$"ED00 F4FF FD00 F7FF 8100 9800 000E 8100"
	$"EB00 F9FF F700 FCFF 8100 9600 000E 8100"
	$"EA00 FBFF F600 FEFF 8100 9400 0026 8100"
	$"E900 FCFF F600 01FF FF84 0000 FFEA 00FD"
	$"FF01 00FF E200 FDFF 0400 FF00 00FF F000"
	$"00FF E800 002F 8100 E900 FBFF F800 01FF"
	$"FF84 0001 FFFF EB00 00FF FD00 04FF FF00"
	$"00FF E600 00FF FD00 04FF FF00 FFFF F100"
	$"01FF FFE8 0000 2B81 00E8 00FC FFF9 0001"
	$"FFFF 8200 00FF EC00 00FF FB00 03FF 0000"
	$"FFE7 0000 FFFB 0003 FF00 00FF F000 00FF"
	$"E800 0049 8100 E800 FCFF F900 01FF FF82"
	$"0001 FF00 FEFF FE00 FEFF FE00 FEFF FC00"
	$"00FF F900 FEFF FC00 FEFF FD00 01FF 00FE"
	$"FFF9 0000 FFF8 0001 FF00 FEFF FB00 FEFF"
	$"FD00 01FF 00FE FFFD 00FD FFF4 0000 5D81"
	$"00E8 00FB FFFB 0001 FFFF 8100 01FF FFFE"
	$"0000 FFFE 0000 FFFC 0000 FFFA 0001 FFFF"
	$"FA00 00FF FC00 00FF FE00 02FF 0000 FEFF"
	$"FE00 00FF F900 01FF FFFA 0001 FFFF FE00"
	$"00FF FD00 00FF FE00 00FF FE00 01FF FFFE"
	$"0003 FF00 00FF FD00 00FF F500 0055 8100"
	$"E700 FCFF FC00 01FF FF81 0001 00FF FC00"
	$"00FF FE00 00FF FE00 00FF F700 01FF FFFC"
	$"0000 FFFD 0000 FFFE 0001 FFFF FE00 00FF"
	$"FC00 00FF F800 01FF FFFC 0000 FFFC 0003"
	$"FF00 00FF FC00 03FF 0000 FFFC 0002 FF00"
	$"FFF0 0000 5481 00E7 00FC FFFD 0001 FFFF"
	$"8100 0200 00FF FC00 00FF FE00 00FF FE00"
	$"00FF F500 01FF FFFE 0000 FFFB 0003 FFFF"
	$"00FF FE00 00FF FC00 00FF F600 01FF FFFE"
	$"0000 FFFC 0002 FF00 00FA FF02 0000 FFFC"
	$"0004 FF00 00FF FFF2 0000 6E81 00E6 00FC"
	$"FF03 0000 FFFF ED00 FAFF F200 FBFF F300"
	$"FEFF FB00 FDFF F200 FDFF 0100 00FC FFE3"
	$"0000 FFFC 0000 FFFD 0002 FF00 FFF9 0000"
	$"FFFB 0003 FF00 00FF FC00 00FF FE00 00FF"
	$"FE00 00FF FC00 00FF FB00 00FF FB00 03FF"
	$"0000 FFFC 0003 FF00 00FF F900 00FF FC00"
	$"00FF FD00 01FF FFF4 0000 7981 00E6 00FC"
	$"FF02 00FF FFEE 00FE FFFD 00FE FFF6 00FD"
	$"FFFE 00FD FFFA 00F9 FFFD 00FB FFF4 00FE"
	$"FFFE 0001 FF00 FDFF E200 00FF FC00 00FF"
	$"FD00 02FF 00FF F900 00FF FB00 03FF 0000"
	$"FFFD 0000 FFFD 0000 FFFE 0000 FFFC 0000"
	$"FFFB 0000 FFFB 0003 FF00 00FF FC00 03FF"
	$"0000 FFFC 0003 FF00 00FF FC00 00FF FB00"
	$"00FF F500 0085 8100 E600 F9FF EF00 01FF"
	$"FFFB 00FD FFF8 00FE FFFB 00FD FFF8 00FC"
	$"FFFE 0002 FF00 00FC FFF7 00FE FFFC 0001"
	$"FF00 FDFF E200 01FF FFFE 0000 FFFB 0000"
	$"FFF8 0001 FFFF FD00 00FF FE00 05FF 0000"
	$"FF00 FFFE 0001 FFFF FE00 00FF FC00 00FF"
	$"FB00 01FF FFFD 0000 FFFE 0000 FFFC 0000"
	$"FFFE 0000 FFFE 0000 FFFE 0001 FFFF FE00"
	$"03FF 0000 FFFD 0000 FFF5 0000 7081 00E5"
	$"00FB FFF0 00FE FFFA 00FD FFF9 00FE FFF9"
	$"00FE FFF8 00FD FFFE 0000 FFFE 00FC FFF8"
	$"00FE FFFA 00FC FFE2 0001 FF00 FEFF FA00"
	$"00FF F800 01FF 00FD FFFC 0001 FFFF FE00"
	$"FEFF 0300 00FF 00FE FFFE 00FE FFFC 0001"
	$"FF00 FDFF FE00 FEFF FE00 FEFF FE00 FEFF"
	$"FD00 01FF 00FE FFFD 00FD FFF4 0000 3081"
	$"00E5 00FC FFF0 00FE FFF9 00FD FFFA 00FE"
	$"FFF8 00FD FFF9 00FD FF02 0000 FFFD 00FC"
	$"FFF9 00FE FFF9 00FC FFD7 0000 FF9C 0000"
	$"3181 00E4 00FC FFF2 00FD FFF8 0001 FFFF"
	$"FA00 FDFF F800 FDFF F900 FDFF 0100 FFFC"
	$"00FD FFF9 00FD FFF9 00FD FFD7 0001 FFFF"
	$"9C00 002C 8100 E400 FCFF F200 FEFF EF00"
	$"FDFF F700 FDFF FA00 FDFF 0200 00FF FC00"
	$"FDFF F900 FEFF F800 FDFF D700 00FF 9B00"
	$"0029 8100 E500 FAFF F400 FDFF F000 FDFF"
	$"F600 FDFF FA00 FDFF 0100 FFFB 00FD FFFA"
	$"00FD FFF8 00FD FF81 00F0 0000 2981 00E6"
	$"00F9 FFF5 00FD FFEF 00FD FFF6 00FD FFFA"
	$"00FD FF01 00FF FC00 FDFF FA00 FDFF F800"
	$"FCFF 8100 F000 002A 8100 E600 02FF FF00"
	$"FCFF F500 FDFF F000 FDFF F500 FDFF FA00"
	$"FCFF FB00 FDFF FA00 FDFF F800 FDFF 8100"
	$"EF00 002B 8100 E700 01FF FFFE 00FC FFF6"
	$"00FD FFF0 00FD FFF6 00FD FFFA 00FB FFFB"
	$"00FD FFFB 00FD FFF7 00FD FF81 00EF 0000"
	$"2B81 00E8 0001 FFFF FD00 FCFF F700 FDFF"
	$"F000 FCFF F600 FDFF FA00 FCFF FA00 FDFF"
	$"FB00 FDFF F800 FCFF 8100 EF00 002B 8100"
	$"E900 01FF FFFC 00FB FFF8 00FD FFF0 00FC"
	$"FFF6 00FD FFFA 00FC FFFB 00FD FFFA 00FD"
	$"FFF8 00FD FF81 00EE 0000 2B81 00EA 0001"
	$"FFFF FA00 FCFF F800 FDFF F000 FDFF F600"
	$"FDFF FA00 FCFF FA00 FDFF FB00 FDFF F800"
	$"FCFF 8100 EE00 002A 8100 EB00 FEFF FA00"
	$"FCFF F800 FDFF F000 FDFF F600 FDFF FA00"
	$"FCFF FA00 FDFF FB00 FDFF F800 FCFF 8100"
	$"EE00 002E 8100 EB00 01FF FFF9 00FB FFF9"
	$"00FD FFF0 00FD FFF7 00FD FFF9 00FD FFFA"
	$"00FC FFFB 00FD FFF9 0001 FF00 FDFF 8100"
	$"EE00 0033 8100 EC00 01FF FFF7 00FC FFF9"
	$"00FD FFF6 0000 FFFC 00FD FFF7 00FE FFF8"
	$"00FD FFFA 00FD FFFD 0002 FF00 00FD FFFA"
	$"00FB FF81 00ED 0000 3681 00ED 0001 FFFF"
	$"F600 FBFF FA00 FDFF F600 00FF FC00 FDFF"
	$"F800 FEFF F800 FDFF F900 FDFF FE00 00FF"
	$"FE00 FDFF FA00 01FF 00FD FF81 00ED 0000"
	$"3681 00EE 00FE FFF5 00FC FFF9 00FD FFF8"
	$"0000 FFFA 00FE FFF9 00FE FFF7 00FD FFF9"
	$"00FD FF02 0000 FFFD 00FD FFFB 0002 FF00"
	$"00FD FF81 00ED 0000 3781 00EF 00FE FFF4"
	$"00FB FFF9 00FE FFFA 0001 FFFF F900 FDFF"
	$"FB00 FEFF F600 FDFF FA00 FCFF 0100 FFFB"
	$"00FD FFFE 0001 FFFF FE00 FDFF 8100 ED00"
	$"002F 8100 F100 FBFF F600 F9FF F900 FEFF"
	$"FC00 00FF F600 FDFF FD00 01FF FFF5 00FC"
	$"FFF9 00FC FFFA 00F9 FFFE 00FD FF81 00EC"
	$"0000 2681 00F2 00F7 FFFC 00F3 FFFB 00FA"
	$"FFF3 00FB FFF3 00FD FFF8 00FD FFF7 00FD"
	$"FFFC 00FD FF81 00EC 0000 0A81 0082 00FD"
	$"FF81 00EC 0000 0A81 0083 00FC FF81 00EC"
	$"0000 0A81 0083 00FD FF81 00EB 0000 0A81"
	$"0083 00FD FF81 00EB 0000 0A81 0083 00FD"
	$"FF81 00EB 0000 0A81 0084 00FD FF81 00EA"
	$"0000 0A81 0084 00FD FF81 00EA 0000 0A81"
	$"0084 00FD FF81 00EA 0000 0A81 0086 00FB"
	$"FF81 00EA 0000 0A81 0088 00F6 FF81 00ED"
	$"0000 0881 0081 0081 00E9 0000 0881 0081"
	$"0081 00E9 0000 0881 0081 0081 00E9 0000"
	$"0881 0081 0081 00E9 0000 0881 0081 0081"
	$"00E9 0000 0881 0081 0081 00E9 0000 3EA2"
	$"0001 FFFF EF00 01FF FFEF 0001 FFFF EF00"
	$"01FF FFEF 0001 FFFF EF00 01FF FFEF 0001"
	$"FFFF EF00 01FF FFEF 0001 FFFF EF00 01FF"
	$"FFEF 0001 FFFF EF00 01FF FFA6 0000 6EA4"
	$"0005 FFFF C0C0 FFFF F300 05FF FFC0 C0FF"
	$"FFF3 0005 FFFF C0C0 FFFF F300 05FF FFC0"
	$"C0FF FFF3 0005 FFFF C0C0 FFFF F300 05FF"
	$"FFC0 C0FF FFF3 0005 FFFF C0C0 FFFF F300"
	$"05FF FFC0 C0FF FFF3 0005 FFFF C0C0 FFFF"
	$"F300 05FF FFC0 C0FF FFF3 0005 FFFF C0C0"
	$"FFFF F300 05FF FFC0 C0FF FFA8 0000 7AA6"
	$"0001 FFFF FBC0 01FF FFF7 0001 FFFF FBC0"
	$"01FF FFF7 0001 FFFF FBC0 01FF FFF7 0001"
	$"FFFF FBC0 01FF FFF7 0001 FFFF FBC0 01FF"
	$"FFF7 0001 FFFF FBC0 01FF FFF7 0001 FFFF"
	$"FBC0 01FF FFF7 0001 FFFF FBC0 01FF FFF7"
	$"0001 FFFF FBC0 01FF FFF7 0001 FFFF FBC0"
	$"01FF FFF7 0001 FFFF FBC0 01FF FFF7 0001"
	$"FFFF FBC0 01FF FFAA 0000 7AA8 0001 FFFF"
	$"F7C0 01FF FFFB 0001 FFFF F7C0 01FF FFFB"
	$"0001 FFFF F7C0 01FF FFFB 0001 FFFF F7C0"
	$"01FF FFFB 0001 FFFF F7C0 01FF FFFB 0001"
	$"FFFF F7C0 01FF FFFB 0001 FFFF F7C0 01FF"
	$"FFFB 0001 FFFF F7C0 01FF FFFB 0001 FFFF"
	$"F7C0 01FF FFFB 0001 FFFF F7C0 01FF FFFB"
	$"0001 FFFF F7C0 01FF FFFB 0001 FFFF F7C0"
	$"01FF FFAC 0000 6FAA 0001 FFFF F3C0 05FF"
	$"FF00 00FF FFF3 C005 FFFF 0000 FFFF F3C0"
	$"05FF FF00 00FF FFF3 C005 FFFF 0000 FFFF"
	$"F3C0 05FF FF00 00FF FFF3 C005 FFFF 0000"
	$"FFFF F3C0 05FF FF00 00FF FFF3 C005 FFFF"
	$"0000 FFFF F3C0 05FF FF00 00FF FFF3 C005"
	$"FFFF 0000 FFFF F3C0 05FF FF00 00FF FFF3"
	$"C001 FFFF AE00 0042 AC00 01FF FFEF C001"
	$"FFFF EFC0 01FF FFEF C001 FFFF EFC0 01FF"
	$"FFEF C001 FFFF EFC0 01FF FFEF C001 FFFF"
	$"EFC0 01FF FFEF C001 FFFF EFC0 01FF FFEF"
	$"C001 FFFF EFC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0043 AC00 01FF FFEF C001 FFFF EFC0"
	$"01FF FFEF C001 FFFF EFC0 01FF FFEF C001"
	$"FFFF EFC0 01FF FFEF C001 FFFF EFC0 01FF"
	$"FFEF C001 FFFF EFC0 01FF FFEF C001 FFFF"
	$"EFC0 01FF FFB0 0000 77AE 0005 FFFF C0C0"
	$"FFFF F3C0 05FF FFE7 E7FF FFF3 C005 FFFF"
	$"B9B9 FFFF F3C0 05FF FFB9 B9FF FFF3 C005"
	$"FFFF B9B9 FFFF F3C0 05FF FFB9 B9FF FFF3"
	$"C005 FFFF B9B9 FFFF F3C0 05FF FFB9 B9FF"
	$"FFF3 C005 FFFF B9B9 FFFF F3C0 05FF FFB9"
	$"B9FF FFF3 C005 FFFF B9B9 FFFF F3C0 05FF"
	$"FFB9 B9FF FFF3 C005 FFFF C0C0 FFFF B200"
	$"0084 B000 01FF FFFB C001 FFFF F7C0 01FF"
	$"FFFB E701 FFFF F7C0 01FF FFFB B901 FFFF"
	$"F7C0 01FF FFFB B901 FFFF F7C0 01FF FFFB"
	$"B901 FFFF F7C0 01FF FFFB B901 FFFF F7C0"
	$"01FF FFFB B901 FFFF F7C0 01FF FFFB B901"
	$"FFFF F7C0 01FF FFFB B901 FFFF F7C0 01FF"
	$"FFFB B901 FFFF F7C0 01FF FFFB B901 FFFF"
	$"F7C0 01FF FFFB B901 FFFF F7C0 01FF FFFB"
	$"C001 FFFF B400 0084 B200 01FF FFF7 C001"
	$"FFFF FBC0 01FF FFF7 E701 FFFF FBC0 01FF"
	$"FFF7 B901 FFFF FBC0 01FF FFF7 B901 FFFF"
	$"FBC0 01FF FFF7 B901 FFFF FBC0 01FF FFF7"
	$"B901 FFFF FBC0 01FF FFF7 B901 FFFF FBC0"
	$"01FF FFF7 B901 FFFF FBC0 01FF FFF7 B901"
	$"FFFF FBC0 01FF FFF7 B901 FFFF FBC0 01FF"
	$"FFF7 B901 FFFF FBC0 01FF FFF7 B901 FFFF"
	$"FBC0 01FF FFF7 C001 FFFF B600 0078 B400"
	$"01FF FFF3 C005 FFFF C0C0 FFFF F3E7 05FF"
	$"FFC0 C0FF FFF3 B905 FFFF C0C0 FFFF F3B9"
	$"05FF FFC0 C0FF FFF3 B905 FFFF C0C0 FFFF"
	$"F3B9 05FF FFC0 C0FF FFF3 B905 FFFF C0C0"
	$"FFFF F3B9 05FF FFC0 C0FF FFF3 B905 FFFF"
	$"C0C0 FFFF F3B9 05FF FFC0 C0FF FFF3 B905"
	$"FFFF C0C0 FFFF F3B9 05FF FFC0 C0FF FFF3"
	$"C001 FFFF B800 004F B600 01FF FFEF C001"
	$"FFFF EFE7 01FF FFFA B9FE FFF9 B901 FFFF"
	$"EFB9 01FF FFEF B901 FFFF EFB9 01FF FFEF"
	$"B901 FFFF EFB9 01FF FFEF B901 FFFF EFB9"
	$"01FF FFFA B9FE FFF9 B901 FFFF EFB9 01FF"
	$"FFEF C000 FFB9 0000 42B6 0000 FFEE C000"
	$"FFEE E700 FFF9 B9FE FFF9 B900 FFEE B900"
	$"FFEE B900 FFEE B900 FFEE B900 FFEE B900"
	$"FFEE B900 FFEE B900 FFF9 B9FE FFF9 B900"
	$"FFEE B900 FFEE C000 FFB9 0000 4EB6 0000"
	$"FFEE C000 FFEE E700 FFF9 B906 FF00 FFB9"
	$"B9FF FFFD B900 FFEE B900 FFEE B900 FFEE"
	$"B900 FFEE B900 FFEE B900 FFEE B900 FFEE"
	$"B900 FFF9 B906 FF00 FFB9 B9FF FFFD B900"
	$"FFEE B900 FFEE C000 FFB9 0000 4CB6 0000"
	$"FFEE C000 FFEE E700 FFF9 B9FE FF03 B9B9"
	$"FFFF FDB9 00FF EEB9 00FF EEB9 00FF EEB9"
	$"00FF EEB9 00FF EEB9 00FF EEB9 00FF EEB9"
	$"00FF F9B9 FEFF 03B9 B9FF FFFD B900 FFEE"
	$"B900 FFEE C000 FFB9 0000 50B6 0000 FFEE"
	$"C000 FFEE E700 FFFE B9FE FF03 B9B9 FF00"
	$"FCFF FDB9 00FF EEB9 00FF EEB9 00FF EEB9"
	$"00FF EEB9 00FF EEB9 00FF EEB9 00FF EEB9"
	$"00FF FEB9 FEFF 03B9 B9FF 00FC FFFD B900"
	$"FFEE B900 FFEE C000 FFB9 0000 5AB6 0000"
	$"FFEE C000 FFEE E700 FFFE B904 FF00 FFB9"
	$"B9FD FF02 00FF FFFD B900 FFEE B900 FFEE"
	$"B900 FFEE B900 FFF9 B9FD FFFA B900 FFEE"
	$"B900 FFEE B900 FFEE B900 FFFE B904 FF00"
	$"FFB9 B9FD FF02 00FF FFFD B900 FFEE B900"
	$"FFEE C000 FFB9 0000 46B6 0000 FFEE C000"
	$"FFEE E700 FFFE B9F5 FFFD B900 FFEE B900"
	$"FFEE B900 FFEE B902 FFB9 B9F5 FFFC B900"
	$"FFEE B900 FFEE B900 FFEE B900 FFFE B9F5"
	$"FFFD B900 FFEE B900 FFEE C000 FFB9 0000"
	$"5EB6 0000 FFEE C000 FFEE E700 FFFE B910"
	$"FF00 FFFF 00FF 00FF FF00 FFFF B9FF FFB9"
	$"FFEE B900 FFEE B900 FFEE B900 FFFA B9F7"
	$"FF02 B9B9 FFEE B900 FFEE B900 FFEE B900"
	$"FFFE B910 FF00 FFFF 00FF 00FF FF00 FFFF"
	$"B9FF FFB9 FFEE B900 FFEE C000 FFB9 0000"
	$"4AB6 0000 FFEE C000 FFEE E700 FFFE B9F5"
	$"FF04 B9FF FFB9 FFEE B900 FFEE B900 FFEE"
	$"B900 FFFE B9F4 FFFE B900 FFEE B900 FFEE"
	$"B900 FFEE B900 FFFE B9F5 FF04 B9FF FFB9"
	$"FFEE B900 FFEE C000 FFB9 0000 5AB6 0000"
	$"FFEE C000 FFEE E700 FFFE B909 FF00 FFFF"
	$"00FF 00FF FF00 FCFF 01B9 FFEE B900 FFEE"
	$"B900 FFEE B900 FFFD B9F6 FFFD B900 FFEE"
	$"B900 FFEE B900 FFEE B900 FFFE B909 FF00"
	$"FFFF 00FF 00FF FF00 FCFF 01B9 FFEE B900"
	$"FFEE C000 FFB9 0000 40B6 0000 FFEE C000"
	$"FFEE E702 FFB9 B9F1 FF01 B9FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 02FF B9B9 F1FF 01B9 FFEE"
	$"B900 FFEE C000 FFB9 0000 3AB6 0000 FFEE"
	$"C000 FFEE E700 FFEE B900 FFEE B900 FFEE"
	$"B900 FFEE B900 FFEE B900 FFEE B900 FFEE"
	$"B900 FFEE B900 FFEE B900 FFEE B900 FFEE"
	$"C000 FFB9 0000 48B6 0001 FFFF EFC0 01FF"
	$"FFEF E701 FFFF EFB9 01FF FFEF B901 FFFF"
	$"EFB9 01FF FFEF B901 FFFF EFB9 01FF FFEF"
	$"B901 FFFF EFB9 01FF FFEF B901 FFFF EFB9"
	$"01FF FFEF B901 FFFF EFC0 01FF FFBA 0000"
	$"80B8 0005 FFFF C0C0 FFFF F3C0 05FF FFE7"
	$"E7FF FFF3 E705 FFFF E7E7 FFFF F3B9 05FF"
	$"FFB9 B9FF FFF3 B905 FFFF B9B9 FFFF F3B9"
	$"05FF FFB9 B9FF FFF3 B905 FFFF B9B9 FFFF"
	$"F3B9 05FF FFB9 B9FF FFF3 B905 FFFF B9B9"
	$"FFFF F3B9 05FF FFB9 B9FF FFF3 B905 FFFF"
	$"B9B9 FFFF F3B9 05FF FFB9 B9FF FFF3 B905"
	$"FFFF B9B9 FFFF F3C0 05FF FFC0 C0FF FFBC"
	$"0000 8EBA 0001 FFFF FBC0 01FF FFF7 C001"
	$"FFFF FBE7 01FF FFF7 E701 FFFF FBE7 01FF"
	$"FFF7 B901 FFFF FBB9 01FF FFF7 B901 FFFF"
	$"FBB9 01FF FFF7 B901 FFFF FBB9 01FF FFF7"
	$"B901 FFFF FBB9 01FF FFF7 B901 FFFF FBB9"
	$"01FF FFF7 B901 FFFF FBB9 01FF FFF7 B901"
	$"FFFF FBB9 01FF FFF7 B901 FFFF FBB9 01FF"
	$"FFF7 B901 FFFF FBB9 01FF FFF7 B901 FFFF"
	$"FBB9 01FF FFF7 C001 FFFF FBC0 01FF FFBE"
	$"0000 8EBC 0001 FFFF F7C0 01FF FFFB C001"
	$"FFFF F7E7 01FF FFFB E701 FFFF F7E7 01FF"
	$"FFFB B901 FFFF F7B9 01FF FFFB B901 FFFF"
	$"F7B9 01FF FFFB B901 FFFF F7B9 01FF FFFB"
	$"B901 FFFF F7B9 01FF FFFB B901 FFFF F7B9"
	$"01FF FFFB B901 FFFF F7B9 01FF FFFB B901"
	$"FFFF F7B9 01FF FFFB B901 FFFF F7B9 01FF"
	$"FFFB B901 FFFF F7B9 01FF FFFB B901 FFFF"
	$"F7B9 01FF FFFB C001 FFFF F7C0 01FF FFC0"
	$"0000 81BE 0001 FFFF F3C0 05FF FFC0 C0FF"
	$"FFF3 E705 FFFF E7E7 FFFF F3E7 05FF FFB9"
	$"B9FF FFF3 B905 FFFF B9B9 FFFF F3B9 05FF"
	$"FFB9 B9FF FFF3 B905 FFFF B9B9 FFFF F3B9"
	$"05FF FFB9 B9FF FFF3 B905 FFFF B9B9 FFFF"
	$"F3B9 05FF FFB9 B9FF FFF3 B905 FFFF B9B9"
	$"FFFF F3B9 05FF FFB9 B9FF FFF3 B905 FFFF"
	$"B9B9 FFFF F3B9 05FF FFC0 C0FF FFF3 C001"
	$"FFFF C200 004C C000 01FF FFEF C001 FFFF"
	$"EFE7 01FF FFEF E701 FFFF EFB9 01FF FFEF"
	$"B901 FFFF EFB9 01FF FFEF B901 FFFF EFB9"
	$"01FF FFEF B901 FFFF EFB9 01FF FFEF B901"
	$"FFFF EFB9 01FF FFEF B901 FFFF EFC0 00FF"
	$"C300 003E C000 00FF EEC0 00FF EEE7 00FF"
	$"EEE7 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEC0 00FF"
	$"C300 003E C000 00FF EEC0 00FF EEE7 00FF"
	$"EEE7 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEC0 00FF"
	$"C300 003E C000 00FF EEC0 00FF EEE7 00FF"
	$"EEE7 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEC0 00FF"
	$"C300 0042 C000 00FF EEC0 00FF EEE7 00FF"
	$"EEE7 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"FAB9 FDFF F9B9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEC0 00FF C300 0042 C000 00FF EEC0 00FF"
	$"EEE7 00FF EEE7 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF FCB9 F5FF 02B9 B9FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEC0 00FF C300 0042 C000 00FF"
	$"EEC0 00FF EEE7 00FF EEE7 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 02FF B9B9 F7FF FAB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEC0 00FF C300 0042"
	$"C000 00FF EEC0 00FF EEE7 00FF EEE7 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF FEB9 F4FF"
	$"FEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEC0 00FF"
	$"C300 0042 C000 00FF EEC0 00FF EEE7 00FF"
	$"EEE7 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"FDB9 F6FF FDB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEC0 00FF C300 003E C000 00FF EEC0 00FF"
	$"EEE7 00FF EEE7 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEC0 00FF C300 003E C000 00FF EEC0 00FF"
	$"EEE7 00FF EEE7 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEC0 00FF C300 003E C000 00FF EEC0 00FF"
	$"EEE7 00FF EEE7 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEB9 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EEC0 00FF C300 004D C000 01FF FFEF C001"
	$"FFFF EFE7 01FF FFEF E701 FFFF EFB9 01FF"
	$"FFEF B901 FFFF EFB9 01FF FFEF B901 FFFF"
	$"EFB9 01FF FFEF B901 FFFF EFB9 01FF FFEF"
	$"B901 FFFF EFB9 01FF FFEF B901 FFFF EFC0"
	$"01FF FFC4 0000 89C2 0005 FFFF C0C0 FFFF"
	$"F3C0 05FF FFE7 E7FF FFF3 E705 FFFF E7E7"
	$"FFFF F3E7 05FF FFB9 B9FF FFF3 B905 FFFF"
	$"E7E7 FFFF F3B9 05FF FF64 64FF FFF3 B905"
	$"FFFF B9B9 FFFF F3B9 05FF FFB9 B9FF FFF3"
	$"B905 FFFF B9B9 FFFF F3B9 05FF FF64 64FF"
	$"FFF3 B905 FFFF 0505 FFFF F3B9 05FF FFB9"
	$"B9FF FFF3 B905 FFFF B9B9 FFFF F3B9 05FF"
	$"FFB9 B9FF FFF3 C005 FFFF C0C0 FFFF C600"
	$"0098 C400 01FF FFFB C001 FFFF F7C0 01FF"
	$"FFFB E701 FFFF F7E7 01FF FFFB E701 FFFF"
	$"F7E7 01FF FFFB B901 FFFF F7B9 01FF FFFB"
	$"E701 FFFF F7B9 01FF FFFB 6401 FFFF F7B9"
	$"01FF FFFB B901 FFFF F7B9 01FF FFFB B901"
	$"FFFF F7B9 01FF FFFB B901 FFFF F7B9 01FF"
	$"FFFB 6401 FFFF F7B9 01FF FFFB 0501 FFFF"
	$"F7B9 01FF FFFB B901 FFFF F7B9 01FF FFFB"
	$"B901 FFFF F7B9 01FF FFFB B901 FFFF F7C0"
	$"01FF FFFB C001 FFFF C800 0098 C600 01FF"
	$"FFF7 C001 FFFF FBC0 01FF FFF7 E701 FFFF"
	$"FBE7 01FF FFF7 E701 FFFF FBE7 01FF FFF7"
	$"B901 FFFF FBB9 01FF FFF7 E701 FFFF FBB9"
	$"01FF FFF7 6401 FFFF FBB9 01FF FFF7 B901"
	$"FFFF FBB9 01FF FFF7 B901 FFFF FBB9 01FF"
	$"FFF7 B901 FFFF FBB9 01FF FFF7 6401 FFFF"
	$"FBB9 01FF FFF7 0501 FFFF FBB9 01FF FFF7"
	$"B901 FFFF FBB9 01FF FFF7 B901 FFFF FBB9"
	$"01FF FFF7 B901 FFFF FBC0 01FF FFF7 C001"
	$"FFFF CA00 008A C800 01FF FFF3 C005 FFFF"
	$"C0C0 FFFF F3E7 05FF FFE7 E7FF FFF3 E705"
	$"FFFF E7E7 FFFF F3B9 05FF FFB9 B9FF FFF3"
	$"E705 FFFF B9B9 FFFF F364 05FF FFB9 B9FF"
	$"FFF3 B905 FFFF B9B9 FFFF F3B9 05FF FFB9"
	$"B9FF FFF3 B905 FFFF B9B9 FFFF F364 05FF"
	$"FFB9 B9FF FFF3 0505 FFFF B9B9 FFFF F3B9"
	$"05FF FFB9 B9FF FFF3 B905 FFFF B9B9 FFFF"
	$"F3B9 05FF FFC0 C0FF FFF3 C001 FFFF CC00"
	$"0059 CA00 01FF FFEF C001 FFFF EFE7 01FF"
	$"FFEF E701 FFFF FAB9 FEFF F9B9 01FF FFEF"
	$"E701 FFFF EF64 01FF FFEF B901 FFFF EFB9"
	$"01FF FFEF B901 FFFF EF64 01FF FFEF 0501"
	$"FFFF FAB9 FEFF F9B9 01FF FFEF B901 FFFF"
	$"EFB9 01FF FFEF C000 FFCD 0000 4ACA 0000"
	$"FFEE C000 FFEE E700 FFEE E700 FFF9 B9FE"
	$"FFF9 B900 FFEE E700 FFEE 6400 FFEE B900"
	$"FFEE B900 FFEE B900 FFEE 6400 FFEE 0500"
	$"FFF9 B9FE FFF9 B900 FFEE B900 FFEE B900"
	$"FFEE C000 FFCD 0000 56CA 0000 FFEE C000"
	$"FFEE E700 FFEE E700 FFF9 B906 FF00 FFB9"
	$"B9FF FFFD B900 FFEE E700 FFEE 6400 FFEE"
	$"B900 FFEE B900 FFEE B900 FFEE 6400 FFEE"
	$"0500 FFF9 B906 FF00 FFB9 B9FF FFFD B900"
	$"FFEE B900 FFEE B900 FFEE C000 FFCD 0000"
	$"54CA 0000 FFEE C000 FFEE E700 FFEE E700"
	$"FFF9 B9FE FF03 B9B9 FFFF FDB9 00FF EEE7"
	$"00FF EE64 00FF EEB9 00FF EEB9 00FF EEB9"
	$"00FF EE64 00FF EE05 00FF F9B9 FEFF 03B9"
	$"B9FF FFFD B900 FFEE B900 FFEE B900 FFEE"
	$"C000 FFCD 0000 58CA 0000 FFEE C000 FFEE"
	$"E700 FFEE E700 FFFE B9FE FF03 B9B9 FF00"
	$"FCFF FDB9 00FF EEE7 00FF EE64 00FF EEB9"
	$"00FF EEB9 00FF EEB9 00FF EE64 00FF EE05"
	$"00FF FEB9 FEFF 03B9 B9FF 00FC FFFD B900"
	$"FFEE B900 FFEE B900 FFEE C000 FFCD 0000"
	$"62CA 0000 FFEE C000 FFEE E700 FFEE E700"
	$"FFFE B904 FF00 FFB9 B9FD FF02 00FF FFFD"
	$"B900 FFEE E700 FFEE 6400 FFEE B900 FFF9"
	$"B9FD FFFA B900 FFEE B900 FFEE 6400 FFEE"
	$"0500 FFFE B904 FF00 FFB9 B9FD FF02 00FF"
	$"FFFD B900 FFEE B900 FFEE B900 FFEE C000"
	$"FFCD 0000 4ECA 0000 FFEE C000 FFEE E700"
	$"FFEE E700 FFFE B9F5 FFFD B900 FFEE E700"
	$"FFEE 6400 FFEE B902 FFB9 B9F5 FFFC B900"
	$"FFEE B900 FFEE 6400 FFEE 0500 FFFE B9F5"
	$"FFFD B900 FFEE B900 FFEE B900 FFEE C000"
	$"FFCD 0000 66CA 0000 FFEE C000 FFEE E700"
	$"FFEE E700 FFFE B910 FF00 FFFF 00FF 00FF"
	$"FF00 FFFF B9FF FFB9 FFEE E700 FFEE 6400"
	$"FFEE B900 FFFA B9F7 FF02 B9B9 FFEE B900"
	$"FFEE 6400 FFEE 0500 FFFE B910 FF00 FFFF"
	$"00FF 00FF FF00 FFFF B9FF FFB9 FFEE B900"
	$"FFEE B900 FFEE C000 FFCD 0000 52CA 0000"
	$"FFEE C000 FFEE E700 FFEE E700 FFFE B9F5"
	$"FF04 B9FF FFB9 FFEE E700 FFEE 6400 FFEE"
	$"B900 FFFE B9F4 FFFE B900 FFEE B900 FFEE"
	$"6400 FFEE 0500 FFFE B9F5 FF04 B9FF FFB9"
	$"FFEE B900 FFEE B900 FFEE C000 FFCD 0000"
	$"62CA 0000 FFEE C000 FFEE E700 FFEE E700"
	$"FFFE B909 FF00 FFFF 00FF 00FF FF00 FCFF"
	$"01B9 FFEE E700 FFEE 6400 FFEE B900 FFFD"
	$"B9F6 FFFD B900 FFEE B900 FFEE 6400 FFEE"
	$"0500 FFFE B909 FF00 FFFF 00FF 00FF FF00"
	$"FCFF 01B9 FFEE B900 FFEE B900 FFEE C000"
	$"FFCD 0000 48CA 0000 FFEE C000 FFEE E700"
	$"FFEE E702 FFB9 B9F1 FF01 B9FF EEE7 00FF"
	$"EE64 00FF EEB9 00FF EEB9 00FF EEB9 00FF"
	$"EE64 00FF EE05 02FF B9B9 F1FF 01B9 FFEE"
	$"B900 FFEE B900 FFEE C000 FFCD 0000 42CA"
	$"0000 FFEE C000 FFEE E700 FFEE E700 FFEE"
	$"B900 FFEE E700 FFEE 6400 FFEE B900 FFEE"
	$"B900 FFEE B900 FFEE 6400 FFEE 0500 FFEE"
	$"B900 FFEE B900 FFEE B900 FFEE C000 FFCD"
	$"0000 51CA 0001 FFFF EFC0 01FF FFEF E701"
	$"FFFF EFE7 01FF FFEF B901 FFFF EFE7 01FF"
	$"FFEF 6401 FFFF EFB9 01FF FFEF B901 FFFF"
	$"EFB9 01FF FFEF 6401 FFFF EF05 01FF FFEF"
	$"B901 FFFF EFB9 01FF FFEF B901 FFFF EFC0"
	$"00FF CD00 008A C800 01FF FFF3 C005 FFFF"
	$"C0C0 FFFF F3E7 05FF FFE7 E7FF FFF3 E705"
	$"FFFF 6464 FFFF F3B9 05FF FF64 64FF FFF3"
	$"E705 FFFF 6464 FFFF F364 05FF FF64 64FF"
	$"FFF3 B905 FFFF B9B9 FFFF F3B9 05FF FF64"
	$"64FF FFF3 B905 FFFF 6464 FFFF F364 05FF"
	$"FF05 05FF FFF3 0505 FFFF 0505 FFFF F3B9"
	$"05FF FF05 05FF FFF3 B905 FFFF 0505 FFFF"
	$"F3B9 05FF FFC0 C0FF FFF3 C001 FFFF CC00"
	$"0098 C600 01FF FFF7 C001 FFFF FBC0 01FF"
	$"FFF7 E701 FFFF FBE7 01FF FFF7 E701 FFFF"
	$"FB64 01FF FFF7 B901 FFFF FB64 01FF FFF7"
	$"E701 FFFF FB64 01FF FFF7 6401 FFFF FB64"
	$"01FF FFF7 B901 FFFF FBB9 01FF FFF7 B901"
	$"FFFF FB64 01FF FFF7 B901 FFFF FB64 01FF"
	$"FFF7 6401 FFFF FB05 01FF FFF7 0501 FFFF"
	$"FB05 01FF FFF7 B901 FFFF FB05 01FF FFF7"
	$"B901 FFFF FB05 01FF FFF7 B901 FFFF FBC0"
	$"01FF FFF7 C001 FFFF CA00 0098 C400 01FF"
	$"FFFB C001 FFFF F7C0 01FF FFFB E701 FFFF"
	$"F7E7 01FF FFFB E701 FFFF F764 01FF FFFB"
	$"B901 FFFF F764 01FF FFFB E701 FFFF F764"
	$"01FF FFFB 6401 FFFF F764 01FF FFFB B901"
	$"FFFF F7B9 01FF FFFB B901 FFFF F764 01FF"
	$"FFFB B901 FFFF F764 01FF FFFB 6401 FFFF"
	$"F705 01FF FFFB 0501 FFFF F705 01FF FFFB"
	$"B901 FFFF F705 01FF FFFB B901 FFFF F705"
	$"01FF FFFB B901 FFFF F7C0 01FF FFFB C001"
	$"FFFF C800 0089 C200 05FF FFC0 C0FF FFF3"
	$"C005 FFFF E7E7 FFFF F3E7 05FF FFE7 E7FF"
	$"FFF3 6405 FFFF B9B9 FFFF F364 05FF FFE7"
	$"E7FF FFF3 6405 FFFF 6464 FFFF F364 05FF"
	$"FFB9 B9FF FFF3 B905 FFFF B9B9 FFFF F364"
	$"05FF FFB9 B9FF FFF3 6405 FFFF 6464 FFFF"
	$"F305 05FF FF05 05FF FFF3 0505 FFFF B9B9"
	$"FFFF F305 05FF FFB9 B9FF FFF3 0505 FFFF"
	$"B9B9 FFFF F3C0 05FF FFC0 C0FF FFC6 0000"
	$"4DC0 0001 FFFF EFC0 01FF FFEF E701 FFFF"
	$"EF64 01FF FFEF 6401 FFFF EF64 01FF FFEF"
	$"6401 FFFF EFB9 01FF FFEF 6401 FFFF EF64"
	$"01FF FFEF 0501 FFFF EF05 01FF FFEF 0501"
	$"FFFF EF05 01FF FFEF C001 FFFF C400 003E"
	$"C000 00FF EEC0 00FF EEE7 00FF EE64 00FF"
	$"EE64 00FF EE64 00FF EE64 00FF EEB9 00FF"
	$"EE64 00FF EE64 00FF EE05 00FF EE05 00FF"
	$"EE05 00FF EE05 00FF EEC0 00FF C300 003E"
	$"C000 00FF EEC0 00FF EEE7 00FF EE64 00FF"
	$"EE64 00FF EE64 00FF EE64 00FF EEB9 00FF"
	$"EE64 00FF EE64 00FF EE05 00FF EE05 00FF"
	$"EE05 00FF EE05 00FF EEC0 00FF C300 003E"
	$"C000 00FF EEC0 00FF EEE7 00FF EE64 00FF"
	$"EE64 00FF EE64 00FF EE64 00FF EEB9 00FF"
	$"EE64 00FF EE64 00FF EE05 00FF EE05 00FF"
	$"EE05 00FF EE05 00FF EEC0 00FF C300 003E"
	$"C000 00FF EEC0 00FF EEE7 00FF EE64 00FF"
	$"EE64 00FF EE64 00FF EE64 00FF EEB9 00FF"
	$"EE64 00FF EE64 00FF EE05 00FF EE05 00FF"
	$"EE05 00FF EE05 00FF EEC0 00FF C300 0042"
	$"C000 00FF EEC0 00FF EEE7 00FF EE64 00FF"
	$"EE64 00FF EE64 00FF EE64 00FF FAB9 FDFF"
	$"F9B9 00FF EE64 00FF EE64 00FF EE05 00FF"
	$"EE05 00FF EE05 00FF EE05 00FF EEC0 00FF"
	$"C300 0042 C000 00FF EEC0 00FF EEE7 00FF"
	$"EE64 00FF EE64 00FF EE64 00FF EE64 00FF"
	$"FCB9 F5FF 02B9 B9FF EE64 00FF EE64 00FF"
	$"EE05 00FF EE05 00FF EE05 00FF EE05 00FF"
	$"EEC0 00FF C300 0042 C000 00FF EEC0 00FF"
	$"EEE7 00FF EE64 00FF EE64 00FF EE64 00FF"
	$"EE64 02FF B9B9 F7FF FAB9 00FF EE64 00FF"
	$"EE64 00FF EE05 00FF EE05 00FF EE05 00FF"
	$"EE05 00FF EEC0 00FF C300 0042 C000 00FF"
	$"EEC0 00FF EEE7 00FF EE64 00FF EE64 00FF"
	$"EE64 00FF EE64 00FF FEB9 F4FF FEB9 00FF"
	$"EE64 00FF EE64 00FF EE05 00FF EE05 00FF"
	$"EE05 00FF EE05 00FF EEC0 00FF C300 0042"
	$"C000 00FF EEC0 00FF EEE7 00FF EE64 00FF"
	$"EE64 00FF EE64 00FF EE64 00FF FDB9 F6FF"
	$"FDB9 00FF EE64 00FF EE64 00FF EE05 00FF"
	$"EE05 00FF EE05 00FF EE05 00FF EEC0 00FF"
	$"C300 003E C000 00FF EEC0 00FF EEE7 00FF"
	$"EE64 00FF EE64 00FF EE64 00FF EE64 00FF"
	$"EEB9 00FF EE64 00FF EE64 00FF EE05 00FF"
	$"EE05 00FF EE05 00FF EE05 00FF EEC0 00FF"
	$"C300 003E C000 00FF EEC0 00FF EEE7 00FF"
	$"EE64 00FF EE64 00FF EE64 00FF EE64 00FF"
	$"EEB9 00FF EE64 00FF EE64 00FF EE05 00FF"
	$"EE05 00FF EE05 00FF EE05 00FF EEC0 00FF"
	$"C300 004C C000 01FF FFEF C001 FFFF EFE7"
	$"01FF FFEF 6401 FFFF EF64 01FF FFEF 6401"
	$"FFFF EF64 01FF FFEF B901 FFFF EF64 01FF"
	$"FFEF 6401 FFFF EF05 01FF FFEF 0501 FFFF"
	$"EF05 01FF FFEF 0501 FFFF EFC0 00FF C300"
	$"0081 BE00 01FF FFF3 C005 FFFF C0C0 FFFF"
	$"F3E7 05FF FFE7 E7FF FFF3 6405 FFFF B9B9"
	$"FFFF F364 05FF FFB9 B9FF FFF3 6405 FFFF"
	$"0505 FFFF F364 05FF FF05 05FF FFF3 B905"
	$"FFFF 0505 FFFF F364 05FF FF05 05FF FFF3"
	$"6405 FFFF 0505 FFFF F305 05FF FF05 05FF"
	$"FFF3 0505 FFFF 0505 FFFF F305 05FF FF05"
	$"05FF FFF3 0505 FFFF C0C0 FFFF F3C0 01FF"
	$"FFC2 0000 8EBC 0001 FFFF F7C0 01FF FFFB"
	$"C001 FFFF F7E7 01FF FFFB E701 FFFF F764"
	$"01FF FFFB B901 FFFF F764 01FF FFFB B901"
	$"FFFF F764 01FF FFFB 0501 FFFF F764 01FF"
	$"FFFB 0501 FFFF F7B9 01FF FFFB 0501 FFFF"
	$"F764 01FF FFFB 0501 FFFF F764 01FF FFFB"
	$"0501 FFFF F705 01FF FFFB 0501 FFFF F705"
	$"01FF FFFB 0501 FFFF F705 01FF FFFB 0501"
	$"FFFF F705 01FF FFFB C001 FFFF F7C0 01FF"
	$"FFC0 0000 8EBA 0001 FFFF FBC0 01FF FFF7"
	$"C001 FFFF FBE7 01FF FFF7 E701 FFFF FB64"
	$"01FF FFF7 B901 FFFF FB64 01FF FFF7 B901"
	$"FFFF FB64 01FF FFF7 0501 FFFF FB64 01FF"
	$"FFF7 0501 FFFF FBB9 01FF FFF7 0501 FFFF"
	$"FB64 01FF FFF7 0501 FFFF FB64 01FF FFF7"
	$"0501 FFFF FB05 01FF FFF7 0501 FFFF FB05"
	$"01FF FFF7 0501 FFFF FB05 01FF FFF7 0501"
	$"FFFF FB05 01FF FFF7 C001 FFFF FBC0 01FF"
	$"FFBE 0000 80B8 0005 FFFF C0C0 FFFF F3C0"
	$"05FF FFE7 E7FF FFF3 E705 FFFF 6464 FFFF"
	$"F3B9 05FF FF64 64FF FFF3 B905 FFFF 6464"
	$"FFFF F305 05FF FF64 64FF FFF3 0505 FFFF"
	$"B9B9 FFFF F305 05FF FF64 64FF FFF3 0505"
	$"FFFF 6464 FFFF F305 05FF FF05 05FF FFF3"
	$"0505 FFFF 0505 FFFF F305 05FF FF05 05FF"
	$"FFF3 0505 FFFF 0505 FFFF F3C0 05FF FFC0"
	$"C0FF FFBC 0000 50B6 0001 FFFF EFC0 01FF"
	$"FFEF E701 FFFF FAB9 FEFF F9B9 01FF FFEF"
	$"B901 FFFF EF05 01FF FFEF 0501 FFFF EF05"
	$"01FF FFEF 0501 FFFF EF05 01FF FFEF 0501"
	$"FFFF FA05 FEFF F905 01FF FFEF 0501 FFFF"
	$"EFC0 01FF FFBA 0000 42B6 0000 FFEE C000"
	$"FFEE E700 FFF9 B9FE FFF9 B900 FFEE B900"
	$"FFEE 0500 FFEE 0500 FFEE 0500 FFEE 0500"
	$"FFEE 0500 FFEE 0500 FFF9 05FE FFF9 0500"
	$"FFEE 0500 FFEE C000 FFB9 0000 4EB6 0000"
	$"FFEE C000 FFEE E700 FFF9 B906 FF00 FFB9"
	$"B9FF FFFD B900 FFEE B900 FFEE 0500 FFEE"
	$"0500 FFEE 0500 FFEE 0500 FFEE 0500 FFEE"
	$"0500 FFF9 0506 FF00 FF05 05FF FFFD 0500"
	$"FFEE 0500 FFEE C000 FFB9 0000 4CB6 0000"
	$"FFEE C000 FFEE E700 FFF9 B9FE FF03 B9B9"
	$"FFFF FDB9 00FF EEB9 00FF EE05 00FF EE05"
	$"00FF EE05 00FF EE05 00FF EE05 00FF EE05"
	$"00FF F905 FEFF 0305 05FF FFFD 0500 FFEE"
	$"0500 FFEE C000 FFB9 0000 50B6 0000 FFEE"
	$"C000 FFEE E700 FFFE B9FE FF03 B9B9 FF00"
	$"FCFF FDB9 00FF EEB9 00FF EE05 00FF EE05"
	$"00FF EE05 00FF EE05 00FF EE05 00FF EE05"
	$"00FF FE05 FEFF 0305 05FF 00FC FFFD 0500"
	$"FFEE 0500 FFEE C000 FFB9 0000 5AB6 0000"
	$"FFEE C000 FFEE E700 FFFE B904 FF00 FFB9"
	$"B9FD FF02 00FF FFFD B900 FFEE B900 FFEE"
	$"0500 FFEE 0500 FFF9 05FD FFFA 0500 FFEE"
	$"0500 FFEE 0500 FFEE 0500 FFFE 0504 FF00"
	$"FF05 05FD FF02 00FF FFFD 0500 FFEE 0500"
	$"FFEE C000 FFB9 0000 46B6 0000 FFEE C000"
	$"FFEE E700 FFFE B9F5 FFFD B900 FFEE B900"
	$"FFEE 0500 FFEE 0502 FF05 05F5 FFFC 0500"
	$"FFEE 0500 FFEE 0500 FFEE 0500 FFFE 05F5"
	$"FFFD 0500 FFEE 0500 FFEE C000 FFB9 0000"
	$"5EB6 0000 FFEE C000 FFEE E700 FFFE B910"
	$"FF00 FFFF 00FF 00FF FF00 FFFF B9FF FFB9"
	$"FFEE B900 FFEE 0500 FFEE 0500 FFFA 05F7"
	$"FF02 0505 FFEE 0500 FFEE 0500 FFEE 0500"
	$"FFFE 0510 FF00 FFFF 00FF 00FF FF00 FFFF"
	$"05FF FF05 FFEE 0500 FFEE C000 FFB9 0000"
	$"4AB6 0000 FFEE C000 FFEE E700 FFFE B9F5"
	$"FF04 B9FF FFB9 FFEE B900 FFEE 0500 FFEE"
	$"0500 FFFE 05F4 FFFE 0500 FFEE 0500 FFEE"
	$"0500 FFEE 0500 FFFE 05F5 FF04 05FF FF05"
	$"FFEE 0500 FFEE C000 FFB9 0000 5AB6 0000"
	$"FFEE C000 FFEE E700 FFFE B909 FF00 FFFF"
	$"00FF 00FF FF00 FCFF 01B9 FFEE B900 FFEE"
	$"0500 FFEE 0500 FFFD 05F6 FFFD 0500 FFEE"
	$"0500 FFEE 0500 FFEE 0500 FFFE 0509 FF00"
	$"FFFF 00FF 00FF FF00 FCFF 0105 FFEE 0500"
	$"FFEE C000 FFB9 0000 40B6 0000 FFEE C000"
	$"FFEE E702 FFB9 B9F1 FF01 B9FF EEB9 00FF"
	$"EE05 00FF EE05 00FF EE05 00FF EE05 00FF"
	$"EE05 00FF EE05 02FF 0505 F1FF 0105 FFEE"
	$"0500 FFEE C000 FFB9 0000 3AB6 0000 FFEE"
	$"C000 FFEE E700 FFEE B900 FFEE B900 FFEE"
	$"0500 FFEE 0500 FFEE 0500 FFEE 0500 FFEE"
	$"0500 FFEE 0500 FFEE 0500 FFEE 0500 FFEE"
	$"C000 FFB9 0000 47B6 0001 FFFF EFC0 01FF"
	$"FFEF E701 FFFF EFB9 01FF FFEF B901 FFFF"
	$"EF05 01FF FFEF 0501 FFFF EF05 01FF FFEF"
	$"0501 FFFF EF05 01FF FFEF 0501 FFFF EF05"
	$"01FF FFEF 0501 FFFF EFC0 00FF B900 0078"
	$"B400 01FF FFF3 C005 FFFF C0C0 FFFF F3E7"
	$"05FF FFC0 C0FF FFF3 B905 FFFF C0C0 FFFF"
	$"F3B9 05FF FFC0 C0FF FFF3 0505 FFFF C0C0"
	$"FFFF F305 05FF FFC0 C0FF FFF3 0505 FFFF"
	$"C0C0 FFFF F305 05FF FFC0 C0FF FFF3 0505"
	$"FFFF C0C0 FFFF F305 05FF FFC0 C0FF FFF3"
	$"0505 FFFF C0C0 FFFF F305 05FF FFC0 C0FF"
	$"FFF3 C001 FFFF B800 0084 B200 01FF FFF7"
	$"C001 FFFF FBC0 01FF FFF7 E701 FFFF FBC0"
	$"01FF FFF7 B901 FFFF FBC0 01FF FFF7 B901"
	$"FFFF FBC0 01FF FFF7 0501 FFFF FBC0 01FF"
	$"FFF7 0501 FFFF FBC0 01FF FFF7 0501 FFFF"
	$"FBC0 01FF FFF7 0501 FFFF FBC0 01FF FFF7"
	$"0501 FFFF FBC0 01FF FFF7 0501 FFFF FBC0"
	$"01FF FFF7 0501 FFFF FBC0 01FF FFF7 0501"
	$"FFFF FBC0 01FF FFF7 C001 FFFF B600 0084"
	$"B000 01FF FFFB C001 FFFF F7C0 01FF FFFB"
	$"E701 FFFF F7C0 01FF FFFB B901 FFFF F7C0"
	$"01FF FFFB B901 FFFF F7C0 01FF FFFB 0501"
	$"FFFF F7C0 01FF FFFB 0501 FFFF F7C0 01FF"
	$"FFFB 0501 FFFF F7C0 01FF FFFB 0501 FFFF"
	$"F7C0 01FF FFFB 0501 FFFF F7C0 01FF FFFB"
	$"0501 FFFF F7C0 01FF FFFB 0501 FFFF F7C0"
	$"01FF FFFB 0501 FFFF F7C0 01FF FFFB C001"
	$"FFFF B400 0077 AE00 05FF FFC0 C0FF FFF3"
	$"C005 FFFF E7E7 FFFF F3C0 05FF FFB9 B9FF"
	$"FFF3 C005 FFFF B9B9 FFFF F3C0 05FF FF05"
	$"05FF FFF3 C005 FFFF 0505 FFFF F3C0 05FF"
	$"FF05 05FF FFF3 C005 FFFF 0505 FFFF F3C0"
	$"05FF FF05 05FF FFF3 C005 FFFF 0505 FFFF"
	$"F3C0 05FF FF05 05FF FFF3 C005 FFFF 0505"
	$"FFFF F3C0 05FF FFC0 C0FF FFB2 0000 43AC"
	$"0001 FFFF EFC0 01FF FFEF C001 FFFF EFC0"
	$"01FF FFEF C001 FFFF EFC0 01FF FFEF C001"
	$"FFFF EFC0 01FF FFEF C001 FFFF EFC0 01FF"
	$"FFEF C001 FFFF EFC0 01FF FFEF C001 FFFF"
	$"B000 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0036 AC00 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"AF00 0036 AC00 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF EEC0 00FF EEC0 00FF"
	$"EEC0 00FF EEC0 00FF AF00 0042 AC00 01FF"
	$"FFEF C001 FFFF EFC0 01FF FFEF C001 FFFF"
	$"EFC0 01FF FFEF C001 FFFF EFC0 01FF FFEF"
	$"C001 FFFF EFC0 01FF FFEF C001 FFFF EFC0"
	$"01FF FFEF C001 FFFF EFC0 00FF AF00 006F"
	$"AA00 01FF FFF3 C005 FFFF 0000 FFFF F3C0"
	$"05FF FF00 00FF FFF3 C005 FFFF 0000 FFFF"
	$"F3C0 05FF FF00 00FF FFF3 C005 FFFF 0000"
	$"FFFF F3C0 05FF FF00 00FF FFF3 C005 FFFF"
	$"0000 FFFF F3C0 05FF FF00 00FF FFF3 C005"
	$"FFFF 0000 FFFF F3C0 05FF FF00 00FF FFF3"
	$"C005 FFFF 0000 FFFF F3C0 01FF FFAE 0000"
	$"7AA8 0001 FFFF F7C0 01FF FFFB 0001 FFFF"
	$"F7C0 01FF FFFB 0001 FFFF F7C0 01FF FFFB"
	$"0001 FFFF F7C0 01FF FFFB 0001 FFFF F7C0"
	$"01FF FFFB 0001 FFFF F7C0 01FF FFFB 0001"
	$"FFFF F7C0 01FF FFFB 0001 FFFF F7C0 01FF"
	$"FFFB 0001 FFFF F7C0 01FF FFFB 0001 FFFF"
	$"F7C0 01FF FFFB 0001 FFFF F7C0 01FF FFFB"
	$"0001 FFFF F7C0 01FF FFAC 0000 7AA6 0001"
	$"FFFF FBC0 01FF FFF7 0001 FFFF FBC0 01FF"
	$"FFF7 0001 FFFF FBC0 01FF FFF7 0001 FFFF"
	$"FBC0 01FF FFF7 0001 FFFF FBC0 01FF FFF7"
	$"0001 FFFF FBC0 01FF FFF7 0001 FFFF FBC0"
	$"01FF FFF7 0001 FFFF FBC0 01FF FFF7 0001"
	$"FFFF FBC0 01FF FFF7 0001 FFFF FBC0 01FF"
	$"FFF7 0001 FFFF FBC0 01FF FFF7 0001 FFFF"
	$"FBC0 01FF FFAA 0000 6EA4 0005 FFFF C0C0"
	$"FFFF F300 05FF FFC0 C0FF FFF3 0005 FFFF"
	$"C0C0 FFFF F300 05FF FFC0 C0FF FFF3 0005"
	$"FFFF C0C0 FFFF F300 05FF FFC0 C0FF FFF3"
	$"0005 FFFF C0C0 FFFF F300 05FF FFC0 C0FF"
	$"FFF3 0005 FFFF C0C0 FFFF F300 05FF FFC0"
	$"C0FF FFF3 0005 FFFF C0C0 FFFF F300 05FF"
	$"FFC0 C0FF FFA8 0000 3EA2 0001 FFFF EF00"
	$"01FF FFEF 0001 FFFF EF00 01FF FFEF 0001"
	$"FFFF EF00 01FF FFEF 0001 FFFF EF00 01FF"
	$"FFEF 0001 FFFF EF00 01FF FFEF 0001 FFFF"
	$"EF00 01FF FFA6 0000 0881 0081 0081 00E9"
	$"0000 0881 0081 0081 00E9 0000 0881 0081"
	$"0081 00E9 0000 0881 0081 0081 00E9 0000"
	$"0881 0081 0081 00E9 0000 0881 0081 0081"
	$"00E9 0000 0881 0081 0081 00E9 0000 0881"
	$"0081 0081 00E9 0000 0881 0081 0081 00E9"
	$"0000 0881 0081 0081 00E9 0000 0881 0081"
	$"0081 00E9 0000 0881 0081 0081 00E9 0000"
	$"0881 0081 0081 00E9 0000 0881 0081 0081"
	$"00E9 0000 0881 0081 0081 00E9 0000 0881"
	$"0081 0081 00E9 0000 0881 0081 0081 00E9"
	$"0000 0881 0081 0081 00E9 0000 0881 0081"
	$"0081 00E9 0000 0881 0081 0081 00E9 0000"
	$"0881 0081 0081 00E9 0000 0881 0081 0081"
	$"00E9 0000 0881 0081 0081 00E9 0000 0881"
	$"0081 0081 00E9 0000 00FF"
};

resource 'PICT' (131, "up/down arrow", purgeable) {
	89,
	{134, 272, 152, 283},
	$"1101 0100 0A00 8601 1000 9801 1B90 0002"
	$"00FE 0160 0110 0170 00FE 0160 0110 016B"
	$"0086 0110 0098 011B 0000 3F80 4040 8420"
	$"8E20 9F20 BFA0 8E20 8E20 8020 8020 8E20"
	$"8E20 BFA0 9F20 8E20 8420 4040 3F80 FF"
};

resource 'PICT' (132, "up/down arrow - up", purgeable) {
	125,
	{188, 503, 206, 514},
	$"1101 0100 0A00 BC01 F700 CE02 0290 0004"
	$"00BC 01F0 00CE 0208 00BC 01F7 00CE 0202"
	$"00BC 01F7 00CE 0202 0000 007F 0000 00FF"
	$"8000 01F7 C000 01E3 C000 01C1 C000 0180"
	$"C000 01E3 C000 01E3 C000 01FF C000 0100"
	$"4000 011C 4000 011C 4000 017F 4000 013E"
	$"4000 011C 4000 0108 4000 0080 8000 007F"
	$"0000 FF"
};

resource 'PICT' (133, "up/down arrow - down", purgeable) {
	89,
	{188, 520, 206, 531},
	$"1101 0100 0A00 BC02 0800 CE02 1390 0002"
	$"00BC 0208 00CE 0218 00BC 0208 00CE 0213"
	$"00BC 0208 00CE 0213 0000 3F80 4040 8420"
	$"8E20 9F20 BFA0 8E20 8E20 8020 FFE0 F1E0"
	$"F1E0 C060 E0E0 F1E0 FBE0 7FC0 3F80 FF"
};

resource 'PICT' (134, "dotdotdot") {
	75,
	{353, 53, 357, 65},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9000 0401 6100 3001 6500 4801 6100 3501"
	$"6500 4101 6100 3501 6500 4100 0000 0000"
	$"0003 3300 0003 3300 0000 0000 00A0 0083"
	$"FF"
};

resource 'PICT' (2010, "New") {
	631,
	{257, 223, 307, 353},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9800 1201 0100 D801 3301 6801 0100 DF01"
	$"3301 6101 0100 DF01 3301 6100 0008 0100"
	$"7FF3 FF01 FE00 0600 00F1 FF00 0006 0001"
	$"F1FF 0080 0801 01FA F3AA 01AF 8008 0101"
	$"F5F3 1101 1780 0801 01E0 F344 014B 8008"
	$"0101 D0F3 0001 1780 0801 01E0 F300 010B"
	$"8008 0101 C8F3 0001 1780 0801 01E0 F300"
	$"010B 8008 0101 D0F3 0001 1780 0801 01E0"
	$"F300 010B 8008 0101 C8F3 0001 1780 0801"
	$"01E0 F300 010B 8008 0101 D0F3 0001 1780"
	$"0801 01E0 F300 010B 8008 0101 C8F3 0001"
	$"1780 0E01 01E0 FD00 0203 C07E FA00 010B"
	$"800D 0101 D0FC 0001 C018 FA00 0117 800E"
	$"0101 E0FD 0002 01C0 30FA 0001 0B80 0E01"
	$"01C8 FD00 0201 E030 FA00 0117 800E 0101"
	$"E0FD 0002 03E0 60FA 0001 0B80 1201 01D0"
	$"FD00 0603 6060 FC7D F7C0 FE00 0117 8012"
	$"0101 E0FD 0006 0670 C1CE 30E3 80FE 0001"
	$"0B80 1101 01C8 FD00 0506 30C3 8630 C7FD"
	$"0001 1780 1101 01E0 FD00 050C 3187 0E31"
	$"CEFD 0001 0B80 1101 01D0 FD00 050C 3987"
	$"FC33 DCFD 0001 1780 1101 01E0 FD00 0518"
	$"1B0E 0036 F8FD 0001 0B80 1101 01C8 FD00"
	$"0518 1B0E 003C F0FD 0001 1780 1101 01E0"
	$"FD00 0530 1E0C 0078 E0FD 0001 0B80 1101"
	$"01D0 FD00 0530 0E0E 1871 C0FD 0001 1780"
	$"1101 01E0 FD00 0560 0C0F F061 80FD 0001"
	$"0B80 1101 01C8 FE00 0501 F80C 07C0 41FC"
	$"0001 1780 0801 01E0 F300 010B 8008 0101"
	$"D0F3 0001 1780 0801 01E0 F300 010B 8008"
	$"0101 C8F3 0001 1780 0801 01E0 F300 010B"
	$"8008 0101 D0F3 0001 1780 0801 01E0 F300"
	$"010B 8008 0101 C8F3 0001 1780 0801 01E0"
	$"F300 010B 8008 0101 D0F3 0001 1780 0801"
	$"01E0 F300 012B 8008 0101 D5F3 5501 5780"
	$"0801 01EA F3AA 01AF 8008 0101 F5F3 5501"
	$"5F80 0600 01F1 FF00 8006 0000 F1FF 0000"
	$"0801 007F F3FF 01FE 00A0 0083 FF"
};

resource 'PICT' (2011, "Open") {
	645,
	{258, 223, 307, 353},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9800 1201 0200 D801 3301 6801 0200 DF01"
	$"3301 6101 0200 DF01 3301 6100 0002 EF00"
	$"02EF 0008 0100 1FF3 FF01 F800 0801 002A"
	$"F3AA 01AC 0008 0100 61F3 1101 1600 0801"
	$"0054 F344 014A 0008 0100 60F3 0001 1600"
	$"0801 0048 F300 010A 0008 0100 60F3 0001"
	$"1600 0801 0050 F300 010A 0008 0100 60F3"
	$"0001 1600 0801 0048 F300 010A 0008 0100"
	$"60F3 0001 1600 0801 0050 F300 010A 0008"
	$"0100 60F3 0001 1600 0801 0048 F300 010A"
	$"000D 0100 60FD 0001 1F80 F900 0116 000D"
	$"0100 50FD 0001 70C0 F900 010A 000E 0100"
	$"60FE 0002 01C0 E0F9 0001 1600 0E01 0048"
	$"FE00 0203 8060 F900 010A 000E 0100 60FE"
	$"0002 0700 60F9 0001 1600 1301 0050 FE00"
	$"070E 0060 7E01 F02D C0FE 0001 0A00 1301"
	$"0060 FE00 071C 00E1 FF07 383F E0FE 0001"
	$"1600 1301 0048 FE00 071C 00E7 E70E 183C"
	$"E0FE 0001 0A00 1301 0060 FE00 0738 01C3"
	$"871C 3871 C0FE 0001 1600 1301 0050 FE00"
	$"0738 01C3 871F F071 C0FE 0001 0A00 1301"
	$"0060 FE00 0730 0387 0E38 00E3 80FE 0001"
	$"1600 1301 0048 FE00 0730 0707 0E38 00E3"
	$"80FE 0001 0A00 1201 0060 FE00 0630 0E0E"
	$"1C30 01C7 FD00 0116 0012 0100 50FE 0006"
	$"381C 0E18 3861 C7FD 0001 0A00 1201 0060"
	$"FE00 0618 701E 701F 838E FD00 0116 0013"
	$"0100 48FE 0007 0FC0 1FC0 0F0F DF80 FE00"
	$"010A 000C 0100 60FC 0000 38F9 0001 1600"
	$"0C01 0050 FC00 0038 F900 010A 000C 0100"
	$"60FC 0000 70F9 0001 1600 0C01 0048 FC00"
	$"0070 F900 010A 000C 0100 60FC 0000 E0F9"
	$"0001 1600 0D01 0050 FD00 0103 F8F9 0001"
	$"0A00 0801 0060 F300 0116 0008 0100 48F3"
	$"0001 0A00 0801 0060 F300 0116 0008 0100"
	$"50F3 0001 0A00 0801 0060 F300 0116 0007"
	$"0100 4AF2 AA00 0007 0000 F255 0156 0008"
	$"0100 2AF3 AA01 AC00 0801 001F F3FF 01F8"
	$"0002 EF00 02EF 00A0 0083 FF"
};

resource 'PICT' (2012, "Connect") {
	619,
	{258, 223, 307, 353},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9800 1201 0200 D801 3301 6801 0200 DF01"
	$"3301 6101 0200 DF01 3301 6100 0002 EF00"
	$"02EF 0008 0100 1FF3 FF01 F800 0801 002A"
	$"F3AA 01AC 0008 0100 61F3 1101 1600 0801"
	$"0054 F344 014A 0008 0100 60F3 0001 1600"
	$"0801 0048 F300 010A 0008 0100 60F3 0001"
	$"1600 0801 0050 F300 010A 0008 0100 60F3"
	$"0001 1600 0801 0048 F300 010A 0008 0100"
	$"60F3 0001 1600 0801 0050 F300 010A 0008"
	$"0100 60F3 0001 1600 0801 0048 F300 010A"
	$"000C 0500 6000 003F B0F7 0001 1600 0C05"
	$"0050 0000 E0F0 F700 010A 000C 0500 6000"
	$"0380 E0F7 0001 1600 0E05 0048 0007 0060"
	$"F900 0318 000A 000E 0500 6000 0E00 C0F9"
	$"0003 7000 1600 1311 0050 001C 0000 F80E"
	$"E03B 807C 01F9 FE00 0A00 1311 0060 0038"
	$"0007 BC1F F07F C1CE 0F78 E000 1600 1311"
	$"0048 0038 000F 1C1E 7079 C386 1E38 E000"
	$"0A00 1311 0060 0070 001C 1C38 E0E3 870E"
	$"3801 C000 1600 1311 0050 0070 001C 1C38"
	$"E0E3 87FC 3801 C000 0A00 1311 0060 0060"
	$"0038 3871 C1C7 0E00 7003 8000 1600 1311"
	$"0048 0060 0038 3871 C1C7 0E00 7003 8000"
	$"0A00 1311 0060 0060 0C30 70E3 838E 0C00"
	$"60C7 0000 1600 1311 0050 0070 1C38 E0E3"
	$"838E 0E18 71C7 3000 0A00 1311 0060 0038"
	$"701B C1C7 071C 07E0 3F07 C000 1600 1311"
	$"0048 001F C00F 07EF DFBF 03C0 1C03 8000"
	$"0A00 0801 0060 F300 0116 0008 0100 50F3"
	$"0001 0A00 0801 0060 F300 0116 0008 0100"
	$"48F3 0001 0A00 0801 0060 F300 0116 0008"
	$"0100 50F3 0001 0A00 0801 0060 F300 0116"
	$"0008 0100 48F3 0001 0A00 0801 0060 F300"
	$"0116 0008 0100 50F3 0001 0A00 0801 0060"
	$"F300 0116 0007 0100 4AF2 AA00 0007 0000"
	$"F255 0156 0008 0100 2AF3 AA01 AC00 0801"
	$"001F F3FF 01F8 0002 EF00 02EF 00A0 0083"
	$"FF"
};

resource 'PICT' (130, "Map Controls TL") {
	783,
	{197, 273, 378, 304},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9000 0400 C501 1001 7A01 3000 C501 1101"
	$"7A01 3000 C501 1101 7A01 3000 0000 0000"
	$"0100 0000 0100 0000 0100 0000 010F FFFF"
	$"F108 0000 1108 0000 1108 0000 1108 1C38"
	$"1108 1818 1108 1428 1108 0420 1108 0240"
	$"1108 03C0 1108 43C2 1108 FFFF 1108 43C2"
	$"1108 03C0 1108 0240 1108 0420 1108 1428"
	$"1108 1818 1108 1C38 1108 0000 1108 0000"
	$"1108 0000 110F FFFF F100 0000 0100 0000"
	$"0100 0000 0100 0000 017F FFFF FF00 0180"
	$"0100 0180 0100 8181 0101 8181 8103 8181"
	$"C107 F99F E10E F99F 711C 1998 390E F99F"
	$"7107 F99F E103 8181 C101 8181 8100 8181"
	$"0100 0180 0100 0180 017F FFFF FF00 0180"
	$"0100 0180 0100 8181 0101 8181 8102 8181"
	$"4104 F99F 2108 0990 1110 0990 0908 0990"
	$"1104 F99F 2102 8181 4101 8181 8100 8181"
	$"0100 0180 0100 0180 017F FFFF FF00 0180"
	$"0100 0180 0100 8181 0101 8181 8102 8181"
	$"4107 F99F E10E E997 711B B99D D90E E997"
	$"7107 F99F E102 8181 4101 8181 8100 8181"
	$"0100 0180 0100 0180 017F FFFF FF00 0180"
	$"0100 0180 0100 8181 0101 8181 8102 8181"
	$"4105 799E A10A A995 5115 599A A90A A995"
	$"5105 799E A102 8181 4101 8181 8100 8181"
	$"0100 0180 0100 0180 017F FFFF FF00 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 0000 0000 0000 0000 0000 0000"
	$"0000 0000 007F FFFF FF00 0000 0100 E010"
	$"8101 1000 8101 05B3 8101 3614 8101 1414"
	$"8101 1414 8100 E413 8100 0000 0100 0000"
	$"017F FFFF FF00 0000 0132 0000 0132 0000"
	$"012A 6763 1D2A 1494 A126 7497 9926 9494"
	$"0522 7493 3900 0000 0100 0000 017F FFFF"
	$"FF00 0000 013C 0001 8122 0000 8122 6338"
	$"993C 94A4 A520 F4A4 BD20 84A4 A120 6338"
	$"9900 0020 0100 0020 017F FFFF FF00 0000"
	$"0107 9800 0104 4800 0104 499C 7107 8852"
	$"8104 09D2 6104 0A52 1104 09D2 E100 0000"
	$"0100 0000 017F FFFF FF00 0000 0100 0420"
	$"0100 0420 0100 0A20 0100 0A20 0100 1F20"
	$"0100 1120 0100 1120 0100 0000 0100 0000"
	$"017F FFFF FF00 0000 011C 0004 6D22 0004"
	$"2520 630A 251C 948A 2502 F79F 2522 8411"
	$"251C 6311 2500 0000 0100 0000 017F FFFF"
	$"FFA0 0083 FF"
};

resource 'PICT' (3001, "You Won!") {
	955,
	{47, 117, 88, 333},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9800 1C00 2F00 7000 5801 5000 2F00 7500"
	$"5801 4D00 2F00 7500 5801 4D00 0002 E500"
	$"02E5 0002 E500 02E5 0016 0500 7FFF C1FF"
	$"C0FA 0006 1FFF CFFF F0FF 80FB 0001 1E00"
	$"1404 001F FC00 3FF9 0005 07FE 01FF 801E"
	$"FA00 013F 0014 0400 0FFC 001E F900 0503"
	$"FE00 FF80 0CFA 0001 7F80 1404 0007 FC00"
	$"1CF9 0005 03FE 00FF 800C FA00 017F 8014"
	$"0400 07FC 001C F900 0503 FE00 7F80 1CFA"
	$"0001 7F80 1404 0003 FE00 18F9 0005 01FE"
	$"007F 8018 FA00 017F 8014 0400 03FE 0030"
	$"F900 0501 FF00 3FC0 18FA 0001 7F80 1404"
	$"0001 FF00 70F9 0005 01FF 003F C030 FA00"
	$"013F 0013 0400 01FF 0060 F800 04FF 007F"
	$"E030 FA00 013F 0013 0400 00FF 80C0 F800"
	$"04FF 807F E030 FA00 013F 001D 1B00 00FF"
	$"80C0 000F E00F F87F C000 007F 80DF E060"
	$"001F C01F F0FC 001E 001D 1B00 007F C180"
	$"007F F807 F83F C000 007F 80DF F060 00FF"
	$"F00F F3FE 001E 001D 1B00 007F C300 00F8"
	$"7C03 F81F C000 007F C19F F060 01F0 F807"
	$"F7FF 001E 001D 1B00 003F E300 01F0 3E03"
	$"F81F C000 003F C18F F0C0 03E0 7C07 FC7F"
	$"800E 001D 1B00 003F E600 03F0 3F03 F81F"
	$"C000 003F C10F F8C0 07E0 7E07 F83F 800C"
	$"001D 1B00 001F FE00 07E0 1F83 F81F C000"
	$"003F E30F F8C0 0FC0 3F07 F03F 800C 001D"
	$"1B00 001F FC00 07E0 1F83 F81F C000 001F"
	$"E307 F980 0FC0 3F07 F03F 800C 001D 1B00"
	$"000F F800 07E0 1F83 F81F C000 001F E607"
	$"FD80 0FC0 3F07 F03F 800C 001D 1B00 000F"
	$"F800 0FE0 1FC3 F81F C000 001F F603 FD80"
	$"1FC0 3F87 F03F 800C 001D 1B00 0007 F800"
	$"0FE0 1FC3 F81F C000 000F F403 FF00 1FC0"
	$"3F87 F03F 800C 001D 1900 0007 F800 0FE0"
	$"1FC3 F81F C000 000F FC03 FF00 1FC0 3F87"
	$"F03F 80FF 001D 1900 0007 F800 0FE0 1FC3"
	$"F81F C000 000F FC01 FF00 1FC0 3F87 F03F"
	$"80FF 001D 1900 0007 F800 0FE0 1FC3 F81F"
	$"C000 0007 F801 FE00 1FC0 3F87 F03F 80FF"
	$"001D 1900 0007 F800 0FE0 1FC3 F81F C000"
	$"0007 F801 FE00 1FC0 3F87 F03F 80FF 001D"
	$"1B00 0007 F800 07E0 1F83 F81F C000 0007"
	$"F000 FE00 0FC0 3F07 F03F 801E 001D 1B00"
	$"0007 F800 07E0 1F83 F81F C000 0003 F000"
	$"FC00 0FC0 3F07 F03F 803F 001D 1B00 0007"
	$"F800 07E0 1F83 F81F C000 0003 F000 FC00"
	$"0FC0 3F07 F03F 807F 801D 1B00 0007 F800"
	$"03F0 3F03 F83F C000 0003 E000 7C00 07E0"
	$"7E07 F03F 807F 801D 1B00 0007 F800 01F0"
	$"3E03 FC7F C000 0001 E000 7800 03E0 7C07"
	$"F03F 807F 801D 1B00 0007 F800 00F8 7C01"
	$"FFDF E000 0001 C000 3800 01F0 F807 F03F"
	$"807F 801D 1B00 000F FC00 007F F800 FF9F"
	$"F000 0001 C000 3800 00FF F00F F87F C03F"
	$"001C 0A00 007F FF80 000F E000 3E1C FD00"
	$"0CC0 0030 0000 1FC0 1FFC FFE0 1E00 02E5"
	$"0002 E500 02E5 0002 E500 02E5 00A0 0083"
	$"FF"
};

resource 'PICT' (3002, "You Lost!") {
	942,
	{258, 135, 299, 344},
	$"1101 A000 8201 000A 0000 0000 02D0 0240"
	$"9800 1C01 0200 8001 2B01 5801 0200 8701"
	$"2B01 5801 0200 8701 2B01 5800 0002 E500"
	$"02E5 0002 E500 02E5 0013 0500 1FFF F07F"
	$"F0FA 0002 07FF F8F8 0002 0780 0013 0500"
	$"07FF 000F C0FA 0002 01FF C0F8 0002 0FC0"
	$"0013 0500 03FF 0007 80F9 0001 FF80 F900"
	$"0380 1FE0 0013 0400 01FF 0007 F800 017F"
	$"80FA 0004 0180 1FE0 0013 0400 01FF 0007"
	$"F800 017F 80FA 0004 0380 1FE0 0013 0400"
	$"00FF 8006 F800 017F 80FA 0004 0780 1FE0"
	$"0013 0400 00FF 800C F800 017F 80FA 0004"
	$"0F80 1FE0 0013 0400 007F C01C F800 017F"
	$"80FA 0004 1F80 0FC0 0013 0400 007F C018"
	$"F800 017F 80FA 0004 3F80 0FC0 0013 0400"
	$"003F E030 F800 017F 80FA 0004 7F80 0FC0"
	$"001D 1B00 003F E030 0003 F803 FE1F F000"
	$"007F 8000 0007 F000 3F08 FFFC 0780 001D"
	$"1B00 001F F060 001F FE01 FE0F F000 007F"
	$"8000 003F FC00 FFF8 FFFC 0780 001D 1B00"
	$"001F F0C0 003E 1F00 FE07 F000 007F 8000"
	$"007C 3E01 E0F8 3F80 0780 001D 1B00 000F"
	$"F8C0 007C 0F80 FE07 F000 007F 8000 00F8"
	$"1F03 C038 3F80 0380 001D 1900 000F F980"
	$"00FC 0FC0 FE07 F000 007F 8000 01F8 1F87"
	$"C018 3F80 03FF 001D 1900 0007 FF80 01F8"
	$"07E0 FE07 F000 007F 8000 03F0 0FC7 C018"
	$"3F80 03FF 001D 1900 0007 FF00 01F8 07E0"
	$"FE07 F000 007F 8000 03F0 0FC7 E008 3F80"
	$"03FF 001D 1900 0003 FE00 01F8 07E0 FE07"
	$"F000 007F 8000 03F0 0FC7 F800 3F80 03FF"
	$"001D 1900 0003 FE00 03F8 07F0 FE07 F000"
	$"007F 8000 07F0 0FE7 FE00 3F80 03FF 001D"
	$"1900 0001 FE00 03F8 07F0 FE07 F000 007F"
	$"8000 07F0 0FE3 FFC0 3F80 03FF 001C 1800"
	$"0001 FE00 03F8 07F0 FE07 F000 007F 8000"
	$"07F0 0FE1 FFE0 3F80 FE00 1C18 0000 01FE"
	$"0003 F807 F0FE 07F0 0000 7F80 0047 F00F"
	$"E0FF F83F 80FE 001C 1800 0001 FE00 03F8"
	$"07F0 FE07 F000 007F 8000 C7F0 0FE0 3FF8"
	$"3F80 FE00 1C18 0000 01FE 0003 F807 F0FE"
	$"07F0 0000 7F80 00C7 F00F E00F FC3F 80FE"
	$"001D 1B00 0001 FE00 01F8 07E0 FE07 F000"
	$"007F 8001 83F0 0FC4 01FC 3F80 0780 001D"
	$"1B00 0001 FE00 01F8 07E0 FE07 F000 007F"
	$"8001 83F0 0FC4 00FC 3F80 0FC0 001D 1B00"
	$"0001 FE00 01F8 07E0 FE07 F000 007F 8003"
	$"83F0 0FC6 007C 3F80 1FE0 001D 1B00 0001"
	$"FE00 00FC 0FC0 FE0F F000 007F 8007 81F8"
	$"1F87 007C 3F86 1FE0 001D 1B00 0001 FE00"
	$"007C 0F80 FF1F F000 007F 801F 80F8 1F07"
	$"8078 3FCC 1FE0 001D 1B00 0001 FE00 003E"
	$"1F00 7FF7 F800 007F C07F 007C 3E07 E0F0"
	$"1FF8 1FE0 001D 0D00 0003 FF00 001F FE00"
	$"3FE7 FC00 00FE FF0A 003F FC07 FFE0 0FF0"
	$"0FC0 001D 0D00 001F FFE0 0003 F800 0F87"
	$"0000 07FE FF0A 0007 F004 3F80 07E0 0780"
	$"0002 E500 02E5 0002 E500 02E5 0002 E500"
	$"A000 83FF"
};

resource 'ICON' (128, "hexagon world") {
	$"0000 0000 0000 0000 01FF FF00 03FF BF80"
	$"03FF EF80 07FF FFC0 07FF F7C0 0FFF FBE0"
	$"0FFC FBE0 1FF8 F7F0 1FE0 F9F0 3F80 FFF8"
	$"3F81 FBF8 7F81 F07C 7FC1 F83C FFFB F81E"
	$"EFBF F81E 7FBF F01C 7FCF F1FC 3FC7 E0F8"
	$"3FE3 E0F8 1FF3 E8F0 1FFB E830 0FFF EC20"
	$"0FFF F460 07DF F0C0 07F7 FFC0 03FF FF80"
	$"03FF FF80 01FF FF"
};

resource 'ICON' (129, "cylinder world") {
	$"0000 0000 00FF FF00 0F80 00F0 3400 015C"
	$"6A80 002A 5400 0156 7A80 002E 7F00 01FE"
	$"7FFF FFFE 6FFF FFFE 4FFF 9FFE 47FF EFFE"
	$"43FF FFEE 41FF F7CE 41FF FF9E 41FF FF9E"
	$"41FF FFCA 63FF FFC2 7FFF F3EA 7FF0 01F2"
	$"7FC1 07FA 7FE0 FFFE 7FF9 FFFE 7FFF FFFE"
	$"7FF7 FFFE 7FF3 FFFE 7CF3 FFBE 7E43 FFFE"
	$"3F81 FFFC 0FFF FFF0 00FF FF"
};

resource 'CURS' (128, preload) {
	$"1201 2102 2102 4084 4084 C0CC C0CC C0CC"
	$"C0CC C0CC C0CC 4084 4084 2102 2102 1201",
	$"1201 2102 2102 4084 4084 C0CC C0CC C0CC"
	$"C0CC C0CC C0CC 4084 4084 2102 2102 1201",
	{7, 4}
};

resource 'CURS' (129, preload) {
	$"4804 8408 8408 0210 0210 0330 0330 0330"
	$"0330 0330 0330 0210 0210 8408 8408 4804",
	$"4804 8408 8408 0210 0210 0330 0330 0330"
	$"0330 0330 0330 0210 0210 8408 8408 4804",
	{7, 4}
};

resource 'CURS' (130, preload) {
	$"2012 1021 1021 0840 0840 0CC0 0CC0 0CC0"
	$"0CC0 0CC0 0CC0 0840 0840 1021 1021 2012",
	$"2012 1021 1021 0840 0840 0CC0 0CC0 0CC0"
	$"0CC0 0CC0 0CC0 0840 0840 1021 1021 2012",
	{7, 4}
};

resource 'CURS' (131, preload) {
	$"8048 4084 4084 2102 2102 3303 3303 3303"
	$"3303 3303 3303 2102 2102 4084 4084 8048",
	$"8048 4084 4084 2102 2102 3303 3303 3303"
	$"3303 3303 3303 2102 2102 4084 4084 8048",
	{7, 4}
};

resource 'CURS' (132, preload) {
	$"0120 0210 0210 8408 8408 CC0C CC0C CC0C"
	$"CC0C CC0C CC0C 8408 8408 0210 0210 0120",
	$"0120 0210 0210 8408 8408 CC0C CC0C CC0C"
	$"CC0C CC0C CC0C 8408 8408 0210 0210 0120",
	{7, 4}
};

resource 'CURS' (133, preload) {
	$"0480 0840 0840 1021 1021 3033 3033 3033"
	$"3033 3033 3033 1021 1021 0840 0840 0480",
	$"0480 0840 0840 1021 1021 3033 3033 3033"
	$"3033 3033 3033 1021 1021 0840 0840 0480",
	{7, 4}
};

resource 'CURS' (201, "Cell") {
	$"0000 0018 0066 0E81 1281 1E81 2281 2466"
	$"4418 4800 8800 9000 D000 E000 C000 80",
	$"0000 0018 007E 0EFF 1EFF 1EFF 3EFF 3C7E"
	$"7C18 7800 F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (292, "Conn") {
	$"0183 006C 0010 0E10 13FF 1FFF 2210 2410"
	$"446C 4983 8800 9000 D000 E000 C000 80",
	$"0183 006C 0010 0E10 1FFF 1FFF 3E10 3C10"
	$"7C6C 7983 F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (291, "Bord") {
	$"0303 00CC 0030 0E30 1230 1E30 2230 2430"
	$"44CC 4B03 8800 9000 D000 E000 C000 80",
	$"0303 00CC 0030 0E30 1E30 1E30 3E30 3C30"
	$"7CCC 7B03 F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (202, "Unit") {
	$"0070 0050 007E 0E5A 127E 1E5A 22FF 2400"
	$"4400 4800 8800 9000 D000 E000 C000 80",
	$"0070 0070 007E 0E7E 1E7E 1E7E 3EFF 3C00"
	$"7C00 7800 F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (138, "shoot") {
	$"07C0 1930 2108 4104 4104 8102 8102 FEFE"
	$"8102 8102 4104 4104 2108 1930 07C0",
	$"07C0 1930 2108 4104 4104 8102 8102 FFFE"
	$"8102 8102 4104 4104 2108 1930 07C0",
	{7, 7}
};

resource 'CURS' (139) {
	$"0000 1CC0 232C 4412 4002 7FFC",
	$"1CC0 3FEC 7FFE FFFF FFFF FFFE 7FFC",
	{5, 7}
};

resource 'CURS' (140) {
	$"0000 1CC0 232C 4412 4002 7FFC 0000 0800"
	$"08",
	$"1CC0 3FEC 7FFE FFFF FFFF FFFE 7FFC",
	{5, 7}
};

resource 'CURS' (141) {
	$"0000 1CC0 232C 4412 4002 7FFC 0000 0800"
	$"0A00 0E00 0A00 02",
	$"1CC0 3FEC 7FFE FFFF FFFF FFFE 7FFC",
	{5, 7}
};

resource 'CURS' (143) {
	$"0000 1CC0 232C 4412 4002 7FFC 0000 0840"
	$"0960 0B20 1F30 1B1C 1216 0232 0666 0444",
	$"1CC0 3FEC 7FFE FFFF FFFF FFFE 7FFC",
	{5, 7}
};

resource 'CURS' (142) {
	$"0000 1CC0 232C 4412 4002 7FFC 0000 0820"
	$"0A20 0E30 0A10 0218 0210 0220",
	$"1CC0 3FEC 7FFE FFFF FFFF FFFE 7FFC",
	{5, 7}
};

resource 'CURS' (144) {
	$"0000 1CC0 232C 4412 4002 7FFC",
	$"1CC0 3FEC 7FFE FFFF FFFF FFFE 7FFC",
	{5, 7}
};

resource 'CURS' (145) {
	$"0000 1CC0 232C 4412 4002 7FFC 0300 0380"
	$"03E0 0338 078C 1C84 3084 2846 4842",
	$"1CC0 3FEC 7FFE FFFF FFFF FFFE 7FFC 07E0"
	$"07F8 07FC 1FFE 3FDE 7DEE 7CEF FCE7 4843",
	{5, 7}
};

resource 'CURS' (146) {
	$"0000 1CC0 232C 4412 4002 7FFC",
	$"1CC0 3FEC 7FFE FFFF FFFF FFFE 7FFC",
	{5, 7}
};

resource 'CURS' (147, "shoot grayed") {
	$"0280 0920 2008 0100 4004 0100 8002 5554"
	$"8002 0100 4004 0100 2008 0920 0280",
	$"0280 0920 2008 0100 4004 0100 8002 5454"
	$"8002 0100 4004 0100 2008 0920 0280",
	{7, 7}
};

resource 'CURS' (148) {
	$"0000 0000 0020 0060 00E0 01E0 01E0 00E0"
	$"0100 0100 0200 0200 0400 04",
	$"0000 0030 0070 00F0 01F0 03F0 03F0 01F0"
	$"03E0 0380 0700 0700 0E00 0E00 0C",
	{2, 10}
};

resource 'CURS' (149) {
	$"0000 0000 0000 0000 0000 0020 0038 3FFC"
	$"0038 0020",
	$"0000 0000 0000 0000 0060 0078 7FFC 7FFE"
	$"7FFC 0078 0060",
	{7, 14}
};

resource 'CURS' (150) {
	$"0000 0000 0400 0400 0200 0200 0100 0100"
	$"00E0 01E0 01E0 00E0 0060 0020",
	$"0000 0C00 0E00 0E00 0700 0700 0380 03E0"
	$"01F0 03F0 03F0 01F0 00F0 0070 0030",
	{13, 10}
};

resource 'CURS' (151) {
	$"0000 0000 0020 0020 0040 0040 0080 0080"
	$"0700 0780 0780 0700 0600 04",
	$"0000 0030 0070 0070 00E0 00E0 01C0 07C0"
	$"0F80 0FC0 0FC0 0F80 0F00 0E00 0C",
	{13, 5}
};

resource 'CURS' (152) {
	$"0000 0000 0000 0000 0000 0400 1C00 3FFC"
	$"1C00 04",
	$"0000 0000 0000 0000 0600 1E00 3FFE 7FFE"
	$"3FFE 1E00 06",
	{7, 2}
};

resource 'CURS' (153) {
	$"0000 0000 0400 0600 0700 0780 0780 0700"
	$"0080 0080 0040 0040 0020 0020",
	$"0000 0C00 0E00 0F00 0F80 0FC0 0FC0 0F80"
	$"07C0 01C0 00E0 00E0 0070 0070 0030",
	{2, 5}
};

resource 'CURS' (154) {
	$"0000 0000 03C0 0240 0240 0240 3E7C 2004"
	$"2004 3E7C 0240 0240 0240 03C0",
	$"0000 0000 03C0 03C0 03C0 03C0 3FFC 3FFC"
	$"3FFC 3FFC 03C0 03C0 03C0 03C0",
	{7, 7}
};

resource 'CURS' (155) {
	$"0820 0820 0440 0EE0 06C0 2288 3838 FD7E"
	$"3838 2288 06C0 0EE0 0440 0820 0820",
	$"0820 0820 0440 0EE0 06C0 2288 3BB8 FFFE"
	$"3BB8 2288 06C0 0EE0 0440 0820 0820",
	{7, 7}
};

resource 'CURS' (203, "People") {
	$"0004 004E 00E4 0E44 124A 1EA0 2200 2400"
	$"4400 4800 8800 9000 D000 E000 C000 80",
	$"00FF 00FF 00FF 0EFF 1EFF 1EFF 3EF0 3C00"
	$"7C00 7800 F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (204, "Material") {
	$"0000 007E 0042 0E42 127E 1E7E 227E 2400"
	$"4400 4800 8800 9000 D000 E000 C000 80",
	$"00FF 00FF 00FF 0EFF 1EFF 1EFF 3EFF 3CFF"
	$"7C00 7800 F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (205, "Feature") {
	$"0000 0010 0010 0E28 1228 1E7C 2244 2444"
	$"4400 4800 8800 9000 D000 E000 C000 80",
	$"0038 0038 007C 0E7C 1EFE 1EFE 3EFE 3CEE"
	$"7CEE 7800 F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (208, "Clouds") {
	$"0000 0060 0090 0E8C 1282 1E7C 2200 2424"
	$"4412 4800 8800 9000 D000 E000 C000 80",
	$"0070 00F8 00FE 0FFF 1FFF 1FFF 3EFE 3CFF"
	$"7C7F 783F F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (206, "Elevation") {
	$"0000 0040 00A8 0F14 1222 1E50 2288 2400"
	$"4400 4800 8800 9000 D000 E000 C000 80",
	$"0040 00E8 01FC 0FFE 1FFF 1FFF 3FFC 3DFC"
	$"7C00 7800 F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (207, "Temperature") {
	$"0000 003C 0024 0E24 1224 1E24 2266 245A"
	$"445A 4866 883C 9000 D000 E000 C000 80",
	$"007E 007E 007E 0E7E 1E7E 1EFF 3EFF 3CFF"
	$"7CFF 78FF F8FF F07E F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (209, "Winds") {
	$"0000 0004 00F2 0E02 12F4 1E02 2272 2404"
	$"4418 4800 8800 9000 D000 E000 C000 80",
	$"000E 01FF 01FF 0FFF 1FFF 1FFF 3FFF 3CFF"
	$"7C3E 783C F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'CURS' (156) {
	$"0000 0000 03C0 0C30 1008 1008 2004 2184"
	$"2184 2004 1008 1008 0C30 03C0",
	$"0000 0000 03C0 0C30 1008 1008 2184 23C4"
	$"23C4 2184 1008 1008 0C30 03C0",
	{7, 7}
};

resource 'CURS' (157) {
	$"0000 4000 2000 5000 2800 5400 2A00 5500"
	$"2A80 5400 2800 4400 0200 0100 02",
	$"C000 E000 F000 F800 FC00 FE00 FF00 FF80"
	$"FFC0 FFE0 FE00 EF00 CF00 8780 0780 0380",
	{0, 0}
};

resource 'CURS' (293, "Coat") {
	$"0000 0028 0054 0EAA 1254 1EAA 2254 2428"
	$"4400 4800 8800 9000 D000 E000 C000 80",
	$"0000 0028 007C 0EFE 1E7C 1EFE 3E7C 3C28"
	$"7C00 7800 F800 F000 F000 E000 C000 80",
	{15, 0}
};

resource 'ALRT' (1000, "Init Warning") {
	{60, 56, 230, 456},
	1000,
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
	/****** Extra bytes follow... ******/
	/* $"300A"                                               /* 0. */
};

resource 'ALRT' (1001, "Init Error") {
	{60, 60, 230, 460},
	1001,
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
	/****** Extra bytes follow... ******/
	/* $"300A"                                               /* 0. */
};

resource 'ALRT' (1002, "Run Warning") {
	{60, 56, 230, 456},
	1002,
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
	/****** Extra bytes follow... ******/
	/* $"300A"                                               /* 0. */
};

resource 'ALRT' (1003, "Run Error") {
	{60, 60, 230, 460},
	1003,
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
	/****** Extra bytes follow... ******/
	/* $"300A"                                               /* 0. */
};

resource 'ALRT' (2000, "Quit Game") {
	{94, 80, 214, 450},
	2000,
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
	/****** Extra bytes follow... ******/
	/* $"300A"                                               /* 0. */
};

resource 'ALRT' (2002, "Out of Game") {
	{94, 80, 211, 434},
	2002,
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
	/****** Extra bytes follow... ******/
	/* $"B00A"                                               /* ∞. */
};

resource 'ALRT' (1004, "Images Missing") {
	{60, 35, 260, 475},
	1004,
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
	/****** Extra bytes follow... ******/
	/* $"300A"                                               /* 0. */
};

resource 'ALRT' (3001, "Confirm Design") {
	{80, 50, 220, 450},
	3001,
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
	/****** Extra bytes follow... ******/
	/* $"300A"                                               /* 0. */
};

resource 'ALRT' (2001, "Resign Game") {
	{94, 80, 214, 450},
	2001,
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
	/****** Extra bytes follow... ******/
	/* $"300A"                                               /* 0. */
};

resource 'ALRT' (3002, "Designer Quit Game") {
	{94, 80, 214, 450},
	3002,
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
	/****** Extra bytes follow... ******/
	/* $"300A"                                               /* 0. */
};

resource 'MBAR' (128) {
	{	/* array MenuArray: 8 elements */
		/* [1] */
		128,
		/* [2] */
		129,
		/* [3] */
		130,
		/* [4] */
		131,
		/* [5] */
		132,
		/* [6] */
		133,
		/* [7] */
		134,
		/* [8] */
		135
	}
};

data 'vers' (1, purgeable) {
	$"0700 8000 0000 0637 2E30 2E38 3824 372E"            /* ..Ä....7.0.88$7. */
	$"302E 3838 2C20 A920 5374 616E 6C65 7920"            /* 0.88, © Stanley  */
	$"542E 2053 6865 6273 2031 3938 362D 3139"            /* T. Shebs 1986-19 */
	$"3935"                                               /* 95 */
};

data 'vers' (2, purgeable) {
	$"0700 8000 0000 0637 2E30 2E38 380C 5863"            /* ..Ä....7.0.88.Xc */
	$"6F6E 7120 372E 302E 3838"                           /* onq 7.0.88 */
};

resource 'PAT#' (128, "Marching Ants", purgeable) {
	{	/* array PatArray: 8 elements */
		/* [1] */
		$"C081 0306 0C18 3060",
		/* [2] */
		$"60C0 8103 060C 1830",
		/* [3] */
		$"3060 C081 0306 0C18",
		/* [4] */
		$"1830 60C0 8103 060C",
		/* [5] */
		$"0C18 3060 C081 0306",
		/* [6] */
		$"060C 1830 60C0 8103",
		/* [7] */
		$"0306 0C18 3060 C081",
		/* [8] */
		$"8103 060C 1830 60C0"
	}
};

resource 'PAT#' (129) {
	{	/* array PatArray: 1 elements */
		/* [1] */
		$"F6ED DBB7 6FDE BD7B"
	}
};

resource 'PAT#' (130) {
	{	/* array PatArray: 4 elements */
		/* [1] */
		$"",
		/* [2] */
		$"4088 0062 1002 2411",
		/* [3] */
		$"C5C2 1833 7220 0C88",
		/* [4] */
		$"FDDF FFBB FFEF FDFF"
	}
};

resource 'PAT#' (131, "Marching Ants", purgeable) {
	{	/* array PatArray: 8 elements */
		/* [1] */
		$"E3C7 8F1F 3E7C F8F1",
		/* [2] */
		$"F1E3 C78F 1F3E 7CF8",
		/* [3] */
		$"F8F1 E3C7 8F1F 3E7C",
		/* [4] */
		$"7CF8 F1E3 C78F 1F3E",
		/* [5] */
		$"3E7C F8F1 E3C7 8F1F",
		/* [6] */
		$"1F3E 7CF8 F1E3 C78F",
		/* [7] */
		$"8F1F 3E7C F8F1 E3C7",
		/* [8] */
		$"C78F 1F3E 7CF8 F1E3"
	}
};

resource 'PAT#' (132, "Marching Ants", purgeable) {
	{	/* array PatArray: 8 elements */
		/* [1] */
		$"C183 070E 1C38 70E0",
		/* [2] */
		$"E0C1 8307 0E1C 3870",
		/* [3] */
		$"70E0 C183 070E 1C38",
		/* [4] */
		$"3870 E0C1 8307 0E1C",
		/* [5] */
		$"1C38 70E0 C183 070E",
		/* [6] */
		$"0E1C 3870 E0C1 8307",
		/* [7] */
		$"070E 1C38 70E0 C183",
		/* [8] */
		$"8307 0E1C 3870 E0C1"
	}
};

data 'XCgm' (128, "intro") {
};

resource 'acur' (128, "read progress") {
	{	/* array CursIdArray: 6 elements */
		/* [1] */
		128,
		/* [2] */
		129,
		/* [3] */
		130,
		/* [4] */
		131,
		/* [5] */
		132,
		/* [6] */
		133
	}
};

resource 'acur' (129, "synth progress", locked, preload) {
	{	/* array CursIdArray: 8 elements */
		/* [1] */
		139,
		/* [2] */
		140,
		/* [3] */
		141,
		/* [4] */
		142,
		/* [5] */
		143,
		/* [6] */
		144,
		/* [7] */
		145,
		/* [8] */
		146
	}
};

resource 'SICN' (2000, "mplayer") {
	{	/* array: 1 elements */
		/* [1] */
		$"0000 1FF8 2004 2FF4 2814 2A94 2814 2A14"
		$"2814 2FF4 2004 2004 20F4 2004 1FF8 1FF8"
	}
};

resource 'SICN' (2001, "hostile") {
	{	/* array: 1 elements */
		/* [1] */
		$"0000 0000 701C 0AA0 0440 3838 1C70 0000"
		$"0000 07C0 1830 2008"
	}
};

resource 'SICN' (2002, "neutral") {
	{	/* array: 1 elements */
		/* [1] */
		$"0000 0000 3C78 0000 0000 3838 1C70 0000"
		$"0000 0000 07C0"
	}
};

resource 'SICN' (2003, "friendly") {
	{	/* array: 1 elements */
		/* [1] */
		$"0000 0000 1C70 2008 0000 1C70 3838 0000"
		$"0000 0000 2008 1830 07C0"
	}
};

resource 'SICN' (3000, "menubutton") {
	{	/* array: 1 elements */
		/* [1] */
		$"FFFE 8003 8403 8603 8703 8783 87C3 87E3"
		$"87C3 8783 8703 8603 8403 8003 FFFF 7FFF"
	}
};

resource 'SICN' (2101, "wind 1") {
	{	/* array: 6 elements */
		/* [1] */
		$"0000 0000 0020 0060 00E0 01E0 01E0 00E0"
		$"0100 0100 0200 0200 0400 04",
		/* [2] */
		$"0000 0000 0000 0000 0000 0020 0038 3FFC"
		$"0038 0020",
		/* [3] */
		$"0000 0000 0400 0400 0200 0200 0100 0100"
		$"00E0 01E0 01E0 00E0 0060 0020",
		/* [4] */
		$"0000 0000 0020 0020 0040 0040 0080 0080"
		$"0700 0780 0780 0700 0600 04",
		/* [5] */
		$"0000 0000 0000 0000 0000 0400 1C00 3FFC"
		$"1C00 04",
		/* [6] */
		$"0000 0000 0400 0600 0700 0780 0780 0700"
		$"0080 0080 0040 0040 0020 0020"
	}
};

resource 'SICN' (2102, "wind 2") {
	{	/* array: 6 elements */
		/* [1] */
		$"0000 0000 0020 0060 00E0 01E0 01E0 00E0"
		$"0180 0180 0300 0300 0600 06",
		/* [2] */
		$"0000 0000 0000 0000 0000 0020 0038 3FFC"
		$"3FFC 0038 0020",
		/* [3] */
		$"0000 0000 0600 0600 0300 0300 0180 0180"
		$"00E0 01E0 01E0 00E0 0060 0020",
		/* [4] */
		$"0000 0000 0060 0060 00C0 00C0 0180 0180"
		$"0700 0780 0780 0700 0600 04",
		/* [5] */
		$"0000 0000 0000 0000 0000 0400 1C00 3FFC"
		$"3FFC 1C00 04",
		/* [6] */
		$"0000 0000 0400 0600 0700 0780 0780 0700"
		$"0180 0180 00C0 00C0 0060 0060"
	}
};

resource 'SICN' (2103, "wind 3") {
	{	/* array: 6 elements */
		/* [1] */
		$"0000 0010 0030 0070 00F0 01F0 01F0 00F0"
		$"01C0 01C0 0380 0380 0700 07",
		/* [2] */
		$"0000 0000 0000 0000 0020 0038 3FFC 3FFE"
		$"3FFC 0038 0020",
		/* [3] */
		$"0000 0000 0700 0700 0380 0380 01C0 01C0"
		$"00F0 01F0 01F0 00F0 0070 0030 0010",
		/* [4] */
		$"0000 0000 00E0 00E0 01C0 01C0 0380 0380"
		$"0F00 0F80 0F80 0F00 0E00 0C00 08",
		/* [5] */
		$"0000 0000 0000 0000 0400 1C00 3FFC 7FFC"
		$"3FFC 1C00 04",
		/* [6] */
		$"0000 0800 0C00 0E00 0F00 0F80 0F80 0F00"
		$"0380 0380 01C0 01C0 00E0 00E0"
	}
};

resource 'SICN' (2104, "wind 4") {
	{	/* array: 6 elements */
		/* [1] */
		$"0000 0030 0070 00F0 01F0 03F0 03F0 01F0"
		$"03C0 03C0 0780 0780 0F00 0F00 0E",
		/* [2] */
		$"0000 0000 0000 0000 0040 0070 7FFC 7FFE"
		$"7FFE 7FFC 0070 0040",
		/* [3] */
		$"0000 0E00 0F00 0F00 0780 0780 03C0 03C0"
		$"01F0 03F0 03F0 01F0 00F0 0070 0030",
		/* [4] */
		$"0000 0070 00F0 00F0 01E0 01E0 03C0 03C0"
		$"0F80 0FC0 0FC0 0F80 0F00 0E00 0C",
		/* [5] */
		$"0000 0000 0000 0000 0200 0E00 3FFE 7FFE"
		$"7FFE 3FFE 0E00 02",
		/* [6] */
		$"0000 0C00 0E00 0F00 0F80 0FC0 0FC0 0F80"
		$"03C0 03C0 01E0 01E0 00F0 00F0 0070"
	}
};

resource 'SICN' (2100, "wind 0") {
	{	/* array: 1 elements */
		/* [1] */
		$"0000 0000 0000 0000 0000 0000 0180 03C0"
		$"03C0 0180"
	}
};

data 'styl' (128) {
	$"0005 0000 0000 000B 0008 0014 0000 0009"            /* ...............∆ */
	$"0000 0000 0000 0000 005A 000C 0009 0014"            /* .........Z...∆.. */
	$"0000 000C 0000 0000 0000 0000 0063 0018"            /* .............c.. */
	$"0012 0014 0000 0018 0000 0000 0000 0000"            /* ................ */
	$"0065 000C 0009 0014 0000 000C 0000 0000"            /* .e...∆.......... */
	$"0000 0000 006C 000B 0008 0014 0000 0009"            /* .....l.........∆ */
	$"0000 0000 0000"                                     /* ...... */
};

resource 'CNTL' (128, "Construct Button") {
	{6, 6, 26, 86},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"Construct"
};

resource 'CNTL' (129, "Research Button") {
	{6, 106, 26, 186},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"Research"
};

resource 'CNTL' (210, "Run Length Popup") {
	{6, 206, 26, 306},
	0,
	visible,
	50,
	210,
	1008,
	0,
	"Run:"
};

resource 'CNTL' (211, "Elevations Popup") {
	{222, 40, 242, 160},
	0,
	visible,
	70,
	211,
	1008,
	0,
	"Elev:"
};

resource 'CNTL' (200, "Sides Popup") {
	{118, 100, 134, 116},
	0,
	visible,
	0,
	200,
	1008,
	0,
	"x"
};

resource 'CNTL' (205, "Unit Types Popup") {
	{82, 100, 98, 116},
	0,
	visible,
	0,
	205,
	1008,
	0,
	"x"
};

resource 'CNTL' (202, "Material Types Popup") {
	{8, 220, 24, 236},
	0,
	visible,
	0,
	202,
	1008,
	0,
	"x"
};

resource 'CNTL' (204, "Terrain Types Popup") {
	{42, 100, 58, 116},
	0,
	visible,
	0,
	204,
	1008,
	0,
	"x"
};

resource 'CNTL' (212, "Features Popup") {
	{148, 100, 164, 116},
	0,
	visible,
	0,
	212,
	1008,
	0,
	"x"
};

resource 'CNTL' (401, "Feature Add Button") {
	{165, 40, 177, 60},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"+"
};

resource 'CNTL' (131, "Topics Button") {
	{5, 50, 25, 100},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"Topics"
};

resource 'CNTL' (132, "Help Button") {
	{5, 105, 25, 155},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"Help"
};

resource 'CNTL' (133, "Prev Button") {
	{5, 160, 25, 210},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"Prev"
};

resource 'CNTL' (134, "Next Button") {
	{5, 215, 25, 265},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"Next"
};

resource 'CNTL' (135, "Back Button") {
	{5, 270, 25, 320},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"Back"
};

resource 'CNTL' (300) {
	{0, 0, 20, 15},
	0,
	visible,
	0,
	0,
	pushButProc,
	0,
	""
};

resource 'CNTL' (403, "Feature Edit Button") {
	{165, 86, 177, 116},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"Edit"
};

resource 'CNTL' (402, "Feature Remove Button") {
	{165, 63, 177, 83},
	0,
	visible,
	1,
	0,
	pushButProc,
	0,
	"-"
};

data 'WDEF' (128, "Infinity Windoid 2.5.1", purgeable) {
	$"4E56 FFFC 48E7 1708 3C2E 000C 2E2E 0008"            /* NVˇ¸HÁ..<....... */
	$"286E 000E 7600 4A46 6712 0C46 0001 670C"            /* (n..v.JFg..F..g. */
	$"0C46 0005 6706 0C46 0006 660E 2F03 4EBA"            /* .F..g..F..f./.N∫ */
	$"0DCC 261F 4A00 6702 7601 1A03 670A 486E"            /* ¬Ã&.J.g.v...g.Hn */
	$"FFFC A874 4EBA 0DE0 3006 6B00 0086 0C40"            /* ˇ¸®tN∫¬‡0.k..Ü.@ */
	$"0006 6E7E D040 303B 0006 4EFB 0000 0032"            /* ..n~–@0;..N˚...2 */
	$"0046 0054 0010 0026 0060 006C 302E 0012"            /* .F.T...&.`.l0... */
	$"48C0 2F00 2F07 2F0C 4EBA 0848 4FEF 000C"            /* H¿/././.N∫.HOÔ.. */
	$"6050 2F07 2F0C 4EBA 08A8 504F 6044 203C"            /* `P/./.N∫.®PO`D < */
	$"0000 FFFF C087 2F00 2F0C 4EBA 0C18 504F"            /* ..ˇˇ¿á/./.N∫..PO */
	$"6030 2F07 2F0C 4EBA 08A6 2C00 504F 6022"            /* `0/./.N∫.¶,.PO`" */
	$"2F07 2F0C 4EBA 0C7A 504F 6016 2F07 2F0C"            /* /./.N∫.zPO`././. */
	$"4EBA 0CFE 504F 600A 2F07 2F0C 4EBA 0CFA"            /* N∫.˛PO`././.N∫.˙ */
	$"504F 4A05 6706 2F2E FFFC A873 2D46 0014"            /* POJ.g./.ˇ¸®s-F.. */
	$"4CEE 10E8 FFE8 4E5E 205F 4FEF 000C 4ED0"            /* LÓ.ËˇËN^ _OÔ..N– */
	$"4E56 0000 48E7 0018 266E 0008 286E 000C"            /* NV..HÁ..&n..(n.. */
	$"206B 0072 2050 5488 224C 22D8 22D8 206B"            /*  k.r PTà"L"ÿ"ÿ k */
	$"0082 2050 4A28 0012 6710 3014 D07C 000D"            /* .Ç PJ(..g.0.–|.¬ */
	$"3940 0004 536C 0006 6010 302C 0002 D07C"            /* 9@..Sl..`.0,..–| */
	$"000D 3940 0006 536C 0004 4CEE 1800 FFF8"            /* .¬9@..Sl..LÓ..ˇ¯ */
	$"4E5E 4E75 4E56 0000 2F0C 286E 0010 206E"            /* N^NuNV../.(n.. n */
	$"000C 224C 22D8 22D8 206E 0008 2068 0082"            /* .."L"ÿ"ÿ n.. h.Ç */
	$"2050 4A28 0012 670E 2F0C 7008 3F00 7203"            /*  PJ(..g./.p.?.r. */
	$"3F01 A8A9 600C 2F0C 7003 3F00 7208 3F01"            /* ?.®©`./.p.?.r.?. */
	$"A8A9 3014 5E40 3940 0004 302C 0002 5E40"            /* ®©0.^@9@..0,..^@ */
	$"3940 0006 286E FFFC 4E5E 4E75 4E56 0000"            /* 9@..(nˇ¸N^NuNV.. */
	$"2F0C 286E 0010 206E 000C 224C 22D8 22D8"            /* /.(n.. n.."L"ÿ"ÿ */
	$"206E 0008 2068 0082 2050 4A28 0012 670E"            /*  n.. h.Ç PJ(..g. */
	$"2F0C 7008 3F00 7203 3F01 A8A9 600C 2F0C"            /* /.p.?.r.?.®©`./. */
	$"7003 3F00 7208 3F01 A8A9 302C 0004 5F40"            /* p.?.r.?.®©0,.._@ */
	$"3880 302C 0006 5F40 3940 0002 286E FFFC"            /* 8Ä0,.._@9@..(nˇ¸ */
	$"4E5E 4E75 4E56 0000 2F0C 286E 0008 302E"            /* N^NuNV../.(n..0. */
	$"000E 670A 5340 670E 5340 6718 604C 7021"            /* ..g.S@g.S@g.`Lp! */
	$"2F00 A862 6044 7001 2F00 2F0C 4EBA 0E5A"            /* /.®b`Dp././.N∫.Z */
	$"504F 6036 206C 0082 2050 4A28 0013 6606"            /* PO`6 l.Ç PJ(..f. */
	$"4A2C 006F 670E 7001 2F00 2F0C 4EBA 0E3A"            /* J,.og.p././.N∫.: */
	$"504F 6016 700A 2F00 7206 2F01 7005 2F00"            /* PO`.p./.r./.p./. */
	$"2F0C 4EBA 0F4E 4FEF 0010 286E FFFC 4E5E"            /* /.N∫.NOÔ..(nˇ¸N^ */
	$"4E75 4E56 0000 2F0C 286E 0008 302E 000E"            /* NuNV../.(n..0... */
	$"670A 5340 670C 5340 6722 6054 4EBA 1130"            /* g.S@g.S@g"`TN∫.0 */
	$"604E 7003 2F00 2F0C 4EBA 0DEE 7004 2F00"            /* `Np././.N∫¬Óp./. */
	$"2F0C 4EBA 0E06 4FEF 0010 6034 4A2E 0013"            /* /.N∫..OÔ..`4J... */
	$"672A 7001 2F00 7206 2F01 7005 2F00 2F0C"            /* g*p./.r./.p././. */
	$"4EBA 0F22 7008 2F00 7206 2F01 7005 2F00"            /* N∫."p./.r./.p./. */
	$"2F0C 4EBA 0EDE 4FEF 0020 6004 4EBA 10E0"            /* /.N∫.ﬁOÔ. `.N∫.‡ */
	$"286E FFFC 4E5E 4E75 4E56 0000 2F0C 286E"            /* (nˇ¸N^NuNV../.(n */
	$"0008 302E 000E 670A 5340 6714 5340 672A"            /* ..0...g.S@g.S@g* */
	$"6050 701E 2F00 A862 7021 2F00 A863 6042"            /* `Pp./.®bp!/.®c`B */
	$"7004 2F00 2F0C 4EBA 0D70 7003 2F00 2F0C"            /* p././.N∫¬pp././. */
	$"4EBA 0D88 4FEF 0010 6028 7008 2F00 7206"            /* N∫¬àOÔ..`(p./.r. */
	$"2F01 7005 2F00 2F0C 4EBA 0EAA 7001 2F00"            /* /.p././.N∫.™p./. */
	$"7206 2F01 7005 2F00 2F0C 4EBA 0E66 4FEF"            /* r./.p././.N∫.fOÔ */
	$"0020 286E FFFC 4E5E 4E75 4E56 FFF8 2F0C"            /* . (nˇ¸N^NuNVˇ¯/. */
	$"286E 0008 206E 0010 43EE FFF8 22D8 22D8"            /* (n.. n..CÓˇ¯"ÿ"ÿ */
	$"486E FFF8 7001 3F00 3F00 A8A9 302E 000E"            /* Hnˇ¯p.?.?.®©0... */
	$"670A 5340 671A 5340 672A 6070 701E 2F00"            /* g.S@g.S@g*`pp./. */
	$"A862 7021 2F00 A863 486E FFF8 A8A1 605C"            /* ®bp!/.®cHnˇ¯®°`\ */
	$"7004 2F00 2F0C 4EBA 0CE0 486E FFF8 A8A1"            /* p././.N∫.‡Hnˇ¯®° */
	$"504F 6048 7004 2F00 720C 2F01 700B 2F00"            /* PO`Hp./.r./.p./. */
	$"2F0C 4EBA 0DEE 41EE 0000 2F20 2F20 4EBA"            /* /.N∫¬ÓAÓ../ / N∫ */
	$"1066 700B 2F00 2F0C 4EBA 0CAE 302E FFFE"            /* .fp././.N∫.Æ0.ˇ˛ */
	$"536E FFFE 302E FFFC 536E FFFC 41EE 0000"            /* Snˇ˛0.ˇ¸Snˇ¸AÓ.. */
	$"2F20 2F20 4EBA 100E 4FEF 0028 286E FFF4"            /* / / N∫..OÔ.((nˇÙ */
	$"4E5E 4E75 4E56 FFF8 48E7 0118 3E2E 000E"            /* N^NuNVˇ¯HÁ..>... */
	$"266E 0010 286E 0008 48C7 2F07 2F0C 4EBA"            /* &n..(n..H«/./.N∫ */
	$"FEC8 2F0B 70FF 3F00 3F00 A8A9 2F0B A8A1"            /* ˛»/.pˇ?.?.®©/.®° */
	$"2F0B 7001 3F00 3F00 A8A9 3007 504F 670C"            /* /.p.?.?.®©0.POg. */
	$"5340 6718 5340 6736 6000 00C6 4EBA 0F70"            /* S@g.S@g6`..∆N∫.p */
	$"2F0B 4EBA 0F7E 584F 6000 00B6 7003 2F00"            /* /.N∫.~XO`..∂p./. */
	$"2F0C 4EBA 0C24 7004 2F00 2F0C 4EBA 0C3C"            /* /.N∫.$p././.N∫.< */
	$"2F0B 4EBA 0F5E 4FEF 0014 6000 0094 700C"            /* /.N∫.^OÔ..`..îp. */
	$"2F00 2F0C 4EBA 0C02 701E 2F00 A863 204B"            /* /./.N∫..p./.®c K */
	$"5088 2F20 2F20 4EBA 0F6C 41EE FFF8 224B"            /* Pà/ / N∫.lAÓˇ¯"K */
	$"20D9 20D9 302E FFF8 526E FFF8 302E FFFA"            /*  Ÿ Ÿ0.ˇ¯Rnˇ¯0.ˇ˙ */
	$"526E FFFA 700B 2F00 2F0C 4EBA 0BCC 486E"            /* Rnˇ˙p././.N∫.ÃHn */
	$"FFF8 A8A1 486E FFF8 7001 3F00 3F00 A8A9"            /* ˇ¯®°Hnˇ¯p.?.?.®© */
	$"700C 2F00 2F0C 4EBA 0BB0 41EE 0000 2F20"            /* p././.N∫.∞AÓ../  */
	$"2F20 4EBA 0F52 302E FFFE 536E FFFE 302E"            /* / N∫.R0.ˇ˛Snˇ˛0. */
	$"FFFC 536E FFFC 7005 2F00 7206 2F01 2F00"            /* ˇ¸Snˇ¸p./.r././. */
	$"2F0C 4EBA 0CAE 486E FFF8 A8A2 4FEF 0038"            /* /.N∫.ÆHnˇ¯®¢OÔ.8 */
	$"4CEE 1880 FFEC 4E5E 4E75 4E56 FFF8 48E7"            /* LÓ.ÄˇÏN^NuNVˇ¯HÁ */
	$"0118 266E 0010 3E2E 000E 286E 0008 2F0B"            /* ..&n..>...(n../. */
	$"48C7 2F07 2F0C 4EBA FECC 41EE FFF8 224B"            /* H«/./.N∫˛ÃAÓˇ¯"K */
	$"20D9 20D9 576E FFFC 576E FFFE 3007 4FEF"            /*  Ÿ ŸWnˇ¸Wnˇ˛0.OÔ */
	$"000C 670A 5340 6706 5340 670A 602A 486E"            /* ..g.S@g.S@g.`*Hn */
	$"FFF8 A8A1 6022 700C 2F00 2F0C 4EBA 0B1A"            /* ˇ¯®°`"p././.N∫.. */
	$"546E FFFA 546E FFF8 41EE 0000 2F20 2F20"            /* Tnˇ˙Tnˇ¯AÓ../ /  */
	$"4EBA 0EB4 4FEF 0010 4CEE 1880 FFEC 4E5E"            /* N∫.¥OÔ..LÓ.ÄˇÏN^ */
	$"4E75 4E56 0000 48E7 0018 266E 0008 286E"            /* NuNV..HÁ..&n..(n */
	$"0010 302E 000E 670A 5340 670C 5340 671A"            /* ..0...g.S@g.S@g. */
	$"6074 2F0C A8A2 606E 7003 2F00 2F0B 4EBA"            /* `t/.®¢`np././.N∫ */
	$"0AC8 2F0C A8A2 504F 605C 7008 2F00 720C"            /* .»/.®¢PO`\p./.r. */
	$"2F01 700B 2F00 2F0B 4EBA 0BD8 2F0C A8A2"            /* /.p././.N∫.ÿ/.®¢ */
	$"7001 2F00 2F0B 4EBA 0AA0 2F0C A8A1 3F2C"            /* p././.N∫.†/.®°?, */
	$"0002 3F14 A893 302C 0006 5340 3F00 302C"            /* ..?.®ì0,..S@?.0, */
	$"0004 5340 3F00 A891 302C 0006 5340 3F00"            /* ..S@?.®ë0,..S@?. */
	$"3F14 A893 3F2C 0002 302C 0004 5340 3F00"            /* ?.®ì?,..0,..S@?. */
	$"A891 4FEF 0018 4CEE 1800 FFF8 4E5E 4E75"            /* ®ëOÔ..LÓ..ˇ¯N^Nu */
	$"4E56 0000 2F0B 266E 0010 226E 000C 4A2E"            /* NV../.&n.."n..J. */
	$"000B 6708 223C 0055 0055 6002 7200 3029"            /* ..g."<.U.U`.r.0) */
	$"0002 0800 0000 6702 E389 3011 0800 0000"            /* ......g.„â0..... */
	$"6702 E189 2681 2741 0004 266E FFFC 4E5E"            /* g.·â&Å'A..&nˇ¸N^ */
	$"4E75 4E56 FFF8 48E7 0018 266E 0008 206E"            /* NuNVˇ¯HÁ..&n.. n */
	$"000C 43EE FFF8 22D8 22D8 598F A8D8 285F"            /* ..CÓˇ¯"ÿ"ÿYè®ÿ(_ */
	$"486E FFF8 70FF 3F00 3F00 A8A9 2F0C 486E"            /* Hnˇ¯pˇ?.?.®©/.Hn */
	$"FFF8 A8DF 2F0B 2F0C 2F0B A8E6 2F0C A8D9"            /* ˇ¯®ﬂ/././.®Ê/.®Ÿ */
	$"4CEE 1800 FFF0 4E5E 4E75 4E56 0000 4E5E"            /* LÓ..ˇN^NuNV..N^ */
	$"4E75 4E56 FFDC 48E7 0338 3C2E 000E 246E"            /* NuNVˇ‹HÁ.8<...$n */
	$"0010 1E2E 0017 266E 0008 598F A8D8 285F"            /* ......&n..Yè®ÿ(_ */
	$"486E FFF0 7000 3F00 3F00 3F00 3F00 A8A7"            /* Hnˇp.?.?.?.?.®ß */
	$"4A07 6710 2F0A 48C6 2F06 2F0B 4EBA FC5C"            /* J.g./.H∆/./.N∫¸\ */
	$"4FEF 000C 7000 1007 2F00 48C6 2F06 2F0B"            /* OÔ..p.../.H∆/./. */
	$"4EBA FB60 41EE FFE8 224A 20D9 20D9 486E"            /* N∫˚`AÓˇË"J Ÿ ŸHn */
	$"FFE8 7001 3F00 3F00 A8A9 4A07 4FEF 000C"            /* ˇËp.?.?.®©J.OÔ.. */
	$"670C 486E FFE8 7001 3F00 3F00 A8A9 2F0C"            /* g.HnˇËp.?.?.®©/. */
	$"486E FFE8 A8DF 486E FFF0 7000 1007 2F00"            /* HnˇË®ﬂHnˇp.../. */
	$"2F0A 48C6 2F06 2F0B 4EBA FF60 558F 486E"            /* /.H∆/./.N∫ˇ`UèHn */
	$"FFF0 A8AE 4A1F 4FEF 0014 661E 598F A8D8"            /* ˇ®ÆJ.OÔ..f.Yè®ÿ */
	$"2D57 FFDC 486E FFF0 A8DF 2F0C 2F2E FFDC"            /* -Wˇ‹Hnˇ®ﬂ/./.ˇ‹ */
	$"2F0C A8E6 2F2E FFDC A8D9 4A07 6720 4A2B"            /* /.®Ê/.ˇ‹®ŸJ.g J+ */
	$"0070 671A 486E FFE0 2F0A 2F0B 4EBA F9B6"            /* .pg.Hnˇ‡/./.N∫˘∂ */
	$"486E FFE0 2F0C 4EBA FECA 4FEF 0014 4A07"            /* Hnˇ‡/.N∫˛ OÔ..J. */
	$"6720 4A2B 0071 671A 486E FFE0 2F0A 2F0B"            /* g J+.qg.Hnˇ‡/./. */
	$"4EBA F9EA 486E FFE0 2F0C 4EBA FEA6 4FEF"            /* N∫˘ÍHnˇ‡/.N∫˛¶OÔ */
	$"0014 486E FFF8 2F0A 7000 1007 2F00 4EBA"            /* ..Hnˇ¯/.p.../.N∫ */
	$"FE50 2F0C 486E FFF8 A8D6 2F0C A8D9 4FEF"            /* ˛P/.Hnˇ¯®÷/.®ŸOÔ */
	$"000C 4CEE 1CC0 FFC8 4E5E 4E75 4E56 FFF8"            /* ..LÓ.¿ˇ»N^NuNVˇ¯ */
	$"2F0C 286E 0008 4A2C 0071 6750 486E FFF8"            /* /.(n..J,.qgPHnˇ¯ */
	$"2F0C 4EBA 0C86 206C 0082 2050 5088 43EE"            /* /.N∫.Ü l.Ç PPàCÓ */
	$"FFF8 20D9 20D9 206C 0082 2050 4A28 0012"            /* ˇ¯ Ÿ Ÿ l.Ç PJ(.. */
	$"504F 670E 302E FFF8 D07C 000C 3D40 FFFC"            /* POg.0.ˇ¯–|..=@ˇ¸ */
	$"600C 302E FFFA D07C 000C 3D40 FFFE 206C"            /* `.0.ˇ˙–|..=@ˇ˛ l */
	$"0082 2050 43EE FFF8 20D9 20D9 286E FFF4"            /* .Ç PCÓˇ¯ Ÿ Ÿ(nˇÙ */
	$"4E5E 4E75 4E56 FFF0 2F0C 286E 0008 206C"            /* N^NuNVˇ/.(n.. l */
	$"0076 2050 43EE FFF0 5488 22D8 22D8 206C"            /* .v PCÓˇTà"ÿ"ÿ l */
	$"0082 2050 43EE FFF8 5088 22D8 22D8 486E"            /* .Ç PCÓˇ¯Pà"ÿ"ÿHn */
	$"FFF8 302E FFFA 48C0 4480 322E FFF2 D240"            /* ˇ¯0.ˇ˙H¿DÄ2.ˇÚ“@ */
	$"3F01 302E FFF8 48C0 4480 322E FFF0 D240"            /* ?.0.ˇ¯H¿DÄ2.ˇ“@ */
	$"3F01 A8A8 558F 486E FFF0 486E FFF8 A8A6"            /* ?.®®UèHnˇHnˇ¯®¶ */
	$"4A1F 6704 7005 6002 7006 286E FFEC 4E5E"            /* J.g.p.`.p.(nˇÏN^ */
	$"4E75 4E56 FFFC 48E7 1118 3E2E 0012 266E"            /* NuNVˇ¸HÁ..>...&n */
	$"0008 422B 0071 7016 A122 2D48 FFFC 6746"            /* ..B+.qp.°"-Hˇ¸gF */
	$"2850 422C 0010 197C 0001 0014 422C 0013"            /* (PB,...|....B,.. */
	$"7601 7001 C047 6608 7002 C047 6602 7600"            /* v.p.¿Gf.p.¿Gf.v. */
	$"4A03 57C3 4403 1943 0012 276E FFFC 0082"            /* J.W√D..C..'nˇ¸.Ç */
	$"422C 0011 3007 C07C 0008 1740 0071 2F0B"            /* B,..0.¿|...@.q/. */
	$"4EBA FECA 584F 4CEE 1888 FFEC 4E5E 4E75"            /* N∫˛ XOLÓ.àˇÏN^Nu */
	$"4E56 0000 2F0C 286E 0008 4AAC 0082 6706"            /* NV../.(n..J¨.Çg. */
	$"206C 0082 A023 286E FFFC 4E5E 4E75 4E56"            /*  l.Ç†#(nˇ¸N^NuNV */
	$"FFD4 48E7 0118 2E2E 000C 286E 0008 2007"            /* ˇ‘HÁ......(n.. . */
	$"4840 48C0 3D40 FFE4 3D47 FFE6 7E00 558F"            /* H@H¿=@ˇ‰=GˇÊ~.Uè */
	$"2F2E FFE4 2F2C 0076 A8E8 4A1F 6706 7E01"            /* /.ˇ‰/,.v®ËJ.g.~. */
	$"6000 010C 486E FFD4 2F0C 4EBA F764 206C"            /* `...Hnˇ‘/.N∫˜d l */
	$"0082 2050 4A28 0014 504F 6700 00F2 558F"            /* .Ç PJ(..POg..ÚUè */
	$"2F2E FFE4 486E FFD4 A8AD 4A1F 6700 00E0"            /* /.ˇ‰Hnˇ‘®≠J.g..‡ */
	$"7E02 206C 0082 2050 4A28 0013 6608 4A2C"            /* ~. l.Ç PJ(..f.J, */
	$"006F 6700 00CA 4A2C 0070 6724 486E FFDC"            /* .og.. J,.pg$Hnˇ‹ */
	$"486E FFD4 2F0C 4EBA F76C 558F 2F2E FFE4"            /* Hnˇ‘/.N∫˜lUè/.ˇ‰ */
	$"486E FFDC A8AD 4A1F 4FEF 000C 6702 7E04"            /* Hnˇ‹®≠J.OÔ..g.~. */
	$"4A2C 0071 6700 0098 486E FFDC 486E FFD4"            /* J,.qg..òHnˇ‹Hnˇ‘ */
	$"2F0C 4EBA F798 558F 2F2E FFE4 486E FFDC"            /* /.N∫˜òUè/.ˇ‰Hnˇ‹ */
	$"A8AD 4A1F 4FEF 000C 6774 2F0C 4EBA FE36"            /* ®≠J.OÔ..gt/.N∫˛6 */
	$"2E00 206C 0076 2050 43EE FFF8 5488 22D8"            /* .. l.v PCÓˇ¯Tà"ÿ */
	$"22D8 206C 0082 2650 204B 5088 43EE FFE8"            /* "ÿ l.Ç&P KPàCÓˇË */
	$"22D8 22D8 41EE FFF0 224B 20D9 20D9 206C"            /* "ÿ"ÿAÓˇ"K Ÿ Ÿ l */
	$"0082 2050 4868 0008 302E FFFA 906E FFEA"            /* .Ç PHh..0.ˇ˙ênˇÍ */
	$"3F00 302E FFF8 906E FFE8 3F00 A8A8 206C"            /* ?.0.ˇ¯ênˇË?.®® l */
	$"0082 2050 2F08 302E FFFA 906E FFF2 3F00"            /* .Ç P/.0.ˇ˙ênˇÚ?. */
	$"302E FFF8 906E FFF0 3F00 A8A8 584F 2007"            /* 0.ˇ¯ênˇ?.®®XO . */
	$"4CEE 1880 FFC8 4E5E 4E75 4E56 FFF0 48E7"            /* LÓ.Äˇ»N^NuNVˇHÁ */
	$"0118 266E 0008 2853 2F0C 2F2E 000C 302E"            /* ..&n..(S/./...0. */
	$"0010 48C0 2F00 302E 0012 48C0 2F00 4EBA"            /* ..H¿/.0...H¿/.N∫ */
	$"07B8 3E00 202B 0004 4FEF 0010 670A 5980"            /* .∏>. +..OÔ..g.YÄ */
	$"6700 012A 6000 0176 701E 2F00 A863 206C"            /* g..*`..vp./.®c l */
	$"0082 2050 4A28 0014 671E 486E FFF0 2F0C"            /* .Ç PJ(..g.Hnˇ/. */
	$"4EBA F5FE 48C7 2F07 2F0C 4EBA F6F8 486E"            /* N∫ı˛H«/./.N∫ˆ¯Hn */
	$"FFF0 A8A1 4FEF 0010 206C 0082 2050 4A28"            /* ˇ®°OÔ.. l.Ç PJ( */
	$"0013 6606 4A2C 006F 6770 206C 0082 2050"            /* ..f.J,.ogp l.Ç P */
	$"4A28 0014 6716 7001 2F00 486E FFF0 48C7"            /* J(..g.p./.HnˇH« */
	$"2F07 2F0C 4EBA FB7C 4FEF 0010 4A2C 0070"            /* /./.N∫˚|OÔ..J,.p */
	$"6720 486E FFF8 486E FFF0 2F0C 4EBA F5F6"            /* g Hnˇ¯Hnˇ/.N∫ıˆ */
	$"486E FFF8 48C7 2F07 2F0C 4EBA F898 4FEF"            /* Hnˇ¯H«/./.N∫¯òOÔ */
	$"0018 4A2C 0071 6738 486E FFF8 486E FFF0"            /* ..J,.qg8Hnˇ¯Hnˇ */
	$"2F0C 4EBA F628 486E FFF8 48C7 2F07 2F0C"            /* /.N∫ˆ(Hnˇ¯H«/./. */
	$"4EBA F988 4FEF 0018 6016 7000 2F00 486E"            /* N∫˘àOÔ..`.p./.Hn */
	$"FFF0 48C7 2F07 2F0C 4EBA FB18 4FEF 0010"            /* ˇH«/./.N∫˚.OÔ.. */
	$"206C 0072 2050 43EE FFF8 5488 22D8 22D8"            /*  l.r PCÓˇ¯Tà"ÿ"ÿ */
	$"302E FFFC 536E FFFC 302E FFFE 536E FFFE"            /* 0.ˇ¸Snˇ¸0.ˇ˛Snˇ˛ */
	$"48C7 2F07 2F0C 4EBA F62C 486E FFF8 A8A1"            /* H«/./.N∫ˆ,Hnˇ¯®° */
	$"486E FFF8 7001 3F00 3F00 A8A8 0C47 0002"            /* Hnˇ¯p.?.?.®®.G.. */
	$"504F 6606 7021 2F00 A862 41EE 0000 2F20"            /* POf.p!/.®bAÓ../  */
	$"2F20 4EBA 0832 504F 6000 00A6 486E FFF0"            /* / N∫.2PO`..¶Hnˇ */
	$"2F0C 4EBA F4EC 486E FFF8 486E FFF0 2F0C"            /* /.N∫ÙÏHnˇ¯Hnˇ/. */
	$"4EBA F532 206C 0082 2050 4A28 0010 4FEF"            /* N∫ı2 l.Ç PJ(..OÔ */
	$"0014 6714 486E FFF8 48C7 2F07 2F0C 4EBA"            /* ..g.Hnˇ¯H«/./.N∫ */
	$"F7C4 4FEF 000C 6068 486E FFF8 48C7 2F07"            /* ˜ƒOÔ..`hHnˇ¯H«/. */
	$"2F0C 4EBA F93E 4FEF 000C 6054 4A2C 0071"            /* /.N∫˘>OÔ..`TJ,.q */
	$"674E 486E FFF0 2F0C 4EBA F496 486E FFF8"            /* gNHnˇ/.N∫ÙñHnˇ¯ */
	$"486E FFF0 2F0C 4EBA F534 206C 0082 2050"            /* Hnˇ/.N∫ı4 l.Ç P */
	$"4A28 0011 4FEF 0014 6714 486E FFF8 48C7"            /* J(..OÔ..g.Hnˇ¯H« */
	$"2F07 2F0C 4EBA F884 4FEF 000C 6012 486E"            /* /./.N∫¯ÑOÔ..`.Hn */
	$"FFF8 48C7 2F07 2F0C 4EBA F8E8 4FEF 000C"            /* ˇ¯H«/./.N∫¯ËOÔ.. */
	$"4EBA 070C 4CEE 1880 FFE4 4E5E 205F 4FEF"            /* N∫..LÓ.Äˇ‰N^ _OÔ */
	$"000C 4ED0 4E56 FFF8 48E7 1108 2E2E 000C"            /* ..N–NVˇ¯HÁ...... */
	$"286E 0008 4A2C 006E 675C 2D4C FFF8 2D47"            /* (n..J,.ng\-Lˇ¯-G */
	$"FFFC 7000 2F00 486E FFF8 487A FDBE 2F2C"            /* ˇ¸p./.Hnˇ¯Hz˝æ/, */
	$"0072 4EBA 0198 2007 4FEF 0010 6738 5980"            /* .rN∫.ò .OÔ..g8YÄ */
	$"6702 601A 206C 0082 2050 4A28 0010 57C3"            /* g.`. l.Ç PJ(..W√ */
	$"4403 206C 0082 2050 1143 0010 6018 206C"            /* D. l.Ç P.C..`. l */
	$"0082 2050 4A28 0011 57C3 4403 206C 0082"            /* .Ç PJ(..W√D. l.Ç */
	$"2050 1143 0011 4CEE 1088 FFEC 4E5E 4E75"            /*  P.C..LÓ.àˇÏN^Nu */
	$"4E56 FFF8 48E7 0018 286E 0008 598F A8D8"            /* NVˇ¯HÁ..(n..Yè®ÿ */
	$"265F 486E FFF8 2F0C 4EBA 0740 2F2C 0076"            /* &_Hnˇ¯/.N∫.@/,.v */
	$"486E FFF8 A8DF 486E FFF8 70FF 3F00 3F00"            /* Hnˇ¯®ﬂHnˇ¯pˇ?.?. */
	$"A8A9 206C 0082 2050 4A28 0014 671A 206C"            /* ®© l.Ç PJ(..g. l */
	$"0082 2050 4A28 0012 6708 046E 000C FFF8"            /* .Ç PJ(..g..n..ˇ¯ */
	$"6006 046E 000C FFFA 2F2C 0072 486E FFF8"            /* `..n..ˇ˙/,.rHnˇ¯ */
	$"A8DF 486E FFF8 7001 3F00 3F00 A8A8 2F0B"            /* ®ﬂHnˇ¯p.?.?.®®/. */
	$"486E FFF8 A8DF 2F0B 2F2C 0072 2F2C 0072"            /* Hnˇ¯®ﬂ/./,.r/,.r */
	$"A8E5 2F0B A8D9 4CEE 1800 FFF0 4E5E 4E75"            /* ®Â/.®ŸLÓ..ˇN^Nu */
	$"4E56 0000 4E5E 4E75 4E56 0000 4E5E 4E75"            /* NV..N^NuNV..N^Nu */
	$"4E56 FFF0 2F03 7600 2F03 7001 41EE FFF0"            /* NVˇ/.v./.p.AÓˇ */
	$"A090 261F 4A40 660A 0C6E 0700 FFF4 6D02"            /* †ê&.J@f..n..ˇÙm. */
	$"7601 1003 262E FFEC 4E5E 4E75 4E56 FFF0"            /* v...&.ˇÏN^NuNVˇ */
	$"2F03 7600 2F03 7001 41EE FFF0 A090 261F"            /* /.v./.p.AÓˇ†ê&. */
	$"4A40 6608 4A2E FFF9 6702 7601 1003 262E"            /* J@f.J.ˇ˘g.v...&. */
	$"FFEC 4E5E 4E75 4E56 FFF8 486E FFF8 A910"            /* ˇÏN^NuNVˇ¯Hnˇ¯©. */
	$"486E FFFC AA48 2F2E FFFC A873 206E FFF8"            /* Hnˇ¸™H/.ˇ¸®s nˇ¯ */
	$"41E8 0030 226E FFFC 43E9 0030 700A A02E"            /* AË.0"nˇ¸CÈ.0p.†. */
	$"206E FFF8 41E8 0042 226E FFFC 43E9 0042"            /*  nˇ¯AË.B"nˇ¸CÈ.B */
	$"700E A02E 206E FFF8 4868 003A A89D 206E"            /* p.†. nˇ¯Hh.:®ù n */
	$"FFF8 4868 0020 A87C 4E5E 4E75 4E56 FFEC"            /* ˇ¯Hh. ®|N^NuNVˇÏ */
	$"48E7 0338 2C2E 0010 4EBA FF46 4A00 6714"            /* HÁ.8,...N∫ˇFJ.g. */
	$"2F2E 0008 2F2E 000C 2F06 2F2E 0014 ABCA"            /* /.../..././...´  */
	$"6000 00CC 4EBA FF56 4A00 6700 00AE 598F"            /* `..ÃN∫ˇVJ.g..ÆYè */
	$"A8D8 2D5F FFF8 598F A8D8 245F 598F A8D8"            /* ®ÿ-_ˇ¯Yè®ÿ$_Yè®ÿ */
	$"265F 2F2E FFF8 A87A 598F AA29 285F 6070"            /* &_/.ˇ¯®zYè™)(_`p */
	$"2054 2D48 FFEC 41E8 0022 43EE FFF0 22D8"            /*  T-HˇÏAË."CÓˇ"ÿ */
	$"22D8 206E FFEC 2068 0016 2050 3E28 0020"            /* "ÿ nˇÏ h.. P>(.  */
	$"486E FFF0 A871 486E FFF4 A871 2F0A 486E"            /* Hnˇ®qHnˇÙ®q/.Hn */
	$"FFF0 A8DF 2F2E 0008 2F0A 2F0B A8E4 2F0B"            /* ˇ®ﬂ/..././.®‰/. */
	$"2F2E FFF8 2F0B A8E4 558F 2F0B A8E2 4A1F"            /* /.ˇ¯/.®‰Uè/.®‚J. */
	$"6616 2F0B A879 3F07 2054 3F28 0014 2F0C"            /* f./.®y?. T?(../. */
	$"2F06 206E 000C 4E90 598F 2F0C AA2B 285F"            /* /. n..NêYè/.™+(_ */
	$"200C 668C 2F2E FFF8 A879 2F2E FFF8 A8D9"            /*  .få/.ˇ¯®y/.ˇ¯®Ÿ */
	$"2F0A A8D9 2F0B A8D9 6014 7001 3F00 7200"            /* /.®Ÿ/.®Ÿ`.p.?.r. */
	$"3F01 7000 2F00 2F06 206E 000C 4E90 4CEE"            /* ?.p././. n..NêLÓ */
	$"1CC0 FFD8 4E5E 4E75 4E56 0000 226E 000C"            /* .¿ˇÿN^NuNV.."n.. */
	$"302E 000A 6B60 0C40 000C 6E5A D040 303B"            /* 0...k`.@..nZ–@0; */
	$"0006 4EFB 0000 001C 0052 0052 0052 001C"            /* ..N˚.....R.R.R.. */
	$"001C 0052 001C 0052 002E 0052 002E 0040"            /* ...R...R...R...@ */
	$"337C FFFF 0004 337C FFFF 0002 32BC FFFF"            /* 3|ˇˇ..3|ˇˇ..2ºˇˇ */
	$"602A 337C CCCC 0002 32BC CCCC 337C FFFF"            /* `*3|ÃÃ..2ºÃÃ3|ˇˇ */
	$"0004 6018 337C 3333 0002 32BC 3333 337C"            /* ..`.3|33..2º333| */
	$"6666 0004 6006 42A9 0002 4251 4E5E 4E75"            /* ff..`.B©..BQN^Nu */
	$"4E56 FFFC 48E7 0308 286E 0010 3E2E 000E"            /* NVˇ¸HÁ..(n..>... */
	$"558F 2F2E 0008 486E FFFC AA42 206E FFFC"            /* Uè/...Hnˇ¸™B nˇ¸ */
	$"2050 2068 0008 2050 3C28 0006 BE46 544F"            /*  P h.. P<(..æFTO */
	$"6F1E 558F 7000 2F00 486E FFFC AA42 206E"            /* o.Uèp./.Hnˇ¸™B n */
	$"FFFC 2050 2068 0008 2050 3C28 0006 544F"            /* ˇ¸ P h.. P<(..TO */
	$"BE46 6F0E 2F0C 48C7 2F07 4EBA FF2C 504F"            /* æFo./.H«/.N∫ˇ,PO */
	$"601C 48C7 206E FFFC 2050 2068 0008 2050"            /* `.H« nˇ¸ P h.. P */
	$"2007 E780 41F0 080A 224C 22D8 32D8 4CEE"            /*  .ÁÄA.."L"ÿ2ÿLÓ */
	$"10C0 FFF0 4E5E 4E75 4E56 FFFA 486E FFFA"            /* .¿ˇN^NuNVˇ˙Hnˇ˙ */
	$"302E 000E 48C0 2F00 2F2E 0008 4EBA FF62"            /* 0...H¿/./...N∫ˇb */
	$"486E FFFA AA14 4E5E 4E75 4E56 FFFA 486E"            /* Hnˇ˙™.N^NuNVˇ˙Hn */
	$"FFFA 302E 000E 48C0 2F00 2F2E 0008 4EBA"            /* ˇ˙0...H¿/./...N∫ */
	$"FF40 486E FFFA AA15 4E5E 4E75 4E56 0000"            /* ˇ@Hnˇ˙™.N^NuNV.. */
	$"48E7 0038 246E 0014 266E 0008 342E 0012"            /* HÁ.8$n..&n..4... */
	$"286E 000C 700F 9042 3400 7000 3013 7200"            /* (n..p.êB4.p.0.r. */
	$"3214 9081 48C2 4C02 0800 4C7C 0800 0000"            /* 2.êÅH¬L...L|.... */
	$"000F 7200 3214 D280 3481 7000 302B 0002"            /* ..r.2.“Ä4Åp.0+.. */
	$"7200 322C 0002 9081 48C2 4C02 0800 4C7C"            /* r.2,..êÅH¬L...L| */
	$"0800 0000 000F 7200 322C 0002 D280 3541"            /* ......r.2,..“Ä5A */
	$"0002 7000 302B 0004 7200 322C 0004 9081"            /* ..p.0+..r.2,..êÅ */
	$"48C2 4C02 0800 4C7C 0800 0000 000F 7200"            /* H¬L...L|......r. */
	$"322C 0004 D280 3541 0004 4CEE 1C00 FFF4"            /* 2,..“Ä5A..LÓ..ˇÙ */
	$"4E5E 4E75 4E56 FFF4 2F0C 286E 0008 486E"            /* N^NuNVˇÙ/.(n..Hn */
	$"FFF4 302E 000E 48C0 2F00 2F0C 4EBA FE82"            /* ˇÙ0...H¿/./.N∫˛Ç */
	$"486E FFFA 302E 0012 48C0 2F00 2F0C 4EBA"            /* Hnˇ˙0...H¿/./.N∫ */
	$"FE70 2F2E 0018 302E 0016 48C0 2F00 486E"            /* ˛p/...0...H¿/.Hn */
	$"FFFA 486E FFF4 4EBA FF24 286E FFF0 4E5E"            /* ˇ˙HnˇÙN∫ˇ$(nˇN^ */
	$"4E75 4E56 FFFA 486E FFFA 302E 0016 48C0"            /* NuNVˇ˙Hnˇ˙0...H¿ */
	$"2F00 302E 0012 48C0 2F00 302E 000E 48C0"            /* /.0...H¿/.0...H¿ */
	$"2F00 2F2E 0008 4EBA FF8C 486E FFFA AA14"            /* /./...N∫ˇåHnˇ˙™. */
	$"4E5E 4E75 4E56 FFFA 486E FFFA 302E 0016"            /* N^NuNVˇ˙Hnˇ˙0... */
	$"48C0 2F00 302E 0012 48C0 2F00 302E 000E"            /* H¿/.0...H¿/.0... */
	$"48C0 2F00 2F2E 0008 4EBA FF5A 486E FFFA"            /* H¿/./...N∫ˇZHnˇ˙ */
	$"AA15 4E5E 4E75 4E56 FFFA 48E7 0F18 266E"            /* ™.N^NuNVˇ˙HÁ..&n */
	$"0018 382E 000E 286E 0008 4246 4247 6046"            /* ..8...(n..BFBG`F */
	$"486E FFFA 48C7 2007 D080 3033 0800 48C0"            /* Hnˇ˙H« .–Ä03..H¿ */
	$"2F00 302E 0012 48C0 2F00 48C4 2F04 2F0C"            /* /.0...H¿/.Hƒ/./. */
	$"4EBA FF12 3A06 598F 486E FFFA AA33 201F"            /* N∫ˇ.:.YèHnˇ˙™3 . */
	$"3C00 4A47 4FEF 0014 6F08 BA46 6604 7000"            /* <.JGOÔ..o.∫Ff.p. */
	$"600C 3007 5247 BE6E 0016 6DB4 7001 4CEE"            /* `.0.RGæn..m¥p.LÓ */
	$"18F0 FFE2 4E5E 4E75 4E56 FFEC 48E7 1718"            /* .ˇ‚N^NuNVˇÏHÁ.. */
	$"3C2E 000A 266E 0014 49EE FFEC 7600 2F03"            /* <...&n..IÓˇÏv./. */
	$"4EBA FB6A 261F 4A00 670A 7001 C06E 000E"            /* N∫˚j&.J.g.p.¿n.. */
	$"6702 7601 1A03 4247 0C46 0004 6D3E 4EBA"            /* g.v...BG.F..m>N∫ */
	$"FB20 4A00 6604 7E01 6032 486E FFF6 700B"            /* ˚ J.f.~.`2Hnˇˆp. */
	$"2F00 2F0B 4EBA FD2A 7000 302E FFF6 4A80"            /* /./.N∫˝*p.0.ˇˆJÄ */
	$"4FEF 000C 6614 7000 302E FFF8 4A80 660A"            /* OÔ..f.p.0.ˇ¯JÄf. */
	$"7000 302E FFFA 4A80 6702 7E02 0C47 0002"            /* p.0.ˇ˙JÄg.~..G.. */
	$"6600 00B0 4A05 6700 00AA 0C46 0008 6E00"            /* f..∞J.g..™.F..n. */
	$"00A2 4247 598F AA32 2D5F FFFC 2F2E 0010"            /* .¢BGYè™2-_ˇ¸/... */
	$"AA31 4254 397C 0007 0002 397C 0008 0004"            /* ™1BT9|....9|.... */
	$"397C 000A 0006 397C 000D 0008 2F0C 7005"            /* 9|....9|.¬../.p. */
	$"2F00 7206 2F01 2F00 2F0B 4EBA FECA 4A00"            /* /.r./././.N∫˛ J. */
	$"4FEF 0014 6756 4254 397C 0001 0002 397C"            /* OÔ..gVBT9|....9| */
	$"0004 0004 2F0C 7003 2F00 7208 2F01 7007"            /* ..../.p./.r./.p. */
	$"2F00 2F0B 4EBA FEA0 4A00 4FEF 0014 672C"            /* /./.N∫˛†J.OÔ..g, */
	$"4254 397C 0004 0002 397C 000F 0004 2F0C"            /* BT9|....9|..../. */
	$"7003 2F00 720C 2F01 700B 2F00 2F0B 4EBA"            /* p./.r./.p././.N∫ */
	$"FE76 4A00 4FEF 0014 6702 7E02 2F2E FFFC"            /* ˛vJ.OÔ..g.~./.ˇ¸ */
	$"AA31 3007 4CEE 18E8 FFD4 4E5E 4E75 4E56"            /* ™10.LÓ.Ëˇ‘N^NuNV */
	$"0000 7021 2F00 A862 701E 2F00 A863 4E5E"            /* ..p!/.®bp./.®cN^ */
	$"4E75 4E56 FFF8 2F0C 286E 0008 41EE FFF8"            /* NuNVˇ¯/.(n..AÓˇ¯ */
	$"224C 20D9 20D9 2F0C A8A1 486E FFF8 7001"            /* "L Ÿ Ÿ/.®°Hnˇ¯p. */
	$"3F00 3F00 A8A9 486E FFF8 A8A3 286E FFF4"            /* ?.?.®©Hnˇ¯®£(nˇÙ */
	$"4E5E 4E75 4E56 0000 302E 000E 536E 000E"            /* N^NuNV..0...Sn.. */
	$"302E 000C 536E 000C 3F2E 000A 3F2E 000C"            /* 0...Sn..?...?... */
	$"A893 2F2E 0008 A891 3F2E 000E 3F2E 0008"            /* ®ì/...®ë?...?... */
	$"A891 4E5E 4E75 4E56 0000 302E 000E 536E"            /* ®ëN^NuNV..0...Sn */
	$"000E 302E 000C 536E 000C 3F2E 000A 3F2E"            /* ..0...Sn..?...?. */
	$"000C A893 2F2E 000C A891 3F2E 000E 3F2E"            /* ..®ì/...®ë?...?. */
	$"0008 A891 4E5E 4E75 4E56 FFFC 2F0C 286E"            /* ..®ëN^NuNVˇ¸/.(n */
	$"000C 486E FFFC A874 2F2E 0008 A873 2F0C"            /* ..Hnˇ¸®t/...®s/. */
	$"7000 3F00 3F00 A880 2F0C A870 2F2E FFFC"            /* p.?.?.®Ä/.®p/.ˇ¸ */
	$"A873 286E FFF8 4E5E 4E75 4E56 FFFC 48E7"            /* ®s(nˇ¯N^NuNVˇ¸HÁ */
	$"0018 266E 000C 286E 0008 41EC 0010 224B"            /* ..&n..(n..AÏ.."K */
	$"22D8 22D8 486E FFFC 2F0C 4EBA FFAC 2F0B"            /* "ÿ"ÿHnˇ¸/.N∫ˇ¨/. */
	$"2F2E FFFC A8A8 4CEE 1800 FFF4 4E5E 4E75"            /* /.ˇ¸®®LÓ..ˇÙN^Nu */
};

resource 'STR#' (128, "Filenames") {
	{	/* array StringArray: 12 elements */
		/* [1] */
		"Xconq Preferences",
		/* [2] */
		":lib:",
		/* [3] */
		":lib-mac:",
		/* [4] */
		":lib-mac:Resources",
		/* [5] */
		":lib-mac:Images",
		/* [6] */
		":lib-mac:Sounds",
		/* [7] */
		"imf.dir",
		/* [8] */
		"news.txt",
		/* [9] */
		"Saved Game",
		/* [10] */
		"Checkpoint",
		/* [11] */
		"Error Save",
		/* [12] */
		"Statistics"
	}
};

data 'TEXT' (128) {
};

data 'LDEF' (128, purgeable) {
	$"600E 0000 4C44 4546 0080 0000 0000 0000"            /* `...LDEF.Ä...... */
	$"41FA FFEE 4E71 4E71 6000 0002 4E56 FFEE"            /* A˙ˇÓNqNq`...NVˇÓ */
	$"48E7 1F30 266E 0008 204B A069 1A00 204B"            /* HÁ.0&n.. K†i.. K */
	$"A029 2453 206A 0050 A069 1800 206A 0050"            /* †)$S j.P†i.. j.P */
	$"A029 206A 0050 2E10 302E 001A 6776 6B74"            /* †) j.P..0...gvkt */
	$"5740 6770 6A6E 5240 6A5C 2F2E 0014 A8A3"            /* W@gpjnR@j\/...®£ */
	$"4A6E 000C 6F4A 3C2E 000C 362A 0012 C7EE"            /* Jn..oJ<...6*..«Ó */
	$"0012 D66A 0002 302A 000E D043 5440 3D40"            /* ..÷j..0*..–CT@=@ */
	$"FFEE 206E 0014 362A 000C D650 486E FFF8"            /* ˇÓ n..6*..÷PHnˇ¯ */
	$"A88B 3F2E FFEE 302E FFF8 D043 3F00 A893"            /* ®ã?.ˇÓ0.ˇ¯–C?.®ì */
	$"4267 A888 7600 2F07 3F2E 000E 3F06 A885"            /* Bg®àv./.?...?.®Ö */
	$"4A2E 0018 670E 4878 0938 42A7 A85F 2F2E"            /* J...g.Hx∆8Bß®_/. */
	$"0014 A8A4 1004 206A 0050 A06A 1005 204B"            /* ..®§.. j.P†j.. K */
	$"A06A 4CDF 0CF8 4E5E 205F 4FEF 0014 4ED0"            /* †jLﬂ.¯N^ _OÔ..N– */
	$"6D61 696E 2020 2020"                                /* main     */
};

