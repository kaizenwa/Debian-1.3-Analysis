"Clipboard*MenuButton*cursor:	hand2",
"Clipboard*Command*cursor:	hand2",
"Clipboard*SimpleMenu*cursor:	hand2",
"Clipboard*Toggle*cursor:	hand2",
"Clipboard*font:			-*-lucida-*-r-*-*-12-*",
"Clipboard*List*font:		-*-lucidatypewriter-medium-r-*-*-14-*",
"Clipboard*Text*font:		-*-lucidatypewriter-medium-r-*-*-14-*",
"Clipboard*iconic:		True",
"Clipboard*quit.label: 			Quit",
"Clipboard*quit.top: 			ChainTop",
"Clipboard*quit.bottom: 			ChainTop",
"Clipboard*quit.left: 			ChainLeft",
"Clipboard*quit.right: 			ChainLeft",
"Clipboard*quit.translations:		#override \\n\
				<Btn1Down>,<Btn1Up>:Quit() unset()",
"Clipboard*delete.label: 			Delete",
"Clipboard*delete.fromHoriz: 		quit",
"Clipboard*delete.top: 			ChainTop",
"Clipboard*delete.bottom:			ChainTop",
"Clipboard*delete.left:			ChainLeft",
"Clipboard*delete.right:			ChainLeft",
"Clipboard*delete.translations:		#override \\n\
				<Btn1Down>,<Btn1Up>:DeleteClip() unset()",
"Clipboard*new.label: 			New",
"Clipboard*new.fromHoriz: 		delete",
"Clipboard*new.top: 			ChainTop",
"Clipboard*new.bottom:			ChainTop",
"Clipboard*new.left:			ChainLeft",
"Clipboard*new.right:			ChainLeft",
"Clipboard*new.translations:		#override \\n\
				<Btn1Down>,<Btn1Up>:NewClip() unset()",
"Clipboard*save.label: 			Save",
"Clipboard*save.fromHoriz: 		new",
"Clipboard*save.top: 			ChainTop",
"Clipboard*save.bottom:			ChainTop",
"Clipboard*save.left:			ChainLeft",
"Clipboard*save.right:			ChainLeft",
"Clipboard*save.translations:		#override \\n\
				<Btn1Down>,<Btn1Up>:Save() unset()",
"Clipboard*next.label: 			Next",
"Clipboard*next.fromHoriz: 		save",
"Clipboard*next.top: 			ChainTop",
"Clipboard*next.bottom:			ChainTop",
"Clipboard*next.left:			ChainLeft",
"Clipboard*next.right:			ChainLeft",
"Clipboard*next.translations:		#override \\n\
				<Btn1Down>,<Btn1Up>:NextClip() unset()",
"Clipboard*prev.label: 			Prev",
"Clipboard*prev.fromHoriz: 		next",
"Clipboard*prev.top: 			ChainTop",
"Clipboard*prev.bottom:			ChainTop",
"Clipboard*prev.left:			ChainLeft",
"Clipboard*prev.right:			ChainLeft",
"Clipboard*prev.translations:		#override \\n\
				<Btn1Down>,<Btn1Up>:PrevClip() unset()",
"Clipboard*index.fromHoriz:		prev",
"Clipboard*index.top: 			ChainTop",
"Clipboard*index.bottom:			ChainTop",
"Clipboard*index.left:			ChainLeft",
"Clipboard*index.right:			ChainLeft",
"Clipboard*index.resizable:		true",
"Clipboard*text.scrollVertical:  		WhenNeeded",
"Clipboard*text.scrollHorizontal:  	WhenNeeded",
"Clipboard*text.autoFill: 		on",
"Clipboard*text.fromVert: 		quit",
"Clipboard*text.top: 			ChainTop",
"Clipboard*text.bottom: 			ChainBottom",
"Clipboard*text.left: 			ChainLeft",
"Clipboard*text.right: 			ChainRight",
"Clipboard*text.resizable:		true",
"Clipboard*text.width:			300",
"Clipboard.geometry:		300x200",
"Clipboard.baseTranslations: #augment\\n\
			<Message>WM_PROTOCOLS: WMProtocols()\\n",
"Clipboard*TransientShell.baseTranslations: #augment\\n\
			<Message>WM_PROTOCOLS: WMProtocols()\\n",
"Clipboard*fileDialog.label:			Save to file:",
"Clipboard*fileDialogShell.allowShellResize:	true",
"Clipboard*fileDialogShell.title:			File Save",
"Clipboard*fileDialog*accept.label:		Accept",
"Clipboard*fileDialog*accept.translations:	#override\
			<BtnUp>: AcceptSave() unset()",
"Clipboard*fileDialog*value.translations:	#override\
			<Key>Return: AcceptSave() \\n\
			Ctrl<Key>S: no-op(ring-bell) \\n\
			Ctrl<Key>R: no-op(ring-bell) \\n\
			Ctrl<Key>M: no-op(ring-bell) \\n\
			Ctrl<Key>J: no-op(ring-bell) \\n\
			Meta<Key>I: no-op(ring-bell)",
"Clipboard*fileDialog*value.baseTranslations:	#override\
			<Key>Return: AcceptSave() \\n\
			Ctrl<Key>S: no-op(ring-bell) \\n\
			Ctrl<Key>R: no-op(ring-bell) \\n\
			Ctrl<Key>M: no-op(ring-bell) \\n\
			Ctrl<Key>J: no-op(ring-bell) \\n\
			Meta<Key>I: no-op(ring-bell)",
"Clipboard*fileDialog*cancel.label:		Cancel",
"Clipboard*fileDialog*cancel.translations:	#override\
			<BtnUp>:CancelSave() unset()",
"Clipboard*failDialog*Label.resizable:		true",
"Clipboard*failDialog.label:			Can't write file",
"Clipboard*failDialogShell.title:			Error",
"Clipboard*failDialogShell.allowShellResize:	true",
"Clipboard*failDialog*continue.label:		Continue",
"Clipboard*failDialog*continue.translations:	#override\
			<BtnUp>:FailContinue() unset()",
"Clipboard*failDialog*value.translations:	#override\
			<Key>Return: FailContinue() \\n\
			Ctrl<Key>S: no-op(ring-bell) \\n\
			Ctrl<Key>R: no-op(ring-bell) \\n\
			Ctrl<Key>M: no-op(ring-bell) \\n\
			Ctrl<Key>J: no-op(ring-bell) \\n\
			Meta<Key>I: no-op(ring-bell)",
