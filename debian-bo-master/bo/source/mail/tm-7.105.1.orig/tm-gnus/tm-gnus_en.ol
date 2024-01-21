『tm-gnus 7.27 Reference manual』
by MORIOKA Tomohiko
$Id: tm-gnus_en.ol,v 1.1 1996/10/05 13:19:25 morioka Exp morioka $

* [Introduction] What is tm-gnus?

tm-gnus is a MIME extender for GNUS using tm.

Note: tm-gnus is for only GNUS. Please use gnus-mime for Gnus.
      (@pxref{(gnus-mime_en.info)})

tm-gnus decodes RFC 1522 MIME encoded-word in summary buffer and
article buffer, and adds a command to preview MIME message using
tm-view in summary mode.

In GNUS 3.15 or later and Gnus, tm-gnus can display MIME preview
buffer instead of article buffer. It is useful for heavy MIME user.


* [Summary buffer] Summary buffer decoding

tm-gnus decodes MIME encoded-word in summary buffer using tm-view
functions. (@pxref{(tm-en.info)encoded-word decoding}) However if an
encoded-word has unsupported charset, it is not decoded.


* [Summary mode] Extension to Summary mode

[@key{M-t}] toggle about MIME processing (*1)

[key{v}] Enter @code{mime/viewer-mode} to view a message

(*1) If running automatic MIME preview support version (namely for
     GNUS 3.15 or later), it is a toggle about automatic MIME
     preview. Otherwise, it is a toggle about decoding of RFC 1522
     MIME encoded-word.

@code{mime/viewer-mode} is a major-mode to view and navigate MIME
message. In this mode, you can move in a message or play a content,
interactively. (@pxref{(tm_en.info)MIME navigation})


* [Automatic MIME Preview] Inline display for MIME message

``Automatic MIME preview'' feature is available for GNUS 3.15 or
later. In automatic MIME preview mode, when reading an article in
summary mode, tm-gnus displays preview buffer processed by tm-view
instead of raw article buffer. (@pxref{(tm_en.info)tm-view})

Therefore if an article is encoded by Quoted-Printable or Base64, a
decoded article is displayed. Or rich text article is automatic
formated. Of course, multipart article is dealt with correctly.

Different from using metamail, speaker does not roar just then read an
article includes audio content, video player does not play just then
read an article includes video content, it does not do anonymous ftp
or send mail when read an article includes external-message. These
contents are played when you do decoding command in preview buffer.

In addition, in XEmacs, images are displayed in preview buffer as same
as text.

However if you use a slow machine, or are just really impatient, you
can stop automatic MIME preview.

[Variable] tm-gnus/automatic-mime-preview

	If it is not-nil, tm-mh-e is in automatic MIME preview mode.
	It is used as initial value of @code{gnus-show-mime} and flag
	whether preview buffer is used or not.


* [MIME-Edit] Composing MIME message

If using mime-setup (@pxref{(tm_en.info)Setting}), you can edit MIME
message in mh-letter-mode using tm-edit. (@pxref{(tm_en.info)tm-edit})

Note: Default setting of mime-setup avoids automatic inserting
      signature when sending a message. Its reason is described at
      @pxref{(tm_en.info)mime-setup}.
