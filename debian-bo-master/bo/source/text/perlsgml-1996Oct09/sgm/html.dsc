<?DTD2HTML -home- >
This document allows you to navigate through the structure of the HTML
DTD.  The element and attribute descriptions contained within this document
were extracted from the document
<a href="http://info.cern.ch/hypertext/WWW/MarkUp/HTML.html">
Hypertext Markup Language (HTML)</a>.
<P>
<?DTD2HTML a >
An anchor is a piece of text which marks the beginning and/or the
end of a hypertext link.
Go <a href="http://info.cern.ch/hypertext/WWW/MarkUp/Elements/A.html">
here</a> for more information on an anchor.
<p>
<strong>Example</strong>:
<pre>
&lt;A HREF="http://info.cern.ch/hypertext/WWW/MarkUp/Elements/A.html"&gt;
</pre>
<P>
<?DTD2HTML a* >
<P>
<?DTD2HTML a*href >
   OPTIONAL. If the HREF attribute is present, the anchor
   is sensitive text: the start of a link. If the reader selects this
   text, (s)he should be presented with another document
   whose network address is defined by the value of the
   HREF attribute.
<P>
<?DTD2HTML a*methods >
   OPTIONAL. The value of this field is a string which if
   present must be a comma separated list of HTTP
   METHODS supported by the object for public use.
<P>
<?DTD2HTML a*name >
   OPTIONAL. If present, the attribute NAME allows the
   anchor to be the destination of a link. The value of the
   attribute is an identifier for the anchor. Identifiers are
   arbitrary strings but must be unique within the HTML
   document. Another document can then make a reference
   explicitly to this anchor by putting the identifier after the
   address, separated by a hash sign.
<P>
<?DTD2HTML a*rel >
   OPTIONAL. An attribute REL may give the relationship
   (s) described by the hypertext link. The value is a
   comma-separated list of relationship values. Values and
   their semantics will be registered by the HTML 
   registration authority . The default relationship if none
   other is given is void. REL should not be present unless
   HREF is present.
<P>
<?DTD2HTML a*rev >
   OPTIONAL. The same as REL , but the semantics of the
   link type are in the reverse direction. A link from A to B
   with REL="X" expresses the same relationship as a link
   from B to A with REV="X". An anchor may have both
   REL and REV attributes.
<P>
<?DTD2HTML a*title >
   OPTIONAL. This is informational only. If present the
   value of this field should equal the value of the TITLE of
   the document whose address is given by the HREF
   attribute.
<P>
<?DTD2HTML a*urn >
   OPTIONAL. If present, this specifies a uniform resource
   number for the document.
<P>
<?DTD2HTML address >
This element is for address information, signatures, authorship, etc,
often at the top or bottom of a document.
<p>
<strong>Example</strong>:
<pre>
&lt;ADDRESS&gt;
John Doe, jdoe@foo.org.com.
&lt;/ADDRESS&gt;
</pre>
<P>
<?DTD2HTML b >
Boldface, where available, otherwise alternative mapping
allowed. 
<P>
<?DTD2HTML base >
This element allows the URL of the document itself to be recorded in
situations in which the document may be read out of context. URLs
within the document may be in a "partial" form relative to this base
address.
<P>
<?DTD2HTML base* >
<P>
<?DTD2HTML base*href >
The URL of the document.
<P>
<?DTD2HTML blockquote >
Block quote.
<P>
<?DTD2HTML body >
The BODY element contains all the information which is part of the
document, as opposed information about the document which is in the
<A HREF="head.html">HEAD</A>.
<P>
<?DTD2HTML cite >
A citation.  Typically italic.
<P>
<?DTD2HTML code >
Example of code. typically monospaced font. (Do not confuse
with <A HREF="pre.html">PRE</A>).
<P>
<?DTD2HTML dd >
Definition text of a term in a definition list
(<A HREF="dl.html">DL</A>).
<P>
<?DTD2HTML dfn >
The defining instance of a term.  Typically bold or bold italic.
<P>
<?DTD2HTML dir >
A list of short elements, typically less than 20 characters.
<P>
<?DTD2HTML dir* >
<P>
<?DTD2HTML dir*compact >
Suggests that a compact rendering be used.
<P>
<?DTD2HTML dl >
A glossary (or definition list).  Apart from glossaries, this element is
useful for presenting a set of named elements to the reader.
<P>
<?DTD2HTML dt >
Definition term in a definition list
(<A HREF="dl.html">DL</A>).
<P>
<?DTD2HTML em >
Emphasis, typically italic.
<P>
<?DTD2HTML h1 >
Level 1 heading.
<P>
<?DTD2HTML h2 >
Level 2 heading.
<P>
<?DTD2HTML h3 >
Level 3 heading.
<P>
<?DTD2HTML h4 >
Level 4 heading.
<P>
<?DTD2HTML h5 >
Level 5 heading.
<P>
<?DTD2HTML h6 >
Level 6 heading.
<P>
<?DTD2HTML head >
The HEAD element contains all information about the document in
general. It does not contain any text which is part of the document: this
is in the <A HREF="body.html">BODY</A>.
<p>
<strong>Example</strong>:
<pre>
&lt;HEAD&gt;
&lt;TITLE&gt;This Is the Title&lt;/TITLE&gt;
&lt;NEXTID N=z20&gt;
&lt;BASE HREF="http://foo.org.com/home/"&gt;
&lt;/HEAD&gt;
</pre>
<P>
<?DTD2HTML hr >
Divider between sections of text such as a full width
horizontal rule or equivalent graphic.
<P>
<?DTD2HTML html >
Topmost element.  Designates the begining of an HTML document.
<P>
<?DTD2HTML i >
Italic font (or slanted if italic unavailable). 
<P>
<?DTD2HTML img >
The IMG element allows another document to be inserted inline.
The document is normally an icon or small graphic, etc. This
element is NOT intended for embedding other HTML text.
<p>
<strong>Example</strong>:
<pre>
&lt;IMG SRC="http://foo.org.com/pics/pic.gif"&gt;
</pre>
<P>
<?DTD2HTML img* >
<P>
<?DTD2HTML img*src >
   The value of this attribute is the URL of the document to be embedded.
   Its syntax is the same as that of the HREF attribute of the 
   <a href="a.html">A</a>
   tag. SRC is mandatory.
<P>
<?DTD2HTML img*align >
   Take values TOP or MIDDLE or BOTTOM, defining
   whether the tops or middles of bottoms of the graphics and
   text should be aligned vertically.
<P>
<?DTD2HTML img*alt >
   Optional alternative text as an alternative to the graphics
   for display in text-only environments.
<P>
<?DTD2HTML isindex >
This element informs the reader that the document is an index
document. As well as reading it, the reader may use a keyword
search.
<P>
<?DTD2HTML kbd >
In an instruction manual, Text typed by a user.
<P>
<?DTD2HTML key >
<P>
<?DTD2HTML li >
A list item.
<P>
<?DTD2HTML link >
The LINK element occurs within the <A HREF="head.html">HEAD</A> element
of an HTML document.
It is used to indicate a relationship between the document and some
other object. A document may have any number of LINK elements. 
<P>
<?DTD2HTML link* >
See <a href="a.attr.html">anchors attributes</a> for a description
of link's attributes.
<P>
<?DTD2HTML link*href >
<P>
<?DTD2HTML link*methods >
<P>
<?DTD2HTML link*name >
<P>
<?DTD2HTML link*rel >
<P>
<?DTD2HTML link*rev >
<P>
<?DTD2HTML link*title >
<P>
<?DTD2HTML link*urn >
<P>
<?DTD2HTML listing >
Example section (obselete).
<P>
<?DTD2HTML menu >
A list of smaller paragraphs. Typically one line per item, with a style
more compact than <A HREF="ul.html">UL</A>.
<P>
<?DTD2HTML menu* >
<P>
<?DTD2HTML menu*compact >
Suggests that a compact rendering be used.
<P>
<?DTD2HTML nextid >
A parameter used by editors to generate unique identifiers.
This tag takes a single attribute which is the number of the next
document-wide numeric identifier to be allocated of the form z123.
<P>
<?DTD2HTML nextid* >
<P>
<?DTD2HTML nextid*n >
Number of the next document.
<P>
<?DTD2HTML ol >
As <A HREF="ul.html">UL</A>, but the paragraphs are typically numbered in some
way to indicate the order as significant.
<P>
<?DTD2HTML ol* >
<P>
<?DTD2HTML ol*compact >
Suggests that a compact rendering be used.
<P>
<?DTD2HTML p >
The empty P element indicates a paragraph break.
<P>
<?DTD2HTML plaintext >
The empty PLAINTEXT tag terminates the HTML entity. What
follows is not SGML. In stead, there's an old HTTP convention
that what follows is an ASCII (MIME "text/plain") body.
<P>
<?DTD2HTML pre >
Section in fixed-width font for preformatted text.
<P>
<?DTD2HTML pre* >
<P>
<?DTD2HTML pre*width >
   This attribute gives the maximum number of characters
   which will occur on a line. It allows the presentation system
   to select a suitable font and indentation. Where the WIDTH
   attribute is not recognized, it is recommended that a width of
   80 be assumed. Where WIDTH is supported, it is
   recommended that at least widths of 40, 80 and 132
   characters be presented optimally, with other widths being
   rounded up.
<P>
<?DTD2HTML samp >
A sequence of literal characters. 
<P>
<?DTD2HTML strong >
Stronger emphasis, typically bold.
<P>
<?DTD2HTML title >
The title of the document.
<P>
<?DTD2HTML tt >
Fixed-width typewriter font. 
<P>
<?DTD2HTML u >
Underline.
<P>
<?DTD2HTML ul >
A list of multi-line paragraphs, typically separated by some
white space and/or marked by bullets, etc.
<P>
<?DTD2HTML ul* >
<P>
<?DTD2HTML ul*compact >
Suggests that a compact rendering be used.
<P>
<?DTD2HTML var >
A variable name.
<P>
<?DTD2HTML xmp >
Example section (obselete).
<P>
