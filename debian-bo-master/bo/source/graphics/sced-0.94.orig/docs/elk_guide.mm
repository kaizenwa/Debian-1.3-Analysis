.S 12
.TL
Sced-Elk Command Line Interface
.br
User's Guide
.AF "Basser Department Of Computer Science, University of Sydney"
.AU "Stephen Chenney" SJC "" "" "" "" "stephen@cs.su.oz.au"
.MT 4
.SK
.H 1 "SCHEME COMMAND LINE INTERFACE"
.P
If the Elk Scheme extension language has been installed on your system,
Sced may be compiled with support for a scheme command line interface.
The interface allows for commands to be entered textually. It supports
expressions and variables as per the scheme programming language. In fact,
the interface is a scheme interpreter with some special purpose functions
added to interface it with Sced.
.P
With the Elk interface compiled in, another window appears below the view
window in both the Scene and CSG windows. Commands entered in each window
generally apply to the respective view windows, unless explicitly set otherwise.
.P
Scheme commands can be entered in the window below the View Window in either
the Scene or CSG windows. To complete a command, type Shift-Enter. The
interpreter will attempt to parse and execute the commands. You can also
put commands in an external file, then load the file
(using \f2(load "filename")\f1) to execute the commands.
The window is a fully functional Athena text window. That means all the
standard text edit commands work (see Appendix A of the Sced User's Guide).
Shift-Enter actually
works by looking back from the current cursor position for the largest
set of balanced brackets. So you can re-issue a command simply by positioning
the cursor immediately after the last command and hitting Shift-Enter. Note
however that return values will get muddled (they are inserted at the
current cursor position).
.P
Note that there is an error in the latest Elk distribution version 2.2
that renders the
error handling inoperable. Hence at this point an error causes the program to
exit. A patch is available and is distributed in the elk directory. The patch
needs to be applied to the source of the elk-2.2 library on your system, and the
library recompiled. You should then rerun configure for Sced and recompile
Sced.
.P
.H 1 "PRELIMINARIES"
.P
The Sced extension defines 3 new scheme object types. A 3d Object type, a
Viewport
object and a CSG Node object. These may be the result of evaluation of a
scheme expression. The only way to create one of these objects is by
evaluating a creation expression.
.P
Several symbols are also recognized. These shall be introduced as required.
.P
Most functions mirror normal Sced functions available off menus.
.P
Any functions that transform an instance cause the constraint satisfaction
algorithm to be run. So if the given transformation does not satisfy existing
constraints on the object, it will be overridden by the minimal amount.
Dependents will also be updated.
.P
.H 1 "ELK COMMANDS"
.P
In the following sections, examples are shown in \f(CRcourier\f(TR font, with
the corresponding result immediately following in \f(CR\f2italics.\f1\f(TR
.P
.H 2 "Window Functions"
.br
(\f3set-window-csg\f1)
.br
(\f3set-window-scene\f1)
.br
Set the current window to be the CSG or Scene window. Any scheme expression
will apply
to this window, regardless of where it is entered. The function allows
commands for both windows to be placed in the same file for loading. It is
good practice to make use of this expression in any file intended for loading.
.P
(\f3set-window-none\f1)
.br
Unset the current window. Subsequent expressions will be evaluated for the
window in which they are entered.
.P
(\f3get-selectlist\f1)
.br
Create a list containing the selected objects from the window. The list is
returned. Each element is a 3d Object. The list may be processed using
any list manipulation functions, such as \f2for-each\f1.
.P
.H 2 "Object Functions"
.br
(\f3object3d-create \f2class\f1)
.br
Create an object of the given class. \f2Class\f1 must be the name of an
existing base object enclosed in double quotes (a string). The standard
objects are sphere, cube, cone, cylinder,
torus, square and plane. The name of a CSG object is that entered when it is
completed. This function mirrors New off the Object menu. The value returned
is a 3d Object.
.br
\f(CR(define my-sphere (object3d-create "sphere"))
.br
\f2my-sphere\f1\f(TR
.P
(\f3object3d-destroy \f2object\f1)
.br
Destroy an object. \f2Object\f1 must be a 3d Object, as returned by
\f2object3d-create\f1. This mirrors the Delete option off the Object menu.
Nothing is returned.
.br
\f(CR(object3d-destroy my-sphere)\f(TR
.P
(\f3object3d-position \f2x y z object\f1)
.br
Position an object. \f2X, y\f1 and \f2z\f1 must be floats (that means
putting
a . in there somewhere). \f2Object\f1 is a 3d Object. The object is moved
to the absolute location specified. The object itself is returned.
.br
\f(CR(object3d-position 1.0 2.0 3.0 my-sphere)
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-displace \f2x y z object\f1)
.br
Displace an object. \f2X, y\f1 and \f2z\f1 must be floats and \f2object\f1 is a
3d Object. The given vector is added to the objects current displacement to
give a new position. The object itself is returned.
.br
\f(CR(object3d-displace 1.0 2.0 3.0 (object3d-position 0.5 1.0 1.5
my-sphere))
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-scale \f2x y z object\f1)
.br
Scale an object. \f2X, y\f1 and \f2z\f1 must be floats and \f2object\f1 is a 
3d Object. The object is scaled in the x, y and z world directions about
the center of the object by
the respective amount. Note it is NOT scaled along its body axes.
hence you will generally want to scale before rotation. Scaling does
not affect the position of the center of the object. The object itself
is returned.
.br
\f(CR(object3d-scale 2.0 3.0 4.0 my-sphere)
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-rotate \f2x y z object\f1)
.br
Rotate an object about the world axes. \f2X, y\f1 and \f2z\f1 must be floats and
\f2object\f1 is a 3d Object. The object is rotated about the world x axis first,
then y then z. It is probably wise to use only one of x, y or z at a time,
leaving the others as 0. \f2X, y\f1 and \f2z\f1 are in degrees. The object
itself is returned.
.br
\f(CR(object3d-rotate 90.0 0.0 0.0 (object3d-rotate 0.0 0.0 90.0 my-sphere))
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-wireframe-query \f2object\f1)
.br
Query the current wireframe level of an object. Level 0 is the startup level.
Each successive density change increases or decreases the level by one.
The level cannot fall below 0. The current level is returned.
.br
\f(CR(object3d-wireframe-query my-sphere)
.br
\f20\f1\f(TR
.P
(\f3object3d-wireframe-level \f2level object\f1)
.br
Set the wireframe density of an object. \f2Level\f1 is an integer while \f2object\f1
is a 3d Object. Each level corresponds to one Dense Wireframe call. You need
not step up one at a time. The object is returned.
.br
\f(CR(object3d-wireframe-level (+ 2 (object3d-wireframe-query my-sphere)) my-sphere)
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-attribs-define \f2state object\f1)
.br
Define or undefine the attributes of an object. This corresponds to
setting the state of the None toggle in the Attributes dialog for the object.
\f2State\f1 can be anything that returns a value. Most things (including
\f2#t\f1) are true (and define the attributes). \f2#f\f1 is false.
\f2Object\f1 is a 3d Object. The object is returned.
.br
\f(CR(object3d-attribs-define #f my-sphere)
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-color \f2r g b object\f1)
.br
Set the color of an object. \f2R, g\f1 and \f2b\f1 are floats between 0.0 and
1.0, and set the red, green and blue components. \f2Object\f1 is a 3d Object.
The object is returned.
.br
\f(CR(object3d-color 1.0 0.6 0.3 (object3d-attribs-define #t my-sphere))
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-diffuse \f2coef object\f1)
.br
Set the diffuse coefficient of an object. \f2Coef\f1 should be a float between
0.0 and 1.0. \f2Object\f1 is a 3d Object. Object is returned.
.br
\f(CR(object3d-diffuse 0.6 my-sphere)
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-specular \f2coef power object\f1)
.br
Set the specular coefficient and power for the object. Both \f2spec\f1 and
\f2power\f1 are floats. \f2Object\f1 is a 3d Object. Object is returned.
.br
\f(CR(object3d-specular 0.2 15.0 my-sphere)
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-reflect \f2coef object\f1)
.br
Set the coefficient of reflection for the object. \f2Coef\f1 is a float
between 0.0 and 1.0. \f2Object\f1 is a 3d Object. Object is returned.
.br
\f(CR(object3d-reflect 0.5 my-sphere)
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
(\f3object3d-transparency \f2coef refract object\f1)
.br
Set the transparency and refractive index of the object. Both \f2coef\f1 and
\f2refract\f1 are floats. \f2Object\f1 is a 3d Object. Object is returned.
.br
\f(CR(object3d-transparency 0.5 1.5 my-sphere)
.br
\f2#[object3 0x13e200]\f1\f(TR
.P
.H 2 "Viewport Functions"
.br
(\f3viewport-create\f1)
.br
Create a new viewport object. The new viewport is returned.
.br
\f(CR(define new-view (viewport-create))
.br
\f2new-view\f1\f(TR
.P
(\f3viewport-current\f1)
.br
Get a copy of the viewport for the current window. The copy is returned.
.br
\f(CR(define the-view (viewport-current))
.br
\f2the-view\f1\f(TR
.P
(\f3viewport-destroy \f2viewport\f1)
.br
Destroy a viewport. \f2Viewport\f1 is a viewport object as created by
\f2viewport-create\f1.
.br
\f(CR(viewport-destroy new-view)\f(TR
.P
(\f3viewport-lookat \f2x y z viewport\f1)
.br
Set the lookat point of a viewport. \f2X, y\f1 and \f2z\f1 are floats.
\f2Viewport\f1 is a viewport object. The viewport is returned.
.br
\f(CR(viewport-lookat 1.0 2.0 0.0 new-view)
.br
\f2#[viewport 0x327700]\f1\f(TR
.P
(\f3viewport-position \f2x y z viewport\f1)
.br
Set the viewpoint. \f2X, y\f1 and \f2z\f1 are floats. \f2Viewport\f1 is a
viewport object. The viewpoint is set such that you appear to be looking
back along the given vector at the look at point. The viewport is returned.
The given vector need not be a unit vector. It must not be parallel to the
current up vector.
.br
\f(CR(viewport-position 0.0 1.0 0.0 new-view)
.br
\f2#[viewport 0x327700]\f1\f(TR
.P 
(\f3viewport-upvector \f2x y z viewport\f1)
.br
Set the up vector. \f2X, y\f1 and \f2z\f1 are floats and \f2viewport\f1 is a 
viewport object. The given vector is set as the new up vector. This vector
must not be parallel to the current viewpoint vector. The viewport is
returned.
.br
\f(CR(viewport-upvector 1.0 0.0 0.0 new-view)
.br
\f2#[viewport 0x327700]\f1\f(TR
.P
(\f3viewport-distance \f2dist viewport\f1)
.br
Set the viewing distance. \f2Dist\f1 is a float and \f2viewport\f1 is a
viewport object. The viewport is returned.
.br
\f(CR(viewport-distance 10.0 new-view)
.br
\f2#[viewport 0x327700]\f1\f(TR
.P
(\f3viewport-eye \f2dist viewport\f1)
.br
Set the eye or focal distance. \f2Dist\f1 is a float and \f2viewport\f1 is a
viewport object. The viewport is returned.
.br
\f(CR(viewport-eye 100.0 new-view)
.br
\f2#[viewport 0x327700]\f1\f(TR
.P
(\f3viewport-zoom \f2zoom viewport\f1)
.br
Set the zoom for a viewport. \f2Zoom\f1 is an integer. The viewport is
returned.
.br
\f(CR(viewport-zoom 200 new-view)
.br
\f2#[viewport 0x327700]\f1\f(TR
.P
(\f3viewport-setup \f2viewport\f1)
.br
Install the given viewport in the current window. In general this expression
will be the last in a sequence that build a viewport and then install it.
Nothing is returned. The example sets the viewport to a plan view.
.br
\f(CR(viewport-setup (viewport-position 0.0 0.0 1.0 (viewport-upvector 1.0 0.0 0.0 (viewport-create))))\f(TR
.P
(\f3viewport-to-camera \f2viewport\f1)
.br
Set the camera to the given viewport. Nothing is returned.
.br
\f(CR(viewport-to-camera (viewport-current))\f(TR
.P
.H 2 "CSG Functions"
.br
(\f3csg-open \f2instances\f1)
.br
Open the CSG window with the given instances. \f2Instances\f1 is a list
of 3d Objects, such as that returned by \f2get-selectlist\f1. Nothing is
returned. Note that this DOES NOT set the current window to be the CSG window.
You must do that explicitly to work on the instances. If the CSG window
already exists it is brought to the front. The following example creates a
sphere and a cube then places them in the CSG window. Note that the attributes
for the objects are defined \(em the usual dialog regarding attributes is
ignored.
.br
\f(CR(csg-open (list (object3d-create "sphere") (object3d-create "cube")))\f(TR
.P
(\f3csg-node \f2object\f1)
.br
Search for the node corresponding to the given object. \f2Object\f1 is a
3d Object. This allows the node for a selected object to be located, and
is the only means for identifying individual nodes. If a node for the given
object is found, that is returned. Otherwise, nothing is returned.
.br
\f(CR(csg-node (object3d-create "cylinder"))
.br
\f2#[csgnode 0x33bf38]\f1\f(TR
.P
(\f3csg-hide \f2csgnode\f1)
.br
(\f3csg-display \f2csgnode\f1)
.br
Hide or display the given node. \f2Csgnode\f1 is a CSG Node object as
returned by \f2csg-node\f1.
.br
\f(CR(csg-hide (csg-node (object3d-create "cylinder")))
.br
\f2#[csgnode 0x33bf38]\f1\f(TR
.P
(\f3csg-attach \f2left right op\f1)
.br
Attach 2 CSG nodes. \f2Left\f1 and \f2right\f1 are CSG nodes. \f2Op\f1 is
one of the symbols \f(CR\'union, 'intersection\f(TR or \f(CR\'difference.\f(TR
The resulting node is returned.
.br
\f(CR(define tree (csg-attach (csg-node (object3d-create "cube")) (csg-node (object3d-create "cylinder")) 'difference))
.br
\f2tree\f1\f(TR
.P
(\f3csg-complete \f2csgnode name\f1)
.br
Complete the CSG node given. \f2Csgnode\f1 is a CSG Node object. \f2Name\f1
is a string \(em the name of the completed object for later
\f2object3d-create\f1 expressions. The object is given a scaling point
automatically. The name is returned, allowing the example below.
.br
\f(CR(object3d-create (csg-complete tree "hole_in_cube"))
.br
\f2#[object3 0x344f78]\f1\f(TR
.P
.H 2 "Miscellaneous"
.br
(\f3clear\f1)
.br
(\f3reset\f1)
.br
These functions correspond to the items on the Clear menu of the Scene window.
They don't return anything.
.P
(\f3preview \f2renderer instances width height camera\f1)
.br
Preview some objects. \f2Renderer\f1 is one of
the symbols \f(CR\'povray, \'rayshade, \'radiance, \'renderman, \'vrml,
\'genray\f(TR or \f(CR\'genscan.\f(TR \f2Instances\f1 is a list of 3d Objects.
If \f2nil\f1, all the
instances in the window are previewed. \f2Width\f1 and \f2height\f1 are
integers specifying the width and height of the preview image. If 0, the
width or height of the current window is used. \f2Camera\f1 determines whether
the defined camera is used. If the expression evaluates to True (eg \f2#t\f1)
the camera is used, otherwise (eg \f2#f\f1) the current viewport is used.
nothing is returned. The example previews selected instances using the
current viewport and screen size.
.br
\f(CR(preview 'rayshade (get-selectlist) 0 0 #f)\f(TR
.P
(\f3target \f2renderer\f1)
.br
Set the target renderer for export purposes. \f2Renderer\f1 is one of
the symbols \f(CR\'povray, \'rayshade, \'radiance, \'renderman, \'vrml,
\'genray\f(TR or \f(CR\'genscan.\f(TR Case matters. Nothing is returned.
.br
\f(CR(target 'rayshade)\f(TR
.P
(\f3export \f2filename\f1)
.br
Export to the named file. \f2Filename\f1 is a string naming an accessible
(or creatable) file. Nothing is returned. All the instances are exported.
.br
\f(CR(export "animation1.ray")\f(TR
.P
(\f3quit-sced\f1)
.br
Exit Sced. There is no save warning.
.TC
