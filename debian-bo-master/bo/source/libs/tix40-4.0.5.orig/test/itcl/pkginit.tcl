setenv env(IWIDGETS_LIBRARY) /home/ioi/dev/itcl2.0/iwidgets2.0.0

@scope :: lappend auto_path $env(IWIDGETS_LIBRARY)
@scope :: source "$env(IWIDGETS_LIBRARY)/init.iwidgets"


