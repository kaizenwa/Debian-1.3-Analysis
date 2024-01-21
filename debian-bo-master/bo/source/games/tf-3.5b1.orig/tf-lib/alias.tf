;;; Aliases.
; Create command aliases.  Like simple macros, but no leading '/' is required.
; syntax:  /alias <name> <command...>
; syntax:  /unalias <name>

/~loaded alias.tf

/if ( alias =~ "old" ) \
    /echo Note: you have alias=old, so argument substitutions will follow \
        the old style, where %%1 is the alias name, %%2 is the first \
        argument, etc.%;\
/endif

/def -i alias = \
    /if ( {#} < 2 ) \
        /quote -S /~listalias `/list -i -mglob alias_%{1-*}%; \
    /else \
        /let body=%-1%;\
;       The alias_* macro /shifts unless [alias=~"old"] at runtime.
        /def -i -ag -mglob -h"send {%1}*" alias_%1 = \
            /shift $$[alias !~ "old"]%%; \
            %body%; \
    /endif

; note: the parameters below depend directly on the format of the /def above.
/def -i ~listalias = /echo /alias $[substr({9}, 6)] %-14

/def -i unalias = /undef alias_%1

