;;; relog - recall into a log
; syntax:  /relog <file> <recall_arguments>
; Starts logging, and silently performs a /recall into the log file.

/~loaded relog.tf

/def -i relog = \
    /def -i -hlog -1 -ag = /echo %%% Recalling to log file %1%;\
    /log %1%;\
    /quote -S /_relog %1 #%-1%;\
    /unset _ARGS

/def -i _relog = \
    /setenv _ARGS=%-1%;\
    /quote -S !echo "$$_ARGS" >> %1

