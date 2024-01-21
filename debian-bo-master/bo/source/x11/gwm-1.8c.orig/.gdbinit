set verbose off
define w
where
end
set confirm off

break stop_if_in_dbx
break _wool_error
break wool_break
break GWM_FatalSignalHandler
break XFatalHandler

break _Insight_trap_error
break MLEAK_break
break purify_stop_here

handle 8 stop nopass
define rf
set GWM_re_init_PointerRoot()
end
