#!/bin/sh

if [ $# -ne 2 ]; then
	echo usage: $0 directory extension
	exit 1
fi

if ! cd $1; then
	echo $0: can not cd to $1
	exit 1
fi

ext=$2

function fixman {
	if [ $# -lt 2 ]; then
		echo fixman: called with too few args
		exit 1
	fi

	local base=$1
	shift 1

	for f in $*; do
		if ! ln -sf $base.$ext $f.$ext; then
			echo fixman: cant ln $base.$ext to $f.$ext
		fi
	done
}

fixman AddErrInfo Tcl_AddErrorInfo Tcl_SetErrorCode Tcl_PosixError
fixman AllowExc Tcl_AllowExceptions
fixman AppInit Tcl_AppInit
fixman Async Tcl_AsyncCreate Tcl_AsyncMark Tcl_AsyncInvoke Tcl_AsyncDelete
fixman Backslash Tcl_Backslash
fixman CallDel Tcl_CallWhenDeleted Tcl_DontCallWhenDeleted
fixman CmdCmplt Tcl_CommandComplete
fixman Concat Tcl_Concat
fixman CrtCommand Tcl_CreateCommand Tcl_DeleteCommand Tcl_GetCommandInfo Tcl_SetCommandInfo
fixman CrtInterp Tcl_CreateInterp Tcl_DeleteInterp
fixman CrtMathFnc Tcl_CreateMathFunc
fixman CrtPipelin Tcl_CreatePipeline
fixman CrtTrace Tcl_CreateTrace Tcl_DeleteTrace
fixman DString Tcl_DStringInit Tcl_DStringAppend Tcl_DStringAppendElement Tcl_DStringStartSublist Tcl_DStringEndSublist Tcl_DStringLength Tcl_DStringValue Tcl_DStringSetLength Tcl_DStringFree Tcl_DStringResult Tcl_DStringGetResult
fixman DetachPids Tcl_DetachPids Tcl_ReapDetachedProcs
fixman EnterFile Tcl_EnterFile Tcl_GetOpenFile Tcl_FilePermissions
fixman Eval Tcl_Eval Tcl_VarEval Tcl_EvalFile Tcl_GlobalEval
fixman ExprLong Tcl_ExprLong Tcl_ExprDouble Tcl_ExprBoolean Tcl_ExprString
fixman GetInt Tcl_GetInt Tcl_GetDouble Tcl_GetBoolean
fixman Hash Tcl_InitHashTable Tcl_DeleteHashTable Tcl_CreateHashEntry Tcl_DeleteHashEntry Tcl_FindHashEntry Tcl_GetHashValue Tcl_SetHashValue Tcl_GetHashKey Tcl_FirstHashEntry Tcl_NextHashEntry Tcl_HashStats
fixman Interp Tcl_Interp
fixman LinkVar Tcl_LinkVar Tcl_UnlinkVar
fixman PrintDbl Tcl_PrintDouble
fixman RecordEval Tcl_RecordAndEval
fixman RegExp Tcl_RegExpMatch Tcl_RegExpCompile Tcl_RegExpExec Tcl_RegExpRange
fixman SetRecLmt Tcl_SetRecursionLimit
fixman SetResult Tcl_SetResult Tcl_AppendResult Tcl_AppendElement Tcl_ResetResult
fixman SetVar Tcl_SetVar Tcl_SetVar2 Tcl_GetVar Tcl_GetVar2 Tcl_UnsetVar Tcl_UnsetVar2
fixman SplitList Tcl_SplitList Tcl_Merge Tcl_ScanElement Tcl_ConvertElement
fixman StrMatch Tcl_StringMatch
fixman TildeSubst Tcl_TildeSubst
fixman TraceVar Tcl_TraceVar Tcl_TraceVar2 Tcl_UntraceVar Tcl_UntraceVar2 Tcl_VarTraceInfo Tcl_VarTraceInfo2
fixman UpVar Tcl_UpVar Tcl_UpVar2
