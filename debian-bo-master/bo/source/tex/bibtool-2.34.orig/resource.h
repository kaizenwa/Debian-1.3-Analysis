/******************************************************************************
** $Id: resource.h,v 2.21 1996/02/22 21:43:51 gerd Exp gerd $
**=============================================================================
** 
** This file is part of BibTool.
** It is distributed under the GNU General Public License.
** See the file COPYING for details.
** 
** (c) 1996 Gerd Neugebauer
** 
** Net: gerd@informatik.uni-koblenz.de
** 
******************************************************************************/

/*-----------------------------------------------------------------------------
** This file is included several times from different places.
** To avoid inconcistencies by duplication, the information is packed in
** macros which are defined differently depending on the task to perform.
**__________________________________________________________________________ */

#ifndef RSC_FIRST
#define RSC_FIRST(C)
#endif
#ifndef RSC_NEXT
#define RSC_NEXT(C)
#endif

RSC_FIRST('a')
  RscByFct(   "add.field"	      , r_af  ,add_field(val)		    )
RSC_NEXT('b')
  RscString(  "bibtex.env.name"	      , r_ben ,rsc_e_bibtex , RSC_BIBINPUTS ) 
  RscString(  "bibtex.search.path"    , r_bsp ,rsc_v_bibtex 
	    					     ,RSC_BIBINPUTS_DEFAULT ) 
RSC_NEXT('c')
  RscBoolean( "check.double"	      , r_cd  ,rsc_double_check	  , FALSE   )
  RscByFct(   "check.rule"	      , r_cr  ,add_check_rule(val)	    )
  RscBoolean( "check.case.sensitive"  , r_ccs ,rsc_case_check	  ,  TRUE   )
  RscBoolean( "count.all"	      , r_ca  ,rsc_cnt_all	  , FALSE   ) 
  RscBoolean( "count.used"	      , r_cu  ,rsc_cnt_used	  , FALSE   ) 
RSC_NEXT('d')
  RscByFct(   "default.key"	      , r_dk  ,set_separator(0,val)	    )
  RscByFct(   "delete.field"	      , r_df  ,delete_field(val)	    )
  RscString(  "dir.file.separator"    , r_dfs ,rsc_dir_file_sep	  , DIR_SEP )
  RscBoolean( "dump.symbols"	      , r_ds  ,rsc_dump_symbols	  , FALSE   )
RSC_NEXT('e')
  RscString(  "env.separator"	      , r_es  ,rsc_env_sep	  , ENV_SEP ) 
  RscByFct(   "extract.file"	      , r_ef  ,read_aux(val,FALSE)	    )
  RscByFct(   "extract.regex"	      , r_er  ,save_regex(val)		    )
  RscBoolean( "expand.macros"	      , r_em  ,rsc_expand_macros  , FALSE   )
RSC_NEXT('f')
  RscByFct(   "fmt.inter.name"	      , r_fin ,set_separator(1,val)	    )
  RscByFct(   "fmt.name.pre"	      , r_fnp ,set_separator(2,val)	    )
  RscByFct(   "fmt.name.name"	      , r_fnn ,set_separator(3,val)	    )
  RscByFct(   "fmt.name.title"	      , r_fnt ,set_separator(4,val)	    )
  RscByFct(   "fmt.title.title"	      , r_ftt ,set_separator(5,val)	    )
  RscByFct(   "fmt.et.al"	      , r_fea ,set_separator(7,val)	    )
  RscByFct(   "field.type"	      , r_ft  ,set_symbol_type(val)	    )
RSC_NEXT('i')
  RscByFct(   "input"		      , r_i   ,save_input_file(val)	    )
  RscByFct(   "ignored.word"	      , r_iw  ,add_ignored_word(val)	    )
RSC_NEXT('k')
  RscBoolean( "key.generation"	      , r_kg  ,rsc_make_key	  , FALSE   ) 
  RscByFct(   "key.base"	      , r_kb  ,set_base(val)		    )
  RscByFct(   "key.format"	      , r_kf  ,add_format(val)		    )
  RscByFct(   "key.number.separator"  , r_kns ,set_separator(6,val)	    )
  RscBoolean( "key.expand.macros"     , r_kem ,rsc_key_expand_macros,TRUE   )
RSC_NEXT('m')
  RscByFct(   "macro.file"	      , r_mf  ,save_macro_file(val)	    )
RSC_NEXT('n')
  RscByFct(   "new.entry.type"	      , r_net ,def_entry_type(val)	    )
  RscByFct(   "new.field.type"	      , r_nft ,def_field_type(val)	    )
  RscByFct(   "new.format.type"	      , r_nfmt,def_format_type(val)	    )
RSC_NEXT('o')
  RscByFct(   "output.file"	      , r_of  ,save_macro_file(val)	    )
RSC_NEXT('p')
  RscBoolean( "pass.comments"	      , r_pc  ,rsc_pass_comment	  , FALSE   )
  RscBoolean( "preserve.key.case"     , r_pkc ,rsc_key_case	  , FALSE   )
  RscNumeric( "print.align.string"    , r_pas ,rsc_col_s	  ,    18   )
  RscNumeric( "print.align.comment"   , r_pac ,rsc_col_c	  ,    10   )
  RscNumeric( "print.align.preamble"  , r_pap ,rsc_col_p	  ,    11   )
  RscNumeric( "print.align.key"	      , r_pak ,rsc_col_key	  ,    18   ) 
  RscNumeric( "print.align"	      , r_pa  ,rsc_col		  ,    18   )
  RscBoolean( "print.all.strings"     , r_pam ,rsc_all_macs	  ,  TRUE   ) 
  RscBoolean( "print.braces"	      , r_pb  ,rsc_braces	  ,  TRUE   )
  RscNumeric( "print.indent"	      , r_pi  ,rsc_indent	  ,     2   ) 
  RscNumeric( "print.line.length"     , r_pll ,rsc_linelen	  ,    77   )
  RscNumeric( "print.newline"         , r_pnl ,rsc_newlines	  ,     1   )
  RscBoolean( "print.parentheses"     , r_pp  ,rsc_parentheses	  , FALSE   )
  RscBoolean( "print.use.tab"	      , r_put ,rsc_use_tabs	  ,  TRUE   )
  RscByFct(   "print"		      , r_p   ,rsc_print(val)	 	    )
RSC_NEXT('q')
  RscBoolean( "quiet"		      , r_q   ,rsc_quiet	  , FALSE   )
RSC_NEXT('r')
  RscByFct(   "resource"	      , r_r   ,load_rsc(val)		    )
  RscByFct(   "resource.search.path"  , r_rsp ,set_rsc_path(val)	    )
  RscByFct(   "rewrite.rule"	      , r_rr  ,add_rewrite_rule(val)	    )
  RscBoolean( "rewrite.case.sensitive", r_rcs ,rsc_case_rewrite	  ,  TRUE   )
  RscNumeric( "rewrite.limit"	      , r_rl  ,rsc_rewrite_limit  ,   512   )
RSC_NEXT('s')
  RscByFct(   "select"		      , r_sel ,add_extract(val) 	    )
  RscBoolean( "select.case.sensitive" , r_scs ,rsc_case_select	  , FALSE   )
  RscString(  "select.fields"	      , r_self,rsc_sel_fields     , "$key"  ) 
  RscBoolean( "sort"		      , r_s   ,rsc_sort		  , FALSE   )
  RscBoolean( "sort.reverse"	      , r_sr  ,rsc_sort_reverse   , FALSE   )
  RscByFct(   "sort.order"	      , r_so  ,add_sort_order(val)	    )
  RscByFct(   "sort.format"	      , r_sf  ,add_sort_format(val)	    )
  RscByFct(   "symbol.type"	      , r_st  ,set_symbol_type(val)	    )
RSC_NEXT('t')
  RscByFct(   "tex.define"	      , r_td  ,TeX_def(val)		    ) 
RSC_NEXT('v')
  RscBoolean( "verbose"		      , r_v   ,rsc_verbose	  , FALSE   ) 
  RscByFct(   "version"		      , r_ver ,usage(FALSE)		    ) 

#undef RSC_FIRST
#undef RSC_NEXT
#undef RscNumeric
#undef RscString
#undef RscBoolean
#undef RscByFct
