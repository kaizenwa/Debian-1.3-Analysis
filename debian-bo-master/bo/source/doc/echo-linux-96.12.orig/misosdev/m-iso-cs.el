(setq iso-case-table (copy-case-table (standard-case-table)))
(setq iso-syntax-table (make-syntax-table))

(set-case-syntax 160 " " iso-case-table iso-syntax-table)	; NBSP (no-break space)
(set-case-syntax 161 "." iso-case-table iso-syntax-table)	; inverted exclamation mark
(set-case-syntax 162 "w" iso-case-table iso-syntax-table)	; cent sign
(set-case-syntax 163 "w" iso-case-table iso-syntax-table)	; pound sign
(set-case-syntax 164 "w" iso-case-table iso-syntax-table)	; general currency sign
(set-case-syntax 165 "w" iso-case-table iso-syntax-table)	; yen sign
(set-case-syntax 166 "_" iso-case-table iso-syntax-table)	; broken vertical line
(set-case-syntax 167 "w" iso-case-table iso-syntax-table)	; section sign
(set-case-syntax 168 "w" iso-case-table iso-syntax-table)	; diaeresis
(set-case-syntax 169 "_" iso-case-table iso-syntax-table)	; copyright sign
(set-case-syntax 170 "w" iso-case-table iso-syntax-table)	; ordinal indicator, feminine
(set-case-syntax-delims 171 187 iso-case-table iso-syntax-table) ; angle quotation marks
(set-case-syntax 172 "_" iso-case-table iso-syntax-table)	; not sign
(set-case-syntax 173 "_" iso-case-table iso-syntax-table)	; soft hyphen
(set-case-syntax 174 "_" iso-case-table iso-syntax-table)	; registered sign
(set-case-syntax 175 "w" iso-case-table iso-syntax-table)	; macron
(set-case-syntax 176 "_" iso-case-table iso-syntax-table)	; degree sign
(set-case-syntax 177 "_" iso-case-table iso-syntax-table)	; plus or minus sign
(set-case-syntax 178 "w" iso-case-table iso-syntax-table)	; superscript two
(set-case-syntax 179 "w" iso-case-table iso-syntax-table)	; superscript three
(set-case-syntax 180 "w" iso-case-table iso-syntax-table)	; acute accent
(set-case-syntax 181 "_" iso-case-table iso-syntax-table)	; micro sign
(set-case-syntax 182 "w" iso-case-table iso-syntax-table)	; pilcrow
(set-case-syntax 183 "_" iso-case-table iso-syntax-table)	; middle dot
(set-case-syntax 184 "w" iso-case-table iso-syntax-table)	; cedilla
(set-case-syntax 185 "w" iso-case-table iso-syntax-table)	; superscript one
(set-case-syntax 186 "w" iso-case-table iso-syntax-table)	; ordinal indicator, masculine
;;    	       	      187          ; See 171 above.
(set-case-syntax 188 "_" iso-case-table iso-syntax-table)	; fraction one-quarter
(set-case-syntax 189 "_" iso-case-table iso-syntax-table)	; fraction one-half
(set-case-syntax 190 "_" iso-case-table iso-syntax-table)	; fraction three-quarters
(set-case-syntax 191 "." iso-case-table iso-syntax-table)	; inverted question mark
(set-case-syntax-pair 192 224 iso-case-table iso-syntax-table) ; A with grave accent
(set-case-syntax-pair 193 225 iso-case-table iso-syntax-table) ; A with acute accent
(set-case-syntax-pair 194 226 iso-case-table iso-syntax-table) ; A with circumflex accent
(set-case-syntax-pair 195 227 iso-case-table iso-syntax-table) ; A with tilde
(set-case-syntax-pair 196 228 iso-case-table iso-syntax-table) ; A with diaeresis or umlaut mark
(set-case-syntax-pair 197 229 iso-case-table iso-syntax-table) ; A with ring
(set-case-syntax-pair 198 230 iso-case-table iso-syntax-table) ; AE diphthong
(set-case-syntax-pair 199 231 iso-case-table iso-syntax-table) ; C with cedilla
(set-case-syntax-pair 200 232 iso-case-table iso-syntax-table) ; E with grave accent
(set-case-syntax-pair 201 233 iso-case-table iso-syntax-table) ; E with acute accent
(set-case-syntax-pair 202 234 iso-case-table iso-syntax-table) ; E with circumflex accent
(set-case-syntax-pair 203 235 iso-case-table iso-syntax-table) ; E with diaeresis or umlaut mark
(set-case-syntax-pair 204 236 iso-case-table iso-syntax-table) ; I with grave accent
(set-case-syntax-pair 205 237 iso-case-table iso-syntax-table) ; I with acute accent
(set-case-syntax-pair 206 238 iso-case-table iso-syntax-table) ; I with circumflex accent
(set-case-syntax-pair 207 239 iso-case-table iso-syntax-table) ; I with diaeresis or umlaut mark
(set-case-syntax-pair 208 240 iso-case-table iso-syntax-table) ; D with stroke, Icelandic eth
(set-case-syntax-pair 209 241 iso-case-table iso-syntax-table) ; N with tilde
(set-case-syntax-pair 210 242 iso-case-table iso-syntax-table) ; O with grave accent
(set-case-syntax-pair 211 243 iso-case-table iso-syntax-table) ; O with acute accent
(set-case-syntax-pair 212 244 iso-case-table iso-syntax-table) ; O with circumflex accent
(set-case-syntax-pair 213 245 iso-case-table iso-syntax-table) ; O with tilde
(set-case-syntax-pair 214 246 iso-case-table iso-syntax-table) ; O with diaeresis or umlaut mark
(set-case-syntax 215 "_" iso-case-table iso-syntax-table)	; multiplication sign
(set-case-syntax-pair 216 248 iso-case-table iso-syntax-table) ; O with slash
(set-case-syntax-pair 217 249 iso-case-table iso-syntax-table) ; U with grave accent
(set-case-syntax-pair 218 250 iso-case-table iso-syntax-table) ; U with acute accent
(set-case-syntax-pair 219 251 iso-case-table iso-syntax-table) ; U with circumflex accent
(set-case-syntax-pair 220 252 iso-case-table iso-syntax-table) ; U with diaeresis or umlaut mark
(set-case-syntax-pair 221 253 iso-case-table iso-syntax-table) ; Y with acute accent
(set-case-syntax-pair 222 254 iso-case-table iso-syntax-table) ; thorn, Icelandic
(set-case-syntax 223 "w" iso-case-table iso-syntax-table)	; small sharp s, German
(set-case-syntax 247 "_" iso-case-table iso-syntax-table)	; division sign
(set-case-syntax 255 "w" iso-case-table iso-syntax-table)	; small y with diaeresis or umlaut mark

(provide 'm-iso-cs)
