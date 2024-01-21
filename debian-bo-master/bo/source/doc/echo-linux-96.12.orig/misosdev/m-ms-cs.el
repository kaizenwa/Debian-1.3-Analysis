(setq ms-case-table (copy-case-table (standard-case-table)))
(setq ms-syntax-table (make-syntax-table))

;;; PC 437 stinks... If accented characters don't have uppercase,they
;;; are not modified. At least, we won't lose information.
(set-case-syntax-pair 128 135 ms-case-table ms-syntax-table) ; ,c
(set-case-syntax-pair 154 129 ms-case-table ms-syntax-table) ; "u
(set-case-syntax-pair 144 130 ms-case-table ms-syntax-table) ; 'e
(if ms850iso
    (set-case-syntax-pair 182 131 ms-case-table ms-syntax-table) ; ^a
  (set-case-syntax 131 "w" ms-case-table ms-syntax-table)) ; inv
(set-case-syntax-pair 142 132 ms-case-table ms-syntax-table) ; "a
(if ms850iso
    (set-case-syntax-pair 183 133 ms-case-table ms-syntax-table) ; `a
  (set-case-syntax 133 "w" ms-case-table ms-syntax-table)) ; inv
(set-case-syntax-pair 143 134 ms-case-table ms-syntax-table) ; a circle
(if ms850iso
    (set-case-syntax-pair 210 136 ms-case-table ms-syntax-table) ; ^e
  (set-case-syntax 136 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 211 137 ms-case-table ms-syntax-table) ; "e
  (set-case-syntax 137 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 212 138 ms-case-table ms-syntax-table) ;`^e
  (set-case-syntax 138 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 216 139 ms-case-table ms-syntax-table) ; "i
  (set-case-syntax 139 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 215 140 ms-case-table ms-syntax-table) ; ^i
  (set-case-syntax 140 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 222 141 ms-case-table ms-syntax-table) ; `i
  (set-case-syntax 141 "w" ms-case-table ms-syntax-table)) ; inv
(set-case-syntax-pair 146 145 ms-case-table ms-syntax-table) ; ae lig.
(if ms850iso
    (set-case-syntax-pair 226 147 ms-case-table ms-syntax-table) ; ^o
  (set-case-syntax 147 "w" ms-case-table ms-syntax-table)) ; inv
(set-case-syntax-pair 153 148 ms-case-table ms-syntax-table) ; "o
(if ms850iso
    (set-case-syntax-pair 227 149 ms-case-table ms-syntax-table) ; `o
  (set-case-syntax 149 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 234 150 ms-case-table ms-syntax-table) ; ^u
  (set-case-syntax 150 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 235 151 ms-case-table ms-syntax-table) ; `u
  (set-case-syntax 151 "w" ms-case-table ms-syntax-table)) ; inv
(set-case-syntax 152 "w" ms-case-table ms-syntax-table)	
(if ms850iso
    (set-case-syntax-pair 157 155 ms-case-table ms-syntax-table) ; o circle
  (set-case-syntax 155 "w" ms-case-table ms-syntax-table)) ; cents
(set-case-syntax 156 "w" ms-case-table ms-syntax-table) ; pounds
(if (not ms850iso)
    (set-case-syntax 157 "w" ms-case-table ms-syntax-table)) ; yen
(set-case-syntax 158 "w" ms-case-table ms-syntax-table) ; Peseta / times
(set-case-syntax 159 "w" ms-case-table ms-syntax-table) ; round f
(if ms850iso
    (set-case-syntax-pair 181 160 ms-case-table ms-syntax-table) ; 'a
  (set-case-syntax 160 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 214 161 ms-case-table ms-syntax-table) ; 'i
  (set-case-syntax 161 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 224 162 ms-case-table ms-syntax-table) ; 'o
  (set-case-syntax 224 "w" ms-case-table ms-syntax-table)) ; inv
(if ms850iso
    (set-case-syntax-pair 233 163 ms-case-table ms-syntax-table) ; 'u
  (set-case-syntax 163 "w" ms-case-table ms-syntax-table)) ; inv
(set-case-syntax-pair 165 164 ms-case-table ms-syntax-table) ; ~n
(set-case-syntax 166 "w" ms-case-table ms-syntax-table)	; a superscript
(set-case-syntax 167 "w" ms-case-table ms-syntax-table)	; o superscript
(set-case-syntax 168 "." ms-case-table ms-syntax-table)	; ? inverted
(set-case-syntax 169 "_" ms-case-table ms-syntax-table)	; fl. log. not/regist.
(set-case-syntax 170 "_" ms-case-table ms-syntax-table)	; Log. not
(set-case-syntax 171 "_" ms-case-table ms-syntax-table)	; 1/2
(set-case-syntax 172 "_" ms-case-table ms-syntax-table)	; 1/4
(set-case-syntax 173 "." ms-case-table ms-syntax-table)	; ! inverted
(set-case-syntax-delims 174 175 ms-case-table ms-syntax-table) ; << >>
(set-case-syntax 176 " " ms-case-table ms-syntax-table)	; Graphics
(set-case-syntax 177 " " ms-case-table ms-syntax-table)	
(set-case-syntax 178 " " ms-case-table ms-syntax-table)	
(set-case-syntax 179 " " ms-case-table ms-syntax-table)	
(set-case-syntax 180 " " ms-case-table ms-syntax-table)	
(if (not ms850iso)
    (progn
      (set-case-syntax 181 " " ms-case-table ms-syntax-table)	
      (set-case-syntax 182 " " ms-case-table ms-syntax-table)	
      (set-case-syntax 183 " " ms-case-table ms-syntax-table)	
      (set-case-syntax 184 " " ms-case-table ms-syntax-table)	
      )
  (set-case-syntax 184 "_" ms-case-table ms-syntax-table)) ; (c)
(set-case-syntax 185 " " ms-case-table ms-syntax-table)	; graphics...
(set-case-syntax 186 " " ms-case-table ms-syntax-table)	
(set-case-syntax 187 " " ms-case-table ms-syntax-table)	
(set-case-syntax 188 " " ms-case-table ms-syntax-table)	
(if ms850iso
    (set-case-syntax 189 "w" ms-case-table ms-syntax-table) ; cents
  (set-case-syntax 189 " " ms-case-table ms-syntax-table)) ; graphics
(if ms850iso
    (set-case-syntax 190 "w" ms-case-table ms-syntax-table)) ; Yen
(set-case-syntax 191 " " ms-case-table ms-syntax-table)	; Graphics
(set-case-syntax 192 " " ms-case-table ms-syntax-table)	
(set-case-syntax 193 " " ms-case-table ms-syntax-table)	
(set-case-syntax 194 " " ms-case-table ms-syntax-table)	
(set-case-syntax 195 " " ms-case-table ms-syntax-table)	
(set-case-syntax 196 " " ms-case-table ms-syntax-table)	
(set-case-syntax 197 " " ms-case-table ms-syntax-table)	
(if (not ms850iso)
    (progn
      (set-case-syntax 198 " " ms-case-table ms-syntax-table)	
      (set-case-syntax 199 " " ms-case-table ms-syntax-table)))
(set-case-syntax 200 " " ms-case-table ms-syntax-table)	; Graphics
(set-case-syntax 201 " " ms-case-table ms-syntax-table)	
(set-case-syntax 202 " " ms-case-table ms-syntax-table)	
(set-case-syntax 203 " " ms-case-table ms-syntax-table)	
(set-case-syntax 204 " " ms-case-table ms-syntax-table)	
(set-case-syntax 205 " " ms-case-table ms-syntax-table)	
(set-case-syntax 206 " " ms-case-table ms-syntax-table)	
(if (not ms850iso)
    (set-case-syntax 207 " " ms-case-table ms-syntax-table)
    (set-case-syntax 208 " " ms-case-table ms-syntax-table)
    (set-case-syntax 209 " " ms-case-table ms-syntax-table)
    (set-case-syntax 210 " " ms-case-table ms-syntax-table)
    (set-case-syntax 211 " " ms-case-table ms-syntax-table)
    (set-case-syntax 212 " " ms-case-table ms-syntax-table)
    (set-case-syntax 213 " " ms-case-table ms-syntax-table)
    (set-case-syntax 214 " " ms-case-table ms-syntax-table)
    (set-case-syntax 215 " " ms-case-table ms-syntax-table)
    (set-case-syntax 216 " " ms-case-table ms-syntax-table)
    (set-case-syntax 221 " " ms-case-table ms-syntax-table)
    (set-case-syntax 222 " " ms-case-table ms-syntax-table)
    )
(if ms850iso
    (progn
      (set-case-syntax-pair 209 208 ms-case-table ms-syntax-table) ; d round
      (set-case-syntax 213 "w" ms-case-table ms-syntax-table) ; 1 superscript
      (set-case-syntax 221 "_" ms-case-table ms-syntax-table))) ; ¦
(set-case-syntax 217 " " ms-case-table ms-syntax-table)
(set-case-syntax 218 " " ms-case-table ms-syntax-table)
(set-case-syntax 219 " " ms-case-table ms-syntax-table)
(set-case-syntax 220 " " ms-case-table ms-syntax-table)
(set-case-syntax 223 " " ms-case-table ms-syntax-table)
(set-case-syntax 225 "w" ms-case-table ms-syntax-table) ; etzet
(set-case-syntax 230 "w" ms-case-table ms-syntax-table) ; mu
(set-case-syntax 240 "_" ms-case-table ms-syntax-table)
(set-case-syntax 241 "_" ms-case-table ms-syntax-table)
(set-case-syntax 242 "_" ms-case-table ms-syntax-table)
(set-case-syntax 244 "_" ms-case-table ms-syntax-table)
(set-case-syntax 245 "_" ms-case-table ms-syntax-table)
(set-case-syntax 246 "_" ms-case-table ms-syntax-table)
(set-case-syntax 247 "_" ms-case-table ms-syntax-table)
(set-case-syntax 248 "_" ms-case-table ms-syntax-table)
(set-case-syntax 249 "_" ms-case-table ms-syntax-table)
(set-case-syntax 250 "_" ms-case-table ms-syntax-table)
(set-case-syntax 251 "w" ms-case-table ms-syntax-table)
(set-case-syntax 252 "w" ms-case-table ms-syntax-table)
(set-case-syntax 253 "w" ms-case-table ms-syntax-table)
(set-case-syntax 254 "_" ms-case-table ms-syntax-table)
(set-case-syntax 255 " " ms-case-table ms-syntax-table)
(set-case-syntax 243 "_" ms-case-table ms-syntax-table)
(if ms850iso
    (progn
      (set-case-syntax-pair 229 228 case-table ms-syntax-table) ; ~o
      (set-case-syntax-pair 232 231 case-table ms-syntax-table) ; p / p
      (set-case-syntax-pair 237 236 case-table ms-syntax-table) ; 'y
      (set-case-syntax 238 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 239 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 247 "w" ms-case-table ms-syntax-table)
      ))
(if (not ms850iso)
    (progn
      (set-case-syntax 224 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 226 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 227 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 228 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 229 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 231 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 232 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 233 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 234 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 235 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 236 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 237 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 238 "w" ms-case-table ms-syntax-table)
      (set-case-syntax 239 "_" ms-case-table ms-syntax-table)
      ))
(provide 'm-ms-cs)
