;; Input methods as an IBus alternative for simple scripts (i.e. not mozc).
;; Currently supports:
;;	- Cyrillic (Russian and Kazakh)
;;	- Turkish
;;	- Greek (Modern)
;;	- Akkadian (out-dated)
;;	- German-esque
;;	- some conlangs

(require 'quail)
(quail-define-package
 "muflax-cyrillic" "Cyrillic (muflax)" "Я" t
 "Support for all Cyrillic-using languages muflax cares about." nil t t nil nil nil nil nil nil nil t)
(quail-define-rules
 ;; straightforward mappings
 ("A"	?А)
 ("B"	?Б)
 ("C"	?Ц)
 ("D"	?Д)
 ("E"	?Э)
 ("F"	?Ф)
 ("G"	?Г)
 ("H"	?Х)
 ("I"	?И)
 ("J"	?Й)
 ("K"	?К)
 ("L"	?Л)
 ("M"	?М)
 ("N"	?Н)
 ("O"	?О)
 ("P"	?П)
 ("R"	?Р)
 ("S"	?С)
 ("T"	?Т)
 ("U"	?У)
 ("V"	?В)
 ("W"	?Ш)
 ("X"	?Х)
 ("Z"	?З)
 ("a"	?а)
 ("b"	?б)
 ("c"	?ц)
 ("d"	?д)
 ("e"	?э)
 ("f"	?ф)
 ("g"	?г)
 ("h"	?х)
 ("i"	?и)
 ("j"	?й)
 ("k"	?к)
 ("l"	?л)
 ("m"	?м)
 ("n"	?н)
 ("o"	?о)
 ("p"	?п)
 ("r"	?р)
 ("s"	?с)
 ("t"	?т)
 ("u"	?у)
 ("v"	?в)
 ("w"	?ш)
 ("x"	?х)
 ("z"	?з)

 ;; fake diacritics
 ("Q"	?Ь)
 ("q"	?ь)
 ("ß"	?ъ)
 ("ẞ"	?Ъ)
 ("Ł"	?Ъ)
 ("ł"	?ъ)

  ;; use y as a prefix for the digraphs
 ("YA"	?Я)
 ("YC"	?Ч)
 ("YE"	?Е)
 ("YI"	?Ы)
 ("YJ"	?Щ)
 ("YO"	?Ё)
 ("YS"	?Ш)
 ("YU"	?Ю)
 ("YZ"	?Ж)
 ("Ya"	?Я)
 ("Yc"	?Ч)
 ("Ye"	?Е)
 ("Yi"	?Ы)
 ("Yj"	?Щ)
 ("Yo"	?Ё)
 ("Ys"	?Ш)
 ("Yu"	?Ю)
 ("Yz"	?Ж)
 ("ya"	?я)
 ("yc"	?ч)
 ("ye"	?е)
 ("yi"	?ы)
 ("yj"	?щ)
 ("yo"	?ё)
 ("ys"	?ш)
 ("yu"	?ю)
 ("yz"	?ж)

 ;; Kazakh letters, use · as a prefix
 ("·C"	?Ҫ)
 ("·E"	?Ә)
 ("·H"	?Һ)
 ("·I"	?Ұ)
 ("·K"	?Ҡ)
 ("·N"	?Ң)
 ("·O"	?Ө)
 ("·Q"	?Қ)
 ("·R"	?Ғ)
 ("·U"	?Ү)
 ("·c"	?ҫ)
 ("·e"	?ә)
 ("·h"	?һ)
 ("·i"	?ұ)
 ("·k"	?ҡ)
 ("·n"	?ң)
 ("·o"	?ө)
 ("·q"	?қ)
 ("·r"	?ғ)
 ("·u"	?ү)

 ;; make the · typable by itself
 ("··"	?·)
)

(quail-define-package
 "muflax-turkish" "Turkish (muflax)" "ı" t
 "Turkish alphabet." nil t t nil nil nil nil nil nil nil t)
(quail-define-rules
 ;; straightforward
 ("a"	?a)
 ("A"	?A)
 ("b"	?b)
 ("B"	?B)
 ("d"	?d)
 ("d"	?d)
 ("E"	?E)
 ("e"	?e)
 ("F"	?F)
 ("f"	?f)
 ("G"	?G)
 ("g"	?g)
 ("H"	?H)
 ("h"	?h)
 ("K"	?K)
 ("k"	?k)
 ("L"	?L)
 ("l"	?l)
 ("M"	?M)
 ("m"	?m)
 ("N"	?N)
 ("n"	?n)
 ("O"	?O)
 ("o"	?o)
 ("P"	?P)
 ("p"	?p)
 ("R"	?R)
 ("r"	?r)
 ("S"	?S)
 ("s"	?s)
 ("T"	?T)
 ("t"	?t)
 ("U"	?U)
 ("u"	?u)
 ("V"	?V)
 ("v"	?v)
 ("Z"	?Z)
 ("z"	?z)

 ;; ǧ is ok as a replacement for ğ, but here's a key for convenience's sake
 ("ß"	?ğ)
 ("ẞ"	?Ğ)

 ;; questionable choices
 ("c"	?c)
 ("C"	?C)
 ("j"	?j)
 ("J"	?J)
 ("y"	?y)
 ("Y"	?Y)

 ;; Pinyin to the rescue
 ("q"	?ç)
 ("Q"	?Ç)
 ("x"	?ş)
 ("X"	?Ş)

 ;; dotless i madness
 ("I"	?İ)
 ("i"	?i)
 ("W"	?I)
 ("w"	?ı)
 )

(quail-define-package
  "muflax-greek" "Greek (muflax)" "ω" t
  "Support for all Greek-using languages muflax cares about." nil t t nil nil nil nil nil nil nil t)
(quail-define-rules
 ("A"	?Α)
 ("B"	?Β)
 ("C"	?Χ)
 ("D"	?Δ)
 ("E"	?Ε)
 ("F"	?Φ)
 ("G"	?Γ)
 ("I"	?Ι)
 ("J"	?Η)
 ("K"	?Κ)
 ("L"	?Λ)
 ("M"	?Μ)
 ("N"	?Ν)
 ("O"	?Ο)
 ("P"	?Π)
 ("Q"	?Θ)
 ("ẞ"	?Θ)
 ("R"	?Ρ)
 ("S"	?Σ)
 ("T"	?Τ)
 ("U"	?Υ)
 ("V"	?Ϝ)
 ("W"	?Ω)
 ("X"	?Ξ)
 ("Y"	?Ψ)
 ("Z"	?Ζ)
 ("a"	?α)
 ("b"	?β)
 ("c"	?χ)
 ("d"	?δ)
 ("e"	?ε)
 ("f"	?φ)
 ("g"	?γ)
 ("i"	?ι)
 ("j"	?η)
 ("k"	?κ)
 ("l"	?λ)
 ("m"	?μ)
 ("n"	?ν)
 ("o"	?ο)
 ("p"	?π)
 ("q"	?θ)
 ("ß"	?θ)
 ("r"	?ρ)
 ("s"	?ς)
 ("t"	?τ)
 ("u"	?υ)
 ("v"	?ϝ)
 ("w"	?ω)
 ("x"	?ξ)
 ("y"	?ψ)
 ("z"	?ζ)
 )

(quail-define-package
 "akkadian" "Akkadian" "𒀸" nil
 "A scheme for Akkadian cuneiform." nil t t t t nil nil nil nil nil t)
(quail-define-rules
 ("a"     	?𒀀)
 ("a2"    	?𒀉)
 ("e"     	?𒂊)
 ("e2"    	?𒂍)
 ("i"     	?𒄿)
 ("i2"    	?𒐊)
 ("u"     	?𒌋)
 ("i2"    	?𒌑)
 ("ba"    	?𒁀)
 ("ba2"   	?𒉺)
 ("ba3"   	?𒂠)
 ("be"    	?𒁁)
 ("be2"   	?𒁉)
 ("be3"   	?𒉌)
 ("bi"    	?𒁉)
 ("bi2"   	?𒉈)
 ("bi3"   	?𒉿)
 ("bu"    	?𒁍)
 ("bu2"   	?𒆜)
 ("bu3"   	?𒅤)
 ("da"    	?𒁕)
 ("da2"   	?𒋫)
 ("de"    	?𒁲)
 ("de3"   	?𒉈)
 ("di"    	?𒁲)
 ("di2"   	?𒄭)
 ("du"    	?𒁺)
 ("du2"   	?𒌅)
 ("du3"   	?𒆕)
 ("du4"   	?𒌈)
 ("ga"    	?𒂵)
 ("ga2"   	?𒂷)
 ("ge"    	?𒄀)
 ("ge2"   	?𒆤)
 ("ge3"   	?𒁹)
 ("gi"    	?𒄀)
 ("gi2"   	?𒆤)
 ("gi3"   	?𒁹)
 ("gi4"   	?𒄄)
 ("gi5"   	?𒆠)
 ("gu"    	?𒄖)
 ("gu2"   	?𒄘)
 ("gu3"   	?𒅗)
 ("gu4"   	?𒄞)
 ("gu5"   	?𒆪)
 ("gu6"   	?𒅘)
 ("gu7"   	?𒅥)
 ("ha"    	?𒄩)
 ("ha2"   	["𒄭𒀀"])
 ("ha3"   	?𒌋)
 ("ha4"   	?𒄭)
 ("he"    	?𒄭)
 ("he2"   	?𒃶)
 ("hi"    	?𒄭)
 ("hi2"   	?𒃶)
 ("hu"    	?𒄷)
 ("ka"    	?𒅗)
 ("ka2"   	?𒆍)
 ("ka3"   	?𒂵)
 ("ke"    	?𒆠)
 ("ke2"   	?𒄀)
 ("ki"    	?𒆠)
 ("ki2"   	?𒄀)
 ("ku"    	?𒆪)
 ("ku2"   	?𒅥)
 ("ku3"   	?𒆬)
 ("ku4"   	?𒆭)
 ("la"    	?𒆷)
 ("la2"   	?𒇲)
 ("la3"   	?𒉡)
 ("le"    	?𒇷)
 ("le2"   	?𒉌)
 ("li"    	?𒇷)
 ("li2"   	?𒉌)
 ("lu"    	?𒇻)
 ("lu2"   	?𒇽)
 ("ma"    	?𒈠)
 ("ma2"   	?𒈣)
 ("me"    	?𒈨)
 ("me2"   	?𒈪)
 ("me3"   	?𒀞)
 ("me3"   	?𒅠)
 ("mi"    	?𒈪)
 ("mi2"   	?𒊩)
 ("mi3"   	?𒈨)
 ("mu"    	?𒈬)
 ("mu2"   	?𒊬)
 ("na"    	?𒈾)
 ("na2"   	?𒈿)
 ("na3"   	?𒀝)
 ("na4"   	["𒉌𒌓"])
 ("ne"    	?𒉈)
 ("ne2"   	?𒉌)
 ("ni"    	?𒉌)
 ("ni2"   	?𒉎)
 ("nu"    	?𒉡)
 ("nu2"   	?𒈿)
 ("pa"    	?𒉺)
 ("pa2"   	?𒐀)
 ("pe"    	?𒉿)
 ("pe2"   	?𒁉)
 ("pi"    	?𒉿)
 ("pi2"   	?𒁉)
 ("pi3"   	?𒁁)
 ("pu"    	?𒁍)
 ("pu2"   	?𒇥)
 ("pu3"   	?𒅤)
 ("qa"    	?𒋡)
 ("qe"    	?𒆥)
 ("qi"    	?𒆥)
 ("qu"    	?𒄣)
 ("aq"    	?𒀝)
 ("eq"    	?𒅅)
 ("iq"    	?𒅅)
 ("uq"    	?𒊌)
 ("ra"    	?𒊏)
 ("ra2"   	?𒁺)
 ("re"    	?𒊑)
 ("re2"   	?𒌷)
 ("ri"    	?𒊑)
 ("ri2"   	?𒌷)
 ("ru"    	?𒊒) ;; U+12292 vs. B.68
 ("ru2"   	?𒆕)
 ("ru3"   	?𒀸)
 ;; ("sa" 	?𒊓)
 ("sa"    	?𒍝)
 ("sa2"   	?𒁲)
 ;; ("sa3"	?𒍝)
 ("sa4"   	["𒄷𒈾"])
 ("se"    	?𒋛)
 ("se2"   	?𒍣)
 ("si"    	?𒋛)
 ("si2"   	?𒍣)
 ("su"    	?𒋢)
 ("su2"   	?𒍪)
 ("su3"   	?𒋤)
 ("su4"   	?𒋜)
 ("sha"   	?𒊭)
 ("sha2"  	?𒐼)
 ("sha3"  	?𒊮)
 ("she"   	?𒊺)
 ("she2"  	?X)
 ("she3"  	?𒂠)
 ("shi"   	?𒅆)
 ("shi2"  	?𒋛)
 ("shu"   	?𒋗)
 ("shu2"  	?𒋙)
 ("shu3"  	?𒂠)
 ("shu4"  	?𒌋)
 ("ta"    	?𒋫)
 ("ta2"   	?𒁕)
 ("te"    	?𒋼)
 ("te2"   	?𒊹)
 ("ti"    	?𒋾)
 ("ti2"   	?𒊹)
 ("ti3"   	?𒁴)
 ("ti4"   	?𒁲)
 ("tu"    	?𒌅)
 ("tu2"   	?𒌓)
 ("tu3"   	?𒁺)
 ("za"    	?𒍝)
 ("za2"   	["𒉌𒌓"])
 ("ze"    	?𒍣)
 ("ze2"   	?𒍢)
 ("zi"    	?𒍣)
 ("zi2"   	?𒍢)
 ("zi3"   	?𒍥)
 ("zu"    	?𒍪)
 ("zu2"   	?𒅗)
 ("a"     	?𒀀)
 ("a2"    	?𒀉)
 ("e"     	?𒂊)
 ("e2"    	?𒂍)
 ("i"     	?𒄿)
 ("i2"    	?𒐊)
 ("u"     	?𒌋)
 ("u2"    	?𒌑)
 ("ab"    	?𒀊)
 ("ab2"   	?𒀖)
 ("eb"    	?𒅁)
 ("eb2"   	?𒌈)
 ("ib"    	?𒅁)
 ("ib2"   	?𒌈)
 ("ub"    	?𒌒)
 ("ub2"   	?𒂠)
 ("ad"    	?𒀜)
 ("ad2"   	?𒄉)
 ("ed"    	?𒀉)
 ("id"    	?𒀉)
 ("id2"   	["𒀀𒇉"])
 ("ud"    	?𒌓)
 ("ud2"   	?𒀾)
 ("ag"    	?𒀝)
 ("ag2"   	?𒉘)
 ("eg"    	?𒅅)
 ("eg2"   	?𒂊)
 ("ig"    	?𒅅)
 ("ig2"   	?𒂊)
 ("ug"    	?𒊌)
 ("ah"    	?𒄴)
 ("ah2"   	?𒋀)
 ("eh"    	?𒄴)
 ("ih"    	?𒄴)
 ("uh"    	?𒄴)
 ("uh2"   	?𒌔)
 ("ak"    	?𒀝)
 ("ek"    	?𒅅)
 ("ik"    	?𒅅)
 ("uk"    	?𒊌)
 ("al"    	?𒀠)
 ("al2"   	?𒀩)
 ("el"    	?𒂖)
 ("el2"   	?𒅋)
 ("il"    	?𒅋)
 ("il2"   	?𒅍)
 ("ul"    	?X)
 ("ul2"   	?𒉡)
 ("am"    	?𒂔) ; was 𒄠
 ("am2"   	?𒉘)
 ("em"    	?𒅎)
 ("im"    	?𒅎)
 ("im2"   	?𒁽)
 ("um"    	?𒌝)
 ("um2"   	?𒌓)
 ("an"    	?𒀭)
 ("en"    	?𒂗)
 ("en2"   	?X)
 ("en3"   	?𒇷)
 ("in"    	?𒅔)
 ("in4"   	?𒂗)
 ("in5"   	["𒊩𒌆"])
 ("un"    	?𒌦)
 ("un2"   	?𒌋)
 ("ap"    	?𒀊)
 ("ep"    	?𒅁)
 ("ep2"   	?𒌈)
 ("ip"    	?𒅁)
 ("ip2"   	?𒌈)
 ("up"    	?𒌒)
 ("up2"   	?𒂠)
 ("ar"    	?𒅈)
 ("ar2"   	?𒌒)
 ("er"    	?𒅕)
 ("ir"    	?𒅕)
 ("ip2"   	["𒀀𒅆"])
 ("ur"    	?𒌨)
 ("ur2"   	?𒌫)
 ("as"    	?𒊍)
 ("es"    	?𒄑)
 ("es2"   	?𒂠)
 ("is"    	?𒄑)
 ("is2"   	?𒂠)
 ("us"    	?X)
 ("us2"   	?𒍑)
 ("s.a"   	?𒍝)
 ("s.e"   	?𒍢)
 ("s.i"   	?𒍢)
 ("s.u"   	?𒍮)
 ("as."   	?𒊍)
 ("es."   	?𒄑)
 ("is."   	?𒄑)
 ("us."   	?X)
 ("ash"   	?𒀸)
 ("ash2"  	?𒀾)
 ("esh"   	?𒌍)
 ;; ("esh"	?𒐁)
 ("esh2"  	?𒂠)
 ("ish"   	?𒅖)
 ("ish2"  	?𒆜)
 ("ush"   	?𒍑)
 ("ush2"  	?𒍗)
 ("ush2"  	?𒁁)
 ("at"    	?𒀜)
 ("at2"   	?𒄉)
 ("et"    	?𒀉)
 ("it"    	?𒀉)
 ("ut"    	?𒌓)
 ("ut2"   	?𒀾)
 ("t.a"   	?𒁕)
 ("t.e"   	?𒁲)
 ("t.i"   	?𒁲)
 ("t.u"   	?𒂅)
 ("at."   	?𒀜)
 ("et."   	?𒀉)
 ("it."   	?𒀉)
 ("ut."   	?𒌓)
 ("wa"    	?𒉿)
 ("we"    	?𒉿)
 ("wi"    	?𒉿)
 ("wu"    	?𒉿)
 ("az"    	?𒊍)
 ("ez"    	?𒄑)
 ("ez2"   	?𒂠)
 ("iz"    	?𒄑)
 ("iz2"   	?𒅖)
 ("uz"    	?X)
 ("uz2"   	?𒍑)
 ("uz3"   	?𒍚)
)

(quail-define-package
 "muflax-latin" "Latin (muflax)" "·" nil
 "Handle the common special characters I use that don't justify their own language mode." nil t t nil nil nil nil nil nil nil t)
(quail-define-rules
 ;; German
 ("·a"   	?ä)
 ("·A"   	?Ä)
 ("·o"   	?ö)
 ("·O"   	?Ö)
 ("·u"   	?ü)
 ("·U"   	?Ü)
 ;; ("·s"	?ß)
 ;; ("·S"	?ẞ)

 ;; conlangs
 ("·d"	?ð)
 ("·D"	?Ð)
 ("·l"	?ł)
 ("·L"	?Ł)
 ("·n"	?ŋ)
 ("·N"	?Ŋ)
 ("·r"	?ř)
 ("·R"	?Ř)
 ("·s"	?š)
 ("·S"	?Š)
 ("·t"	?θ)
 ("·T"	?Θ)
 ("·z"	?ž)
 ("·Z"	?Ž)

 ;; borrowings in German
 ("·e"	?é)
 ("·E"	?É)
 
 ;; better quotes
 ("·<"	?«)
 ("·>"	?»)

 ;; make the · typable by itself
 ("··"	?·)
 ("· "	?·)
 )

(quail-define-package
 "muflax-lojban" "lojban" "'" t
 "Slightly faster Lojban typing." nil t t nil nil nil nil nil nil nil t)
(quail-define-rules
 ("h"	?')
 ("H"	?')
 )

(provide 'custom-input-methods)

;; Local Variables:
;; before-save-hook: ()
;; End:
