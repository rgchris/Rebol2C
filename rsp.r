Rebol [
	Title: "RSP Preprocessor"
	Author: "Christopher Ross-Gill"
	Date: 13-Aug-2013
	File: %rsp.r
	Version: 0.4.1
	Purpose: "Rebol-based hypertext pre-processor"
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.rsp
	Exports: [sanitize load-rsp render render-each]
	History: [
		17-Jan-2017 0.4.1 "Updated Unicode/UTF8 handling"
		13-Aug-2013 0.4.0 "Extracted from the QuarterMaster web framework"
	]
	Notes: {
		<% ... "evaluate Rebol code" [ ... %><% ... ] %>
		<%= ... "evaluate Rebol code and emit the product" ... %>
		<%== ... "evaluate Rebol code, sanitize and emit the product" ... %>
		<%! ... "pass contents to COMPOSE then BUILD-TAG and emit" ... %>
	}
]

sanitize: use [html* utf-8 decode-utf][
	ascii: exclude charset ["^/^-" #"^(20)" - #"^(7E)"] charset {&<>"}
	html*: exclude ascii charset {&<>"}

	utf-8: use [utf-2 utf-3 utf-4 utf-b][
		; probably need to adapt the rule from below...
		utf-2: charset [#"^(C2)" - #"^(DF)"]
		utf-3: charset [#"^(E0)" - #"^(EF)"]
		utf-4: charset [#"^(F0)" - #"^(F4)"]
		utf-b: charset [#"^(80)" - #"^(BF)"]

		[utf-2 utf-b | utf-3 2 utf-b | utf-4 3 utf-b]
	]

	decode-utf8: use [
		utf-2 utf-3 utf-3-low utf-4 utf-4-low utf-4-high utf-b
		utf-x1 utf-x2 utf-x3 bounds out
	][
		; U+000080..U+0007FF _____________ C2..DF 80..BF
		; U+000800..U+000FFF __________ E0 A0..BF 80..BF
		; U+001000..U+00FFFF ______ E1..EF 80..BF 80..BF
		; U+010000..U+03FFFF ___ F0 90..BF 80..BF 80..BF
		; U+040000..U+0FFFFF F1..F3 80..BF 80..BF 80..BF
		; U+100000..U+10FFFF ___ F4 80..8F 80..BF 80..BF
		utf-2: charset [#"^(C2)" - #"^(DF)"]
		utf-3-low: charset [#"^(A0)" - #"^(BF)"]
		utf-3: charset [#"^(E1)" - #"^(EF)"]
		utf-4-low: charset [#"^(90)" - #"^(BF)"]
		utf-4-high: charset [#"^(80)" - #"^(8F)"]
		utf-4: charset [#"^(F1)" - #"^(F3)"]
		utf-b: charset [#"^(80)" - #"^(BF)"]

		utf-x1: charset [#"^(A0)" - #"^(BF)"]
		utf-x2: charset [#"^(90)" - #"^(AF)"]
		utf-x3: charset [#"^(8F)" #"^(9F)" #"^(AF)" #"^(BF)"]

		func [char [string! binary!] /strict][
			bounds: [0 0]
			out: -1
			any [
				all [
					any [
						parse/all char: as-binary char [
							; Test for invalid sequences first
							[
								; invalid U+D800 - U+DFFF ; UTF-8 Surrogates
								#"^(ED)" utf-x1 utf-b
								|
								; invalid U+FDD0 - U+FDEF ; ???
								#"^(EF)" #"^(B7)" utf-x2
								|
								; invalid U+nFFFE - U+nFFFF ; Troublesome UTF-16 sequences
								[#"^(EF)" | [#"^(F0)" | utf-4] utf-x3] #"^(BF)" [#"^(BE)" | #"^(BF)"]
							]
							|
							utf-2 utf-b (
								bounds: [127 2048]
								out: char/1 xor 192 * 64
								+ (char/2 xor 128)
							)
							| [
								  #"^(E0)" utf-3-low utf-b (bounds: [2047 4096])
								| utf-3 2 utf-b (bounds: [4095 65534])
							] (
								out: char/1 xor 224 * 4096
								+ (char/2 xor 128 * 64)
								+ (char/3 xor 128)
							)
							| [
								  #"^(F0)" utf-4-low 2 utf-b (bounds: [65535 262144])
								| utf-4 3 utf-b (bounds: [262143 1048576])
								| #"^(F4)" utf-4-high 2 utf-b (bounds: [1048575 1114112])
							] (
								out: char/1 xor 240 * 262144
								+ (char/2 xor 128 * 4096)
								+ (char/3 xor 128 * 64)
								+ (char/4 xor 128)
							)
						]
						not strict
					]
					out > bounds/1
					out < bounds/2
					out
				]
				65533 ; Unknown character
			]
		]
	]

	sanitize: func [text [any-string!] /local char][
		parse/all copy text [
			copy text any [
				text: some html*
				| #"&" (text: change/part text "&amp;" 1) :text
				| #"<" (text: change/part text "&lt;" 1) :text
				| #">" (text: change/part text "&gt;" 1) :text
				| #"^"" (text: change/part text "&quot;" 1) :text
				| #"^M" (remove text) :text 
				| copy char utf-8 (text: change/part text rejoin ["&#" decode-utf8 char ";"] length? char)
				| skip (text: change/part text rejoin ["#(" to-integer text/1 ")"] 1) :text
				; | skip (text: change text "#") :text
			]
		]
		any [text ""]
	]
]

load-rsp: use [prototype to-set-block][
	prototype: context [
		out*: "" prin: func [val][repend out* val]
		print: func [val][prin val prin newline]
	]

	to-set-block: func [block [block! object!] /local word][
		either object? block [block: third block][
			parse copy block [
				(block: copy [])
				any [set word word! (repend block [to-set-word word get/any word])]
			]
		]
		block
	]

	func [[catch] body [string!] /local code mk][
		code: make string! length? body

		append code "^/out*: make string! {}^/"
		parse/all body [
			any [
				end (append code "out*") break
				| "<%" [
					  "==" copy mk to "%>" (repend code ["prin sanitize form (" mk "^/)^/"])
					| "=" copy mk to "%>" (repend code ["prin (" mk "^/)^/"])
					| [#":" | #"!"] copy mk to "%>" (repend code ["prin build-tag [" mk "^/]^/"])
					| copy mk to "%>" (repend code [mk newline])
					| (throw make error! "Expected '%>'")
				] 2 skip
				| copy mk [to "<%" | to end] (repend code ["prin " mold mk "^/"])
			]
		]

		func [args [block! object!]] compose/only [
			args: make prototype to-set-block args
			do bind/copy (throw-on-error [load code]) args
		]
	]
]

render: use [depth*][
	depth*: 0 ;-- to break recursion

	func [
		[catch] rsp [file! url! string!]
		/with locals [block! object!]
	][
		if depth* > 20 [return ""]
		depth*: depth* + 1

		rsp: case/all [
			file? rsp [rsp: read rsp]
			url? rsp [rsp: read rsp]
			binary? rsp [rsp: to string! rsp]
			string? rsp [
				throw-on-error [rsp: load-rsp rsp]
				throw-on-error [rsp any [locals []]]
			]
		]

		depth*: depth* - 1
		rsp
	]
]

render-each: func [
	'items [word! block!]
	source [series!]
	body [file! url! string!]
	/with locals /local out
][
	out: copy ""
	locals: append any [locals []] items: compose [(items)]
	foreach :items source compose/only [
		append out render/with body (locals)
	]
	return out
]
