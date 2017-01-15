Rebol [
	Title: "RSP Preprocessor"
	Author: "Christopher Ross-Gill"
	Date: 13-Aug-2013
	File: %rsp.r
	Version: 0.4.0
	Purpose: "Rebol-based hypertext pre-processor"
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.rsp
	Exports: [sanitize load-rsp render render-each]
	History: [
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

	utf-8: use [utf-2 utf-3 utf-4 utf-5 utf-b][
		utf-2: charset [#"^(C0)" - #"^(DF)"]
		utf-3: charset [#"^(E0)" - #"^(EF)"]
		utf-4: charset [#"^(F0)" - #"^(F7)"]
		utf-5: charset [#"^(F8)" - #"^(FB)"]
		utf-b: charset [#"^(80)" - #"^(BF)"]

		[utf-2 1 utf-b | utf-3 2 utf-b | utf-4 3 utf-b | utf-5 4 utf-b]
	]

	decode-utf: use [utf-os utf-fc int][
		utf-os: [0 192 224 240 248 252]
		utf-fc: [1 64 4096 262144 16777216]

		func [char][
			int: 0
			char: change char char/1 xor pick utf-os length? char
			forskip char 1 [change char char/1 xor 128]
			char: head reverse head char
			forskip char 1 [int: (to-integer char/1) * (pick utf-fc index? char) + int]
			all [int > 127 int <= 65535 int]
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
				| copy char utf-8 (text: change/part text rejoin ["&#" decode-utf char ";"] length? char)
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
