Rebol [
	Title: "JSON Parser/Encoder for Rebol 2"
	Author: "Christopher Ross-Gill"
	Date: 6-Feb-2017
	Home: http://www.ross-gill.com/page/JSON_and_Rebol
	File: %altjson.r
	Version: 0.3.8
	Purpose: "De/Serialize a JSON string to Rebol data."
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.altjson
	Exports: [load-json to-json]
	History: [
		06-Feb-2017 0.3.8 "Fix Unicode -> UTF-8 decoding"
		02-May-2016 0.3.7 "Support for /, : and , characters in JSON object keys"
		22-Sep-2015 0.3.6 "Sync with v0.3.6 for Rebol 3"
		07-Jul-2014 0.3.0 "Initial support for JSONP"
		15-Jul-2011 0.2.6 "Flattens Flickr '_content' objects"
		02-Dec-2010 0.2.5 "Support for time! added"
		28-Aug-2010 0.2.4 "Encodes tag! any-type! paired blocks as an object"
		06-Aug-2010 0.2.2 "Issue! composed of digits encoded as integers"
		22-May-2005 0.1.0 "Original Version"
	]
	Notes: {
		- Simple Escaping
		- Converts date! to RFC 3339 Date String
	}
]

load-json: use [
	tree branch here val is-flat emit new-child to-parent neaten resolutions
	space word comma number string block object _content value ident
][
	branch: make block! 10

	emit: func [val][here: insert/only here val]
	new-child: [(insert/only branch insert/only here here: copy [])]
	to-parent: [(here: take branch)]
	neaten: [
		(new-line/all head here true)
		(new-line/all/skip head here true 2)
	]
	resolutions: [
		[]
		[end skip]
	]

	space: use [space][
		space: charset " ^-^/^M"
		[any space]
	]

	comma: [space #"," space]

	number: use [dg ex nm as-num][
		dg: charset "0123456789"
		ex: [[#"e" | #"E"] opt [#"+" | #"-"] some dg]
		nm: [opt #"-" some dg opt [#"." some dg] opt ex]

		as-num: func [val /num][
			num: load val

			all [
				parse val [opt "-" some dg]
				decimal? num
				num: to issue! val
			]

			num
		]

		[copy val nm (val: as-num val)]
	]

	string: use [ch dq es hx mp decode][
		ch: complement charset {\"}
		es: charset {"\/bfnrt}
		hx: charset "0123456789ABCDEFabcdef"
		mp: [#"^"" "^"" #"\" "\" #"/" "/" #"b" "^H" #"f" "^L" #"r" "^M" #"n" "^/" #"t" "^-"]

		decode: use [ch mk escape encode-utf8][
			encode-utf8: func [
				"Encode a code point in UTF-8 format" 
				char [integer!] "Unicode code point"
			][
				as-string to binary! reduce case [
					char <= 127 [[char]]

					; U+0080 - U+07FF
					char <= 2047 [[
						char and 1984 / 64 + 192 
						char and 63 + 128
					]]

					any [
						; http://www.unicode.org/faq/private_use.html#nonchar4
						; invalid U+D800 - U+DFFF ; UTF-16 Surrogates
						all [char >= 55296 char <= 57343]
						; invalid U+FDD0 - U+FDEF ; Noncharacters
						all [char >= 64976 char <= 65007]
						; invalid U+nFFFE - U+nFFFF ; Noncharacters
						equal? 65534 char and 65534
						equal? 65535 char and 65535
					][
						[239 191 189]
					]

					; U+0800 - U+FFFD ; upper U+FFFF for tests
					char <= 65535 [[
						char and 61440 / 4096 + 224 
						char and 4032 / 64 + 128 
						char and 63 + 128
					]]

					; U+010000 - U+10FFFF
					char <= 1114111 [[
						char and 1835008 / 262144 + 240 
						char and 258048 / 4096 + 128 
						char and 4032 / 64 + 128 
						char and 63 + 128
					]]

					true [[239 191 189]] ; Unknown codepoint
				]
			]

			escape: [
				mk: #"\" [
					  es (mk: change/part mk select mp mk/2 2)
					| #"u" copy ch 4 hx (
						mk: change/part mk encode-utf8 to integer! to issue! ch 6
					)
				] :mk
			]

			func [text [string! none!] /mk][
				either none? text [copy ""][
					all [parse/all text [any [to "\" escape] to end] text]
				]
			]
		]

		[#"^"" copy val [any [some ch | #"\" [#"u" 4 hx | es]]] #"^"" (val: decode val)]
	]

	word: use [word1 word+ special escapes escape wordify is-word][
		word1: charset ["!&*=?_|~" #"A" - #"Z" #"a" - #"z"]
		word+: charset ["!&'*+-.0123456789=?_|~" #"A" - #"Z" #"a" - #"z"]
		special: charset "^-^/ '+,-./0123456789:^^"

		escapes: [
			#"^-" "^^t" #"^/" "^^n"
			#" "  "^^_" #"/"  "^^|"
			#","  "^^&" #":"  "^^!"
			#"^^" "^^^^"
		]

		escape: use [mk][
			[
				mk: special (
					mk: change/part mk any [
						select escapes mk/1
						join "^^" mk/1
					] 1
				) :mk
			]
		]

		wordify: func [value /local mk][
			if parse/all value: copy value [
				[some word1 | escape]
				any [some word+ | escape]
			][
				if value = "self" [value: "^^*self"]
				to set-word! value
			]
		]

		[
			string (is-word: pick resolutions set-word? val: wordify val)
			is-word
		]
	]

	block: use [list][
		list: [space opt [value any [comma value]] space]

		[#"[" new-child list #"]" neaten/1 to-parent]
	]

	_content: [#"{" space {"_content"} space #":" space value space "}"] ; Flickr

	object: use [name list as-object token][
		name: [
			token space #":" space
			(emit either is-flat [to tag! val][val])
		]
		list: [
			(token: either is-flat [string][word])
			space opt [name value any [comma name value]] space
		]
		as-object: [(unless is-flat [here: change back here make object! here/-1])]

		[#"{" new-child list #"}" neaten/2 to-parent as-object]
	]

	ident: use [initial ident][
		initial: charset ["$_" #"a" - #"z" #"A" - #"Z"]
		ident: union initial charset [#"0" - #"9"]

		[initial any ident]
	]

	value: [
		  "null" (emit none)
		| "true" (emit true)
		| "false" (emit false)
		| number (emit val)
		| string (emit val)
		| _content
		| object | block
	]

	func [
		[catch] "Convert a JSON string to Rebol data"
		json [string! binary! file! url!] "JSON string"
		/flat "Objects are imported as tag-value pairs"
		/padded "Loads JSON data wrapped in a JSONP envelope"
	][
		is-flat: :flat
		tree: here: copy []
		if any [file? json url? json][
			if error? json: try [read (json)][
				throw :json
			]
		]
		unless parse/all json either padded [
			[space ident space "(" space opt value space ")" opt ";" space]
		][
			[space opt value space]
		][
			throw make error! either is-flat [
				"Not a valid JSON string"
			][
				"Cannot load JSON string, try in /FLAT mode"
			]
		]
		pick tree 1
	]
]

to-json: use [
	json emit emits escape to-token emit-issue emit-date
	here lookup comma block block-of-pairs object value
][
	emit: func [data][repend json data]
	emits: func [data][emit {"} emit data emit {"}]

	escape: use [mp ch encode utf-8 decode-utf8][
		mp: [#"^/" "\n" #"^M" "\r" #"^-" "\t" #"^"" "\^"" #"\" "\\" #"/" "\/"]
		ch: complement charset compose [
			{^@^A^B^C^D^E^F^G^H^K^L^M^N^O^P^Q^R^S^T^U^V^W^X^Y^Z^[^\^]^!^_}
			(extract mp 2) #"^(80)" - #"^(FF)"
		]

		encode: func [here /local char][
			char: any [
				select mp here/1
				either here/1 > 127 ["\uFFFD"][
					join "\u" skip tail mold to-hex to integer! char -4
				]
			]
			change/part here char 1
		]

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

		encode-unicode: func [mark [string!] ext [string!] /local char][
			either 65535 > char: decode-utf8 mark [
				change/part mark join "\u" skip tail mold to-hex char -4 ext
			][
				ext
			]
		]

		func [txt [string! binary!] /local ext][
			parse/all txt [
				any [
					  txt: some ch
					| utf-8 ext: (txt: encode-unicode txt ext) :txt
					| skip (txt: encode txt) :txt
				]
			]
			head txt
		]
	]

	to-token: use [special escapes mark][
		special: charset "!&'+-.0123456789nt^^_|"
		escapes: [
			#"t" "^-" #"n" "^/"
			#"_" " "  #"|" "/"
			#"&" ","  #"!" ":"
		]

		func [name [set-word!]][
			if parse/all name: to string! name [
				"^^*self" end (remove remove name)
				| some [
					mark: "^^" special (
						mark: change/part mark any [
							select escapes mark/2
							mark/2
						] 2
					) :mark
					| skip
				]
			][
				name
			]
		]
	]

	emit-issue: use [dg nm][
		dg: charset "0123456789"
		nm: [opt "-" some dg]

		[(either parse/all here/1 nm [emit here/1][emits here/1])]
	]

	emit-date: use [pad second][
		pad: func [part length][
			part: to string! part head insert/dup part "0" length - length? part
		]

		[(
			emits rejoin collect [
				keep reduce [pad here/1/year 4 "-" pad here/1/month 2 "-" pad here/1/day 2]
				if here/1/time [
					keep reduce ["T" pad here/1/time/hour 2 ":" pad here/1/time/minute 2 ":"]
					second: parse/all to string! here/1/time/second "."
					keep pad second/1 2
					unless second/2 = "0" [keep join "." second/2]

					keep either any [
						none? here/1/zone
						zero? here/1/zone
					]["Z"][
						reduce [
							either here/1/zone/hour < 0 ["-"]["+"]
							pad abs here/1/zone/hour 2 ":" pad here/1/zone/minute 2
						]
					]
				]
			]
		)]
	]

	lookup: [
		here: [get-word! | get-path!]
		(change here reduce reduce [here/1])
		end skip
	]

	comma: [(if not tail? here [emit ","])]

	block: [(emit "[") any [here: value here: comma] (emit "]")]

	block-of-pairs: [
		  some [set-word! skip]
		| some [tag! skip]
	]

	object: [
		(emit "{")
		any [
			here: [set-word! (change here to-token here/1) | tag!]
			(emit [{"} escape to string! here/1 {":}])
			here: value here: comma
		]
		(emit "}")
	]

	value: [
		  lookup
		| number! (emit here/1)
		| [logic! | 'true | 'false] (emit to string! here/1)
		| [none! | 'none] (emit 'null)
		| date! emit-date
		| issue! emit-issue
		| [
			any-string! | word! | lit-word! | tuple! | pair! | money! | time!
		] (emits escape form here/1)
		| any-word! (emits escape form to word! here/1)

		| object! :here (change/only here body-of first here) into object
		| into block-of-pairs :here (change/only here copy first here) into object
		| any-block! :here (change/only here copy first here) into block

		| any-type! (emits to tag! type?/word first here)
	]

	func [
		"Convert a Rebol value to JSON string"
		item [any-type!] "Rebol value to convert"
	][
		json: make string! ""
		if parse compose/only [(item)][here: value][json]
	]
]
