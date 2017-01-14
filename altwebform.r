Rebol [
	Title: "Web Form Encoder/Decoder for Rebol 2"
	Author: "Christopher Ross-Gill"
	Date: 30-Aug-2013
	Home: http://www.ross-gill.com/page/Web_Forms_and_Rebol
	File: %altwebform.r
	Version: 0.1.7
	Purpose: "Convert a Rebol block to a URL-Encoded Web Form string"
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.altwebform
	Exports: [url-decode url-encode load-webform to-webform]
	History: [
		30-Aug-2013 0.1.7 "Clean up bitsets"
		30-Jan-2013 0.1.6 "Resolve GET-WORD! values"
		03-Jul-2011 0.1.5 "Minor Changes"
		19-Sep-2009 0.1.3 "First Published Version"
	]
	Notes: "Conforms to application/x-www-form-urlencoded"
	Example: [
		"a=3&aa.a=1&b.c=1&b.c=2"
		[a "3" aa [a "1"] b [c ["1" "2"]]]
	]
]

url-decode: use [deplus sp decrlf][
	deplus: func [text][
		parse/all text [
			some [to sp text: (text: change text #" ") :text] to end
		]
		head text
	]

	decrlf: func [text][
		parse/all text [
			some [to crlf text: (text: change/part text #"^/" 2) :text] to end
		]
		head text
	]

	func [text [any-string! none!] /wiki][
		sp: either wiki [#"_"][#"+"]
		decrlf dehex deplus form any [text ""]
	]
]

load-webform: use [result path string pair term as-path][
	result: copy []

	as-path: func [name [string!]][to-path to-block replace/all name #"." #" "]

	path: use [aa an wd][
		aa: charset [#"a" - #"z" #"A" - #"Z" #"_"]
		an: union aa charset ["-" #"0" - #"9"]
		wd: [aa 0 40 an] ; one alpha, any alpha/digit/dash/underscore
		[wd 0 6 [#"." wd]]
	]

	string: use [ch hx][
		ch: charset ["!'*,-._~" #"0" - #"9" #"A" - #"Z" #"a" - #"z"]
		hx: charset [#"0" - #"9" #"A" - #"F" #"a" - #"f"]
		[any [ch | #"+" | #"%" 2 hx]] ; any [unreserved | percent-encoded]
	]

	term: [#"&" | end]

	pair: use [name value tree][
		[
			copy name path [
				#"=" copy value string term | term (value: none)
			] (
				tree: :result
				name: as-path name
				value: url-decode value

				until [
					tree: any [
						find/tail tree name/1
						insert tail tree name/1
					]

					name: next name

					switch type?/word tree/1 [
						none! [unless tail? name [insert/only tree tree: copy []]]
						string! [change/only tree tree: reduce [tree/1]]
						block! [tree: tree/1]
					]

					if tail? name [append tree value]
				]
			)
		]
	]

	func [
		[catch] "Loads Data from a URL-Encoded Web Form string"
		webform [string! none!]
	][
		webform: any [webform ""]
		result: copy []

		either parse/all webform [opt [#"&" | #"?"] any pair][
			new-line/all/skip result true 2
		][
			make error! "Not a URL Encoded Web Form"
		]
	]
]

url-encode: use [ch sp encode][
	ch: charset ["!'*,-.~" #"0" - #"9" #"A" - #"Z" #"a" - #"z"]
	encode: func [text][insert next text enbase/base form text/1 16 change text "%"]

	func [text [any-string!] /wiki][
		sp: either wiki [#"_"][#"+"]

		parse/all copy text [
			copy text any [
				  text: some ch | #" " (change text sp)
				| #"_" (all [wiki encode text]) | skip (encode text)
			]
		]
		text
	]
]

to-webform: use [
	webform form-key emit
	here path reference value block array object
][
	path: []
	form-key: does [
		remove head foreach key path [insert "" reduce ["." key]]
	]

	emit: func [data][
		repend webform ["&" form-key "=" url-encode data]
	]

	reference: [
		here: get-word! (change/only here attempt [get/any here/1]) :here
	]

	value: [
		  here: number! (emit form here/1)
		| [logic! | 'true | 'false] (emit form here/1)
		| [none! | 'none]
		| date! (replace form date "/" "T")
		| [any-string! | tuple! | money! | time! | pair!] (emit form here/1)
	]

	array: [any value end]

	object: [
		any [
			here: [word! | set-word!] (insert path to word! here/1)
			any reference [value | block] (remove path)
		] end
	]

	block: [
		here: [
			  any-block! (change/only here copy here/1)
			| object! (change/only here body-of here/1)
		] :here into [object | mk: array]
	]

	func [
		"Serializes block data as URL-Encoded Web Form string"
		data [block! object!] /prefix
	][
		clear path
		webform: copy ""
		data: either object? data [body-of data][copy data]
		if parse copy data object [
			either all [prefix not tail? next webform][back change webform "?"][remove webform]
		]
	]
]