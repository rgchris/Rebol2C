Rebol [
	Title: "Clean"
	Author: "Christopher Ross-Gill"
	Date: 22-Dec-2015
	File: %clean.r
	Version: 0.2.0
	Purpose: "Trims and Converts Errant CP-1252 to UTF-8"
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.clean
	Exports: [clean]
	History: [
		22-Dec-2015 0.2.0 "Added /Lines"
		14-Aug-2013 0.1.1 "Original Version"
	]
]

clean: use [codepoints cleaner lines?][
	codepoints: collect [
		foreach [point char][
			; for 8-bit encodings other than CP-1252, change these codepoints
			128 #{E282AC} 130 #{E2809A} 131 #{C692}
			132 #{E2809E} 133 #{E280A6} 134 #{E280A0}
			135 #{E280A1} 136 #{CB86} 137 #{E280B0}
			138 #{C5A0} 139 #{E280B9} 140 #{C592}
			142 #{C5BD} 145 #{E28098} 146 #{E28099}
			147 #{E2809C} 148 #{E2809D} 149 #{E280A2}
			150 #{E28093} 151 #{E28094} 152 #{CB9C}
			153 #{E284A2} 154 #{C5A1} 155 #{E280BA}
			156 #{C593} 158 #{C5BE}
		][
			keep to char! point keep char
		]
	]

	cleaner: use [here ascii utf2 utf-3 utf-4 utf-b][
		ascii: charset [#"^(00)" - #"^(0C)" #"^(0E)" - #"^(7F)"]
		utf-2: charset [#"^(C2)" - #"^(DF)"]
		utf-3: charset [#"^(E0)" - #"^(EF)"]
		utf-4: charset [#"^(F0)" - #"^(F4)"]
		utf-b: charset [#"^(80)" - #"^(BF)"]

		[
			ascii | utf-2 utf-b | utf-3 2 utf-b | utf-4 3 utf-b ; OK
			| here: [ ; Not OK
				#"^M" (here: either lines? [remove here][next here])
				|
				skip (
					here: change/part here case [
						here/1 > #"^(BF)" [append copy #{C3} here/1 and #"^(BF)"]
						here/1 > #"^(9E)" [append copy #{C2} here/1]
						here/1 [any [select codepoints here/1 #{EFBFBD} "<?>"]]
					] 1
				)
			] :here
		]
	]

	clean: func [
		"Convert Windows 1252 characters in a string to UTF-8"
		string [any-string!] "String to convert"
		/lines "Also Clean Line Endings"
	][
		lines?: :lines
		parse/all string [any cleaner]
		string
	]
]