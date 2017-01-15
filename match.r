Rebol [
	Title: "Match"
	Author: "Christopher Ross-Gill"
	Date: 17-Sep-2013
	Home: http://www.ross-gill.com/page/Match
	File: %match.r
	Version: 0.2.0
	Purpose: {Extract structured data from an unstructured block.}
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.match
	Exports: [match]
	History: [
		17-Sep-2013 0.2.0 "Extracted from the QuarterMaster web framework"
	]
	Usage: [
		result: match ["Product" $12.99][
			name: string!       ; requires a string value to be present, set to string value
			price: some money!  ; requires one or more money values, set to block
			place: opt url!     ; optional url value, set to url value or none
		]
	]
]

wrap: func [body [block!]][
	use collect [
		parse body [
			any [body: set-word! (keep to word! body/1) | skip]
		]
	] head body
]

match: wrap [
	interpolate: func [body [string!] escapes [any-block!] /local out][
		body: out: copy body

		parse/all body [
			any [
				to #"%" body: (
					body: change/part body reduce any [
						select/case escapes body/2 body/2
					] 2
				) :body
			]
		]

		out
	]

	filter: [
		result: errors: #[none]

		messages: [
			not-included "is not included in the list"
			excluded "is reserved"
			invalid "is missing or invalid"
			not-confirmed "doesn't match confirmation"
			not-accepted "must be accepted"
			empty "can't be empty"
			blank "can't be blank"
			too-long "is too long (maximum is %d characters)"
			too-short "is too short (minimum is %d characters)"
			wrong-length "is the wrong length (should be %d characters)"
			not-a-number "is not a number"
			too-many "has too many arguments"
		]

		datatype: [
			'any-string! | 'binary! | 'block! | 'char! | 'date! | 'decimal! | 'email! | 'file! |
			'get-word! | 'integer! | 'issue! | 'lit-path! | 'lit-word! | 'logic! | 'money! |
			'none! | 'number! | 'pair! | 'paren! | 'path! | 'range! | 'refinement! |
			'set-path! | 'set-word! | 'string! | 'tag! | 'time! | 'tuple! | 'url! | 'word!
		]

		else: #[none]
		otherwise: [
			['else | 'or][
				set else string! | copy else any [word! string!]
			] | (else: #[none])
		]

		source: spec: rule: key: value: required: present: target: type: format: constraints: else: none

		constraint: use [is is-not? is-or-length-is op val val-type range group][
			op: val: val-type: none
			is: ['is | 'are]
			is-or-length-is: [
				[
					['length | 'size] (val: string-length? value val-type: integer!)
					| (val: :value val-type: :type)
				] is
			]
			is-not?: ['not (op: false) | (op: true)]

			[
				is [
					'accepted otherwise (
						unless true = value [report not-accepted]
					) |
					'confirmed opt 'by set val get-word! otherwise (
						val: to-word val
						unless value = as/where :type get-from source :val format [
							report not-confirmed
						]
					) |
					is-not? 'within set group any-block! otherwise (
						either case [
							block? value [value = intersect value group]
							true [found? find group value]
						][
							unless op [report excluded]
						][
							if op [report not-included]
						]
					)
				] |
				is-or-length-is [
					is-not? 'between [set range [range! | into [2 val-type]]] otherwise (
						either op [
							case [
								val < target: range/1 [report too-short]
								val > target: range/2 [report too-long]
							]
						][
							unless any [
								val < range/1
								val > range/2
							][report excluded]
						]
					) |
					'more-than set target val-type otherwise (
						unless val > target [report too-short]
					) |
					'less-than set target val-type otherwise (
						unless val < target [report too-long]
					) |
					set target val-type otherwise (
						unless val = target [report wrong-length]
					)
				]
			]
		]

		do-constraints: does [constraints: [any constraint]]
		skip-constraints: does [constraints: [to set-word! | to end]]

		humanize: func [word][uppercase/part replace/all form word "-" " " 1]

		report: func ['message [word!]][
			message: any [
				all [string? else else]
				all [block? else select else message]
				reform [humanize key any [select messages message ""]]
			]
			unless select errors :key [repend errors [:key copy []]]
			append select errors :key interpolate message [
				#"w" [form key]
				#"W" [humanize key]
				#"d" [form target]
				#"t" [form type]
			]
		]

		engage: does [parse spec rule]
	]

	make-filter: func [source spec rule][
		spec: context compose/deep [
			(filter)
			errors: copy []
			result: copy []
			rule: [(copy/deep rule)]
			spec: [(spec)]
		]
		spec/source: copy source
		spec
	]

	get-one: func [data type /local res][
		parse data [some [res: type to end break | skip]]
		unless tail? res [take res]
	]

	get-some: func [data type /local pos res][
		res: make block! length? data
		parse data [some [pos: type (append/only res take pos) :pos | skip]]
		unless empty? res [res]
	]

	match: func [
		[catch] source [block!] spec [block!]
		/report-to errs [block!]
	][
		spec: make-filter source spec [
			(
				remove-each item result: copy spec [not set-word? item]
				result: context append result none
			)

			some [
				set key set-word! (key: to-word key)
				set required ['opt | 'any | 'some | none]
				copy type [lit-word! any ['| lit-word!] | datatype any ['| datatype]]
				otherwise

				(
					switch/default required [
						any [
							value: get-some source type
							either value [do-constraints][skip-constraints]
						]
						opt [
							value: get-one source type
							either value [do-constraints][skip-constraints]
						]
						some [
							value: get-some source type
							either value [do-constraints][skip-constraints report invalid]
						]
					][
						value: get-one source type
						either value [do-constraints][skip-constraints report invalid]
					]

					result/(key): value
				)

				constraints
			]

			end (if all [not empty? source empty? errors][key: 'match report too-many])
		]

		unless spec/engage [raise "Could not parse Match specification"]

		all [block? errs insert clear errs spec/errors]
		if empty? spec/errors [spec/result]
	]
]