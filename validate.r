Rebol [
	Title: "Validate"
	Author: "Christopher Ross-Gill"
	Date: 26-Apr-2007
	File: %validate.r
	Version: 0.6.0
	Purpose: {Validates user input data}
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.validate
	Exports: [validate]
	History: [
		23-Apr-2014 0.6.0 "Rearrange source to be more modular"
		26-Apr-2007 0.5.0 "Original Version"
	]
	Notes: {Extracted from the QuarterMaster web framework}
]

do %as.r

envelop: func [val [any-type!]][
	either any-block? val [val][reduce [val]]
]

wrap: func [body [block!]][
	use collect [
		parse body [
			any [body: set-word! (keep to word! body/1) | skip]
		]
	] head body
]

add-to: func [series key val][
	key: envelop key
	map key func [key][as word! key]
	if find key none! [return none]
	until [
		series: any [
			find/tail series key/1
			insert tail series key/1
		]

		key: next key

		switch type?/word series/1 [
			none! [unless tail? key [insert/only series series: copy []]]
			string! [change/only series series: envelop series/1]
			block! [series: series/1]
		]

		if tail? key [append series val]
	]
]

get-from: func [series 'key][
	key: copy envelop key
	while [all [not tail? key any-block? series]][
		series: select series take key
	]
	all [tail? key series]
]

validate: wrap [
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
					['length | 'size] (val: string-length? form value val-type: integer!)
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
						val: to word! val
						unless value = as/where :type get-from source :val format [
							report not-confirmed
						]
					) |
					is-not? 'within set group [any-block! | get-word!] otherwise (
						if get-word? group [
							unless all [
								function? group: get :group
								block? group: group
							][
								group: []
							]
						]

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

		valid?: func [value][any [value find type 'none!]]

		humanize: func [key /local initial][
			initial: true
			if parse form key amend [
				copy key any [
					key: alpha (if initial [uppercase/part key 1] initial: false)
					| ["-" | "_" | " "] (change key " " initial: true)
					| skip (initial: false)
				]
			][
				rejoin ["'" key "'"]
			]
		]

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

	validate: func [
		"Validates user input" [catch]
		source [any-type!] {User input: [word! any-type! ...]}
		spec [block!] {Validation Rules}
		/block "Implied keys determined by order"
		/loose "Ignores constraints"
		/report-to "Recover exceptions"
		errs [block!] "Block to hold exceptions"
	][
		unless block? source [return none]

		spec: make-filter source compose/deep/only spec either loose [
			[
				(skip-constraints)
				any [
					set key set-word! (key: to word! key)
					opt 'opt
					[set type datatype (type: get type)
					| set type ['dialect | 'match | 'object!] (type: :block!)]
					set format opt [block! | get-word!]
					otherwise

					(
						value: get-from source :key

						present: not any [
							none? value
							empty? trim form value
						]

						if all [
							present
							not none? value: case [
								:type = block! [value]
								:type = logic! [as logic! value]
								value [form value]
							]
						][
							repend result [key value]
						]
					)

					constraints
				]
			]
		][
			[
				any [
					set key set-word! (key: to word! key)
					set required opt 'opt (required: required <> 'opt)
					[
						set type datatype (type: get type)
						| set type ['dialect | 'match | 'object!]
					]
					set format opt [block! | get-word!]
					otherwise

					(
						value: either block [
							pick source 1
						][
							get-from source :key
						]

						either all [
							present: not any [
								all [
									none? value
									not find reduce [
										logic! block! 'object!
									] :type
								]
								empty? trim form value
							]

							not none? value: case [
								find [dialect match] :type [
									use [values][
										all [
											block? format
											values: attempt [to block! value]
											switch type [
												dialect [parse values format]
												match [match values format]
											]
											value
										]
									]
								]

								:type = 'object! [
									value: envelop value
									either block? format [
										validate value format
									][value]
								]

								:type = block! [
									value: envelop value
									either block? format [
										all [parse value format value]
									][value]
								]

								:type = logic! [
									either as logic! value [true][false]
								]

								true [as/where :type value format]
							]
						][
							do-constraints
							if block [source: next source]
							repend result [key value]
						][
							skip-constraints
							case [
								all [present not block] [report invalid]
								required [report blank]
								not required [repend result [key none]]
							]
						]
					)

					constraints
				]

				end (if all [block not tail? source empty? errors][key: 'validate report too-many])
			]
		]

		unless spec/engage [do make error! "Could not parse Validate specification"]

		all [block? errs insert clear errs spec/errors]
		if empty? spec/errors [spec/result]
	]
]