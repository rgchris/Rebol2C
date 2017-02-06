Rebol [
	Title: "REST-Friendly HTTP Protocol"
	Author: "Christopher Ross-Gill"
	Date: 5-Feb-2017
	Home: http://www.ross-gill.com/page/REST_Protocol
	File: %rest.r
	Version: 0.1.5
	Purpose: {
		An elementary HTTP protocol allowing more versatility when developing Web
		Services clients.
	}
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.rest
	History: [
		05-Feb-2017 0.1.5 "Correct usage of CURL's /INTO refinement"
		12-Jan-2017 0.1.4 "Tidy up of OAuth portion"
		30-Oct-2012 0.1.3 "Use CURL in place of native TCP; Added OAuth"
		21-Aug-2010 0.1.2 "Submitted to Rebol.org"
		09-Nov-2008 0.1.1 "Minor changes"
		15-Aug-2006 0.1.0 "Original REST Version"
	]
	Notes: {To Do: Multipart and Multipart OAuth; Better Header Support}
]

do %altwebform.r
do %curl.r

unless in system/schemes 'rest [
	system/schemes: make system/schemes [REST: none]
]

system/schemes/rest: make system/standard/port [
	scheme: 'rest
	port-id: 80
	passive: none
	cache-size: 5
	proxy: make object! [host: port-id: user: pass: type: bypass: none]
]

system/schemes/rest/handler: use [prepare transcribe execute][
	prepare: use [
		request-prototype header-prototype
		oauth-credentials oauth-prototype
		sign
	][
		request-prototype: context [
			version: 1.1
			action: "GET"
			headers: none
			query: none
			oauth: target: content: length: timeout: none
			type: 'application/x-www-form-urlencoded
		]

		header-prototype: context [
			Accept: "*/*"
			Connection: "close"
			User-Agent: rejoin ["Rebol/" system/product " " system/version]
			Content-Length: Content-Type: Authorization: Range: none
		]

		oauth-credentials: context [
			consumer-key: consumer-secret:
			oauth-token: oauth-token-secret:
			oauth-callback: none
		]

		oauth-prototype: context [
			oauth_callback: none
			oauth_consumer_key: none
			oauth_token: oauth_nonce: none
			oauth_signature_method: "HMAC-SHA1"
			oauth_timestamp: none
			oauth_version: 1.0
			oauth_verifier: oauth_signature: none
		]

		sign: func [request [object!] /local header params timestamp out][
			out: copy ""
			timestamp: now/precise

			header: make oauth-prototype [
				oauth_consumer_key: request/oauth/consumer-key
				oauth_token: request/oauth/oauth-token
				oauth_callback: request/oauth/oauth-callback
				oauth_nonce: enbase/base checksum/secure join timestamp oauth_consumer_key 64
				oauth_timestamp: form any [
					attempt [to integer! difference timestamp 1-Jan-1970/0:0:0]
					timestamp - 1-Jan-1970/0:0:0 * 86400.0
				]
				clear find/last oauth_timestamp "."
			]

			params: make header any [request/content []]
			params: sort/skip body-of params 2

			header/oauth_signature: enbase/base checksum/secure/key rejoin [
				uppercase form request/action "&" url-encode form request/url "&"
				url-encode replace/all to-webform params "+" "%20"
			] rejoin [
				request/oauth/consumer-secret "&" any [request/oauth/oauth-token-secret ""]
			] 64

			foreach [name value] body-of header [
				if value [
					repend out [", " form name {="} url-encode form value {"}]
				]
			]

			switch request/action [
				"GET" [
					if request/content [
						request/url: join request/url to-webform/prefix request/content
						request/content: none
					]
				]

				"POST" "PUT" [
					request/content: to-webform any [request/content []]
					request/headers/Content-Type: "application/x-www-form-urlencoded"
				]
			]

			request/headers/Authorization: join "OAuth" next out
		]

		prepare: func [port [port!] /local request][
			port/locals/request: request: make request-prototype port/locals/request
			request/action: uppercase form request/action
			request/headers: make header-prototype any [request/headers []]
			request/content: any [port/state/custom request/content]

			either request/oauth [
				request/oauth: make oauth-credentials request/oauth
				sign request
			][
				request/headers/Authorization: any [
					request/headers/authorization
					if all [port/user port/pass][
						join "Basic " enbase join port/user [#":" port/pass]
					]
				]
			]

			if port/state/index > 0 [
				request/version: 1.1
				request/headers/Range: rejoin ["bytes=" port/state/index "-"]
			]

			case/all [
				block? request/content [
					request/content: to-webform request/content
					request/headers/Content-Type: "application/x-www-form-urlencoded"
				]

				string? request/content [
					request/length: length? request/content
					request/headers/Content-Length: length? request/content
					request/headers/Content-Type: request/type
				]
			]

			port
		]
	]

	execute: func [port [port!]][
		curl/full/method/header/with/timeout/into ; url action headers content response
			port/locals/request/url
			port/locals/request/action
			port/locals/request/headers
			port/locals/request/content
			port/locals/request/timeout
			port/locals/response
	]

	transcribe: use [
		response-code header-feed header-name header-part
		response-prototype header-prototype
	][
		response-code: use [digit][
			digit: charset "0123456789"
			[3 digit]
		]

		header-feed: [newline | crlf]

		header-part: use [chars][
			chars: complement charset [#"^(00)" - #"^(1F)"]
			[some chars any [header-feed some " " some chars]]
		]

		header-name: use [chars][
			chars: charset ["_-0123456789" #"a" - #"z" #"A" - #"Z"]
			[some chars]
		]

		space: use [space][
			space: charset " ^-"
			[some space]
		]

		response-prototype: context [
			status: message: http-headers: headers: content: binary: type: length: none
		]

		header-prototype: context [
			Date: Server: Last-Modified: Accept-Ranges: Content-Encoding: Content-Type:
			Content-Length: Location: Expires: Referer: Connection: Authorization: none
		]

		transcribe: func [port [port!] /local response name value pos][
			port/locals/response: response: make response-prototype [
				unless parse/all port/locals/response [
					"HTTP/1." ["0" | "1"] space
					copy status response-code (message: "")
					opt [space copy message header-part] header-feed
					(net-utils/net-log reform ["HTTP Response:" status message])
					(
						status: load status
						headers: make block! []
					)
					some [
						copy name header-name ":" any " "
						copy value header-part header-feed
						(repend headers [to set-word! name value])
					]
					header-feed content: to end (
						content: as-string binary: as-binary copy content
					)
				][
					net-utils/net-log pos
					make error! "Could Not Parse Response"
				]

				headers: make header-prototype http-headers: new-line/all/skip headers true 2

				type: all [
					path? type: attempt [load headers/Content-Type]
					type
				]

				length: any [attempt [headers/Content-Length: to integer! headers/Content-Length] 0]
			]
		]
	]

	context [
		port-flags: system/standard/port-flags/pass-thru

		init: func [port [port!] spec [url! block!] /local url][
			port/locals: context [
				request: case/all [
					url? spec [
						spec: compose [url: (to url! replace form spec rest:// http://)]
					]
					block? spec [
						if verify [
							url: case/all [
								get-word? url: pick find compose spec quote url: 2 [
									url: get :url
								]

								url? url [url]
							][
								make error! "REST spec needs a URL"
							]

							parse/all url ["http" opt "s" "://" to end] [
								make error! "REST Spec only works with HTTP(S) urls"
							]
						][
							spec
						]
					]
				]

				response: make string! ""
			]
		]

		open: func [port [port!]][
			port/state/flags: port/state/flags or port-flags
			execute prepare port
		]

		copy: :transcribe

		close: does []
	]
]