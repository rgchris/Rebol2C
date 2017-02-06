Rebol [
	Title: "Web Server Scheme for Rebol 2"
	Author: "Christopher Ross-Gill"
	Date: 6-Feb-2017
	File: %httpd.r
	Version: 0.2.0
	Purpose: "An elementary Web Server scheme for creating fast prototypes"
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.httpd
	History: [
		06-Feb-2017 0.2.0 "Include HTTP Parser/Dispatcher"
		12-Jan-2017 0.1.0 "Original Version"
	]
	Notes: {
		Original version based on Carl Sassenrath's Micro Web Server
		http://www.rebol.org/view-script.r?script=webserver.r
	}
]

unless in system/schemes 'httpd [
	system/schemes: make system/schemes [httpd: none]
]

set in system/schemes 'httpd make system/standard/port [
	scheme: 'httpd
	port-id: 8888

	; Default Settings, Ignore
	passive: #[none]
	cache-size: 0
	proxy: make object! [host: port-id: user: pass: type: bypass: #[none]]

	; Handler
	handler: make object! bind [
		init: func [server [port!] spec][
			if url? spec [
				spec: decode-url spec
			]

			spec: make make object! [port-id: awake: none] spec
			; probe spec

			either integer? server/port-id: spec/port-id [
				server/sub-port: make port! [
					scheme: 'tcp
					port-id: spec/port-id
					async-modes: 'connect
				]
				server/state/flags: server/state/flags or system/standard/port-flags/pass-thru
			][
				make error! "Invalid HTTPd Spec"
			]
		]

		open: func [server [port!]][
			server/sub-port/awake: prepare-listener server
			net-utils/net-log rejoin ["Opening HTTP listening port " server/port-id]
			insert system/ports/wait-list server/sub-port
			system/words/open/direct/binary server/sub-port
		]

		close: func [server [port!]][
			net-utils/net-log rejoin ["Closing HTTP listening port " server/port-id]
			remove find system/ports/wait-list server/sub-port
			system/words/close server/sub-port
		]

	] make object! [
		buffer: make binary! 2 + buffer-size: 64 * 1024

		request-prototype: make object! [
			version: 1.1
			method: "GET"
			action: headers: http-headers: none
			oauth: target: binary: content: length: timeout: none
			type: 'application/x-www-form-urlencoded
			server-software: rejoin [
				system/script/header/title " v" system/script/header/version " "
				"Rebol/" system/product " v" system/version
			]
			server-name: gateway-interface: none
			server-protocol: "http"
			server-port: request-method: request-uri:
			path-info: path-translated: script-name: query-string:
			remote-host: remote-addr: auth-type:
			remote-user: remote-ident: content-type: content-length: none
			error: none
		]

		response-prototype: make object! [
			status: 404
			content: "Not Found"
			location: none
			type: "text/html"
			kill?: false
		]

		start: func [port [port!]][
			append system/ports/wait-list port
		]

		stop: func [port [port!]][
			remove find system/ports/wait-list port
			close port
		]

		num: 0

		wake-client: func [client [port!] /local state wire request response this][
			this: num
			++ num

			while [
				case [
					error? state: try [
						read-io client clear buffer buffer-size
					][
						state: disarm :state
						either state/code = 517 [ ; would-block
							; end of input?
							net-utils/net-log rejoin ["[" this "] End of Stream"]
						][
							net-utils/net-log rejoin ["[" this "] Read Error"]
							help state
						]
						false
					]

					state <= 0 [
						net-utils/net-log rejoin ["[" this "] Client closed connection: " state]
						; probe buffer
						false
					]

					state [true]
				]
			][
				net-utils/net-log rejoin ["[" this "] Reading stream..."]
				append client/locals/wire buffer
			]

			transcribe client
			dispatch client

			net-utils/net-log rejoin ["[" this "] Responding..."]
			client/async-modes: 'write
			write-io client as-string client/locals/wire length? client/locals/wire
			stop client

			client/locals/response/kill?
		]

		prepare-listener: func [
			server [port!] /local handler
		][
			handler: func [
				request [object!]
				response [object!]
			] case [
				function? get in server 'awake [body-of get in server 'awake]
				block? server/awake [server/awake]
				block? server/state/custom [server/state/custom]
				true [default-response]
			]

			; listener AWAKE function
			func [listener [port!] /local client part state] compose [
				; help listener
				client: first listener
				; help client

				net-utils/net-log reform ["HTTP request from" client/host]
				; probe client/user-data

				client/async-modes: 'read
				client/awake: :wake-client
				client/local-service: quote (:handler)
				client/locals: make object! [
					request: response: none
					wire: make binary! 0
					protect [request response wire]
				]

				start client
				false
			]
		]

		transcribe: use [
			request-action request-path request-query
			header-prototype header-feed header-name header-part
		][
			request-action: ["HEAD" | "GET" | "POST" | "PUT" | "DELETE"]

			request-path: use [chars][
				chars: complement charset [#"^@" - #" " #"?"]
				[some chars]
			]

			request-query: use [chars][
				chars: complement charset [#"^@" - #" "]
				[some chars]
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

			header-prototype: context [
				Accept: "*/*"
				Connection: "close"
				User-Agent: rejoin ["Rebol/" system/product " " system/version]
				Content-Length: Content-Type: Authorization: Range: none
			]

			transcribe: func [
				client [port!]
				/local request name value pos
			][
				client/locals/request: make request-prototype [
					either parse/all client/locals/wire [
						copy method request-action space
						copy request-uri [
							copy target request-path opt [
								"?" copy query-string request-query
							]
						] space
						"HTTP/" copy version ["1.0" | "1.1"]
						header-feed
						(headers: make block! [])
						some [
							copy name header-name ":" any " "
							copy value header-part header-feed
							(
								repend headers [to set-word! name value]
								switch name [
									"Content-Type" [content-type: value]
									"Content-Length" [length: content-length: value]
								]
							)
						]
						header-feed content: to end (
							content: as-string binary: as-binary copy content
						)
					][
						request-method: :method
						path-info: :target
						action: reform [method target]
						remote-addr: client/remote-ip

						headers: make header-prototype http-headers: new-line/all/skip headers true 2

						type: if string? headers/Content-Type [
							copy/part type: headers/Content-Type any [
								find type ";"
								tail type
							]
						]

						length: content-length: any [
							attempt [length: to integer! length]
							0
						]

						net-utils/net-log action
					][
						; action: target: request-method: query-string: binary: content: request-uri: none
						net-utils/net-log error: "Could Not Parse Request"
					]
				]
			]
		]

		dispatch: use [status-codes][
			status-codes: [
				200 "OK" 201 "Created" 204 "No Content"
				301 "Moved Permanently" 302 "Moved temporarily" 303 "See Other" 307 "Temporary Redirect"
				400 "Bad Request" 401 "No Authorization" 403 "Forbidden" 404 "Not Found" 411 "Length Required"
				500 "Internal Server Error" 503 "Service Unavailable"
			]

			func [client [port!] /local response continue?][
				client/locals/response: response: make response-prototype []
				client/local-service client/locals/request response
				head collect/into [
					case/all [
						not find status-codes response/status [
							response/status: 500
						]
						any [not any-string? response/content empty? response/content][
							response/content: " "
						]
					]

					keep reform ["HTTP/1.0" response/status select status-codes response/status]
					keep reform ["^/Content-Type:" response/type]
					keep reform ["^/Content-Length:" length? response/content]
					if response/location [
						keep reform ["^/Location:" response/location]
					]
					keep "^/^/"
					keep response/content
				] clear client/locals/wire
			]
		]
	]
]