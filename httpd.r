Rebol [
	Title: "Web Server Scheme for Rebol 2"
	Author: "Christopher Ross-Gill"
	Date: 12-Jan-2017
	File: %httpd.r
	Version: 0.1.0
	Purpose: "An elementary Web Server scheme for creating fast prototypes"
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.httpd
	History: [
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
	passive: #[none]
	cache-size: 0
	proxy: make object! [host: port-id: user: pass: type: bypass: #[none]]
	handler: make object! bind [
		init: func [port [port!] spec][
			use [digit port-number][
				digit: charset "0123456789"
				port-number: none

				either all [
					url? spec
					parse/all spec [
						"httpd://" opt [
							":" copy port-number some digit end
							(port-number: to integer! port-number)
						]
					]
				][
					port/sub-port: make port! [
						scheme: 'tcp
						port-id: any [port-number port/port-id]
						async-modes: [connect]
						locals: make binary! buffer-size: 64 * 1024
					]
				][
					make error! "Invalid HTTPd Spec"
				]
			]
		]

		open: func [port [port!]][
			port/state/flags: port/state/flags or system/standard/port-flags/pass-thru
			port/buffer-size: 64 * 1024

			; port/awake: func [request [object!]] either block? port/state/custom [
			; 	:port/state/custom
			; ][
			; 	[]
			; ]

			port/sub-port/awake: func [listen-port] use [handler][
				handler: either block? port/state/custom [
					port/state/custom
				][
					response: "I'm Alive!!!"
				]

				[
					use [port part][
						port: first listen-port
						clear listen-port/locals
						probe port/async-modes
						; port/async-modes: [read write]
					

						; repend listen-port/locals ["Address: " port/host newline]
						; set-modes port [no-wait: true]
						read-io port listen-port/locals listen-port/buffer-size
						probe transcribe port listen-port/locals
						write-io port data: "HTTP/1.0 200 OK^/Content-type: text/plain^/^/I'm Alive!!!" length? data
						system/words/close port
					]
					false
				]
			]

			; set-modes port/sub-port [read: true write: true binary: false lines: true no-wait: false direct: true]
			insert system/ports/wait-list port/sub-port
			system/words/open/direct/binary port/sub-port
		]

		close: func [port [port!]][
			probe "CLOSED!!!"
			remove find system/ports/wait-list port/sub-port
			system/words/close port/sub-port
		]
	] make object! [
		transcribe: use [
			request-uri version-number
			header-feed header-name header-part
			request-prototype header-prototype
		][
			request-uri: use [chars][
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

			request-prototype: context [
				version: 1.1
				action: "GET"
				headers: http-headers: none
				query: none
				oauth: target: binary: content: length: timeout: none
				type: 'application/x-www-form-urlencoded
			]

			header-prototype: context [
				Accept: "*/*"
				Connection: "close"
				User-Agent: rejoin ["Rebol/" system/product " " system/version]
				Content-Length: Content-Type: Authorization: Range: none
			]

			transcribe: func [port [port!] request-data [binary!] /local request name value pos][
				port/locals: request: make request-prototype [
					unless parse/all request-data [
						copy action ["HEAD" | "GET" | "POST" | "PUT" | "DELETE"] space
						copy target request-uri space
						"HTTP/" copy version ["1.0" | "1.1"]
						header-feed
						(net-utils/net-log reform ["HTTP Request:" action target])
						(
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
						make error! "Could Not Parse Request"
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
	]
]