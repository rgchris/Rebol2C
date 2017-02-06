Rebol [
	Title: "Complete the Etsy User Authentication"
	Date: 28-Jan-2017
	Author: "Christopher Ross-Gill"
	Home: http://re-bol.com/etsy_api_tutorial.html
	File: %authenticate-etsy.r
	Version: 0.1.0
	Purpose: {Obtain permissions to use Etsy API}
	Rights: http://opensource.org/licenses/Apache-2.0
	History: [
		28-Jan-2017 0.1.0 "Original Version"
	]

	API: https://openapi.etsy.com/v2/
	Credentials: [
		Consumer-Key: <consumer-key>
		Consumer-Secret: <consumer-secret>
	]
	Port: 8080
	Scope: [email_r listings_r]
]

do %verify.r
do %rest.r
do %validate.r
do %httpd.r
do %rsp.r
do %form-error.r

etsy: make object! [
	api: system/script/header/api
	root: join http://127.0.0.1: system/script/header/port
	request-broker: api/(%oauth/request_token)
	access-broker: api/(%oauth/access_token)
	callback: root/verify
	api-scope: form system/script/header/scope

	handshake: values: params: errors: secret: none

	credentials: make object! system/script/header/credentials

	handler: [
		handshake: values: params: errors: none

		switch/default request/action [
			"GET /favicon.ico" [
				response/status: 200
				response/type: "image/png"
				response/content: read/binary %rebol.png
			]

			"GET /request" [
				if verify [
					not error? handshake: try [
						read/custom [
							scheme: 'rest
							url: (request-broker)
							oauth: (
								make credentials [
									oauth-callback: :callback
								]
							)
						][
							scope: :api-scope
						]
					][
						response/status: 500
						print response/content: "Bad Request (connection error)"
						print form-error :handshake
					]

					handshake/status = 200 [
						response/status: 500
						print response/content: "Bad Request (status error)"
						probe handshake/content
					]

					block? values: try [load-webform handshake/content][
						response/status: 500
						print response/content: "Bad Request (encoding error)"
						help handshake
					]

					params: validate/report-to values [
						login_url: url! ["https://www.etsy.com/oauth/signin?" to end]
						oauth_token: string! [30 32 hex]
						oauth_token_secret: string! [10 hex]
						oauth_consumer_key: string! [24 alphanum]
						oauth_callback: url!
						oauth_callback_confirmed: logic! ["true"]
					] errors: copy [] [
						response/status: 500
						print response/content: "Bad Request (params error)"
						probe neaten/pairs values
						probe errors
					]
				][
					secret: params/oauth_token_secret
					response/status: 303
					response/location: params/login_url
				]
			]

			"GET /verify" [
				if verify [
					params: validate load-webform request/query-string [
						oauth_token: string! [some hex]
						oauth_verifier: string! [some hex]
					][
						response/status: 500
						print response/content: "Bad Request, <a href='/request'>Try Again</a>"
						help request
					]

					not error? handshake: try [
						read/custom [
							scheme: 'rest
							url: :access-broker
							action: 'post
							oauth: make credentials [
								oauth-token: (params/oauth_token)
								oauth-token-secret: (secret)
							]
						] compose [
							oauth_verifier: (params/oauth_verifier)
						]
					][
						response/status: 500
						print response/content: "Bad Request, <a href='/request'>Try Again</a> (curl error)"
						print form-error :handshake
					]

					handshake/status = 200 [
						response/status: 500
						print response/content: "Bad Request, <a href='/request'>Try Again</a> (status error)"
						help handshake
					]

					block? values: try [load-webform handshake/content][
						response/status: 500
						print response/content: "Bad Request, <a href='/request'>Try Again</a> (encoding error)"
						help handshake
					]

					params: validate values [
						oauth_token: string! [30 hex]
						oauth_token_secret: string! [10 hex]
					][
						response/status: 500
						response/content: "Bad Request, <a href='/request'>Try Again</a> (params error)"
						help handshake
						probe values
					]
				][
					response/status: 200
					response/kill?: true ; we're done with the server!
					response/content: render/with {
						<dl>
							<dt>Token</dt>
							<dd><%== params/oauth_token %></dd>
							<dt>Secret</dt>
							<dd><%== params/oauth_token_secret %></dd>
						</dl>
					} [params]
				]
			]
		][
			response/content: "Not sure what you're trying to do here..."
		]
	]
]

server: open/custom join httpd://: system/script/header/port etsy/handler
do probe compose [browse (etsy/root/request)]
wait 60 ; automatically closes after a minute
close server
