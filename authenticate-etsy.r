Rebol [
	Title: "Complete the Etsy User Authentication"
	Date: 12-Feb-2017
	Author: "Christopher Ross-Gill"
	Home: http://re-bol.com/etsy_api_tutorial.html
	File: %authenticate-etsy.r
	Version: 0.2.0
	Purpose: {Obtain permissions to use Etsy API}
	Rights: http://opensource.org/licenses/Apache-2.0
	History: [
		to-do       0.3.0 "Include scope on initiation form"
		12-Feb-2017 0.2.0 "Initiates with form requesting credentials"
		28-Jan-2017 0.1.0 "Original Version"
	]

	API: https://openapi.etsy.com/v2/
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
	port: system/script/header/port
	root: join http://127.0.0.1: port
	request-broker: api/(%oauth/request_token)
	access-broker: api/(%oauth/access_token)
	callback: root/verify
	api-scope: form system/script/header/scope

	handshake: credentials: values: params: errors: secret: none

	views: make object! [
		display: {
			<link rel="stylesheet" type="text/css"
				href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
				integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u"
				crossorigin="anonymous">
			<div class="container" style="margin-top: 1em;">
			<div class="page-header"><h1>Etsy Authentication</h1></div>
			<div class="row"><div class="col-sm-8">
			<%= content %>
			</div></div></div>
		}

		reject: {
			<link rel="stylesheet" type="text/css"
				href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
				integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u"
				crossorigin="anonymous">
			<div class="container" style="margin-top: 1em;">
			<div class="page-header"><h1>Error</h1></div>
			<div class="row"><div class="col-sm-8">
			<p class="alert alert-danger"><%= content %></p>
			</div></div></div>
		}

		init: {
			<% if find credentials 'error [ %>
			<p class="alert alert-danger">The credentials provided were not sufficient.</p>
			<% ] %>
			<p class="lead">First enter your Application credentials:</p>
			<form method="post" action="/request" class="form-horizontal">
				<div class="form-group">
					<label for="i-key" class="col-sm-3 control-label">Key</label>
					<div class="col-sm-9">
						<%! ; build-tag notation
							input value (credentials/consumer-key)
							name "consumer-key" type "text"
							id "i-key" placeholder "Key" class "form-control"
						 %>
					</div>
				</div>
				<div class="form-group">
					<label for="i-secret" class="col-sm-3 control-label">Secret</label>
					<div class="col-sm-9">
						<%! ; build-tag notation
							input value (credentials/consumer-secret)
							name "consumer-secret" type "text"
							id "i-secret" placeholder "Secret" class "form-control"
						 %>
					</div>
				</div>
				<div class="form-group">
					<div class="col-sm-offset-3 col-sm-9">
						<button type="submit" class="btn btn-primary">Start Authentication</button>
					</div>
				</div>
			</form>
		}

		done: {
			<p class="lead">Your request credentials:</p>
			<table class="table table-condensed">
			<tr>
				<th>Consumer-Key:</th>
				<td><tt><%== mold credentials/consumer-key %></tt></td>
			</tr>
			<tr>
				<th>Consumer-Secret</th>
				<td><tt><%== mold credentials/consumer-secret %></tt></td>
			</tr>
			<tr>
				<th>OAuth-Token:</th>
				<td><tt><%== mold params/oauth_token %></tt></td>
			</tr>
			<tr>
				<th>OAuth-Token-Secret</th>
				<td><tt><%== mold params/oauth_token_secret %></tt></td>
			</tr>
			</table>
		}
	]

	foreach name words-of views [
		set :name load-rsp trim/auto get :name
	]

	display: func [response [object!] status [integer!] content [string! binary!]][
		response/status: status
		response/content: either string? content [
			views/display [content]
		][
			content
		]
		content
	]

	reject: func [reponse [object!] status [integer!] content [string! binary!]][
		response/status: status
		response/content: views/reject [content]
		content
	]

	redirect: func [response [object!] status [integer!] target [file! url!]][
		response/status: status
		response/location: target
	]

	handler: [
		handshake: values: params: errors: webform: none

		switch/default request/action [
			"GET /favicon.ico" [
				response/type: "image/png"
				display response 200 read/binary %rebol.png
			]

			"GET /request" [
				credentials: [Consumer-Key "" Consumer-Secret ""]
				display response 200 views/init [credentials]
			]

			"POST /request" [
				if verify [
					all [
						request/type = "application/x-www-form-urlencoded"
						block? webform: try [load-webform request/content]
					][
						reject response 400 "Web Form incorrectly submitted."
					]

					credentials: validate webform [
						consumer-key: string! [24 alphanum]
						consumer-secret: string! [10 alphanum]
					][
						credentials: collect [
							keep validate/loose webform [
								consumer-key: string!
								consumer-secret: string!
							]
							keep [consumer-key "" consumer-secret "" error]
						]

						; if at first you don't succeed...
						display response 400 views/init [credentials]
					]

					not error? handshake: try [
						read/custom [
							scheme: 'rest
							url: (request-broker)
							oauth: (
								make credentials: make object! [
									consumer-key: credentials/consumer-key
									consumer-secret: credentials/consumer-secret
								][
									oauth-callback: :callback
								]
							)
						][
							scope: :api-scope
						]
					][
						print reject response 500 "Bad Request (connection error)"
						print form-error :handshake
					]

					handshake/status = 200 [
						print reject response 500 "Bad Request (status error)"
						probe handshake/content
					]

					block? values: try [load-webform handshake/content][
						print reject response 500 "Bad Request (encoding error)"
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
						print reject response 500 "Bad Request (params error)"
						probe neaten/pairs values
						probe errors
					]
				][
					secret: params/oauth_token_secret
					redirect response 303 params/login_url
				]
			]

			"GET /verify" [
				if verify [
					params: validate load-webform request/query-string [
						oauth_token: string! [some hex]
						oauth_verifier: string! [some hex]
					][
						print reject response 500 "Bad Request, <a href='/request'>Try Again</a> (request error)"
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
						print reject response 500 "Bad Request, <a href='/request'>Try Again</a> (curl error)"
						print form-error :handshake
					]

					handshake/status = 200 [
						print reject response 500 "Bad Request, <a href='/request'>Try Again</a> (status error)"
						help handshake
					]

					block? values: try [load-webform handshake/content][
						print reject response 500 "Bad Request, <a href='/request'>Try Again</a> (encoding error)"
						help handshake
					]

					params: validate values [
						oauth_token: string! [30 hex]
						oauth_token_secret: string! [10 hex]
					][
						print reject response  500 "Bad Request, <a href='/request'>Try Again</a> (params error)"
						help handshake
						probe values
					]
				][
					response/kill?: true ; we're done with the server!
					display response 200 views/done [credentials params]
				]
			]
		][
			response/content: "Not sure what you're trying to do here..."
		]
	]

	server: open/custom join httpd://: port handler
	do probe compose [browse (root/request)]
	wait 60 ; automatically closes after a minute
	close server
]

