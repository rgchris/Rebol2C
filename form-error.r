Rebol [
	Title: "Form Error"
	Author: "Christopher Ross-Gill"
	Date: 6-Feb-2017
	File: %form-error.r
	Version: 1.0.0
	Purpose: "Pretty prints an error."
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.form-error
	Exports: [form-error]
	History: [
		6-Feb-2017 1.0.0 "Original Version"
	]
]


form-error: func [reason [error!] /local type message][
	reason: make disarm reason []

	type: system/error/(reason/type)/type
	message: reform bind compose [(system/error/(reason/type)/(reason/id))] reason
	reason: rejoin [
		"** " type ": " message
		"^/** Where: " copy/part mold reason/where 100
		"^/** Near: " copy/part mold reason/near 100
	]
]
