Rebol [
	Title: "Form Date"
	Author: "Christopher Ross-Gill"
	Date: 26-Apr-2007
	Version: 1.0.2
	File: %form-date.r
	Rights: http://creativecommons.org/licenses/by-sa/3.0/
	Purpose: {Return formatted date string using strftime style format specifiers}
	Home: http://www.ross-gill.com/QM/
	Comment: {Extracted from the QuarterMaster web framework}
	History: [
		1.0.2 03-Mar-2013 rgchris "Fixed leaked local words"
		1.0.1 05-Feb-2013 rgchris "Minor update to padding/pairs for R3 compatability"
		1.0.1 18-Jul-2007 btiffin "Obtained permission to add %c and %s precise seconds"
		1.0.0 26-Apr-2007 btiffin "Obtained permission to prepare script for rebol.org library"
		1.0.0 24-Apr-2007 rgchris "The original"
	]
	Notes: {
		do http://reb4.me/r/form-date to include the form-date function in the global namespace

		>> form-date now "%A %e%i %B, %Y at %T"
		== "Thursday 26th April, 2007 at 00:44:12"

		>> form-date now "%d-%b-%Y/%H:%M:%S%Z"
		== "26-Apr-2007/00:49:39-04:00"

		>> now
		== 26-Apr-2007/0:52:13-4:00

		>> form-date now/precise "%c"
		== "19-Jul-2007/01:02:03.012000-04:00"
	}
]

form-date: use [
	get-class interpolate
	pad pad-zone pad-precise to-epoch-time to-iso-week
	date-codes
][

;--## SERIES HELPER
;-------------------------------------------------------------------##
	get-class: func [classes [block!] item][
		all [
			classes: find classes item
			classes: find/reverse classes type? pick head classes 1
			first classes
		]
	]

;--## STRING HELPERS
;-------------------------------------------------------------------##
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

	pad: func [text length [integer!] /with padding [char!]][
		padding: any [padding #"0"]
		text: form text
		skip tail insert/dup text padding length negate length
	]

;--## DATE HELPERS
;-------------------------------------------------------------------##
	pad-zone: func [time /flat][
		rejoin [
			pick "-+" time/hour < 0
			pad abs time/hour 2
			either flat [""][#":"]
			pad time/minute 2
		]
	]

	pad-precise: func [seconds [number!] /local out][
		seconds: form round/to make time! seconds 1E-6 ; works so long as 0 <= seconds < 60
		head change copy "00.000000" find/last/tail form seconds ":"
	]

	to-epoch-time: func [date [date!]][
		; date/time: date/time - date/zone
		date: form any [
			attempt [to integer! difference date 1-Jan-1970/0:0:0]
			date - 1-Jan-1970/0:0:0 * 86400.0
		]
		clear find/last date "."
		date
	]

	to-iso-week: use [get-iso-year][
		get-iso-year: func [year [integer!] /local d1 d2][
			d1: to-date join "4-Jan-" year
			d2: to-date join "28-Dec-" year
			reduce [d1 + 1 - d1/weekday d2 + 7 - d2/weekday]
		]

		func [date [date!] /local out d1 d2][
			out: 0x0
			set [d1 d2] get-iso-year out/y: date/year

			case [
				date < d1 [d1: first get-iso-year out/y: date/year - 1]
				date > d2 [d1: first get-iso-year out/y: date/year + 1]
			]

			out/x: date + 8 - date/weekday - d1 / 7
			out
		]
	]

	date-codes: [
		#"a" [copy/part pick system/locale/days date/weekday 3]
		#"A" [pick system/locale/days date/weekday]
		#"b" [copy/part pick system/locale/months date/month 3]
		#"B" [pick system/locale/months date/month]
		#"C" [to-integer date/year / 100]
		#"d" [pad date/day 2]
		#"D" [date/year #"-" pad date/month 2 #"-" pad date/day 2]
		#"e" [date/day]
		#"g" [pad (to integer! second to-iso-week date) // 100 2]
		#"G" [to integer! second to-iso-week date]
		#"h" [time/hour + 11 // 12 + 1]
		#"H" [pad time/hour 2]
		#"i" [any [get-class ["st" 1 21 31 "nd" 2 22 "rd" 3 23] date/day "th"]]
		#"I" [pad time/hour + 11 // 12 + 1 2]
		#"j" [pad date/julian 3]
		#"J" [date/julian]
		#"m" [pad date/month 2]
		#"M" [pad time/minute 2]
		#"p" [pick ["am" "pm"] time/hour < 12]
		#"P" [pick ["AM" "PM"] time/hour < 12]
		#"R" [pad time/hour 2 #":" pad time/minute 2]
		#"s" [to-epoch-time date]
		#"S" [pad to integer! time/second 2]
		#"t" [#"^-"]
		#"T" [pad time/hour 2 #":" pad time/minute 2 #":" pad round time/second 2]
		#"u" [date/weekday]
		#"U" [pad to integer! date/julian + 6 - (date/weekday // 7) / 7 2]
		#"V" [pad to integer! first to-iso-week date 2]
		#"w" [date/weekday // 7]
		#"W" [pad to integer! date/julian + 7 - date/weekday / 7 2]
		#"y" [pad date/year // 100 2]
		#"x" [pad-precise time/second]
		#"Y" [date/year]
		#"z" [pad-zone/flat zone]
		#"Z" [pad-zone zone]
		#"%" ["%"]

		#"c" [
			date/year #"-" pad date/month 2 "-" pad date/day 2 "T"
			pad time/hour 2 #":" pad time/minute 2 #":" pad to integer! time/second 2 
			either gmt ["Z"][pad-zone zone]
		]
	]

	func [
		"Renders a date to a given format (largely compatible with strftime)"
		date [date!] format [any-string!]
		/gmt "Align time with GMT"
		/local time zone nyd
	][
		bind date-codes 'date
		all [
			gmt date/time date/zone
			date/time: date/time - date/zone
			date/zone: none
		]

		time: any [date/time 0:00]
		zone: any [date/zone 0:00]
		interpolate format date-codes
	]
]
