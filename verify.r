Rebol [
	Title: "Verify"
	Author: "Christopher Ross-Gill"
	Date: 22-Feb-2013
	Home: http://www.ross-gill.com/page/Verify
	File: %verfiy.r
	Version: 0.2.0
	Purpose: {Defensive Design Helper}
	Rights: http://opensource.org/licenses/Apache-2.0
	Type: 'module
	Name: 'rgchris.verify
	Exports: [verify]
	History: [
		22-Feb-2013 0.2.0 "Extracted from the QuarterMaster web framework"
	]
]

verify: func [
	"Steps through a series of cases/resolutions. Returns last case result where all cases are positive."
	[throw] cases [block!] /local step value
][
	until [
		either block? step: try compose/only [do/next (cases)][
			set [value cases] step
		][
			:step
		]

		unless value cases/1
		cases: next cases
		any [not value tail? cases]
	]

	any [value]
]
