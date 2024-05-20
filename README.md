# cobcol
```
A tool to generate helper columns for COBOL records.
(c) 2024 by ssulser

commandline:	enclose in '
		X(n) - alphanumeric
		N(n) - numeric

If you want record helper columns for this:
	01 NAME    PIC X(10).
	01 ID      PIC 9(4).
	01 PRICE   PIC 99V99.

the commandline looks like this:
	cobcol	'X(10);N(4);N(4)' or
                'X(10)N(4)N(4)'   or
		'X(10) N(4) N(4)'

this produces this output:
	XXXXXXXXXX99997777

which can then be used in an editor to enter some data:
	XXXXXXXXXX99997777
	SIMON SULS12350850
	TESTER TES45671250
```
