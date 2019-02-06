#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: bitstream.st,v 1.1 2019-02-01 14:36:25-08 - - $"
execname := '$RCSfile: bitstream.st,v $' =~ '.*: (.*),v.*' at: 1.

Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 16]
   ]
]

nl := Character nl.

"Frequency Array"
frequency := Array new: 257.

"setting all elements in the frequency array to 0"
(1 to: 257) do: [:i|
   frequency at: i put: 0.
].

stdinBits := FileStream open: (Smalltalk arguments at: 1) mode: FileStream read.

[stdinBits atEnd] whileFalse: [
   character := stdinBits next.
   charNum := character asciiValue.
   frequency at: charNum put: (frequency at: charNum) + 1.
   stdout << character.
].

(1 to: 257) do: [:i|
   stdout nextPutByte: i. 
   stdout << ': '.
   stdout << (frequency at: i).
   stdout << nl.
].

stdinBits close.

"TEST: echo hello world | ./bitstream.st 2>&1"

