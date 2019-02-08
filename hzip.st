#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"prints the correct symbol for the ASCII characters"
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

"leaf class"
Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [^ count ]
   
   "do not implement is not given a character or an integer"
   Leaf class >> new [
      self shouldNotImplement
   ]
   
   "constructor"
   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count) |
      ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
]

"tree subclass; leaf is a subclass of tree"
Leaf subclass: Tree [
   |left right|
   
   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]
   
   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]
   
   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
]

"Frequency Array"
frequency := Array new: 257.

"setting all elements in the frequency array to 0"
(1 to: 257) do: [:i|
   frequency at: i put: 0.
].

stdinBits := FileStream open: (Smalltalk arguments at: 1) mode: FileStream read.

"creating histogram from input file"
[stdinBits atEnd] whileFalse: [
   character := stdinBits next.
   charNum := character asciiValue.
   frequency at: charNum put: (frequency at: charNum) + 1.
   stdout << character.
].

stdinBits close.

"add an EOF when the file closes to indicate end of file"
frequency at: 257 put: 1.

"(1 to: 257) do: [:i|
   i = 257 ifTrue: [stdout << 'EOF']
           ifFalse: [stdout << i asCharacter visible.].
   stdout nextPutByte: i.
   stdout << ': '.
   stdout << (frequency at: i).
   stdout << nl.
]."

"creating the priority queue"
sortFreq := SortedCollection new.
(1 to: 257) do: [:i|
   |node freqCount|
   freqCount := frequency at: i.
   (freqCount > 0) ifTrue: [
      node := Leaf new: (i asCharacter visible) count: freqCount.
      sortFreq add: node.
   ]
].

sortFreq inspect.

"creating one tree from priority queue"
[sortFreq size > 2] whileTrue: [
   |lLeaf rLeaf tree totalCount|
   lLeaf := sortFreq removeFirst.
   rLeaf := sortFreq removeFirst.
   totalCount := (lLeaf count) + (rLeaf count).
   tree := Tree new: $a count: totalCount left: lLeaf right: rLeaf.
   sortFreq add: tree.
].
"TEST: echo hello world | ./bitstream.st 2>&1"

