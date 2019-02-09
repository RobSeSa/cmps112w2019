#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"prints the correct symbol for the ASCII characters"
Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ self asString]
                   ifFalse: [^ self asInteger printPaddedWith: 0 to: 1 base: 16]
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
      ^ aStream << self class << '(' << (char visible) << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   postorder: visitor [
      visitor value: char.
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

   postorder: visitor [
      left postorder: visitor.
      right postorder: visitor.
   ]
]

"too many or too few arguments"
((Smalltalk arguments size) < 2) | ((Smalltalk arguments size) > 3) ifTrue: [
   stdout << 'Error: incorrect argument number. ' << nl << 'Exiting...' << nl.
   ObjectMemory quit: 1.
].

"hzip. -dtcu inputFile outputFile"
((Smalltalk arguments size) = 3) ifTrue: [
   stdout << '3 arguments detected.' << nl << 'options = ' << (Smalltalk arguments at: 1) << nl.
   fileIn := FileStream open: (Smalltalk arguments at: 2) mode: FileStream read.
   fileOut := FileStream open: (Smalltalk arguments at: 3) mode: FileStream write.
].

"hzip. -dtcu inputFile"
((Smalltalk arguments size) = 2) ifTrue: [
   stdout << '2 arguments detected.' << nl << 'options = ' << (Smalltalk arguments at: 1) << nl.
   fileIn := FileStream open: (Smalltalk arguments at: 2) mode: FileStream read.
   fileOut := stdout.
].


"Frequency Array"
frequency := Array new: 257.

"setting all elements in the frequency array to 0"
(1 to: 257) do: [:i|
   frequency at: i put: 0.
].




"creating histogram from input file"
[fileIn atEnd] whileFalse: [
   character := fileIn next.
   charNum := character asciiValue.
   frequency at: charNum put: (frequency at: charNum) + 1.
   "stdout << character."
].

fileIn close.

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
      node := Leaf new: (i asCharacter) count: freqCount.
      sortFreq add: node.
   ]
].

"sortFreq inspect."

"creating one tree from priority queue"
[sortFreq size > 1] whileTrue: [
   |lLeaf rLeaf tree totalCount|
   lLeaf := sortFreq removeFirst.
   rLeaf := sortFreq removeFirst.
   totalCount := (lLeaf count) + (rLeaf count).
   tree := Tree new: (lLeaf char) count: totalCount left: lLeaf right: rLeaf.
   sortFreq add: tree.
].


"sortFreq inspect."
stdout << ';;;;;;;;;;;' << nl.
bitTree := Array new: 257.

stdout << nl << 'Before vising sortFreq Tree' << nl.
(sortFreq last) inorder: [:char :string |
   stdout << '[' << char << ']=' << string << nl.
   bitTree at: (char asInteger) put: string.
] prefix: ''.

(sortFreq last) inspect.
bitTree inspect.

"(sortFreq last) postorder: [:char |
   |pathSize path|
   path := bitTree at: (char asInteger).
   pathSize := path size.
   (1 to: pathSize) do: [:i|
      fileOut bitAt: idx put: (path bitAt: i).
   ]
].
"
((Smalltalk arguments size) = 3) ifTrue: [
   fileOut close.
].
