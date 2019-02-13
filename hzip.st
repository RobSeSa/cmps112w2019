#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

"prints the correct symbol for the ASCII characters"
Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ self asString]
                   ifFalse: [^ self asInteger]
   ]
]

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

   postorder: aStream[
      aStream writeBit: 0.
      aStream writeChar: (char asInteger).
   ]
   
]

"Tree class; Tree is a subclass of Leaf"
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

   inorder: visitor prefix: string[
      left inorder: visitor prefix: (string, '0').
      right inorder: visitor prefix: (string, '1').
   ]

   postorder: aStream [
      left postorder: aStream.
      right postorder: aStream.
      aStream writeBit: 1.
   ]
]

"handles writing a single byte to output file"
Object subclass: ByteWriter [
   |byteWrite byteIdx aStream|

   ByteWriter class >> new: outFile [
      | result |
      result := super new.
      result init: outFile.
      ^ result
   ]

   init: outFile [
      byteWrite := 0.
      byteIdx := 8.
      aStream := outFile.
   ]

   writeBit: aBit [
      byteWrite := byteWrite bitAt: byteIdx put: aBit.
      byteIdx := byteIdx - 1.

      byteIdx < 1 ifTrue: [
         byteIdx := 8.
         aStream nextPutByte: byteWrite.
      ].
   ]

   writeChar: aChar [
      "aStream << 'input to writeChar = ' << aChar << Character nl."
      (1 to: 8) do: [:i|
         byteWrite := byteWrite bitAt: byteIdx put: (aChar bitAt: (9 - i)).
         "aStream << 'putting: ' << (aChar bitAt: (9 - i)) << ' at bit ' << byteIdx 
            << Character nl."
         byteIdx := byteIdx - 1.

         byteIdx < 1 ifTrue: [
            byteIdx := 8.
            aStream nextPutByte: byteWrite.
         ].
      ].
   ]

   padZero [
      "byteIdx = 8 means there are not bits in byteWrite to be written"
      (byteIdx < 8) ifTrue: [
         (1 to: byteIdx) do: [:i|
            byteWrite := byteWrite bitAt: (9 - i) put: 0.
         ].
         aStream nextPutByte: byteWrite.
      ].
   ]

   endTree [
      aStream nextPutByte: 1.
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

"creating one tree from priority queue"
[sortFreq size > 1] whileTrue: [
   |lLeaf rLeaf tree totalCount|
   lLeaf := sortFreq removeFirst.
   rLeaf := sortFreq removeFirst.
   totalCount := (lLeaf count) + (rLeaf count).
   tree := Tree new: (lLeaf char) count: totalCount left: lLeaf right: rLeaf.
   sortFreq add: tree.
].

"creating the bitTree that holds binary encoding of each char as a string"
bitTree := Array new: 257.
huffTree := sortFreq removeFirst.
huffTree inorder: [:char :string |
   "stdout << nl <<'[' << char << ']=' << string."
   bitTree at: (char asInteger) put: string.
] prefix: ''.

"option: -t --> prints out the encoding table"
(1 to: 257) do: [:i|
   |count countPadded|
   count := frequency at: i.
   (count > 0) ifTrue: [
      countPadded := count printPaddedWith: Character space to: 6.
      (i = 257) ifTrue: [
         stdout << 'EOF' << countPadded << ' ' << (bitTree at: i).
      ]
      ifFalse: [
         ((i > 32) & (i < 127)) ifTrue: [
            stdout << ' ' << (i asCharacter) << ' '.
         ]
         ifFalse: [
            stdout << (i printStringRadix: 16).
         ].
         stdout << countPadded << ' '.
         stdout << (bitTree at: i).
         stdout << nl.
      ].
   ].
].

writeByte := ByteWriter new: fileOut.

"encoding path to characters in huffTree"
huffTree postorder: writeByte.
writeByte endTree.

"encoding fileIn contents"
fileIn := FileStream open: (Smalltalk arguments at: 2) mode: FileStream read.
[fileIn atEnd] whileFalse: [
   character := fileIn next.
   charNum := character asInteger.
   pathStr := bitTree at: charNum.
   (1 to: pathStr size) do: [:i|
      |binary|
      binary := (((pathStr at: i) asInteger) - 48).
      writeByte writeBit: binary.
   ].
].

"writing EOF path to file"
EOF := bitTree at: 257.
(1 to: EOF size) do: [:i|
   |binary|
   binary := (((EOF at: i) asInteger) - 48).
   writeByte writeBit: binary.
].
fileIn close.
writeByte padZero.

"File remove: 'encodedTreeStream.txt'."
((Smalltalk arguments size) = 3) ifTrue: [
   fileOut close.
].

stdout << nl << 'Done.' << nl.
