#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: wc.st,v 1.2 2018-05-17 18:21:54-07 - - $"

chars := 0.
lines := 0.

[stdin atEnd] whileFalse: [
   |byte|
   byte := stdin next.
   chars := chars + 1.
   (byte = Character nl) ifTrue: [ lines := lines + 1 ].
].

stdout << lines << ' lines, ' << chars << ' chars.' << Character nl.

