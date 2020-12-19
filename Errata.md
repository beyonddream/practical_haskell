Errata (pdf)
------

page 32
Create a folder for each component of its name but the last one, in this case a folder A inside a folder B.
	- should be "folder B inside a folder A"

page 38
two destructor functions -> "destructure"

page 45
Let’s see how the execution of a call to clientName (Individual [Person "Jack" "Smith" Male])
should be "clientName (Individual (Person "Jack" "Smith" Male) False)"

page 66
"As you can see from the examples, a polymorphic type is written with its name along with a list of all its type parameters, like Maybe Integer"
- "Maybe Integer" should be "Maybe a"

page 72
https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base/Prelude.html#v:not - link seem to be broken.

page 76
But if you curry it, you must call it with only one argument, which is a tuple.
*Chapter3.FnsParams> (uncurry max) (3,2)
should be "But if you uncurry it"

page 78
"import Data.List (permutations, subsequence)"
should be "import Data.List (permutations, subsequences)"

page 83
1 + (2 + foldr (+) [3])
should be "1 + (2 + foldr (+) 0 [3])"

page 98
[ toUpper c | s <- "A","list"], c <- ' ':s ]
should be "[ toUpper c | s <- ["A","list"], c <- ' ':s ]"

page 110
"When your use Stack"
should be "When you use Stack"

page 129
In such a case, you call graphFromEdges, which takes a list of triples, (value, key, [key]), the latest component being the aforementioned list of neighbors
should be "last component"

page 144
"The special property of binary trees is that any node in the left subtree will hold only those values smaller than the one in the node..."
should be "The special property of binary search trees..."

page 149
"Some of the most important ones are All, which implements the monoid structure of Bool under the operation (&&) with neutral element True; and All, which does the same with (||) and neutral element False"
should be "and Any, which does the same with (||)"

page 152 (e-book)
        "In this case, just remember that a f b" should be "remember that, a f b"

page 161
" As a special offer, Timely Inc. supplies an infinite number of time machines to travel to the year 2021"
But the example is "infinite2020Machines = TM "Timely Inc." 2020 : infinite2020Machines"
Either 2021 has to be changed to 2020 or the example has to be changed to 2021

page 281
"Since you don’t need to perform any computation, you can use the readMVar function, which is equivalent to readMVar followed by putMVar with that same value."

Should be "equivalent to takeMVar"

page 293
downgrade to below.
amqp-worker-0.2.5@sha256:59d6454305e9d416a20420c65119a5b4f988d0aea69f4c0d97b5c61205313059,1930

page 350
replace "(" "\\(" . replace ")" "\\(" . pack should be replace "(" "\\(" . replace ")" "\\)" . pack

page 351
In page 351, there is no corresponding change for saveClients function shown in the Builders example.
fixed it with `L.sourceList clients .| L.map (L.toStrict . B.toLazyText . clientToText)`

page 513 . st should be s
```haskell
...
                          H.ul $ mapM_ (\(s, e) -> H.li $ H.a H.! A.href (fromString ("#elt" ++ show e))
                                                        $ H.toHtml st)
                                    @root.subtitles
...
```
