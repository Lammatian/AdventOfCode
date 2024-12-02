⍝ Get input
filename←⊃2⌷⊢2⎕NQ#'GetCommandLineArgs'
inp←⍎¨⊃⎕nget filename 1

⍝ Part 1
inc←{∧/2</⍵}
dec←{∧/2>/⍵}
sdif←{3≥⌈/|2-/⍵}
safe←(((sdif∧dec)∨(sdif∧inc))⊢)
⎕←+/safe¨inp

⍝ Part 2
⍝ Identity matrix, slow but nice version
id←{∘.=⍨⍳⍵}
⍝ Fast version
⍝ id←{⍵ ⍵⍴1,⍵⍴0}
⎕←+/{∨/safe ⍵((/⍨)⍤1)~id≢⍵}¨inp
