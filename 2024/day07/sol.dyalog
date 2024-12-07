⍝ Get and parse input
filename←⊃2⌷⊢2⎕NQ#'GetCommandLineArgs'
s←{⍎∊':'(≠⊆⊢)⍵}¨⊃⎕nget filename 1

⍝ Part 1
⍝ All binary numbers of length ⍵
bins←{'0',{∊⍕¨2⊥⍣¯1⊢⍵}¨⍳(¯1+2*⍵)}
⍝ Pad ⍵ to length ⍺ with zeros
zpad←{∊((⍺-≢⍵)⍴'0'),⍕⍵}
ops←{⍵≡'1':'+' ⋄ '×'}
⎕←+/⊃¨s/⍨∨/¨{⍎¨⍕¨{¯1↓⍵}¨(∊((⊃⍵),⌽1↓⍵),¨⊢)¨('=',⊢)¨⌽¨ops¨¨(¯1+≢⍵)zpad¨bins ¯2+≢⍵}¨s
